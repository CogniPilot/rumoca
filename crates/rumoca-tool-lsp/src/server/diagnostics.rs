use super::*;

pub(super) fn source_root_preparation_failure_diagnostic(
    error: &SourceRootPreparationError,
) -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position::new(0, 0),
            end: Position::new(0, 1),
        },
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("rumoca".to_string()),
        message: format!("Source-root preparation failed: {error}"),
        ..Diagnostic::default()
    }
}

struct DiagnosticsRequestTiming {
    started: Instant,
    request_edit_epoch: u64,
    uri: String,
    trigger: DiagnosticsTrigger,
    stats_before: rumoca_compile::compile::SessionCacheStatsSnapshot,
    output_path: Option<PathBuf>,
}

struct DiagnosticsRunTiming {
    request_was_stale: bool,
    requested_source_root_load: bool,
    source_root_load_ms: u64,
    ran_compile: bool,
    diagnostics_compute_ms: u64,
    semantic_layer_override: Option<&'static str>,
}

impl DiagnosticsRequestTiming {
    fn record(self, run: DiagnosticsRunTiming) {
        let stats_after = session_cache_stats();
        let session_cache_delta = stats_after.delta_since(self.stats_before);
        write_diagnostics_timing_summary(
            &DiagnosticsTimingSummary {
                requested_edit_epoch: self.request_edit_epoch,
                request_was_stale: run.request_was_stale,
                uri: self.uri,
                trigger: diagnostics_trigger_label(self.trigger),
                semantic_layer: run.semantic_layer_override.unwrap_or_else(|| {
                    diagnostics_semantic_layer_label(
                        run.request_was_stale,
                        run.ran_compile,
                        &session_cache_delta,
                    )
                }),
                requested_source_root_load: run.requested_source_root_load,
                source_root_load_ms: run.source_root_load_ms,
                ran_compile: run.ran_compile,
                diagnostics_compute_ms: run.diagnostics_compute_ms,
                total_ms: self.started.elapsed().as_millis() as u64,
                session_cache_delta,
            },
            self.output_path.as_deref(),
        );
    }
}

impl ModelicaLanguageServer {
    pub(super) async fn ensure_source_roots_loaded_with_paths(
        &self,
        text: &str,
        current_document_path: &str,
        source_root_paths: &[String],
    ) -> std::result::Result<bool, SourceRootPreparationError> {
        let (already_loaded, mut source_root_state_epoch) = {
            let session = self.session.read().await;
            (
                session.loaded_source_root_path_keys(),
                session.source_root_state_epoch(),
            )
        };
        let mut loaded_any = false;
        let referenced_source_root_paths =
            rumoca_compile::source_roots::referenced_unloaded_source_root_paths(
                text,
                source_root_paths,
                &already_loaded,
            )?;
        let load_plan = rumoca_compile::source_roots::plan_source_root_loads(
            &referenced_source_root_paths,
            &already_loaded,
        )?;

        let mut progress_messages = Vec::new();
        for skipped in &load_plan.duplicate_root_skips {
            progress_messages.push(format!(
                "[rumoca] Skipping source root {} (duplicate root '{}' already loaded from {})",
                skipped.source_root_path, skipped.root_name, skipped.provider_path
            ));
        }
        for source_root_path in load_plan.load_paths {
            let path_key = canonical_path_key(&source_root_path);
            progress_messages.push(format!("[rumoca] Loading source root: {source_root_path}"));
            let source_set_id = source_root_source_set_key(&source_root_path);
            let loaded = match self
                .load_source_root_if_current(
                    &source_root_path,
                    &path_key,
                    &source_set_id,
                    Some(current_document_path),
                    source_root_state_epoch,
                    SourceRootIndexingReason::SaveDiagnostics,
                )
                .await
            {
                Ok(SourceRootLoadDisposition::Loaded(loaded)) => loaded,
                Ok(SourceRootLoadDisposition::AlreadyLoaded) => continue,
                Err(error) => return Err(error),
            };
            progress_messages.push(
                loaded
                    .status
                    .as_ref()
                    .map(render_source_root_status_message)
                    .unwrap_or_else(|| {
                        format!(
                            "[rumoca] Source root {} — {} files, {} inserted",
                            source_root_path, loaded.parsed_file_count, loaded.inserted_file_count
                        )
                    }),
            );
            loaded_any = true;
            source_root_state_epoch = self.session.read().await.source_root_state_epoch();
        }
        for message in progress_messages {
            self.client.log_message(MessageType::INFO, message).await;
        }
        Ok(loaded_any)
    }

    pub(super) async fn publish_diagnostics(
        &self,
        uri: Url,
        text: &str,
        trigger: DiagnosticsTrigger,
        stats_before: rumoca_compile::compile::SessionCacheStatsSnapshot,
    ) {
        let request_token = self.begin_analysis_request().await;
        self.publish_diagnostics_with_token(uri, text, trigger, stats_before, request_token)
            .await;
    }

    pub(super) async fn publish_diagnostics_with_token(
        &self,
        uri: Url,
        text: &str,
        trigger: DiagnosticsTrigger,
        stats_before: rumoca_compile::compile::SessionCacheStatsSnapshot,
        request_token: AnalysisRequestToken,
    ) {
        let started = Instant::now();
        let file_name = session_document_uri_key(&uri);
        let timing = DiagnosticsRequestTiming {
            started,
            request_edit_epoch: request_token.mutation_epoch,
            uri: file_name.clone(),
            trigger,
            stats_before,
            output_path: self.diagnostics_timing_path.read().await.clone(),
        };
        match trigger {
            DiagnosticsTrigger::Save => {
                self.publish_save_diagnostics(uri, text, &file_name, request_token, timing)
                    .await;
            }
            DiagnosticsTrigger::Live => {
                self.publish_live_diagnostics(uri, text, &file_name, request_token, timing)
                    .await;
            }
        }
    }

    async fn publish_save_diagnostics(
        &self,
        uri: Url,
        text: &str,
        file_name: &str,
        request_token: AnalysisRequestToken,
        timing: DiagnosticsRequestTiming,
    ) {
        let source_root_load_started = Instant::now();
        let source_root_paths = self.source_root_paths.read().await.clone();
        if let Err(error) = self
            .ensure_source_roots_loaded_with_paths(text, file_name, &source_root_paths)
            .await
        {
            let source_root_load_ms = source_root_load_started.elapsed().as_millis() as u64;
            self.client
                .log_message(MessageType::ERROR, error.to_string())
                .await;
            self.client
                .publish_diagnostics(
                    uri,
                    vec![source_root_preparation_failure_diagnostic(&error)],
                    None,
                )
                .await;
            timing.record(DiagnosticsRunTiming {
                request_was_stale: false,
                requested_source_root_load: true,
                source_root_load_ms,
                ran_compile: false,
                diagnostics_compute_ms: 0,
                semantic_layer_override: Some("source_root_error"),
            });
            return;
        }
        let source_root_load_ms = source_root_load_started.elapsed().as_millis() as u64;
        let request_token = self.refresh_analysis_request_revision(request_token).await;
        if self.analysis_request_is_stale(request_token).await {
            timing.record(DiagnosticsRunTiming {
                request_was_stale: true,
                requested_source_root_load: true,
                source_root_load_ms,
                ran_compile: false,
                diagnostics_compute_ms: 0,
                semantic_layer_override: None,
            });
            return;
        }
        let diagnostics_started = Instant::now();
        let tool_options = self.tool_options_for_document_or_default(file_name).await;
        let mut session = self.session.write().await;
        let mut diagnostics = handlers::compute_diagnostics_with_options(
            text,
            file_name,
            Some(&mut session),
            &tool_options.lint,
            rumoca_compile::compile::SemanticDiagnosticsMode::Save,
        );
        drop(session);
        let diagnostics_compute_ms = diagnostics_started.elapsed().as_millis() as u64;
        diagnostics.extend(self.stored_source_root_load_diagnostics(file_name).await);
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
        timing.record(DiagnosticsRunTiming {
            request_was_stale: false,
            requested_source_root_load: true,
            source_root_load_ms,
            ran_compile: true,
            diagnostics_compute_ms,
            semantic_layer_override: None,
        });
    }

    async fn publish_live_diagnostics(
        &self,
        uri: Url,
        text: &str,
        file_name: &str,
        request_token: AnalysisRequestToken,
        timing: DiagnosticsRequestTiming,
    ) {
        let diagnostics_started = Instant::now();
        let tool_options = self.tool_options_for_document_or_default(file_name).await;
        let mut diagnostics = handlers::compute_diagnostics_with_options(
            text,
            file_name,
            None,
            &tool_options.lint,
            rumoca_compile::compile::SemanticDiagnosticsMode::Standard,
        );
        let diagnostics_compute_ms = diagnostics_started.elapsed().as_millis() as u64;
        if self.analysis_request_is_stale(request_token).await {
            timing.record(DiagnosticsRunTiming {
                request_was_stale: true,
                requested_source_root_load: false,
                source_root_load_ms: 0,
                ran_compile: false,
                diagnostics_compute_ms,
                semantic_layer_override: None,
            });
            return;
        }
        diagnostics.extend(self.stored_source_root_load_diagnostics(file_name).await);
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
        timing.record(DiagnosticsRunTiming {
            request_was_stale: false,
            requested_source_root_load: false,
            source_root_load_ms: 0,
            ran_compile: false,
            diagnostics_compute_ms,
            semantic_layer_override: None,
        });
    }
}
