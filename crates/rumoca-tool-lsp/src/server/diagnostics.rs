use super::*;

impl ModelicaLanguageServer {
    pub(super) async fn ensure_source_roots_loaded_with_paths(
        &self,
        text: &str,
        current_document_path: &str,
        source_root_paths: &[String],
    ) -> bool {
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
            );
        let load_plan = rumoca_compile::source_roots::plan_source_root_loads(
            &referenced_source_root_paths,
            &already_loaded,
        );

        let mut progress_messages = Vec::new();
        let mut load_errors = Vec::new();
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
                Ok(Some(loaded)) => loaded,
                Ok(None) => continue,
                Err(err) => {
                    load_errors.push(err);
                    continue;
                }
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
        for err in load_errors {
            self.client.log_message(MessageType::WARNING, err).await;
        }
        loaded_any
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
        mut request_token: AnalysisRequestToken,
    ) {
        let request_started = Instant::now();
        let file_name = session_document_uri_key(&uri);
        let request_mutation_epoch = request_token.mutation_epoch;
        let diagnostics_timing_path = self.diagnostics_timing_path.read().await.clone();
        let publish_diagnostics_timing =
            |request_was_stale: bool,
             requested_source_root_load: bool,
             source_root_load_ms: u64,
             ran_compile: bool,
             diagnostics_compute_ms: u64| {
                let stats_after = session_cache_stats();
                let session_cache_delta = stats_after.delta_since(stats_before);
                write_diagnostics_timing_summary(
                    &DiagnosticsTimingSummary {
                        requested_edit_epoch: request_mutation_epoch,
                        request_was_stale,
                        uri: file_name.clone(),
                        trigger: diagnostics_trigger_label(trigger),
                        semantic_layer: diagnostics_semantic_layer_label(
                            request_was_stale,
                            ran_compile,
                            &session_cache_delta,
                        ),
                        requested_source_root_load,
                        source_root_load_ms,
                        ran_compile,
                        diagnostics_compute_ms,
                        total_ms: request_started.elapsed().as_millis() as u64,
                        session_cache_delta,
                    },
                    diagnostics_timing_path.as_deref(),
                );
            };
        let should_compile = trigger == DiagnosticsTrigger::Save;
        if should_compile {
            let source_root_load_started = Instant::now();
            let source_root_paths = self.source_root_paths.read().await.clone();
            self.ensure_source_roots_loaded_with_paths(text, &file_name, &source_root_paths)
                .await;
            let source_root_load_ms = source_root_load_started.elapsed().as_millis() as u64;
            request_token = self.refresh_analysis_request_revision(request_token).await;
            if self.analysis_request_is_stale(request_token).await {
                publish_diagnostics_timing(true, should_compile, source_root_load_ms, false, 0);
                return;
            }
            let diagnostics_started = Instant::now();
            let mut session = self.session.write().await;
            let mut diagnostics = handlers::compute_diagnostics_with_mode(
                text,
                &file_name,
                Some(&mut session),
                rumoca_compile::compile::SemanticDiagnosticsMode::Save,
            );
            drop(session);
            let diagnostics_compute_ms = diagnostics_started.elapsed().as_millis() as u64;
            diagnostics.extend(self.stored_source_root_load_diagnostics(&file_name).await);
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
            publish_diagnostics_timing(
                false,
                should_compile,
                source_root_load_ms,
                true,
                diagnostics_compute_ms,
            );
            return;
        }
        let diagnostics_started = Instant::now();
        let mut diagnostics = handlers::compute_diagnostics(text, &file_name, None);
        let diagnostics_compute_ms = diagnostics_started.elapsed().as_millis() as u64;
        if self.analysis_request_is_stale(request_token).await {
            publish_diagnostics_timing(true, false, 0, false, diagnostics_compute_ms);
            return;
        }
        diagnostics.extend(self.stored_source_root_load_diagnostics(&file_name).await);
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
        publish_diagnostics_timing(false, false, 0, false, diagnostics_compute_ms);
    }
}
