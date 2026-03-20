use super::*;

impl ModelicaLanguageServer {
    pub(super) async fn ensure_source_libraries_loaded(
        &self,
        text: &str,
        current_document_path: &str,
    ) -> bool {
        let library_paths = self.library_paths.read().await.clone();
        self.ensure_source_libraries_loaded_with_paths(text, current_document_path, &library_paths)
            .await
    }

    pub(super) async fn source_requires_unloaded_libraries(&self, text: &str) -> bool {
        let library_paths = self.library_paths.read().await.clone();
        let loaded_libraries = self.loaded_libraries.read().await.clone();
        let mut seen_library_paths = HashSet::new();

        for lib_path in library_paths {
            let path_key = canonical_path_key(&lib_path);
            if !seen_library_paths.insert(path_key.clone()) {
                continue;
            }
            if loaded_libraries.contains(&path_key) {
                continue;
            }
            let should_load =
                should_load_library_for_source(text, Path::new(&lib_path)).unwrap_or(true);
            if should_load {
                return true;
            }
        }

        false
    }

    pub(super) async fn ensure_source_libraries_loaded_with_paths(
        &self,
        text: &str,
        current_document_path: &str,
        library_paths: &[String],
    ) -> bool {
        let mut library_state_epoch = self.current_library_state_epoch();
        let already_loaded = self.loaded_libraries.read().await.clone();
        let mut seen_library_paths = HashSet::new();
        let mut claimed_roots = HashMap::new();
        let mut loaded_any = false;
        for loaded_path in &already_loaded {
            seen_library_paths.insert(canonical_path_key(loaded_path));
            for root in infer_library_roots(Path::new(loaded_path)).unwrap_or_default() {
                claimed_roots
                    .entry(root)
                    .or_insert_with(|| loaded_path.clone());
            }
        }

        let mut progress_messages = Vec::new();
        let mut load_errors = Vec::new();
        for lib_path in library_paths {
            let path_key = canonical_path_key(lib_path);
            if !seen_library_paths.insert(path_key.clone()) {
                continue;
            }
            if already_loaded.contains(&path_key) {
                continue;
            }
            let inferred_roots = infer_library_roots(Path::new(lib_path)).unwrap_or_default();
            if let Some((root, provider)) = duplicate_root_provider(&inferred_roots, &claimed_roots)
            {
                progress_messages.push(format!(
                    "[rumoca] Skipping library {} (duplicate root '{}' already loaded from {})",
                    lib_path, root, provider
                ));
                continue;
            }
            let should_load =
                should_load_library_for_source(text, Path::new(lib_path)).unwrap_or(true);
            if !should_load {
                continue;
            }
            progress_messages.push(format!("[rumoca] Loading library: {lib_path}"));
            let source_set_id = library_source_set_id(lib_path);
            let loaded = match self
                .load_library_source_set_if_current(
                    lib_path,
                    &path_key,
                    &source_set_id,
                    Some(current_document_path),
                    library_state_epoch,
                    LibraryIndexingReason::SaveDiagnostics,
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
            let status = match loaded.cache_status {
                LibraryCacheStatus::Hit => "cache hit",
                LibraryCacheStatus::Miss => "cache miss",
                LibraryCacheStatus::Disabled => "cache disabled",
            };
            progress_messages.push(format!(
                "[rumoca] Library {} ({}) — {} files, {} inserted [key={}, cache={}]",
                lib_path,
                status,
                loaded.indexed_file_count,
                loaded.inserted_file_count,
                loaded.cache_key,
                loaded.cache_path
            ));
            loaded_any = true;
            claim_roots(&mut claimed_roots, inferred_roots, lib_path);
            library_state_epoch = self.current_library_state_epoch();
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
        stats_before: rumoca_session::compile::SessionCacheStatsSnapshot,
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
        stats_before: rumoca_session::compile::SessionCacheStatsSnapshot,
        mut request_token: AnalysisRequestToken,
    ) {
        let request_started = Instant::now();
        let file_name = session_document_uri_key(&uri);
        let request_mutation_epoch = request_token.mutation_epoch;
        let diagnostics_timing_path = self.diagnostics_timing_path.read().await.clone();
        let publish_diagnostics_timing =
            |request_was_stale: bool,
             requested_library_load: bool,
             library_load_ms: u64,
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
                        requested_library_load,
                        library_load_ms,
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
            let library_load_started = Instant::now();
            self.ensure_source_libraries_loaded(text, &file_name).await;
            let library_load_ms = library_load_started.elapsed().as_millis() as u64;
            request_token = self.refresh_analysis_request_revision(request_token).await;
            if self.analysis_request_is_stale(request_token).await {
                publish_diagnostics_timing(true, should_compile, library_load_ms, false, 0);
                return;
            }
            let diagnostics_started = Instant::now();
            let mut session = self.session.write().await;
            let mut diagnostics = handlers::compute_diagnostics_with_mode(
                text,
                &file_name,
                Some(&mut session),
                rumoca_session::compile::SemanticDiagnosticsMode::Save,
            );
            drop(session);
            let diagnostics_compute_ms = diagnostics_started.elapsed().as_millis() as u64;
            diagnostics.extend(self.stored_library_load_diagnostics(&file_name).await);
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
            publish_diagnostics_timing(
                false,
                should_compile,
                library_load_ms,
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
        diagnostics.extend(self.stored_library_load_diagnostics(&file_name).await);
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
        publish_diagnostics_timing(false, false, 0, false, diagnostics_compute_ms);
    }
}
