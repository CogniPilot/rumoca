use super::*;

impl ModelicaLanguageServer {
    async fn parse_source_root_on_indexing_lane(
        &self,
        source_root_path: &str,
    ) -> anyhow::Result<rumoca_compile::source_roots::ParsedSourceRoot> {
        let _indexing_guard = self.work_lanes.indexing.lock().await;
        let source_root_path = source_root_path.to_string();
        tokio::task::spawn_blocking(move || {
            parse_source_root_with_cache(Path::new(&source_root_path))
        })
        .await
        .map_err(|error| anyhow::anyhow!("source-root indexing worker failed: {error}"))?
    }

    async fn notify_source_root_indexing(
        &self,
        reason: SourceRootIndexingReason,
        level: MessageType,
        message: String,
    ) {
        if reason == SourceRootIndexingReason::StartupDurablePrewarm {
            self.client.log_message(level, message).await;
        } else {
            self.client.show_message(level, message).await;
        }
    }

    async fn source_root_load_failure(
        &self,
        lib_path: &str,
        path_key: &str,
        expected_epoch: u64,
        reason: SourceRootIndexingReason,
        err: anyhow::Error,
    ) -> String {
        self.session
            .write()
            .await
            .cancel_source_root_load(path_key, expected_epoch);
        let error_message = source_root_load_error_message(lib_path, &err);
        self.notify_source_root_indexing(
            reason,
            MessageType::WARNING,
            render_source_root_indexing_failed_message(lib_path, reason.label(), &error_message),
        )
        .await;
        if let Some(layout) = err.downcast_ref::<PackageLayoutError>() {
            self.replace_source_root_load_diagnostics(
                lib_path,
                source_root_load_diagnostics_for_package_layout_error(layout),
            )
            .await;
        }
        error_message
    }

    pub(super) async fn reset_session_and_loaded_source_roots(&self) {
        self.clear_simulation_compile_cache().await;
        self.session.write().await.reset_to_open_documents();
        self.source_root_read_prewarm_finished.notify_waiters();
    }

    pub(super) async fn stored_source_root_load_diagnostics(
        &self,
        uri_path: &str,
    ) -> Vec<Diagnostic> {
        self.source_root_load_diagnostics
            .read()
            .await
            .get(&canonical_path_key(uri_path))
            .cloned()
            .unwrap_or_default()
    }

    pub(super) async fn replace_source_root_load_diagnostics(
        &self,
        source_root_path: &str,
        diagnostics_by_uri: HashMap<String, Vec<Diagnostic>>,
    ) {
        let path_key = canonical_path_key(source_root_path);
        let new_keys: HashSet<String> = diagnostics_by_uri.keys().cloned().collect();
        let old_keys = {
            let mut stored = self.source_root_load_diagnostics.write().await;
            let mut owners = self.source_root_load_diagnostic_uris.write().await;
            let old_keys = owners.remove(&path_key).unwrap_or_default();
            for uri in &old_keys {
                stored.remove(uri);
            }
            for (uri, diagnostics) in &diagnostics_by_uri {
                stored.insert(uri.clone(), diagnostics.clone());
            }
            if !new_keys.is_empty() {
                owners.insert(path_key, new_keys.clone());
            }
            old_keys
        };

        for (uri_path, diagnostics) in diagnostics_by_uri {
            if let Ok(uri) = Url::from_file_path(&uri_path) {
                self.client
                    .publish_diagnostics(uri, diagnostics, None)
                    .await;
            }
        }
        for cleared in old_keys.difference(&new_keys) {
            if let Ok(uri) = Url::from_file_path(cleared) {
                self.client.publish_diagnostics(uri, Vec::new(), None).await;
            }
        }
    }

    pub(super) async fn rebuild_dirty_source_roots_before_compile(
        &self,
        current_document_path: &str,
    ) -> std::result::Result<(), String> {
        let dirty_roots = self.dirty_source_roots_for_compile().await;
        if dirty_roots.is_empty() {
            return Ok(());
        }

        for (source_set_key, source_root_kind) in &dirty_roots {
            if self
                .refresh_dirty_source_root_from_live_subtree(source_set_key)
                .await
            {
                continue;
            }

            self.rebuild_dirty_source_root_from_disk(
                source_set_key,
                *source_root_kind,
                current_document_path,
            )
            .await?;
        }
        Ok(())
    }

    async fn dirty_source_roots_for_compile(&self) -> Vec<(String, SourceRootKind)> {
        let session = self.session.read().await;
        session
            .dirty_source_root_keys()
            .into_iter()
            .filter_map(|source_root_key| {
                session
                    .source_root_kind(&source_root_key)
                    .map(|kind| (source_root_key, kind))
            })
            .collect()
    }

    async fn refresh_dirty_source_root_from_live_subtree(&self, source_set_key: &str) -> bool {
        let (plan, status) = {
            let _indexing_guard = self.work_lanes.indexing.lock().await;
            let mut session = self.session.write().await;
            let plan = session.apply_source_root_refresh_plan(source_set_key);
            let status = session.source_root_status(source_set_key);
            (plan, status)
        };
        if plan.as_ref().is_none_or(|plan| plan.full_root_fallback) {
            return false;
        }

        self.source_root_read_prewarm_finished.notify_waiters();
        self.clear_simulation_compile_cache().await;
        let message = status
            .map(|status| render_source_root_status_message(&status))
            .unwrap_or_else(|| {
                format!(
                    "[rumoca] Refreshed dirty source root {} from live subtree edits",
                    source_set_key
                )
            });
        self.client.log_message(MessageType::INFO, message).await;
        true
    }

    async fn rebuild_dirty_source_root_from_disk(
        &self,
        source_set_key: &str,
        source_root_kind: SourceRootKind,
        current_document_path: &str,
    ) -> std::result::Result<(), String> {
        let Some((_, source_root_path)) = source_set_key.split_once("::") else {
            return Ok(());
        };
        let is_compatibility_external_root = matches!(
            source_root_kind,
            SourceRootKind::External | SourceRootKind::DurableExternal
        );
        if is_compatibility_external_root {
            self.client
                .show_message(
                    MessageType::INFO,
                    render_source_root_indexing_started_message(
                        source_root_path,
                        SourceRootIndexingReason::SimulationCompile.label(),
                    ),
                )
                .await;
        }

        let parsed = match self
            .parse_source_root_on_indexing_lane(source_root_path)
            .await
        {
            Ok(parsed) => parsed,
            Err(err) => {
                return Err(self
                    .simulation_compile_parse_error_message(
                        source_root_path,
                        &err,
                        is_compatibility_external_root,
                    )
                    .await);
            }
        };
        if is_compatibility_external_root {
            self.replace_source_root_load_diagnostics(source_root_path, HashMap::new())
                .await;
        }
        let (inserted_count, status) = {
            let mut session = self.session.write().await;
            let inserted_count = session.replace_parsed_source_set(
                source_set_key,
                source_root_kind,
                parsed.documents,
                Some(current_document_path),
            );
            let status = session.source_root_status(source_set_key);
            (inserted_count, status)
        };
        self.source_root_read_prewarm_finished.notify_waiters();
        self.clear_simulation_compile_cache().await;
        if is_compatibility_external_root {
            self.client
                .show_message(
                    MessageType::INFO,
                    render_source_root_indexing_finished_message(
                        source_root_path,
                        SourceRootIndexingReason::SimulationCompile.label(),
                        parsed.file_count,
                        inserted_count,
                        parsed.cache_status,
                    ),
                )
                .await;
        }
        let message = status
            .map(|status| render_source_root_status_message(&status))
            .unwrap_or_else(|| {
                format!(
                    "[rumoca] Rebuilt dirty source root {} ({:?}) — {} files inserted",
                    source_root_path, parsed.cache_status, inserted_count
                )
            });
        self.client.log_message(MessageType::INFO, message).await;
        Ok(())
    }

    pub(super) async fn simulation_compile_parse_error_message(
        &self,
        source_root_path: &str,
        err: &anyhow::Error,
        is_compatibility_external_root: bool,
    ) -> String {
        let error_message = source_root_load_error_message(source_root_path, err);
        if !is_compatibility_external_root {
            return error_message;
        }
        self.client
            .show_message(
                MessageType::WARNING,
                render_source_root_indexing_failed_message(
                    source_root_path,
                    SourceRootIndexingReason::SimulationCompile.label(),
                    &error_message,
                ),
            )
            .await;
        if let Some(layout) = err.downcast_ref::<PackageLayoutError>() {
            self.replace_source_root_load_diagnostics(
                source_root_path,
                source_root_load_diagnostics_for_package_layout_error(layout),
            )
            .await;
        }
        error_message
    }

    pub(super) async fn load_source_root_if_current(
        &self,
        lib_path: &str,
        path_key: &str,
        source_set_id: &str,
        current_document_path: Option<&str>,
        expected_epoch: u64,
        reason: SourceRootIndexingReason,
    ) -> std::result::Result<Option<SourceRootLoadOutcome>, String> {
        if !self
            .session
            .write()
            .await
            .reserve_source_root_load(path_key, expected_epoch)
        {
            return Ok(None);
        }
        self.notify_source_root_indexing(
            reason,
            MessageType::INFO,
            render_source_root_indexing_started_message(lib_path, reason.label()),
        )
        .await;

        let parsed = match self.parse_source_root_on_indexing_lane(lib_path).await {
            Ok(parsed) => parsed,
            Err(err) => {
                return Err(self
                    .source_root_load_failure(lib_path, path_key, expected_epoch, reason, err)
                    .await);
            }
        };
        self.replace_source_root_load_diagnostics(lib_path, HashMap::new())
            .await;

        let cache_status = parsed.cache_status;
        let cache_key = parsed.cache_key.clone();
        let cache_timing = parsed.timing;
        let initial_source_root_paths = self.initial_source_root_paths.read().await.clone();
        let source_root_kind =
            classify_configured_source_root_kind(lib_path, &initial_source_root_paths);
        let cache_path = parsed
            .cache_file
            .as_deref()
            .map(|p| p.display().to_string())
            .unwrap_or_else(|| "<none>".to_string());
        let parsed_file_count = parsed.file_count;
        let apply_started = Instant::now();
        let Some((inserted_file_count, status)) = ({
            self.session
                .write()
                .await
                .apply_parsed_source_root_if_current(
                    source_set_id,
                    ParsedSourceRootLoad {
                        source_root_kind,
                        source_root_path: Path::new(lib_path),
                        cache_status,
                        path_key,
                        current_document_path,
                        documents: parsed.documents,
                        expected_epoch,
                    },
                )
        }) else {
            let finished_message = render_source_root_indexing_finished_message(
                lib_path,
                &reason.stale_label(),
                parsed_file_count,
                0,
                cache_status,
            );
            self.notify_source_root_indexing(reason, MessageType::INFO, finished_message)
                .await;
            return Ok(None);
        };
        self.source_root_read_prewarm_finished.notify_waiters();
        self.clear_simulation_compile_cache().await;

        if let Some(status) = status.as_ref() {
            self.notify_source_root_indexing(
                reason,
                MessageType::INFO,
                render_source_root_status_message(status),
            )
            .await;
        } else {
            let finished_message = render_source_root_indexing_finished_message(
                lib_path,
                reason.label(),
                parsed_file_count,
                inserted_file_count,
                cache_status,
            );
            self.notify_source_root_indexing(reason, MessageType::INFO, finished_message)
                .await;
        }

        Ok(Some(SourceRootLoadOutcome {
            cache_status,
            parsed_file_count,
            inserted_file_count,
            cache_key,
            cache_path,
            timing: DurableSourceRootLoadTiming {
                cache: cache_timing,
                apply_ms: apply_started.elapsed().as_millis() as u64,
            },
            status,
        }))
    }

    pub(super) async fn reload_project_config(&self) {
        let _ = self.reload_project_config_with_timing().await;
    }

    pub(super) async fn reload_project_config_with_timing(&self) -> ProjectReloadTiming {
        let reload_started = Instant::now();
        let previous_paths = self.source_root_paths.read().await.clone();
        let initial_source_root_paths = self.initial_source_root_paths.read().await.clone();
        let workspace_root = self.workspace_root.read().await.clone();
        let mut timing = ProjectReloadTiming::default();
        let next_paths = if let Some(workspace_root) = workspace_root {
            let discover_started = Instant::now();
            match ProjectConfig::discover(&workspace_root) {
                Ok(config) => {
                    self.log_project_diagnostics(
                        config
                            .as_ref()
                            .map_or(&[], |cfg| cfg.diagnostics.as_slice()),
                    )
                    .await;
                    *self.project_config.write().await = config;
                }
                Err(error) => {
                    *self.project_config.write().await = None;
                    self.client
                        .log_message(
                            MessageType::WARNING,
                            format!("[rumoca] failed to load .rumoca/project.toml: {error}"),
                        )
                        .await;
                }
            }
            timing.project_discover_ms = discover_started.elapsed().as_millis() as u64;
            let resolve_started = Instant::now();
            let project_paths = self
                .project_config
                .read()
                .await
                .as_ref()
                .map(|cfg| cfg.resolve_all_source_root_paths())
                .unwrap_or_default();
            let next_paths = merge_source_root_paths(&project_paths, &initial_source_root_paths);
            timing.resolve_source_root_paths_ms = resolve_started.elapsed().as_millis() as u64;
            next_paths
        } else {
            *self.project_config.write().await = None;
            let resolve_started = Instant::now();
            let next_paths = merge_source_root_paths(&[], &initial_source_root_paths);
            timing.resolve_source_root_paths_ms = resolve_started.elapsed().as_millis() as u64;
            next_paths
        };

        let should_reset = source_root_paths_changed(&previous_paths, &next_paths);
        timing.source_root_paths_changed = should_reset;
        *self.source_root_paths.write().await = next_paths;
        if should_reset {
            let reset_started = Instant::now();
            self.reset_session_and_loaded_source_roots().await;
            timing.reset_session_ms = reset_started.elapsed().as_millis() as u64;
            let initial_startup =
                previous_paths.is_empty() && self.session.read().await.document_uris().is_empty();

            let durable_started = Instant::now();
            let durable_timing = self.prewarm_durable_source_roots().await;
            timing.durable_prewarm_ms = durable_started.elapsed().as_millis() as u64;
            timing.durable_collect_files_ms = durable_timing.durable_collect_files_ms;
            timing.durable_hash_inputs_ms = durable_timing.durable_hash_inputs_ms;
            timing.durable_cache_lookup_ms = durable_timing.durable_cache_lookup_ms;
            timing.durable_cache_deserialize_ms = durable_timing.durable_cache_deserialize_ms;
            timing.durable_parse_files_ms = durable_timing.durable_parse_files_ms;
            timing.durable_validate_layout_ms = durable_timing.durable_validate_layout_ms;
            timing.durable_cache_write_ms = durable_timing.durable_cache_write_ms;
            timing.durable_apply_ms = durable_timing.durable_apply_ms;

            if !initial_startup {
                let workspace_symbol_started = Instant::now();
                self.session
                    .read()
                    .await
                    .workspace_symbol_snapshot()
                    .prewarm_workspace_symbol_queries();
                timing.workspace_symbol_prewarm_ms =
                    workspace_symbol_started.elapsed().as_millis() as u64;
            }

            let namespace_started = Instant::now();
            self.spawn_background_source_root_read_prewarm().await;
            timing.source_root_read_prewarm_spawn_ms =
                namespace_started.elapsed().as_millis() as u64;
        }
        timing.total_ms = reload_started.elapsed().as_millis() as u64;
        timing
    }

    pub(super) async fn prewarm_durable_source_roots(&self) -> ProjectReloadTiming {
        let durable_source_root_paths = self.initial_source_root_paths.read().await.clone();
        let (already_loaded, mut source_root_state_epoch) = {
            let session = self.session.read().await;
            (
                session.loaded_source_root_path_keys(),
                session.source_root_state_epoch(),
            )
        };
        let load_plan = plan_source_root_loads(&durable_source_root_paths, &already_loaded);
        let mut timing = ProjectReloadTiming::default();

        for source_root_path in load_plan.load_paths {
            let path_key = canonical_path_key(&source_root_path);
            let source_set_id = source_root_source_set_key(&source_root_path);
            let Ok(Some(outcome)) = self
                .load_source_root_if_current(
                    &source_root_path,
                    &path_key,
                    &source_set_id,
                    None,
                    source_root_state_epoch,
                    SourceRootIndexingReason::StartupDurablePrewarm,
                )
                .await
            else {
                continue;
            };
            outcome.timing.accumulate_into(&mut timing);
            source_root_state_epoch = self.session.read().await.source_root_state_epoch();
        }
        timing
    }

    pub(super) async fn spawn_background_source_root_read_prewarm(&self) {
        let session_revision = self.current_analysis_revision().await;
        let snapshot = self.session_snapshot().await;
        if !snapshot.needs_source_root_read_prewarm() {
            return;
        }
        if !self
            .session
            .write()
            .await
            .begin_source_root_read_prewarm(session_revision)
        {
            return;
        }
        let server = self.clone();
        tokio::spawn(async move {
            server
                .run_source_root_read_prewarm_snapshot(snapshot, session_revision)
                .await;
            server
                .finish_source_root_read_prewarm(session_revision)
                .await;
        });
    }
}
