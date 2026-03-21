use super::*;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct LoggedStartupTimingSummary {
    initial_source_root_paths: usize,
    source_root_paths_changed: bool,
    parse_init_options_ms: u64,
    workspace_root_ms: u64,
    reload_project_config_ms: u64,
    project_discover_ms: u64,
    resolve_source_root_paths_ms: u64,
    reset_session_ms: u64,
    durable_prewarm_ms: u64,
    #[serde(default)]
    durable_collect_files_ms: u64,
    #[serde(default)]
    durable_hash_inputs_ms: u64,
    #[serde(default)]
    durable_cache_lookup_ms: u64,
    #[serde(default)]
    durable_cache_deserialize_ms: u64,
    #[serde(default)]
    durable_parse_files_ms: u64,
    #[serde(default)]
    durable_validate_layout_ms: u64,
    #[serde(default)]
    durable_cache_write_ms: u64,
    #[serde(default)]
    durable_apply_ms: u64,
    workspace_symbol_prewarm_ms: u64,
    source_root_read_prewarm_spawn_ms: u64,
    total_ms: u64,
}

#[test]
fn initial_source_root_paths_are_classified_as_durable_roots() {
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let durable_root = std::env::temp_dir().join("rumoca-msl-root");
        let durable_path = durable_root.to_string_lossy().to_string();
        *server.initial_source_root_paths.write().await = vec![durable_path.clone()];
        let initial_source_root_paths = server.initial_source_root_paths.read().await.clone();

        assert_eq!(
            classify_configured_source_root_kind(&durable_path, &initial_source_root_paths),
            SourceRootKind::DurableExternal
        );
        assert_eq!(
            classify_configured_source_root_kind("/tmp/project-lib", &initial_source_root_paths),
            SourceRootKind::External
        );
    });
}

#[test]
fn initialize_writes_startup_timing_breakdown() {
    run_async_test(async {
        let workspace_root = new_temp_dir("initialize-startup-timing");
        let source_root_dir = write_test_source_root(&workspace_root, "InitLib");
        let source_root_path = source_root_dir.to_string_lossy().to_string();
        let workspace_uri = Url::from_directory_path(&workspace_root).expect("workspace uri");
        let timing_path = workspace_root.join("startup-timings.jsonl");
        let service = new_test_service();
        let server = service.inner();
        *server.startup_timing_path.write().await = Some(timing_path.clone());

        server
            .initialize(InitializeParams {
                root_uri: Some(workspace_uri.clone()),
                initialization_options: Some(serde_json::json!({
                    "sourceRootPaths": [source_root_path]
                })),
                workspace_folders: Some(vec![WorkspaceFolder {
                    uri: workspace_uri,
                    name: "workspace".to_string(),
                }]),
                ..InitializeParams::default()
            })
            .await
            .expect("initialize should succeed");

        let entries: Vec<LoggedStartupTimingSummary> = read_jsonl(&timing_path);
        assert_eq!(entries.len(), 1, "expected one startup timing entry");
        let entry = &entries[0];
        assert_eq!(entry.initial_source_root_paths, 1);
        assert!(
            entry.source_root_paths_changed,
            "initialize should reset/prewarm when configured source-root paths change"
        );
        assert!(entry.total_ms >= entry.reload_project_config_ms);
        assert!(entry.reload_project_config_ms >= entry.project_discover_ms);
        assert!(entry.reload_project_config_ms >= entry.resolve_source_root_paths_ms);
        assert!(entry.reload_project_config_ms >= entry.reset_session_ms);
        assert!(entry.reload_project_config_ms >= entry.durable_prewarm_ms);
        let durable_substeps_ms = entry.durable_collect_files_ms
            + entry.durable_hash_inputs_ms
            + entry.durable_cache_lookup_ms
            + entry.durable_cache_deserialize_ms
            + entry.durable_parse_files_ms
            + entry.durable_validate_layout_ms
            + entry.durable_cache_write_ms
            + entry.durable_apply_ms;
        assert!(entry.durable_prewarm_ms >= durable_substeps_ms);
        assert!(entry.reload_project_config_ms >= entry.workspace_symbol_prewarm_ms);
        assert!(
            entry.parse_init_options_ms <= entry.total_ms
                && entry.workspace_root_ms <= entry.total_ms
                && entry.source_root_read_prewarm_spawn_ms <= entry.reload_project_config_ms,
            "startup breakdown should stay internally consistent"
        );
    });
}
