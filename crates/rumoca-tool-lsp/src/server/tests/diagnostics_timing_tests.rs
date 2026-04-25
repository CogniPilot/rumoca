use super::*;

fn assert_no_model_query_builds(
    delta: rumoca_compile::compile::SessionCacheStatsSnapshot,
    context: &str,
) {
    for (stage, builds) in [
        ("instantiated_model", delta.instantiated_model_builds),
        ("typed_model", delta.typed_model_builds),
        ("flat_model", delta.flat_model_builds),
        ("dae_model", delta.dae_model_builds),
    ] {
        assert_eq!(builds, 0, "{context} should not build {stage}");
    }
}

fn assert_save_diagnostics_timings(
    cold: &LoggedDiagnosticsTimingSummary,
    warm: &LoggedDiagnosticsTimingSummary,
) {
    assert_eq!(cold.uri, warm.uri);
    assert_eq!(cold.trigger, "save");
    assert_eq!(warm.trigger, "save");
    assert_eq!(cold.semantic_layer, "model_stage");
    assert_eq!(warm.semantic_layer, "model_stage");
    assert!(
        cold.requested_source_root_load,
        "save diagnostics should use save path"
    );
    assert!(
        warm.requested_source_root_load,
        "warm save diagnostics should use save path"
    );
    assert!(cold.ran_compile, "cold save diagnostics should compile");
    assert!(
        warm.ran_compile,
        "warm save diagnostics should still run compile path"
    );
    assert!(
        cold.session_cache_delta
            .interface_semantic_diagnostics_builds
            >= 1,
        "cold save diagnostics should build interface-stage diagnostics artifacts"
    );
    assert!(
        cold.session_cache_delta.body_semantic_diagnostics_builds >= 1,
        "cold save diagnostics should build body-stage diagnostics artifacts"
    );
    assert!(
        cold.session_cache_delta
            .model_stage_semantic_diagnostics_builds
            >= 1,
        "cold save diagnostics should build model-stage diagnostics artifacts"
    );
    assert_model_query_build_chain(cold.session_cache_delta, "cold save diagnostics");
    assert!(
        warm.session_cache_delta
            .interface_semantic_diagnostics_cache_hits
            >= 1,
        "warm save diagnostics should reuse cached interface-stage diagnostics"
    );
    assert!(
        warm.session_cache_delta
            .body_semantic_diagnostics_cache_hits
            >= 1,
        "warm save diagnostics should reuse cached body-stage diagnostics"
    );
    assert!(
        warm.session_cache_delta
            .model_stage_semantic_diagnostics_cache_hits
            >= 1,
        "warm save diagnostics should reuse cached model-stage diagnostics"
    );
    // Session cache stats are process-global, so unrelated parallel LSP tests can
    // contribute incidental cache misses. The stable invariant for the warm save
    // path is that it does not rebuild model stages once semantic diagnostics are
    // already cached.
    assert_no_model_query_builds(warm.session_cache_delta, "warm save diagnostics");
}

fn assert_stale_save_diagnostics_timing(entry: &LoggedDiagnosticsTimingSummary) {
    assert!(
        entry.request_was_stale,
        "stale save diagnostics should be marked stale"
    );
    assert_eq!(entry.requested_edit_epoch, 0);
    assert_eq!(entry.semantic_layer, "stale");
    assert!(
        !entry.ran_compile,
        "stale save diagnostics should skip strict compile"
    );
    assert_eq!(
        entry
            .session_cache_delta
            .interface_semantic_diagnostics_builds,
        0,
        "stale save diagnostics should not build interface-stage diagnostics"
    );
    assert_eq!(
        entry.session_cache_delta.body_semantic_diagnostics_builds, 0,
        "stale save diagnostics should not build body-stage diagnostics"
    );
    assert_eq!(
        entry
            .session_cache_delta
            .model_stage_semantic_diagnostics_builds,
        0,
        "stale save diagnostics should not build model-stage diagnostics"
    );
}

fn assert_no_stale_save_diagnostics_stats(
    before: rumoca_compile::compile::SessionCacheStatsSnapshot,
) {
    let delta = session_cache_stats().delta_since(before);
    assert_eq!(
        delta.interface_semantic_diagnostics_builds, 0,
        "stale save diagnostics should not contribute interface-stage statistics"
    );
    assert_eq!(
        delta.body_semantic_diagnostics_builds, 0,
        "stale save diagnostics should not contribute body-stage statistics"
    );
    assert_eq!(
        delta.model_stage_semantic_diagnostics_builds, 0,
        "stale save diagnostics should not contribute model-stage statistics"
    );
}

#[test]
fn live_publish_diagnostics_stays_parse_only_for_small_sessions() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("small-live-diagnostics");
    let timing_path = temp.join("diagnostics-timings.jsonl");
    run_async_test(async {
        reset_session_cache_stats();
        let service = new_test_service();
        let server = service.inner();
        *server.diagnostics_timing_path.write().await = Some(timing_path.clone());
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_source = "model Active\n  Real x;\nequation\n  der(x) = -x;\nend Active;\n";
        {
            let mut session = server.session.write().await;
            session.update_document(&active_path.to_string_lossy(), active_source);
        }

        server
            .publish_diagnostics(
                active_uri,
                active_source,
                DiagnosticsTrigger::Live,
                session_cache_stats(),
            )
            .await;

        assert!(
            !server.session.read().await.has_resolved_cached(),
            "live diagnostics should stay off the standard resolved path"
        );
        assert!(
            !server.session.read().await.has_standard_resolved_cached(),
            "live diagnostics should not populate the standard resolved cache"
        );
        assert!(
            !server
                .session
                .read()
                .await
                .has_semantic_diagnostics_cached("Active"),
            "live diagnostics should stay off the semantic diagnostics closure path"
        );
    });

    let entries: Vec<LoggedDiagnosticsTimingSummary> = read_jsonl(&timing_path);
    assert_eq!(
        entries.len(),
        1,
        "expected one live diagnostics timing entry"
    );
    let entry = &entries[0];
    assert_eq!(entry.trigger, "live");
    assert_eq!(entry.semantic_layer, "parse_only");
}

#[test]
fn save_publish_diagnostics_reuses_semantic_diagnostics_cache() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("save-diagnostics-timing");
    let timing_path = temp.join("diagnostics-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let service = new_test_service();
        let server = service.inner();
        *server.diagnostics_timing_path.write().await = Some(timing_path.clone());
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_source = "model Active\n  Real x;\nequation\n  der(x) = -x;\nend Active;\n";

        {
            let mut session = server.session.write().await;
            session.update_document(&active_path.to_string_lossy(), active_source);
        }

        server
            .publish_diagnostics(
                active_uri.clone(),
                active_source,
                DiagnosticsTrigger::Save,
                session_cache_stats(),
            )
            .await;
        server
            .publish_diagnostics(
                active_uri,
                active_source,
                DiagnosticsTrigger::Save,
                session_cache_stats(),
            )
            .await;

        assert!(
            !server.session.read().await.has_resolved_cached(),
            "save diagnostics should not populate the legacy resolved owner"
        );
        assert!(
            !server.session.read().await.has_standard_resolved_cached(),
            "save diagnostics should stay off the standard resolved cache"
        );
        assert!(
            server
                .session
                .read()
                .await
                .has_semantic_diagnostics_cached("Active"),
            "save diagnostics should still cache semantic diagnostics artifacts"
        );
    });

    let entries: Vec<LoggedDiagnosticsTimingSummary> = read_jsonl(&timing_path);
    assert_eq!(
        entries.len(),
        2,
        "expected cold and warm diagnostics timings"
    );
    assert_save_diagnostics_timings(&entries[0], &entries[1]);
}

#[test]
fn save_publish_diagnostics_skips_stale_requests_before_strict_compile() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("save-diagnostics-stale");
    let timing_path = temp.join("diagnostics-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let service = new_test_service();
        let server = service.inner();
        *server.diagnostics_timing_path.write().await = Some(timing_path.clone());
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_source = "model Active\n  Real x;\neqn\n  der(x) = -x;\nend Active;\n";

        {
            let mut session = server.session.write().await;
            session.update_document(&active_path.to_string_lossy(), active_source);
        }

        let before = session_cache_stats();
        let request_token = server.begin_analysis_request().await;
        let source_root_paths_guard = server.source_root_paths.write().await;
        let publish_task = tokio::spawn({
            let server = server.clone();
            let active_source = active_source.to_string();
            let active_uri = active_uri.clone();
            async move {
                server
                    .publish_diagnostics_with_token(
                        active_uri,
                        &active_source,
                        DiagnosticsTrigger::Save,
                        session_cache_stats(),
                        request_token,
                    )
                    .await
            }
        });
        tokio::task::yield_now().await;
        server
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: active_uri.clone(),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text:
                        "model Active\n  Real x;\neqn\n  der(x) = -x;\n  // changed\nend Active;\n"
                            .to_string(),
                }],
            })
            .await;
        drop(source_root_paths_guard);

        publish_task
            .await
            .expect("save diagnostics task should finish");

        let entries: Vec<LoggedDiagnosticsTimingSummary> = read_jsonl(&timing_path);
        let save_entries: Vec<_> = entries
            .iter()
            .filter(|entry| entry.trigger == "save")
            .collect();
        assert_eq!(
            save_entries.len(),
            1,
            "expected one save timing entry for stale save diagnostics"
        );
        assert_stale_save_diagnostics_timing(save_entries[0]);
        assert_no_stale_save_diagnostics_stats(before);
    });
}
