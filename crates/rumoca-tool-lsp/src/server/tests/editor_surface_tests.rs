use super::*;
use std::sync::atomic::Ordering;

pub(super) async fn assert_formatting_wraps_handler(
    server: &ModelicaLanguageServer,
    formatting_uri: &Url,
) {
    let formatting = server
        .formatting(DocumentFormattingParams {
            text_document: TextDocumentIdentifier {
                uri: formatting_uri.clone(),
            },
            options: FormattingOptions::default(),
            work_done_progress_params: WorkDoneProgressParams::default(),
        })
        .await
        .expect("formatting should succeed")
        .expect("formatting should return an edit");
    assert_eq!(formatting.len(), 1);
    assert!(
        formatting[0].new_text.contains("x = 1;"),
        "formatting should rewrite the document into the formatter's normalized form"
    );
}

pub(super) async fn assert_document_links_and_inlay_hints(
    server: &ModelicaLanguageServer,
    surface_uri: &Url,
) {
    let links = server
        .document_link(DocumentLinkParams {
            text_document: TextDocumentIdentifier {
                uri: surface_uri.clone(),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        })
        .await
        .expect("document links should succeed")
        .expect("document links response");
    assert!(
        links.iter().any(|link| {
            link.target
                .as_ref()
                .is_some_and(|target| target.as_str().starts_with("https://example.com/docs"))
        }),
        "document links should include the external URL"
    );
    assert!(
        links.iter().any(|link| {
            link.target
                .as_ref()
                .is_some_and(|target| target.as_str().ends_with("/Lib/package.mo"))
        }),
        "document links should include the quoted file path"
    );

    let hints = server
        .inlay_hint(InlayHintParams {
            text_document: TextDocumentIdentifier {
                uri: surface_uri.clone(),
            },
            range: Range {
                start: Position::new(0, 0),
                end: Position::new(20, 0),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        })
        .await
        .expect("inlay hints should succeed")
        .expect("inlay hints response");
    assert!(
        hints.iter().any(|hint| matches!(
            &hint.label,
            InlayHintLabel::String(label) if label.contains("[2x3]")
        )),
        "inlay hints should include the array-dimension hint"
    );
}

pub(super) async fn assert_code_actions_wrap_handler(
    server: &ModelicaLanguageServer,
    formatting_uri: &Url,
) {
    let mut diagnostic_session = Session::default();
    let diagnostics = handlers::compute_diagnostics(
        BROKEN_ACTION_SOURCE,
        "input.mo",
        Some(&mut diagnostic_session),
    );
    let range = diagnostics
        .iter()
        .find(|diag| diag.code.as_ref() == Some(&NumberOrString::String("ET001".to_string())))
        .map(|diag| diag.range)
        .expect("ET001 diagnostic range");
    let actions = server
        .code_action(CodeActionParams {
            text_document: TextDocumentIdentifier {
                uri: formatting_uri.clone(),
            },
            range,
            context: CodeActionContext {
                diagnostics,
                only: None,
                trigger_kind: Some(CodeActionTriggerKind::INVOKED),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        })
        .await
        .expect("code action should succeed")
        .expect("code actions should exist");
    assert!(
        actions.iter().any(|action| matches!(
            action,
            CodeActionOrCommand::CodeAction(code_action)
                if code_action.title.contains("Replace `startdt` with `start`")
        )),
        "code actions should surface the unknown-modifier fix"
    );
}

const COMPLETION_SEMANTIC_LIBRARY_SOURCE: &str = r#"package Lib
  model Plane
    Real x, y, theta;
equation
  der(x) = cos(theta);
  der(y) = sin(theta);
  der(theta) = 1;
end Plane;
end Lib;
"#;

const COMPLETION_SEMANTIC_ACTIVE_SOURCE: &str = r#"model Sim
  import Lib.Plane;
  Plane p1, p2;
equation
  p1.x = 1;
end Sim;
"#;

async fn assert_two_completion_calls_contain_label<F>(
    server: &ModelicaLanguageServer,
    mut request: F,
    label: &str,
) where
    F: FnMut() -> CompletionParams,
{
    for response in [request(), request()] {
        let response = server
            .completion(response)
            .await
            .expect("completion should succeed");
        let Some(CompletionResponse::Array(items)) = response else {
            panic!("expected array completion response");
        };
        assert!(
            items.iter().any(|item| item.label == label),
            "completion should include {label}"
        );
    }
}

fn assert_warm_namespace_completion_timings(
    cold: &LoggedCompletionTimingSummary,
    warm: &LoggedCompletionTimingSummary,
) {
    assert!(!cold.request_was_stale && !warm.request_was_stale);
    assert_eq!(cold.uri, warm.uri);
    assert!(
        cold.library_completion_prime_ms >= warm.library_completion_prime_ms,
        "cold namespace completion should spend at least as much time priming the library cache"
    );
    assert!(!cold.needs_resolved_session && !warm.needs_resolved_session);
    assert!(!cold.ast_fast_path_matched && !warm.ast_fast_path_matched);
    assert!(!cold.query_fast_path_matched && !warm.query_fast_path_matched);
    assert!(
        cold.namespace_index_query_misses >= 1,
        "cold namespace completion should miss namespace query cache"
    );
    assert!(cold.class_name_count_after_ensure > 0);
    assert!(
        cold.declaration_index_query_hits + cold.declaration_index_query_misses == 0,
        "cold namespace completion should stay off full declaration index queries"
    );
    assert!(
        cold.source_set_package_membership_query_hits
            + cold.source_set_package_membership_query_misses
            >= 1,
        "cold namespace completion should exercise package membership queries"
    );
    assert!(
        warm.namespace_index_query_hits >= 1,
        "warm namespace completion should hit namespace query cache"
    );
    assert!(
        cold.session_cache_delta.library_completion_cache_misses >= 1,
        "cold namespace completion should miss the library completion cache"
    );
    assert!(
        warm.session_cache_delta.library_completion_cache_hits >= 1,
        "warm namespace completion should hit the library completion cache"
    );
}

fn assert_query_backed_member_completion_timing(
    entry: &LoggedCompletionTimingSummary,
    phase: &str,
) {
    assert_eq!(entry.semantic_layer, "class_interface");
    assert!(
        !entry.request_was_stale,
        "{phase} member completion should not be marked stale"
    );
    assert!(
        entry
            .file_item_index_query_hits
            .saturating_add(entry.file_item_index_query_misses)
            >= entry.file_item_index_query_hits,
        "file-item query counters should be consistent for {phase} member completion"
    );
    assert!(
        !entry.built_resolved_tree,
        "{phase} member completion should stay on the query fast path"
    );
    assert!(
        !entry.had_resolved_cache_before,
        "{phase} member completion should avoid semantic navigation"
    );
    assert_eq!(
        entry.session_cache_delta.semantic_navigation_builds, 0,
        "{phase} member completion should not build semantic navigation"
    );
    assert_no_model_query_activity(entry.session_cache_delta, phase);
}

#[test]
fn execute_command_dispatches_safe_project_command() {
    run_async_test(async {
        let workspace_root = new_temp_dir("execute-command");
        let workspace_uri = Url::from_directory_path(&workspace_root).expect("workspace uri");
        let service = new_test_service();
        let server = service.inner();
        server
            .initialize(InitializeParams {
                root_uri: Some(workspace_uri),
                ..InitializeParams::default()
            })
            .await
            .expect("initialize should succeed");

        let response = server
            .execute_command(ExecuteCommandParams {
                command: "rumoca.project.filesMoved".to_string(),
                arguments: vec![serde_json::json!({
                    "workspaceRoot": workspace_root.display().to_string(),
                    "files": [],
                })],
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
            .expect("execute command should succeed")
            .expect("execute command should return a payload");
        assert_eq!(response, serde_json::json!({ "ok": true }));
    });
}

#[test]
fn resync_sidecars_preserves_loaded_libraries_and_simulation_cache_when_paths_unchanged() {
    run_async_test(async {
        let workspace_root = new_temp_dir("resync-preserves-simulation-cache");
        let focus = workspace_root.join("Root.mo");
        let library_root = workspace_root.join("ModelicaStandardLibrary");
        std::fs::create_dir_all(&library_root).expect("mkdir library root");
        std::fs::write(
            &focus,
            "model Root\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Root;\n",
        )
        .expect("write focus");

        let service = new_test_service();
        let server = service.inner();
        *server.workspace_root.write().await = Some(workspace_root.clone());
        *server.initial_library_paths.write().await =
            vec![library_root.to_string_lossy().to_string()];
        *server.library_paths.write().await = vec![library_root.to_string_lossy().to_string()];

        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        server
            .compile_model_for_simulation("Root", &focus.to_string_lossy())
            .await
            .expect("initial simulation compile should succeed");
        let library_key = canonical_path_key(&library_root.to_string_lossy());
        server
            .loaded_libraries
            .write()
            .await
            .insert(library_key.clone());
        assert_eq!(
            server.simulation_compile_cache.read().await.len(),
            1,
            "initial compile should populate the simulation cache"
        );

        let response = server
            .execute_command(ExecuteCommandParams {
                command: "rumoca.project.resyncSidecars".to_string(),
                arguments: vec![serde_json::json!({
                    "workspaceRoot": workspace_root.display().to_string(),
                    "dryRun": false,
                    "pruneOrphans": false,
                })],
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
            .expect("resync sidecars command should succeed")
            .expect("resync sidecars command should return a payload");
        assert_eq!(
            response.get("ok").and_then(serde_json::Value::as_bool),
            Some(true),
            "resync sidecars should report success"
        );
        assert_eq!(
            server.simulation_compile_cache.read().await.len(),
            1,
            "resync sidecars should not flush simulation cache when library paths are unchanged"
        );
        assert!(
            server.loaded_libraries.read().await.contains(&library_key),
            "resync sidecars should preserve the loaded library set when library paths are unchanged"
        );
    });
}

#[test]
fn reload_project_config_rewarms_durable_libraries_when_paths_change() {
    run_async_test(async {
        let workspace_root = new_temp_dir("reload-project-config-library-reset");
        let focus = workspace_root.join("Root.mo");
        let library_a = write_test_library(&workspace_root, "LibA");
        let library_b = write_test_library(&workspace_root, "LibB");
        std::fs::write(
            &focus,
            "model Root\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Root;\n",
        )
        .expect("write focus");

        let service = new_test_service();
        let server = service.inner();
        *server.workspace_root.write().await = Some(workspace_root.clone());
        *server.initial_library_paths.write().await = vec![library_a.to_string_lossy().to_string()];
        *server.library_paths.write().await = vec![library_a.to_string_lossy().to_string()];

        {
            let mut session = server.session.write().await;
            session.update_document(
                &focus.to_string_lossy(),
                &std::fs::read_to_string(&focus).expect("read focus"),
            );
        }

        server
            .compile_model_for_simulation("Root", &focus.to_string_lossy())
            .await
            .expect("initial simulation compile should succeed");
        server
            .loaded_libraries
            .write()
            .await
            .insert(canonical_path_key(&library_a.to_string_lossy()));

        *server.initial_library_paths.write().await = vec![library_b.to_string_lossy().to_string()];
        server.reload_project_config().await;

        assert!(
            server.simulation_compile_cache.read().await.is_empty(),
            "changing effective library paths must flush simulation cache"
        );
        assert!(
            server
                .loaded_libraries
                .read()
                .await
                .contains(&canonical_path_key(&library_b.to_string_lossy())),
            "changing effective library paths should immediately rewarm durable roots"
        );
        assert!(
            wait_for_library_namespace_cache(server)
                .await
                .contains(&"LibB.A".to_string()),
            "changing effective library paths should also prewarm namespace completion for the new durable root"
        );
        let library_path_keys = server
            .library_paths
            .read()
            .await
            .iter()
            .map(|path| canonical_path_key(path))
            .collect::<Vec<_>>();
        assert_eq!(
            library_path_keys,
            vec![canonical_path_key(&library_b.to_string_lossy())],
            "reloaded project config should publish the updated library path set"
        );
    });
}

#[test]
fn completion_timing_summary_reports_query_backed_local_alias_completion() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("completion-timing-ast-fast-path");
    let timing_path = temp.join("completion-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        *server.completion_timing_path.write().await = Some(timing_path.clone());
        seed_surface_document(server, &active_uri).await;

        let response = server
            .completion(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: active_uri },
                    position: Position {
                        line: 17,
                        character: "  helperInst.".len() as u32,
                    },
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .await
            .expect("completion request should succeed");

        let Some(CompletionResponse::Array(items)) = response else {
            panic!("expected array completion response");
        };
        assert!(
            items.iter().any(|item| item.label == "gain"),
            "local alias completion should expose Helper members"
        );
    });

    let entries: Vec<LoggedCompletionTimingSummary> = read_jsonl(&timing_path);
    assert_eq!(entries.len(), 1, "expected one completion timing entry");

    let entry = &entries[0];
    assert_eq!(entry.semantic_layer, "class_interface");
    assert!(
        !entry.request_was_stale,
        "single local completion request should not be marked stale"
    );
    assert!(
        entry
            .file_item_index_query_hits
            .saturating_add(entry.file_item_index_query_misses)
            >= entry.file_item_index_query_hits,
        "file-item query counters should be consistent"
    );
    assert!(
        !entry.built_resolved_tree,
        "local alias completion should stay on the query-backed path"
    );
    assert!(
        !entry.had_resolved_cache_before,
        "local alias completion should not require a semantic cache entry"
    );
    assert_eq!(
        entry.session_cache_delta.semantic_navigation_builds, 0,
        "local alias completion should not build semantic navigation"
    );
    assert_no_model_query_activity(entry.session_cache_delta, "local alias completion");
    // strict/standard resolved build counters are process-global instrumentation.
    // This fast-path assertion should stay focused on the request-local timing flags
    // and the semantic-navigation delta above.
}

#[test]
fn completion_timing_summary_reports_query_backed_member_reuse() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("completion-timing-summary");
    let timing_path = temp.join("completion-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let library_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);
        let service = new_test_service();
        let server = service.inner();
        *server.completion_timing_path.write().await = Some(timing_path.clone());
        {
            let mut session = server.session.write().await;
            session.update_document(
                &library_path.to_string_lossy(),
                COMPLETION_SEMANTIC_LIBRARY_SOURCE,
            );
            session.update_document(&active_key, COMPLETION_SEMANTIC_ACTIVE_SOURCE);
        }

        let request = || CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: active_uri.clone(),
                },
                position: Position {
                    line: 4,
                    character: "  p1.".len() as u32,
                },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: None,
        };

        assert_two_completion_calls_contain_label(server, request, "x").await;
    });

    let entries: Vec<LoggedCompletionTimingSummary> = read_jsonl(&timing_path);
    assert_eq!(
        entries.len(),
        2,
        "expected cold and warm completion timings"
    );

    let cold = &entries[0];
    let warm = &entries[1];
    assert_query_backed_member_completion_timing(cold, "cold member completion");
    assert_query_backed_member_completion_timing(warm, "warm member completion");
    assert_eq!(
        cold.requested_edit_epoch, warm.requested_edit_epoch,
        "member completion warm replay should share the same request epoch"
    );
    assert_eq!(cold.uri, warm.uri);
    assert_eq!(
        cold.session_cache_delta.namespace_index_query_misses, 0,
        "cold member completion should not build the library namespace cache"
    );
    assert_eq!(
        cold.session_cache_delta.library_completion_cache_misses, 0,
        "cold member completion should not miss the library completion cache"
    );
    assert_eq!(
        warm.session_cache_delta.semantic_navigation_builds, 0,
        "warm member completion should remain on the query fast path"
    );
    assert_eq!(
        warm.session_cache_delta.namespace_index_query_hits, 0,
        "warm member completion should not touch the library namespace cache"
    );
    assert_eq!(
        warm.session_cache_delta.library_completion_cache_hits, 0,
        "warm member completion should not use the library completion cache"
    );
}

#[test]
fn completion_timing_summary_reports_warm_library_namespace_cache_reuse() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("library-completion-timing");
    let timing_path = temp.join("completion-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let library_path = write_test_library(&temp, "Lib");
        let library_key = canonical_path_key(library_path.to_string_lossy().as_ref());
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);
        let active_source = "model Active\n  Lib.\nend Active;\n";

        let service = new_test_service();
        let server = service.inner();
        *server.completion_timing_path.write().await = Some(timing_path.clone());
        *server.library_paths.write().await = vec![library_path.to_string_lossy().to_string()];
        {
            let mut session = server.session.write().await;
            session.update_document(&active_key, active_source);
        }

        let request = || CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: active_uri.clone(),
                },
                position: Position {
                    line: 1,
                    character: "  Lib.".len() as u32,
                },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: None,
        };

        assert_two_completion_calls_contain_label(server, request, "A").await;
        assert!(
            server.loaded_libraries.read().await.contains(&library_key),
            "completion should load the referenced library root"
        );
    });

    let entries: Vec<LoggedCompletionTimingSummary> = read_jsonl(&timing_path);
    assert_eq!(
        entries.len(),
        2,
        "expected cold and warm completion timings"
    );

    let cold = &entries[0];
    let warm = &entries[1];
    assert_eq!(cold.semantic_layer, "package_def_map");
    assert_eq!(warm.semantic_layer, "package_def_map");
    assert!(
        cold.file_item_index_query_hits
            .saturating_add(cold.file_item_index_query_misses)
            >= cold.file_item_index_query_hits,
        "file-item query counters should be consistent for cold namespace completion"
    );
    assert!(
        warm.file_item_index_query_hits
            .saturating_add(warm.file_item_index_query_misses)
            >= warm.file_item_index_query_hits,
        "file-item query counters should be consistent for warm namespace completion"
    );
    assert_warm_namespace_completion_timings(cold, warm);
    assert_eq!(cold.uri, warm.uri);
    assert!(
        !cold.built_resolved_tree,
        "namespace completion should not build a resolved tree"
    );
    assert!(
        !cold.had_resolved_cache_before,
        "cold namespace completion should stay off semantic navigation state"
    );
    assert!(
        warm.class_name_count_after_ensure > 0,
        "warm namespace completion should keep cached class names available"
    );
    assert!(
        !warm.built_resolved_tree,
        "warm namespace completion should not build a resolved tree"
    );
    assert!(
        !warm.had_resolved_cache_before,
        "warm namespace completion should continue staying off semantic navigation state"
    );
}

#[test]
fn hover_timing_summary_reports_query_fast_path_for_local_alias() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("hover-timing-ast-fast-path");
    let timing_path = temp.join("navigation-timings.jsonl");
    let mut expected_request_epoch: Option<u64> = None;

    run_async_test(async {
        reset_session_cache_stats();
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_alias_navigation_document(server, &active_uri, Some(&timing_path)).await;
        expected_request_epoch = Some(server.completion_mutation_epoch.load(Ordering::Acquire));

        let hover = server
            .hover(hover_alias_request(&active_uri))
            .await
            .expect("hover should succeed")
            .expect("hover should return a payload");
        assert!(
            hover_text(&hover).contains("block Target"),
            "hover should resolve the imported alias target"
        );
    });

    let entries: Vec<LoggedNavigationTimingSummary> = read_jsonl(&timing_path);
    assert_eq!(entries.len(), 1, "expected one hover timing entry");

    let entry = &entries[0];
    assert_eq!(entry.request, "hover");
    assert_eq!(entry.request_path, "query_only");
    assert_eq!(entry.semantic_layer, "class_interface");
    assert_eq!(
        entry.requested_edit_epoch,
        expected_request_epoch.expect("expected request epoch should be captured"),
        "hover should report request epoch"
    );
    assert!(
        !entry.request_was_stale,
        "single hover request should not be marked stale"
    );
    assert!(
        !entry.built_resolved_tree,
        "local alias hover should stay on the query-backed path"
    );
    assert!(
        !entry.had_resolved_cache_before,
        "local alias hover should not require a semantic cache entry"
    );
    assert_eq!(
        entry.session_cache_delta.semantic_navigation_builds, 0,
        "local alias hover should not build semantic navigation"
    );
    assert_eq!(
        entry.session_cache_delta.standard_resolved_builds, 0,
        "local alias hover should avoid the standard resolved session"
    );
    assert_eq!(
        entry.session_cache_delta.strict_resolved_builds, 0,
        "local alias hover should avoid strict resolved state"
    );
}

#[test]
fn hover_timing_summary_reports_parsed_library_fast_path_for_qualified_type_path() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("hover-qualified-path-fast");
    let timing_path = temp.join("navigation-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let library_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_cross_file_qualified_path_document(
            server,
            &library_path,
            &active_uri,
            Some(&timing_path),
        )
        .await;

        let hover = server
            .hover(qualified_path_hover_request(&active_uri))
            .await
            .expect("hover should succeed")
            .expect("hover should return a payload");
        assert!(
            hover_text(&hover).contains("block Target"),
            "qualified type-path hover should resolve the library target"
        );
    });

    let entries: Vec<LoggedNavigationTimingSummary> = read_jsonl(&timing_path);
    assert_eq!(entries.len(), 1, "expected one hover timing entry");

    let entry = &entries[0];
    assert_eq!(entry.request, "hover");
    assert_eq!(entry.request_path, "query_only");
    assert_eq!(entry.semantic_layer, "class_interface");
    assert!(
        !entry.request_was_stale,
        "single hover request should not be marked stale"
    );
    assert!(
        !entry.built_resolved_tree,
        "qualified type-path hover should stay off semantic navigation"
    );
    assert!(
        !entry.had_resolved_cache_before,
        "qualified type-path hover should not require a semantic cache entry"
    );
    assert_eq!(
        entry.session_cache_delta.semantic_navigation_builds, 0,
        "qualified type-path hover should not build semantic navigation"
    );
    assert_eq!(
        entry.session_cache_delta.standard_resolved_builds, 0,
        "qualified type-path hover should avoid the standard resolved session"
    );
    assert_eq!(
        entry.session_cache_delta.strict_resolved_builds, 0,
        "qualified type-path hover should avoid strict resolved state"
    );
}

#[test]
fn hover_timing_summary_reports_query_fast_path_for_imported_class() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("hover-timing-summary");
    let timing_path = temp.join("navigation-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let library_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_cross_file_alias_navigation_document(
            server,
            &library_path,
            &active_uri,
            Some(&timing_path),
        )
        .await;

        let first = server
            .hover(cross_file_alias_hover_request(&active_uri))
            .await
            .expect("cold hover should succeed");
        let second = server
            .hover(cross_file_alias_hover_request(&active_uri))
            .await
            .expect("warm hover should succeed");
        for response in [first, second] {
            let hover = response.expect("hover should resolve the imported alias");
            assert!(
                hover_text(&hover).contains("block Target"),
                "hover should resolve the imported class target"
            );
        }
    });

    let entries: Vec<LoggedNavigationTimingSummary> = read_jsonl(&timing_path);
    assert_eq!(entries.len(), 2, "expected cold and warm hover timings");

    let cold = &entries[0];
    let warm = &entries[1];
    assert!(
        !cold.request_was_stale,
        "initial hover request should not be stale"
    );
    assert!(
        !warm.request_was_stale,
        "warm hover request should remain non-stale"
    );
    assert_eq!(cold.uri, warm.uri);
    assert_eq!(cold.request, "hover");
    assert_eq!(warm.request, "hover");
    assert_eq!(cold.request_path, "query_only");
    assert_eq!(warm.request_path, "query_only");
    assert_eq!(cold.semantic_layer, "class_interface");
    assert_eq!(warm.semantic_layer, "class_interface");
    assert!(!cold.built_resolved_tree);
    assert!(!cold.had_resolved_cache_before);
    assert_eq!(cold.session_cache_delta.semantic_navigation_builds, 0);
    assert_eq!(
        cold.session_cache_delta.standard_resolved_builds, 0,
        "cold hover should avoid the standard resolved session"
    );
    assert_no_model_query_activity(cold.session_cache_delta, "cold imported-class hover");
    assert!(!warm.had_resolved_cache_before);
    assert!(!warm.built_resolved_tree);
    assert_eq!(warm.session_cache_delta.semantic_navigation_cache_hits, 0);
    assert_eq!(
        warm.session_cache_delta.standard_resolved_builds, 0,
        "warm hover should continue avoiding the standard resolved session"
    );
    assert_no_model_query_activity(warm.session_cache_delta, "warm imported-class hover");
}

#[test]
fn goto_definition_timing_summary_reports_query_fast_path_for_imported_class() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("definition-timing-summary");
    let timing_path = temp.join("navigation-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let library_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_cross_file_alias_navigation_document(
            server,
            &library_path,
            &active_uri,
            Some(&timing_path),
        )
        .await;

        let first = server
            .goto_definition(cross_file_alias_definition_request(&active_uri))
            .await
            .expect("cold goto-definition should succeed");
        let second = server
            .goto_definition(cross_file_alias_definition_request(&active_uri))
            .await
            .expect("warm goto-definition should succeed");
        for response in [first, second] {
            let Some(GotoDefinitionResponse::Scalar(location)) = response else {
                panic!("expected scalar goto-definition response");
            };
            assert_eq!(
                location.uri,
                Url::from_file_path(&library_path).expect("file uri"),
                "goto-definition should jump to the library file"
            );
            assert_eq!(
                location.range.start.line, 1,
                "goto-definition should jump to the imported class"
            );
        }
    });

    let entries: Vec<LoggedNavigationTimingSummary> = read_jsonl(&timing_path);
    assert_eq!(
        entries.len(),
        2,
        "expected cold and warm goto-definition timings"
    );

    let cold = &entries[0];
    let warm = &entries[1];
    assert!(
        !cold.request_was_stale,
        "initial goto request should not be stale"
    );
    assert!(
        !warm.request_was_stale,
        "warm goto request should remain non-stale"
    );
    assert_eq!(cold.uri, warm.uri);
    assert_eq!(cold.request, "definition");
    assert_eq!(warm.request, "definition");
    assert_eq!(cold.request_path, "query_only");
    assert_eq!(warm.request_path, "query_only");
    assert_eq!(cold.semantic_layer, "class_interface");
    assert_eq!(warm.semantic_layer, "class_interface");
    assert!(!cold.built_resolved_tree);
    assert!(!cold.had_resolved_cache_before);
    assert_eq!(cold.session_cache_delta.semantic_navigation_builds, 0);
    assert_eq!(
        cold.session_cache_delta.standard_resolved_builds, 0,
        "cold goto-definition should avoid the standard resolved session"
    );
    assert_no_model_query_activity(cold.session_cache_delta, "cold imported-class goto");
    assert!(!warm.had_resolved_cache_before);
    assert!(!warm.built_resolved_tree);
    assert_eq!(warm.session_cache_delta.semantic_navigation_cache_hits, 0);
    assert_eq!(
        warm.session_cache_delta.standard_resolved_builds, 0,
        "warm goto-definition should continue avoiding the standard resolved session"
    );
    assert_no_model_query_activity(warm.session_cache_delta, "warm imported-class goto");
}

#[test]
fn goto_definition_timing_summary_reports_parsed_library_fast_path_for_qualified_type_path() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("definition-qualified-path-fast");
    let timing_path = temp.join("navigation-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let library_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_cross_file_qualified_path_document(
            server,
            &library_path,
            &active_uri,
            Some(&timing_path),
        )
        .await;

        let definition = server
            .goto_definition(qualified_path_definition_request(&active_uri))
            .await
            .expect("goto-definition should succeed")
            .expect("goto-definition should resolve a target");
        let GotoDefinitionResponse::Scalar(location) = definition else {
            panic!("expected scalar goto-definition response");
        };
        assert_eq!(
            location.uri,
            Url::from_file_path(&library_path).expect("library uri"),
            "qualified type-path goto-definition should jump to the library file"
        );
    });

    let entries: Vec<LoggedNavigationTimingSummary> = read_jsonl(&timing_path);
    assert_eq!(
        entries.len(),
        1,
        "expected one goto-definition timing entry"
    );

    let entry = &entries[0];
    assert_eq!(entry.request, "definition");
    assert_eq!(entry.request_path, "query_only");
    assert_eq!(entry.semantic_layer, "class_interface");
    assert!(
        !entry.request_was_stale,
        "single goto request should not be marked stale"
    );
    assert!(
        !entry.built_resolved_tree,
        "qualified type-path goto-definition should stay off semantic navigation"
    );
    assert!(
        !entry.had_resolved_cache_before,
        "qualified type-path goto-definition should not require a semantic cache entry"
    );
    assert_eq!(
        entry.session_cache_delta.semantic_navigation_builds, 0,
        "qualified type-path goto-definition should not build semantic navigation"
    );
    assert_eq!(
        entry.session_cache_delta.standard_resolved_builds, 0,
        "qualified type-path goto-definition should avoid the standard resolved session"
    );
    assert_eq!(
        entry.session_cache_delta.strict_resolved_builds, 0,
        "qualified type-path goto-definition should avoid strict resolved state"
    );
}

#[test]
fn hover_timing_summary_marks_flat_preview_requests() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("hover-flat-preview");
    let timing_path = temp.join("navigation-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);
        let service = new_test_service();
        let server = service.inner();
        *server.navigation_timing_path.write().await = Some(timing_path.clone());
        {
            let mut session = server.session.write().await;
            session.update_document(
                &active_key,
                "model Helper\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Helper;\n",
            );
        }

        let hover = server
            .hover(HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: active_uri.clone(),
                    },
                    position: Position {
                        line: 0,
                        character: "model Help".len() as u32,
                    },
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
            .expect("hover should succeed")
            .expect("hover should return a payload");
        assert!(
            hover_text(&hover).contains("Flattened DAE Preview"),
            "model hover should append the flattened preview"
        );
    });

    let entries: Vec<LoggedNavigationTimingSummary> = read_jsonl(&timing_path);
    assert_eq!(entries.len(), 1, "expected one hover timing entry");

    let entry = &entries[0];
    assert_eq!(entry.request, "hover");
    assert_eq!(entry.request_path, "flat_preview");
    assert_eq!(entry.semantic_layer, "flat_model");
    assert!(
        !entry.request_was_stale,
        "single hover request should not be marked stale"
    );
}

#[test]
#[ignore = "requires cached MSL under target/msl"]
fn msl_completion_loads_libraries_on_demand() {
    run_async_test(async {
        let msl_root = cached_msl_library_root().expect("cached MSL should exist");
        let temp = new_temp_dir("msl-completion-load");
        let active_path = temp.join("active.mo");
        let library_key = canonical_path_key(msl_root.to_string_lossy().as_ref());

        let service = new_test_service();
        let server = service.inner();
        *server.library_paths.write().await = vec![msl_root.to_string_lossy().to_string()];
        server
            .ensure_completion_libraries(
                "within Modelica.Electrical.Analog.Examples;\nmodel Resistor\n  Modelica.Electrical.Analog.Basic.Ground g;\nend Resistor;\n",
                Position {
                    line: 2,
                    character: "  Modelica.".len() as u32,
                },
                &active_path.to_string_lossy(),
            )
            .await;

        assert!(
            server.loaded_libraries.read().await.contains(&library_key),
            "cached MSL should be loaded on demand"
        );
    });
}

#[test]
#[ignore = "requires cached MSL under target/msl"]
fn msl_completion_on_open_library_example_returns_namespace_members() {
    run_async_test(async {
        let msl_root = cached_msl_library_root().expect("cached MSL should exist");
        let example_path = msl_root.join("Electrical/Analog/Examples/Resistor.mo");
        let example_uri = Url::from_file_path(&example_path).expect("file uri");
        let source = std::fs::read_to_string(&example_path).expect("read MSL example");
        let library_key = canonical_path_key(msl_root.to_string_lossy().as_ref());

        let probe_line = source
            .lines()
            .position(|line| line.contains("Modelica.Electrical.Analog.Basic.Ground"))
            .expect("probe line should exist") as u32;
        let probe_col = source
            .lines()
            .nth(probe_line as usize)
            .and_then(|line| line.find("Modelica."))
            .map(|offset| offset as u32 + "Modelica.".len() as u32)
            .expect("probe column should exist");

        let service = new_test_service();
        let server = service.inner();
        *server.library_paths.write().await = vec![msl_root.to_string_lossy().to_string()];

        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: example_uri.clone(),
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: source,
                },
            })
            .await;

        let completion = server
            .completion(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: example_uri },
                    position: Position {
                        line: probe_line,
                        character: probe_col,
                    },
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .await
            .expect("completion request should succeed");

        let items = match completion {
            Some(CompletionResponse::Array(items)) => items,
            other => panic!("expected array completion response, got {other:?}"),
        };
        let labels = items
            .iter()
            .map(|item| item.label.clone())
            .collect::<Vec<_>>();

        assert!(
            server.loaded_libraries.read().await.contains(&library_key),
            "completion should load cached MSL library root"
        );
        assert!(
            labels.iter().any(|label| label == "Electrical"),
            "expected MSL completion items to include Electrical, got: {labels:?}"
        );
    });
}

#[test]
fn code_lens_request_stays_parse_only_until_resolved() {
    run_async_test(async {
        let temp = new_temp_dir("code-lens-parse-only");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &active_key,
                r#"
                    model Root
                      Real x(start=0);
                    equation
                      der(x) = 1;
                    end Root;
                    "#,
            );
        }

        let lenses = server
            .code_lens(CodeLensParams {
                text_document: TextDocumentIdentifier {
                    uri: active_uri.clone(),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("code lens request should succeed")
            .expect("code lens should return the model lens");
        assert_eq!(lenses.len(), 1, "expected one model code lens");
        assert!(
            lenses[0].command.is_none(),
            "initial code lens response should be unresolved"
        );
        assert!(
            lenses[0].data.is_some(),
            "initial code lens response should carry resolve data"
        );
        assert!(
            !server.session.read().await.has_resolved_cached(),
            "code lens list request should stay parse-only"
        );

        let resolved = server
            .code_lens_resolve(lenses[0].clone())
            .await
            .expect("code lens resolve should succeed");
        let title = resolved
            .command
            .as_ref()
            .map(|command| command.title.clone())
            .expect("resolved code lens should supply a title");
        assert!(
            title.starts_with("Balanced"),
            "resolved code lens should reflect the strict compile result: {title}"
        );
        assert!(
            server.session.read().await.has_resolved_cached(),
            "code lens resolve should build resolved state on a cold request"
        );
        assert!(
            !server.session.read().await.has_standard_resolved_cached(),
            "code lens resolve should avoid the standard resolved session"
        );
    });
}

#[test]
fn code_lens_resolve_skips_when_request_becomes_stale() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        let temp = new_temp_dir("code-lens-stale-resolve");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session.update_document(
                &active_key,
                r#"
                    model Root
                      Real x(start=0);
                    equation
                      der(x) = 1;
                    end Root;
                    "#,
            );
        }

        let lenses = server
            .code_lens(CodeLensParams {
                text_document: TextDocumentIdentifier {
                    uri: active_uri.clone(),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("code lens request should succeed")
            .expect("code lens should return the model lens");
        let lens = lenses
            .into_iter()
            .next()
            .expect("code lens should return one model code lens");

        reset_session_cache_stats();
        let before = session_cache_stats();
        let strict_guard = server.work_lanes.strict.lock().await;
        let resolve_task = tokio::spawn({
            let server = server.clone();
            let lens = lens.clone();
            async move { server.code_lens_resolve(lens).await }
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
                    text: r#"
                    model Root
                      Real x(start=1);
                    equation
                      der(x) = 1;
                    end Root;
                    "#
                    .to_string(),
                }],
            })
            .await;
        drop(strict_guard);

        let resolved = resolve_task
            .await
            .expect("code lens resolve task should finish")
            .expect("code lens resolve should succeed");
        assert_eq!(
            resolved.command, lens.command,
            "stale code lens resolve should keep response unchanged"
        );
        assert_eq!(
            resolved.data, lens.data,
            "stale code lens resolve should keep existing data"
        );

        let delta = session_cache_stats().delta_since(before);
        assert_eq!(
            delta.strict_resolved_builds, 0,
            "stale code lens resolve should skip strict compile"
        );
    });
}

#[test]
fn code_lens_defers_when_required_libraries_are_unloaded() {
    run_async_test(async {
        let temp = new_temp_dir("code-lens-library-defer");
        let library_path = write_test_library(&temp, "Lib");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_source = "model Active\n  Lib.A a;\nend Active;\n";

        let service = new_test_service();
        let server = service.inner();
        *server.library_paths.write().await = vec![library_path.to_string_lossy().to_string()];
        {
            let mut session = server.session.write().await;
            session.update_document(&active_path.to_string_lossy(), active_source);
        }

        let response = server
            .code_lens(CodeLensParams {
                text_document: TextDocumentIdentifier { uri: active_uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("code lens request should succeed");

        assert!(
            response.is_none(),
            "code lens should defer until required libraries are loaded"
        );
        assert!(
            server.loaded_libraries.read().await.is_empty(),
            "code lens should not synchronously load libraries"
        );
    });
}

#[test]
fn code_lens_ignores_unrelated_library_resolve_errors() {
    let _guard = session_stats_test_guard();
    run_async_test(async {
        reset_session_cache_stats();
        let temp = new_temp_dir("code-lens-strict-closure");
        let active_path = temp.join("root.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);
        let dep_key = temp.join("good_dep.mo").to_string_lossy().to_string();
        let broken_key = temp.join("broken.mo").to_string_lossy().to_string();
        let lib_key = temp.join("lib.mo").to_string_lossy().to_string();

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session
                .add_document(
                    &dep_key,
                    r#"
                    within Lib;
                    model GoodDep
                      Real x(start=0);
                    equation
                      der(x) = 1;
                    end GoodDep;
                    "#,
                )
                .expect("good dependency should parse");
            session
                .add_document(
                    &broken_key,
                    r#"
                    connector Bus
                    end Bus;

                    block BusTranscription
                      Bus stackBus;
                    end BusTranscription;
                    "#,
                )
                .expect("broken sibling should parse");
            session
                .add_document(&lib_key, "package Lib\nend Lib;\n")
                .expect("library package should parse");
            session
                .add_document(
                    &active_key,
                    r#"
                    model Root
                      Lib.GoodDep dep;
                    end Root;
                    "#,
                )
                .expect("root should parse");
        }

        let response = server
            .code_lens(CodeLensParams {
                text_document: TextDocumentIdentifier { uri: active_uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("code lens request should succeed")
            .expect("root model should produce a code lens");
        assert!(
            response.iter().all(|lens| lens.command.is_none()),
            "code lens request should return unresolved items"
        );
        let before_resolve = session_cache_stats();
        let resolved = server
            .code_lens_resolve(
                response
                    .into_iter()
                    .next()
                    .expect("root model should produce one unresolved code lens"),
            )
            .await
            .expect("code lens resolve should succeed");
        let resolve_delta = session_cache_stats().delta_since(before_resolve);

        let titles = vec![
            resolved
                .command
                .map(|command| command.title)
                .expect("resolved code lens should supply a title"),
        ];
        assert!(
            titles.iter().any(|title| title.starts_with("Balanced")),
            "strict code lens should stay focused on the requested model: {titles:?}"
        );
        assert!(
            titles.iter().all(|title| !title.contains("stackBus")),
            "unrelated library resolve errors must not leak into code lens titles: {titles:?}"
        );
        assert!(
            resolve_delta.strict_resolved_builds >= 1,
            "code lens resolve should build strict resolved state when needed"
        );
    });
}

#[test]
fn collect_simulation_parsed_docs_snapshot_keeps_focus_and_libraries_only() {
    let focus_uri = "focus.mo";
    let other_uri = "other.mo";
    let library_uri = "/opt/msl/Modelica/package.mo";
    let source = "model M end M;";

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(focus_uri, source)
        .expect("focus should parse");
    session
        .add_document(other_uri, source)
        .expect("other should parse");
    session.replace_parsed_source_set(
        "library::/opt/msl",
        SourceRootKind::DurableLibrary,
        vec![(library_uri.to_string(), ast::StoredDefinition::default())],
        None,
    );

    let snapshot = session.snapshot();
    let focus_key = canonical_path_key(focus_uri);
    let docs = collect_simulation_parsed_docs_snapshot(&snapshot, focus_uri, &focus_key)
        .expect("docs should build");
    let uris: HashSet<String> = docs.into_iter().map(|(uri, _)| uri).collect();

    assert!(uris.contains(focus_uri), "focus doc must be included");
    assert!(uris.contains(library_uri), "library docs must be included");
    assert!(
        !uris.contains(other_uri),
        "non-focus workspace docs must be excluded"
    );
}

#[test]
fn collect_local_compile_unit_sources_loads_same_directory_siblings() {
    let temp = new_temp_dir("local-compile-unit");
    let focus = temp.join("Root.mo");
    let sibling = temp.join("Helper.mo");
    std::fs::write(&focus, "model Root\n  Helper h;\nend Root;\n").expect("write focus");
    std::fs::write(
        &sibling,
        "model Helper\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Helper;\n",
    )
    .expect("write sibling");

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            &focus.to_string_lossy(),
            &std::fs::read_to_string(&focus).expect("read"),
        )
        .expect("focus should parse");

    let snapshot = session.snapshot();
    let docs = collect_local_compile_unit_sources_snapshot(&snapshot, &focus.to_string_lossy())
        .expect("local compile unit docs should load");
    let uris: HashSet<String> = docs.into_iter().map(|(uri, _)| uri).collect();

    assert!(
        uris.contains(&focus.to_string_lossy().to_string()),
        "focus document must be included"
    );
    assert!(
        uris.contains(&sibling.to_string_lossy().to_string()),
        "same-directory sibling must be included"
    );
}

#[test]
fn collect_local_compile_unit_sources_keep_unrelated_syntax_errors_as_sources() {
    let temp = new_temp_dir("local-compile-unit-errors");
    let focus = temp.join("Root.mo");
    let sibling = temp.join("Helper.mo");
    let broken = temp.join("Broken.mo");
    std::fs::write(&focus, "model Root\n  Helper h;\nend Root;\n").expect("write focus");
    std::fs::write(
        &sibling,
        "model Helper\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Helper;\n",
    )
    .expect("write sibling");
    std::fs::write(&broken, "model Broken\n  Real x\nend Broken;\n").expect("write broken");

    let mut session = Session::new(SessionConfig::default());
    session.update_document(
        &focus.to_string_lossy(),
        &std::fs::read_to_string(&focus).expect("read"),
    );

    let snapshot = session.snapshot();
    let docs = collect_local_compile_unit_sources_snapshot(&snapshot, &focus.to_string_lossy())
        .expect("local compile unit sources should load");
    let uris: HashSet<String> = docs.into_iter().map(|(uri, _)| uri).collect();

    assert!(uris.contains(&broken.to_string_lossy().to_string()));
}

#[test]
fn compile_model_for_simulation_loads_same_directory_siblings_from_disk() {
    run_async_test(async {
        let temp = new_temp_dir("compile-siblings");
        let focus = temp.join("Root.mo");
        let sibling = temp.join("Helper.mo");
        std::fs::write(&focus, "model Root\n  Helper h;\nend Root;\n").expect("write focus");
        std::fs::write(
            &sibling,
            "model Helper\n  Real x(start=0);\nequation\n  der(x) = 1;\nend Helper;\n",
        )
        .expect("write sibling");

        let service = new_test_service();
        let server = service.inner();
        {
            let mut session = server.session.write().await;
            session
                .add_document(
                    &focus.to_string_lossy(),
                    &std::fs::read_to_string(&focus).expect("read focus"),
                )
                .expect("focus should parse");
        }

        let compiled = server
            .compile_model_for_simulation("Root", &focus.to_string_lossy())
            .await
            .expect("compile with sibling file should succeed");
        assert_eq!(compiled.dae.states.len(), 1);
    });
}

#[test]
fn package_layout_errors_map_to_plain_lsp_diagnostics() {
    let temp = new_temp_dir("package-layout-diag");
    let lib = temp.join("Modelica");
    std::fs::create_dir_all(&lib).expect("mkdir");
    std::fs::write(lib.join("package.mo"), "package Modelica end Modelica;")
        .expect("write package");
    std::fs::write(lib.join("A.mo"), "model A end A;").expect("write child");

    let err = parse_library_with_cache(&lib).expect_err("missing within must fail");
    let layout = err
        .downcast_ref::<PackageLayoutError>()
        .expect("package layout error type must be preserved");
    let diagnostics = library_load_diagnostics_for_package_layout_error(layout);
    let file_key = canonical_path_key(&lib.join("A.mo").to_string_lossy());
    let file_diagnostics = diagnostics
        .get(&file_key)
        .expect("source-backed package-layout diagnostic should be keyed by file");
    assert!(
        file_diagnostics
            .iter()
            .any(|diag| diag.code.as_ref() == Some(&NumberOrString::String("PKG-009".to_string()))),
        "expected PKG-009 diagnostic for child file: {file_diagnostics:?}"
    );
    assert!(
        file_diagnostics
            .iter()
            .all(|diag| diag.source.as_deref() == Some("rumoca")),
        "expected standard LSP diagnostics, not rendered miette output: {file_diagnostics:?}"
    );
}

#[test]
fn package_layout_library_load_warning_is_concise_when_file_diagnostics_exist() {
    let temp = new_temp_dir("package-layout-warning");
    let lib = temp.join("Modelica");
    std::fs::create_dir_all(&lib).expect("mkdir");
    std::fs::write(lib.join("package.mo"), "package Modelica end Modelica;")
        .expect("write package");
    std::fs::write(lib.join("A.mo"), "model A end A;").expect("write child");

    let err = parse_library_with_cache(&lib).expect_err("missing within must fail");
    let message = library_load_error_message(&lib.to_string_lossy(), &err);
    assert!(
        message.contains("see diagnostics"),
        "source-backed package-layout failures should defer details to diagnostics: {message}"
    );
    assert!(
        !message.contains("PKG-009"),
        "warning should not repeat the full diagnostic summary: {message}"
    );
}

#[test]
fn did_open_library_document_moves_live_text_into_session() {
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let library_root_path = std::env::temp_dir().join("rumoca-lsp-test-open");
        let library_path = library_root_path
            .join("Modelica")
            .join("Blocks")
            .join("Continuous.mo");
        let library_uri = canonical_path_key(library_path.to_string_lossy().as_ref());
        let text = "within Modelica.Blocks;\npackage Continuous\nend Continuous;\n";

        {
            let mut session = server.session.write().await;
            session.replace_parsed_source_set(
                "library::Modelica",
                SourceRootKind::Library,
                vec![(library_uri.clone(), ast::StoredDefinition::default())],
                None,
            );
        }
        let uri = Url::from_file_path(&library_path).expect("file uri");
        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri,
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            })
            .await;

        let session_doc = server
            .session
            .read()
            .await
            .get_document(&library_uri)
            .cloned()
            .expect("opened library document must be owned by the session");
        assert_eq!(
            session_doc.content, text,
            "session should track the live library document text directly"
        );
    });
}

#[test]
fn did_change_ignores_stale_document_versions() {
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let path = new_temp_dir("stale-version").join("active.mo");
        let uri = Url::from_file_path(&path).expect("file uri");
        let uri_path = session_document_uri_key(&uri);
        let fresh = "model Fresh\nend Fresh;\n";
        let stale = "model Stale\nend Stale;\n";

        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "modelica".to_string(),
                    version: 2,
                    text: fresh.to_string(),
                },
            })
            .await;

        server
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier { uri, version: 1 },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: stale.to_string(),
                }],
            })
            .await;

        let doc = server
            .session
            .read()
            .await
            .get_document(&uri_path)
            .cloned()
            .expect("document should remain tracked");
        assert_eq!(
            doc.content, fresh,
            "older document versions must not overwrite newer session state"
        );
    });
}

#[test]
fn did_close_library_document_restores_cached_session_document() {
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let library_root_path = std::env::temp_dir().join("rumoca-lsp-test-close");
        let library_path = library_root_path
            .join("Modelica")
            .join("Blocks")
            .join("Continuous.mo");
        let library_uri = canonical_path_key(library_path.to_string_lossy().as_ref());

        {
            let mut session = server.session.write().await;
            session.replace_parsed_source_set(
                "library::Modelica",
                SourceRootKind::Library,
                vec![(library_uri.clone(), ast::StoredDefinition::default())],
                None,
            );
        }
        let uri = Url::from_file_path(&library_path).expect("file uri");
        server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: "within Modelica.Blocks;".to_string(),
                },
            })
            .await;
        server
            .did_close(DidCloseTextDocumentParams {
                text_document: TextDocumentIdentifier { uri },
            })
            .await;

        let session_doc = server
            .session
            .read()
            .await
            .get_document(&library_uri)
            .cloned();
        assert!(
            session_doc.is_some(),
            "closing a live library document should restore the cached library document"
        );
        assert!(
            session_doc.is_some_and(|doc| doc.content.is_empty()),
            "restored library document should return to source-root ownership"
        );
        assert!(
            server.namespace_prewarm_state.read().await.is_none(),
            "closing a live library document should not enqueue background namespace prewarm"
        );
    });
}
