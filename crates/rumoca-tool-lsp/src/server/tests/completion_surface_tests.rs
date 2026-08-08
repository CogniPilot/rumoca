//! Completion surface: the logged timing summary must show completion staying
//! on the query/AST fast paths and reusing the namespace-completion cache, and
//! MSL completion must load durable libraries on demand.

use super::*;

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
        cold.session_cache_delta.namespace_completion_cache_misses >= 1,
        "cold namespace completion should miss the namespace completion cache"
    );
    assert!(
        warm.session_cache_delta.namespace_completion_cache_hits >= 1,
        "warm namespace completion should hit the namespace completion cache"
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
        let source_root_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);
        let service = new_test_service();
        let server = service.inner();
        *server.completion_timing_path.write().await = Some(timing_path.clone());
        {
            let mut session = server.session.write().await;
            session.update_document(
                &source_root_path.to_string_lossy(),
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
        "cold member completion should not build the source-root namespace cache"
    );
    assert_eq!(
        cold.session_cache_delta.namespace_completion_cache_misses, 0,
        "cold member completion should not miss the namespace completion cache"
    );
    assert_eq!(
        warm.session_cache_delta.semantic_navigation_builds, 0,
        "warm member completion should remain on the query fast path"
    );
    assert_eq!(
        warm.session_cache_delta.namespace_index_query_hits, 0,
        "warm member completion should not touch the source-root namespace cache"
    );
    assert_eq!(
        warm.session_cache_delta.namespace_completion_cache_hits, 0,
        "warm member completion should not use the namespace completion cache"
    );
}

#[test]
fn completion_timing_summary_reports_warm_source_root_namespace_cache_reuse() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("source-root-completion-timing");
    let timing_path = temp.join("completion-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let source_root_path = write_test_source_root(&temp, "Lib");
        let source_root_key = canonical_path_key(source_root_path.to_string_lossy().as_ref());
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let active_key = session_document_uri_key(&active_uri);
        let active_source = "model Active\n  Lib.\nend Active;\n";

        let service = new_test_service();
        let server = service.inner();
        *server.completion_timing_path.write().await = Some(timing_path.clone());
        *server.source_root_paths.write().await =
            vec![source_root_path.to_string_lossy().to_string()];
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
            server
                .session
                .read()
                .await
                .is_source_root_path_loaded(&source_root_key),
            "completion should load the referenced source root"
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
fn msl_completion_loads_libraries_on_demand() {
    run_async_test(async {
        let Some(msl_root) = cached_msl_source_root() else {
            eprintln!(
                "msl_completion_loads_libraries_on_demand: cached MSL not found under target/msl"
            );
            return;
        };
        let temp = new_temp_dir("msl-completion-load");
        let active_path = temp.join("active.mo");
        let source_root_key = canonical_path_key(msl_root.to_string_lossy().as_ref());

        let service = new_test_service();
        let server = service.inner();
        *server.source_root_paths.write().await = vec![msl_root.to_string_lossy().to_string()];
        server
            .ensure_completion_source_roots(
                "within Modelica.Electrical.Analog.Examples;\nmodel Resistor\n  Modelica.Electrical.Analog.Basic.Ground g;\nend Resistor;\n",
                Position {
                    line: 2,
                    character: "  Modelica.".len() as u32,
                },
                &active_path.to_string_lossy(),
            )
            .await;

        assert!(
            server
                .session
                .read()
                .await
                .is_source_root_path_loaded(&source_root_key),
            "cached MSL should be loaded on demand"
        );
    });
}

#[test]
fn msl_completion_on_open_source_root_example_returns_namespace_members() {
    run_async_test(async {
        let Some(msl_root) = cached_msl_source_root() else {
            eprintln!(
                "msl_completion_on_open_source_root_example_returns_namespace_members: cached MSL not found under target/msl"
            );
            return;
        };
        let example_path = msl_root.join("Electrical/Analog/Examples/Resistor.mo");
        let example_uri = Url::from_file_path(&example_path).expect("file uri");
        let source = std::fs::read_to_string(&example_path).expect("read MSL example");
        let source_root_key = canonical_path_key(msl_root.to_string_lossy().as_ref());

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
        *server.source_root_paths.write().await = vec![msl_root.to_string_lossy().to_string()];

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
            server
                .session
                .read()
                .await
                .is_source_root_path_loaded(&source_root_key),
            "completion should load cached MSL source root"
        );
        assert!(
            labels.iter().any(|label| label == "Electrical"),
            "expected MSL completion items to include Electrical, got: {labels:?}"
        );
    });
}
