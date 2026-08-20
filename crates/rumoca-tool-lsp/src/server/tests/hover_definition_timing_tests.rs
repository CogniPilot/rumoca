//! Hover and go-to-definition timing: both navigation surfaces must resolve
//! from the query index or the parsed source root without building a resolved
//! tree, and the logged summary must record which fast path answered.

use super::*;
use std::sync::atomic::Ordering;

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
fn hover_timing_summary_reports_parsed_source_root_fast_path_for_qualified_type_path() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("hover-qualified-path-fast");
    let timing_path = temp.join("navigation-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let source_root_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_cross_file_qualified_path_document(
            server,
            &source_root_path,
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
            "qualified type-path hover should resolve the source-root target"
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
        let source_root_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_cross_file_alias_navigation_document(
            server,
            &source_root_path,
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
        let source_root_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_cross_file_alias_navigation_document(
            server,
            &source_root_path,
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
                Url::from_file_path(&source_root_path).expect("file uri"),
                "goto-definition should jump to the source-root file"
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
fn goto_definition_timing_summary_reports_parsed_source_root_fast_path_for_qualified_type_path() {
    let _guard = session_stats_test_guard();
    let temp = new_temp_dir("definition-qualified-path-fast");
    let timing_path = temp.join("navigation-timings.jsonl");

    run_async_test(async {
        reset_session_cache_stats();
        let source_root_path = temp.join("lib.mo");
        let active_path = temp.join("active.mo");
        let active_uri = Url::from_file_path(&active_path).expect("file uri");
        let service = new_test_service();
        let server = service.inner();
        seed_cross_file_qualified_path_document(
            server,
            &source_root_path,
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
            Url::from_file_path(&source_root_path).expect("source-root uri"),
            "qualified type-path goto-definition should jump to the source-root file"
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
