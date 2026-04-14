use super::*;

#[test]
fn test_wasm_lsp_completion_keeps_local_members_on_ast_fast_path() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    reset_session_cache_stats();

    let source = r#"
    model Plane
      Real x, y, theta;
    equation
      der(x) = cos(theta);
      der(y) = sin(theta);
      der(theta) = 1;
    end Plane;

    model Sim
      Plane p1, p2;
    equation
      p1.x = 1;
    end Sim;
    "#;

    let before = session_cache_stats();
    let first_json = lsp_completion(source, 12, "      p1.".len() as u32).expect("cold completion");
    let after_first = session_cache_stats();
    let first_delta = after_first.delta_since(before);
    let first_items: Vec<serde_json::Value> =
        serde_json::from_str(&first_json).expect("completion payload should be valid JSON");
    assert!(
        first_items
            .iter()
            .any(|item| item.get("label").and_then(|v| v.as_str()) == Some("x")),
        "expected member completion for x, got: {first_items:?}"
    );
    assert_eq!(
        first_delta.semantic_navigation_builds, 0,
        "local member completion should stay off semantic navigation"
    );
    assert!(
        !singleton_session_has_standard_resolved_cached(),
        "completion should avoid populating the standard resolved session"
    );

    let second_json =
        lsp_completion(source, 12, "      p1.".len() as u32).expect("warm completion");
    let second_delta = session_cache_stats().delta_since(after_first);
    let second_items: Vec<serde_json::Value> =
        serde_json::from_str(&second_json).expect("completion payload should be valid JSON");
    assert_eq!(
        first_items, second_items,
        "warm completion should preserve completion results"
    );
    assert_eq!(
        second_delta.semantic_navigation_builds, 0,
        "warm completion should keep using the AST fast path"
    );
    assert!(
        !singleton_session_has_standard_resolved_cached(),
        "warm completion should still avoid the standard resolved session"
    );

    clear_source_root_cache();
}

#[test]
fn test_wasm_lsp_diagnostics_reuse_semantic_diagnostics_cache() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    reset_session_cache_stats();

    let source = r#"
    model M
      Real x(start=0);
    equation
      der(x) = -x;
    end M;
    "#;

    let before = session_cache_stats();
    let first_json = lsp_diagnostics(source).expect("cold diagnostics");
    let after_first = session_cache_stats();
    let first_delta = after_first.delta_since(before);
    let first_diagnostics: Vec<serde_json::Value> =
        serde_json::from_str(&first_json).expect("diagnostics payload should be valid JSON");
    assert!(
        first_diagnostics.is_empty(),
        "expected clean diagnostics for valid model, got: {first_diagnostics:?}"
    );
    assert!(
        first_delta.interface_semantic_diagnostics_builds >= 1,
        "cold diagnostics should build interface-stage diagnostics artifacts"
    );
    assert!(
        first_delta.body_semantic_diagnostics_builds >= 1,
        "cold diagnostics should build body-stage diagnostics artifacts"
    );
    assert!(
        first_delta.model_stage_semantic_diagnostics_builds >= 1,
        "cold diagnostics should build model-stage diagnostics artifacts"
    );

    let second_json = lsp_diagnostics(source).expect("warm diagnostics");
    let second_delta = session_cache_stats().delta_since(after_first);
    let second_diagnostics: Vec<serde_json::Value> =
        serde_json::from_str(&second_json).expect("diagnostics payload should be valid JSON");
    assert_eq!(
        first_diagnostics, second_diagnostics,
        "warm diagnostics should preserve the diagnostics payload"
    );
    assert_eq!(
        second_delta.interface_semantic_diagnostics_builds, 0,
        "warm diagnostics should not rebuild interface-stage diagnostics artifacts"
    );
    assert_eq!(
        second_delta.body_semantic_diagnostics_builds, 0,
        "warm diagnostics should not rebuild body-stage diagnostics artifacts"
    );
    assert_eq!(
        second_delta.model_stage_semantic_diagnostics_builds, 0,
        "warm diagnostics should not rebuild model-stage diagnostics artifacts"
    );
    assert!(
        second_delta.interface_semantic_diagnostics_cache_hits >= 1,
        "warm diagnostics should reuse the interface-stage diagnostics cache"
    );
    assert!(
        second_delta.body_semantic_diagnostics_cache_hits >= 1,
        "warm diagnostics should reuse the body-stage diagnostics cache"
    );
    assert!(
        second_delta.model_stage_semantic_diagnostics_cache_hits >= 1,
        "warm diagnostics should reuse the model-stage diagnostics cache"
    );

    clear_source_root_cache();
}
