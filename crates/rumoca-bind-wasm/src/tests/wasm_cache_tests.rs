use super::*;

#[test]
fn interactive_compile_reuses_the_session_owned_result() {
    let mut session = Session::default();
    let source = "model M Real x(start=1); equation der(x)=-x; end M;";
    assert!(session.update_document("input.mo", source).is_none());
    let first = compile_cached_requested_model(&mut session, "M").unwrap();
    assert!(session.update_document("input.mo", source).is_none());
    let second = compile_cached_requested_model(&mut session, "M").unwrap();
    assert!(std::sync::Arc::ptr_eq(&first.dae, &second.dae));

    let uncached = compile_requested_model(&mut session, "M").unwrap();
    assert!(!std::sync::Arc::ptr_eq(&second.dae, &uncached.dae));
    assert_eq!(
        serde_json::to_value(first.dae.as_ref()).unwrap(),
        serde_json::to_value(uncached.dae.as_ref()).unwrap()
    );
}

#[test]
fn interactive_compile_invalidates_changed_and_invalid_dependencies() {
    let mut session = Session::default();
    let base = "model Base Real x(start=1); equation der(x)=-x; end Base;";
    assert!(session.update_document("base.mo", base).is_none());
    assert!(
        session
            .update_document("input.mo", "model M extends Base; end M;")
            .is_none()
    );
    let first = compile_cached_requested_model(&mut session, "M").unwrap();
    let changed = base.replace("-x", "-2*x");
    assert!(session.update_document("base.mo", &changed).is_none());
    let second = compile_cached_requested_model(&mut session, "M").unwrap();
    assert!(!std::sync::Arc::ptr_eq(&first.dae, &second.dae));
    assert_ne!(
        serde_json::to_value(first.dae.as_ref()).unwrap(),
        serde_json::to_value(second.dae.as_ref()).unwrap()
    );
    assert!(
        session
            .update_document("base.mo", "model Base Real")
            .is_some()
    );
    assert!(compile_cached_requested_model(&mut session, "M").is_err());
    assert!(session.update_document("base.mo", base).is_none());
    let restored = compile_cached_requested_model(&mut session, "M").unwrap();
    assert_eq!(
        serde_json::to_value(first.dae.as_ref()).unwrap(),
        serde_json::to_value(restored.dae.as_ref()).unwrap()
    );
}

#[test]
fn interactive_compile_keeps_model_selection_and_active_edits_distinct() {
    let mut session = Session::default();
    let source = "model A Real x; equation x=1; end A; model B Real x; equation x=2; end B;";
    assert!(session.update_document("input.mo", source).is_none());
    let first = compile_cached_requested_model(&mut session, "A").unwrap();
    let other = compile_cached_requested_model(&mut session, "B").unwrap();
    assert!(!std::sync::Arc::ptr_eq(&first.dae, &other.dae));
    let changed = source.replace("x=1", "x=3");
    assert!(session.update_document("input.mo", &changed).is_none());
    let edited = compile_cached_requested_model(&mut session, "A").unwrap();
    assert!(!std::sync::Arc::ptr_eq(&first.dae, &edited.dae));
    assert_ne!(
        serde_json::to_value(first.dae.as_ref()).unwrap(),
        serde_json::to_value(edited.dae.as_ref()).unwrap()
    );
    assert!(
        session
            .update_document("input.mo", "model A Real")
            .is_some()
    );
    assert!(compile_cached_requested_model(&mut session, "A").is_err());
}

#[test]
fn test_wasm_lsp_completion_keeps_local_members_on_ast_fast_path() {
    let _guard = session_test_guard();
    clear_source_root_cache().expect("clear source-root cache");
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
    assert_eq!(session_cache_stats().standard_resolved_builds, 0);

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
    assert_eq!(session_cache_stats().standard_resolved_builds, 0);

    clear_source_root_cache().expect("clear source-root cache");
}

#[test]
fn test_wasm_lsp_diagnostics_reuse_semantic_diagnostics_cache() {
    let _guard = session_test_guard();
    clear_source_root_cache().expect("clear source-root cache");
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

    clear_source_root_cache().expect("clear source-root cache");
}
