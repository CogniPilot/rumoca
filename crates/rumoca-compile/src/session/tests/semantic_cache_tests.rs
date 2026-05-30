use super::*;

#[test]
fn compile_model_diagnostics_reuses_semantic_closure_cache() {
    let source = r#"model M
  Real x(start=0);
equation
  der(x) = 1;
end M;
"#;

    let mut session = Session::default();
    session
        .add_document("test.mo", source)
        .expect("document should parse");

    let first = session.compile_model_diagnostics("M");
    assert!(
        first.diagnostics.is_empty(),
        "test model should be clean before cache mutation"
    );
    let cached = model_stage_semantic_diagnostics_artifact_mut(
        &mut session,
        "M",
        SemanticDiagnosticsMode::Standard,
    );
    cached
        .diagnostics
        .diagnostics
        .push(CommonDiagnostic::warning(
            "ETEST",
            "cached semantic diagnostics reused",
            PrimaryLabel::new(Span::DUMMY).with_message("cache sentinel"),
        ));

    let second = session.compile_model_diagnostics("M");
    assert!(
        second
            .diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("ETEST")),
        "second diagnostics request must reuse the cached artifact"
    );
}

#[test]
fn semantic_diagnostics_cache_invalidates_after_document_edit() {
    let source = r#"model M
  Real x(start=0);
equation
  der(x) = 1;
end M;
"#;
    let updated = r#"model M
  Real x(start=0);
equation
  der(x) = 2;
end M;
"#;

    let mut session = Session::default();
    session
        .add_document("test.mo", source)
        .expect("document should parse");

    let first = session.compile_model_diagnostics("M");
    assert!(
        first.diagnostics.is_empty(),
        "test model should be clean before cache mutation"
    );
    let cached = model_stage_semantic_diagnostics_artifact_mut(
        &mut session,
        "M",
        SemanticDiagnosticsMode::Standard,
    );
    cached
        .diagnostics
        .diagnostics
        .push(CommonDiagnostic::warning(
            "ETEST",
            "cached semantic diagnostics reused",
            PrimaryLabel::new(Span::DUMMY).with_message("cache sentinel"),
        ));

    let parse_err = session.update_document("test.mo", updated);
    assert!(parse_err.is_none(), "edited document should remain valid");

    let second = session.compile_model_diagnostics("M");
    assert!(
        second
            .diagnostics
            .iter()
            .all(|diag| diag.code.as_deref() != Some("ETEST")),
        "document edits must rebuild diagnostics instead of reusing stale cache"
    );
}

#[test]
fn semantic_diagnostics_cache_survives_unrelated_edits_but_rebuilds_after_dependency_changes() {
    let base_v1 = r#"model Base
  Real y(start=0);
equation
  der(y) = 1;
end Base;
"#;
    let base_v2 = r#"model Base
  Real y(start=0);
equation
  der(y) = 3;
end Base;
"#;
    let child = r#"model Child
  Base base;
  Real x(start=0);
equation
  der(x) = base.y;
end Child;
"#;
    let other_v1 = "model Other\n  Real z(start=0);\nequation\n  der(z) = 0;\nend Other;\n";
    let other_v2 = "model Other\n  Real z(start=0);\nequation\n  der(z) = 4;\nend Other;\n";

    let mut session = Session::default();
    session
        .add_document("base.mo", base_v1)
        .expect("Base should parse");
    session
        .add_document("child.mo", child)
        .expect("Child should parse");
    session
        .add_document("other.mo", other_v1)
        .expect("Other should parse");

    let first = session.compile_model_diagnostics("Child");
    assert!(
        first.diagnostics.is_empty(),
        "Child should be clean before cache mutation"
    );
    model_stage_semantic_diagnostics_artifact_mut(
        &mut session,
        "Child",
        SemanticDiagnosticsMode::Standard,
    )
    .diagnostics
    .diagnostics
    .push(CommonDiagnostic::warning(
        "ETEST",
        "cached semantic diagnostics reused",
        PrimaryLabel::new(Span::DUMMY).with_message("cache sentinel"),
    ));

    session.update_document("other.mo", other_v2);
    let second = session.compile_model_diagnostics("Child");
    assert!(
        second
            .diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("ETEST")),
        "unrelated edits should keep the cached semantic diagnostics artifact"
    );

    session.update_document("base.mo", base_v2);
    let third = session.compile_model_diagnostics("Child");
    assert!(
        third
            .diagnostics
            .iter()
            .all(|diag| diag.code.as_deref() != Some("ETEST")),
        "dependency edits must rebuild semantic diagnostics instead of reusing stale cache"
    );
}

pub(super) fn set_child_compile_cache_marker(session: &mut Session, marker: String) {
    session
        .query_state
        .dae
        .compile_results
        .get_mut("Child")
        .expect("Child should have a compile cache entry")
        .result = CachedCompileResult::Full(PhaseResult::NeedsInner {
        missing_inners: vec![marker],
        missing_spans: Vec::new(),
    });
}

pub(super) fn expect_cached_child_compile(session: &mut Session, marker: &str) {
    match session
        .compile_model_phases("Child")
        .expect("Child should compile after unrelated edit")
    {
        PhaseResult::NeedsInner { missing_inners, .. } => {
            assert_eq!(missing_inners, vec![marker.to_string()]);
        }
        other => panic!("expected cached Child compile marker, got {other:?}"),
    }
}

pub(super) fn set_child_navigation_cache_sentinel(session: &mut Session) {
    session
        .query_state
        .resolved
        .semantic_navigation
        .get_mut("Child")
        .expect("Child semantic navigation should be cached")
        .resolved = Arc::new(ast::ResolvedTree::new(ast::ClassTree::new()));
}

pub(super) fn expect_warm_child_navigation(session: &mut Session) {
    set_child_navigation_cache_sentinel(session);
    let resolved = session
        .resolved_for_semantic_navigation("Child")
        .expect("Child semantic navigation should succeed");
    assert!(
        resolved.0.definitions.classes.is_empty(),
        "unrelated edits should not rebuild Child semantic navigation"
    );
}

pub(super) fn expect_cold_child_navigation(session: &mut Session) {
    let resolved = session
        .resolved_for_semantic_navigation("Child")
        .expect("Child semantic navigation should rebuild after dependency edit");
    assert!(
        resolved.0.get_class_by_qualified_name("Child").is_some(),
        "rebuilt semantic navigation should still resolve Child"
    );
    assert!(
        !resolved.0.definitions.classes.is_empty(),
        "dependency edits should rebuild Child semantic navigation"
    );
}

pub(super) fn set_child_diagnostics_cache_sentinel(session: &mut Session) {
    let cached = model_stage_semantic_diagnostics_artifact_mut(
        session,
        "Child",
        SemanticDiagnosticsMode::Standard,
    );
    cached.diagnostics.diagnostics = vec![CommonDiagnostic::warning(
        "ETEST",
        "cached semantic diagnostics reused",
        PrimaryLabel::new(Span::DUMMY).with_message("cache sentinel"),
    )];
}

#[test]
fn lru_cache_helpers_bound_size_and_refresh_recent_entries() {
    let mut cache = IndexMap::new();
    insert_lru_cache_entry(&mut cache, "A".to_string(), 1_u8, 2);
    insert_lru_cache_entry(&mut cache, "B".to_string(), 2_u8, 2);

    assert_eq!(get_lru_cache_entry(&mut cache, "A"), Some(1));

    insert_lru_cache_entry(&mut cache, "C".to_string(), 3_u8, 2);

    assert!(
        cache.contains_key("A"),
        "recently touched entry should stay cached"
    );
    assert!(
        !cache.contains_key("B"),
        "least-recently-used entry should be evicted first"
    );
    assert!(cache.contains_key("C"), "new entry should be inserted");
}
