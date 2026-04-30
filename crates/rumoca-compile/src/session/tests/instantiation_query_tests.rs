use super::*;

fn assert_needs_inner_result(result: PhaseResult, expected: &str) {
    match result {
        PhaseResult::NeedsInner { missing_inners } => {
            assert_eq!(missing_inners, vec![expected.to_string()]);
        }
        other => panic!("expected needs-inner result, got {other:?}"),
    }
}

fn assert_diagnostics_contain_message(diagnostics: &ModelDiagnostics, expected: &str) {
    assert!(
        diagnostics
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains(expected)),
        "expected diagnostic message containing `{expected}`"
    );
}

fn assert_diagnostics_do_not_contain_message(diagnostics: &ModelDiagnostics, unexpected: &str) {
    assert!(
        diagnostics
            .diagnostics
            .iter()
            .all(|diag| !diag.message.contains(unexpected)),
        "did not expect diagnostic message containing `{unexpected}`"
    );
}

#[test]
fn instantiated_model_query_cache_is_reused_by_compile_and_diagnostics() {
    let mut session = Session::default();
    session
        .add_document(
            "target.mo",
            "model Target\n  outer Real shared;\nend Target;\n",
        )
        .expect("target should parse");
    session
        .add_document("other.mo", "model Other\n  Real x;\nend Other;\n")
        .expect("other should parse");

    let first = session
        .compile_model_phases("Target")
        .expect("compile should return phase result");
    assert_needs_inner_result(first, "shared");

    let cache_key = standard_instantiation_cache_key(&mut session, "Target");
    match session
        .query_state
        .flat
        .instantiated_models
        .get(&cache_key)
        .expect("instantiation query should be cached")
        .outcome
        .clone()
    {
        InstantiatedModelOutcome::NeedsInner { .. } => {}
        other => panic!("expected cached needs-inner outcome, got {other:?}"),
    }

    session
        .query_state
        .flat
        .instantiated_models
        .get_mut(&cache_key)
        .expect("instantiation query should be cached")
        .outcome = InstantiatedModelOutcome::NeedsInner {
        missing_inners: vec!["sentinel".to_string()],
        missing_spans: vec![Span::DUMMY],
    };

    let parse_error = session.update_document(
        "other.mo",
        "model Other\n  Real x;\n  Real y;\nend Other;\n",
    );
    assert!(parse_error.is_none(), "unrelated edit should remain valid");

    session
        .query_state
        .dae
        .compile_results
        .shift_remove("Target");
    let dae_cache_key = standard_dae_cache_key(&mut session, "Target");
    session
        .query_state
        .dae
        .dae_models
        .shift_remove(&dae_cache_key);
    let typed_cache_key = standard_typed_cache_key(&mut session, "Target");
    session
        .query_state
        .flat
        .typed_models
        .artifacts
        .shift_remove(&typed_cache_key);
    let flat_cache_key = standard_flat_cache_key(&mut session, "Target");
    session
        .query_state
        .flat
        .flat_models
        .artifacts
        .shift_remove(&flat_cache_key);

    let warm_compile = session
        .compile_model_phases("Target")
        .expect("warm compile should return phase result");
    assert_needs_inner_result(warm_compile, "sentinel");

    let warm_diagnostics = session.compile_model_diagnostics("Target");
    assert_diagnostics_contain_message(&warm_diagnostics, "sentinel");

    let parse_error = session.update_document(
        "target.mo",
        "model Target\n  outer Real shared2;\nend Target;\n",
    );
    assert!(parse_error.is_none(), "target edit should remain valid");

    let rebuilt = session
        .compile_model_phases("Target")
        .expect("rebuild should return phase result");
    assert_needs_inner_result(rebuilt, "shared2");

    let rebuilt_diagnostics = session.compile_model_diagnostics("Target");
    assert_diagnostics_contain_message(&rebuilt_diagnostics, "shared2");
    assert_diagnostics_do_not_contain_message(&rebuilt_diagnostics, "sentinel");
}
