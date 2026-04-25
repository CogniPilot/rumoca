use super::*;

fn assert_typecheck_failure(result: PhaseResult, expected_code: &str) {
    match result {
        PhaseResult::Failed {
            phase, error_code, ..
        } => {
            assert_eq!(phase, FailedPhase::Typecheck);
            assert_eq!(error_code.as_deref(), Some(expected_code));
        }
        other => panic!("expected typecheck failure, got {other:?}"),
    }
}

#[test]
fn typed_model_query_cache_is_reused_by_compile_and_diagnostics() {
    let mut session = Session::default();
    session
        .add_document(
            "target.mo",
            "model Target\n  Real x(startd = 1.0);\nequation\n  der(x) = -x;\nend Target;\n",
        )
        .expect("target should parse");
    session
        .add_document("other.mo", "model Other\n  Real y;\nend Other;\n")
        .expect("other should parse");

    let first = session
        .compile_model_phases("Target")
        .expect("compile should return phase result");
    assert_typecheck_failure(first, "ET001");

    let cache_key = standard_typed_cache_key(&mut session, "Target");
    match session
        .query_state
        .flat
        .typed_models
        .artifacts
        .get(&cache_key)
        .expect("typed query should be cached")
        .outcome
        .clone()
    {
        TypedModelOutcome::TypecheckError(diags) => {
            assert!(
                diags
                    .iter()
                    .any(|diag| diag.code.as_deref() == Some("ET001")),
                "initial typed artifact should preserve the real typecheck failure"
            );
        }
        other => panic!("expected cached typecheck error, got {other:?}"),
    }

    session
        .query_state
        .flat
        .typed_models
        .artifacts
        .get_mut(&cache_key)
        .expect("typed query should be cached")
        .outcome = TypedModelOutcome::TypecheckError(vec![CommonDiagnostic::error(
        "TTEST",
        "typed cache sentinel",
        PrimaryLabel::new(Span::DUMMY).with_message("cache sentinel"),
    )]);

    let parse_error = session.update_document(
        "other.mo",
        "model Other\n  Real y;\n  Real z;\nend Other;\n",
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
    assert_typecheck_failure(warm_compile, "TTEST");

    let warm_diagnostics = session.compile_model_diagnostics("Target");
    assert_diagnostics_have_code(&warm_diagnostics, "TTEST");

    let parse_error = session.update_document(
        "target.mo",
        "model Target\n  Real x(start = 1.0);\nequation\n  der(x) = -x;\nend Target;\n",
    );
    assert!(parse_error.is_none(), "target edit should remain valid");

    let rebuilt = session
        .compile_model_phases("Target")
        .expect("rebuild should return phase result");
    assert!(
        matches!(rebuilt, PhaseResult::Success(_)),
        "target edit should rebuild the typed model instead of reusing the stale typecheck failure"
    );

    let rebuilt_diagnostics = session.compile_model_diagnostics("Target");
    assert_diagnostics_lack_code(&rebuilt_diagnostics, "TTEST");
}
