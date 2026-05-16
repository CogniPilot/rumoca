use super::*;

fn assert_todae_failure(result: PhaseResult, expected_code: &str) {
    match result {
        PhaseResult::Failed {
            phase, error_code, ..
        } => {
            assert_eq!(phase, FailedPhase::ToDae);
            assert_eq!(error_code.as_deref(), Some(expected_code));
        }
        other => panic!("expected ToDae failure, got {other:?}"),
    }
}

#[test]
fn dae_model_query_cache_is_reused_by_compile_and_diagnostics() {
    let mut session = Session::default();
    session
        .add_document(
            "target.mo",
            "model Target\n  Real x(start = 1.0);\nequation\n  der(x) = -x;\nend Target;\n",
        )
        .expect("target should parse");
    session
        .add_document("other.mo", "model Other\n  Real y;\nend Other;\n")
        .expect("other should parse");

    let first = session
        .compile_model_phases("Target")
        .expect("compile should return phase result");
    assert!(
        matches!(first, PhaseResult::Success(_)),
        "target should compile before cache mutation"
    );

    let cache_key = standard_dae_cache_key(&mut session, "Target");
    match session
        .query_state
        .dae
        .dae_models
        .get(&cache_key)
        .expect("dae query should be cached")
        .outcome
        .clone()
    {
        DaeModelOutcome::Success(_) => {}
        other => panic!("expected cached dae-model success, got {other:?}"),
    }

    session
        .query_state
        .dae
        .dae_models
        .get_mut(&cache_key)
        .expect("dae query should be cached")
        .outcome = DaeModelOutcome::ToDaeError {
        error: Box::new(ToDaeError::internal("dae cache sentinel")),
    };

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

    let warm_compile = session
        .compile_model_phases("Target")
        .expect("warm compile should return phase result");
    assert_todae_failure(warm_compile, "rumoca::todae::ED003");

    let warm_diagnostics = session.compile_model_diagnostics("Target");
    assert_diagnostics_have_code(&warm_diagnostics, "rumoca::todae::ED003");

    let parse_error = session.update_document(
        "target.mo",
        "model Target\n  Real x(start = 1.0);\nequation\n  der(x) = -2 * x;\nend Target;\n",
    );
    assert!(parse_error.is_none(), "target edit should remain valid");

    let rebuilt = session
        .compile_model_phases("Target")
        .expect("rebuild should return phase result");
    assert!(
        matches!(rebuilt, PhaseResult::Success(_)),
        "target edit should rebuild the dae model instead of reusing the stale ToDae failure"
    );

    let rebuilt_diagnostics = session.compile_model_diagnostics("Target");
    assert_diagnostics_lack_code(&rebuilt_diagnostics, "rumoca::todae::ED003");
}
