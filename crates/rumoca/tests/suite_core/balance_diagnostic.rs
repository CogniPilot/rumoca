use rumoca_compile::compile::{FailedPhase, Session, SessionConfig};

#[test]
fn unbalanced_model_is_rejected_at_todae_with_exact_counts() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "unbalanced.mo",
            "model U Real x; Real y; equation x=1; end U;",
        )
        .expect("fixture parses");

    let failure = session
        .compile_model_dae_strict_reachable_uncached_with_recovery_detailed("U")
        .expect_err("unbalanced model cannot inhabit production DAE");

    assert_eq!(failure.phase, Some(FailedPhase::ToDae));
    assert_eq!(failure.error_code.as_deref(), Some("ED001"));
    let detail = failure
        .balance_detail
        .expect("balance rejection carries exact phase evidence");
    assert_eq!(detail.equations(), 1);
    assert_eq!(detail.unknowns(), 2);
    assert_eq!(detail.balance(), -1);
}

#[test]
fn balanced_model_carries_the_same_constructor_evidence() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("balanced.mo", "model B Real x; equation x=1; end B;")
        .expect("fixture parses");
    let result = session.compile_model("B").expect("balanced model compiles");

    assert!(result.balance_detail.is_balanced());
    assert_eq!(result.balance_detail.equations_unknowns(), (1, 1));
}
