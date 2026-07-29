use rumoca_compile::compile::{FailedPhase, Session, SessionConfig};

#[test]
fn connector_model_balance_is_constructor_evidence() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "connector.mo",
            r#"
connector Pin
  Real v;
  flow Real i;
end Pin;

model Source
  Pin pin;
equation
  pin.v = 1;
end Source;

model OpenCircuit
  Pin pin;
equation
  pin.i = 0;
end OpenCircuit;

model ConnectedSystem
  Source source;
  OpenCircuit load;
equation
  connect(source.pin, load.pin);
end ConnectedSystem;
"#,
        )
        .expect("fixture parses");
    let result = session
        .compile_model("ConnectedSystem")
        .expect("connector model compiles");
    assert!(result.balance_detail.is_balanced());
    assert_eq!(result.balance_detail.equations_unknowns(), (4, 4));
}

#[test]
fn redundant_boundary_flow_equations_fail_at_dae_construction() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "overdetermined_connector.mo",
            r#"
connector Pin
  Real v;
  flow Real i;
end Pin;

model Pair
  Pin a;
  Pin b;
equation
  connect(a, b);
  a.v = 1;
  a.i = 0;
end Pair;
"#,
        )
        .expect("fixture parses");

    let failure = session
        .compile_model_dae_strict_reachable_uncached_with_recovery_detailed("Pair")
        .expect_err("redundant boundary closures cannot inhabit the checked DAE");

    assert_eq!(failure.phase, Some(FailedPhase::ToDae));
    assert_eq!(failure.error_code.as_deref(), Some("ED001"));
    let detail = failure
        .balance_detail
        .expect("balance failure carries constructor evidence");
    assert_eq!(detail.equations_unknowns(), (6, 4));
    assert_eq!(detail.balance(), 2);
}
