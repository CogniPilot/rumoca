use rumoca_compile::compile::{Session, SessionConfig};

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
    let result = session
        .compile_model("Pair")
        .expect("connector model compiles");
    assert!(result.balance_detail.is_balanced());
}
