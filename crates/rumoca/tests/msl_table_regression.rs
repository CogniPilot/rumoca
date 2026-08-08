use rumoca_compile::compile::{Session, SessionConfig};

#[test]
fn unsupported_external_table_semantics_are_rejected_early() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "table.mo",
            r#"
model TableUse
  Real y;
equation
  y = ModelicaStandardTables.CombiTable1D.getTable1DValue(1, 1, 0.0);
end TableUse;
"#,
        )
        .expect("fixture parses");
    let error = session
        .compile_model("TableUse")
        .expect_err("unsupported external table call must not become a plausible simulation");
    assert!(
        error.to_string().contains("function")
            || error.to_string().contains("unsupported")
            || error.to_string().contains("unresolved")
    );
}
