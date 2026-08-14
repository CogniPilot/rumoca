use rumoca_compile::compile::{Session, SessionConfig};

/// What this fixture ACTUALLY covers: `ModelicaStandardTables` is not in this
/// workspace and the session is built with no source root, so the model is
/// rejected by NAME RESOLUTION — `unresolved function call` — and never
/// reaches any table semantics at all.
///
/// The old assertion accepted any error containing "function", "unsupported"
/// OR "unresolved", which nearly every compiler diagnostic satisfies, so the
/// gap was invisible. It is pinned below so the fixture states its real
/// subject.
///
/// COVERAGE HOLE (not fixed here — no production change in this slice): the
/// external-table semantics this test is named for have no coverage anywhere.
/// A real test needs a fixture whose `ModelicaStandardTables` call RESOLVES
/// (a stub package or a loaded MSL source root) so the external-function
/// rejection is the diagnostic under test.
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
    assert_eq!(
        error.to_string(),
        "Resolve errors: unresolved function call: \
         'ModelicaStandardTables.CombiTable1D.getTable1DValue'",
    );
}
