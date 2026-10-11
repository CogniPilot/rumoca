//! Original compact-domain source reproductions, before backend preparation.

use super::*;

fn compile(source: &str, model: &str) -> CompilationResult {
    let mut session = Session::default();
    session.add_document("Repro.mo", source).expect("source parses");
    session
        .compile_model(model)
        .unwrap_or_else(|error| panic!("source compiles: {error:#}"))
}

#[test]
fn issue_426_inactive_neighbor_preserves_the_structured_family() {
    // MLS §3.6.5: only the selected if-expression arm is evaluated.
    let compiled = compile(include_str!("fixtures/reported_bugs/issue_426.mo"), "G1");
    assert_eq!(compiled.balance_detail.equations_unknowns(), (4, 4));
}

#[test]
fn issue_423_iterator_mod_parameter_stays_in_its_domain() {
    // MLS §3.7.2: a time-invariant quotient needs no model event root.
    let compiled = compile(include_str!("fixtures/reported_bugs/issue_423.mo"), "C13");
    assert_eq!(compiled.balance_detail.equations_unknowns(), (4, 4));
    compiled.dae.inspect(|view| assert_eq!(view.root_count(), 0));
}

#[test]
fn issue_424_multi_iterator_constructor_preserves_axis_order() {
    // MLS §10.4.1: one dimension per iterator, in source order.
    let compiled = compile(include_str!("fixtures/reported_bugs/issue_424.mo"), "C3");
    assert_eq!(compiled.balance_detail.equations_unknowns(), (1, 1));
}
