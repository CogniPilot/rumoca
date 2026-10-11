//! Original compact-domain source reproductions, before backend preparation.

use super::*;

fn compile(source: &str, model: &str) -> CompilationResult {
    let mut session = Session::default();
    session
        .add_document("Repro.mo", source)
        .expect("source parses");
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
    compiled
        .dae
        .inspect(|view| assert_eq!(view.root_count(), 0));
}

#[test]
fn issue_424_original_constructor_rejects_incompatible_declared_shape() {
    // MLS §10.4.1.2 makes the last textual iterator outermost: [2, 3].
    // The original issue declares [3, 2]; pinned OMC rejects that source too.
    let mut session = Session::default();
    session
        .add_document(
            "Repro.mo",
            include_str!("fixtures/reported_bugs/issue_424.mo"),
        )
        .expect("original source parses");
    match session.compile_model_phases("C3").expect("phase result") {
        PhaseResult::Failed {
            phase,
            error_code,
            error,
            ..
        } => {
            assert_eq!(phase, FailedPhase::ToDae);
            assert_eq!(error_code.as_deref(), Some("ED020"));
            assert!(error.contains("expression shape mismatch"), "{error}");
        }
        other => panic!("original mismatched constructor must be typed-rejected: {other:?}"),
    }
}

#[test]
fn issue_424_conforming_constructor_has_last_iterator_outermost() {
    let compiled = compile(
        r#"
model C3Conforming
  parameter Real m[2,3] = {i+10*j for i in 1:3, j in 1:2};
  Real y(start=1, fixed=true);
equation
  der(y) = -m[2,3]*y;
end C3Conforming;
"#,
        "C3Conforming",
    );
    assert_eq!(compiled.balance_detail.equations_unknowns(), (1, 1));
}
