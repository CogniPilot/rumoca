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
    // Parameter-only assignments settle m before runtime balance is counted.
    assert_eq!(
        compiled.flat.variables[&rumoca_core::VarName::new("m")].dims,
        vec![3]
    );
    assert_eq!(compiled.balance_detail.equations_unknowns(), (1, 1));
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

#[test]
fn issue_424_reported_reverse_order_constructor_has_conforming_dimensions_and_values() {
    let compiled = compile(
        r#"
model C3ReverseOrder
  parameter Real m[3,2] = {i+10*j for j in 1:2, i in 1:3};
  Real y(start=1, fixed=true);
equation
  der(y) = -m[3,2]*y;
end C3ReverseOrder;
"#,
        "C3ReverseOrder",
    );
    assert_eq!(compiled.balance_detail.equations_unknowns(), (1, 1));
    let parameter = &compiled.flat.variables[&rumoca_core::VarName::new("m")];
    assert_eq!(parameter.dims, vec![3, 2]);
    let values = compiled.dae.inspect(|view| {
        let (_, declaration) = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "m")
            .expect("checked matrix parameter");
        let binding = declaration.binding().expect("checked constructor binding");
        rumoca_eval_dae::NumericEvaluator::new(view)
            .expression(binding)
            .expect("checked matrix constructor evaluates")
    });
    assert_eq!(values, vec![11.0, 21.0, 12.0, 22.0, 13.0, 23.0]);
}

#[test]
fn issue_424_partially_unknown_declaration_keeps_mismatched_known_axis() {
    let mut session = Session::default();
    session.add_document("Mixed.mo", "model Mixed parameter Real m[3,:]={i+10*j for i in 1:3,j in 1:2}; Real y(start=1,fixed=true); equation der(y)=-m[1,1]*y; end Mixed;").expect("source parses");
    match session.compile_model_phases("Mixed").expect("phase result") {
        PhaseResult::Failed { phase, error, .. } => {
            assert!(matches!(phase, FailedPhase::Typecheck | FailedPhase::ToDae));
            assert!(
                error.contains("shape mismatch") || error.contains("dimension"),
                "{error}"
            );
        }
        other => panic!("known first axis must not be reauthored: {other:?}"),
    }
}
