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

fn assert_checked_constructor_values(
    compiled: &CompilationResult,
    name: &str,
    dimensions: &[i64],
    expected: &[f64],
) {
    let variable = &compiled.flat.variables[&rumoca_core::VarName::new(name)];
    assert_eq!(variable.dims, dimensions);
    assert!(
        matches!(
            variable.binding.as_ref(),
            Some(rumoca_core::Expression::ArrayComprehension { .. })
        ),
        "parameter-evaluable constructor retains its compact owner"
    );
    let values = compiled.dae.inspect(|view| {
        let (_, declaration) = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == name)
            .expect("checked constructor declaration");
        let binding = declaration.binding().expect("checked constructor binding");
        rumoca_eval_dae::NumericEvaluator::new(view)
            .expression(binding)
            .expect("checked constructor evaluates")
    });
    assert_eq!(values, expected);
}

#[test]
fn issue_424_parameter_extents_preserve_both_constructor_axis_orders() {
    for (declaration, dimensions, expected) in [
        (
            "parameter Real m[nr,nx]={i+10*j for i in 1:nx,j in 1:nr};",
            [2, 3],
            [11.0, 12.0, 13.0, 21.0, 22.0, 23.0],
        ),
        (
            "parameter Real m[nx,nr]={i+10*j for j in 1:nr,i in 1:nx};",
            [3, 2],
            [11.0, 21.0, 12.0, 22.0, 13.0, 23.0],
        ),
    ] {
        let source = format!(
            "model C3ParameterExtents parameter Integer nx=3; parameter Integer nr=2; \
             {declaration} Real y(start=1,fixed=true); \
             equation der(y)=-m[1,1]*y; end C3ParameterExtents;"
        );
        let compiled = compile(&source, "C3ParameterExtents");
        assert_checked_constructor_values(&compiled, "m", &dimensions, &expected);
    }
}

#[test]
fn issue_424_boolean_parameter_masks_preserve_axis_order_and_exact_values() {
    for (declaration, dimensions, expected) in [
        (
            "parameter Boolean solid[nr,nx]={xc[i]<0.3 and j>2 for i in 1:nx,j in 1:nr};",
            [4, 3],
            [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0],
        ),
        (
            "parameter Boolean solid[nx,nr]={xc[i]<0.3 and j>2 for j in 1:nr,i in 1:nx};",
            [3, 4],
            [0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0],
        ),
    ] {
        let source = format!(
            "model C3BooleanParameters parameter Integer nx=3; parameter Integer nr=4; \
             parameter Real xc[nx]={{0.1,0.4,0.2}}; {declaration} \
             Real y(start=1,fixed=true); \
             equation der(y)=if solid[1,1] then -y else -2*y; end C3BooleanParameters;"
        );
        let compiled = compile(&source, "C3BooleanParameters");
        assert_checked_constructor_values(&compiled, "solid", &dimensions, &expected);
    }
}

#[test]
fn issue_424_reported_boolean_binding_accepts_conforming_parameter_dimensions() {
    let compiled = compile(
        "model C3BooleanBinding parameter Integer nx=3; parameter Integer nr=4; \
         parameter Real xc[nx]={0.1,0.4,0.2}; \
         Boolean solid[nx,nr]={xc[i]<0.3 and j>2 for j in 1:nr,i in 1:nx}; \
         Real y(start=1,fixed=true); \
         equation der(y)=if solid[1,1] then -y else -2*y; end C3BooleanBinding;",
        "C3BooleanBinding",
    );
    let variable = &compiled.flat.variables[&rumoca_core::VarName::new("solid")];
    assert_eq!(variable.dims, vec![3, 4]);
    assert!(matches!(
        variable.binding.as_ref(),
        Some(rumoca_core::Expression::ArrayComprehension { .. })
    ));
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
