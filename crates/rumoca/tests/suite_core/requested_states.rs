use rumoca::Compiler;
use rumoca_ir_dae::{Dae, VariableRole};
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model PreferredTensorState
  Real x[2](start={1,2}, each fixed=true, each stateSelect=StateSelect.always);
  Real q[2](each stateSelect=StateSelect.avoid);
equation
  x = q;
  der(q) = -q;
end PreferredTensorState;
"#;

fn state_declarations(model: &Dae) -> Vec<(String, usize)> {
    model.inspect(|view| {
        view.variables()
            .filter(|(_, variable)| variable.role() == VariableRole::State)
            .map(|(_, variable)| (variable.name().to_string(), variable.scalar_count()))
            .collect()
    })
}

#[test]
fn requested_tensor_state_is_selected_without_an_explicit_derivative() {
    // MLS §4.8.7.1: always applies even when only an avoided alias is differentiated.
    let compiled = Compiler::new()
        .model("PreferredTensorState")
        .compile_str(SOURCE, "PreferredTensorState.mo")
        .unwrap();
    assert_eq!(state_declarations(&compiled.dae), [("q".to_owned(), 2)]);
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae).unwrap();
    assert_eq!(state_declarations(prepared.as_dae()), [("x".to_owned(), 2)]);
    let counts = |view: rumoca_ir_dae::DaeView<'_>| {
        (view.variable_count(), view.continuous_owners().count())
    };
    assert_eq!(
        compiled.dae.inspect(counts),
        prepared.as_dae().inspect(counts)
    );
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 1.0,
                dt: Some(0.1),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap();
        for (name, amplitude) in [("x[1]", 1.0), ("q[1]", 1.0), ("x[2]", 2.0), ("q[2]", 2.0)] {
            let column = result.names.iter().position(|value| value == name).unwrap();
            for (row, &time) in result.times.iter().enumerate() {
                let expected = amplitude * (-time).exp();
                assert!(
                    (result.data[column][row] - expected).abs() < 1e-5,
                    "{solver_mode:?}: {name} at {time}"
                );
            }
        }
    }
}

#[test]
fn requested_output_state_preserves_causality_parameters_and_assertions() {
    let source = r#"
model PreferredOutput
  parameter Real rate(stateSelect=StateSelect.always) = 2;
  output Real x(start=1, fixed=true, stateSelect=StateSelect.always);
  Real q(stateSelect=StateSelect.avoid);
equation
  x = q;
  der(q) = -rate*q;
  assert(q > 0, "positive trajectory");
end PreferredOutput;
"#;
    let compiled = Compiler::new()
        .model("PreferredOutput")
        .compile_str(source, "PreferredOutput.mo")
        .unwrap();
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae).unwrap();
    assert_eq!(state_declarations(prepared.as_dae()), [("x".to_owned(), 1)]);
    prepared.as_dae().inspect(|view| {
        let role = |name: &str| {
            view.variables()
                .find(|(_, variable)| variable.name().as_str() == name)
                .unwrap()
                .1
        };
        assert_eq!(role("rate").role(), VariableRole::Parameter);
        assert_eq!(
            role("x").causality(),
            rumoca_ir_dae::VariableCausality::Output
        );
    });
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.5,
                dt: Some(0.1),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap();
        for name in ["x", "q"] {
            let column = result.names.iter().position(|value| value == name).unwrap();
            for (row, &time) in result.times.iter().enumerate() {
                assert!((result.data[column][row] - (-2.0 * time).exp()).abs() < 1e-5);
            }
        }
    }
    let rejected = Compiler::new()
        .model("PreferredOutput")
        .compile_str(&source.replace("q > 0", "q > 2"), "RejectedOutput.mo")
        .unwrap();
    let error = simulate_dae_with_diagnostics(&rejected.dae, &SimOptions::default())
        .expect_err("the source assertion must still execute");
    assert!(error.to_string().contains("positive trajectory"), "{error}");
}

#[test]
fn requested_state_does_not_erase_conflicting_fixed_alias_values() {
    let source = SOURCE.replace(
        "Real q[2](each stateSelect=StateSelect.avoid);",
        "Real q[2](start={0,0}, each fixed=true, each stateSelect=StateSelect.avoid);",
    );
    let compiled = Compiler::new()
        .model("PreferredTensorState")
        .compile_str(&source, "ConflictingInitialValues.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                ..Default::default()
            },
        );
        assert!(result.is_err(), "contradictory fixed values were accepted");
    }
}

#[test]
fn requested_components_accumulate_within_one_tensor_constraint() {
    let source = r#"
model SelectedComponents
  Real x(start=1, fixed=true, stateSelect=StateSelect.always);
  Real y(start=2, fixed=true, stateSelect=StateSelect.always);
  Real p[3](start={1,2,3}, each fixed=true, each stateSelect=StateSelect.avoid);
  Real z;
equation
  der(p) = -p;
  p = {x,y,z};
end SelectedComponents;
"#;
    let compiled = Compiler::new()
        .model("SelectedComponents")
        .compile_str(source, "SelectedComponents.mo")
        .unwrap();
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae)
        .expect("both requested components must have defining derivatives");
    let counts = |view: rumoca_ir_dae::DaeView<'_>| {
        (view.variable_count(), view.continuous_owners().count())
    };
    let (variables, owners) = compiled.dae.inspect(counts);
    // x/y receive derivative aliases; p remains one three-component owner.
    assert_eq!(
        prepared.as_dae().inspect(counts),
        (variables + 2, owners + 2)
    );
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap();
        for (name, amplitude) in [
            ("x", 1.0),
            ("y", 2.0),
            ("z", 3.0),
            ("p[1]", 1.0),
            ("p[2]", 2.0),
            ("p[3]", 3.0),
        ] {
            let column = result.names.iter().position(|value| value == name).unwrap();
            for (row, &time) in result.times.iter().enumerate() {
                assert!(
                    (result.data[column][row] - amplitude * (-time).exp()).abs() < 1e-5,
                    "{solver_mode:?}: {name} at {time}"
                );
            }
        }
    }
}

const PRODUCT_SOURCE: &str = r#"
model ProductConstraint
  Real x(start=-1, fixed=true, stateSelect=StateSelect.always);
  Real q(start=1, stateSelect=StateSelect.avoid);
equation
  der(q) = -q;
  0 = x*q + 1;
end ProductConstraint;
"#;

#[test]
fn requested_state_accepts_a_differentiated_product_constraint() {
    check_product_constraint(PRODUCT_SOURCE);
}

#[test]
fn requested_state_accepts_a_differentiated_dot_product_constraint() {
    check_product_constraint(&PRODUCT_SOURCE.replace("x*q + 1", "{x,1}*{q,1}"));
}

fn check_product_constraint(source: &str) {
    let compiled = Compiler::new()
        .model("ProductConstraint")
        .compile_str(source, "ProductConstraint.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap();
        for (name, sign) in [("x", -1.0), ("q", 1.0)] {
            let column = result.names.iter().position(|value| value == name).unwrap();
            for (row, &time) in result.times.iter().enumerate() {
                let expected = sign * (-sign * time).exp();
                assert!(
                    (result.data[column][row] - expected).abs() < 1e-5,
                    "{solver_mode:?}: {name} at {time}"
                );
            }
        }
    }
}
