use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model MixedDerivativeBlock
  Real x[2](start={1,2}, each fixed=true);
  Real a[2];
equation
  der(x) = a;
  a + der(x) = -2*x;
end MixedDerivativeBlock;
"#;

#[test]
fn derivative_aliases_preserve_tensor_owners_and_do_not_add_initial_constraints() {
    use rumoca_ir_dae::{VariableOrigin, VariableRole};
    use rumoca_phase_structural::{PreparedDae, prepare_for_solve};
    let compiled = Compiler::new()
        .model("MixedDerivativeBlock")
        .compile_str(SOURCE, "MixedDerivativeBlock.mo")
        .unwrap();
    let prepared = prepare_for_solve(&compiled.dae).unwrap();
    let counts = |view: rumoca_ir_dae::DaeView<'_>| {
        (
            view.variable_count(),
            view.continuous_owners().count(),
            view.initialization_owners().count(),
        )
    };
    let (variables, equations, initial) = compiled.dae.inspect(counts);
    assert_eq!(
        prepared.as_dae().inspect(counts),
        (variables + 1, equations + 1, initial)
    );
    prepared.as_dae().inspect(|view| {
        let aliases = view
            .variables()
            .filter(|(_, variable)| variable.origin() == VariableOrigin::Generated)
            .collect::<Vec<_>>();
        assert_eq!(aliases.len(), 1);
        let (_, alias) = aliases[0];
        assert_eq!(alias.role(), VariableRole::Algebraic);
        assert_eq!(alias.value_type().dimensions(), &[2]);
        assert_eq!(alias.fixed(), Some(false));
        assert!(alias.start().is_some());
        assert!(alias.binding().is_none());
    });
    assert!(matches!(
        prepare_for_solve(prepared.as_dae()).unwrap(),
        PreparedDae::Borrowed { .. }
    ));
}

#[test]
fn mixed_derivative_algebraic_tensor_block_is_solved_jointly() {
    let compiled = Compiler::new()
        .model("MixedDerivativeBlock")
        .compile_str(SOURCE, "MixedDerivativeBlock.mo")
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
        for (name, amplitude) in [("x[1]", 1.0), ("x[2]", 2.0), ("a[1]", -1.0), ("a[2]", -2.0)] {
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

#[test]
fn mixed_derivative_dot_product_preserves_the_joint_constraint() {
    let source = r#"
model MixedConstraint
  Real x(start=-1, fixed=true, stateSelect=StateSelect.always);
  Real q(start=1, stateSelect=StateSelect.avoid);
  Real a;
equation
  der(q) = a;
  a = -q + 0.2*der(x);
  0 = {x,1}*{q,1};
end MixedConstraint;
"#;
    let compiled = Compiler::new()
        .model("MixedConstraint")
        .compile_str(source, "MixedConstraint.mo")
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
        let column = |name| result.names.iter().position(|value| value == name).unwrap();
        for (row, &time) in result.times.iter().enumerate() {
            let x = result.data[column("x")][row];
            let q = result.data[column("q")][row];
            let a = result.data[column("a")][row];
            assert!(
                (x * q + 1.0).abs() < 1e-5,
                "{solver_mode:?}: constraint at {time}"
            );
            assert!(
                ((-x).ln() - 0.1 * x * x - time + 0.1).abs() < 1e-5,
                "{solver_mode:?}: trajectory at {time}"
            );
            assert!(
                (a + q / (1.0 - 0.2 / (q * q))).abs() < 1e-5,
                "{solver_mode:?}: acceleration at {time}"
            );
        }
    }
}
