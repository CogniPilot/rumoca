use rumoca::Compiler;
use rumoca_ir_dae as dae;
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
fn implicit_tensor_rates_are_available_during_index_reduction() {
    let source = include_str!("../fixtures/index_reduction/ImplicitRateConstraint.mo");
    check_implicit_rate_motion(source);
}

#[test]
fn implicit_tensor_rates_preserve_explicit_domain_derivative_owners() {
    let source = include_str!("../fixtures/index_reduction/ImplicitRateConstraint.mo").replace(
        "der(omega) = {0,0};",
        "for i in 1:2 loop der(omega[i]) = 0; end for;",
    );
    check_implicit_rate_motion(&source);
}

#[test]
fn coupled_tensor_rates_are_available_during_index_reduction() {
    let source = include_str!("../fixtures/index_reduction/ImplicitRateConstraint.mo").replace(
        "2*der(q) + q = omega;",
        "{{4,-1},{-2,3}}*der(q) + q = omega;",
    );
    check_implicit_rate_motion(&source);
}

#[test]
fn coupled_rate_block_supplies_higher_derivatives_of_an_independent_constraint() {
    let source = include_str!("../fixtures/index_reduction/CoupledRateConstraint.mo");
    let compiled = Compiler::new()
        .model("CoupledRateConstraint")
        .compile_str(source, "CoupledRateConstraint.mo")
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
        for (row, &time) in result.times.iter().enumerate() {
            for (name, expected) in [
                ("q[1]", 0.2 + 0.32 * time),
                ("q[2]", 0.4 + 0.68 * time),
                ("omega[1]", 0.6),
                ("omega[2]", 1.4),
                ("theta", 0.6 + time),
                ("force[1]", 0.0),
                ("force[2]", 0.0),
            ] {
                let column = result.names.iter().position(|v| v == name).unwrap();
                assert!(
                    (result.data[column][row] - expected).abs() < 1e-6,
                    "{solver_mode:?}: {name} at {time}"
                );
            }
        }
    }
}

#[test]
fn a_complete_coupled_tensor_ode_keeps_its_native_derivative_block() {
    let source = "model CoupledOde Real q[2](start={1,2}, each fixed=true); equation {{4,-1},{-2,3}}*der(q) = -q; end CoupledOde;";
    let compiled = Compiler::new()
        .model("CoupledOde")
        .compile_str(source, "CoupledOde.mo")
        .unwrap();
    assert!(matches!(
        rumoca_phase_structural::prepare_for_solve(&compiled.dae).unwrap(),
        rumoca_phase_structural::PreparedDae::Borrowed { .. }
    ));
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
        for (row, &time) in result.times.iter().enumerate() {
            for (name, amplitude) in [("q[1]", 1.0), ("q[2]", 2.0)] {
                let column = result.names.iter().position(|v| v == name).unwrap();
                assert!((result.data[column][row] - amplitude * (-time / 2.0).exp()).abs() < 1e-6);
            }
        }
    }
}

#[test]
fn partial_explicit_derivative_coverage_still_receives_a_whole_tensor_alias() {
    let source = include_str!("../fixtures/index_reduction/ImplicitRateConstraint.mo").replace(
        "2*der(q) + q = omega;",
        "der(q[1]) = 0.5*(omega[1]-q[1]);\n  2*der(q[2]) + q[2] = omega[2];",
    );
    let compiled = Compiler::new()
        .model("ImplicitRateConstraint")
        .compile_str(&source, "ImplicitRateConstraint.mo")
        .unwrap();
    let (prepared, report) = rumoca_phase_structural::inspect_prepare_for_solve(&compiled.dae);
    // Reconstructing the alias from separate scalar projections remains a
    // distinct proof obligation; inspect the exact admitted or stalled DAE.
    let model = match &prepared {
        Ok(prepared) => prepared.as_dae(),
        Err(_) => report.stalled.as_ref().unwrap().as_dae(),
    };
    model.inspect(|view| {
        let generated = view
            .variables()
            .filter(|(_, v)| v.origin() == dae::VariableOrigin::Generated)
            .collect::<Vec<_>>();
        assert_eq!(generated.len(), 1);
        assert_eq!(generated[0].1.role(), dae::VariableRole::Algebraic);
        assert_eq!(generated[0].1.value_type().dimensions(), &[2]);
        assert_eq!(generated[0].1.fixed_uniform(), Some(false));
    });
}

fn check_implicit_rate_motion(source: &str) {
    let compiled = Compiler::new()
        .model("ImplicitRateConstraint")
        .compile_str(source, "ImplicitRateConstraint.mo")
        .unwrap();
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae).unwrap();
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
        for (row, &time) in result.times.iter().enumerate() {
            let decay = (-time / 2.0).exp();
            for (name, expected) in [
                ("q[1]", 0.6 - 0.4 * decay),
                ("q[2]", 1.2 - 0.8 * decay),
                ("omega[1]", 0.6),
                ("omega[2]", 1.2),
                ("z", 0.6 - 0.4 * decay),
                ("v", 0.2 * decay),
                ("force", -0.1 * decay),
            ] {
                let column = result.names.iter().position(|v| v == name).unwrap();
                assert!(
                    (result.data[column][row] - expected).abs() < 1e-6,
                    "{solver_mode:?}: {name} at {time}"
                );
            }
        }
    }
    prepared.as_dae().inspect(|view| {
        assert_eq!(
            view.variables()
                .filter(|(_, v)| v.role() == dae::VariableRole::State)
                .map(|(_, v)| v.scalar_count())
                .sum::<usize>(),
            4,
            "retained states: {:?}",
            view.variables()
                .filter(|(_, v)| v.role() == dae::VariableRole::State)
                .map(|(_, v)| v.name().to_string())
                .collect::<Vec<_>>()
        );
        let generated = view
            .variables()
            .filter(|(_, v)| v.origin() == dae::VariableOrigin::Generated)
            .collect::<Vec<_>>();
        assert_eq!(generated.len(), 1);
        assert_eq!(generated[0].1.value_type().dimensions(), &[2]);
        assert_eq!(generated[0].1.fixed_uniform(), Some(false));
    });
}

#[test]
fn derivative_aliases_preserve_tensor_owners_and_do_not_add_initial_constraints() {
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
            .filter(|(_, variable)| variable.origin() == dae::VariableOrigin::Generated)
            .collect::<Vec<_>>();
        assert_eq!(aliases.len(), 1);
        let (_, alias) = aliases[0];
        assert_eq!(alias.role(), dae::VariableRole::Algebraic);
        assert_eq!(alias.value_type().dimensions(), &[2]);
        assert_eq!(alias.fixed_uniform(), Some(false));
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
