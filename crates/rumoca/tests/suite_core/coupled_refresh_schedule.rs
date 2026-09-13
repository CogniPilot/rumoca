use rumoca::Compiler;
use rumoca_ir_solve::RefreshStage;
use rumoca_sim::{
    SimOptions, SimSolverMode, lower_dae_for_simulation, simulate_dae_with_diagnostics,
};
use rumoca_solver::SimExecutionPolicy;

const SOURCE: &str = r#"
model CoupledRefresh
  Real x(start=1, fixed=true);
  Real input_value;
  Real a;
  Real b;
  Real sum_value;
  Real result;
equation
  der(x) = -x;
  input_value = sin(x);
  a = input_value + b;
  b = 0.25*a;
  sum_value = a + b;
  result = sum_value + cos(x);
end CoupledRefresh;
"#;

const AFFINE_SOURCE: &str = r#"
package BlockAffine
  function transform
    input Real R[2,2];
    input Real v[2];
    output Real w[2];
  algorithm
    w := transpose(R)*v;
    annotation(Inline=false);
  end transform;
  model Probe
    Real x(start=1, fixed=true);
    Real a[2];
    Real b[2];
    Real R[2,2];
  equation
    der(x) = -x;
    R = [x,0;0,2];
    a = transform(R,b) + {x,1};
    b = {0.25*a[1],0.125*a[2]};
  end Probe;
end BlockAffine;
"#;

const ZERO_SOURCE: &str = r#"
model ZeroRefresh
  connector Port
    Real driven[3];
    flow Real force[3](start={7,11,13});
  end Port;
  Port port;
  Real x(start=1, fixed=true);
equation
  port.driven = {x,2*x,3*x};
  der(x) = -x + sum(port.force);
end ZeroRefresh;
"#;

#[test]
fn zero_array_equation_issues_complete_seeds_and_rejects_forged_wire() {
    let compiled = Compiler::new()
        .model("ZeroRefresh")
        .compile_str(ZERO_SOURCE, "zero_refresh.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let plan = model.problem.continuous.refresh_owners.algebraic();
    for name in ["port.force[1]", "port.force[2]", "port.force[3]"] {
        let row = plan
            .causal_rows()
            .iter()
            .find(|row| model.problem.solve_layout.solver_maps.names[row.target_index()] == name)
            .unwrap_or_else(|| panic!("{name} must have a causal assignment"));
        assert!(row.direct_assignment_certified());
        assert!(matches!(
            row.assignment_shape(),
            Some(rumoca_ir_solve::TargetAssignmentShape::Zero { .. })
        ));
    }
    let mut wire = serde_json::to_value(rumoca_sim::solve_model_wire(&model).unwrap()).unwrap();
    let bytes = serde_json::to_vec(&wire).unwrap();
    rumoca_sim::deserialize_solve_model(&mut serde_json::Deserializer::from_slice(&bytes)).unwrap();
    let rows = wire["problem"]["continuous"]["refresh_owners"]["algebraic"]["rows"]
        .as_array_mut()
        .unwrap();
    let shape = rows
        .iter_mut()
        .find_map(|row| row["assignment_shape"].get_mut("Zero"))
        .unwrap();
    shape["expr_eval_len"] = serde_json::json!(0);
    let forged = serde_json::to_vec(&wire).unwrap();
    assert!(
        rumoca_sim::deserialize_solve_model(&mut serde_json::Deserializer::from_slice(&forged))
            .is_err()
    );
}

#[test]
fn zero_array_equation_preserves_all_analytic_channels() {
    let compiled = Compiler::new()
        .model("ZeroRefresh")
        .compile_str(ZERO_SOURCE, "zero_refresh.mo")
        .unwrap();
    for (solver_mode, execution_policy) in [
        (SimSolverMode::Bdf, SimExecutionPolicy::Auto),
        (SimSolverMode::Bdf, SimExecutionPolicy::Interpreter),
        (SimSolverMode::RkLike, SimExecutionPolicy::Auto),
        (SimSolverMode::RkLike, SimExecutionPolicy::Interpreter),
    ] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                rtol: 1e-8,
                atol: 1e-8,
                solver_mode,
                execution_policy,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!(result.names.len(), 7);
        for (row, &time) in result.times.iter().enumerate() {
            let x = (-time).exp();
            for (name, expected) in [
                ("x", x),
                ("port.force[1]", 0.0),
                ("port.force[2]", 0.0),
                ("port.force[3]", 0.0),
                ("port.driven[1]", x),
                ("port.driven[2]", 2.0 * x),
                ("port.driven[3]", 3.0 * x),
            ] {
                let column = result.names.iter().position(|value| value == name).unwrap();
                assert!(
                    (result.data[column][row] - expected).abs() < 1e-6,
                    "{solver_mode:?}/{execution_policy:?}: {name}({time})"
                );
            }
        }
    }
}

#[test]
fn state_dependent_tensor_coefficients_issue_block_affinity() {
    let compiled = Compiler::new()
        .model("BlockAffine.Probe")
        .compile_str(AFFINE_SOURCE, "block_affine.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let coupled = model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .iter()
        .enumerate()
        .filter(|(_, block)| block.y_indices.len() > 1)
        .collect::<Vec<_>>();
    assert!(!coupled.is_empty());
    for (index, _) in coupled {
        assert!(
            model
                .problem
                .continuous
                .refresh_owners
                .algebraic_projection_block_is_affine(index)
        );
    }
}

#[test]
fn state_dependent_linear_tensor_solve_matches_all_analytic_channels() {
    let compiled = Compiler::new()
        .model("BlockAffine.Probe")
        .compile_str(AFFINE_SOURCE, "block_affine.mo")
        .unwrap();
    for (solver_mode, execution_policy) in [
        (SimSolverMode::Bdf, SimExecutionPolicy::Auto),
        (SimSolverMode::Bdf, SimExecutionPolicy::Interpreter),
        (SimSolverMode::RkLike, SimExecutionPolicy::Auto),
        (SimSolverMode::RkLike, SimExecutionPolicy::Interpreter),
    ] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                rtol: 1e-8,
                atol: 1e-8,
                solver_mode,
                execution_policy,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!(result.names.len(), 9);
        for (row, &time) in result.times.iter().enumerate() {
            let x = (-time).exp();
            let a = x / (1.0 - 0.25 * x);
            for (name, expected) in [
                ("x", x),
                ("a[1]", a),
                ("a[2]", 4.0 / 3.0),
                ("b[1]", 0.25 * a),
                ("b[2]", 1.0 / 6.0),
                ("R[1,1]", x),
                ("R[1,2]", 0.0),
                ("R[2,1]", 0.0),
                ("R[2,2]", 2.0),
            ] {
                let column = result.names.iter().position(|value| value == name).unwrap();
                let actual = result.data[column][row];
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{solver_mode:?}/{execution_policy:?}: {name}({time}) = {actual}, expected {expected}"
                );
            }
        }
    }
}

#[test]
fn model_wire_rederives_block_affinity_and_rejects_erased_interactions() {
    let compiled = Compiler::new()
        .model("BlockAffine.Probe")
        .compile_str(AFFINE_SOURCE, "block_affine.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let mut wire = serde_json::to_value(rumoca_sim::solve_model_wire(&model).unwrap()).unwrap();
    let original = serde_json::to_vec(&wire).unwrap();
    let restored =
        rumoca_sim::deserialize_solve_model(&mut serde_json::Deserializer::from_slice(&original))
            .unwrap();
    for index in 0..model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .len()
    {
        assert_eq!(
            model
                .problem
                .continuous
                .refresh_owners
                .algebraic_projection_block_is_affine(index),
            restored
                .problem
                .continuous
                .refresh_owners
                .algebraic_projection_block_is_affine(index)
        );
    }
    let programs =
        wire["problem"]["continuous"]["implicit_rhs"]["nodes"][0]["ScalarPrograms"]["programs"]
            .as_array_mut()
            .unwrap();
    let site = programs
        .iter_mut()
        .flat_map(|program| program.as_array_mut().unwrap())
        .find_map(|op| op.get_mut("PureCall"))
        .unwrap();
    let interactions = &mut site["site"]["affinity"][0]["nonlinear"];
    assert!(!interactions.as_array().unwrap().is_empty());
    *interactions = serde_json::json!([]);
    let forged = serde_json::to_vec(&wire).unwrap();
    assert!(
        rumoca_sim::deserialize_solve_model(&mut serde_json::Deserializer::from_slice(&forged))
            .is_err()
    );
}

#[test]
fn coupled_refresh_projects_only_the_coupled_equations() {
    let compiled = Compiler::new()
        .model("CoupledRefresh")
        .compile_str(SOURCE, "coupled_refresh.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let plan = model.problem.continuous.refresh_owners.algebraic();
    assert!(!plan.causal_solution_certified);
    let projected = plan
        .value_stages
        .iter()
        .filter_map(|stage| match stage {
            RefreshStage::ProjectionBlock { plan, .. } => Some(plan),
            _ => None,
        })
        .flat_map(|plan| &plan.blocks)
        .flat_map(|block| &block.y_indices)
        .map(|&index| model.problem.solve_layout.solver_maps.names[index].as_str())
        .collect::<std::collections::BTreeSet<_>>();
    assert_eq!(projected, std::collections::BTreeSet::from(["a", "b"]));
}

#[test]
fn coupled_refresh_preserves_values_before_and_after_the_coupled_solve() {
    let compiled = Compiler::new()
        .model("CoupledRefresh")
        .compile_str(SOURCE, "coupled_refresh.mo")
        .unwrap();
    for (solver_mode, execution_policy) in [
        (SimSolverMode::Bdf, SimExecutionPolicy::Auto),
        (SimSolverMode::Bdf, SimExecutionPolicy::Interpreter),
        (SimSolverMode::RkLike, SimExecutionPolicy::Auto),
        (SimSolverMode::RkLike, SimExecutionPolicy::Interpreter),
    ] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                solver_mode,
                execution_policy,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!(result.names.len(), 6);
        for (index, &time) in result.times.iter().enumerate() {
            let x = (-time).exp();
            for (name, expected) in [
                ("x", x),
                ("input_value", x.sin()),
                ("a", 4.0 * x.sin() / 3.0),
                ("b", x.sin() / 3.0),
                ("sum_value", 5.0 * x.sin() / 3.0),
                ("result", 5.0 * x.sin() / 3.0 + x.cos()),
            ] {
                let column = result.names.iter().position(|value| value == name).unwrap();
                let actual = result.data[column][index];
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{solver_mode:?}/{execution_policy:?}: {name}({time}) = {actual}, expected {expected}"
                );
            }
        }
    }
}
