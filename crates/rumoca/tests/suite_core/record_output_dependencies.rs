//! Record-valued pure calls keep exact per-output dependencies through Solve
//! lowering and wire replay. The fixture callees refuse inlining
//! (`Inline=false`) so each call stays a pure-call site the wire checks.

use rumoca::Compiler;
use rumoca_sim::{
    SimOptions, SimSolverMode, deserialize_solve_model, lower_dae_for_simulation,
    simulate_dae_with_diagnostics, solve_model_wire,
};
use rumoca_solver::SimExecutionPolicy;

const SOURCE: &str = r#"
package RecordDependency
  record Orientation
    Real T[2,2];
    Real w[2];
  end Orientation;
  function rotation
    input Real q;
    input Real w[2];
    output Orientation R;
  algorithm
    R.T := {{cos(q), -sin(q)}, {sin(q), cos(q)}};
    R.w := w;
    annotation(Inline=false);
  end rotation;
  model Probe
    Real q(start=0.1, fixed=true);
    Real velocity[2];
    Orientation R;
  equation
    der(q) = 1;
    R = rotation(q, velocity);
    velocity = {2*R.T[1,1], 3*R.T[2,2]};
  end Probe;
end RecordDependency;
"#;

const COPY_SOURCE: &str = r#"
package CopiedCoordinates
  record Orientation
    Real T[2,2];
    Real w[2];
  end Orientation;
  function angularVelocity2
    input Orientation R;
    output Real w[2];
  algorithm
    w := R.w;
    annotation(Inline=false);
  end angularVelocity2;
  model Probe
    Real q[2](start={1, 2}, each fixed=true);
    Orientation R;
  equation
    der(q) = -q;
    q = angularVelocity2(R);
    R.T = identity(2);
  end Probe;
end CopiedCoordinates;
"#;

#[test]
fn copied_record_vector_has_componentwise_inverse_dependencies() {
    let compiled = Compiler::new()
        .model("CopiedCoordinates.Probe")
        .compile_str(COPY_SOURCE, "copied_coordinates.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let names = model
        .problem
        .solver_named_report(0..model.solver_scalar_count())
        .unwrap();
    let pattern = model
        .artifacts
        .continuous
        .structural
        .implicit()
        .unwrap()
        .pattern();
    let coordinates =
        ["R.w[1]", "R.w[2]"].map(|name| names.iter().position(|value| value == name).unwrap());
    let mut checked = 0;
    for block in model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .scalar_blocks()
    {
        for (&row, &target) in block.rows.iter().zip(&block.y_indices) {
            if !coordinates.contains(&target) {
                continue;
            }
            for &coordinate in &coordinates {
                assert_eq!(
                    pattern.contains(row as u32, coordinate as u32),
                    coordinate == target
                );
            }
            checked += 1;
        }
    }
    assert_eq!(checked, 2);
}

#[test]
fn copied_record_vector_has_exact_refresh_assignments() {
    let compiled = Compiler::new()
        .model("CopiedCoordinates.Probe")
        .compile_str(COPY_SOURCE, "copied_coordinates.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let names = model
        .problem
        .solver_named_report(0..model.solver_scalar_count())
        .unwrap();
    let plan = model.problem.continuous.refresh_owners.algebraic();
    for name in ["R.w[1]", "R.w[2]"] {
        let target = names.iter().position(|value| value == name).unwrap();
        assert!(
            plan.rows
                .iter()
                .any(|row| { row.target_index() == target && row.exact_assignment_certified() }),
            "the returned input coordinate {name} has no exact refresh assignment"
        );
    }
    assert!(plan.causal_solution_certified);
}

#[test]
fn copied_record_vector_preserves_all_observables_on_both_execution_backends() {
    let compiled = Compiler::new()
        .model("CopiedCoordinates.Probe")
        .compile_str(COPY_SOURCE, "copied_coordinates.mo")
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
        assert_eq!(result.names.len(), 8);
        for (index, &time) in result.times.iter().enumerate() {
            let q = (-time).exp();
            for (name, expected) in [
                ("q[1]", q),
                ("q[2]", 2.0 * q),
                ("R.w[1]", q),
                ("R.w[2]", 2.0 * q),
                ("R.T[1,1]", 1.0),
                ("R.T[1,2]", 0.0),
                ("R.T[2,1]", 0.0),
                ("R.T[2,2]", 1.0),
            ] {
                let column = result.names.iter().position(|value| value == name).unwrap();
                assert!(
                    (result.data[column][index] - expected).abs() < 1e-6,
                    "{solver_mode:?}/{execution_policy:?}: {name}({time})"
                );
            }
        }
    }
}

#[test]
fn record_rotation_matrix_does_not_depend_on_angular_velocity() {
    let compiled = Compiler::new()
        .model("RecordDependency.Probe")
        .compile_str(SOURCE, "record_dependencies.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let names = model
        .problem
        .solver_named_report(0..model.solver_scalar_count())
        .unwrap();
    let velocity = names.iter().position(|name| name == "velocity[1]").unwrap();
    let q = names.iter().position(|name| name == "q").unwrap();
    let pattern = model
        .artifacts
        .continuous
        .structural
        .implicit()
        .unwrap()
        .pattern();
    let mut checked = 0;
    for block in model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .scalar_blocks()
    {
        for (&row, &target) in block.rows.iter().zip(&block.y_indices) {
            if names[target].starts_with("R.T[") {
                assert!(pattern.contains(row as u32, q as u32));
                assert!(
                    !pattern.contains(row as u32, velocity as u32),
                    "{} falsely depends on velocity[1] at implicit row {row}",
                    names[target]
                );
                checked += 1;
            }
        }
    }
    assert_eq!(checked, 4);
}

#[test]
fn record_output_dependency_summary_preserves_all_observables() {
    let compiled = Compiler::new()
        .model("RecordDependency.Probe")
        .compile_str(SOURCE, "record_dependencies.mo")
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
        assert_eq!(result.names.len(), 9);
        for (index, &time) in result.times.iter().enumerate() {
            let q = 0.1 + time;
            for (name, expected) in [
                ("q", q),
                ("R.T[1,1]", q.cos()),
                ("R.T[1,2]", -q.sin()),
                ("R.T[2,1]", q.sin()),
                ("R.T[2,2]", q.cos()),
                ("velocity[1]", 2.0 * q.cos()),
                ("velocity[2]", 3.0 * q.cos()),
                ("R.w[1]", 2.0 * q.cos()),
                ("R.w[2]", 3.0 * q.cos()),
            ] {
                let column = result.names.iter().position(|value| value == name).unwrap();
                assert!(
                    (result.data[column][index] - expected).abs() < 1e-6,
                    "{solver_mode:?}/{execution_policy:?}: {name}({time})"
                );
            }
        }
    }
}

#[test]
fn copied_record_wire_carries_no_projection_claim_and_replay_rederives_it() {
    let compiled = Compiler::new()
        .model("CopiedCoordinates.Probe")
        .compile_str(COPY_SOURCE, "copied_coordinates.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let wire = serde_json::to_value(solve_model_wire(&model).unwrap()).unwrap();
    let original = serde_json::to_vec(&wire).unwrap();
    let restored =
        deserialize_solve_model(&mut serde_json::Deserializer::from_slice(&original)).unwrap();
    assert_owner_claims_are_derived(&wire, &model, &restored);
}

#[test]
fn model_wire_carries_no_dependency_claim_and_replay_rederives_it() {
    let compiled = Compiler::new()
        .model("RecordDependency.Probe")
        .compile_str(SOURCE, "record_dependencies.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let wire = serde_json::to_value(solve_model_wire(&model).unwrap()).unwrap();
    let original = serde_json::to_vec(&wire).unwrap();
    let restored =
        deserialize_solve_model(&mut serde_json::Deserializer::from_slice(&original)).unwrap();
    assert_owner_claims_are_derived(&wire, &model, &restored);
}

/// Dependencies, projections and affinity are derived from the owner body at replay and are
/// never read from the wire: the owner entry carries no such claim, and the replayed table
/// equals the constructed one field for field.
pub(crate) fn assert_owner_claims_are_derived(
    wire: &serde_json::Value,
    model: &rumoca_ir_solve::SolveModel,
    restored: &rumoca_ir_solve::SolveModel,
) {
    let owner = pure_call_owner_index(wire);
    let entry = &wire["pure_calls"]["owners"][owner];
    for claim in ["dependencies", "projections", "affinity"] {
        assert!(entry.get(claim).is_none(), "{claim} is not a wire claim");
    }
    assert_eq!(
        format!("{:?}", restored.pure_calls),
        format!("{:?}", model.pure_calls)
    );
}

/// The owner-table index referenced by the first retained call of the implicit RHS; sites
/// carry only that id on the wire.
fn pure_call_owner_index(wire: &serde_json::Value) -> usize {
    wire["problem"]["continuous"]["implicit_rhs"]["nodes"][0]["ScalarPrograms"]["programs"]
        .as_array()
        .unwrap()
        .iter()
        .flat_map(|program| program.as_array().unwrap())
        .find_map(|op| op.get("PureCall"))
        .unwrap()["site"]["owner"]
        .as_u64()
        .unwrap() as usize
}
