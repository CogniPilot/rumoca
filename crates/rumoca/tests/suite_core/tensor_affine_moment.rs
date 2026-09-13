use rumoca::Compiler;
use rumoca_ir_solve::TargetAssignmentShape;
use rumoca_sim::{
    SimOptions, SimSolverMode, lower_dae_for_simulation, simulate_dae_with_diagnostics,
};
use rumoca_solver::SimExecutionPolicy;

const SOURCE: &str = r#"
model TensorAffineMoment
  parameter Real r[3] = {2,3,5};
  Real x(start=1, fixed=true);
  Real force[3](each start=1e30);
  Real torque[3];
equation
  der(x) = -x;
  force[1] = 4*x;
  force[3] = 7*x;
  torque[3] = 18*x;
  torque = cross(r,force);
end TensorAffineMoment;
"#;

#[test]
fn tensor_moment_issues_a_force_isolator_and_rejects_forged_projection_wire() {
    let compiled = Compiler::new()
        .model("TensorAffineMoment")
        .compile_str(SOURCE, "tensor_affine_moment.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let row = model
        .problem
        .continuous
        .refresh_owners
        .algebraic()
        .causal_rows()
        .iter()
        .find(|row| model.problem.solve_layout.solver_maps.names[row.target_index()] == "force[2]")
        .expect("the force inside the moment equation requires an exact seed");
    assert!(row.exact_assignment_certified());
    assert!(!row.direct_assignment_certified());
    assert!(matches!(
        row.assignment_shape(),
        Some(TargetAssignmentShape::TensorAffine { .. })
    ));
    let wire = serde_json::to_value(rumoca_sim::solve_model_wire(&model).unwrap()).unwrap();
    let original = serde_json::to_vec(&wire).unwrap();
    rumoca_sim::deserialize_solve_model(&mut serde_json::Deserializer::from_slice(&original))
        .unwrap();
    for field in ["steps", "independent_ranges", "output"] {
        let mut forged = wire.clone();
        let projection = forged["problem"]["continuous"]["refresh_owners"]["algebraic"]["rows"]
            .as_array_mut()
            .unwrap()
            .iter_mut()
            .find_map(|row| row["assignment_shape"].get_mut("TensorAffine"))
            .unwrap()
            .get_mut("projection")
            .unwrap();
        projection[field] = if field == "output" {
            serde_json::json!(0)
        } else {
            serde_json::json!([])
        };
        let bytes = serde_json::to_vec(&forged).unwrap();
        assert!(
            rumoca_sim::deserialize_solve_model(&mut serde_json::Deserializer::from_slice(&bytes))
                .is_err(),
            "forged {field}"
        );
    }
}

#[test]
fn tensor_moment_matches_all_analytic_channels_from_large_starts() {
    let compiled = Compiler::new()
        .model("TensorAffineMoment")
        .compile_str(SOURCE, "tensor_affine_moment.mo")
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
            for (name, scale) in [
                ("x", 1.0),
                ("force[1]", 4.0),
                ("force[2]", 15.0),
                ("force[3]", 7.0),
                ("torque[1]", -54.0),
                ("torque[2]", 6.0),
                ("torque[3]", 18.0),
            ] {
                let column = result.names.iter().position(|value| value == name).unwrap();
                let expected = scale * (-time).exp();
                assert!(
                    (result.data[column][row] - expected).abs() < 1e-6,
                    "{solver_mode:?}/{execution_policy:?}: {name}({time}) = {}, expected {expected}",
                    result.data[column][row]
                );
            }
        }
    }
}
