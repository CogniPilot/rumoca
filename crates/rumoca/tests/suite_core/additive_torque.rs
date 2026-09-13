use rumoca::Compiler;
use rumoca_ir_solve::TargetAssignmentShape;
use rumoca_sim::{
    SimOptions, SimSolverMode, lower_dae_for_simulation, simulate_dae_with_diagnostics,
};
use rumoca_solver::SimExecutionPolicy;

// The FixedRotation moment equation retains its typed rotation call and cross
// product; integer parameters make the independent analytic solution exact.
const SOURCE: &str = r#"
package AdditiveTorque
  function rotate
    input Real R[3,3];
    input Real v[3];
    output Real w[3];
  algorithm
    w := transpose(R)*v;
    annotation(Inline=false);
  end rotate;
  model Probe
    parameter Real R[3,3] = [0,-1,0;1,0,0;0,0,1];
    parameter Real r[3] = {1,2,3};
    Real x(start=1, fixed=true);
    Real torque_a[3](each start=1e30);
    Real torque_b[3];
    Real force[3];
  equation
    der(x) = -x;
    torque_b = {x,2*x,3*x};
    force = {2*x,3*x,4*x};
    zeros(3) = torque_a + rotate(R,torque_b) - cross(r,force);
  end Probe;
end AdditiveTorque;
"#;

#[test]
fn additive_moment_equation_issues_independent_offsets_and_rejects_forged_wire() {
    let compiled = Compiler::new()
        .model("AdditiveTorque.Probe")
        .compile_str(SOURCE, "additive_torque.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    for component in 1..=3 {
        let name = format!("torque_a[{component}]");
        let row = model
            .problem
            .continuous
            .refresh_owners
            .algebraic()
            .causal_rows()
            .iter()
            .find(|row| model.problem.solve_layout.solver_maps.names[row.target_index()] == name)
            .unwrap_or_else(|| panic!("{name} requires a causal seed"));
        assert!(matches!(
            row.assignment_shape(),
            Some(TargetAssignmentShape::Additive { .. })
        ));
        assert!(row.exact_assignment_certified());
    }
    let wire = serde_json::to_value(rumoca_sim::solve_model_wire(&model).unwrap()).unwrap();
    let original = serde_json::to_vec(&wire).unwrap();
    rumoca_sim::deserialize_solve_model(&mut serde_json::Deserializer::from_slice(&original))
        .unwrap();
    for field in ["offset_terms", "coefficient", "expr_eval_len"] {
        let mut forged = wire.clone();
        let shape = forged["problem"]["continuous"]["refresh_owners"]["algebraic"]["rows"]
            .as_array_mut()
            .unwrap()
            .iter_mut()
            .find_map(|row| row["assignment_shape"].get_mut("Additive"))
            .unwrap();
        shape[field] = if field == "offset_terms" {
            serde_json::json!([])
        } else {
            serde_json::json!(0)
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
fn additive_moment_equation_matches_all_analytic_channels_from_large_starts() {
    let compiled = Compiler::new()
        .model("AdditiveTorque.Probe")
        .compile_str(SOURCE, "additive_torque.mo")
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
        assert_eq!(result.names.len(), 10);
        for (row, &time) in result.times.iter().enumerate() {
            let x = (-time).exp();
            for (name, scale) in [
                ("x", 1.0),
                ("torque_a[1]", -3.0),
                ("torque_a[2]", 3.0),
                ("torque_a[3]", -4.0),
                ("torque_b[1]", 1.0),
                ("torque_b[2]", 2.0),
                ("torque_b[3]", 3.0),
                ("force[1]", 2.0),
                ("force[2]", 3.0),
                ("force[3]", 4.0),
            ] {
                let column = result.names.iter().position(|value| value == name).unwrap();
                let actual = result.data[column][row];
                assert!(
                    (actual - scale * x).abs() < 1e-6,
                    "{solver_mode:?}/{execution_policy:?}: {name}({time}) = {actual}, expected {}",
                    scale * x
                );
            }
        }
    }
}
