use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

#[test]
fn atan2_constraint_preserves_angular_acceleration() {
    check_angular_motion(0.0, false);
}

#[test]
fn atan2_constraint_differentiates_both_changing_arguments() {
    check_angular_motion(1.0, false);
}

#[test]
fn atan2_constraint_rejects_singular_initial_orientation() {
    let source = include_str!("../fixtures/index_reduction/Atan2Constraint.mo")
        .replace("start={1,0}", "start={0,0}")
        .replace("q[1] = 1", "q[1] = 0");
    let compiled = Compiler::new()
        .model("Atan2Constraint")
        .compile_str(&source, "singular_atan2.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                t_end: 0.1,
                ..Default::default()
            },
        )
        .expect_err("the angular derivative is undefined when both arguments vanish");
    }
}

fn check_angular_motion(speed: f64, observed: bool) {
    let mut source = include_str!("../fixtures/index_reduction/Atan2Constraint.mo")
        .replace("radialSpeed = 0", &format!("radialSpeed = {speed}"));
    if observed {
        source = source
            .replace("Real force;", "Real observed[2]; Real force;")
            .replace(
                "0 = atan2(q[2],q[1])-time;",
                "observed = 2*q; 0 = atan2(observed[2],observed[1])-time;",
            );
    }
    let compiled = Compiler::new()
        .model("Atan2Constraint")
        .compile_str(&source, "Atan2Constraint.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                t_end: 0.1,
                dt: Some(0.01),
                ..Default::default()
            },
        )
        .unwrap();
        for (row, &time) in result.times.iter().enumerate() {
            let radius = 1.0 + speed * time;
            let tangent = time.tan();
            let secant_squared = 1.0 / time.cos().powi(2);
            let mut expected = vec![
                ("q[1]", radius),
                ("q[2]", radius * tangent),
                ("v[1]", speed),
                ("v[2]", speed * tangent + radius * secant_squared),
                ("force", 2.0 * secant_squared * (speed + radius * tangent)),
            ];
            if observed {
                expected.extend([
                    ("observed[1]", 2.0 * radius),
                    ("observed[2]", 2.0 * radius * tangent),
                ]);
            }
            for (name, expected) in expected {
                let column = result.names.iter().position(|n| n == name).unwrap();
                let actual = result.data[column][row];
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{name} at {time}, speed {speed}, {solver_mode:?}: {actual} != {expected}"
                );
            }
        }
    }
}

#[test]
fn atan2_constraint_through_one_observed_state_tensor() {
    check_angular_motion(0.0, true);
    check_angular_motion(1.0, true);
}
