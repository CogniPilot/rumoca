//! Velocity reconstruction must close the same no-slip system as the source.

use super::{Compiler, SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = include_str!("../../fixtures/index_reduction/TensorNoSlipContact.mo");

#[test]
fn a_fixed_tensor_anchor_does_not_create_derivative_dependencies() {
    let source = include_str!("../../fixtures/index_reduction/TensorFixedAnchor.mo");
    let compiled = Compiler::new()
        .model("TensorFixedAnchor")
        .compile_str(source, "TensorFixedAnchor.mo")
        .unwrap();
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae).unwrap();
    prepared.as_dae().inspect(|view| {
        let states = view
            .variables()
            .filter(|(_, v)| v.role() == rumoca_ir_dae::VariableRole::State)
            .map(|(_, v)| v.name().to_string())
            .collect::<Vec<_>>();
        assert!(
            states.is_empty(),
            "fixed geometry retains states: {states:?}"
        );
    });
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
        let mut checked = 0;
        for (column, name) in result.names.iter().enumerate() {
            let expected = match name.as_str() {
                "anchor[1]" | "position[1]" => 1.0,
                "anchor[2]" | "position[2]" => 2.0,
                "anchor[3]" | "position[3]" => 3.0,
                name if ["anchor_v[", "velocity[", "acceleration[", "relative["]
                    .iter()
                    .any(|prefix| name.starts_with(prefix)) =>
                {
                    0.0
                }
                _ => continue,
            };
            checked += 1;
            for &actual in &result.data[column] {
                assert!(
                    (actual - expected).abs() < 1e-10,
                    "{solver_mode:?} {name}: {actual} != {expected}"
                );
            }
        }
        assert_eq!(checked, 18);
    }
}

#[test]
fn zero_coefficient_derivatives_preserve_singular_matrix_rejection() {
    let source = include_str!("../../fixtures/index_reduction/TensorFixedAnchor.mo")
        .replace("{{2,1,0},{1,3,1},{0,1,2}}", "{{2,1,2},{4,2,4},{1,3,1}}");
    let compiled = Compiler::new()
        .model("TensorFixedAnchor")
        .compile_str(&source, "TensorFixedAnchor.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap_err();
        assert!(
            error.to_string().contains("singular"),
            "{solver_mode:?}: {error}"
        );
    }
}

#[test]
fn no_slip_reconstruction_preserves_source_assertions() {
    let source = SOURCE.replace("theta0 = 0.2", "theta0 = 1.5");
    let compiled = Compiler::new()
        .model("TensorNoSlipContact")
        .compile_str(&source, "TensorNoSlipContact.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.01,
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap_err();
        assert!(
            error.to_string().contains("Contact basis is singular"),
            "{error}"
        );
    }
}

#[test]
fn no_slip_velocity_uses_only_the_independent_state_coordinates() {
    check_motion(SOURCE);
}

#[test]
fn no_slip_state_reconstruction_follows_connector_position_aliases() {
    let source = SOURCE
        .replace("Real r[3]", "Real position[3];\n  Real r[3]")
        .replace("r = {x,y,z};", "position = {x,y,z};\n  position = r;")
        .replace("delta = road - r;", "delta = road - position;");
    check_motion(&source);
}

#[test]
fn no_slip_state_reconstruction_follows_negated_position_aliases() {
    let source = SOURCE
        .replace("Real r[3]", "Real position[3];\n  Real r[3]")
        .replace("r = {x,y,z};", "position = {x,y,z};\n  position = -r;")
        .replace("der(r) = v;", "der(r) = -v;")
        .replace("delta = road - r;", "delta = road - position;");
    check_motion_with_position_sign(&source, -1.0);
}

#[test]
fn no_slip_state_reconstruction_follows_velocity_aliases() {
    check_velocity_alias("");
}

#[test]
fn no_slip_state_reconstruction_follows_negated_velocity_aliases() {
    check_velocity_alias("-");
}

fn check_velocity_alias(sign: &str) {
    let source = SOURCE
        .replace("Real v[3]", "Real joint_v[3];\n  Real v[3]")
        .replace(
            "der(r) = v;",
            &format!("joint_v = {sign}v;\n  joint_v = {sign}der(r);"),
        )
        .replace("vContact = v +", &format!("vContact = {sign}joint_v +"));
    check_motion(&source);
}

fn check_motion(source: &str) {
    check_motion_with_position_sign(source, 1.0);
}

fn check_motion_with_position_sign(source: &str, position_sign: f64) {
    let compiled = Compiler::new()
        .model("TensorNoSlipContact")
        .compile_str(source, "TensorNoSlipContact.mo")
        .unwrap();
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae).unwrap();
    let states = prepared.as_dae().inspect(|view| {
        view.variables()
            .filter(|(_, variable)| variable.role() == rumoca_ir_dae::VariableRole::State)
            .map(|(_, variable)| variable.name().as_str().to_owned())
            .collect::<Vec<_>>()
    });
    assert_eq!(states, ["theta", "x", "y"]);
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
            for (name, expected) in analytical_values(time) {
                let expected = if name.starts_with("r[") {
                    position_sign * expected
                } else {
                    expected
                };
                let column = result.names.iter().position(|value| value == name).unwrap();
                let actual = result.data[column][row];
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{solver_mode:?} {name}({time}): {actual} != {expected}"
                );
            }
        }
    }
}

fn analytical_values(time: f64) -> Vec<(&'static str, f64)> {
    let theta = 0.2 + time;
    let (sin, cos) = theta.sin_cos();
    let y = 0.3 + 0.2_f64.sin() - sin;
    vec![
        ("theta", theta),
        ("x", 0.2),
        ("y", y),
        ("z", cos),
        ("r[1]", 0.2),
        ("r[2]", y),
        ("r[3]", cos),
        ("v[1]", 0.0),
        ("v[2]", -cos),
        ("v[3]", -sin),
        ("force[1]", 0.0),
        ("force[2]", sin),
        ("force[3]", -cos),
        ("s", 0.2),
        ("w", 0.3 + 0.2_f64.sin()),
        ("delta[1]", 0.0),
        ("delta[2]", sin),
        ("delta[3]", -cos),
        ("vContact[1]", 0.0),
        ("vContact[2]", 0.0),
        ("vContact[3]", 0.0),
    ]
}
