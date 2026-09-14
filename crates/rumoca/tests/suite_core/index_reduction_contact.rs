//! Coupled tensor contact geometry and its explicitly solved diagnostic control.

mod linear_solve;
mod no_slip;

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = include_str!("../fixtures/index_reduction/TensorContact.mo");
const FUNCTION_SOURCE: &str = include_str!("../fixtures/index_reduction/TensorContactFunction.mo");
const ANNOTATED_SOURCE: &str =
    include_str!("../fixtures/index_reduction/TensorContactAnnotated.mo");
const ROLLING_SOURCE: &str = include_str!("../fixtures/index_reduction/RollingContact.mo");
const ANGULAR_RATE_SOURCE: &str = include_str!("../fixtures/index_reduction/AngularRateContact.mo");
const SCOPED_SOURCE: &str = include_str!("../fixtures/index_reduction/ScopedContact.mo");
const TENSOR_STATE_SOURCE: &str = include_str!("../fixtures/index_reduction/TensorStateContact.mo");

#[test]
fn tensor_state_contact_reconstructs_dependent_position_and_velocity() {
    for source in [
        TENSOR_STATE_SOURCE.to_owned(),
        TENSOR_STATE_SOURCE.replace("delta = road - r;", "road - r = delta;"),
        TENSOR_STATE_SOURCE.replace("delta = road - r;", "{0,0,0} = delta - (road - r);"),
    ] {
        for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
            check_motion(
                &source,
                "TensorStateContact",
                solver,
                3,
                tensor_state_values,
            );
        }
    }
}

#[test]
fn tensor_state_reconstruction_retains_source_assertions() {
    let source = TENSOR_STATE_SOURCE.replace("abs(axis[3]) < 0.99", "theta < 0.25");
    let compiled = Compiler::new()
        .model("TensorStateContact")
        .compile_str(&source, "TensorStateContact.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
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
fn tensor_state_definition_cannot_determine_its_own_free_coordinate() {
    let source = r#"
model FallingTensor
  Real r[3](start={0,0,2});
  Real v[3](start={0,0,3});
  Real x;
  Real y;
  Real z(start=2);
equation
  r = {x,y,z};
  der(r) = v;
  der(v) = {0,0,-1};
end FallingTensor;
"#;
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_motion(source, "FallingTensor", solver, 2, |t| {
            vec![
                ("z", 2.0 + 3.0 * t - 0.5 * t * t),
                ("r[1]", 0.0),
                ("r[2]", 0.0),
                ("r[3]", 2.0 + 3.0 * t - 0.5 * t * t),
                ("v[1]", 0.0),
                ("v[2]", 0.0),
                ("v[3]", 3.0 - t),
            ]
        });
    }
}

fn tensor_state_values(time: f64) -> Vec<(&'static str, f64)> {
    let theta = 0.2 + time;
    let (sin, cos) = theta.sin_cos();
    vec![
        ("theta", theta),
        ("x", 0.2),
        ("y", 0.3),
        ("z", cos),
        ("r[1]", 0.2),
        ("r[2]", 0.3),
        ("r[3]", cos),
        ("v[1]", 0.0),
        ("v[2]", 0.0),
        ("v[3]", -sin),
        ("force[1]", 0.0),
        ("force[2]", 0.0),
        ("force[3]", -cos),
        ("s", 0.2),
        ("w", 0.3 + sin),
        ("delta[1]", 0.0),
        ("delta[2]", sin),
        ("delta[3]", -cos),
    ]
}

#[test]
fn shared_function_derivatives_preserve_nested_call_arguments() {
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_motion(SCOPED_SOURCE, "ScopedContact", solver, 1, scoped_values);
    }
}

fn scoped_values(time: f64) -> Vec<(&'static str, f64)> {
    let theta = 0.2 + time;
    let mut z = 0.0;
    let mut vz = 0.0;
    let mut force = 0.0;
    for angle in [theta, theta + 0.3] {
        z += angle.cos() + (2.0 * angle).cos();
        vz -= angle.sin() + 2.0 * (2.0 * angle).sin();
        force -= angle.cos() + 4.0 * (2.0 * angle).cos();
    }
    vec![("theta", theta), ("z", z), ("vz", vz), ("force", force)]
}

#[test]
fn tensor_angular_rate_map_permits_the_second_contact_derivative() {
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_motion(
            ANGULAR_RATE_SOURCE,
            "AngularRateContact",
            solver,
            2,
            angular_rate_values,
        );
    }
}

fn angular_rate_values(time: f64) -> Vec<(&'static str, f64)> {
    let rate = 0.8 * (-time).exp();
    let theta = 1.0 - rate;
    let (sin, cos) = theta.sin_cos();
    vec![
        ("theta[1]", theta),
        ("theta[2]", 0.3 + time),
        ("omega[1]", 1.0),
        ("omega[2]", 1.0),
        ("rate[1]", rate),
        ("rate[2]", 1.0),
        ("z", cos),
        ("vz", -sin * rate),
        ("force", sin * rate - cos * rate * rate),
    ]
}

#[test]
fn first_derivative_annotation_does_not_block_a_proved_second_derivative() {
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_contact(ANNOTATED_SOURCE, solver, 3);
    }
}

#[test]
fn tensor_rolling_contact_preserves_both_no_slip_constraints() {
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_motion(ROLLING_SOURCE, "RollingContact", solver, 7, rolling_values);
    }
}

#[test]
fn tensor_contact_materializes_a_selected_function_branch() {
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_contact(FUNCTION_SOURCE, solver, 3);
    }
}

#[test]
fn implicit_tensor_contact_preserves_motion_with_bdf() {
    check_contact(SOURCE, SimSolverMode::Bdf, 3);
}

#[test]
fn implicit_tensor_contact_preserves_motion_with_rk() {
    check_contact(SOURCE, SimSolverMode::RkLike, 3);
}

#[test]
fn implicit_tensor_contact_accepts_zero_residual_equation_form() {
    let source = SOURCE.replace(
        "radius = delta*cross(longitudinal,axis);",
        "0 = radius - delta*cross(longitudinal,axis);",
    );
    assert_ne!(source, SOURCE);
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_contact(&source, solver, 3);
    }
}

#[test]
fn explicitly_solved_contact_coordinates_preserve_the_same_motion() {
    let source = SOURCE.replace(
        "0 = delta*axis;\n  0 = delta*longitudinal;\n  radius = delta*cross(longitudinal,axis);",
        "s = 0;\n  w = radius*sin(theta);\n  z = radius*cos(theta);",
    );
    assert_ne!(source, SOURCE);
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_contact(&source, solver, 1);
    }
}

#[test]
fn explicit_contact_height_uses_the_proved_auxiliary_derivative() {
    let source = SOURCE.replace(
        "delta = road - {0,0,z};",
        "delta[1] = s;\n  delta[2] = w;\n  z = -delta[3];",
    );
    assert_ne!(source, SOURCE);
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_contact(&source, solver, 1);
    }
}

fn check_contact(source: &str, solver_mode: SimSolverMode, expected_states: usize) {
    check_motion(
        source,
        "TensorContact",
        solver_mode,
        expected_states,
        contact_values,
    );
}

fn check_motion(
    source: &str,
    model: &str,
    solver_mode: SimSolverMode,
    expected_states: usize,
    values: fn(f64) -> Vec<(&'static str, f64)>,
) {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap();
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae).unwrap();
    let counts = |view: rumoca_ir_dae::DaeView<'_>| {
        (view.variable_count(), view.continuous_owners().count())
    };
    assert_eq!(
        compiled.dae.inspect(counts),
        prepared.as_dae().inspect(counts)
    );
    assert_eq!(
        prepared.as_dae().inspect(|view| view
            .variables()
            .filter(|(_, v)| v.role() == rumoca_ir_dae::VariableRole::State)
            .count()),
        expected_states
    );
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.01),
            solver_mode,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("{solver_mode:?}: {error}"));
    for (row, &time) in result.times.iter().enumerate() {
        for (name, expected) in values(time) {
            let column = result.names.iter().position(|value| value == name).unwrap();
            let actual = result.data[column][row];
            assert!(
                (actual - expected).abs() < 1e-6,
                "{solver_mode:?} {name}({time}): {actual} != {expected}"
            );
        }
    }
}

fn contact_values(time: f64) -> Vec<(&'static str, f64)> {
    let theta = 0.2 + time;
    let (sin, cos) = theta.sin_cos();
    vec![
        ("theta", theta),
        ("z", cos),
        ("vz", -sin),
        ("force", -cos),
        ("s", 0.0),
        ("w", sin),
        ("normal[1]", 0.0),
        ("normal[2]", 0.0),
        ("normal[3]", 1.0),
        ("axis[1]", 0.0),
        ("axis[2]", cos),
        ("axis[3]", sin),
        ("auxiliary[1]", -cos),
        ("auxiliary[2]", 0.0),
        ("auxiliary[3]", 0.0),
        ("longitudinal[1]", -1.0),
        ("longitudinal[2]", 0.0),
        ("longitudinal[3]", 0.0),
        ("road[1]", 0.0),
        ("road[2]", sin),
        ("road[3]", 0.0),
        ("delta[1]", 0.0),
        ("delta[2]", sin),
        ("delta[3]", -cos),
    ]
}

fn rolling_values(time: f64) -> Vec<(&'static str, f64)> {
    let (sin, cos) = (0.2 + time).sin_cos();
    let mut values = contact_values(time);
    for (name, value) in &mut values {
        if matches!(*name, "w" | "road[2]") {
            *value = 0.0;
        }
    }
    values.extend([
        ("x", 0.0),
        ("y", -sin),
        ("vx", 0.0),
        ("vy", -cos),
        ("fx", 0.0),
        ("fy", sin),
        ("lateral[1]", 0.0),
        ("lateral[2]", 1.0),
        ("lateral[3]", 0.0),
        ("vContact[1]", 0.0),
        ("vContact[2]", 0.0),
        ("vContact[3]", 0.0),
    ]);
    values
}
