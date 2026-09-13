//! An implicit tensor block fixes a dependent coordinate without new states.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model ImplicitTensorAuxiliary
  Real theta(start=0.2,fixed=true,stateSelect=StateSelect.always);
  Real z(start=0.2);
  Real v(start=1);
  Real force;
  Real q[2];
equation
  der(theta) = 1;
  der(z) = v;
  der(v) = force;
  q*{1.0,1.0} = theta;
  q*{1.0,-1.0} = 3*theta;
  0 = q[2] + z;
end ImplicitTensorAuxiliary;
"#;

#[test]
fn coupled_tensor_auxiliary_preserves_linear_motion_with_bdf() {
    check_motion(SOURCE, SimSolverMode::Bdf);
}

#[test]
fn coupled_tensor_auxiliary_preserves_linear_motion_with_rk() {
    check_motion(SOURCE, SimSolverMode::RkLike);
}

#[test]
fn signed_zero_equations_share_derivative_and_auxiliary_normalization() {
    let source = SOURCE
        .replace("der(theta) = 1;", "0 = -(der(theta)-1);")
        .replace("der(z) = v;", "0 = 0+(der(z)-v);")
        .replace("der(v) = force;", "0 = (der(v)-force)-0;")
        .replace("q*{1.0,1.0} = theta;", "0 = theta - q*{1.0,1.0};");
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_motion(&source, solver);
    }
}

#[test]
fn varying_tensor_coefficients_preserve_shape_and_second_derivative() {
    let source = SOURCE.replace(
        "q*{1.0,1.0} = theta;",
        "q*{1.0+theta,2.0+theta} = theta*theta;",
    );
    assert_ne!(source, SOURCE);
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_motion(&source, solver);
    }
}

#[test]
fn a_nonlinear_tensor_block_remains_outside_the_affine_proof() {
    let source = SOURCE.replace("q*{1.0,1.0} = theta;", "q*{q[1],1.0} = theta;");
    let compiled = Compiler::new()
        .model("ImplicitTensorAuxiliary")
        .compile_str(&source, "ImplicitTensorAuxiliary.mo")
        .unwrap();
    let error = rumoca_phase_structural::prepare_for_solve(&compiled.dae)
        .err()
        .expect("unknown-dependent coefficients must not receive a linear reconstruction");
    assert!(
        error.to_string().contains("structurally singular"),
        "{error}"
    );
}

#[test]
fn a_singular_auxiliary_matrix_fails_at_the_checked_execution_boundary() {
    let source = SOURCE.replace("q*{1.0,-1.0} = 3*theta;", "q*{1.0,1.0} = 3*theta;");
    let compiled = Compiler::new()
        .model("ImplicitTensorAuxiliary")
        .compile_str(&source, "ImplicitTensorAuxiliary.mo")
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
        .expect_err("a singular auxiliary matrix must refuse simulation");
        assert!(
            error
                .to_string()
                .contains("tensor linear solve is singular or non-finite"),
            "{error}"
        );
    }
}

fn check_motion(source: &str, solver_mode: SimSolverMode) {
    let compiled = Compiler::new()
        .model("ImplicitTensorAuxiliary")
        .compile_str(source, "ImplicitTensorAuxiliary.mo")
        .unwrap();
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae)
        .unwrap_or_else(|error| panic!("{error:?}"));
    let counts = |view: rumoca_ir_dae::DaeView<'_>| {
        (
            view.variable_count(),
            view.continuous_owners().count(),
            view.variables()
                .filter(|(_, v)| v.role() == rumoca_ir_dae::VariableRole::State)
                .count(),
        )
    };
    assert_eq!(
        compiled.dae.inspect(counts),
        prepared.as_dae().inspect(counts)
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
        let theta = 0.2 + time;
        for (name, expected) in [
            ("theta", theta),
            ("z", theta),
            ("v", 1.0),
            ("force", 0.0),
            ("q[1]", 2.0 * theta),
            ("q[2]", -theta),
        ] {
            let column = result
                .names
                .iter()
                .position(|actual| actual == name)
                .unwrap();
            let actual = result.data[column][row];
            assert!(
                (actual - expected).abs() < 1e-6,
                "{solver_mode:?} {name}({time}): {actual} != {expected}"
            );
        }
    }
}
