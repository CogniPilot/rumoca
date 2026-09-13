//! MLS §3.6.5/§3.8.3/§8.6: an initialized parameter selects one fixed branch.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model ParameterBranchKinematics
  parameter Boolean positive(fixed=false);
  Real q(start=0.4, fixed=true, stateSelect=StateSelect.always);
  Real v(start=0.3, fixed=true, stateSelect=StateSelect.always);
  Real x;
  Real velocity;
  Real acceleration;
initial equation
  positive = q >= 0;
equation
  der(q) = v;
  der(v) = -q;
  x = if positive then q else -q;
  velocity = der(x);
  acceleration = der(velocity);
end ParameterBranchKinematics;
"#;

#[test]
fn unconditional_alias_preserves_kinematic_derivatives() {
    let source = SOURCE
        .replace("  parameter Boolean positive(fixed=false);\n", "")
        .replace("initial equation\n  positive = q >= 0;\n", "")
        .replace("if positive then q else -q", "q");
    check_motion(&source, 0.4, 1.0);
}

#[test]
fn literal_parameter_branch_preserves_kinematic_derivatives() {
    let source = SOURCE
        .replace("positive(fixed=false)", "positive=true")
        .replace("initial equation\n  positive = q >= 0;\n", "");
    check_motion(&source, 0.4, 1.0);
}

#[test]
fn initialized_parameter_branch_preserves_both_kinematic_derivatives() {
    check_motion(SOURCE, 0.4, 1.0);
    check_motion(&SOURCE.replace("start=0.4", "start=-0.4"), -0.4, -1.0);
}

#[test]
fn parameter_guard_survives_function_argument_substitution() {
    let source = format!(
        "function selectPosition\ninput Boolean positive;\ninput Real q;\noutput Real x;\nalgorithm\nx := if positive then q else -q;\nend selectPosition;\n{}",
        SOURCE.replace("if positive then q else -q", "selectPosition(positive, q)")
    );
    check_motion(&source, 0.4, 1.0);
    check_motion(&source.replace("start=0.4", "start=-0.4"), -0.4, -1.0);
}

#[test]
fn tensor_parameter_branches_preserve_values_and_shaped_zero_derivatives() {
    let source = SOURCE
        .replace("Real x;", "Real x[2];")
        .replace("Real velocity;", "Real velocity[2];")
        .replace("Real acceleration;", "Real acceleration[2];")
        .replace("then q else -q", "then {q, 2*q} else {0.0, 0.0}");
    check_motion_shape(&source, 0.4, 1.0, true);
    check_motion_shape(&source.replace("start=0.4", "start=-0.4"), -0.4, 0.0, true);
}

#[test]
fn varying_guards_do_not_receive_the_parameter_derivative_proof() {
    for guard in ["time < 0.05", "q > 0"] {
        let source = SOURCE.replace("if positive then", &format!("if {guard} then"));
        let compiled = Compiler::new()
            .model("ParameterBranchKinematics")
            .compile_str(&source, "varying_guard.mo")
            .unwrap();
        let error = rumoca_phase_structural::prepare_for_solve(&compiled.dae)
            .err()
            .expect("a varying branch needs a separate event-aware differentiation proof");
        assert!(
            error.to_string().contains("structurally singular"),
            "{error}"
        );
    }
}

fn check_motion(source: &str, start: f64, sign: f64) {
    check_motion_shape(source, start, sign, false);
}

fn check_motion_shape(source: &str, start: f64, sign: f64, tensor: bool) {
    let compiled = Compiler::new()
        .model("ParameterBranchKinematics")
        .compile_str(source, "parameter_branch_kinematics.mo")
        .unwrap();
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae)
        .unwrap_or_else(|error| panic!("{error:?}"));
    assert_eq!(
        prepared.as_dae().inspect(|view| view
            .variables()
            .filter(|(_, variable)| variable.role() == rumoca_ir_dae::VariableRole::State)
            .count()),
        2
    );
    if tensor {
        prepared.as_dae().inspect(assert_tensor_conditionals);
    }
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
        .unwrap_or_else(|error| panic!("{solver_mode:?}: {error}"));
        for (row, &time) in result.times.iter().enumerate() {
            let q = start * time.cos() + 0.3 * time.sin();
            let v = -start * time.sin() + 0.3 * time.cos();
            let mut expected = vec![("q", q), ("v", v)];
            if tensor {
                expected.extend([
                    ("x[1]", sign * q),
                    ("x[2]", 2.0 * sign * q),
                    ("velocity[1]", sign * v),
                    ("velocity[2]", 2.0 * sign * v),
                    ("acceleration[1]", -sign * q),
                    ("acceleration[2]", -2.0 * sign * q),
                ]);
            } else {
                expected.extend([
                    ("x", sign * q),
                    ("velocity", sign * v),
                    ("acceleration", -sign * q),
                ]);
            }
            for (name, expected) in expected {
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

fn assert_tensor_conditionals(view: rumoca_ir_dae::DaeView<'_>) {
    let mut count = 0;
    for index in 0..view.expression_count() {
        let expression = view.expression(view.expression_id(index).unwrap()).unwrap();
        if matches!(
            expression.operation(),
            rumoca_ir_dae::ExpressionOperation::Conditional(_)
        ) && expression.value_type().scalar_type() == rumoca_ir_dae::ScalarType::Real
        {
            assert_eq!(expression.value_type().dimensions(), &[2]);
            count += 1;
        }
    }
    assert!(
        count >= 3,
        "position and both derivatives retain tensor conditionals"
    );
}
