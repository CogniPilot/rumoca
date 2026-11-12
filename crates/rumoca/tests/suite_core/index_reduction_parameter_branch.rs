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
fn indexed_parameter_guard_survives_nested_function_substitution() {
    let source = indexed_branch_source("directions", "2");
    check_motion(&source, 0.4, 1.0);
    check_motion(&source.replace("start=0.4", "start=-0.4"), -0.4, -1.0);
}

#[test]
fn indexed_varying_guards_do_not_receive_a_parameter_proof() {
    for (array, index) in [
        ("{false, q > 0}", "2"),
        ("directions", "if time < 0.05 then 1 else 2"),
    ] {
        let source = indexed_branch_source(array, index);
        let compiled = Compiler::new()
            .model("ParameterBranchKinematics")
            .compile_str(&source, "varying_indexed_guard.mo")
            .unwrap();
        let error = rumoca_phase_structural::prepare_for_solve(&compiled.dae)
            .err()
            .expect("a varying array or index cannot prove a fixed branch");
        assert!(
            error.to_string().contains("structurally singular"),
            "{error}"
        );
    }
}

fn indexed_branch_source(array: &str, index: &str) -> String {
    format!(
        "function selectPosition
          input Boolean positive; input Real q; output Real x;
          algorithm x := if positive then q else -q;
        end selectPosition;
        function indexedPosition
          input Boolean directions[2]; input Integer selected; input Real q; output Real x;
          algorithm x := selectPosition(directions[selected], q);
        end indexedPosition;
        {}",
        SOURCE
            .replace(
                "  Real x;",
                "  parameter Boolean directions[2] = {not positive, positive};\n  Real x;"
            )
            .replace(
                "if positive then q else -q",
                &format!("indexedPosition({array}, {index}, q)")
            )
    )
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

/// MLS §8.5: a relation changes only at its event, so a relation guard is
/// piecewise constant in time and the branch derivatives hold between events,
/// across a time event and a state event alike.
#[test]
fn relation_guards_differentiate_branch_wise_between_events() {
    for (guard, t_end) in [("time < 0.05", 0.1), ("q > 0", 3.0)] {
        let source = SOURCE.replace("if positive then", &format!("if {guard} then"));
        let compiled = Compiler::new()
            .model("ParameterBranchKinematics")
            .compile_str(&source, "relation_guard.mo")
            .unwrap();
        let options = SimOptions {
            t_end,
            dt: Some(0.01),
            ..Default::default()
        };
        let result = match simulate_dae_with_diagnostics(&compiled.dae, &options) {
            Ok(result) => result,
            Err(error) => panic!("{guard}: {error}"),
        };
        assert!(result.times.len() > 5, "{guard} produced an output grid");
        let column = |name: &str| {
            let index = result.names.iter().position(|value| value == name).unwrap();
            &result.data[index]
        };
        for (row, &time) in result.times.iter().enumerate() {
            let q = 0.4 * time.cos() + 0.3 * time.sin();
            let v = -0.4 * time.sin() + 0.3 * time.cos();
            let crossing = if guard.starts_with("time") {
                0.05
            } else {
                (-0.4_f64 / 0.3).atan() + std::f64::consts::PI
            };
            if (time - crossing).abs() < 1e-6 {
                continue;
            }
            let sign = if time < crossing { 1.0 } else { -1.0 };
            for (name, expected) in [
                ("x", sign * q),
                ("velocity", sign * v),
                ("acceleration", -sign * q),
            ] {
                let actual = column(name)[row];
                assert!(
                    (actual - expected).abs() < 1e-5,
                    "{guard} {name}({time}): {actual} != {expected}"
                );
            }
        }
    }
}

/// A relation that owns no event is not fixed between events, so its branch
/// derivatives carry no proof and index reduction keeps refusing them: a
/// `noEvent` guard, a relation inside a `noEvent` conditional, and a relation
/// inside `smooth` that owns no root.
#[test]
fn relation_guards_without_an_owned_event_keep_the_refusal() {
    for replacement in [
        "if noEvent(q > 0) then q else -q",
        "noEvent(if q > 0 then q else -q)",
        "smooth(0, if q > 0 then q else -q)",
        "smooth(2, if q > 0 then q else -q)",
    ] {
        let source = SOURCE.replace("if positive then q else -q", replacement);
        let compiled = Compiler::new()
            .model("ParameterBranchKinematics")
            .compile_str(&source, "no_event_guard.mo")
            .unwrap();
        let Err(error) = rumoca_phase_structural::prepare_for_solve(&compiled.dae) else {
            panic!("{replacement}: an eventless guard needs a refusal");
        };
        assert!(
            error.to_string().contains("structurally singular"),
            "{replacement}: {error}"
        );
    }
}

/// A `for` binder compared with a parameter selects one fixed branch per
/// scalar instance (MLS §3.6.5). End-to-end smoke coverage only, not a guard
/// for the binder rule itself: scalar row owners (flatten's binder-selected
/// families) already pass without the binder rule in `parameter_conditionals`,
/// which `dae_transform::tests::binder_guards` pins directly.
#[test]
fn binder_selected_family_rows_keep_branch_derivatives_end_to_end() {
    let source = "model BinderBranchKinematics
          parameter Integer k = 1;
          Real q(start=0.4, fixed=true, stateSelect=StateSelect.always);
          Real v(start=0.3, fixed=true, stateSelect=StateSelect.always);
          Real x[2];
          Real velocity[2];
        equation
          der(q) = v;
          der(v) = -q;
          for i in 1:2 loop
            x[i] = if i == k then q else 0;
          end for;
          velocity = der(x);
        end BinderBranchKinematics;";
    let compiled = Compiler::new()
        .model("BinderBranchKinematics")
        .compile_str(source, "binder_branch.mo")
        .unwrap();
    if let Err(error) = rumoca_phase_structural::prepare_for_solve(&compiled.dae) {
        panic!("a binder guard is fixed per instance: {error:?}");
    }
    let options = SimOptions {
        t_end: 0.1,
        dt: Some(0.01),
        ..Default::default()
    };
    let result = simulate_dae_with_diagnostics(&compiled.dae, &options).unwrap();
    let column = |name: &str| {
        let index = result.names.iter().position(|value| value == name);
        &result.data[index.unwrap_or_else(|| panic!("missing output {name}"))]
    };
    for (row, &time) in result.times.iter().enumerate() {
        let q = 0.4 * time.cos() + 0.3 * time.sin();
        let v = -0.4 * time.sin() + 0.3 * time.cos();
        for (name, expected) in [
            ("x[1]", q),
            ("x[2]", 0.0),
            ("velocity[1]", v),
            ("velocity[2]", 0.0),
        ] {
            let actual = column(name)[row];
            assert!(
                (actual - expected).abs() < 1e-6,
                "{name}({time}): {actual} != {expected}"
            );
        }
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
    let prepared = match rumoca_phase_structural::prepare_for_solve(&compiled.dae) {
        Ok(prepared) => prepared,
        Err(error) => panic!("{error:?}"),
    };
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
        let options = SimOptions {
            solver_mode,
            t_end: 0.1,
            dt: Some(0.01),
            ..Default::default()
        };
        let result = match simulate_dae_with_diagnostics(&compiled.dae, &options) {
            Ok(result) => result,
            Err(error) => panic!("{solver_mode:?}: {error}"),
        };
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
