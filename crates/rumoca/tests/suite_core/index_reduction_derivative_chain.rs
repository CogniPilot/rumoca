//! MLS §12.7.1: higher-order annotations apply to a compiler-issued chain.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package SuppliedDerivatives
  function position
    input Real q;
    input Real floor;
    output Real x;
  algorithm
    x := if q > floor then q else floor;
    annotation(derivative=first);
  end position;
  function first
    input Real q;
    input Real floor;
    input Real q_d;
    input Real floor_d;
    output Real x_d;
  algorithm
    x_d := if q > floor then q_d else floor_d;
    annotation(derivative(order=2)=second);
  end first;
  function second
    input Real q;
    input Real floor;
    input Real q_d;
    input Real floor_d;
    input Real q_dd;
    input Real floor_dd;
    output Real x_dd;
  algorithm
    x_dd := if q > floor then q_dd else floor_dd;
  end second;
end SuppliedDerivatives;
model DerivativeChainKinematics
  Real q(start=0.4, fixed=true, stateSelect=StateSelect.always);
  Real v(start=0.3, fixed=true, stateSelect=StateSelect.always);
  Real x;
  Real velocity;
  Real acceleration;
equation
  der(q) = v;
  der(v) = -q;
  x = SuppliedDerivatives.position(q, 0.1);
  velocity = der(x);
  acceleration = der(velocity);
end DerivativeChainKinematics;
"#;

#[test]
fn first_order_annotation_preserves_the_kinematic_velocity() {
    let source = SOURCE
        .replace("  Real acceleration;\n", "")
        .replace("  acceleration = der(velocity);\n", "");
    check_motion(&source, 0.4, false);
}

#[test]
fn second_order_annotation_preserves_both_kinematic_derivatives() {
    check_motion(SOURCE, 0.4, true);
    check_motion(&SOURCE.replace("start=0.4", "start=-0.4"), -0.4, true);
}

#[test]
fn tensor_derivative_chain_retains_its_whole_argument_history() {
    let mut source = SOURCE.to_owned();
    for name in ["q", "floor", "q_d", "floor_d", "q_dd", "floor_dd"] {
        source = source.replace(
            &format!("input Real {name};"),
            &format!("input Real {name}[2];"),
        );
    }
    for name in ["x", "x_d", "x_dd", "velocity", "acceleration"] {
        source = source.replace(&format!("Real {name};"), &format!("Real {name}[2];"));
    }
    source = source
        .replace("q > floor", "q[1] > floor[1]")
        .replace("position(q, 0.1)", "position({q, 2*q}, {0.1, 0.2})");
    check_motion_shape(&source, 0.4, true, true);
    check_motion_shape(&source.replace("start=0.4", "start=-0.4"), -0.4, true, true);
}

#[test]
fn retained_primal_call_preserves_its_assertion() {
    let source = SOURCE.replace(
        "x := if",
        "assert(abs(q-floor) > 0.2, \"position domain\");\n    x := if",
    );
    check_motion(&source, 0.4, true);
    let source = source.replace("start=0.4", "start=0.15");
    let compiled = Compiler::new()
        .model("DerivativeChainKinematics")
        .compile_str(&source, "derivative_chain_assertion.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                t_end: 0.1,
                ..Default::default()
            },
        )
        .expect_err("retaining a position constraint must retain its call assertion");
        assert!(
            error.to_string().to_lowercase().contains("assert"),
            "{error}"
        );
    }
}

#[test]
fn direct_call_cannot_claim_the_higher_order_annotation_context() {
    let source = square_source()
        .replace(
            "SuppliedDerivatives.position(q, 0.1)",
            "SuppliedDerivatives.first(q, 0.1, 2*v, 0.0)",
        )
        .replace("  Real acceleration;\n", "")
        .replace("  acceleration = der(velocity);\n", "");
    let compiled = Compiler::new()
        .model("DerivativeChainKinematics")
        .compile_str(&source, "direct_derivative_call.mo")
        .unwrap();
    let error = rumoca_phase_structural::prepare_for_solve(&compiled.dae)
        .err()
        .expect("a direct call has no proof that q_d equals der(q)");
    assert!(
        error.to_string().contains("structurally singular"),
        "{error}"
    );
}

#[test]
fn nonlinear_second_derivative_uses_all_prior_argument_orders() {
    let source = square_source();
    check_motion_law(&source, 0.4, true, false, true);
    check_motion_law(
        &source.replace("start=0.4", "start=-0.4"),
        -0.4,
        true,
        false,
        true,
    );
}

fn square_source() -> String {
    SOURCE
        .replace("then q else floor", "then q*q else floor*floor")
        .replace("then q_d else floor_d", "then 2*q*q_d else 2*floor*floor_d")
        .replace(
            "then q_dd else floor_dd",
            "then 2*q_d*q_d+2*q*q_dd else 2*floor_d*floor_d+2*floor*floor_dd",
        )
}

fn check_motion(source: &str, start: f64, second: bool) {
    check_motion_shape(source, start, second, false);
}

fn check_motion_shape(source: &str, start: f64, second: bool, tensor: bool) {
    check_motion_law(source, start, second, tensor, false);
}

fn check_motion_law(source: &str, start: f64, second: bool, tensor: bool, square: bool) {
    let compiled = Compiler::new()
        .model("DerivativeChainKinematics")
        .compile_str(source, "derivative_chain_kinematics.mo")
        .unwrap();
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae)
        .unwrap_or_else(|error| panic!("{error:?}"));
    prepared.inspect(|system| {
        let states: usize = system
            .view
            .variables()
            .filter(|(_, v)| v.role() == rumoca_ir_dae::VariableRole::State)
            .map(|(_, v)| v.value_type().scalar_count().unwrap())
            .sum();
        let constraints: usize = system
            .manifold
            .iter()
            .map(|id| {
                system
                    .view
                    .expression(*id)
                    .unwrap()
                    .value_type()
                    .scalar_count()
                    .unwrap()
            })
            .sum();
        assert_eq!(states - constraints, 2);
        if second {
            assert_second_call_shape(system.view, tensor);
        }
    });
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let trace = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                t_end: 0.1,
                dt: Some(0.01),
                ..Default::default()
            },
        )
        .unwrap_or_else(|error| panic!("{solver_mode:?}: {error}"));
        for (row, &time) in trace.times.iter().enumerate() {
            let q = start * time.cos() + 0.3 * time.sin();
            let v = -start * time.sin() + 0.3 * time.cos();
            for (name, expected) in expected_motion(q, v, second, tensor, square) {
                let column = trace.names.iter().position(|value| value == &name).unwrap();
                let actual = trace.data[column][row];
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{solver_mode:?} {name}({time}): {actual} != {expected}"
                );
            }
        }
    }
}

fn expected_motion(q: f64, v: f64, second: bool, tensor: bool, square: bool) -> Vec<(String, f64)> {
    let mut expected = vec![("q".to_owned(), q), ("v".to_owned(), v)];
    let x = q.max(0.1);
    let velocity = if q > 0.1 { v } else { 0.0 };
    let acceleration = if q > 0.1 { -q } else { 0.0 };
    let mut kinematic = vec![
        ("x", if square { x * x } else { x }),
        (
            "velocity",
            if square { 2.0 * x * velocity } else { velocity },
        ),
    ];
    if second {
        kinematic.push((
            "acceleration",
            if square {
                2.0 * (velocity * velocity + x * acceleration)
            } else {
                acceleration
            },
        ));
    }
    for (name, value) in kinematic {
        if tensor {
            expected.extend([
                (format!("{name}[1]"), value),
                (format!("{name}[2]"), 2.0 * value),
            ]);
        } else {
            expected.push((name.to_owned(), value));
        }
    }
    expected
}

fn assert_second_call_shape(view: rumoca_ir_dae::DaeView<'_>, tensor: bool) {
    let targets = (0..view.function_count())
        .flat_map(|index| {
            view.function(view.function_id(index).unwrap())
                .unwrap()
                .derivatives()
        })
        .filter(|link| link.order() == 2)
        .map(|link| link.target())
        .collect::<Vec<_>>();
    let dimensions: &[u32] = if tensor { &[2] } else { &[] };
    let mut count = 0;
    for index in 0..view.expression_count() {
        let node = view.expression(view.expression_id(index).unwrap()).unwrap();
        if let rumoca_ir_dae::ExpressionOperation::Call {
            function,
            arguments,
            ..
        } = node.operation()
            && targets.contains(&function)
        {
            assert_eq!(node.value_type().dimensions(), dimensions);
            assert_eq!(arguments.len(), 6);
            for argument in arguments.iter() {
                assert_eq!(
                    view.expression(argument).unwrap().value_type().dimensions(),
                    dimensions
                );
            }
            count += 1;
        }
    }
    assert!(
        count > 0,
        "the prepared equations must call the issued second derivative"
    );
}
