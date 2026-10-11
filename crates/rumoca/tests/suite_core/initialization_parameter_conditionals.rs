//! MLS §8.6/§18.6: only immutable conditions select initialization dependencies.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, SimSolverMode, simulate_dae};
use rumoca_solver::SimExecutionPolicy;

// The opposite dependency directions are the original Fluid.Pipes pattern:
// m_flow_nominal reads m_flow_small only in the absolute arm, while
// m_flow_small reads m_flow_nominal only in the relative arm.
const SOURCE: &str = r#"model ConditionalParameterOrder
  parameter Boolean relative = false annotation(Evaluate=true);
  parameter Real nominal = if relative then 1 else 100*small;
  parameter Real small = if relative then 0.0001*nominal else 0.01;
  Real x(start=0, fixed=true);
equation
  der(x) = nominal + small;
end ConditionalParameterOrder;
"#;

fn assert_trace(result: &SimResult, slope: f64, policy: SimExecutionPolicy) {
    assert_eq!(result.times.len(), 11);
    assert_eq!(result.times.first(), Some(&0.0));
    assert_eq!(result.times.last(), Some(&0.1));
    let channel = result.names.iter().position(|name| name == "x").unwrap();
    assert_eq!(result.data[channel].len(), result.times.len());
    for (sample, (&time, &actual)) in result.times.iter().zip(&result.data[channel]).enumerate() {
        assert!((time - sample as f64 * 0.01).abs() < 1e-14);
        let expected = slope * time;
        assert!(
            actual.is_finite() && (actual - expected).abs() < 1e-8,
            "{policy:?}: x({time})={actual}, expected {expected}"
        );
    }
}

fn simulate_branch(source: &str, slope: f64) {
    let compiled = Compiler::new()
        .model("ConditionalParameterOrder")
        .compile_str(source, "conditional_parameter_order.mo")
        .expect("original opposite-arm parameter bindings compile");
    for execution_policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let result = simulate_dae(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                atol: 1e-10,
                rtol: 1e-10,
                solver_mode: SimSolverMode::RkLike,
                execution_policy,
                ..Default::default()
            },
        )
        .expect("selected immutable binding graph is acyclic");
        assert_trace(&result, slope, execution_policy);
    }
}

#[test]
fn immutable_opposite_arm_parameter_dependencies_preserve_both_traces() {
    simulate_branch(SOURCE, 1.01);
    simulate_branch(
        &SOURCE.replace("relative = false", "relative = true"),
        1.0001,
    );
}

fn assert_cycle(source: &str) {
    let compiled = Compiler::new()
        .model("ConditionalParameterOrder")
        .compile_str(source, "conditional_parameter_cycle.mo")
        .expect("parameter bindings reach checked Solve dependency admission");
    let error = simulate_dae(&compiled.dae, &SimOptions::default())
        .expect_err("a reachable or tunable dependency cycle needs a coupled owner");
    assert!(
        error
            .to_string()
            .contains("cyclic initialization parameter definitions"),
        "{error}"
    );
}

#[test]
fn tunable_condition_cannot_discard_a_parameter_dependency_cycle() {
    assert_cycle(&SOURCE.replace("Evaluate=true", "Evaluate=false"));
}

#[test]
fn selected_immutable_parameter_cycle_still_requires_a_coupled_owner() {
    let source = SOURCE
        .replace("relative = false", "relative = true")
        .replace("then 1 else 100*small", "then small else 1");
    assert_cycle(&source);
}

#[test]
fn compact_binder_condition_retains_dependencies_without_a_scalar_point() {
    let source = r#"model ConditionalParameterOrder
      parameter Real gain = 2;
      parameter Real values[2] = {if i > 1 then gain else 1 for i in 1:2};
      Real x(start=0, fixed=true);
    equation
      der(x) = sum(values);
    end ConditionalParameterOrder;"#;
    simulate_branch(source, 3.0);
}
