use rumoca::Compiler;
use rumoca_sim::{
    SimOptions, SimSolverMode, lower_dae_for_simulation, simulate_dae_with_diagnostics,
};
use rumoca_solver::SimExecutionPolicy;

const SOURCE: &str = r#"
model AffineSwitchingCircuit
  parameter Real resistance = 20;
  parameter Real snubberResistance = 100;
  parameter Real capacitance = 0.005;
  parameter Real onResistance = 1e-5;
  parameter Real offConductance = 1e-5;
  parameter Real threshold = 0.8;
  parameter Real unitVoltage = 1;
  parameter Real unitCurrent = 1;
  Real capacitorVoltage(start=0, fixed=true);
  Real capacitorCurrent;
  Real voltage;
  Real current;
  Real forwardCurrent;
  Real reverseCurrent;
  Real forwardSwitch;
  Real reverseSwitch;
  Boolean forwardOff(start=true, fixed=true);
  Boolean reverseOff(start=true, fixed=true);
  Boolean fire;
equation
  fire = (time >= 0.1 and time < 0.225) or (time >= 0.35 and time < 0.475);
  forwardOff = forwardSwitch < 0 or pre(forwardOff) and not fire;
  reverseOff = reverseSwitch < 0 or pre(reverseOff) and not fire;
  voltage = forwardSwitch*unitVoltage*(if forwardOff then 1 else onResistance) + threshold;
  forwardCurrent = forwardSwitch*unitCurrent*(if forwardOff then offConductance else 1)
                   + offConductance*threshold;
  -voltage = reverseSwitch*unitVoltage*(if reverseOff then 1 else onResistance) + threshold;
  reverseCurrent = reverseSwitch*unitCurrent*(if reverseOff then offConductance else 1)
                   + offConductance*threshold;
  voltage = 5*sin(4*3.141592653589793*time) - resistance*current;
  current = forwardCurrent - reverseCurrent + capacitorCurrent;
  voltage = snubberResistance*capacitorCurrent + capacitorVoltage;
  capacitance*der(capacitorVoltage) = capacitorCurrent;
end AffineSwitchingCircuit;
"#;

#[test]
fn affine_switching_relations_have_post_side_memory() {
    let compiled = Compiler::new()
        .model("AffineSwitchingCircuit")
        .compile_str(SOURCE, "affine_switching_circuit.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let roots = &model.problem.events;
    let algebraic = roots
        .root_relation_refresh_roles
        .iter()
        .zip(&roots.root_relation_memory_targets)
        .filter(|(role, _)| **role == rumoca_ir_solve::RootRelationRefreshRole::AlgebraicDependent)
        .collect::<Vec<_>>();
    assert_eq!(algebraic.len(), 2);
    assert!(
        algebraic.iter().all(|(_, target)| target.is_some()),
        "each event-generating relational leaf needs its own buffered post-side owner"
    );
}

#[test]
fn affine_switching_circuit_auto_retains_the_turn_off_relation() {
    check_turn_off_relation(SimExecutionPolicy::Auto);
}

#[test]
fn affine_switching_circuit_interpreter_retains_the_turn_off_relation() {
    check_turn_off_relation(SimExecutionPolicy::Interpreter);
}

#[test]
fn buffered_relations_preserve_strict_and_nonstrict_initial_truth() {
    let source = r#"
model InitialRelations
  Real x(start=0, fixed=true);
  Boolean less;
  Boolean lessEqual;
  Boolean greater;
  Boolean greaterEqual;
equation
  der(x) = 1;
  less = x < 0;
  lessEqual = x <= 0;
  greater = x > 0;
  greaterEqual = x >= 0;
end InitialRelations;
"#;
    let compiled = Compiler::new()
        .model("InitialRelations")
        .compile_str(source, "initial_relations.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.02,
            dt: Some(0.01),
            ..Default::default()
        },
    )
    .unwrap();
    let initial = result.times.iter().rposition(|time| *time == 0.0).unwrap();
    for (name, expected) in [
        ("less", 0.0),
        ("lessEqual", 1.0),
        ("greater", 0.0),
        ("greaterEqual", 1.0),
    ] {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap();
        assert_eq!(
            result.data[index][initial], expected,
            "{name} at initialization"
        );
    }
}

#[test]
fn algebraic_root_accuracy_survives_derivative_coordinate_reuse() {
    let source = r#"
model AmplifiedRoot
  Real integral(start=0, fixed=true);
  Real current;
  Real voltage;
  Boolean negative;
equation
  voltage = 1e6*current;
  current = 0.5e-6*voltage + (time-0.25)*1e-11;
  der(integral) = voltage;
  negative = voltage < 0;
end AmplifiedRoot;
"#;
    let compiled = Compiler::new()
        .model("AmplifiedRoot")
        .compile_str(source, "amplified_root.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let runtime = rumoca_solver::SolveRuntime::new(&model).unwrap();
    assert!(runtime.derivative_settled_coordinate_can_refresh_roots());
    for time in [0.0, 0.5] {
        let expected = 2e-5 * (time - 0.25);
        let roots = runtime
            .eval_root_conditions(time, &[0.0], &model.parameters, 1e-10, 64)
            .unwrap();
        assert_eq!(roots.len(), 1);
        assert!((roots[0] - expected).abs() < 1e-10, "cold: {roots:?}");
        let mut guess = model.initial_y.clone();
        runtime
            .eval_state_derivatives_with_guess(
                time,
                &[0.0],
                &model.parameters,
                &mut guess,
                1e-10,
                64,
            )
            .unwrap();
        let mut roots = [0.0];
        runtime
            .eval_root_search_conditions_after_derivative_settle_into(
                time,
                &model.parameters,
                &mut guess,
                1e-10,
                64,
                &mut roots,
            )
            .unwrap();
        assert!((roots[0] - expected).abs() < 1e-10, "reused: {roots:?}");
    }
}

fn check_turn_off_relation(execution_policy: SimExecutionPolicy) {
    let compiled = Compiler::new()
        .model("AffineSwitchingCircuit")
        .compile_str(SOURCE, "affine_switching_circuit.mo")
        .unwrap();
    let solver_mode = SimSolverMode::Bdf;
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.6,
            dt: Some(0.002),
            rtol: 1e-8,
            atol: 1e-8,
            solver_mode,
            execution_policy,
            ..Default::default()
        },
    )
    .unwrap();
    for (switch, off) in [
        ("forwardSwitch", "forwardOff"),
        ("reverseSwitch", "reverseOff"),
    ] {
        let switch = result.names.iter().position(|name| name == switch).unwrap();
        let off = result.names.iter().position(|name| name == off).unwrap();
        for (row, &time) in result.times.iter().enumerate() {
            if result.data[switch][row] < -1e-6 {
                assert!(
                    result.data[off][row] > 0.5,
                    "{solver_mode:?}/{execution_policy:?}: {}({time})={} but {}={}",
                    result.names[switch],
                    result.data[switch][row],
                    result.names[off],
                    result.data[off][row],
                );
            }
        }
    }
}
