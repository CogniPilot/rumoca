//! SPEC_0044 ME-EVENT-008: a relation inside `smooth(0, ..)` that reads an
//! unknown of its own algebraic block owns an event; other `smooth` relations
//! do not.

use rumoca::Compiler;
use rumoca_sim::{
    SimOptions, SimSolverMode, lower_dae_for_simulation, simulate_dae_with_diagnostics,
};

fn root_count(source: &str, model: &str) -> usize {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "loop_guarded_smooth_relations.mo")
        .unwrap();
    lower_dae_for_simulation(&compiled.dae, &SimOptions::default())
        .unwrap()
        .problem
        .events
        .root_conditions
        .output_count()
}

fn simulates(source: &str, model: &str, t_end: f64) -> rumoca_sim::SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "loop_guarded_smooth_relations.mo")
        .unwrap();
    let options = SimOptions {
        t_end,
        solver_mode: SimSolverMode::Auto,
        ..SimOptions::default()
    };
    simulate_dae_with_diagnostics(&compiled.dae, &options).unwrap()
}

/// The Semiconductors.Thyristor gate law: `vGK` guards the branch of its own
/// defining equation, so the scalar block is implicit in `vGK`.
const THYRISTOR_GATE: &str = r#"
model ThyristorGate
  parameter Real VGT = 0.7;
  parameter Real IGT = 5e-3;
  parameter Real vRef = 0.65;
  Real iGK;
  Real vGK;
equation
  iGK = 6e-3 * sin(10 * time);
  vGK = smooth(0, if vGK < vRef then VGT / IGT * iGK else vRef ^ 2 / VGT + iGK * (VGT - vRef) / IGT);
end ThyristorGate;
"#;

/// The IdealizedOpAmpLimited limiter inside a positive-feedback loop: the
/// guard reads `v_in`, an unknown of the loop that also solves `v_out`.
const LIMITER_LOOP: &str = r#"
model LimiterLoop
  parameter Real V0 = 15000;
  parameter Real vps = 15;
  parameter Real vns = -15;
  parameter Real k = 0.5;
  Real v_in;
  Real v_out;
equation
  v_in = k * v_out - 10 * sin(20 * time);
  v_out = smooth(0, if V0 * v_in > vps then vps else if V0 * v_in < vns then vns else V0 * v_in);
end LimiterLoop;
"#;

/// A loop-guarded relation of a once-differentiable `smooth(1, ..)`, as in
/// Semiconductors.ZDiode: the residual stays differentiable across the
/// branches, so the relation keeps the MLS default and owns no event.
const DIFFERENTIABLE_GUARD: &str = r#"
model DifferentiableGuard
  Real v;
  Real i;
equation
  i = smooth(1, if v > 1 then 2 * v - 1 else v ^ 2);
  v + i = 3 * sin(time);
end DifferentiableGuard;
"#;

/// A `smooth` relation over a state alone: no algebraic block owns it.
const STATE_GUARD: &str = r#"
model StateGuard
  Real x(start = -1, fixed = true);
  Real y;
equation
  der(x) = 1;
  y = smooth(0, if x < 0 then 0 else x);
end StateGuard;
"#;

#[test]
fn a_self_guarded_scalar_block_owns_its_relation() {
    assert_eq!(root_count(THYRISTOR_GATE, "ThyristorGate"), 1);
    simulates(THYRISTOR_GATE, "ThyristorGate", 1.0);
}

#[test]
fn a_loop_guarded_limiter_owns_its_relations() {
    assert_eq!(root_count(LIMITER_LOOP, "LimiterLoop"), 2);
    // The loop has a fold where the output is at +vps: past it neither the
    // linear nor the upper branch is a solution, so the event must reach the
    // lower branch at once, and the output never leaves the rails.
    let result = simulates(LIMITER_LOOP, "LimiterLoop", 1.0);
    let v_out = result
        .names
        .iter()
        .position(|name| name == "v_out")
        .unwrap();
    assert!(result.data[v_out].iter().all(|v| v.abs() <= 15.0 + 1e-9));
    let after = result.times.iter().position(|t| *t > 0.0425).unwrap();
    assert_eq!(result.data[v_out][after], -15.0);
}

#[test]
fn a_differentiable_smooth_guard_owns_no_event() {
    assert_eq!(root_count(DIFFERENTIABLE_GUARD, "DifferentiableGuard"), 0);
    simulates(DIFFERENTIABLE_GUARD, "DifferentiableGuard", 2.0);
}

#[test]
fn a_smooth_relation_over_states_owns_no_event() {
    assert_eq!(root_count(STATE_GUARD, "StateGuard"), 0);
    simulates(STATE_GUARD, "StateGuard", 2.0);
}

/// The same limiter loop with the strict `noEvent` guard of
/// IdealizedOpAmpLimited(strict = true), as in OpAmps.Multivibrator: MLS 3.7.3
/// forbids localizing the switch, and past the fold the loop has no branch the
/// model states: construction records it (ES016) and the run fails with a
/// typed fold error where the branch ends, instead of passing it by accident.
const NO_EVENT_LIMITER_LOOP: &str = r#"
model NoEventLimiterLoop
  parameter Real V0 = 15000;
  parameter Real vps = 15;
  parameter Real vns = -15;
  parameter Real k = 0.5;
  Real v_in;
  Real v_out;
equation
  v_in = k * v_out - 10 * sin(20 * time);
  v_out = smooth(0, noEvent(if V0 * v_in > vps then vps else if V0 * v_in < vns then vns else V0 * v_in));
end NoEventLimiterLoop;
"#;

/// A `noEvent` relation over an input of the block, not its own unknown:
/// the block stays explicit in the guard, so it compiles and simulates.
const NO_EVENT_OPEN_LIMITER: &str = r#"
model NoEventOpenLimiter
  Real v_in;
  Real v_out;
equation
  v_in = 10 * sin(20 * time);
  v_out = smooth(0, noEvent(if v_in > 1 then 1 else if v_in < -1 then -1 else v_in));
end NoEventOpenLimiter;
"#;

#[test]
fn a_no_event_guard_that_switches_its_own_loop_fails_with_a_typed_fold() {
    let compiled = Compiler::new()
        .model("NoEventLimiterLoop")
        .compile_str(NO_EVENT_LIMITER_LOOP, "loop_guarded_smooth_relations.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let guards = &model.problem.continuous.unlocalizable_guards;
    assert_eq!(guards.len(), 1, "{guards:?}");
    assert!(guards[0].warning().contains("noEvent"));
    let options = SimOptions {
        t_end: 1.0,
        solver_mode: SimSolverMode::Auto,
        ..SimOptions::default()
    };
    let Err(error) = simulate_dae_with_diagnostics(&compiled.dae, &options) else {
        panic!("the loop has no stated branch past its fold");
    };
    let text = error.to_string();
    assert!(text.contains("[ES016]") && text.contains("fold"), "{text}");
}

/// A `noEvent` guard over another scalar of the vector its block solves, as
/// in SpacePhasors.Blocks.ToPolar: the block is explicit in the guard.
const NO_EVENT_OTHER_SCALAR: &str = r#"
model NoEventOtherScalar
  Real u[2] = {sin(time), cos(time)};
  Real y[2];
equation
  y[1] = sqrt(u[1]^2 + u[2]^2);
  y[2] = if noEvent(y[1] <= 1e-12) then 0 else atan2(u[2], u[1]);
end NoEventOtherScalar;
"#;

#[test]
fn a_no_event_guard_over_another_scalar_of_its_vector_is_not_a_fact() {
    let compiled = Compiler::new()
        .model("NoEventOtherScalar")
        .compile_str(NO_EVENT_OTHER_SCALAR, "loop_guarded_smooth_relations.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    assert!(model.problem.continuous.unlocalizable_guards.is_empty());
    simulates(NO_EVENT_OTHER_SCALAR, "NoEventOtherScalar", 1.0);
}

#[test]
fn a_no_event_guard_over_block_inputs_compiles() {
    simulates(NO_EVENT_OPEN_LIMITER, "NoEventOpenLimiter", 1.0);
}

#[test]
fn limiter_event_geometry_is_independent_of_output_cadence() {
    let compiled = Compiler::new()
        .model("LimiterLoop")
        .compile_str(LIMITER_LOOP, "loop_guarded_smooth_relations.mo")
        .unwrap();
    for dt in [0.001, 0.003] {
        let options = SimOptions {
            t_end: 1.0,
            dt: Some(dt),
            solver_mode: SimSolverMode::Auto,
            ..SimOptions::default()
        };
        let result = simulate_dae_with_diagnostics(&compiled.dae, &options).unwrap();
        let output = result
            .names
            .iter()
            .position(|name| name == "v_out")
            .unwrap();
        for (time, value) in result.times.iter().zip(&result.data[output]) {
            assert!(
                value.abs() <= 15.0 + 1e-9,
                "dt={dt}, t={time}, v_out={value}"
            );
            // After each positive-feedback upper fold, the outgoing valid
            // solution is the lower rail, irrespective of observation cadence.
            if (0.043..0.055).contains(time)
                || (0.358..0.370).contains(time)
                || (0.673..0.685).contains(time)
                || (0.987..0.999).contains(time)
            {
                assert!(
                    (*value + 15.0).abs() < 1e-9,
                    "dt={dt}, t={time}: upper fold did not reach lower rail: {value}"
                );
            }
        }
    }
}
