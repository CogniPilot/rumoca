//! State events that chatter: a relation whose dynamics on both sides push the
//! state back onto its surface (a sliding mode) re-crosses immediately after
//! every event. Each crossing located to the location tolerance advanced time
//! by about 1e-8 s, so these models used to run for minutes per simulated
//! second; the ME host now applies such crossings at the scan coordinate.

use rumoca::Compiler;
use rumoca_sim::{
    SimOptions, SimResult, SimSolverMode, SimulationSession, reset_step_counts,
    simulate_dae_with_diagnostics, step_counts,
};

/// Below the limit `i` is pushed up, above it down: it slides along `i = -1`.
const SLIDE: &str = "
model Slide
  parameter Real e = -1;
  Real i(start = -0.9, fixed = true);
equation
  der(i) = if i < -1 then 1 else e;
end Slide;
";

/// The same limit as a clamping integrator: once below it, `i` stops. The
/// crossing is isolated and is located to the full tolerance.
const CLAMP: &str = "
model Clamp
  parameter Real e = -1;
  Real i(start = -0.9, fixed = true);
equation
  der(i) = if i < -1 then max(0, e) else e;
end Clamp;
";

/// The anti-windup integrator of a controller whose error is an input.
const SLIDE_INPUT: &str = "
model SlideInput
  input Real e;
  Real i(start = -0.9, fixed = true);
equation
  der(i) = if i < -1 then 1 else e;
end SlideInput;
";

fn simulate(source: &str, model: &str, solver_mode: SimSolverMode) -> SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("compile {model}: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 2.0,
            dt: Some(0.01),
            max_wall_seconds: Some(30.0),
            solver_mode,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} on {solver_mode:?} failed: {error:?}"))
}

fn channel<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("missing `{name}` in {:?}", result.names));
    &result.data[index]
}

#[test]
fn a_sliding_mode_stays_on_its_surface_with_bounded_events() {
    for solver_mode in [SimSolverMode::RkLike, SimSolverMode::Bdf] {
        reset_step_counts();
        let result = simulate(SLIDE, "Slide", solver_mode);
        let counts = step_counts();
        let i = channel(&result, "i");
        // Each crossing is applied at the end of the accepted step it occurs
        // in, so `i` overshoots the surface by at most one restart step
        // (`dt`) times `|der(i)| = 1`.
        for (time, value) in result.times.iter().zip(i) {
            if *time > 0.2 {
                assert!(
                    (value + 1.0).abs() < 2.0e-2,
                    "{solver_mode:?}: i({time}) = {value} left the surface"
                );
            }
        }
        // Two seconds of sliding: one event per accepted step, never one per
        // location tolerance.
        assert!(counts.root_hits < 20_000, "{solver_mode:?}: {counts:?}");
    }
}

#[test]
fn an_isolated_limit_crossing_is_still_located_exactly() {
    for solver_mode in [SimSolverMode::RkLike, SimSolverMode::Bdf] {
        let result = simulate(CLAMP, "Clamp", solver_mode);
        let end = *channel(&result, "i").last().expect("a trajectory");
        assert!(
            (end + 1.0).abs() < 1.0e-9,
            "{solver_mode:?}: the clamp settled at {end}"
        );
    }
}

#[test]
fn an_interactive_session_slides_with_a_changing_input() {
    let compiled = Compiler::new()
        .model("SlideInput")
        .compile_str(SLIDE_INPUT, "SlideInput.mo")
        .unwrap_or_else(|error| panic!("compile SlideInput: {error:?}"));
    let mut session = SimulationSession::new(
        &compiled.dae,
        SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.0,
            max_wall_seconds: Some(30.0),
            initial_inputs: vec![("e".to_owned(), -1.0)],
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("session SlideInput: {error:?}"));
    reset_step_counts();
    for step in 0..400_u32 {
        let jitter = 1.0e-6 * f64::from(step % 13);
        session
            .set_input("e", -1.0 + jitter)
            .unwrap_or_else(|error| panic!("set_input: {error:?}"));
        session
            .step(0.005)
            .unwrap_or_else(|error| panic!("step {step}: {error:?}"));
    }
    let i = session
        .get("i")
        .unwrap_or_else(|error| panic!("get i: {error:?}"))
        .expect("`i` is a session variable");
    assert!((i + 1.0).abs() < 1.0e-2, "i = {i} left the surface");
    assert!(step_counts().root_hits < 20_000, "{:?}", step_counts());
}
