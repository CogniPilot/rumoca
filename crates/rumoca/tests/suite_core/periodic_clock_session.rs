//! A live session announces periodic clock ticks from the clock definition and
//! the current time, never from a preparation horizon (MLS 8.3.5, 16.3; SPEC_0044
//! ME-HOST).

use rumoca::Compiler;
use rumoca_sim::{
    SessionCommand, SessionEvent, SimExecutionEngine, SimExecutionPolicy, SimNativeRefusal,
    SimOptions, SimSolverMode, SimulationSession, simulate_with_diagnostics,
};

const CLOCK: &str = r#"
model PeriodicControllerClock
  parameter Real period = 0.01;
  discrete output Real ticks(start=0, fixed=true);
algorithm
  when sample(0, period) then
    ticks := pre(ticks)+1;
  end when;
end PeriodicControllerClock;
"#;

const TWO_CLOCKS: &str = r#"
model TwoClocks
  discrete output Real a(start=0, fixed=true);
  discrete output Real b(start=0, fixed=true);
algorithm
  when sample(0, 0.01) then
    a := pre(a)+1;
  end when;
  when sample(0.025, 0.1) then
    b := pre(b)+1;
  end when;
end TwoClocks;
"#;

const RESET: &str = r#"
model ClockReset
  output Real x(start=0, fixed=true);
  discrete output Real n(start=0, fixed=true);
  discrete output Real seen(start=0, fixed=true);
equation
  der(x) = 1;
  when sample(0.1, 0.1) then
    n = pre(n) + 1;
    seen = pre(n);
    reinit(x, 0);
  end when;
end ClockReset;
"#;

const INTEGRATED: &str = r#"
model IntegratedClock
  Real s(start=0, fixed=true);
  discrete output Real ticks(start=0, fixed=true);
equation
  der(s) = ticks;
  when sample(0, 0.01) then
    ticks = pre(ticks) + 1;
  end when;
end IntegratedClock;
"#;

fn options(t_end: f64) -> SimOptions {
    SimOptions {
        solver_mode: SimSolverMode::RkLike,
        t_end,
        dt: Some(0.005),
        rtol: 1e-8,
        atol: 1e-8,
        ..SimOptions::default()
    }
}

fn dae_of(source: &str, model: &str) -> std::sync::Arc<rumoca_ir_dae::Dae> {
    Compiler::new()
        .model(model)
        .compile_str(source, "periodic_clock_session.mo")
        .unwrap()
        .dae
}

fn session(source: &str, model: &str, opts: SimOptions) -> SimulationSession {
    SimulationSession::new(&dae_of(source, model), opts).unwrap()
}

fn get(session: &SimulationSession, name: &str) -> f64 {
    session.get(name).unwrap().unwrap()
}

#[test]
fn the_controller_clock_ticks_past_the_initial_horizon_when_stepped() {
    // The horizon the options carry (t_end = 1) is not a property of the clock.
    let mut s = session(CLOCK, "PeriodicControllerClock", options(1.0));
    assert_eq!(get(&s, "ticks"), 1.0);
    let checkpoints = [
        (0.1, 11.0),
        (0.5, 51.0),
        (1.0, 101.0),
        (1.1, 111.0),
        (2.0, 201.0),
        (3.0, 301.0),
    ];
    for (target, expected) in checkpoints {
        while s.time() < target - 1e-9 {
            s.step(0.005).unwrap();
        }
        assert_eq!(get(&s, "ticks"), expected, "ticks at t = {target}");
    }
}

#[test]
fn a_large_jump_counts_every_tick_exactly_once() {
    let mut jump = session(CLOCK, "PeriodicControllerClock", options(1.0));
    jump.advance_to(3.0).unwrap();
    assert_eq!(get(&jump, "ticks"), 301.0);
    // Advancing again by a jump spanning 1000 ticks adds exactly 1000.
    jump.advance_to(13.0).unwrap();
    assert_eq!(get(&jump, "ticks"), 1301.0);
    // The count is independent of how the same span is partitioned.
    let mut ragged = session(CLOCK, "PeriodicControllerClock", options(1.0));
    for target in [0.003, 0.0101, 0.7, 0.7000001, 2.5, 3.0, 9.99, 13.0] {
        ragged.advance_to(target).unwrap();
    }
    assert_eq!(get(&ragged, "ticks"), 1301.0);
    // A reset restarts the same schedule from the new start.
    jump.reset(0.0).unwrap();
    jump.advance_to(3.0).unwrap();
    assert_eq!(get(&jump, "ticks"), 301.0);
}

#[test]
fn two_clocks_with_different_periods_and_an_offset_each_keep_their_lattice() {
    let mut s = session(TWO_CLOCKS, "TwoClocks", options(1.0));
    let checkpoints = [
        (0.5, 51.0, 5.0),
        (1.0, 101.0, 10.0),
        (1.1, 111.0, 11.0),
        (2.0, 201.0, 20.0),
        (3.0, 301.0, 30.0),
    ];
    for (target, a, b) in checkpoints {
        s.advance_to(target).unwrap();
        assert_eq!((get(&s, "a"), get(&s, "b")), (a, b), "t = {target}");
    }
}

#[test]
fn pre_and_reinit_are_evaluated_once_per_tick_beyond_the_horizon() {
    let mut s = session(RESET, "ClockReset", options(1.0));
    for (target, n) in [(0.55, 5.0), (1.05, 10.0), (2.05, 20.0), (3.05, 30.0)] {
        s.advance_to(target).unwrap();
        assert_eq!(get(&s, "n"), n, "t = {target}");
        assert_eq!(get(&s, "seen"), n - 1.0, "pre(n) at t = {target}");
        assert!((get(&s, "x") - 0.05).abs() < 1e-6, "reinit at t = {target}");
    }
}

#[test]
fn batch_and_session_agree_on_the_schedule_and_the_integral() {
    let dae = dae_of(INTEGRATED, "IntegratedClock");
    let batch = simulate_with_diagnostics(&dae, &options(3.0)).unwrap();
    let column = |name: &str| batch.names.iter().position(|n| n == name).unwrap();
    let last = batch.times.len() - 1;
    assert_eq!(batch.times[last], 3.0);
    let (ticks, s) = (
        batch.data[column("ticks")][last],
        batch.data[column("s")][last],
    );
    assert_eq!(ticks, 301.0);

    // The session's horizon is shorter than the run it is asked for.
    let mut live = SimulationSession::new(&dae, options(1.0)).unwrap();
    live.advance_to(3.0).unwrap();
    assert_eq!(get(&live, "ticks"), ticks);
    let live_s = get(&live, "s");
    assert_eq!(
        live_s.to_bits(),
        s.to_bits(),
        "integral differs: batch {s:e}, session {live_s:e}"
    );
}

#[test]
fn the_session_reports_the_engine_it_selected() {
    // Auto on a model with a continuous state selects compiled execution.
    let stateful = session(RESET, "ClockReset", options(1.0));
    let mut stateful = stateful;
    stateful.advance_to(0.5).unwrap();
    let receipt = stateful.execution_receipt();
    assert!(
        stateful.execution_receipt().declined.is_empty(),
        "the compiled engine declined nothing on this model"
    );
    assert_ne!(receipt.engine, SimExecutionEngine::Interpreter);
    assert_eq!(receipt.refusal, None);

    // A pure-discrete model is refused compiled execution, with the reason.
    let discrete = session(CLOCK, "PeriodicControllerClock", options(1.0));
    let receipt = discrete.execution_receipt();
    assert_eq!(receipt.engine, SimExecutionEngine::Interpreter);
    assert_eq!(receipt.refusal, Some(SimNativeRefusal::NoContinuousStates));

    // A pinned interpreter is a request, not a refusal.
    let pinned = session(
        RESET,
        "ClockReset",
        SimOptions {
            execution_policy: SimExecutionPolicy::Interpreter,
            ..options(1.0)
        },
    );
    let receipt = pinned.execution_receipt();
    assert_eq!(receipt.engine, SimExecutionEngine::Interpreter);
    assert_eq!(receipt.refusal, None);
}

#[test]
fn the_hello_event_carries_the_engine_receipt_on_the_wire() {
    let mut s = session(CLOCK, "PeriodicControllerClock", options(1.0));
    let event = s.apply(SessionCommand::Hello {
        protocol_version: rumoca_sim::SESSION_PROTOCOL_VERSION,
    });
    let SessionEvent::Hello { engine, .. } = &event else {
        panic!("expected hello, got {event:?}");
    };
    assert_eq!(*engine, s.execution_receipt());
    let wire = serde_json::to_value(&event).unwrap();
    assert_eq!(
        wire["engine"],
        serde_json::json!({"engine": "interpreter", "refusal": "no_continuous_states", "declined": []})
    );
}
