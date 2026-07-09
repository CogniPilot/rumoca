//! Regression tests for discrete `when`-equation event firing of `pre()`
//! accumulators.
//!
//! These cover the periodic-source counter idiom used by
//! `Modelica.*.Sources.{Trapezoid,Pulse,SawTooth}Voltage` and friends:
//!
//! ```modelica
//! when time >= (pre(count) + 1) * period + startTime then
//!   count = pre(count) + 1;
//! end when;
//! ```
//!
//! Three distinct bugs are guarded here:
//!   1. A constant-threshold `when time >= c` accumulator must fire exactly
//!      once per crossing (no right-limit double-application of the update).
//!   2. A `time >= f(pre(count))` guard must not degenerate to the
//!      always-false `c and not c` edge (MLS §3.7.3 time-event firing).
//!   3. `pre(count)` must advance after each event so the self-rescheduling
//!      threshold moves forward and the `when` fires on every period.
//!   4. Vectorized when-conditions apply the same scalar activation semantics
//!      to each element before OR-combining the guards.
//!   5. Non-target `pre(...)` values in arithmetic factors must keep ordinary
//!      edge semantics rather than becoming level-sensitive guards.
//!   6. Stateful root-event handling must commit `pre(...)` slots after each
//!      event so the next dynamic threshold observes the updated discrete
//!      state.
//!   7. Assignment deltas that depend on another `pre(...)` value must not
//!      refire on unrelated later events.
//!   8. Initial events commit `pre(...)` slots after the event settles, so a
//!      self-rescheduling guard that is already true at `t_start` continues to
//!      advance on later periods.
//!   9. Indexed array thresholds keep structured subscript information through
//!      DAE/Solve lowering instead of being treated as opaque names.

use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae};

const CONST_THRESHOLD_COUNTER: &str = r#"
model ConstThresholdCounter
  discrete Integer count(start = 0, fixed = true);
equation
  when time >= 0.3 then
    count = pre(count) + 1;
  end when;
end ConstThresholdCounter;
"#;

const CONST_THRESHOLD_WITH_LATER_EVENT: &str = r#"
model ConstThresholdWithLaterEvent
  discrete Integer count(start = 0, fixed = true);
  discrete Integer unrelated(start = 0, fixed = true);
equation
  when time >= 0.3 then
    count = pre(count) + 1;
  end when;

  when time >= 0.6 then
    unrelated = pre(unrelated) + 1;
  end when;
end ConstThresholdWithLaterEvent;
"#;

const SELF_RESCHEDULING_COUNTER: &str = r#"
model SelfReschedulingCounter
  parameter Real period = 0.1;
  parameter Real startTime = -0.035;
  discrete Integer count(start = 0, fixed = true);
equation
  when time >= (pre(count) + 1) * period + startTime then
    count = pre(count) + 1;
  end when;
end SelfReschedulingCounter;
"#;

const INITIAL_SELF_RESCHEDULING_COUNTER: &str = r#"
model InitialSelfReschedulingCounter
  parameter Real period = 0.1;
  parameter Real startTime = -0.1;
  discrete Integer count(start = 0, fixed = true);
equation
  when time >= (pre(count) + 1) * period + startTime then
    count = pre(count) + 1;
  end when;
end InitialSelfReschedulingCounter;
"#;

const SAMPLE_COUNTER: &str = r#"
model SampleCounter
  discrete Integer count(start = 0, fixed = true);
equation
  when sample(0.1, 0.1) then
    count = pre(count) + 1;
  end when;
end SampleCounter;
"#;

const ZERO_PHASE_SAMPLE_COUNTER: &str = r#"
model ZeroPhaseSampleCounter
  discrete Integer count(start = 0, fixed = true);
equation
  when sample(0.0, 0.1) then
    count = pre(count) + 1;
  end when;
end ZeroPhaseSampleCounter;
"#;

const VECTOR_SELF_RESCHEDULING_COUNTER: &str = r#"
model VectorSelfReschedulingCounter
  parameter Real period = 0.1;
  parameter Real startTime = -0.035;
  discrete Integer count(start = 0, fixed = true);
equation
  when {time >= (pre(count) + 1) * period + startTime, false} then
    count = pre(count) + 1;
  end when;
end VectorSelfReschedulingCounter;
"#;

const INDEXED_PARAMETER_THRESHOLD_WITH_LATER_EVENT: &str = r#"
model IndexedParameterThresholdWithLaterEvent
  parameter Real deadline[2] = {0.3, 0.9};
  discrete Integer count(start = 0, fixed = true);
  discrete Integer unrelated(start = 0, fixed = true);
equation
  when time >= deadline[1] then
    count = pre(count) + 1;
  end when;

  when time >= 0.6 then
    unrelated = pre(unrelated) + 1;
  end when;
end IndexedParameterThresholdWithLaterEvent;
"#;

const NONLINEAR_PRE_FACTOR_WITH_LATER_EVENT: &str = r#"
model NonlinearPreFactorWithLaterEvent
  discrete Integer count(start = 0, fixed = true);
  discrete Real scale(start = -1.0, fixed = true);
  discrete Integer unrelated(start = 0, fixed = true);
equation
  when time >= 0.1 + (pre(count) + 1) * (pre(scale) + 1) then
    count = pre(count) + 1;
    scale = pre(scale);
  end when;

  when time >= 0.2 then
    unrelated = pre(unrelated) + 1;
  end when;
end NonlinearPreFactorWithLaterEvent;
"#;

const NON_TARGET_PRE_DELTA_WITH_LATER_EVENT: &str = r#"
model NonTargetPreDeltaWithLaterEvent
  discrete Integer count(start = 0, fixed = true);
  discrete Integer shift(start = -2, fixed = true);
  discrete Integer unrelated(start = 0, fixed = true);
equation
  when time >= (pre(count) + 1) * 0.1 then
    count = pre(count) + 1 + pre(shift);
    shift = pre(shift);
  end when;

  when time >= 0.2 then
    unrelated = pre(unrelated) + 1;
  end when;
end NonTargetPreDeltaWithLaterEvent;
"#;

const STATEFUL_SELF_RESCHEDULING_COUNTER: &str = r#"
model StatefulSelfReschedulingCounter
  parameter Real period = 0.1;
  Real x(start = 0.0, fixed = true);
  discrete Integer count(start = 0, fixed = true);
equation
  der(x) = 1.0;

  when time >= (pre(count) + 1) * period then
    count = pre(count) + 1;
  end when;
end StatefulSelfReschedulingCounter;
"#;

const STATEFUL_INITIAL_SELF_RESCHEDULING_COUNTER: &str = r#"
model StatefulInitialSelfReschedulingCounter
  parameter Real period = 0.1;
  parameter Real startTime = -0.1;
  Real x(start = 0.0, fixed = true);
  discrete Integer count(start = 0, fixed = true);
equation
  der(x) = 1.0;

  when time >= (pre(count) + 1) * period + startTime then
    count = pre(count) + 1;
  end when;
end StatefulInitialSelfReschedulingCounter;
"#;

const PRE_THRESHOLD_WITH_LATER_EVENT: &str = r#"
model PreThresholdWithLaterEvent
  discrete Real deadline(start = 0.3, fixed = true);
  discrete Integer count(start = 0, fixed = true);
  discrete Integer unrelated(start = 0, fixed = true);
equation
  when time >= pre(deadline) then
    deadline = pre(deadline);
    count = pre(count) + 1;
  end when;

  when time >= 0.6 then
    unrelated = pre(unrelated) + 1;
  end when;
end PreThresholdWithLaterEvent;
"#;

fn trace_values<'a>(sim: &'a rumoca_sim::SimResult, name: &str) -> &'a [f64] {
    let idx = sim
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("trace should contain `{name}`; names={:?}", sim.names));
    &sim.data[idx]
}

fn value_at(sim: &rumoca_sim::SimResult, name: &str, t: f64) -> f64 {
    let values = trace_values(sim, name);
    // Last recorded sample at or before `t` (right-continuous hold).
    let mut result = values[0];
    for (idx, &time) in sim.times.iter().enumerate() {
        if time <= t + 1.0e-9 {
            result = values[idx];
        } else {
            break;
        }
    }
    result
}

#[test]
fn const_threshold_when_accumulator_fires_exactly_once() {
    let compiled = rumoca::Compiler::new()
        .model("ConstThresholdCounter")
        .compile_str(CONST_THRESHOLD_COUNTER, "const_threshold_counter.mo")
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.05),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate");

    let count = trace_values(&sim, "count");
    let last = *count.last().expect("count trace should be non-empty");
    // MLS §8.3.5.1: a single false->true crossing fires the `when` once. A
    // right-limit re-application would double-count to 2.
    assert!(
        (last - 1.0).abs() <= 1.0e-9,
        "count must reach exactly 1 (fired once), got {last}"
    );
    assert!(
        value_at(&sim, "count", 0.2) <= 0.5,
        "count must stay 0 before the 0.3 threshold"
    );
}

#[test]
fn const_threshold_guard_does_not_refire_on_later_unrelated_event() {
    let compiled = rumoca::Compiler::new()
        .model("ConstThresholdWithLaterEvent")
        .compile_str(
            CONST_THRESHOLD_WITH_LATER_EVENT,
            "const_threshold_with_later_event.mo",
        )
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.8,
            dt: Some(0.05),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate");

    assert!(
        (value_at(&sim, "count", 0.8) - 1.0).abs() <= 1.0e-9,
        "constant time-threshold guard must not refire at the unrelated 0.6 event"
    );
    assert!(
        (value_at(&sim, "unrelated", 0.8) - 1.0).abs() <= 1.0e-9,
        "unrelated later event should still fire"
    );
}

#[test]
fn self_rescheduling_counter_advances_every_period() {
    let compiled = rumoca::Compiler::new()
        .model("SelfReschedulingCounter")
        .compile_str(SELF_RESCHEDULING_COUNTER, "self_rescheduling_counter.mo")
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.5,
            dt: Some(0.02),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate");

    // First event at (0+1)*0.1 - 0.035 = 0.065, then every 0.1 thereafter as
    // pre(count) advances and the threshold re-arms.
    let checks = [
        (0.05, 0.0),
        (0.10, 1.0),
        (0.20, 2.0),
        (0.30, 3.0),
        (0.40, 4.0),
    ];
    for (t, expected) in checks {
        let actual = value_at(&sim, "count", t);
        assert!(
            (actual - expected).abs() <= 1.0e-9,
            "count at t={t} should be {expected}, got {actual} \
             (self-rescheduling `pre(count)` threshold must advance each event)"
        );
    }
}

#[test]
fn initial_self_rescheduling_counter_advances_after_t_start_event() {
    let compiled = rumoca::Compiler::new()
        .model("InitialSelfReschedulingCounter")
        .compile_str(
            INITIAL_SELF_RESCHEDULING_COUNTER,
            "initial_self_rescheduling_counter.mo",
        )
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.36,
            dt: Some(0.02),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate");

    let checks = [
        (0.00, 1.0),
        (0.05, 1.0),
        (0.15, 2.0),
        (0.25, 3.0),
        (0.35, 4.0),
    ];
    for (t, expected) in checks {
        let actual = value_at(&sim, "count", t);
        assert!(
            (actual - expected).abs() <= 1.0e-9,
            "initial self-rescheduling count at t={t} should be {expected}, got {actual} \
             (initial event must commit pre slots before runtime threshold scheduling)"
        );
    }
}

#[test]
fn vector_self_rescheduling_counter_advances_every_period() {
    let compiled = rumoca::Compiler::new()
        .model("VectorSelfReschedulingCounter")
        .compile_str(
            VECTOR_SELF_RESCHEDULING_COUNTER,
            "vector_self_rescheduling_counter.mo",
        )
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.5,
            dt: Some(0.02),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate");

    let checks = [
        (0.05, 0.0),
        (0.10, 1.0),
        (0.20, 2.0),
        (0.30, 3.0),
        (0.40, 4.0),
    ];
    for (t, expected) in checks {
        let actual = value_at(&sim, "count", t);
        assert!(
            (actual - expected).abs() <= 1.0e-9,
            "vector when count at t={t} should be {expected}, got {actual} \
             (each scalar vector guard must keep its own edge activation)"
        );
    }
}

#[test]
fn indexed_parameter_threshold_does_not_refire_on_later_unrelated_event() {
    let compiled = rumoca::Compiler::new()
        .model("IndexedParameterThresholdWithLaterEvent")
        .compile_str(
            INDEXED_PARAMETER_THRESHOLD_WITH_LATER_EVENT,
            "indexed_parameter_threshold_with_later_event.mo",
        )
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.8,
            dt: Some(0.05),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate");

    assert!(
        (value_at(&sim, "count", 0.8) - 1.0).abs() <= 1.0e-9,
        "indexed array threshold must preserve subscript structure and fire once"
    );
    assert!(
        (value_at(&sim, "unrelated", 0.8) - 1.0).abs() <= 1.0e-9,
        "unrelated later event should still fire"
    );
}

#[test]
fn nonlinear_pre_factor_guard_does_not_refire_on_later_unrelated_event() {
    let compiled = rumoca::Compiler::new()
        .model("NonlinearPreFactorWithLaterEvent")
        .compile_str(
            NONLINEAR_PRE_FACTOR_WITH_LATER_EVENT,
            "nonlinear_pre_factor_with_later_event.mo",
        )
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.3,
            dt: Some(0.02),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate");

    assert!(
        (value_at(&sim, "count", 0.3) - 1.0).abs() <= 1.0e-9,
        "non-target pre factors must keep edge semantics and avoid refiring"
    );
    assert!(
        (value_at(&sim, "unrelated", 0.3) - 1.0).abs() <= 1.0e-9,
        "unrelated later event should still fire"
    );
}

#[test]
fn non_target_pre_delta_guard_does_not_refire_on_later_unrelated_event() {
    let compiled = rumoca::Compiler::new()
        .model("NonTargetPreDeltaWithLaterEvent")
        .compile_str(
            NON_TARGET_PRE_DELTA_WITH_LATER_EVENT,
            "non_target_pre_delta_with_later_event.mo",
        )
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.3,
            dt: Some(0.02),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate");

    assert!(
        (value_at(&sim, "count", 0.3) + 1.0).abs() <= 1.0e-9,
        "assignment deltas depending on another pre value must not refire"
    );
    assert!(
        (value_at(&sim, "unrelated", 0.3) - 1.0).abs() <= 1.0e-9,
        "unrelated later event should still fire"
    );
}

#[test]
fn stateful_self_rescheduling_counter_advances_every_period() {
    let compiled = rumoca::Compiler::new()
        .model("StatefulSelfReschedulingCounter")
        .compile_str(
            STATEFUL_SELF_RESCHEDULING_COUNTER,
            "stateful_self_rescheduling_counter.mo",
        )
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.36,
            dt: Some(0.02),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate");

    let checks = [(0.05, 0.0), (0.15, 1.0), (0.25, 2.0), (0.35, 3.0)];
    for (t, expected) in checks {
        let actual = value_at(&sim, "count", t);
        assert!(
            (actual - expected).abs() <= 1.0e-9,
            "stateful count at t={t} should be {expected}, got {actual} \
             (root-handled events must commit pre slots for the next threshold)"
        );
    }
}

#[test]
fn stateful_initial_self_rescheduling_counter_advances_after_t_start_event() {
    let compiled = rumoca::Compiler::new()
        .model("StatefulInitialSelfReschedulingCounter")
        .compile_str(
            STATEFUL_INITIAL_SELF_RESCHEDULING_COUNTER,
            "stateful_initial_self_rescheduling_counter.mo",
        )
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.36,
            dt: Some(0.02),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate");

    let checks = [
        (0.00, 1.0),
        (0.05, 1.0),
        (0.15, 2.0),
        (0.25, 3.0),
        (0.35, 4.0),
    ];
    for (t, expected) in checks {
        let actual = value_at(&sim, "count", t);
        assert!(
            (actual - expected).abs() <= 1.0e-9,
            "stateful initial self-rescheduling count at t={t} should be {expected}, got {actual} \
             (stateful initialization must commit pre slots before runtime roots)"
        );
    }
}

#[test]
fn sample_when_accumulator_advances_once_per_tick() {
    let compiled = rumoca::Compiler::new()
        .model("SampleCounter")
        .compile_str(SAMPLE_COUNTER, "sample_counter.mo")
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.36,
            dt: Some(0.02),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate");

    let checks = [(0.05, 0.0), (0.15, 1.0), (0.25, 2.0), (0.35, 3.0)];
    for (t, expected) in checks {
        let actual = value_at(&sim, "count", t);
        assert!(
            (actual - expected).abs() <= 1.0e-9,
            "count at t={t} should be {expected}, got {actual} \
             (sample(start, interval) must fire once per tick)"
        );
    }
}

#[test]
fn rk_like_sample_when_accumulator_advances_once_per_tick() {
    let compiled = rumoca::Compiler::new()
        .model("SampleCounter")
        .compile_str(SAMPLE_COUNTER, "sample_counter.mo")
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.36,
            dt: Some(0.02),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate with RK-like solver");

    let checks = [(0.05, 0.0), (0.15, 1.0), (0.25, 2.0), (0.35, 3.0)];
    for (t, expected) in checks {
        let actual = value_at(&sim, "count", t);
        assert!(
            (actual - expected).abs() <= 1.0e-9,
            "RK-like count at t={t} should be {expected}, got {actual} \
             (scheduled sample condition memory must clear between ticks)"
        );
    }
}

#[test]
fn rk_like_zero_phase_sample_when_accumulator_advances_after_initial_tick() {
    let compiled = rumoca::Compiler::new()
        .model("ZeroPhaseSampleCounter")
        .compile_str(ZERO_PHASE_SAMPLE_COUNTER, "zero_phase_sample_counter.mo")
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.36,
            dt: Some(0.02),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate with RK-like solver");

    let checks = [
        (0.00, 1.0),
        (0.05, 1.0),
        (0.15, 2.0),
        (0.25, 3.0),
        (0.35, 4.0),
    ];
    for (t, expected) in checks {
        let actual = value_at(&sim, "count", t);
        assert!(
            (actual - expected).abs() <= 1.0e-9,
            "RK-like zero-phase count at t={t} should be {expected}, got {actual} \
             (sample(0, interval) must re-arm after the initialization tick)"
        );
    }
}

#[test]
fn pre_threshold_guard_does_not_refire_on_later_unrelated_event() {
    let compiled = rumoca::Compiler::new()
        .model("PreThresholdWithLaterEvent")
        .compile_str(
            PRE_THRESHOLD_WITH_LATER_EVENT,
            "pre_threshold_with_later_event.mo",
        )
        .expect("model should compile");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.8,
            dt: Some(0.05),
            ..SimOptions::default()
        },
    )
    .expect("model should simulate");

    assert!(
        (value_at(&sim, "count", 0.8) - 1.0).abs() <= 1.0e-9,
        "pre-based time-threshold guard must not refire at the unrelated 0.6 event"
    );
    assert!(
        (value_at(&sim, "unrelated", 0.8) - 1.0).abs() <= 1.0e-9,
        "unrelated later event should still fire"
    );
}
