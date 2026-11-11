//! A source jump at an event is a jump of `delay(u, d)` exactly `d` later
//! (MLS §3.7.2), and `integer` of that delayed value is an event-generating
//! step (MLS §3.7.1, §8.5), so it switches at the transported instant.
//!
//! The pure-discrete fixture below is stepped at the delay time, so its
//! periodic time events are reached by roundoff-coincident adoption rather
//! than by an exact step end. Each tick must still record the source jump as
//! an event-coordinate left/right pair and switch the delayed integer one
//! delay later, as the transport delay of `Modelica.Electrical.Digital` does.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const SOURCE: &str = r#"
model DelayedToggle
  Integer c(start = 0, fixed = true);
  Real xr;
  Integer y;
equation
  when sample(0.5, 1) then
    c = 1 - pre(c);
  end when;
  xr = pre(c);
  y = integer(delay(xr, 0.001));
end DelayedToggle;
"#;

#[test]
fn every_periodic_source_jump_switches_the_delayed_integer() {
    let compiled = Compiler::new()
        .model("DelayedToggle")
        .compile_str(SOURCE, "DelayedToggle.mo")
        .unwrap_or_else(|error| panic!("DelayedToggle compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 10.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("DelayedToggle simulates: {error:?}"));
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == "y")
        .expect("DelayedToggle exposes y");
    let value_at = |time: f64| {
        let row = result
            .times
            .iter()
            .rposition(|sample| *sample <= time)
            .expect("a sample at or before the requested time");
        result.data[column][row]
    };
    // Tick k at 0.5 + k sets c to (k + 1) mod 2; y follows 0.001 later and
    // holds until the next tick's delayed jump.
    let mismatches: Vec<(f64, f64, f64)> = (0..9)
        .map(|k| {
            let time = 1.0 + f64::from(k);
            let expected = f64::from((k + 1) % 2);
            (time, expected, value_at(time))
        })
        .filter(|(_, expected, actual)| expected != actual)
        .collect();
    assert!(
        mismatches.is_empty(),
        "delayed integer missed transported jumps (time, expected, actual): {mismatches:?}"
    );
}

/// `delay(u, d)` is `u(time.start)` up to `time.start + d`, and `u(time.start)`
/// is the solution of the initialization problem (MLS §8.6). A phase-zero
/// `sample` ticks in the first event iteration after initialization (MLS
/// §3.7.5), so the source jump it causes is an event jump at `time.start`
/// that the delay transports to `time.start + d`. The second model carries a
/// continuous state so the integrating host is exercised as well as the
/// pure-discrete one.
const START_TICK_SOURCE: &str = r#"
package StartTick
  model Discrete
    Integer c(start = 0, fixed = true);
    Real xr;
    Real d;
    Integer y;
  equation
    when sample(0, 1) then
      c = 1 - pre(c);
    end when;
    xr = pre(c);
    d = delay(xr, 0.1);
    y = integer(delay(xr, 0.1));
  end Discrete;
  model WithState
    Real s(start = 0, fixed = true);
    discrete Real c(start = 2, fixed = true);
    Real d;
    Integer y;
  equation
    der(s) = 1;
    when sample(0, 1) then
      c = pre(c) + 1;
    end when;
    d = delay(c, 0.1);
    y = integer(delay(c, 0.1));
  end WithState;
end StartTick;
"#;

fn start_tick_value_at(model: &str, name: &str, time: f64) -> f64 {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(START_TICK_SOURCE, "StartTick.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.5,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error:?}"));
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{model} exposes {name}"));
    let row = result
        .times
        .iter()
        .rposition(|sample| *sample <= time)
        .expect("a sample at or before the requested time");
    result.data[column][row]
}

#[test]
fn a_start_tick_jump_reaches_a_discrete_delay_one_delay_later() {
    for (name, before, after) in [("d", 0.0, 1.0), ("y", 0.0, 1.0)] {
        assert_eq!(
            start_tick_value_at("StartTick.Discrete", name, 0.05),
            before,
            "{name} before time.start + d"
        );
        assert_eq!(
            start_tick_value_at("StartTick.Discrete", name, 0.15),
            after,
            "{name} after time.start + d"
        );
    }
}

#[test]
fn a_start_tick_jump_reaches_a_delay_one_delay_later_with_a_state() {
    for (name, before, after) in [("d", 2.0, 3.0), ("y", 2.0, 3.0)] {
        assert_eq!(
            start_tick_value_at("StartTick.WithState", name, 0.05),
            before,
            "{name} before time.start + d"
        );
        assert_eq!(
            start_tick_value_at("StartTick.WithState", name, 0.15),
            after,
            "{name} after time.start + d"
        );
    }
}

/// MLS §3.7.2: a `delay` read inside a conditional arm records its history at
/// every instant, so its operand, and the relation and runtime quotient nested
/// in it, are owned outside the arm. That ownership must survive the checked
/// DAE wire, whose replay moves between arms one parent edge at a time, so a
/// read more than one arm deep is a typed refusal instead of a DAE that does
/// not replay.
const ARM_DELAY: &str = r#"
model ArmDelay
  parameter Integer p = 2;
  Real x(start = 1.0, fixed = true);
  Boolean a = time > 0.2;
  Boolean b = time > 0.4;
  Real y;
equation
  der(x) = 1;
  y = BODY;
end ArmDelay;
"#;

const ARM_DELAYED: &str = "delay(div(integer(x), p) + (if x > 1.5 then 1 else 0), 0.1)";

fn arm_delay(body: &str) -> String {
    ARM_DELAY.replace("BODY", body)
}

#[test]
fn a_delay_one_arm_deep_replays_from_the_wire() {
    let source = arm_delay(&format!("if a then {ARM_DELAYED} else 0"));
    let compiled = Compiler::new()
        .model("ArmDelay")
        .compile_str(&source, "arm_delay.mo")
        .expect("a delay inside one arm constructs checked DAE");
    let wire = serde_json::to_string(&compiled.dae).expect("the arm delay serializes");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("wire replay rebuilds the arm delay owners");
    assert_eq!(
        serde_json::to_string(&decoded).expect("the replayed DAE serializes"),
        wire,
        "a replayed arm delay has one wire representation"
    );
}

#[test]
fn a_delay_two_arms_deep_is_a_typed_refusal() {
    let source = arm_delay(&format!(
        "if a then (if b then {ARM_DELAYED} else 0) else 0"
    ));
    let error = Compiler::new()
        .model("ArmDelay")
        .compile_str(&source, "arm_delay.mo")
        .expect_err("a nested arm delay has no replayable operand owner");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("UnsupportedNestedArmDelay")
            || rendered.contains("more than one conditional arm deep"),
        "the refusal names the nested arm delay: {rendered}"
    );
}
