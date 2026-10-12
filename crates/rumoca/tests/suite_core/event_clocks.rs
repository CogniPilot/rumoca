//! MLS §16.3 event clocks `Clock(condition, startInterval)`: a clocked
//! partition that ticks on the rising edge of a Boolean coordinate.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae};

fn simulate(source: &str, model: &str, t_end: f64) -> SimResult {
    let compiled = match Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
    {
        Ok(compiled) => compiled,
        Err(error) => panic!("{model} compiles: {error:?}"),
    };
    let options = SimOptions {
        t_end,
        dt: Some(0.01),
        ..Default::default()
    };
    let result = match simulate_dae(&compiled.dae, &options) {
        Ok(result) => result,
        Err(error) => panic!("{model} simulates: {error:?}"),
    };
    assert!(result.times.len() > 10, "{model} produced an output grid");
    result
}

fn refusal(source: &str, model: &str) -> String {
    match Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
    {
        Ok(_) => panic!("{model} is refused"),
        Err(error) => format!("{error:?}"),
    }
}

/// The value `name` holds at the last output instant not after `time`.
fn value_at(result: &SimResult, name: &str, time: f64) -> f64 {
    let Some(column) = result.names.iter().position(|candidate| candidate == name) else {
        panic!("result has no column `{name}`");
    };
    let row = result
        .times
        .iter()
        .rposition(|candidate| *candidate <= time + 1.0e-12)
        .expect("an output instant precedes the probe");
    result.data[column][row]
}

/// The clock ticks once per period at the rising edge of `b` (t = 1/12 + k),
/// never at its falling edge. `previous`, `sample`, and `interval()` read the
/// partition's own ticks; `interval()` is the default `startInterval` 0 at the
/// first tick (MLS §16.10 Operator 16.15).
#[test]
fn an_event_clock_ticks_on_the_rising_edge_of_its_condition() {
    let source = r#"
model RisingEdge
  Real s(start = 0, fixed = true);
  Boolean b = sin(2 * 3.141592653589793 * time) > 0.5;
  Clock c = Clock(b);
  Integer n(start = 0);
  Real y(start = 0);
  Real dt(start = 0);
  Real held;
equation
  der(s) = 1;
  when c then
    n = previous(n) + 1;
    y = sample(s);
    dt = interval();
  end when;
  held = hold(y);
end RisingEdge;
"#;
    let result = simulate(source, "RisingEdge", 3.0);
    let first = 1.0 / 12.0;
    for (tick, time) in [(0.0, 0.05), (1.0, 0.5), (1.0, 1.05), (2.0, 1.5), (3.0, 2.9)] {
        assert_eq!(value_at(&result, "n", time), tick, "n at {time}");
    }
    assert!((value_at(&result, "y", 0.5) - first).abs() < 1.0e-6);
    assert!((value_at(&result, "y", 1.5) - (first + 1.0)).abs() < 1.0e-6);
    assert_eq!(value_at(&result, "dt", 0.5), 0.0);
    assert!((value_at(&result, "dt", 1.5) - 1.0).abs() < 1.0e-6);
}

/// MLS §16.3: the event clock and `sample` both delay by one event
/// iteration, so the first tick of `Clock(b)` is at 0.5 and samples `b` true.
#[test]
fn the_first_tick_samples_its_own_condition_true() {
    let source = r#"
model SampledCondition
  Boolean b = time >= 0.5;
  Clock c = Clock(b);
  Boolean b2 = sample(b, c);
  Real v;
  Real held;
  Real s(start = 0, fixed = true);
equation
  der(s) = 1;
  v = if b2 then 1.0 else -1.0;
  held = hold(v);
end SampledCondition;
"#;
    let result = simulate(source, "SampledCondition", 1.0);
    assert_eq!(value_at(&result, "held", 0.4), -1.0);
    assert_eq!(value_at(&result, "held", 0.5), 1.0);
    assert_eq!(value_at(&result, "b2", 0.9), 1.0);
}

/// An event clock has no periodic lattice, so an exact clock conversion of it
/// is refused, and its condition must name a Boolean coordinate.
#[test]
fn unsupported_event_clock_forms_are_refused() {
    let conversion = r#"
model Conversion
  Boolean b = time >= 0.5;
  Clock c = subSample(Clock(b), 2);
  Real y = sample(time, c);
end Conversion;
"#;
    let message = refusal(conversion, "Conversion");
    assert!(
        message.contains("event clock has no periodic lattice"),
        "{message}"
    );
    let inline = r#"
model Inline
  Clock c = Clock(time >= 0.5);
  Real y = sample(time, c);
end Inline;
"#;
    let message = refusal(inline, "Inline");
    assert!(
        message.contains("event clock condition must name a scalar Boolean variable"),
        "{message}"
    );
}

/// `firstTick()` and a conditional in an event-clock partition, as in the MSL
/// `IntegerChange` block: `changed` is false at the first tick and true
/// whenever the sampled Integer differs from its previous tick.
#[test]
fn first_tick_and_conditions_follow_the_event_clock() {
    let source = r#"
model ChangeDetector
  Boolean b = sin(2 * 3.141592653589793 * time) > 0.5;
  Clock c = Clock(b);
  Integer k = if time >= 1.5 then 1 else 0;
  Integer u(start = 0);
  Boolean changed(start = false);
  Real flag(start = 0);
  Real held;
  Real s(start = 0, fixed = true);
equation
  der(s) = 1;
  when c then
    u = sample(k);
    changed = if firstTick() then false else not (u == previous(u));
    flag = if changed then 1.0 else 0.0;
  end when;
  held = hold(flag);
end ChangeDetector;
"#;
    let result = simulate(source, "ChangeDetector", 3.0);
    // Ticks at 1/12 + j sample k = 0, 0, 1, so only the tick at 2 1/12 changes it.
    assert_eq!(value_at(&result, "held", 0.5), 0.0);
    assert_eq!(value_at(&result, "held", 1.5), 0.0);
    assert_eq!(value_at(&result, "held", 2.5), 1.0);
}

/// The clock ticks once when `edge(pre(b))` rises: `b` staying true through the
/// later event at 0.5 does not tick it again. The tick samples `s` at its left
/// limit (MLS §16.5.1), before the tick's own `hold(off)` update, so `dir` is
/// `a` at the tick and not zero.
#[test]
fn an_event_clock_ticks_once_while_its_condition_stays_true() {
    let source = r#"
model HeldCondition
  Boolean b = time > 0.05;
  Clock c = Clock(b);
  Real a = 10 * time + 10;
  Real off(start = 0);
  Real dir(start = 0);
  Real s = a - hold(off);
  Integer n(start = 0);
  Integer late(start = 0);
equation
  off = sample(a, c);
  dir = sample(s, c);
  when c then
    n = previous(n) + 1;
  end when;
  when time > 0.5 then
    late = 1;
  end when;
end HeldCondition;
"#;
    let result = simulate(source, "HeldCondition", 1.0);
    assert_eq!(value_at(&result, "late", 0.9), 1.0);
    assert_eq!(value_at(&result, "n", 0.9), 1.0);
    assert!((value_at(&result, "off", 0.9) - 10.5).abs() < 1.0e-6);
    assert!((value_at(&result, "dir", 0.9) - 10.5).abs() < 1.0e-6);
}

/// A clock array (the MSL `ClockVectorInput`) is a set of connection hubs:
/// each element carries the clock connected to it into the block.
#[test]
fn clock_array_elements_carry_their_connected_clocks() {
    let source = r#"
model ClockHub
  connector ClockIn = input Clock;
  connector ClockOut = output Clock;
  block Source
    input Boolean u;
    ClockOut y;
  equation
    y = Clock(u);
  end Source;
  block Counter
    ClockIn u;
    Integer n(start = 0);
  equation
    when u then
      n = previous(n) + 1;
    end when;
  end Counter;
  block Hub
    ClockIn u[2];
    Counter k[2];
  equation
    connect(u[1], k[1].u);
    connect(u[2], k[2].u);
  end Hub;
  Source s1(u = time > 0.2);
  Source s2(u = time > 0.6);
  Hub h;
  Real s(start = 0, fixed = true);
equation
  der(s) = 1;
  connect(s1.y, h.u[1]);
  connect(s2.y, h.u[2]);
end ClockHub;
"#;
    let result = simulate(source, "ClockHub", 1.0);
    assert_eq!(value_at(&result, "h.k[1].n", 0.4), 1.0);
    assert_eq!(value_at(&result, "h.k[2].n", 0.4), 0.0);
    assert_eq!(value_at(&result, "h.k[1].n", 0.9), 1.0);
    assert_eq!(value_at(&result, "h.k[2].n", 0.9), 1.0);
}

/// MLS §16.5.2: `shiftSample(c, 2)` of an event clock skips its first two
/// ticks and then ticks with it, whether it shifts the clock itself or a value
/// of its partition: of the ticks at 1/12 + j it keeps 2 1/12 and 3 1/12, and
/// `y` carries `u` from the same tick.
#[test]
fn a_shifted_event_clock_skips_its_first_ticks() {
    let source = r#"
model ShiftedEventClock
  Real s(start = 0, fixed = true);
  Boolean b = sin(2 * 3.141592653589793 * time) > 0.5;
  Clock c = Clock(b);
  Clock c2 = shiftSample(c, 2);
  Integer n(start = 0);
  Integer m(start = 0);
  Real u(start = 0);
  Real y(start = -1);
  Real held;
equation
  der(s) = 1;
  when c then
    n = previous(n) + 1;
    u = sample(s);
  end when;
  when c2 then
    m = previous(m) + 1;
  end when;
  y = shiftSample(u, 2);
  held = hold(y);
end ShiftedEventClock;
"#;
    let result = simulate(source, "ShiftedEventClock", 4.0);
    for (time, ticks, shifted) in [(1.5, 2.0, 0.0), (2.5, 3.0, 1.0), (3.5, 4.0, 2.0)] {
        assert_eq!(value_at(&result, "n", time), ticks, "n at {time}");
        assert_eq!(value_at(&result, "m", time), shifted, "m at {time}");
    }
    assert_eq!(value_at(&result, "held", 1.5), -1.0);
    assert!((value_at(&result, "held", 2.5) - (2.0 + 1.0 / 12.0)).abs() < 1.0e-6);
    assert!((value_at(&result, "held", 3.5) - (3.0 + 1.0 / 12.0)).abs() < 1.0e-6);
}

/// MLS §16.3: with `b(start = true)` and `b` true at the start, `pre(b)` never
/// rises at t = 0, so the clock does not tick there; its first tick is the
/// first later rise of `b`, at 0.75, and the next at 1.75.
#[test]
fn an_event_clock_whose_condition_starts_true_first_ticks_at_its_next_rise() {
    let source = r#"
model StartsTrue
  Real s(start = 0, fixed = true);
  Boolean b(start = true);
  Clock c = Clock(b);
  Integer n(start = 0);
  Real y(start = -1);
  Real held;
equation
  der(s) = 1;
  b = cos(2 * 3.141592653589793 * time) > 0;
  when c then
    n = previous(n) + 1;
    y = sample(s);
  end when;
  held = hold(y);
end StartsTrue;
"#;
    let result = simulate(source, "StartsTrue", 2.0);
    for (time, ticks) in [(0.0, 0.0), (0.7, 0.0), (0.8, 1.0), (1.7, 1.0), (1.8, 2.0)] {
        assert_eq!(value_at(&result, "n", time), ticks, "n at {time}");
    }
    assert_eq!(value_at(&result, "held", 0.7), -1.0);
    assert!((value_at(&result, "held", 0.8) - 0.75).abs() < 1.0e-6);
    assert!((value_at(&result, "held", 1.8) - 1.75).abs() < 1.0e-6);
}
