//! Same-tick value exchange across coincidentally ticking periodic clocks.
//!
//! A whole-interval `shiftSample` (and every `subSample`) places its result on
//! a clock whose identity differs from the source clock's, yet the two clocks
//! fire on shared instants. On such an instant the transfer must read the
//! freshly issued source value, independent of equation source order; reading
//! the held entry storage would delay the signal by one extra sample period
//! (the `Modelica.Clocked` HoldWithDAeffects1/2 oracle regression). The
//! schedule proves coincidence over the rational clock lattice
//! (`ClockCoincidence` in `rumoca-phase-structural`), so both source orders
//! below issue the source producer first.

use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae};

/// sin(4*pi*t) sampled on a 20 ms clock; the shifted clock ticks at
/// t = 40 ms, 60 ms, ... and every one of its ticks coincides with a source
/// tick, so `ys` must equal the same-instant `xs` there.
const SHIFT_TRANSFER_FIRST: &str = r#"
model CoincidentShift
  Clock clk = Clock(20, 1000);
  discrete Real xs(start = 0);
  discrete Real ys(start = 0);
equation
  ys = shiftSample(xs, 2, 1);
  xs = sample(sin(12.566370614359172 * time), clk);
end CoincidentShift;
"#;

const SHIFT_SOURCE_FIRST: &str = r#"
model CoincidentShift
  Clock clk = Clock(20, 1000);
  discrete Real xs(start = 0);
  discrete Real ys(start = 0);
equation
  xs = sample(sin(12.566370614359172 * time), clk);
  ys = shiftSample(xs, 2, 1);
end CoincidentShift;
"#;

const SUB_TRANSFER_FIRST: &str = r#"
model CoincidentSub
  Clock clk = Clock(20, 1000);
  discrete Real xs(start = 0);
  discrete Real ys(start = 0);
equation
  ys = subSample(xs, 2);
  xs = sample(sin(12.566370614359172 * time), clk);
end CoincidentSub;
"#;

const SUB_SOURCE_FIRST: &str = r#"
model CoincidentSub
  Clock clk = Clock(20, 1000);
  discrete Real xs(start = 0);
  discrete Real ys(start = 0);
equation
  xs = sample(sin(12.566370614359172 * time), clk);
  ys = subSample(xs, 2);
end CoincidentSub;
"#;

/// Simulate one source and return `(xs, ys)` settled at `time`.
fn transfer_values_at(source: &str, model: &str, file: &str, time: f64) -> (f64, f64) {
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_str(source, file)
        .unwrap_or_else(|error| panic!("`{file}` should compile: {error:?}"));
    let sim = simulate_dae(
        compiled.dae(),
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.06,
            dt: Some(0.005),
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("`{file}` should simulate: {error:?}"));
    (
        settled_value_at_time(&sim, "xs", time),
        settled_value_at_time(&sim, "ys", time),
    )
}

fn settled_value_at_time(sim: &rumoca_sim::SimResult, name: &str, time: f64) -> f64 {
    let column = sim
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("trace must contain `{name}`; names = {:?}", sim.names));
    let index = sim
        .times
        .iter()
        .rposition(|candidate| (candidate - time).abs() <= 1.0e-12)
        .unwrap_or_else(|| panic!("trace must contain a row at time {time}"));
    sim.data[column][index]
}

/// The value the 20 ms sample holds after its tick at t = 0.04:
/// sin(4*pi*0.04).
const FRESH_SAMPLE_AT_40MS: f64 = 0.4817536741017153;

#[test]
fn whole_interval_shift_sample_reads_the_fresh_coincident_value() {
    let (xs, ys) = transfer_values_at(
        SHIFT_TRANSFER_FIRST,
        "CoincidentShift",
        "coincident_shift_transfer_first.mo",
        0.04,
    );
    assert!(
        (xs - FRESH_SAMPLE_AT_40MS).abs() <= 1.0e-9,
        "sampled source should hold sin(4*pi*0.04) after the 40 ms tick; got {xs}"
    );
    assert!(
        (ys - xs).abs() <= 1.0e-12,
        "shiftSample by two whole intervals must read the same-instant source \
         value at the coincident 40 ms tick, not the held previous-tick value; \
         got ys = {ys}, xs = {xs}"
    );
}

#[test]
fn shift_sample_value_is_independent_of_equation_order() {
    let (_, transfer_first) = transfer_values_at(
        SHIFT_TRANSFER_FIRST,
        "CoincidentShift",
        "coincident_shift_transfer_first.mo",
        0.04,
    );
    let (_, source_first) = transfer_values_at(
        SHIFT_SOURCE_FIRST,
        "CoincidentShift",
        "coincident_shift_source_first.mo",
        0.04,
    );
    assert!(
        (transfer_first - source_first).abs() <= 1.0e-12,
        "the issued same-tick order is causal, so equation source order must \
         not change the transferred value; transfer-first = {transfer_first}, \
         source-first = {source_first}"
    );
}

#[test]
fn sub_sample_reads_the_fresh_coincident_value() {
    let (xs, ys) = transfer_values_at(
        SUB_TRANSFER_FIRST,
        "CoincidentSub",
        "coincident_sub_transfer_first.mo",
        0.04,
    );
    assert!(
        (ys - xs).abs() <= 1.0e-12,
        "subSample ticks coincide with source ticks and must read the \
         same-instant source value; got ys = {ys}, xs = {xs}"
    );
}

#[test]
fn sub_sample_value_is_independent_of_equation_order() {
    let (_, transfer_first) = transfer_values_at(
        SUB_TRANSFER_FIRST,
        "CoincidentSub",
        "coincident_sub_transfer_first.mo",
        0.04,
    );
    let (_, source_first) = transfer_values_at(
        SUB_SOURCE_FIRST,
        "CoincidentSub",
        "coincident_sub_source_first.mo",
        0.04,
    );
    assert!(
        (transfer_first - source_first).abs() <= 1.0e-12,
        "subSample equation source order must not change the transferred \
         value; transfer-first = {transfer_first}, source-first = {source_first}"
    );
}
