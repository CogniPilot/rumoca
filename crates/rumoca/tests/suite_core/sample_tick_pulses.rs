//! `sample(start, interval)` ticks inside activations that no clock owns.
//!
//! MLS §3.7.5 makes `sample` a Boolean event operator that is true only at its
//! tick instants, and §8.3.5.1 activates a `when` on the rising edge of its
//! condition (of each element `bi` for a vector condition). A tick at the start
//! instant is the first event after initialization, so `initial()` and a tick
//! at `start = 0` are two activations of the same instant.

use rumoca_sim::{SimOptions, simulate_dae};

const VECTOR_INITIAL_AND_ZERO_PHASE_TICK: &str = r#"
model VectorInitialTick
  discrete Integer s(start = 0, fixed = true);
algorithm
  when {initial(), sample(0, 0.05)} then
    s := pre(s) + 1;
  end when;
end VectorInitialTick;
"#;

const VECTOR_INITIAL_AND_LATER_TICK: &str = r#"
model VectorInitialLaterTick
  discrete Integer s(start = 0, fixed = true);
algorithm
  when {initial(), sample(0.05, 0.05)} then
    s := pre(s) + 1;
  end when;
end VectorInitialLaterTick;
"#;

const ALGORITHM_INITIAL_ELSEWHEN_TICK: &str = r#"
model AlgorithmElsewhenTick
  output Real r;
protected
  discrete Integer s;
algorithm
  when initial() then
    s := 3;
    r := 0;
  elsewhen sample(0, 0.05) then
    s := pre(s) + 1;
    r := 0.5;
  end when;
end AlgorithmElsewhenTick;
"#;

const EQUATION_INITIAL_ELSEWHEN_TICK: &str = r#"
model EquationElsewhenTick
  output Real r;
  discrete Integer s;
equation
  when initial() then
    s = 3;
    r = 0;
  elsewhen sample(0, 0.05) then
    s = pre(s) + 1;
    r = 0.5;
  end when;
end EquationElsewhenTick;
"#;

fn simulate(source: &str, model: &str) -> rumoca_sim::SimResult {
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .expect("model compiles");
    simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.01),
            ..SimOptions::default()
        },
    )
    .expect("model simulates")
}

/// The value recorded last at or before `t` (the right limit at an event).
fn value_at(sim: &rumoca_sim::SimResult, name: &str, t: f64) -> f64 {
    let column = sim
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("trace contains `{name}`; names={:?}", sim.names));
    let mut value = sim.data[column][0];
    for (index, &time) in sim.times.iter().enumerate() {
        if time > t + 1.0e-9 {
            break;
        }
        value = sim.data[column][index];
    }
    value
}

fn assert_counts(sim: &rumoca_sim::SimResult, name: &str, expected: &[(f64, f64)]) {
    for &(time, count) in expected {
        let value = value_at(sim, name, time);
        assert!(
            (value - count).abs() < 1.0e-9,
            "`{name}` at t = {time} is {value}, expected {count}"
        );
    }
}

#[test]
fn a_vector_activation_counts_initial_and_every_zero_phase_tick() {
    let sim = simulate(VECTOR_INITIAL_AND_ZERO_PHASE_TICK, "VectorInitialTick");
    assert_counts(
        &sim,
        "s",
        &[(0.0, 2.0), (0.04, 2.0), (0.05, 3.0), (0.1, 4.0), (0.2, 6.0)],
    );
}

#[test]
fn a_vector_activation_counts_initial_and_every_later_tick() {
    let sim = simulate(VECTOR_INITIAL_AND_LATER_TICK, "VectorInitialLaterTick");
    assert_counts(
        &sim,
        "s",
        &[(0.0, 1.0), (0.04, 1.0), (0.05, 2.0), (0.1, 3.0), (0.2, 5.0)],
    );
}

#[test]
fn an_algorithm_elsewhen_tick_fires_after_initial_at_the_start_instant() {
    let sim = simulate(ALGORITHM_INITIAL_ELSEWHEN_TICK, "AlgorithmElsewhenTick");
    assert_counts(
        &sim,
        "s",
        &[(0.0, 4.0), (0.05, 5.0), (0.1, 6.0), (0.2, 8.0)],
    );
    assert_counts(&sim, "r", &[(0.0, 0.5), (0.2, 0.5)]);
}

#[test]
fn an_equation_elsewhen_tick_fires_after_initial_at_the_start_instant() {
    let sim = simulate(EQUATION_INITIAL_ELSEWHEN_TICK, "EquationElsewhenTick");
    assert_counts(
        &sim,
        "s",
        &[(0.0, 4.0), (0.05, 5.0), (0.1, 6.0), (0.2, 8.0)],
    );
    assert_counts(&sim, "r", &[(0.0, 0.5), (0.2, 0.5)]);
}
