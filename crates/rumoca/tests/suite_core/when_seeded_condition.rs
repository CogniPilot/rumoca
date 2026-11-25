//! A `when` whose condition is already true when the simulation starts does
//! not fire there.
//!
//! MLS 3.7 §8.6: a when-clause is not active during initialization unless it
//! is enabled with `initial()`, and after initialization `pre(vi) := vi` is
//! set before the event iteration at the start time. A condition that holds
//! from the start, here one fixed by a parameter, therefore has `pre` true at
//! that iteration and no edge: the body acts only when the condition rises
//! later. The condition is seeded before its memory is taken (#746, R009),
//! so reading it while still at its start value would fire the body at t = 0.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const SOURCE: &str = r#"
package P
  model SeededCondition
    parameter Real k = 1;
    Boolean b;
    discrete Integer n(start = 0);
  equation
    b = k > 0;
    when b then
      n = pre(n) + 1;
    end when;
  end SeededCondition;
  model SeededConditionRisesLater
    parameter Real k = 1;
    Boolean b;
    discrete Integer n(start = 0);
  equation
    b = k > 0 and not (time >= 0.3 and time < 0.6);
    when b then
      n = pre(n) + 1;
    end when;
  end SeededConditionRisesLater;
end P;
"#;

/// The trace of `name` in `model` over [0, 1], with its times.
fn trace(model: &str, name: &str) -> (Vec<f64>, Vec<f64>) {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "WhenSeededCondition.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error:?}"));
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{model} exposes {name}"));
    (result.times.clone(), result.data[index].clone())
}

#[test]
fn a_condition_true_from_the_start_does_not_fire_its_when() {
    let (_, n) = trace("P.SeededCondition", "n");
    assert!(
        n.iter().all(|value| *value == 0.0),
        "n stays at its start through t = 0 and after: {n:?}"
    );
}

#[test]
fn a_condition_true_from_the_start_fires_when_it_rises_again() {
    let (times, n) = trace("P.SeededConditionRisesLater", "n");
    // At t = 0.6 itself the trace may hold both the left and the right value.
    for (time, value) in times.iter().zip(&n) {
        if *time < 0.6 {
            assert_eq!(*value, 0.0, "n before the rise, t = {time}");
        } else if *time > 0.6 {
            assert_eq!(*value, 1.0, "n after the rise, t = {time}");
        }
    }
    assert!(
        times.iter().any(|time| *time > 0.6),
        "the trace reaches past the rise"
    );
}
