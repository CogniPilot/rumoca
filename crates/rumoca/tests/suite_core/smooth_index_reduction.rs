//! Index reduction through `smooth(p, expr)` (MLS §3.7.5).
//!
//! `smooth(p, expr)` states that `expr` is `p` times continuously
//! differentiable, so a conditional inside it differentiates branch-wise up to
//! order `p` with its guard evaluated live: where the guard changes without an
//! event, the branch derivatives agree. `Modelica.Media.Common.smoothStep` is
//! such a conditional, and `der(state.p)` of a state built by
//! `setSmoothState` differentiates through it. A conditional with a live guard
//! that no `smooth` certifies keeps the refusal.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

fn series<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let Some(index) = result.names.iter().position(|candidate| candidate == name) else {
        panic!("simulation result missing column {name}");
    };
    result.data[index].as_slice()
}

const SMOOTH_STATE: &str = r#"
package SmoothState
  record State
    Real p;
    Real T;
  end State;
  function smoothStep
    input Real x;
    input Real y1;
    input Real y2;
    input Real x_small(min = 0) = 1e-5;
    output Real y;
  algorithm
    y := smooth(1, if x > x_small then y1 else if x < -x_small then y2 else
      if abs(x_small) > 0 then (x / x_small) * ((x / x_small)^2 - 3) * (y2 - y1) / 4
      + (y1 + y2) / 2 else (y1 + y2) / 2);
  end smoothStep;
  function setSmoothState
    input Real x;
    input State state_a;
    input State state_b;
    input Real x_small(min = 0);
    output State state;
  algorithm
    state := State(p = smoothStep(x, state_a.p, state_b.p, x_small),
      T = smoothStep(x, state_a.T, state_b.T, x_small));
  end setSmoothState;
  model Model
    State a;
    State b;
    State s;
    Real dp;
    Real dT;
  equation
    a.p = 1e5;
    a.T = 200 + 1000 * time;
    b.p = 2e5;
    b.T = 500;
    s = setSmoothState(time - 0.5, a, b, 0.1);
    dp = der(s.p);
    dT = der(s.T);
  end Model;
end SmoothState;
"#;

/// The derivative of `smoothStep` on its cubic blend, with `y1` and `y2`
/// varying at the rates `dy1` and `dy2`.
fn smooth_step_rate(x: f64, y1: f64, y2: f64, dy1: f64, dy2: f64, x_small: f64) -> f64 {
    if x > x_small {
        dy1
    } else if x < -x_small {
        dy2
    } else {
        let u = x / x_small;
        (3.0 * u * u - 3.0) / x_small * (y2 - y1) / 4.0
            + u * (u * u - 3.0) * (dy2 - dy1) / 4.0
            + (dy1 + dy2) / 2.0
    }
}

#[test]
fn smooth_conditional_differentiates_branch_wise_with_live_guards() {
    let compiled = Compiler::new()
        .model("SmoothState.Model")
        .compile_str(SMOOTH_STATE, "SmoothState.mo")
        .expect("the smooth state model compiles");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.01),
            ..SimOptions::default()
        },
    )
    .expect("the smooth state model simulates");
    let blended = result
        .times
        .iter()
        .filter(|time| (*time - 0.5).abs() < 0.1)
        .count();
    assert!(blended > 5, "the output grid samples the cubic blend");
    for (index, time) in result.times.iter().enumerate() {
        let x = time - 0.5;
        let a_t = 200.0 + 1000.0 * time;
        let dp = smooth_step_rate(x, 1e5, 2e5, 0.0, 0.0, 0.1);
        let dt = smooth_step_rate(x, a_t, 500.0, 1000.0, 0.0, 0.1);
        assert!(
            (series(&result, "dp")[index] - dp).abs() < 1e-6 * dp.abs().max(1.0),
            "der(s.p) at t = {time}"
        );
        assert!(
            (series(&result, "dT")[index] - dt).abs() < 1e-6 * dt.abs().max(1.0),
            "der(s.T) at t = {time}"
        );
    }
}

const UNCERTIFIED: &str = r#"
model Uncertified
  function ramp
    input Real x;
    output Real y;
  algorithm
    y := if x > 0.5 then x else 2 * x;
  end ramp;
  Real y;
  Real dy;
equation
  y = ramp(time);
  dy = der(y);
end Uncertified;
"#;

/// A function-body conditional switches without an event, so with no
/// `smooth` certificate its branch derivative is not the derivative where the
/// guard changes; index reduction keeps refusing it.
#[test]
fn uncertified_live_guard_keeps_the_refusal() {
    let compiled = Compiler::new()
        .model("Uncertified")
        .compile_str(UNCERTIFIED, "Uncertified.mo")
        .expect("the uncertified model constructs its DAE");
    let error = simulate_dae_with_diagnostics(&compiled.dae, &SimOptions::default())
        .expect_err("an uncertified live guard must not be differentiated");
    assert!(
        error.to_string().contains("structurally singular"),
        "expected the structural refusal, got: {error}"
    );
}
