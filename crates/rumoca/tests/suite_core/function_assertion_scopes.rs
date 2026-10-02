//! Assertions of calls that a function reaches inside a loop body or in the
//! first condition of a correlated conditional (MLS 3.7 §8.3.7, §11.2.8.1).
//!
//! The first failing assertion ends the function: inside a `for` loop that is
//! the first failing iteration, and a call in a conditional's condition runs
//! before either branch. Both shapes occur in `Modelica.Media.Water.IF97`
//! property functions.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model Scopes
  function g
    input Real x;
    output Real y;
  algorithm
    assert(x > -10, "x too small: " + String(x));
    y := 2*x;
  end g;
  function sumg
    input Real x;
    input Integer n;
    output Real s;
  algorithm
    s := 0;
    for i in 1:n loop
      s := 0.5*s + g(x + i);
    end for;
  end sumg;
  function h
    input Real x;
    output Real y;
  algorithm
    assert(x < 10, "x too large");
    y := x;
  end h;
  function pick
    input Real x;
    output Real y;
    output Real z;
  algorithm
    if h(x) > 0.5 then
      y := 1;
      z := 2;
    else
      y := 3;
      z := 4;
    end if;
  end pick;
  Real s = sumg(time + OFFSET, 3);
  Real a;
  Real b;
equation
  (a, b) = pick(time + SHIFT);
end Scopes;
"#;

fn simulate(offset: &str, shift: &str) -> Result<SimResult, String> {
    let source = SOURCE.replace("OFFSET", offset).replace("SHIFT", shift);
    let compiled = Compiler::new()
        .model("Scopes")
        .compile_str(&source, "Scopes.mo")
        .unwrap_or_else(|error| panic!("Scopes compiles: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .map_err(|error| format!("{error:?}"))
}

fn column<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} is recorded"));
    &result.data[index]
}

#[test]
fn calls_in_loops_and_conditions_keep_their_values_when_assertions_hold() {
    let result = simulate("0", "0").expect("Scopes simulates");
    for (sample, time) in result.times.iter().enumerate() {
        let s = column(&result, "s")[sample];
        // s = 0.25 g(x + 1) + 0.5 g(x + 2) + g(x + 3) with g(v) = 2 v.
        assert!((s - (3.5 * time + 8.5)).abs() < 1e-9, "s({time}) = {s}");
        let (a, b) = (column(&result, "a")[sample], column(&result, "b")[sample]);
        let (ea, eb) = if *time > 0.5 { (1.0, 2.0) } else { (3.0, 4.0) };
        if (time - 0.5).abs() > 1e-6 {
            assert!(
                (a - ea).abs() < 1e-12 && (b - eb).abs() < 1e-12,
                "({a}, {b}) at {time}"
            );
        }
    }
}

#[test]
fn the_first_failing_loop_iteration_ends_the_call() {
    let error = simulate("(-13)", "0").expect_err("g fails on the first iteration");
    assert!(error.contains("x too small: -12"), "{error}");
}

#[test]
fn a_call_in_a_condition_asserts_before_either_branch() {
    let error = simulate("0", "20").expect_err("h fails in the condition");
    assert!(error.contains("x too large"), "{error}");
}
