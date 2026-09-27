//! Conditional arms decided at translation and selected at run time.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

/// `n > 0` is proven at translation, so the conditional keeps its first
/// (unproven) arm and falls back to `2*x` rather than lowering `3*x`.
const PROVEN_FALLBACK: &str = "
model ProvenFallback
  constant Integer n = 1;
  Real x(start = 1, fixed = true);
  Real y;
equation
  der(x) = -x;
  y = if x > 0.5 then x elseif n > 0 then 2*x else 3*x;
end ProvenFallback;
";

/// A tunable `mode` guards call arms that read the same unknowns, so each
/// conditional stays a run-time selection over its arms (SPEC_0040 DAE-C22),
/// including one over the points of a `for` equation.
const SELECTED_ARMS: &str = "
model SelectedArms
  function twice
    input Real u;
    output Real v;
  algorithm
    v := 2*u;
  end twice;
  parameter Integer mode = 2;
  Real x(start = 1, fixed = true);
  Real y;
  Real z[2];
equation
  der(x) = -x;
  y = if mode == 1 then twice(x) elseif mode == 2 then twice(2*x) else twice(3*x);
  for i in 1:2 loop
    z[i] = if mode == 1 then twice(i*x) else twice(x);
  end for;
end SelectedArms;
";

fn simulate(model: &str, source: &str, overrides: &[(&str, f64)]) -> SimResult {
    let compiled = match Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
    {
        Ok(compiled) => compiled,
        Err(error) => panic!("compile {model}: {error:#}"),
    };
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.5),
        param_overrides: overrides
            .iter()
            .map(|(name, value)| ((*name).to_string(), *value))
            .collect(),
        ..Default::default()
    };
    match simulate_dae_with_diagnostics(&compiled.dae, &options) {
        Ok(result) => result,
        Err(error) => panic!("simulate {model}: {error:#}"),
    }
}

/// The value of `name` at every recorded time, with `x` beside it.
fn traces<'a>(result: &'a SimResult, name: &str) -> (&'a [f64], &'a [f64]) {
    let mut column = None;
    let mut x = None;
    for (index, candidate) in result.names.iter().enumerate() {
        if candidate == name {
            column = Some(index);
        }
        if candidate == "x" {
            x = Some(index);
        }
    }
    let (Some(column), Some(x)) = (column, x) else {
        panic!("the trace records {name} and x");
    };
    (&result.data[column], &result.data[x])
}

fn assert_trace(result: &SimResult, name: &str, expected: f64) {
    let (values, x) = traces(result, name);
    for (value, x) in values.iter().zip(x) {
        let scaled = expected * x;
        assert!(
            (value - scaled).abs() <= 1e-6 * scaled.abs().max(1.0),
            "{name} = {value}, expected {scaled}"
        );
    }
}

#[test]
fn a_proven_later_condition_becomes_the_fallback_of_an_unproven_one() {
    let result = simulate("ProvenFallback", PROVEN_FALLBACK, &[]);
    let (y, x) = traces(&result, "y");
    for (y, x) in y.iter().zip(x) {
        let expected = if *x > 0.5 { *x } else { 2.0 * x };
        assert!((y - expected).abs() <= 1e-6, "y = {y} at x = {x}");
    }
}

#[test]
fn a_tunable_guard_selects_among_several_call_arms_at_run_time() {
    let default = simulate("SelectedArms", SELECTED_ARMS, &[]);
    assert_trace(&default, "y", 4.0);
    assert_trace(&default, "z[2]", 2.0);
    let first = simulate("SelectedArms", SELECTED_ARMS, &[("mode", 1.0)]);
    assert_trace(&first, "y", 2.0);
    assert_trace(&first, "z[2]", 4.0);
    let last = simulate("SelectedArms", SELECTED_ARMS, &[("mode", 3.0)]);
    assert_trace(&last, "y", 6.0);
}
