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

/// Every arm reads `x[i]` or `x[i + 1]` over `for i in 1:3` into `x[4]`, so each
/// subscript is proven inside the array at every point of the family and the
/// arms are total (SPEC_0040 DAE-C22): the conditional lowers to an eager
/// selection.
const IN_BOUNDS_FAMILY_ARMS: &str = "
model InBoundsFamilyArms
  Real x[4](each start = 1, each fixed = true);
equation
  for i in 1:3 loop
    der(x[i]) = noEvent(if x[i] > 0.5 then -x[i] else -x[i + 1]);
  end for;
  der(x[4]) = -x[4];
end InBoundsFamilyArms;
";

/// `x[k]` reads a subscript no binder interval bounds, so that arm is not
/// total and only the selected arm runs; the guard never selects it.
const COMPUTED_SUBSCRIPT_ARM: &str = "
model ComputedSubscriptArm
  parameter Integer k = 2;
  Real x[3](each start = 1, each fixed = true);
equation
  for i in 1:3 loop
    der(x[i]) = noEvent(if x[i] > 10 then x[k] else -x[i]);
  end for;
end ComputedSubscriptArm;
";

/// The (selection, function-conditional) operation counts of the continuous
/// derivative right-hand side.
fn derivative_conditional_counts(model: &str, source: &str) -> (usize, usize) {
    let compiled = match Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
    {
        Ok(compiled) => compiled,
        Err(error) => panic!("compile {model}: {error:#}"),
    };
    let package = match rumoca_phase_solve::lower_solve_package(&compiled.dae) {
        Ok(package) => package,
        Err(error) => panic!("lower {model}: {error:#}"),
    };
    let block = &package.problem.continuous.derivative_rhs;
    let scalar = match rumoca_eval_solve::to_scalar_program_block(block) {
        Ok(scalar) => scalar,
        Err(error) => panic!("scalarize {model}: {error:#}"),
    };
    scalar
        .programs()
        .iter()
        .flatten()
        .fold((0, 0), |(selects, conditionals), op| match op {
            rumoca_ir_solve::LinearOp::Select { .. } => (selects + 1, conditionals),
            rumoca_ir_solve::LinearOp::FunctionConditional { .. } => (selects, conditionals + 1),
            _ => (selects, conditionals),
        })
}

/// The last recorded value of `name`, against `x(1) = exp(-1)`.
fn assert_unit_decay(result: &SimResult, name: &str) {
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("the trace records {name}"));
    let last = *result.data[column].last().expect("the trace is recorded");
    let expected = (-1.0_f64).exp();
    assert!(
        (last - expected).abs() <= 1e-5,
        "{name}(1) = {last}, expected {expected}"
    );
}

#[test]
fn family_arms_with_in_bounds_binder_subscripts_lower_to_an_eager_selection() {
    let (selects, conditionals) =
        derivative_conditional_counts("InBoundsFamilyArms", IN_BOUNDS_FAMILY_ARMS);
    assert!(selects > 0, "the family conditional lowers to a selection");
    assert_eq!(
        conditionals, 0,
        "no in-bounds arm needs a selected-arm program"
    );
    let result = simulate("InBoundsFamilyArms", IN_BOUNDS_FAMILY_ARMS, &[]);
    assert_unit_decay(&result, "x[1]");
}

#[test]
fn a_family_arm_with_an_unbounded_subscript_runs_only_when_selected() {
    let (_, conditionals) =
        derivative_conditional_counts("ComputedSubscriptArm", COMPUTED_SUBSCRIPT_ARM);
    assert!(
        conditionals > 0,
        "the arm reading x[k] stays a selected-arm program"
    );
    let result = simulate("ComputedSubscriptArm", COMPUTED_SUBSCRIPT_ARM, &[]);
    assert_unit_decay(&result, "x[3]");
}
