//! A discrete Integer defined by an equation is read by a continuous row at
//! initialization. MLS 3.7 §8.6: every equation holds at the initial instant,
//! so the index equation must be settled before the algebraic row reads it.
use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model Selected
  parameter Real p[4] = {1, 2, 3, 4};
  input Integer k(start = 1);
  Integer k2;
  output Real y;
equation
  k2 = k;
  y = p[k2];
end Selected;
"#;

/// A chain of discrete aliases: `k3` reads `k2`, not a parameter or input.
const CHAINED: &str = r#"
model Chained
  parameter Real p[4] = {1, 2, 3, 4};
  input Integer k(start = 1);
  Integer k2;
  Integer k3;
  output Real y;
equation
  k2 = k;
  k3 = k2 + 1;
  y = p[k3];
end Chained;
"#;

/// The seeded equation `a = 1` has a start that depends on a parameter. The
/// `pre` kept through initialization is the start the run's parameters give,
/// not the one the model was constructed with.
const START_FROM_PARAMETER: &str = r#"
model StartFromParameter
  parameter Integer n = 1;
  Integer a(start = n, fixed = true);
  Integer x;
  Integer first;
  Integer cnt(start = 0, fixed = true);
equation
  a = 1;
  x = pre(a);
  when initial() then
    first = x;
  end when;
  when {initial(), change(x) and not initial()} then
    cnt = pre(cnt) + 1;
  end when;
end StartFromParameter;
"#;

fn solver_value(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == name)
        .unwrap_or_else(|| panic!("missing solver value {name}"))
        .value
}

fn initial_value(result: &rumoca_sim::SimResult, name: &str) -> f64 {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} is recorded by the simulation"));
    result.data[index][0]
}

fn options(k: f64) -> SimOptions {
    SimOptions {
        t_end: 0.01,
        dt: Some(0.01),
        initial_inputs: vec![("k".into(), k)],
        ..SimOptions::default()
    }
}

fn check(source: &str, model: &str, cases: &[(f64, f64)]) {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap();
    for &(k, expected) in cases {
        // the simulation initializes the model before its first output row
        let result = simulate_dae_with_diagnostics(&compiled.dae, &options(k))
            .unwrap_or_else(|error| panic!("{model} with k = {k} initializes: {error}"));
        assert_eq!(
            initial_value(&result, "k2"),
            k,
            "{model} k2 at t = 0 for k = {k}"
        );
        assert_eq!(
            initial_value(&result, "y"),
            expected,
            "{model} y at t = 0 for k = {k}"
        );
        // the probe evaluates the same initial point
        let probe = eval_dae_at(&compiled.dae, &options(k), &[], 0.0).unwrap();
        assert!(
            probe.report.error.is_none(),
            "{model} probe with k = {k}: {:?}",
            probe.report.error
        );
        assert_eq!(
            solver_value(&probe.report, "y"),
            expected,
            "{model} probe y for k = {k}"
        );
    }
}

/// The discrete index equation `k2 = k` holds at t = 0, so `y = p[k2]` reads
/// `p[3]` when the input is 3, not the declaration default `p[0]`, which the
/// one-based check refuses.
#[test]
fn a_continuous_row_reads_a_discrete_integer_index_settled_at_initialization() {
    check(SOURCE, "Selected", &[(1.0, 1.0), (3.0, 3.0)]);
}

/// `k3 = k2 + 1` reads a discrete value a seeded row defines, so it is seeded
/// after `k2`, and `y = p[k3]` reads `p[k + 1]`.
#[test]
fn a_chain_of_discrete_index_equations_is_settled_in_dependency_order() {
    check(CHAINED, "Chained", &[(1.0, 2.0), (3.0, 4.0)]);
}

/// `x = pre(a)` at the initialization pass is the start of `a`, which the
/// parameter `n` gives: with `n` overridden to 5 the initial pass reads 5 and
/// the event iteration after it sees `change(x)` (5 to the seeded 1) as an
/// event of its own, so `cnt` counts two activations; with the default 1 the
/// seeded value equals the start and `cnt` counts the initial one. The
/// generated C saves the value it overwrites at seed time (the live instance
/// value); the solver must take the start from the same place.
#[test]
fn the_pre_of_a_seeded_target_keeps_the_start_an_overridden_parameter_gives() {
    let compiled = Compiler::new()
        .model("StartFromParameter")
        .compile_str(START_FROM_PARAMETER, "StartFromParameter.mo")
        .unwrap();
    for (n, first, cnt) in [(None, 1.0, 1.0), (Some(5.0), 5.0, 2.0)] {
        let options = SimOptions {
            t_end: 0.01,
            dt: Some(0.01),
            param_overrides: n.map(|n| ("n".into(), n)).into_iter().collect(),
            ..SimOptions::default()
        };
        let result = simulate_dae_with_diagnostics(&compiled.dae, &options)
            .unwrap_or_else(|error| panic!("n = {n:?} initializes: {error}"));
        assert_eq!(
            initial_value(&result, "first"),
            first,
            "first for n = {n:?}"
        );
        assert_eq!(initial_value(&result, "cnt"), cnt, "cnt for n = {n:?}");
    }
}
