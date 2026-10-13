//! A `for` loop over a run-time Integer bound keeps the static capacity of the
//! bound as its domain and carries `i <= n` as the fold's exit test, so the
//! passes past the bound never run (MLS §11.2.2). A `while` condition that
//! calls a pure builtin is carried the same way (MLS §11.2.3).

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const MODELS: &str = r#"
package Bound
  function sumPositive "the loop bound is the number of positive entries"
    input Real x[6];
    output Real y;
  protected
    Integer n;
  algorithm
    n := 0;
    for k in 1:6 loop
      if x[k] > 0 then
        n := n + 1;
      end if;
    end for;
    y := 0;
    for r in 1:n loop
      y := y + x[r]*r;
    end for;
  end sumPositive;
  function halvings "a pure builtin in the while condition"
    input Integer size;
    output Integer steps;
  protected
    Integer root;
  algorithm
    steps := 0;
    root := 1;
    while root <= div(size, 2) and steps < 5 loop
      root := 2*root;
      steps := steps + 1;
    end while;
  end halvings;
  function suppressed "a guarded body whose nested loops have run-time bounds"
    input Integer n;
    input Integer limit;
    output Real taken;
  protected
    Real marks[9];
  algorithm
    marks := zeros(9);
    taken := 0;
    for c in 1:min(n,9) loop
      if taken < limit and marks[c] == 0 then
        taken := taken + 1;
        for k in max(1,c-1):min(9,c+1) loop
          marks[k] := 1;
        end for;
      end if;
    end for;
  end suppressed;
  model Run
    Real total = sumPositive({1, -2, 3, -4, 5, 6} .* time);
    Real none = sumPositive({-1, -2, -3, -4, -5, -6});
    Real counted = halvings(integer(8 + 8*time));
    Real suppressedCount = suppressed(integer(5 + 4*time), 3);
  end Run;
end Bound;
"#;

fn simulate() -> SimResult {
    let compiled = Compiler::new()
        .model("Bound.Run")
        .compile_str(MODELS, "Bound.mo")
        .expect("the model compiles");
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .expect("the model simulates")
}

fn at(result: &SimResult, name: &str, time: f64) -> f64 {
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .expect("the result records the column");
    let row = result
        .times
        .iter()
        .position(|sample| (sample - time).abs() < 1e-9)
        .expect("the result has a sample at the time");
    result.data[column][row]
}

/// Whether the function has a fold with an exit test, and whether it reads
/// that fold's own binder.
fn exit_tests(view: rumoca_ir_dae::DaeView<'_>, function: &str) -> (bool, bool) {
    let function = (0..view.function_count())
        .filter_map(|index| view.function_id(index).and_then(|id| view.function(id)))
        .find(|candidate| candidate.name().as_str().ends_with(function))
        .expect("the function remains visible");
    function
        .statements()
        .filter_map(|statement| match statement {
            rumoca_ir_dae::FunctionStatementView::For { fold, .. } => view.function_fold(fold),
            _ => None,
        })
        .filter_map(|fold| {
            fold.continuation()
                .map(|condition| (condition, fold.domain()))
        })
        .fold((false, false), |(_, binder), (condition, domain)| {
            (
                true,
                binder || rumoca_ir_dae::expr_reads_binder_of(view, condition, domain),
            )
        })
}

#[test]
fn a_run_time_for_bound_is_carried_as_the_exit_test_of_a_static_capacity_fold() {
    let compiled = Compiler::new()
        .model("Bound.Run")
        .compile_str(MODELS, "Bound.mo")
        .expect("the model compiles");
    compiled.dae.inspect(|view| {
        assert_eq!(exit_tests(view, "sumPositive"), (true, true));
        assert_eq!(exit_tests(view, "halvings"), (true, false));
        assert_eq!(exit_tests(view, "suppressed"), (true, true));
    });
}

#[test]
fn passes_past_a_run_time_for_bound_leave_the_result_unchanged() {
    let result = simulate();
    assert_eq!(at(&result, "none", 0.5), 0.0);
    assert_eq!(at(&result, "total", 0.0), 0.0);
    // x = {1,-2,3,-4,5,6} has n = 4 positives, so r runs over 1:4 of the 6
    // passes the static capacity allows.
    assert!((at(&result, "total", 1.0) - (1.0 - 4.0 + 9.0 - 16.0)).abs() < 1e-12);
}

#[test]
fn a_while_condition_with_a_pure_builtin_exits_early_and_counts_the_same() {
    let result = simulate();
    // root doubles while root <= div(size, 2): size 8 gives 3 steps, 16 gives 4.
    assert_eq!(at(&result, "counted", 0.0), 3.0);
    assert_eq!(at(&result, "counted", 1.0), 4.0);
}

#[test]
fn nested_run_time_loops_in_a_guarded_body_keep_the_outer_exit_and_the_result() {
    let result = simulate();
    // Candidates 1, 3 and 5 are taken before the limit of 3 stops the rest;
    // each marks its neighbours so 2 and 4 are skipped.
    assert_eq!(at(&result, "suppressedCount", 0.0), 3.0);
    assert_eq!(at(&result, "suppressedCount", 1.0), 3.0);
}
