//! `while` loops in Modelica functions with a proven iteration bound
//! (MLS 3.7 §11.2.3), and the loops inside conditional branches they sit in.
//!
//! `Blocks.Sources.TimeTable.getInterpolationCoefficients` advances a table
//! index with `while next < nrow and tp >= table[next, 1] loop next := next +
//! 1; end while;` inside the `else` branch of a conditional. The loop runs at
//! most `nrow` times (`next` indexes `table`, so it is at least 1, and grows by
//! 1 while it stays below `nrow`), so it lowers to the compact `for` owner with
//! its condition as a guard; the branch keeps the values every path defines.
//! Its `when` algorithm reads `last` before assigning it, which MLS §11.1.2
//! makes `pre(last)`.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const MODELS: &str = r#"
package While
  function advance "counter proven at least 1 by the table index it reads"
    input Real table[:, 2];
    input Real tp;
    input Integer last;
    output Integer next;
  protected
    Integer nrow = size(table, 1);
  algorithm
    next := last;
    while next < nrow and tp >= table[next, 1] loop
      next := next + 1;
    end while;
  end advance;
  function halve "counter started at a literal, as in the Media inversions"
    input Real x;
    output Real y;
  protected
    Integer i;
    Boolean found;
  algorithm
    y := x;
    found := false;
    i := 1;
    while i < 100 and not found loop
      y := y/2;
      found := y < 0.1;
      i := i + 1;
    end while;
  end halve;
  function branched "a loop inside one branch of a conditional"
    input Real table[:, 2];
    input Real tp;
    output Real a;
  protected
    Integer next;
    Integer nrow = size(table, 1);
  algorithm
    next := 1;
    if tp < 0 then
      a := -1;
    else
      while next < nrow and tp >= table[next, 1] loop
        next := next + 1;
      end while;
      a := table[next, 2];
    end if;
  end branched;
  function firstAbove "a branch writes the loop limit before the increment"
    input Real x[:];
    input Real v;
    output Integer k;
  protected
    Integer n = size(x, 1);
    Integer i;
  algorithm
    k := 0;
    i := 1;
    while i < n loop
      if x[i] > v then
        k := i;
        i := n;
      else
        k := 0;
      end if;
      i := i + 1;
    end while;
  end firstAbove;
  model Functions
    parameter Real table[4, 2] = [0, 0; 0.25, 1; 0.5, 2; 0.75, 3];
    Real index = advance(table, time, 1);
    Real halved = halve(1 + time);
    Real selected = branched(table, time - 0.1);
    Real above = firstAbove({0.1, 0.4, 0.7, 1.0}, time);
    Real divided = shrinking(1 + time, 16);
    Real emptyDivision = shrinking(1 + time, 0);
    Real descended = descending(1 + time);
    Real nested = nestedShrinking(1 + time);
    Real walked = shiftedWalk({1, 2, 3}, time);
  end Functions;
  function shrinking
    input Real x;
    input Integer start;
    output Real y;
  protected
    Integer gap;
  algorithm
    gap := start;
    y := x;
    while gap > 0 loop
      y := y + gap;
      gap := div(gap, 2);
    end while;
  end shrinking;
  function descending
    input Real x;
    output Real y;
  protected
    Integer k;
  algorithm
    k := 3;
    y := x;
    while k >= 0 loop
      y := y + k;
      k := k - 1;
    end while;
  end descending;
  function nestedShrinking
    input Real x;
    output Real y;
  protected
    Integer gap;
    Integer i;
    Integer j;
  algorithm
    y := x;
    gap := 4;
    while gap > 0 loop
      i := 0;
      while i < 3 loop
        j := i;
        while j >= 0 loop
          y := y + 1;
          j := j - 1;
        end while;
        i := i + 1;
      end while;
      gap := div(gap, 2);
    end while;
  end nestedShrinking;
  function shiftedWalk
    input Real samples[:];
    input Real threshold;
    output Real total;
  protected
    Integer position;
    Boolean again;
  algorithm
    total := 0;
    position := size(samples, 1) - 1;
    again := true;
    while again loop
      total := total + samples[position + 1];
      position := position - 1;
      if position >= 0 then
        again := samples[position + 1] > threshold;
      else
        again := false;
      end if;
    end while;
  end shiftedWalk;
  function coefficients "Blocks.Sources.TimeTable.getInterpolationCoefficients, reduced"
    input Real table[:, 2];
    input Real tp;
    input Integer last;
    output Real a;
    output Real b;
    output Real nextEvent;
    output Integer next;
  protected
    Integer nrow = size(table, 1);
  algorithm
    next := last;
    nextEvent := tp;
    if nrow < 2 then
      a := 0;
      b := table[1, 2];
    else
      while next < nrow and tp >= table[next, 1] loop
        next := next + 1;
      end while;
      if next < nrow then
        nextEvent := table[next, 1];
      end if;
      a := (table[next, 2] - table[next - 1, 2])/(table[next, 1] - table[next - 1, 1]);
      b := table[next - 1, 2] - a*table[next - 1, 1];
    end if;
  end coefficients;
  model Table
    parameter Real table[4, 2] = [0, 0; 0.25, 1; 0.5, 2; 0.75, 3];
    discrete Real a;
    discrete Real b;
    Integer last(start = 1);
    discrete Real nextEvent(start = 0, fixed = true);
    discrete Real nextEventScaled(start = 0, fixed = true);
    Real timeScaled;
    Real y;
  algorithm
    when {time >= pre(nextEvent), initial()} then
      (a, b, nextEventScaled, last) := coefficients(table, timeScaled, last);
      nextEvent := nextEventScaled;
    end when;
  equation
    timeScaled = time;
    y = a*timeScaled + b;
  end Table;
end While;
"#;

fn simulate(model: &str) -> SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "While.mo")
        .expect("the model compiles");
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.1),
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

#[test]
fn bounded_while_loops_compute_their_iterated_values() {
    let result = simulate("While.Functions");
    // tp = 0.3: rows 1 and 2 are passed, next stops at 3; tp = 0.8: at nrow.
    assert_eq!(at(&result, "index", 0.3), 3.0);
    assert_eq!(at(&result, "index", 0.8), 4.0);
    // 1.0 halves four times to 0.0625; 1.6 halves to 0.1, then 0.05.
    assert!((at(&result, "halved", 0.0) - 0.0625).abs() < 1e-12);
    assert!((at(&result, "halved", 0.6) - 0.05).abs() < 1e-12);
    // time - 0.1 < 0 selects -1; at 0.4 the loop stops at row 3 (value 2).
    assert_eq!(at(&result, "selected", 0.0), -1.0);
    assert_eq!(at(&result, "selected", 0.4), 2.0);
    // The first entry above 0.3 is the second; above 0.8 none within i < 4.
    assert_eq!(at(&result, "above", 0.3), 2.0);
    assert_eq!(at(&result, "above", 0.8), 0.0);
    assert!((at(&result, "divided", 0.3) - 32.3).abs() < 1e-12);
    assert!((at(&result, "emptyDivision", 0.3) - 1.3).abs() < 1e-12);
    assert!((at(&result, "descended", 0.3) - 7.3).abs() < 1e-12);
    assert!((at(&result, "nested", 0.3) - 19.3).abs() < 1e-12);
    assert_eq!(at(&result, "walked", 0.3), 6.0);
    assert_eq!(at(&result, "walked", 1.0), 5.0);
}

#[test]
fn an_event_algorithm_reads_its_target_entry_value_as_pre() {
    // The Appendix B solved-form proof admits `last := f(last)` inside a
    // `when` algorithm: the entry read is `pre(last)`, so no current-value
    // cycle exists. (The MSL `TimeTable` itself simulates in the parity sweep.)
    Compiler::new()
        .model("While.Table")
        .compile_str(MODELS, "While.mo")
        .expect("the event algorithm constructs");
}

#[test]
fn a_bounded_while_fold_ends_at_its_first_false_condition() {
    // `halve` runs while `i < 100 and not found`: its compact fold carries
    // that condition as a continuation instead of running all 100 passes.
    let compiled = Compiler::new()
        .model("While.Functions")
        .compile_str(MODELS, "While.mo")
        .expect("the model compiles");
    compiled.dae.inspect(|view| {
        let function = (0..view.function_count())
            .filter_map(|index| view.function_id(index).and_then(|id| view.function(id)))
            .find(|function| function.name().as_str().ends_with("halve"))
            .expect("the halve function remains visible");
        let continued = function
            .statements()
            .filter_map(|statement| match statement {
                rumoca_ir_dae::FunctionStatementView::For { fold, .. } => Some(fold),
                _ => None,
            })
            .filter_map(|fold| view.function_fold(fold))
            .any(|fold| fold.continuation().is_some());
        assert!(continued, "the while fold carries its condition");
    });
}

/// A `while` that can leave through `break` has no exit at which its
/// condition is known false, so no exit fact or iteration envelope is proven
/// from the condition, and the loop is refused rather than bounded by it.
#[test]
fn a_while_loop_that_can_break_is_refused() {
    let source = r#"
function firstAbove
  input Real x[4];
  input Real limit;
  output Integer k;
protected
  Integer i;
algorithm
  i := 1;
  k := 0;
  while i < 5 loop
    if x[i] > limit then
      k := i;
      break;
    end if;
    i := i + 1;
  end while;
end firstAbove;
model WhileBreak
  input Real limit = 2;
  Real k;
equation
  k = firstAbove({1, 2, 3, 4}, limit + time);
end WhileBreak;
"#;
    let error = Compiler::new()
        .model("WhileBreak")
        .compile_str(source, "WhileBreak.mo")
        .map(|_| ())
        .expect_err("a while loop with break has no proven envelope");
    let message = error.to_string();
    assert!(
        message.contains("`firstAbove`"),
        "unexpected refusal: {message}"
    );
}

#[test]
fn msl_interpolation_searches_forward_and_backward() {
    let root = super::native_linear_solve::msl_root().expect("the required MSL fixture exists");
    let source = r#"
model CheckedInterpolation
  Real backward;
  Real forward;
  Integer backwardIndex;
  Integer forwardIndex;
  Real extrapolated;
  Integer extrapolatedIndex;
equation
  (backward, backwardIndex) = Modelica.Math.Vectors.interpolate(
    {0, 0.25, 0.5, 0.75, 1}, {0, 0.5, 1, 1.5, 2}, time, 4);
  (forward, forwardIndex) = Modelica.Math.Vectors.interpolate(
    {0, 0.25, 0.5, 0.75, 1}, {0, 0.5, 1, 1.5, 2}, time, 1);
  (extrapolated, extrapolatedIndex) = Modelica.Math.Vectors.interpolate(
    {0, 0.25, 0.5, 0.75, 1}, {0, 0.5, 1, 1.5, 2}, 2*time - 0.5, 3);
end CheckedInterpolation;
"#;
    let compiled = Compiler::new()
        .model("CheckedInterpolation")
        .source_root(root.to_string_lossy().as_ref())
        .compile_str(source, "CheckedInterpolation.mo")
        .unwrap_or_else(|error| panic!("the actual MSL interpolation searches construct: {error}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.1),
            ..SimOptions::default()
        },
    )
    .expect("the actual MSL interpolation searches simulate");
    for &time in &result.times {
        for (name, expected) in [
            ("forward", 2.0 * time),
            ("backward", 2.0 * time),
            ("extrapolated", 4.0 * time - 1.0),
        ] {
            let value = at(&result, name, time);
            assert!(
                (value - expected).abs() < 1e-12,
                "{name} at {time}: {value} != {expected}"
            );
        }
        for name in ["forwardIndex", "backwardIndex", "extrapolatedIndex"] {
            let index = at(&result, name, time);
            assert!(index.fract() == 0.0 && (1.0..=4.0).contains(&index));
        }
    }
}

#[test]
fn msl_shell_sort_constructs_both_runtime_directions() {
    let root = super::native_linear_solve::msl_root().expect("the required MSL fixture exists");
    let source = r#"
model CheckedSort
  Real values[5] = {time, 2, -1, 2, -time};
  Real ascending[5];
  Real descending[5];
  Integer ascendingIndex[5];
  Integer descendingIndex[5];
equation
  (ascending, ascendingIndex) = Modelica.Math.Vectors.sort(values, true);
  (descending, descendingIndex) = Modelica.Math.Vectors.sort(values, false);
end CheckedSort;
"#;
    let compiled = Compiler::new()
        .model("CheckedSort")
        .source_root(root.to_string_lossy().as_ref())
        .compile_str(source, "CheckedSort.mo")
        .expect("the actual MSL Shell sort constructs");
    compiled.dae.inspect(|view| {
        let sorts = (0..view.function_count())
            .filter_map(|index| view.function_id(index).and_then(|id| view.function(id)))
            .filter(|function| function.name().as_str().ends_with("Vectors.sort"))
            .collect::<Vec<_>>();
        assert_eq!(sorts.len(), 1, "both calls share one shape specialization");
        for sort in sorts {
            assert!(
                sort.statements().any(|statement| matches!(
                    statement,
                    rumoca_ir_dae::FunctionStatementView::For { .. }
                )),
                "the actual Shell sort retains compact loop owners"
            );
        }
    });
}

/// MLS §11.2.3 evaluates a `while` condition before the first pass even when
/// no pass runs, so a call whose value the condition needs is evaluated. The
/// head facts prove `k < min(check(k), 2)` false from `k = 5` alone, but `min`
/// still needs `check(k)`; a zero-pass proof that dropped the condition would
/// skip the call's failing assertion. (A call MLS §3.3 lets a tool skip, such
/// as `check(k) and false`, is not this case.)
const ZERO_PASS_CALL: &str = r#"
model ZeroPassCall
  function check
    input Integer k;
    output Integer limit;
  algorithm
    assert(k < 0, "the while condition was evaluated");
    limit := 3;
  end check;
  function search
    input Real u;
    output Integer r;
  protected
    Integer k;
  algorithm
    r := 0;
    k := 5;
    while k < min(check(k), 2) loop
      k := k + 1;
      r := r + 1;
    end while;
  end search;
  Real x(start = 0, fixed = true);
  Real y;
equation
  der(x) = 1;
  y = search(x);
end ZeroPassCall;
"#;

#[test]
fn a_zero_pass_while_still_evaluates_a_call_in_its_condition() {
    let compiled = Compiler::new()
        .model("ZeroPassCall")
        .compile_str(ZERO_PASS_CALL, "ZeroPassCall.mo");
    let outcome = compiled.map(|compiled| {
        simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.1),
                ..SimOptions::default()
            },
        )
    });
    let failed = match outcome {
        Err(_) | Ok(Err(_)) => true,
        Ok(Ok(_)) => false,
    };
    assert!(
        failed,
        "the condition's failing assertion must not be skipped by a zero-pass bound"
    );
}
