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
  end Functions;
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
