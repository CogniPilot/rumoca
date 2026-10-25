//! MLS 3.7 §11.2.7: a when-statement body executes only when its condition
//! becomes true, and §8.6 executes no when body during initialization. An
//! `assert` in such a body therefore evaluates its condition only on
//! activation: an index that is valid from the first activation on, but not
//! before it, must not fault while the body is inactive. Once active, a false
//! condition and an out-of-range index still fail exactly as before.
//!
//! `k` starts at -1 and `sample(0, dt)` makes it 0 at t = 0, before the body
//! under `sample(0.5*dt, dt)` first runs at t = 0.05, so `table[k + 1]` is
//! row 1 at that first activation.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package InactiveWhen
  constant Real table[3] = {10, 20, 30};
  partial model Counter
    parameter Real dt = 0.1;
    discrete Integer k(start = -1, fixed = true);
    discrete Integer checked(start = 0, fixed = true);
  equation
    when sample(0, dt) then
      k = pre(k) + 1;
    end when;
  end Counter;
  model Holds "the Phase B fixture shape; the condition holds on every activation"
    extends Counter;
  algorithm
    when sample(0.5*dt, dt) then
      assert(table[k + 1] > 0, "the table entry for this step is positive");
      checked := pre(checked) + 1;
    end when;
  end Holds;
  model Fails "active, valid index, false condition"
    extends Counter;
  algorithm
    when sample(0.5*dt, dt) then
      assert(table[k + 1] > 15, "the first step's entry is below 15");
      checked := pre(checked) + 1;
    end when;
  end Fails;
  model OutOfRange "active, and the index leaves the table at the third activation"
    extends Counter;
  algorithm
    when sample(0.5*dt, dt) then
      assert(table[k + 2] > 0, "the next entry is positive");
      checked := pre(checked) + 1;
    end when;
  end OutOfRange;
  model Warning "the same inactive index under a warning-level assert"
    extends Counter;
  algorithm
    when sample(0.5*dt, dt) then
      assert(table[k + 1] > 0, "the table entry for this step is positive", level = AssertionLevel.warning);
      checked := pre(checked) + 1;
    end when;
  end Warning;
  function entry "table[i], with its own range assert"
    input Integer i;
    output Real y;
  algorithm
    assert(i >= 1 and i <= 3, "entry index " + String(i) + " is outside the table");
    y := table[i];
  end entry;
  model CallHolds "the failing conjunct calls a function with its own assert; inactive at k = -1"
    extends Counter;
  algorithm
    when sample(0.5*dt, dt) then
      assert(entry(k + 1) > 0, "the called entry for this step is positive");
      checked := pre(checked) + 1;
    end when;
  end CallHolds;
  model CallFails "the called function's own assert fails once its index leaves the table"
    extends Counter;
  algorithm
    when sample(0.5*dt, dt) then
      assert(entry(k + 2) > 0, "the called next entry is positive");
      checked := pre(checked) + 1;
    end when;
  end CallFails;
  model EquationHolds "when-equation twin of Holds"
    extends Counter;
  equation
    when sample(0.5*dt, dt) then
      assert(table[k + 1] > 0, "the table entry for this step is positive");
      checked = pre(checked) + 1;
    end when;
  end EquationHolds;
  model ClockedHolds "clocked partition: kc is -1 before the first tick, 0 at it (MLS 16)"
    Clock c = Clock(1, 10);
    Integer kc(start = -1);
  equation
    when c then
      kc = previous(kc) + 1;
      assert(table[kc + 1] > 0, "the clocked table entry is positive");
    end when;
  end ClockedHolds;
  function early "table[i] is read BEFORE its range assert"
    input Integer i;
    output Real y;
  protected
    Real y0;
  algorithm
    y0 := table[i];
    assert(i >= 1 and i <= 3, "early index " + String(i) + " is outside the table");
    y := y0;
  end early;
  function plain "no assert: lowers byte-identically before and after the statement gate"
    input Integer i;
    output Real y;
  algorithm
    y := 2*table[i];
  end plain;
  model DirectHolds "a direct call whose own assert holds"
    Integer i(start = 2) = if time >= 0 then 2 else 1 "runtime value, so the call is not folded; its start equals that value";
    Real y = entry(i);
  end DirectHolds;
  model DirectOwnMessage "a direct call whose own assert fails before its later index"
    Integer i(start = 4) = if time >= 0 then 4 else 1 "runtime value, so the call is not folded; its start equals that value";
    Real y = entry(i);
  end DirectOwnMessage;
  model DirectEarlyFault "the index BEFORE the assert still faults first"
    Integer i(start = 4) = if time >= 0 then 4 else 1 "runtime value, so the call is not folded; its start equals that value";
    Real y = early(i);
  end DirectEarlyFault;
  model NoAssertCall "control for byte identity of an assert-free function"
    Integer i(start = 3) = if time >= 0 then 3 else 1 "runtime value, so the call is not folded; its start equals that value";
    Real y = plain(i);
  end NoAssertCall;
  function positive "its own assert reads table[i]"
    input Integer i;
    output Boolean ok;
  algorithm
    assert(table[i] > 0, "table entry " + String(i) + " is positive");
    ok := true;
  end positive;
  model CallAssertIndex "a called function's own assert reads the index; inactive at k = -1"
    extends Counter;
  algorithm
    when sample(0.5*dt, dt) then
      assert(positive(k + 1), "the called check for this step holds");
      checked := pre(checked) + 1;
    end when;
  end CallAssertIndex;
  function scaledEntry "u*table[i], with an assert that reads table[i]"
    input Real u;
    input Integer i;
    output Real y;
  algorithm
    assert(table[i] > 0, "scaled entry " + String(i) + " is positive");
    y := u*table[i];
  end scaledEntry;
  function scaledOuter "calls scaledEntry, so its assert sits one call deeper"
    input Real u;
    input Integer i;
    output Real y;
  algorithm
    y := scaledEntry(u, i);
  end scaledOuter;
  model ExpandedCallAssertIndex "a state argument, so the call expands in place"
    extends Counter;
    Real x(start = 1, fixed = true);
  equation
    der(x) = 0;
  algorithm
    when sample(0.5*dt, dt) then
      assert(scaledEntry(x, k + 1) > 0, "the scaled entry for this step is positive");
      checked := pre(checked) + 1;
    end when;
  end ExpandedCallAssertIndex;
  model NestedCallAssertIndex "the indexing assert is in a function called by the called function"
    extends Counter;
    Real x(start = 1, fixed = true);
  equation
    der(x) = 0;
  algorithm
    when sample(0.5*dt, dt) then
      assert(scaledOuter(x, k + 1) > 0, "the nested scaled entry for this step is positive");
      checked := pre(checked) + 1;
    end when;
  end NestedCallAssertIndex;
  model TriggerGuard "control: a compound when trigger keeps its edge buffer"
    extends Counter;
    discrete Real value(start = 0, fixed = true);
  algorithm
    when sample(0.5*dt, dt) and k >= 0 then
      value := table[k + 1];
      checked := pre(checked) + 1;
    end when;
  end TriggerGuard;
  model Read "control: the same index in a when-assigned discrete Real"
    extends Counter;
    discrete Real value(start = 0, fixed = true);
  algorithm
    when sample(0.5*dt, dt) then
      value := table[k + 1];
    end when;
  end Read;
end InactiveWhen;
"#;

fn simulate(model: &str) -> Result<SimResult, String> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "inactive_when_assertion.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.26,
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            ..SimOptions::default()
        },
    )
    .map_err(|error| format!("{error} | {error:?}"))
}

fn final_value(result: &SimResult, name: &str) -> f64 {
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} is recorded"));
    *result.data[column].last().expect("samples")
}

#[test]
fn an_inactive_when_assert_does_not_evaluate_its_condition() {
    // Activations at 0.05, 0.15 and 0.25 each check rows 1, 2 and 3.
    let result = simulate("InactiveWhen.Holds").expect("no activation reads outside the table");
    assert_eq!(final_value(&result, "checked"), 3.0);
}

#[test]
fn an_active_false_condition_still_fails() {
    let error = simulate("InactiveWhen.Fails").expect_err("table[1] = 10 is not above 15");
    assert!(
        error.contains("the first step's entry is below 15"),
        "unexpected failure: {error}"
    );
}

#[test]
fn an_active_out_of_range_index_still_faults() {
    // The second activation (k = 1) reads table[3]; the third (k = 2) reads table[4].
    let error = simulate("InactiveWhen.OutOfRange").expect_err("table[4] is out of range");
    // Native execution reports "index 4 is outside 1..=3", the interpreter
    // "Solve-IR tensor index 4 on axis 0 is outside 1..=3".
    assert!(
        error.contains("index 4") && error.contains("is outside 1..=3"),
        "unexpected failure: {error}"
    );
}

#[test]
fn an_inactive_when_assert_does_not_run_a_called_functions_assert() {
    let result =
        simulate("InactiveWhen.CallHolds").expect("no activation calls entry outside the table");
    assert_eq!(final_value(&result, "checked"), 3.0);
}

#[test]
fn an_active_called_functions_assert_still_fails() {
    // The third activation (k = 2) calls entry(4).
    let error = simulate("InactiveWhen.CallFails").expect_err("entry(4) asserts");
    assert!(
        error.contains("entry index 4 is outside the table"),
        "unexpected failure: {error}"
    );
}

#[test]
fn an_inactive_warning_level_assert_does_not_evaluate_its_condition() {
    let result = simulate("InactiveWhen.Warning").expect("no activation reads outside the table");
    assert_eq!(final_value(&result, "checked"), 3.0);
}

#[test]
fn a_when_assigned_read_of_the_same_index_completes() {
    let result = simulate("InactiveWhen.Read").expect("the guarded assignment is lazy");
    assert_eq!(final_value(&result, "value"), 30.0);
}

#[test]
fn an_inactive_when_equation_assert_does_not_evaluate_its_condition() {
    let result =
        simulate("InactiveWhen.EquationHolds").expect("no activation reads outside the table");
    assert_eq!(final_value(&result, "checked"), 3.0);
}

#[test]
fn a_clocked_when_assert_does_not_evaluate_before_its_first_tick() {
    // Ticks at 0, 0.1 and 0.2 read rows 1 to 3; before the first tick kc is -1.
    let result = simulate("InactiveWhen.ClockedHolds").expect("no tick reads outside the table");
    assert_eq!(final_value(&result, "kc"), 2.0);
}

#[test]
fn a_direct_call_whose_assert_holds_returns_its_value() {
    let result = simulate("InactiveWhen.DirectHolds").expect("entry(2) is in range");
    assert_eq!(final_value(&result, "y"), 20.0);
}

#[test]
fn a_called_functions_failed_assert_is_reported_before_its_later_index() {
    let error = simulate("InactiveWhen.DirectOwnMessage").expect_err("entry(4) asserts");
    assert!(
        error.contains("entry index 4 is outside the table"),
        "unexpected failure: {error}"
    );
}

#[test]
fn an_index_before_a_called_functions_assert_still_faults_first() {
    // MLS 3.7 statement order: `y0 := table[i]` runs before the assert.
    let error = simulate("InactiveWhen.DirectEarlyFault").expect_err("table[4] is read first");
    assert!(
        error.contains("index 4") && error.contains("is outside 1..=3"),
        "unexpected failure: {error}"
    );
}

#[test]
fn an_assert_free_function_call_is_unchanged() {
    let result = simulate("InactiveWhen.NoAssertCall").expect("plain(3) is in range");
    assert_eq!(final_value(&result, "y"), 60.0);
}

#[test]
fn an_inactive_called_functions_indexing_assert_does_not_evaluate() {
    let result = simulate("InactiveWhen.CallAssertIndex")
        .expect("positive(0) is never called while inactive");
    assert_eq!(final_value(&result, "checked"), 3.0);
}

#[test]
fn an_inactive_expanded_call_does_not_evaluate_its_indexing_assert() {
    let result = simulate("InactiveWhen.ExpandedCallAssertIndex")
        .expect("scaledEntry(x, 0) is never evaluated while inactive");
    assert_eq!(final_value(&result, "checked"), 3.0);
}

#[test]
fn an_inactive_nested_call_does_not_evaluate_its_indexing_assert() {
    let result = simulate("InactiveWhen.NestedCallAssertIndex")
        .expect("scaledOuter(x, 0) is never evaluated while inactive");
    assert_eq!(final_value(&result, "checked"), 3.0);
}

#[test]
fn a_compound_when_trigger_keeps_its_edge_buffer() {
    let result = simulate("InactiveWhen.TriggerGuard").expect("the trigger reads its own buffer");
    // A lost buffer would make the edge the level and fire on every pass.
    assert_eq!(final_value(&result, "checked"), 3.0);
    assert_eq!(final_value(&result, "value"), 30.0);
}
