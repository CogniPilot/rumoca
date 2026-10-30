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
  function relationEntryCheck "nonfold call-scoped assertion under a relation-triggered when"
    input Integer i;
    input Real limit;
    output Real y;
  algorithm
    assert(table[i] > limit, "relation entry is above the limit");
    y := table[i];
  end relationEntryCheck;
  model RelationCallHolds
    extends Counter;
    discrete Real value(start = 0, fixed = true);
  algorithm
    when time >= 0.5*dt then
      value := relationEntryCheck(k + 1, 0);
      checked := pre(checked) + 1;
    end when;
  end RelationCallHolds;
  model RelationCallFails
    extends Counter;
    discrete Real value(start = 0, fixed = true);
  algorithm
    when time >= 0.5*dt then
      value := relationEntryCheck(k + 1, 15);
      checked := pre(checked) + 1;
    end when;
  end RelationCallFails;
  model RelationCallIndexFails
    extends Counter;
    discrete Real value(start = 0, fixed = true);
  algorithm
    when time >= 0.5*dt then
      value := relationEntryCheck(k + 5, 0);
      checked := pre(checked) + 1;
    end when;
  end RelationCallIndexFails;
  model TriggerGuard "control: a compound when trigger keeps its edge buffer"
    extends Counter;
    discrete Real value(start = 0, fixed = true);
  equation
    when sample(0.5*dt, dt) and k >= 0 then
      value = table[k + 1];
      checked = pre(checked) + 1;
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

// MLS §11.2.7: a relation-triggered when has no periodic clock owner. These
// nonfold calls target the guarded-assignment activation path, while retaining
// inactive-index, active-false and active-index-fault discrimination.
#[test]
fn an_inactive_relation_when_skips_a_nonfold_function_assertion() {
    let result = simulate("InactiveWhen.RelationCallHolds")
        .expect("the out-of-range initial index must remain unevaluated before the relation rises");
    assert_eq!(final_value(&result, "checked"), 1.0);
    assert_eq!(final_value(&result, "value"), 10.0);
}

#[test]
fn an_active_relation_when_reports_a_nonfold_function_assertion() {
    let error = simulate("InactiveWhen.RelationCallFails")
        .expect_err("the first active entry is10, below15");
    assert!(
        error.contains("relation entry is above the limit"),
        "{error}"
    );
}

#[test]
fn an_active_relation_when_keeps_a_nonfold_index_fault() {
    let error = simulate("InactiveWhen.RelationCallIndexFails")
        .expect_err("the active function reads table5");
    assert!(
        error.contains("index 5") && error.contains("is outside 1..=3"),
        "{error}"
    );
}

/// Change only the selected model's start; do not mask another model's binding.
fn runtime_index_source(model: &str, binding: i32, start: Option<i32>) -> String {
    let name = model.strip_prefix("InactiveWhen.").expect("fixture model");
    let opening = format!("  model {name} ");
    let closing = format!("  end {name};");
    assert_eq!(SOURCE.matches(&opening).count(), 1);
    let (prefix, rest) = SOURCE.split_once(&opening).unwrap();
    let (body, suffix) = rest.split_once(&closing).expect("fixture model end");
    let needle = format!("Integer i(start = {binding}) =");
    assert_eq!(
        body.matches(&needle).count(),
        1,
        "exact selected index seed"
    );
    let replacement = start.map_or_else(
        || "Integer i =".to_owned(),
        |value| format!("Integer i(start = {value}) ="),
    );
    let body = body.replacen(&needle, &replacement, 1);
    format!("{prefix}{opening}{body}{closing}{suffix}")
}

fn simulate_runtime_index_source(model: &str, source: &str) -> Result<SimResult, String> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "issue468_runtime_integer.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.02,
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            ..SimOptions::default()
        },
    )
    .map_err(|error| format!("{error} | {error:?}"))
}

/// Issue #468: bindings must win over default, matching and conflicting seeds.
#[test]
fn runtime_integer_function_indices_use_bindings_not_declaration_seeds() {
    for (model, expected, binding, conflicting) in [
        ("InactiveWhen.NoAssertCall", 60.0, 3, 1),
        ("InactiveWhen.DirectHolds", 20.0, 2, 3),
    ] {
        for start in [None, Some(binding), Some(conflicting)] {
            let source = runtime_index_source(model, binding, start);
            let result = simulate_runtime_index_source(model, &source)
                .unwrap_or_else(|error| panic!("{model}/{start:?}: {error}"));
            for (name, expected) in [("i", f64::from(binding)), ("y", expected)] {
                let column = result
                    .names
                    .iter()
                    .position(|candidate| candidate == name)
                    .unwrap_or_else(|| panic!("{name} is recorded"));
                assert!(!result.data[column].is_empty());
                assert!(
                    result.data[column].iter().all(|&value| value == expected),
                    "{model}/{start:?}/{name}: {:?}",
                    result.data[column]
                );
            }
        }
    }
}

#[test]
fn runtime_integer_index_faults_report_bound_value_and_statement_order() {
    for (model, message) in [
        (
            "InactiveWhen.DirectOwnMessage",
            "entry index 4 is outside the table",
        ),
        ("InactiveWhen.DirectEarlyFault", "is outside 1..=3"),
    ] {
        for start in [None, Some(4), Some(1)] {
            let source = runtime_index_source(model, 4, start);
            let error = simulate_runtime_index_source(model, &source)
                .expect_err("the actual bound index 4 is invalid");
            assert!(
                error.contains("index 4") && error.contains(message),
                "{model}/{start:?}: {error}"
            );
            assert!(!error.contains("index 0"), "seed must not win: {error}");
            if model.ends_with("DirectEarlyFault") {
                assert!(
                    !error.contains("early index 4"),
                    "the indexed statement before the assertion must fail first: {error}"
                );
            }
        }
    }
}

#[test]
fn runtime_integer_index_is_valid_after_initial_state_projection() {
    let source = SOURCE.replace(
        "end InactiveWhen;",
        r#"  model ProjectedIndex
    Real x(start = 0, fixed = false);
    Integer i = integer(x);
    Real y = entry(i);
  initial equation
    x = 2;
  equation
    der(x) = 0;
  end ProjectedIndex;
end InactiveWhen;"#,
    );
    let result = simulate_runtime_index_source("InactiveWhen.ProjectedIndex", &source)
        .expect("the index is valid at the solved initial state, not its seed");
    for (name, expected) in [("x", 2.0), ("i", 2.0), ("y", 20.0)] {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        assert!(!result.data[column].is_empty());
        assert!(
            result.data[column].iter().all(|&value| value == expected),
            "{name}: {:?}",
            result.data[column]
        );
    }
}

/// Issue #468's R3 callback regressed this stateless Boolean/event network.
/// Keep the actual MSL class in the normal suite, not only the external canary.
#[test]
fn initialization_bindings_preserve_msl_boolean_network_convergence() {
    let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    // Jobs without the pinned MSL checkout skip it, as the other MSL tests do.
    if !root.is_dir() {
        return;
    }
    let model = "Modelica.Blocks.Examples.BooleanNetwork1";
    let compiled = Compiler::new()
        .model(model)
        .source_root(root.to_string_lossy().as_ref())
        .compile_str(
            "package BooleanProbe import Modelica; end BooleanProbe;",
            "boolean_probe.mo",
        )
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.02,
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} initialization converges: {error:?}"));
    assert!(!result.data.is_empty(), "the model records its outputs");
    assert!(
        result
            .data
            .iter()
            .all(|column| { !column.is_empty() && column.iter().all(|value| value.is_finite()) })
    );
}
