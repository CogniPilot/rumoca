//! MLS 3.7 section 10.5: an element assignment `x[i, j] := e` in a function
//! replaces one element. When every subscript is an Integer literal proved
//! inside its extent, the element is known when the function is translated,
//! so the update is the static slice update of a one-element value and cannot
//! fault. Every other element assignment keeps the checked run-time update:
//! an index past its extent faults only when the assignment runs, exactly as
//! before.

use rumoca::Compiler;
use rumoca_ir_solve::{SolveOperation, TypedProgram};
use rumoca_sim::{SimOptions, eval_dae_at, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
function wedge "the skew matrix of v, one literal element at a time"
  input Real v[3];
  output Real S[3, 3];
algorithm
  S[1, 1] := 0;     S[1, 2] := -v[3]; S[1, 3] := v[2];
  S[2, 1] := v[3];  S[2, 2] := 0;     S[2, 3] := -v[1];
  S[3, 1] := -v[2]; S[3, 2] := v[1];  S[3, 3] := 0;
end wedge;

function edges "first and last element, and one cell written twice"
  input Real a;
  input Real b;
  output Real x[4];
algorithm
  x := {10, 20, 30, 40};
  x[1] := a;
  x[4] := b;
  x[2] := a;
  x[2] := b;
end edges;

function pastEnd "an element past the extent, written only when reach holds"
  input Real a;
  input Boolean reach;
  output Real x[3];
algorithm
  x := {1, 2, 3};
  if reach then
    x[4] := a;
  end if;
  x[3] := a;
end pastEnd;

function dynamicIndex
  input Real a;
  input Integer k;
  output Real x[3];
algorithm
  x := {1, 2, 3};
  x[k] := a;
end dynamicIndex;

function checkedValue "the value of a literal update is a checked Integer division"
  input Integer d;
  output Real x[2];
algorithm
  x := {1, 2};
  x[2] := div(6, d);
end checkedValue;

function zeroIndex "a literal index 0, written only when reach holds"
  input Real a;
  input Boolean reach;
  output Real x[2];
algorithm
  x := {1, 2};
  if reach then
    x[0] := a;
  end if;
end zeroIndex;

model Wedge
  Real S[3, 3] = wedge({1 + time, 2 + time, 3 + time});
end Wedge;

model Edges
  Real x[4] = edges(1 + time, 2 + time);
end Edges;

model PastEndUnreached
  Real x[3] = pastEnd(time, time > 10);
end PastEndUnreached;

model PastEndReached
  Real x[3] = pastEnd(time, time > 0.5);
end PastEndReached;

model DynamicIndex
  discrete Integer k(start = 1, fixed = true);
  Real x[3] = dynamicIndex(time, k);
equation
  when time > 0.5 then
    k = 3;
  end when;
end DynamicIndex;

model ZeroUnreached
  Real x[2] = zeroIndex(time, time > 10);
end ZeroUnreached;

model ZeroReached
  Real x[2] = zeroIndex(time, time > 0.5);
end ZeroReached;

model CheckedValueReached
  discrete Integer d(start = 3, fixed = true);
  Real x[2] = checkedValue(d);
equation
  when time > 0.5 then
    d = 0;
  end when;
end CheckedValueReached;
"#;

/// The region bodies of `operation`.
fn regions(operation: &SolveOperation) -> Vec<&TypedProgram> {
    match operation {
        SolveOperation::Fold {
            transition,
            continuation,
            ..
        } => std::iter::once(transition.body())
            .chain(continuation.as_deref().map(|region| region.body()))
            .collect(),
        SolveOperation::Map { body, .. } => vec![body.body()],
        SolveOperation::AssignedConditional { program } => {
            vec![program.if_true().body(), program.if_false().body()]
        }
        SolveOperation::Conditional {
            if_true, if_false, ..
        } => vec![if_true.body(), if_false.body()],
        _ => Vec::new(),
    }
}

/// Adds the (run-time `UpdateElement`, static `UpdateSlice`) operations of
/// `program`, nested regions included, to `counts`.
fn count_updates(program: &TypedProgram, counts: &mut (usize, usize)) {
    for spanned in program.operations() {
        match spanned.operation() {
            SolveOperation::UpdateElement { .. } => counts.0 += 1,
            SolveOperation::UpdateSlice { .. } => counts.1 += 1,
            _ => {}
        }
        for region in regions(spanned.operation()) {
            count_updates(region, counts);
        }
    }
}

/// Element updates of every typed owner of `model`: (run-time, static).
fn element_updates(model: &str) -> (usize, usize) {
    let compiled = compile(model);
    let lowered = rumoca_phase_solve::lower_solve_model(
        &compiled.dae,
        &std::collections::HashMap::new(),
        |_| {},
    )
    .unwrap_or_else(|error| panic!("{model} lowers: {error:?}"));
    let mut counts = (0, 0);
    for owner in lowered.model().pure_calls.owners() {
        count_updates(owner.body(), &mut counts);
    }
    counts
}

fn compile(model: &str) -> rumoca::CompilationResult {
    Compiler::new()
        .model(model)
        .compile_str(SOURCE, "LiteralElementUpdates.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"))
}

/// Solver values of `model` at `time`, by name.
fn values_at(model: &str, time: f64) -> Vec<(String, f64)> {
    let probe = eval_dae_at(&compile(model).dae, &SimOptions::default(), &[], time)
        .unwrap_or_else(|error| panic!("{model} evaluates: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    probe
        .report
        .solver_y
        .iter()
        .map(|slot| (slot.name.clone(), slot.value))
        .collect()
}

fn value(values: &[(String, f64)], name: &str) -> f64 {
    values
        .iter()
        .find(|(slot, _)| slot == name)
        .unwrap_or_else(|| panic!("{name} is a solver value: {values:?}"))
        .1
}

fn simulate(model: &str) -> Result<rumoca_sim::SimResult, String> {
    simulate_dae_with_diagnostics(
        &compile(model).dae,
        &SimOptions {
            t_end: 1.0,
            ..Default::default()
        },
    )
    .map_err(|error| error.to_string())
}

fn final_value(result: &rumoca_sim::SimResult, name: &str) -> f64 {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} is recorded"));
    *result.data[index].last().expect("a recorded sample")
}

#[test]
fn in_bounds_literal_element_assignments_lower_to_static_slice_updates() {
    let (dynamic, slices) = element_updates("Wedge");
    assert_eq!(
        dynamic, 0,
        "every literal element update of wedge is static"
    );
    assert_eq!(slices, 9, "one static slice update per element assignment");
    assert_eq!(element_updates("Edges"), (0, 4));
}

#[test]
fn static_element_updates_keep_every_element_value() {
    let values = values_at("Wedge", 0.25);
    let v: [f64; 3] = [1.25, 2.25, 3.25];
    let expected: [[f64; 3]; 3] = [[0.0, -v[2], v[1]], [v[2], 0.0, -v[0]], [-v[1], v[0], 0.0]];
    for (row, cells) in expected.iter().enumerate() {
        for (column, cell) in cells.iter().enumerate() {
            let name = format!("S[{},{}]", row + 1, column + 1);
            assert_eq!(value(&values, &name).to_bits(), cell.to_bits(), "{name}");
        }
    }
    // First and last element, and the last of two writes to one cell.
    let values = values_at("Edges", 0.25);
    let expected: [f64; 4] = [1.25, 2.25, 30.0, 2.25];
    for (ordinal, cell) in expected.iter().enumerate() {
        let name = format!("x[{}]", ordinal + 1);
        assert_eq!(value(&values, &name).to_bits(), cell.to_bits(), "{name}");
    }
}

#[test]
fn an_update_past_the_extent_stays_a_run_time_update() {
    // x[4] keeps its checked run-time update; the in-bounds x[3] is static.
    assert_eq!(element_updates("PastEndUnreached"), (1, 1));
    assert_eq!(element_updates("PastEndReached"), (1, 1));
    assert_eq!(element_updates("DynamicIndex"), (1, 0));
}

#[test]
fn an_unreached_update_past_the_extent_compiles_and_runs() {
    let result = simulate("PastEndUnreached").expect("x[4] is never written");
    assert_eq!(final_value(&result, "x[3]"), 1.0);
    assert_eq!(final_value(&result, "x[1]"), 1.0);
}

#[test]
fn a_reached_update_past_the_extent_faults_when_it_runs() {
    let error = simulate("PastEndReached").expect_err("x[4] is written after t = 0.5");
    // The same cause and span as before: the subscript `4` of `x[4]`.
    assert!(
        error.contains("native tensor index is out of bounds: index 4 is outside 1..=3")
            && error.contains("start: BytePos(700), end: BytePos(701)"),
        "unexpected failure: {error}"
    );
}

#[test]
fn a_run_time_index_keeps_its_checked_update() {
    let result = simulate("DynamicIndex").expect("k stays inside 1..3");
    assert_eq!(final_value(&result, "x[3]"), 1.0);
    assert_eq!(final_value(&result, "x[1]"), 1.0);
}

#[test]
fn the_checked_value_of_a_static_update_still_faults_first() {
    let error = simulate("CheckedValueReached").expect_err("div(6, 0) runs after t = 0.5");
    // The same fault as the checked run-time update gave before.
    assert_eq!(
        error,
        "invalid Solve-IR row: native Integer quotient has a zero divisor or no representable result"
    );
    assert_eq!(element_updates("CheckedValueReached"), (0, 1));
}

#[test]
fn a_literal_index_zero_is_refused_at_translation_as_before() {
    // The DAE owner refuses a non-positive literal subscript before Solve
    // lowering sees it, whether or not the assignment could run.
    for model in ["ZeroUnreached", "ZeroReached"] {
        let error = Compiler::new()
            .model(model)
            .compile_str(SOURCE, "LiteralElementUpdates.mo")
            .expect_err(model);
        assert!(
            format!("{error:?}").contains("one-based positive integers"),
            "{model}: {error:?}"
        );
    }
}
