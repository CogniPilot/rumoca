//! MLS 3.7 sections 10.5.1 and 11.1.2: an array assignment `a[i:j] := v` or a
//! read `a[i:j]` whose bounds read run-time values (a loop binder, a
//! loop-local piecewise bound) has the fixed extent that the function shape
//! proof sizes it by. Solve lowering keeps the window as one packed coordinate
//! range on a compact tensor update (or a fold patch of the carried tensor),
//! never one select per base element.
//!
//! The function inputs are model inputs, so the call has no derivative
//! relation and Solve lowering expands its body in place.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

fn evaluate(source: &str, names: &[&str]) -> Vec<f64> {
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(source, "Probe.mo")
        .unwrap_or_else(|error| panic!("the model compiles: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("the model lowers to Solve: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    names
        .iter()
        .map(|name| {
            probe
                .report
                .solver_y
                .iter()
                .find(|slot| slot.name == *name)
                .unwrap_or_else(|| panic!("the model exposes {name}"))
                .value
        })
        .collect()
}

#[test]
fn a_slice_start_read_from_the_loop_binder_updates_its_window() {
    // i=1 writes v[1:2] = {s, 10}, i=2 writes v[2:3] = {2s, 20},
    // i=3 writes v[3:4] = {3s, 30}; with s = 1 the vector is {1, 2, 3, 30}.
    let values = evaluate(
        r#"
function Window
  input Real s;
  output Real v[4];
algorithm
  v := zeros(4);
  for i in 1:3 loop
    v[i:i + 1] := {s * i, 10 * i};
  end for;
end Window;
model Probe
  input Real s = 1;
  output Real v[4];
equation
  v = Window(s);
end Probe;
"#,
        &["v[1]", "v[2]", "v[3]", "v[4]"],
    );
    assert_eq!(values, [1.0, 2.0, 3.0, 30.0]);
}

#[test]
fn a_loop_local_piecewise_start_updates_the_selected_blocks() {
    // node 1: offset 0, stateOffset 0, rows 1:3 columns 1:3 hold s*identity.
    // node 2: offset 3, stateOffset 15, rows 4:6 columns 16:18 hold 2s*identity.
    // With s = 1: H[2,2] = 1, H[5,17] = 2, H[2,17] = 0, H[5,2] = 0.
    let values = evaluate(
        r#"
function Blocks
  input Real s;
  output Real H[6, 21];
protected
  Integer offset;
  Integer stateOffset;
algorithm
  H := zeros(6, 21);
  for node in 1:2 loop
    offset := 3 * (node - 1);
    stateOffset := if node == 1 then 0 else 15;
    H[offset + 1:offset + 3, stateOffset + 1:stateOffset + 3] := s * node * identity(3);
  end for;
end Blocks;
model Probe
  input Real s = 1;
  output Real H[6, 21];
equation
  H = Blocks(s);
end Probe;
"#,
        &["H[2,2]", "H[5,17]", "H[2,17]", "H[5,2]"],
    );
    assert_eq!(values, [1.0, 2.0, 0.0, 0.0]);
}

#[test]
fn a_row_window_and_a_column_window_update_a_matrix() {
    // Row 2 columns 2:3 take {s, 2s}; rows 2:3 of column 4 take {3s, 4s}.
    let values = evaluate(
        r#"
function Windows
  input Real s;
  input Integer k;
  output Real A[3, 4];
algorithm
  A := zeros(3, 4);
  A[k, k:k + 1] := {s, 2 * s};
  A[k:k + 1, 4] := {3 * s, 4 * s};
end Windows;
model Probe
  input Real s = 1;
  input Integer k = 2;
  output Real A[3, 4];
equation
  A = Windows(s, k);
end Probe;
"#,
        &["A[2,2]", "A[2,3]", "A[2,4]", "A[3,4]", "A[1,1]"],
    );
    assert_eq!(values, [1.0, 2.0, 3.0, 4.0, 0.0]);
}

#[test]
fn a_loop_with_a_run_time_count_writes_one_window_per_iteration() {
    // The second node is active (second = true), so node 1 writes rows 1:3 of
    // the innovation and node 2 rows 7:9 (6 * (node - 1) + 1:3); each takes
    // node * s.
    let values = evaluate(
        r#"
function Innovations
  input Real s;
  input Boolean second;
  output Real innovation[12];
  output Real H[12, 21];
protected
  Integer count;
  Integer offset;
  Integer stateOffset;
algorithm
  count := if second then 2 else 1;
  innovation := zeros(12);
  H := zeros(12, 21);
  for node in 1:count loop
    offset := 6 * (node - 1);
    stateOffset := if node == 1 then 0 else 15;
    innovation[offset + 1:offset + 3] := {s, 2 * s, 3 * s} * node;
    H[offset + 1:offset + 3, stateOffset + 1:stateOffset + 3] := s * node * identity(3);
  end for;
end Innovations;
model Probe
  input Real s = 1;
  input Boolean second = true;
  output Real innovation[12];
  output Real H[12, 21];
equation
  (innovation, H) = Innovations(s, second);
end Probe;
"#,
        &[
            "innovation[1]",
            "innovation[3]",
            "innovation[7]",
            "innovation[9]",
            "innovation[10]",
            "H[2,2]",
            "H[8,17]",
            "H[8,2]",
        ],
    );
    assert_eq!(values, [1.0, 3.0, 2.0, 6.0, 0.0, 1.0, 2.0, 0.0]);
}

#[test]
fn a_window_read_from_a_loop_binder_gathers_its_elements() {
    // Window i reads v[i:i + 2] of {1, 2, 3, 4, 5}: sums 6, 9, 12 weighted by
    // 1, 10, 100 give 6 + 90 + 1200 = 1296.
    let values = evaluate(
        r#"
function Reads
  input Real s;
  output Real y;
protected
  Real v[5];
algorithm
  v := {1, 2, 3, 4, 5} * s;
  y := 0;
  for i in 1:3 loop
    y := y + 10 ^ (i - 1) * sum(v[i:i + 2]);
  end for;
end Reads;
model Probe
  input Real s = 1;
  output Real y;
equation
  y = Reads(s);
end Probe;
"#,
        &["y"],
    );
    assert_eq!(values, [1296.0]);
}

#[test]
fn a_two_dimensional_window_read_gathers_its_block() {
    // A = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}; block (r, c) is A[r:r + 1, c:c + 1]
    // with sums 12, 16, 24, 28 weighted by 1, 10, 100, 1000: 30572.
    let values = evaluate(
        r#"
function Blocks
  input Real s;
  output Real y;
protected
  Real A[3, 3];
algorithm
  A := {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}} * s;
  y := 0;
  for r in 1:2 loop
    for c in 1:2 loop
      y := y + 10 ^ ((r - 1) * 2 + (c - 1)) * sum(A[r:r + 1, c:c + 1]);
    end for;
  end for;
end Blocks;
model Probe
  input Real s = 1;
  output Real y;
equation
  y = Blocks(s);
end Probe;
"#,
        &["y"],
    );
    assert_eq!(values, [30572.0]);
}

#[test]
fn a_slice_whose_extent_is_not_constant_stays_refused() {
    let error = Compiler::new()
        .model("Probe")
        .compile_str(
            r#"
function Varying
  input Integer n;
  output Real v[4];
algorithm
  v := zeros(4);
  v[1:n] := ones(n);
end Varying;
model Probe
  input Integer n = 2;
  output Real v[4];
equation
  v = Varying(n);
end Probe;
"#,
            "Probe.mo",
        )
        .map(|_| ())
        .expect_err("a run-time extent has no fixed shape");
    assert!(error.to_string().contains("range"), "{error}");
}

#[test]
fn a_window_written_from_selected_rows_and_read_back_in_the_same_iteration() {
    // Node 1 stores graph row 1 minus `prev` in rows 1:3 and node 2 stores graph
    // row 2 minus `ref` in rows 7:9; both differences are {1, 0, -1}, and
    // every window passes the validity bound read back from the same slice.
    let values = evaluate(
        r#"
function Correct
  input Real prev[3];
  input Real ref[3];
  input Boolean useRef;
  input Real graphPos[2, 3];
  output Real innovation[12];
  output Real valid;
protected
  Integer count;
  Integer offset;
  Boolean ok;
algorithm
  innovation := zeros(12);
  ok := true;
  count := if useRef then 2 else 1;
  for node in 1:count loop
    offset := 6 * (node - 1);
    innovation[offset + 1:offset + 3] := graphPos[node, :] - (if node == 1 then prev else ref);
    ok := ok and sum(innovation[offset + 1:offset + 3] .^ 2) <= 100.0;
  end for;
  valid := if ok then 1 else 0;
end Correct;
model Probe
  input Real prev[3] = {1, 2, 3};
  input Real ref[3] = {0, 1, 2};
  input Boolean useRef = true;
  input Real graphPos[2, 3] = {{2, 2, 2}, {1, 1, 1}};
  output Real innovation[12];
  output Real valid;
equation
  (innovation, valid) = Correct(prev, ref, useRef, graphPos);
end Probe;
"#,
        &[
            "innovation[1]",
            "innovation[2]",
            "innovation[3]",
            "innovation[4]",
            "innovation[7]",
            "innovation[8]",
            "innovation[9]",
            "valid",
        ],
    );
    assert_eq!(values, [1.0, 0.0, -1.0, 0.0, 1.0, 0.0, -1.0, 1.0]);
}
