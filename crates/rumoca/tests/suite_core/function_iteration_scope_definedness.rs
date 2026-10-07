//! MLS 3.7 §11.2.2 / §12.4.4: element definitions made earlier in the same
//! iteration of an enclosing loop are visible to a nested loop, a loop
//! binder shadows a local of the same name, and a value written under one
//! captured selection stays defined under an equivalent selection. Reads of
//! elements no earlier statement wrote stay refused. Expected values are
//! computed by hand.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODELS: &str = r#"
function rowThenRead
  input Real A[3,3];
  output Real total;
protected
  Real n[3,3];
algorithm
  total := 0;
  for i in 1:3 loop
    for j in 1:3 loop
      n[i,j] := A[i,j];
    end for;
    for j in 1:2 loop
      total := total + n[i,j] + j;
    end for;
  end for;
end rowThenRead;

model RowThenRead
  input Real A[3,3] = {{1,2,3},{4,5,6},{7,8,9}};
  output Real total;
equation
  total = rowThenRead(A);
end RowThenRead;

function columnSolve
  input Real B[2,3];
  output Real total;
protected
  Real s[2,3];
algorithm
  total := 0;
  for c in 1:3 loop
    s[2,c] := B[2,c];
    s[1,c] := B[1,c] + s[2,c];
    for i in 1:2 loop
      total := total + s[i,c] + i;
    end for;
  end for;
end columnSolve;

model ColumnSolve
  input Real B[2,3] = {{1,2,3},{4,5,6}};
  output Real total;
equation
  total = columnSolve(B);
end ColumnSolve;

function shadowedLocal
  input Real x[:];
  output Real s;
protected
  Integer i;
  Integer count;
  Integer idx[size(x, 1)];
algorithm
  s := 0;
  count := 0;
  idx := fill(0, size(x, 1));
  for k in 1:size(x, 1) loop
    if x[k] > 0 then
      count := count + 1;
      idx[count] := k;
    end if;
  end for;
  for slot in 1:count loop
    i := idx[slot];
    s := s + x[i];
  end for;
  for i in 1:size(x, 1) loop
    s := s + i;
  end for;
end shadowedLocal;

model ShadowedLocal
  input Real x[3] = {1, -2, 3};
  output Real s;
equation
  s = shadowedLocal(x);
end ShadowedLocal;

function selectedLoop
  input Real c;
  output Real s;
protected
  Integer a[2];
algorithm
  s := 0;
  if c > 0.5 then
    s := 1;
  elseif c > 0.25 then
    for h in 1:3 loop
      a[1] := h;
      a[2] := 2*h;
      if a[2] >= a[1] then
        a[2] := a[2] + 1;
      end if;
      s := s + a[1] + a[2];
    end for;
  end if;
end selectedLoop;

model SelectedLoop
  input Real c = 0.3;
  output Real s;
equation
  s = selectedLoop(c);
end SelectedLoop;

function partialRow
  input Real A[3,3];
  output Real total;
protected
  Real n[3,3];
algorithm
  total := 0;
  for i in 1:3 loop
    for j in 1:1 loop
      n[i,j] := A[i,j];
    end for;
    for j in 1:2 loop
      total := total + n[i,j];
    end for;
  end for;
end partialRow;

model PartialRow
  input Real A[3,3] = {{1,2,3},{4,5,6},{7,8,9}};
  output Real total;
equation
  total = partialRow(A);
end PartialRow;

function readBeforeWrite
  input Real A[3,3];
  output Real total;
protected
  Real n[3,3];
algorithm
  total := 0;
  for i in 1:3 loop
    for j in 1:2 loop
      total := total + n[i,j];
    end for;
    for j in 1:3 loop
      n[i,j] := A[i,j];
    end for;
  end for;
end readBeforeWrite;

model ReadBeforeWrite
  input Real A[3,3] = {{1,2,3},{4,5,6},{7,8,9}};
  output Real total;
equation
  total = readBeforeWrite(A);
end ReadBeforeWrite;
"#;

fn value(model: &str, name: &str) -> f64 {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "IterationScopeDefinedness.mo")
        .unwrap_or_else(|error| panic!("{model} should compile: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("{model} should evaluate: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == name)
        .unwrap_or_else(|| panic!("{model} has no {name}"))
        .value
}

fn refusal(model: &str) -> String {
    Compiler::new()
        .model(model)
        .compile_str(MODELS, "IterationScopeDefinedness.mo")
        .map(|_| ())
        .expect_err("the read names elements no earlier statement wrote")
        .to_string()
}

#[test]
fn a_nested_loop_reads_what_its_enclosing_iteration_wrote() {
    // Columns 1 and 2 of A (27) plus 1 + 2 per row.
    assert_eq!(value("RowThenRead", "total"), 36.0);
    // Rows {5, 7, 9} + 1 and {4, 5, 6} + 2.
    assert_eq!(value("ColumnSolve", "total"), 45.0);
}

#[test]
fn a_loop_binder_shadows_a_local_of_its_name() {
    // Positive elements 1 + 3, then 1 + 2 + 3.
    assert_eq!(value("ShadowedLocal", "s"), 10.0);
}

#[test]
fn a_selection_captured_into_a_loop_keeps_its_element_writes() {
    // (h + 2h + 1) for h = 1, 2, 3.
    assert_eq!(value("SelectedLoop", "s"), 21.0);
}

#[test]
fn a_read_beyond_or_before_the_enclosing_writes_is_refused() {
    for model in ["PartialRow", "ReadBeforeWrite"] {
        let error = refusal(model);
        assert!(
            error.contains("that do not all have a definition"),
            "{error}"
        );
    }
}
