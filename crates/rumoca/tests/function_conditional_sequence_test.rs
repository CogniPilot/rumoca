//! Regression: MLS §11 sequential algorithm semantics inside a function
//! conditional branch, and MLS §12.4.4 element-wise definition of a function
//! value that has no declared binding.
//!
//! Before the fix `ToDae` rejected any branch that assigned the same value more
//! than once (ED019 `function conditional`), rejected a branch that read a value
//! an earlier branch statement had assigned, and had no checked owner at all for
//! `y[i] := ...` when `y` had no prior definition (ED020 "missing function value
//! definition"). All three shapes are ordinary Modelica; the LieGroups example
//! library in the coverage corpus needs every one of them.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

/// `log_map`-shaped body: element writes outside a conditional, then branches
/// that write more elements of the same value, reassign one of them, and read a
/// branch-local value the same branch just defined.
const SEQUENCE_MODEL: &str = r#"
within;
function pick
  input Real u;
  output Real y[4];
protected
  Real w[2];
  constant Real eps = 0.5;
algorithm
  y[3] := 30 * u;
  y[4] := 40 * u;
  if u < eps then
    y[1] := u;
    y[2] := 2 * u;
    y[1] := y[1] + 100;
  else
    w := {u, 2 * u};
    y[1] := w[1] + 1000;
    y[2] := w[2] + 1000;
  end if;
end pick;
model CondSequence
  Real z[4];
  Real lo[4];
equation
  z = pick(time + 0.8);
  lo = pick(time + 0.2);
end CondSequence;
"#;

/// A whole 2-D value defined only by constant element writes, exactly like the
/// `adjoint`/`to_Matrix` functions of the coverage example library.
const MATRIX_MODEL: &str = r#"
within;
function build
  input Real u;
  output Real m[2, 2];
algorithm
  m[1, 1] := u;
  m[1, 2] := 2 * u;
  m[2, 1] := 3 * u;
  m[2, 2] := 4 * u;
end build;
model MatrixElements
  Real a[2, 2];
equation
  a = build(time + 1.0);
end MatrixElements;
"#;

/// A slice write covers every element it names, so `y[1:2]` plus `y[3:4]` is a
/// total definition of `y[4]`.
const SLICE_MODEL: &str = r#"
within;
function halves
  input Real u;
  output Real y[4];
algorithm
  y[1:2] := {u, 2 * u};
  y[3:4] := {3 * u, 4 * u};
end halves;
model SliceElements
  Real s[4];
equation
  s = halves(time + 1.0);
end SliceElements;
"#;

/// A runtime branch whose nested conditional reads a value established by the
/// enclosing branch.  Both nested paths define `y`, so the inner join is a
/// total branch-local value and the outer join can own it without scalarizing
/// the control flow.
const NESTED_CONDITIONAL_MODEL: &str = r#"
within;
function nestedPick
  input Real u;
  output Real y;
protected
  Real x;
algorithm
  if u > 0 then
    x := u + 1;
    if x > 2 then
      y := 10 * x;
    else
      y := 20 * x;
    end if;
  else
    y := -u;
  end if;
end nestedPick;
model NestedConditional
  Real high;
  Real low;
  Real negative;
equation
  high = nestedPick(2);
  low = nestedPick(0.5);
  negative = nestedPick(-3);
end NestedConditional;
"#;

/// `axesRotationsAngles` shape: one exact element is defined on every runtime
/// branch and then read, while the remaining local aggregate stays undefined.
const BRANCH_ELEMENT_MODEL: &str = r#"
within;
function branchElement
  input Real u;
  output Real y;
protected
  Real angles[3];
algorithm
  if u >= 0 then
    angles[1] := u + 1;
  else
    angles[1] := 1 - u;
  end if;
  y := angles[1];
end branchElement;
model BranchElement
  Real positive;
  Real negative;
equation
  positive = branchElement(2);
  negative = branchElement(-3);
end BranchElement;
"#;

fn algebraic(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == name)
        .unwrap_or_else(|| {
            panic!(
                "missing solver value {name}; have: {:?}",
                report
                    .solver_y
                    .iter()
                    .map(|slot| slot.name.clone())
                    .collect::<Vec<_>>()
            )
        })
        .value
}

fn evaluate(source: &str, model: &str, file: &str) -> rumoca_sim::EvalAtReport {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, file)
        .expect("model should compile to a checked DAE");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("checked DAE should evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    probe.report
}

#[test]
fn conditional_branch_keeps_assignment_order() {
    let report = evaluate(SEQUENCE_MODEL, "CondSequence", "CondSequence.mo");
    // u = 0.8 takes the else branch: the branch-local `w` feeds both writes.
    assert_eq!(algebraic(&report, "z[1]"), 1000.8);
    assert_eq!(algebraic(&report, "z[2]"), 1001.6);
    // u = 0.2 takes the then branch, where the second write to y[1] wins and
    // reads what the first write left there.
    assert_eq!(algebraic(&report, "lo[1]"), 100.2);
    assert_eq!(algebraic(&report, "lo[2]"), 0.4);
}

#[test]
fn element_writes_outside_the_conditional_survive_the_join() {
    let report = evaluate(SEQUENCE_MODEL, "CondSequence", "CondSequence.mo");
    // y[3] and y[4] are written before the conditional and no branch touches
    // them, so the join must keep their definitions on every path.
    assert_eq!(algebraic(&report, "z[3]"), 24.0);
    assert_eq!(algebraic(&report, "z[4]"), 32.0);
    assert_eq!(algebraic(&report, "lo[3]"), 6.0);
    assert_eq!(algebraic(&report, "lo[4]"), 8.0);
}

#[test]
fn constant_element_writes_define_a_whole_matrix() {
    let report = evaluate(MATRIX_MODEL, "MatrixElements", "MatrixElements.mo");
    assert_eq!(algebraic(&report, "a[1,1]"), 1.0);
    assert_eq!(algebraic(&report, "a[1,2]"), 2.0);
    assert_eq!(algebraic(&report, "a[2,1]"), 3.0);
    assert_eq!(algebraic(&report, "a[2,2]"), 4.0);
}

#[test]
fn slice_writes_define_a_whole_vector() {
    let report = evaluate(SLICE_MODEL, "SliceElements", "SliceElements.mo");
    assert_eq!(algebraic(&report, "s[1]"), 1.0);
    assert_eq!(algebraic(&report, "s[2]"), 2.0);
    assert_eq!(algebraic(&report, "s[3]"), 3.0);
    assert_eq!(algebraic(&report, "s[4]"), 4.0);
}

#[test]
fn nested_conditional_joins_branch_local_definitions() {
    let report = evaluate(
        NESTED_CONDITIONAL_MODEL,
        "NestedConditional",
        "NestedConditional.mo",
    );
    assert_eq!(algebraic(&report, "high"), 30.0);
    assert_eq!(algebraic(&report, "low"), 30.0);
    assert_eq!(algebraic(&report, "negative"), 3.0);
}

#[test]
fn exact_element_defined_on_every_branch_is_readable() {
    let report = evaluate(BRANCH_ELEMENT_MODEL, "BranchElement", "BranchElement.mo");
    assert_eq!(algebraic(&report, "positive"), 3.0);
    assert_eq!(algebraic(&report, "negative"), 4.0);
}

#[test]
fn partial_element_coverage_is_rejected() {
    let source = SLICE_MODEL.replace("  y[3:4] := {3 * u, 4 * u};\n", "");
    let error = Compiler::new()
        .model("SliceElements")
        .compile_str(&source, "PartialElements.mo")
        .expect_err("a function output missing element definitions has no checked DAE owner");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("without defining every declared element"),
        "unexpected diagnostic: {rendered}"
    );
}

#[test]
fn defined_elements_of_a_partial_aggregate_are_readable() {
    let source = MATRIX_MODEL.replace("  m[2, 2] := 4 * u;\n", "  m[2, 2] := m[1, 1] + m[2, 1];\n");
    let report = evaluate(&source, "MatrixElements", "PartialRead.mo");
    assert_eq!(algebraic(&report, "a[1,1]"), 1.0);
    assert_eq!(algebraic(&report, "a[1,2]"), 2.0);
    assert_eq!(algebraic(&report, "a[2,1]"), 3.0);
    assert_eq!(algebraic(&report, "a[2,2]"), 4.0);
}

#[test]
fn reading_an_undefined_element_of_a_partial_aggregate_is_rejected() {
    let source = MATRIX_MODEL.replace("  m[2, 2] := 4 * u;\n", "  m[2, 2] := m[1, 1] + m[2, 2];\n");
    let error = Compiler::new()
        .model("MatrixElements")
        .compile_str(&source, "PartialRead.mo")
        .expect_err("reading an undefined element has no checked DAE owner");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("elements of `m` that do not all have a definition"),
        "unexpected diagnostic: {rendered}"
    );
}

#[test]
fn reading_a_whole_partial_aggregate_is_rejected() {
    let source = MATRIX_MODEL.replace("  m[2, 2] := 4 * u;\n", "  m := m;\n");
    let error = Compiler::new()
        .model("MatrixElements")
        .compile_str(&source, "PartialWholeRead.mo")
        .expect_err("reading a partial aggregate has no checked DAE owner");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("elements of `m` that do not all have a definition"),
        "unexpected diagnostic: {rendered}"
    );
}

#[test]
fn a_value_only_one_branch_defines_may_not_escape_the_conditional() {
    let source = r#"
within;
function leak
  input Real u;
  output Real y;
protected
  Real w;
algorithm
  if u < 0.5 then
    w := u;
  end if;
  y := w;
end leak;
model LeakedBranchValue
  Real z;
equation
  z = leak(time);
end LeakedBranchValue;
"#;
    let error = Compiler::new()
        .model("LeakedBranchValue")
        .compile_str(source, "LeakedBranchValue.mo")
        .expect_err("a value only one branch defines has no owner past the conditional");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("only some branches of the conditional"),
        "unexpected diagnostic: {rendered}"
    );
}

#[test]
fn an_output_missing_from_one_branch_is_rejected() {
    let source = r#"
within;
function branch_output
  input Real u;
  output Real y;
algorithm
  if u < 0.5 then
    y := u;
  else
    y := 2 * u;
  end if;
end branch_output;
model PartialOutput
  Real z;
equation
  z = branch_output(time);
end PartialOutput;
"#;
    let missing_else = source.replace("  else\n    y := 2 * u;\n", "");
    let error = Compiler::new()
        .model("PartialOutput")
        .compile_str(&missing_else, "PartialOutput.mo")
        .expect_err("an output no path defines has no checked DAE owner");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("without a definition on some branch")
            || rendered.contains("without defining every declared element"),
        "unexpected diagnostic: {rendered}"
    );

    // The complete conditional is the accepted shape.
    let report = evaluate(source, "PartialOutput", "PartialOutput.mo");
    assert_eq!(algebraic(&report, "z"), 0.0);
}

#[test]
fn a_conditional_inside_a_loop_body_is_reported_not_panicked() {
    let source = r#"
within;
model LoopConditional
  function accumulate
    input Real u;
    output Real y;
  protected
    Real acc;
  algorithm
    acc := 0;
    for i in 1:3 loop
      if u > 0 then
        acc := acc + u;
      end if;
    end for;
    y := acc;
  end accumulate;
  Real z;
equation
  z = accumulate(time);
end LoopConditional;
"#;
    let error = Compiler::new()
        .model("LoopConditional")
        .compile_str(source, "LoopConditional.mo")
        .expect_err("a compact loop transition owns no nested conditional");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("requires direct value assignments in a loop body"),
        "unexpected diagnostic: {rendered}"
    );
}
