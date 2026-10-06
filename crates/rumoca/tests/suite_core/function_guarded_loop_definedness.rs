//! MLS 3.6 §12.4.4: a function local is undefined until a statement assigns
//! it. A value that a loop assigns only under a runtime guard therefore has no
//! definition on the path where that guard is false. Reading it on that path
//! is refused rather than reading the compact transition's carried slot; a
//! read under the same guard is admitted and evaluates to the source value.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODELS: &str = r#"
function scalarAfterLoop
  input Boolean mask;
  output Real y;
protected
  Real s;
algorithm
  if mask then
    for j in 1:2 loop
      s := 2.0;
    end for;
  end if;
  y := s;
end scalarAfterLoop;

model ScalarAfterLoop
  input Boolean mask = false;
  output Real y;
equation
  y = scalarAfterLoop(mask);
end ScalarAfterLoop;

function scalarInOuterLoop
  input Boolean mask[3];
  output Real y[3];
protected
  Real s;
algorithm
  y := zeros(3);
  for i in 1:3 loop
    if mask[i] then
      for j in 1:2 loop
        s := 2.0;
      end for;
    end if;
    y[i] := s;
  end for;
end scalarInOuterLoop;

model ScalarInOuterLoop
  input Boolean mask[3] = {false, true, false};
  output Real y[3];
equation
  y = scalarInOuterLoop(mask);
end ScalarInOuterLoop;

function elementsAfterLoop
  input Boolean mask;
  output Real y;
protected
  Real a[2];
algorithm
  if mask then
    for j in 1:2 loop
      a[j] := j;
    end for;
  end if;
  y := a[1] + a[2];
end elementsAfterLoop;

model ElementsAfterLoop
  input Boolean mask = false;
  output Real y;
equation
  y = elementsAfterLoop(mask);
end ElementsAfterLoop;

record Pair
  Real first;
  Real second;
end Pair;

function fieldAfterLoop
  input Boolean mask;
  output Real y;
protected
  Pair p;
algorithm
  p.second := 0.0;
  if mask then
    for j in 1:2 loop
      p.first := j;
    end for;
  end if;
  y := p.first;
end fieldAfterLoop;

model FieldAfterLoop
  input Boolean mask = false;
  output Real y;
equation
  y = fieldAfterLoop(mask);
end FieldAfterLoop;

function guardedReadAfterLoop
  input Boolean mask;
  output Real y;
protected
  Real s;
algorithm
  y := -1.0;
  if mask then
    for j in 1:2 loop
      s := 2.0*j;
    end for;
  end if;
  if mask then
    y := s;
  end if;
end guardedReadAfterLoop;

model GuardedReadTrue
  input Boolean mask = true;
  output Real y;
equation
  y = guardedReadAfterLoop(mask);
end GuardedReadTrue;

model GuardedReadFalse
  input Boolean mask = false;
  output Real y;
equation
  y = guardedReadAfterLoop(mask);
end GuardedReadFalse;
"#;

fn refusal(model: &str) -> String {
    let error = Compiler::new()
        .model(model)
        .compile_str(MODELS, "GuardedLoopDefinedness.mo")
        .map(|_| ())
        .expect_err("a read on the unselected path has no definition");
    format!("{error:?}")
}

fn value(model: &str) -> f64 {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "GuardedLoopDefinedness.mo")
        .unwrap_or_else(|error| panic!("{model} should compile: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("{model} should evaluate: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "y")
        .expect("y is an algebraic result")
        .value
}

#[test]
fn a_scalar_assigned_only_under_a_false_guard_is_not_read_after_the_loop() {
    let message = refusal("ScalarAfterLoop");
    assert!(
        message.contains("reads `s`, which only some branches"),
        "the refusal names the undefined value and its conditional: {message}"
    );
}

#[test]
fn a_scalar_left_undefined_on_an_earlier_outer_iteration_is_refused() {
    let message = refusal("ScalarInOuterLoop");
    assert!(
        message.contains("reads `s`, which only some branches"),
        "{message}"
    );
}

#[test]
fn array_elements_assigned_only_under_a_false_guard_are_refused() {
    let message = refusal("ElementsAfterLoop");
    assert!(
        message.contains("reads `a`, which only some branches"),
        "{message}"
    );
}

#[test]
fn a_record_field_assigned_only_under_a_false_guard_is_refused() {
    let message = refusal("FieldAfterLoop");
    assert!(
        message.contains("which only some branches") && message.contains("first"),
        "{message}"
    );
}

#[test]
fn a_read_under_the_same_guard_keeps_the_loop_value() {
    assert_eq!(value("GuardedReadTrue"), 4.0);
    assert_eq!(value("GuardedReadFalse"), -1.0);
}
