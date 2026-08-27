//! Regression for a record field updated inside a conditional while the
//! enclosing function is still assembling the record.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const STAGED_RECORD_UPDATE: &str = r#"
within;
record Result
  Real first;
  Real second;
  Boolean valid;
end Result;

function build
  input Real u;
  output Result result;
algorithm
  result.first := u;
  result.valid := true;
  if u < 0.0 then
    result.valid := false;
  end if;
  result.second := if result.valid then 2.0 * u else -2.0 * u;
end build;

function observe
  input Real u;
  output Real values[3];
protected
  Result result;
algorithm
  result := build(u);
  values := {
    result.first,
    result.second,
    if result.valid then 1.0 else 0.0};
end observe;

model ObserveStagedRecordUpdate
  Real positive[3];
  Real negative[3];
equation
  positive = observe(3.0);
  negative = observe(-4.0);
end ObserveStagedRecordUpdate;
"#;

const UNDEFINED_RECORD_UPDATE: &str = r#"
within;
record Result
  Real value;
  Boolean valid;
end Result;

function incomplete
  input Real u;
  output Result result;
algorithm
  if u < 0.0 then
    result.valid := false;
  end if;
end incomplete;

model ObserveUndefinedRecordUpdate
  Result result;
equation
  result = incomplete(-1.0);
end ObserveUndefinedRecordUpdate;
"#;

fn value(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == name)
        .unwrap_or_else(|| panic!("missing solver value {name}"))
        .value
}

#[test]
fn conditional_update_keeps_the_staged_record_field() {
    let compiled = Compiler::new()
        .model("ObserveStagedRecordUpdate")
        .compile_str(STAGED_RECORD_UPDATE, "ObserveStagedRecordUpdate.mo")
        .expect("an existing staged field should remain writable in a nested branch");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the checked staged-record DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    assert_eq!(value(&probe.report, "positive[1]"), 3.0);
    assert_eq!(value(&probe.report, "positive[2]"), 6.0);
    assert_eq!(value(&probe.report, "positive[3]"), 1.0);
    assert_eq!(value(&probe.report, "negative[1]"), -4.0);
    assert_eq!(value(&probe.report, "negative[2]"), 8.0);
    assert_eq!(value(&probe.report, "negative[3]"), 0.0);
}

#[test]
fn conditional_update_does_not_invent_an_unwritten_record() {
    let error = Compiler::new()
        .model("ObserveUndefinedRecordUpdate")
        .compile_str(UNDEFINED_RECORD_UPDATE, "ObserveUndefinedRecordUpdate.mo")
        .expect_err("an unwritten record field has no checked value to preserve");
    assert!(
        error.to_string().contains("record output assembly")
            && error.to_string().contains("result.value"),
        "unexpected diagnostic: {error}"
    );
}
