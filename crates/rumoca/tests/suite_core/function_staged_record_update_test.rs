//! Regression for a record field updated inside a conditional while the
//! enclosing function is still assembling the record.

use rumoca::{Compiler, CompilerError};
use rumoca_compile::compile::FailedPhase;
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
  if result.valid and u < 0.0 then
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

const UNDERSCORE_SIBLING_RECORD_FIELDS: &str = r#"
within;
record CollisionResult
  Real a;
  Real a_b;
end CollisionResult;

function buildCollisionResult
  output CollisionResult result;
algorithm
  result.a := 1.0;
  result.a_b := 2.0;
end buildCollisionResult;

model ObserveUnderscoreSiblingRecordFields
  CollisionResult result;
equation
  result = buildCollisionResult();
end ObserveUnderscoreSiblingRecordFields;
"#;

const NESTED_PARTIAL_RECORD_WRITE: &str = r#"
within;
record InnerResult
  Real p;
end InnerResult;

record OuterResult
  InnerResult nested_value;
end OuterResult;

function buildNestedPartial
  output OuterResult result;
algorithm
  result.nested_value.p := 1.0;
end buildNestedPartial;

model ObserveNestedPartialRecordWrite
  OuterResult result;
equation
  result = buildNestedPartial();
end ObserveNestedPartialRecordWrite;
"#;

const FUTURE_STAGING_READ: &str = r#"
within;
record FutureResult
  Real first;
  Real second;
end FutureResult;

function readFuture
  output Real observed;
protected
  FutureResult result;
algorithm
  observed := result.first;
  result.first := 1.0;
  result.second := 2.0;
end readFuture;

model ObserveFutureStagingRead
  Real observed;
equation
  observed = readFuture();
end ObserveFutureStagingRead;
"#;

const BRANCH_WHOLE_RECORD_THEN_PROJECTION: &str = r#"
within;
record BranchResult
  Real first;
  Real second;
end BranchResult;

function branchWholeRead
  input Boolean chooseWhole;
  output Real observed;
protected
  BranchResult result;
algorithm
  result := BranchResult(1.0, 2.0);
  if chooseWhole then
    result := BranchResult(10.0, 20.0);
    observed := result.first;
  else
    observed := result.first;
  end if;
  result.first := 30.0;
  observed := observed + 0.0;
  result.second := 40.0;
end branchWholeRead;

model ObserveBranchWholeRecordThenProjection
  Real whole;
  Real staged;
equation
  whole = branchWholeRead(true);
  staged = branchWholeRead(false);
end ObserveBranchWholeRecordThenProjection;
"#;

const MIXED_STAGED_AND_WHOLE_RECORD_JOIN: &str = r#"
within;
record MixedResult
  Real first;
  Real second;
end MixedResult;

function mixedWholeAndStagedRead
  input Boolean chooseWhole;
  output Real observed;
protected
  MixedResult result;
algorithm
  result.first := 1.0;
  if chooseWhole then
    result := MixedResult(10.0, 20.0);
    observed := result.first;
  else
    observed := result.first;
  end if;
  result.second := 2.0;
end mixedWholeAndStagedRead;

model ObserveMixedStagedAndWholeRecordJoin
  Real observed;
equation
  observed = mixedWholeAndStagedRead(true);
end ObserveMixedStagedAndWholeRecordJoin;
"#;

const WHOLE_RECORD_REPLACEMENT: &str = r#"
within;
record ReplacementResult
  Real first;
  Real second;
end ReplacementResult;

function replaceWhole
  output Real observed;
protected
  ReplacementResult result;
algorithm
  result.first := 1.0;
  result.second := 2.0;
  result := ReplacementResult(10.0, 20.0);
  observed := result.first;
end replaceWhole;

model ObserveWholeRecordReplacement
  Real observed;
equation
  observed = replaceWhole();
end ObserveWholeRecordReplacement;
"#;

const EXHAUSTIVE_WHOLE_RECORD_JOIN: &str = r#"
within;
record JoinedResult
  Real first;
  Real second;
end JoinedResult;

function exhaustiveWholeJoin
  input Boolean chooseFirst;
  output Real observed;
protected
  JoinedResult result;
algorithm
  result.first := 1.0;
  if chooseFirst then
    result := JoinedResult(10.0, 20.0);
  else
    result := JoinedResult(30.0, 40.0);
  end if;
  observed := result.first;
  result.second := 2.0;
end exhaustiveWholeJoin;

model ObserveExhaustiveWholeRecordJoin
  Real first;
  Real second;
equation
  first = exhaustiveWholeJoin(true);
  second = exhaustiveWholeJoin(false);
end ObserveExhaustiveWholeRecordJoin;
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
    let probe = eval_dae_at(compiled.dae(), &SimOptions::default(), &[], 0.0)
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

#[test]
fn underscore_in_a_sibling_field_name_does_not_create_a_nested_path() {
    let compiled = Compiler::new()
        .model("ObserveUnderscoreSiblingRecordFields")
        .compile_str(
            UNDERSCORE_SIBLING_RECORD_FIELDS,
            "ObserveUnderscoreSiblingRecordFields.mo",
        )
        .expect("resolved sibling field identities must remain distinct");
    let probe = eval_dae_at(compiled.dae(), &SimOptions::default(), &[], 0.0)
        .expect("the collision regression should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    assert_eq!(value(&probe.report, "result.a"), 1.0);
    assert_eq!(value(&probe.report, "result.a_b"), 2.0);
}

#[test]
fn nested_partial_record_write_is_refused_by_the_typed_dae_boundary() {
    let error = Compiler::new()
        .model("ObserveNestedPartialRecordWrite")
        .compile_str(
            NESTED_PARTIAL_RECORD_WRITE,
            "ObserveNestedPartialRecordWrite.mo",
        )
        .expect_err("a three-part mutable path has no complete staged DAE owner yet");
    let rendered = error.to_string();
    assert!(
        rendered.contains("failed in ToDae") && rendered.contains("one exact record field"),
        "unexpected diagnostic: {error}"
    );
}

#[test]
fn future_staging_storage_is_not_a_reaching_definition() {
    let error = Compiler::new()
        .model("ObserveFutureStagingRead")
        .compile_str(FUTURE_STAGING_READ, "ObserveFutureStagingRead.mo")
        .expect_err("a future staging local cannot authorize an earlier field read");
    let expected_start = FUTURE_STAGING_READ
        .find("observed := result.first")
        .expect("fixture retains the premature read")
        + "observed := ".len();
    match error {
        CompilerError::CompileDiagnosticsError { failures, .. } => {
            let [failure] = failures.as_slice() else {
                panic!("expected one exact future-read failure, got {failures:?}")
            };
            assert_eq!(failure.phase, Some(FailedPhase::ToDae));
            assert_eq!(failure.error_code.as_deref(), Some("ED019"));
            assert!(failure.error.contains("reaching definition"), "{failure:?}");
            assert_eq!(
                failure
                    .primary_label
                    .as_ref()
                    .expect("ED019 retains the offending field span")
                    .span
                    .start
                    .0,
                expected_start
            );
        }
        other => panic!("expected structured ToDae refusal, got {other:?}"),
    }
}

#[test]
fn branch_local_whole_record_shadows_prior_staging() {
    let compiled = Compiler::new()
        .model("ObserveBranchWholeRecordThenProjection")
        .compile_str(
            BRANCH_WHOLE_RECORD_THEN_PROJECTION,
            "ObserveBranchWholeRecordThenProjection.mo",
        )
        .expect("a branch-local whole record owns subsequent projections in that branch");
    let probe = eval_dae_at(compiled.dae(), &SimOptions::default(), &[], 0.0)
        .expect("the branch-local replacement regression should evaluate");
    assert_eq!(value(&probe.report, "whole"), 10.0);
    assert_eq!(value(&probe.report, "staged"), 1.0);
}

#[test]
fn mixed_staged_and_whole_record_join_is_typed_refused() {
    let error = Compiler::new()
        .model("ObserveMixedStagedAndWholeRecordJoin")
        .compile_str(
            MIXED_STAGED_AND_WHOLE_RECORD_JOIN,
            "ObserveMixedStagedAndWholeRecordJoin.mo",
        )
        .expect_err("a divergent staged/whole join has no typed field phi yet");
    let expected_start = MIXED_STAGED_AND_WHOLE_RECORD_JOIN
        .find("if chooseWhole then")
        .expect("fixture retains the divergent conditional");
    match error {
        CompilerError::CompileDiagnosticsError { failures, .. } => {
            let [failure] = failures.as_slice() else {
                panic!("expected one exact mixed-join failure, got {failures:?}")
            };
            assert_eq!(failure.phase, Some(FailedPhase::ToDae));
            assert_eq!(failure.error_code.as_deref(), Some("ED019"));
            assert!(
                failure
                    .error
                    .contains("mixes exact staged-field and whole-record reaching definitions"),
                "{failure:?}"
            );
            assert_eq!(
                failure
                    .primary_label
                    .as_ref()
                    .expect("ED019 retains the conditional span")
                    .span
                    .start
                    .0,
                expected_start
            );
        }
        other => panic!("expected structured ToDae refusal, got {other:?}"),
    }
}

#[test]
fn later_whole_record_replacement_invalidates_staging() {
    let compiled = Compiler::new()
        .model("ObserveWholeRecordReplacement")
        .compile_str(WHOLE_RECORD_REPLACEMENT, "ObserveWholeRecordReplacement.mo")
        .expect("a whole replacement invalidates every earlier staged field");
    let probe = eval_dae_at(compiled.dae(), &SimOptions::default(), &[], 0.0)
        .expect("the whole replacement regression should evaluate");
    assert_eq!(value(&probe.report, "observed"), 10.0);
}

#[test]
fn exhaustive_whole_record_join_invalidates_staging() {
    let compiled = Compiler::new()
        .model("ObserveExhaustiveWholeRecordJoin")
        .compile_str(
            EXHAUSTIVE_WHOLE_RECORD_JOIN,
            "ObserveExhaustiveWholeRecordJoin.mo",
        )
        .expect("an exhaustive whole-record join owns later projections");
    let probe = eval_dae_at(compiled.dae(), &SimOptions::default(), &[], 0.0)
        .expect("the exhaustive replacement regression should evaluate");
    assert_eq!(value(&probe.report, "first"), 10.0);
    assert_eq!(value(&probe.report, "second"), 30.0);
}
