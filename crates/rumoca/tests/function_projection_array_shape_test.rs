//! Regressions for array-shape preservation while projecting Modelica functions.

use rumoca::Compiler;
use rumoca_ir_dae::{DaeGeneration, DaeProvenanceOrigin, ExpressionKind};
use rumoca_sim::{EvalAtReport, SimOptions, eval_dae_at};

const NESTED_ARRAY_CALL_MODEL: &str = r#"
within;
function makeControlPoints
  input Real u;
  output Real controlPoint[2, 3];
algorithm
  controlPoint := [u, 2.0, 3.0; 4.0, 5.0, 6.0];
end makeControlPoints;

function evaluateGeneric
  input Real controlPoint[:, :];
  output Real value[size(controlPoint, 1)];
protected
  Real stage[size(controlPoint, 1), size(controlPoint, 2)];
algorithm
  stage := controlPoint;
  for pointIndex in 1:size(controlPoint, 2) - 1 loop
    stage[:, pointIndex] := stage[:, pointIndex] + stage[:, pointIndex + 1];
  end for;
  value := stage[:, 1];
end evaluateGeneric;

function evaluateWrapper
  input Real controlPoint[2, 3];
  output Real value[2];
algorithm
  value := evaluateGeneric(controlPoint);
end evaluateWrapper;

model NestedArrayCall
  Real x[2](each start = 0.0, each fixed = true);
equation
  der(x) = evaluateWrapper(makeControlPoints(time));
end NestedArrayCall;
"#;

const RECORD_RESULT_MODEL: &str = r#"
within;
record ProjectionResult
  Real matrix[3, 3];
  Real scalar;
end ProjectionResult;

function makeProjectionResult
  input Real u;
  output ProjectionResult result;
protected
  Real column[3];
algorithm
  column := {u + 1.0, u + 2.0, u + 3.0};
  result.matrix[:, 1] := column;
  result.matrix[:, 2] := {4.0, 5.0, 6.0};
  result.matrix[:, 3] := {7.0, 8.0, 9.0};
  result.scalar := u + 6.0;
end makeProjectionResult;

model ObserveRecordScalar
  ProjectionResult reference;
  output Real observed;
equation
  reference = makeProjectionResult(time);
  observed = reference.scalar;
end ObserveRecordScalar;
"#;

const INTERLEAVED_RECORD_RESULT_MODEL: &str = r#"
within;
record InterleavedResult
  Real matrix[3, 3];
  Real scalar;
end InterleavedResult;

function makeInterleavedResult
  input Real u;
  output InterleavedResult result;
protected
  Real column[3];
algorithm
  result.matrix[:, 1] := {u + 1.0, u + 2.0, u + 3.0};
  result.matrix[:, 2] := {4.0, 5.0, 6.0};
  result.matrix[:, 3] := {7.0, 8.0, 9.0};
  column := result.matrix[:, 1];
  result.scalar := column[1] + 5.0;
end makeInterleavedResult;

model ObserveInterleavedRecord
  InterleavedResult reference;
  output Real observed;
equation
  reference = makeInterleavedResult(time);
  observed = reference.scalar;
end ObserveInterleavedRecord;
"#;

const SHAPE_DERIVED_LOCAL_LOOP_MODEL: &str = r#"
within;
function sumRows
  input Real values[:, 1];
  output Real total;
protected
  Integer rowCount;
algorithm
  rowCount := size(values, 1);
  total := 0.0;
  for row in 1:rowCount loop
    total := total + values[row, 1];
  end for;
end sumRows;

model ObserveShapeDerivedLoop
  output Real observed;
equation
  observed = sumRows([1.0; 2.0; 3.0]);
end ObserveShapeDerivedLoop;
"#;

const LOOP_CONDITIONAL_CARRY_MODEL: &str = r#"
within;
function lastPositiveIndex
  input Real values[:];
  output Integer selected;
algorithm
  selected := 0;
  for index in 1:size(values, 1) loop
    if values[index] > 0.0 then
      selected := index;
    end if;
  end for;
end lastPositiveIndex;

model ObserveLoopConditionalCarry
  output Real observed;
equation
  observed = lastPositiveIndex({1.0, -2.0, 3.0, -4.0});
end ObserveLoopConditionalCarry;
"#;

const MULTI_OUTPUT_EQUATION_MODEL: &str = r#"
within;
function splitReference
  input Real u;
  output Real vector[2];
  output Real scalar;
algorithm
  vector := {u + 1.0, u + 2.0};
  scalar := u + 3.0;
end splitReference;

model ObserveMultiOutputEquation
  output Real vector[2];
  output Real scalar;
equation
  (vector, scalar) = splitReference(time);
end ObserveMultiOutputEquation;
"#;

const CONDITIONAL_ROW_DEFINITION_MODEL: &str = r#"
within;
function selectRows
  input Real values[:, 2];
  input Boolean enabled;
  output Real selected[size(values, 1), 2];
algorithm
  for row in 1:size(values, 1) loop
    if enabled then
      selected[row, :] := values[row, :];
    else
      selected[row, :] := zeros(2);
    end if;
  end for;
end selectRows;

model ObserveConditionalRows
  output Real selected[2, 2];
equation
  selected = selectRows([1.0, 2.0; 3.0, 4.0], time > 1.0);
end ObserveConditionalRows;
"#;

fn slot_value(report: &EvalAtReport, name: &str) -> f64 {
    report
        .solver_y
        .iter()
        .chain(report.derivatives.iter())
        .find(|slot| slot.name == name)
        .unwrap_or_else(|| {
            panic!(
                "missing `{name}`; have solver values {:?} and derivatives {:?}",
                report
                    .solver_y
                    .iter()
                    .map(|slot| slot.name.as_str())
                    .collect::<Vec<_>>(),
                report
                    .derivatives
                    .iter()
                    .map(|slot| slot.name.as_str())
                    .collect::<Vec<_>>()
            )
        })
        .value
}

#[test]
fn nested_call_preserves_function_produced_array_shape() {
    let compiled = Compiler::new()
        .model("NestedArrayCall")
        .compile_str(NESTED_ARRAY_CALL_MODEL, "NestedArrayCall.mo")
        .expect("nested dimension-generic array functions should compile");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("nested dimension-generic array functions should lower and evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    assert_eq!(slot_value(&probe.report, "der(x[1])"), 2.0);
    assert_eq!(slot_value(&probe.report, "der(x[2])"), 9.0);
}

#[test]
fn observed_scalar_field_lowers_record_function_matrix_slice_assignments() {
    let compiled = Compiler::new()
        .model("ObserveRecordScalar")
        .compile_str(RECORD_RESULT_MODEL, "ObserveRecordScalar.mo")
        .expect("record-valued function with matrix slice assignments should compile");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("observed record field should lower and evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    assert_eq!(slot_value(&probe.report, "observed"), 6.0);

    let (record_nodes, field_nodes, record_provenance) = compiled.dae.inspect(|view| {
        let mut record_nodes = 0;
        let mut field_nodes = 0;
        let mut record_provenance = None;
        for index in 0..view.expression_count() {
            let expression = view
                .expression(view.expression_id(index).expect("dense expression id"))
                .expect("dense expression resolves");
            match expression.kind() {
                ExpressionKind::Record => {
                    record_nodes += 1;
                    record_provenance = Some(expression.provenance());
                }
                ExpressionKind::Field => field_nodes += 1,
                _ => {}
            }
        }
        (record_nodes, field_nodes, record_provenance)
    });
    assert_eq!(record_nodes, 1, "one total record value owns the result");
    assert_eq!(
        field_nodes, 2,
        "each record field equation is a typed projection"
    );
    let record_provenance = record_provenance.expect("record aggregate provenance");
    assert_eq!(
        record_provenance.origin(),
        DaeProvenanceOrigin::Generated(DaeGeneration::FunctionAggregateLowering)
    );
    assert!(
        compiled
            .dae
            .source_text(record_provenance)
            .is_some_and(|source| source.contains("result.matrix")),
        "generated aggregate retains its semantically responsible source assignment"
    );

    let wire = serde_json::to_string(&compiled.dae).expect("record DAE wire encoding");
    let decoded: rumoca_ir_dae::Dae =
        serde_json::from_str(&wire).expect("record DAE wire reconstruction");
    let decoded_probe = eval_dae_at(&decoded, &SimOptions::default(), &[], 0.0)
        .expect("wire-reconstructed record projection should evaluate");
    assert_eq!(slot_value(&decoded_probe.report, "observed"), 6.0);
}

#[test]
fn record_fields_remain_readable_before_later_fields_are_assigned() {
    let compiled = Compiler::new()
        .model("ObserveInterleavedRecord")
        .compile_str(
            INTERLEAVED_RECORD_RESULT_MODEL,
            "ObserveInterleavedRecord.mo",
        )
        .expect("independently total record fields should stage in source order");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("interleaved record field assembly should evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    assert_eq!(slot_value(&probe.report, "observed"), 6.0);
}

#[test]
fn shape_derived_integer_local_proves_later_compact_loop_domain() {
    let compiled = Compiler::new()
        .model("ObserveShapeDerivedLoop")
        .compile_str(SHAPE_DERIVED_LOCAL_LOOP_MODEL, "ObserveShapeDerivedLoop.mo")
        .expect("a local settled from a specialized input shape should prove the loop domain");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the compact shape-derived loop should evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    assert_eq!(slot_value(&probe.report, "observed"), 6.0);
}

#[test]
fn loop_conditional_retains_carried_value_when_branch_is_false() {
    let compiled = Compiler::new()
        .model("ObserveLoopConditionalCarry")
        .compile_str(
            LOOP_CONDITIONAL_CARRY_MODEL,
            "ObserveLoopConditionalCarry.mo",
        )
        .expect("a conditional loop update should retain its prior value on fallthrough");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the conditional loop carry should evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    assert_eq!(slot_value(&probe.report, "observed"), 3.0);
}

#[test]
fn continuous_multi_output_equation_owns_each_typed_result() {
    let compiled = Compiler::new()
        .model("ObserveMultiOutputEquation")
        .compile_str(MULTI_OUTPUT_EQUATION_MODEL, "ObserveMultiOutputEquation.mo")
        .expect("a typed tuple equation should lower each function-result ordinal");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the multi-output equation should evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    assert_eq!(slot_value(&probe.report, "vector[1]"), 1.0);
    assert_eq!(slot_value(&probe.report, "vector[2]"), 2.0);
    assert_eq!(slot_value(&probe.report, "scalar"), 3.0);
}

#[test]
fn multi_output_equation_rejects_receiver_shape_mismatch() {
    let source = MULTI_OUTPUT_EQUATION_MODEL.replace(
        "model ObserveMultiOutputEquation\n  output Real vector[2];",
        "model ObserveMultiOutputEquation\n  output Real vector[3];",
    );
    let error = Compiler::new()
        .model("ObserveMultiOutputEquation")
        .compile_str(&source, "MismatchedMultiOutputEquation.mo")
        .expect_err("a tuple receiver with the wrong shape must not construct a DAE");
    assert!(
        error
            .to_string()
            .contains("receiver `vector` has shape [3]")
            && error.to_string().contains("result 1 has shape [2]"),
        "unexpected diagnostic: {error}"
    );
}

#[test]
fn exhaustive_loop_conditional_defines_tensor_rows_without_a_seed() {
    let compiled = Compiler::new()
        .model("ObserveConditionalRows")
        .compile_str(
            CONDITIONAL_ROW_DEFINITION_MODEL,
            "ObserveConditionalRows.mo",
        )
        .expect("an exhaustive row definition should construct one compact tensor owner");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the conditional row definition should evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    for row in 1..=2 {
        for column in 1..=2 {
            assert_eq!(
                slot_value(&probe.report, &format!("selected[{row},{column}]")),
                0.0
            );
        }
    }
}

#[test]
fn incomplete_record_output_fails_before_dae_construction() {
    let source = RECORD_RESULT_MODEL.replace("  result.scalar := u + 6.0;\n", "");
    let error = Compiler::new()
        .model("ObserveRecordScalar")
        .compile_str(&source, "IncompleteRecord.mo")
        .expect_err("an incomplete record result must not construct a DAE");
    assert!(
        error.to_string().contains("leaves scalar 1 undefined"),
        "unexpected diagnostic: {error}"
    );
}

#[test]
fn overlapping_record_output_fails_before_dae_construction() {
    let source = RECORD_RESULT_MODEL.replace(
        "result.matrix[:, 3] := {7.0, 8.0, 9.0}",
        "result.matrix[:, 2] := {7.0, 8.0, 9.0}",
    );
    let error = Compiler::new()
        .model("ObserveRecordScalar")
        .compile_str(&source, "OverlappingRecord.mo")
        .expect_err("overlapping record-field coverage must not construct a DAE");
    assert!(
        error.to_string().contains("assigned more than once"),
        "unexpected diagnostic: {error}"
    );
}
