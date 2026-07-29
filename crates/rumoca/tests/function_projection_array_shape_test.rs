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
