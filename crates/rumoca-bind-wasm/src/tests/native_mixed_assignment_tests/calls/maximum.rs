//! The causal max(abs(tensor)) form retains both compact directional owners.
use super::*;

#[test]
fn typed_maximum_of_abs_actual_source_keeps_fold_map_and_parameter_values() {
    let _lock = session_test_guard();
    let source = r#"
function MaximumOfAbs
  input Real values[2,2];
  input Real offset;
  output Real maximum;
algorithm
  maximum := max(abs(values)) + offset;
end MaximumOfAbs;
model AbsoluteMaximum
  input Real values[2,2] = [-3,2;-7,1];
  parameter Real offset = 5;
  output Real maximum;
equation
  maximum = MaximumOfAbs(values,offset);
end AbsoluteMaximum;
"#;
    let encoded = crate::native_assignment_api::with_prepared_native_model(
        source,
        "AbsoluteMaximum",
        |model, source, name| {
            let call_start = source.find("MaximumOfAbs(values,offset)").unwrap();
            let owner = model
                .pure_calls
                .owners()
                .iter()
                .find(|owner| owner.provenance().start.0 == call_start)
                .unwrap();
            let body = owner
                .directional()
                .expect("maximum must remain typed through AD")
                .body();
            let maps = body
                .operations()
                .iter()
                .filter(|op| matches!(op.operation(), rumoca_ir_solve::SolveOperation::Map { .. }))
                .count();
            let folds = body
                .operations()
                .iter()
                .filter(|op| matches!(op.operation(), rumoca_ir_solve::SolveOperation::Fold { .. }))
                .count();
            assert_eq!((maps, folds), (1, 1));
            crate::native_program_api::model_artifact(model, source, name)
        },
    )
    .unwrap();
    let artifact: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let mut execution = CallExecution::new(&artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_f64().unwrap())
        .collect::<Vec<_>>();
    let output = execution.run(&parameters);
    assert_eq!(output[slot(&artifact, "maximum", "Y")], 12.0);
    parameters[slot(&artifact, "offset", "P")] = 7.0;
    let output = execution.run(&parameters);
    assert_eq!(output[slot(&artifact, "maximum", "Y")], 14.0);
}
