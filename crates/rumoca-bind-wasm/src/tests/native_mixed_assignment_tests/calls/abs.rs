//! Authored tensor Abs retains its checked typed directional owner.
use super::*;

#[test]
fn typed_tensor_abs_actual_source_keeps_directional_map_and_wasmi_values() {
    let _lock = session_test_guard();
    let source = r#"
function TensorAbs
  input Real values[2,2];
  output Real absolute[2,2];
algorithm
  absolute := abs(values);
end TensorAbs;
model AbsoluteTensor
  input Real values[2,2] = [-3,-0.0;0.0,4];
  output Real absolute[2,2];
equation
  absolute = TensorAbs(values);
end AbsoluteTensor;
"#;
    let encoded = crate::native_assignment_api::with_prepared_native_model(
        source,
        "AbsoluteTensor",
        |model, source, name| {
            let call_start = source.find("TensorAbs(values)").unwrap();
            let owner = model
                .pure_calls
                .owners()
                .iter()
                .find(|owner| owner.provenance().start.0 == call_start)
                .unwrap();
            let directional = owner
                .directional()
                .expect("tensor Abs must remain typed through AD");
            assert_eq!(
                directional
                    .body()
                    .operations()
                    .iter()
                    .filter(|op| matches!(
                        op.operation(),
                        rumoca_ir_solve::SolveOperation::Map { .. }
                    ))
                    .count(),
                1
            );
            crate::native_program_api::model_artifact(model, source, name)
        },
    )
    .unwrap();
    let artifact: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let mut execution = CallExecution::new(&artifact);
    let parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_f64().unwrap())
        .collect::<Vec<_>>();
    let output = execution.run(&parameters);
    for (name, expected) in [
        ("absolute[1,1]", 3.0),
        ("absolute[1,2]", 0.0),
        ("absolute[2,1]", 0.0),
        ("absolute[2,2]", 4.0),
    ] {
        assert_eq!(output[slot(&artifact, name, "Y")], expected);
    }
}
