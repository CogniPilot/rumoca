//! Authored broadcast power must retain its checked typed directional owner.
use super::*;

#[test]
fn typed_broadcast_power_actual_source_keeps_directional_maps_and_wasmi_values() {
    let _lock = session_test_guard();
    let source = r#"
function VectorPowers
  input Real values[3];
  input Real exponent;
  output Real powers[3];
  output Real reversePowers[3];
algorithm
  powers := values.^exponent;
  reversePowers := exponent.^values;
end VectorPowers;
model BroadcastPower
  input Real values[3] = {1.5,2.0,3.0};
  input Real exponent = 2.5;
  output Real powers[3];
  output Real reversePowers[3];
equation
  (powers,reversePowers) = VectorPowers(values,exponent);
end BroadcastPower;
"#;
    let encoded = crate::native_assignment_api::with_prepared_native_model(
        source,
        "BroadcastPower",
        |model, source, name| {
            let call_start = source.find("VectorPowers(values,exponent)").unwrap();
            let owner = model
                .pure_calls
                .owners()
                .iter()
                .find(|owner| owner.provenance().start.0 == call_start)
                .unwrap();
            let directional = owner
                .directional()
                .expect("broadcast powers must remain typed through AD");
            assert_eq!(
                directional
                    .body()
                    .operations()
                    .iter()
                    .filter(|operation| matches!(
                        operation.operation(),
                        rumoca_ir_solve::SolveOperation::Map { .. }
                    ))
                    .count(),
                2
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
    for (i, value) in [1.5_f64, 2.0, 3.0].into_iter().enumerate() {
        let powers = output[slot(&artifact, &format!("powers[{}]", i + 1), "Y")];
        let reverse = output[slot(&artifact, &format!("reversePowers[{}]", i + 1), "Y")];
        assert!((powers - value.powf(2.5)).abs() < 1e-12);
        assert!((reverse - 2.5_f64.powf(value)).abs() < 1e-12);
    }
}
