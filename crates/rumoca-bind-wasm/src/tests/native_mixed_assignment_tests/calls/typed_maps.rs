//! Actual source must issue a typed Map before this execution gate can pass.
use super::*;
use sha2::{Digest, Sha256};

const SOURCE: &str = "function AdjacentComparison
    input Real extended[24]; output Real low2[22];
  algorithm
    for i in 1:22 loop
      low2[i] := if noEvent(extended[i] < extended[i+1]) then extended[i] else extended[i+1];
    end for;
  end AdjacentComparison;
  model TypedMapComparison
    input Real x[24] = fill(0.0,24); output Real y[22];
  equation y = AdjacentComparison(x);
  end TypedMapComparison;";

fn artifact(source: &str) -> serde_json::Value {
    let encoded = crate::native_assignment_api::with_prepared_native_model(
        source,
        "TypedMapComparison",
        |model, source, name| {
            assert!(
                model.pure_calls.owners().iter().any(|owner| {
                    owner.body().operations().iter().any(|op| {
                        matches!(op.operation(),
                    rumoca_ir_solve::SolveOperation::Map { domain, .. }
                        if domain.scalar_count() == Ok(22))
                    })
                }),
                "source must retain the canonical 22-point typed Map"
            );
            crate::native_program_api::model_artifact(model, source, name)
        },
    )
    .unwrap();
    let artifact: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    assert_eq!(artifact["profile"], "native-direct-program-f64-v3");
    assert_eq!(artifact["abi"]["transactional_y"], true);
    assert_eq!(
        artifact["source_sha256"],
        format!("{:x}", Sha256::digest(source.as_bytes()))
    );
    artifact
}

fn frames(artifact: &serde_json::Value, edited: bool) {
    let mut runner = CallExecution::new(artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_f64().unwrap())
        .collect::<Vec<_>>();
    let values = [
        0.,
        -0.,
        f64::from_bits(1),
        3.,
        -4.,
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::from_bits(0x7ff8_dead_beef_1234),
    ];
    for frame in 0..8 {
        let input = (0..24).map(|i| values[(i + frame) % 8]).collect::<Vec<_>>();
        for (i, value) in input.iter().enumerate() {
            parameters[slot(artifact, &format!("x[{}]", i + 1), "P")] = *value;
        }
        let output = runner.run(&parameters);
        for i in 0..22 {
            let selected = if edited {
                input[i] > input[i + 1]
            } else {
                input[i] < input[i + 1]
            };
            let value = if selected { input[i] } else { input[i + 1] };
            assert_eq!(
                output[slot(artifact, &format!("y[{}]", i + 1), "Y")].to_bits(),
                value.to_bits()
            );
        }
    }
}

#[test]
fn typed_map_actual_source_native_v3_preserves_22_point_comparisons_and_edit() {
    let _lock = session_test_guard();
    frames(&artifact(SOURCE), false);
    let edited = SOURCE.replace("extended[i] < extended[i+1]", "extended[i] > extended[i+1]");
    frames(&artifact(&edited), true);
}
