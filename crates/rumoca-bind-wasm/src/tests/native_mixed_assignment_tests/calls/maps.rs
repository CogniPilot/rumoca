//! A source whose only values are pure calls selects the linked status ABI.
use super::*;
use sha2::{Digest, Sha256};

fn artifact(source: &str) -> serde_json::Value {
    let encoded =
        crate::native_program_api::prepare_native_program(source, "CompactCalls").unwrap();
    let artifact: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    assert_eq!(artifact["profile"], "native-direct-program-f64-v3");
    assert_eq!(artifact["abi"]["transactional_y"], true);
    assert_eq!(
        artifact["source_sha256"],
        format!("{:x}", Sha256::digest(source.as_bytes()))
    );
    artifact
}

fn expected(value: f64, edited: bool) -> f64 {
    if value == 0. {
        value
    } else if edited {
        2.0 * value
    } else {
        -value
    }
}

fn check_frames(artifact: &serde_json::Value, edited: bool) {
    let mut runner = CallExecution::new(artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_f64().unwrap())
        .collect::<Vec<_>>();
    for values in [
        [0., -0., 2.5],
        [f64::INFINITY, f64::NEG_INFINITY, 4.],
        [3., 2., 1.],
    ] {
        for (i, value) in values.iter().enumerate() {
            parameters[slot(artifact, &format!("x[{}]", i + 1), "P")] = *value;
        }
        let output = runner.run(&parameters);
        for (i, value) in values.into_iter().enumerate() {
            assert_eq!(
                output[slot(artifact, &format!("y[{}]", i + 1), "Y")].to_bits(),
                expected(value, edited).to_bits()
            );
        }
    }
}

#[test]
fn pure_call_values_select_v3_and_execute_source_edits() {
    let _lock = session_test_guard();
    let source = "function ExactEqual
      input Real left; input Real right; output Boolean equal;
      algorithm equal := left == right; end ExactEqual;
      model CompactCalls
      input Real x[3] = {1,2,3}; output Real y[3];
      equation for i in 1:3 loop
        y[i] = if ExactEqual(x[i],0.0) then x[i] else -x[i];
      end for; end CompactCalls;";
    for (source, edited) in [
        (source.to_owned(), false),
        (source.replace("else -x[i]", "else 2.0*x[i]"), true),
    ] {
        check_frames(&artifact(&source), edited);
    }
}
