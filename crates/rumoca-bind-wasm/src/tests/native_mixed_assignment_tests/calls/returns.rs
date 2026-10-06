//! Execute early-return predicate activation in the emitted SolveIR WASM.

use super::*;

const SOURCE: &str = r#"
function ReturnFeature
  input Boolean first;
  input Real samples[1];
  input Integer k;
  output Real result;
algorithm
  result := 0;
  if first then
    result := 1;
    return;
  elseif samples[k] > 0 then
    result := 2;
    return;
  end if;
  result := 3;
end ReturnFeature;
model ReturnPredicates
  input Boolean first = true;
  input Real samples[1] = {5};
  input Integer k = 1;
  output Real result;
equation
  result = ReturnFeature(first, samples, k);
end ReturnPredicates;
"#;

#[test]
fn native_wasm_return_predicates_preserve_inactive_and_active_gathers() {
    let _lock = session_test_guard();
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program_impl(SOURCE, "ReturnPredicates")
            .expect("unsettled return predicates lower to native SolveIR WASM"),
    )
    .unwrap();
    assert_eq!(artifact["abi"]["result"], "status:i32");
    let mut execution = CallExecution::new(&artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let first = slot(&artifact, "first", "P");
    let sample = slot(&artifact, "samples", "P");
    let index = slot(&artifact, "k", "P");
    let result = slot(&artifact, "result", "Y");
    for (enabled, value, k, expected) in [
        (1.0, 5.0, 2.0, 1.0),
        (0.0, 5.0, 1.0, 2.0),
        (0.0, -5.0, 1.0, 3.0),
        (1.0, -5.0, 2.0, 1.0),
    ] {
        parameters[first] = enabled;
        parameters[sample] = value;
        parameters[index] = k;
        assert_eq!(execution.run(&parameters)[result], expected);
    }
    // The same artifact must fault when the invalid gather becomes active.
    // Its transactional ABI leaves the previously committed result intact.
    parameters[first] = 0.0;
    parameters[index] = 2.0;
    let bytes = parameters
        .iter()
        .flat_map(|v| v.to_le_bytes())
        .collect::<Vec<_>>();
    execution
        .memory
        .write(&mut execution.store, execution.p, &bytes)
        .unwrap();
    let mut before = vec![0; execution.y * 8];
    execution
        .memory
        .read(&execution.store, 0, &mut before)
        .unwrap();
    let CallEntry::Checked(call) = execution.call else {
        panic!("a faulting gather requires the status ABI");
    };
    let status = call
        .call(
            &mut execution.store,
            (0, execution.p as i32, 0.0, execution.scratch as i32, 0),
        )
        .expect("a checked gather reports status without trapping");
    assert_ne!(status, 0, "an active invalid gather must fail");
    let mut after = vec![0; before.len()];
    execution
        .memory
        .read(&execution.store, 0, &mut after)
        .unwrap();
    assert_eq!(after, before, "failed invocation must not commit output");
    parameters[first] = 1.0;
    assert_eq!(execution.run(&parameters)[result], 1.0);
}
