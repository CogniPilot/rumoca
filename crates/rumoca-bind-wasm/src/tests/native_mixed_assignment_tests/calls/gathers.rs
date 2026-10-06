//! Checked gather-only model profile, independent of typed-call admission.
use super::*;

#[test]
fn native_model_gather_only_uses_checked_abi_preserves_inputs_and_fault_provenance() {
    let _lock = session_test_guard();
    let source = r#"model ModelGather
      input Boolean first = false;
      input Real samples[2,2] = {{5,6},{7,8}};
      input Integer k = 1;
      output Real result;
    equation
      result = if first then 1 else samples[2,k];
    end ModelGather;"#;
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program_impl(source, "ModelGather").unwrap(),
    )
    .unwrap();
    assert_eq!(artifact["profile"], "native-direct-program-f64-v3");
    assert_eq!(artifact["abi"]["transactional_y"], true);
    let faults = artifact["faults"].as_array().unwrap();
    assert!(
        faults
            .iter()
            .any(|fault| fault["opcode"] == "LoadIndexedRegister"
                && fault["kind"] == "IndexBounds"
                && fault["owner"].is_null()
                && fault["provenance"]["source"].is_string())
    );
    let mut execution = CallExecution::new(&artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let first = slot(&artifact, "first", "P");
    let k = slot(&artifact, "k", "P");
    let result = slot(&artifact, "result", "Y");
    assert_eq!(execution.run(&parameters)[result], 7.);
    parameters[k] = 2.;
    assert_eq!(execution.run(&parameters)[result], 8.);
    parameters[k] = 3.;
    parameters[first] = 1.;
    assert_eq!(execution.run(&parameters)[result], 1.);
    parameters[first] = 0.;
    for invalid in [0., 3., -1., 1.5, f64::NAN, f64::INFINITY] {
        parameters[k] = invalid;
        let input = parameters
            .iter()
            .flat_map(|v| v.to_le_bytes())
            .collect::<Vec<_>>();
        execution
            .memory
            .write(&mut execution.store, execution.p, &input)
            .unwrap();
        let before = execution.memory.data(&execution.store)[..execution.y * 8].to_vec();
        let CallEntry::Checked(call) = execution.call else {
            panic!("checked gather ABI")
        };
        let status = call
            .call(
                &mut execution.store,
                (0, execution.p as i32, 0., execution.scratch as i32, 0),
            )
            .unwrap();
        assert_ne!(status, 0);
        assert!(faults.iter().any(|fault| fault["status"] == status));
        assert_eq!(
            &execution.memory.data(&execution.store)[..execution.y * 8],
            before
        );
        assert_eq!(
            &execution.memory.data(&execution.store)[execution.p..execution.p + input.len()],
            input
        );
    }
    parameters[k] = 1.;
    assert_eq!(execution.run(&parameters)[result], 7.);
}
