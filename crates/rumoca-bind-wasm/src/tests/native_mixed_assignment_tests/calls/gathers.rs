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
        &crate::native_program_api::prepare_native_program(source, "ModelGather").unwrap(),
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
    let parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let result = slot(&artifact, "result", "Y");
    // `first` and `k` are typed input lanes (SOLVE-C69); `samples` stays in P.
    assert_eq!(execution.run(&parameters)[result], 7.);
    execution.set_input("k", 2);
    assert_eq!(execution.run(&parameters)[result], 8.);
    execution.set_input("k", 3);
    execution.set_input("first", 1);
    assert_eq!(execution.run(&parameters)[result], 1.);
    execution.set_input("first", 0);
    let input = parameters
        .iter()
        .flat_map(|v| v.to_le_bytes())
        .collect::<Vec<_>>();
    for invalid in [0, 3, -1] {
        execution.set_input("k", invalid);
        let before = execution.memory.data(&execution.store)[..execution.y * 8].to_vec();
        let (status, _) = execution.run_typed(&parameters);
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
    execution.set_input("k", 1);
    assert_eq!(execution.run(&parameters)[result], 7.);
}
