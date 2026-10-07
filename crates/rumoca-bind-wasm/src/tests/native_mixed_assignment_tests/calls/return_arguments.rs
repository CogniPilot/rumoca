//! Actual argument evaluation is owned by the call site, before early return.

use super::*;

const FUNCTION: &str = r#"
function argumentReturn
  input Boolean first;
  input Real sample;
  output Real result;
algorithm
  result := 0;
  if first then
    result := 1;
    return;
  elseif sample > 0 then
    result := 2;
    return;
  end if;
  result := 3;
end argumentReturn;
"#;

fn prepare(guarded: bool) -> serde_json::Value {
    let call = "argumentReturn(first, samples[k])";
    let expression = if guarded {
        format!("if first then 1 else {call}")
    } else {
        call.into()
    };
    let source = format!(
        "{FUNCTION}\nmodel ReturnArgument\n\
         input Boolean first = true;\n input Real samples[1] = {{5}};\n\
         input Integer k = 1;\n output Real result;\n\
         equation\n result={expression};\n end ReturnArgument;"
    );
    serde_json::from_str(
        &crate::native_program_api::prepare_native_program(&source, "ReturnArgument")
            .expect("call actuals lower to native SolveIR WASM"),
    )
    .unwrap()
}

fn failed_call_keeps_output(execution: &mut CallExecution, parameters: &[f64]) {
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
        panic!("invalid actual arguments require checked status ABI");
    };
    let status = call
        .call(
            &mut execution.store,
            (0, execution.p as i32, 0.0, execution.scratch as i32, 0),
        )
        .unwrap();
    assert_ne!(
        status, 0,
        "call actual must fault before callee early return"
    );
    let mut after = vec![0; before.len()];
    execution
        .memory
        .read(&execution.store, 0, &mut after)
        .unwrap();
    assert_eq!(after, before, "a failed call must not commit output");
    let mut inputs = vec![0; bytes.len()];
    execution
        .memory
        .read(&execution.store, execution.p, &mut inputs)
        .unwrap();
    assert_eq!(inputs, bytes, "call must preserve input bytes");
}

#[test]
fn native_wasm_return_predicates_keep_call_actual_faults() {
    let _lock = session_test_guard();
    for guarded in [false, true] {
        let artifact = prepare(guarded);
        let mut execution = CallExecution::new(&artifact);
        let mut parameters = artifact["parameters"]
            .as_array()
            .unwrap()
            .iter()
            .map(|value| value.as_f64().unwrap())
            .collect::<Vec<_>>();
        let first = slot(&artifact, "first", "P");
        let index = slot(&artifact, "k", "P");
        let result = slot(&artifact, "result", "Y");
        for (value, expected) in [(1.0, 1.0), (0.0, 2.0)] {
            parameters[first] = value;
            assert_eq!(execution.run(&parameters)[result], expected);
        }
        parameters[index] = 2.0;
        parameters[first] = 1.0;
        if guarded {
            assert_eq!(execution.run(&parameters)[result], 1.0);
        } else {
            failed_call_keeps_output(&mut execution, &parameters);
        }
        parameters[first] = 0.0;
        failed_call_keeps_output(&mut execution, &parameters);
        parameters[index] = 1.0;
        assert_eq!(execution.run(&parameters)[result], 2.0);
    }
}
