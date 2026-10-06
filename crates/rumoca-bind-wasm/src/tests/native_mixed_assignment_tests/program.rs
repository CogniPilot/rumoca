//! Actual editable Modelica sources, one-call execution and portable metadata.

mod execution;
use super::*;
use execution::ProgramExecution;
use sha2::{Digest, Sha256};

fn program_artifact(source: &str, name: &str) -> serde_json::Value {
    serde_json::from_str(
        &crate::native_program_api::prepare_native_program_impl(source, name).unwrap(),
    )
    .unwrap()
}

#[test]
fn fused_v2_actual_rectangular_source_preserves_gaps_source_edits_and_copy_abi_refusal() {
    let _lock = session_test_guard();
    let source = "model RectangularCopy
      input Real pixels[15,15] = identity(15);
      output Real result[15,15];
      equation
      for i in 1:15 loop
        for j in 1:6 loop result[i,j] = pixels[i,j]; end for;
        for j in 7:15 loop result[i,j] = pixels[i,j]; end for;
      end for;
      end RectangularCopy;";
    let artifact = program_artifact(source, "RectangularCopy");
    assert_eq!(artifact["profile"], "native-direct-program-f64-v2");
    let stages = artifact["issued_schedule"].as_array().unwrap();
    assert_eq!(stages.len(), 2);
    assert_eq!(stages[0]["target_stride"], 15);
    assert_eq!(stages[0]["target_block_width"], 6);
    assert_eq!(stages[0]["target_count"], 90);
    assert_eq!(stages[1]["target_stride"], 15);
    assert_eq!(stages[1]["target_block_width"], 9);
    assert_eq!(stages[1]["target_count"], 135);
    let mut execution = ProgramExecution::new(&artifact);
    let mut inputs = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    for frame in 0..3 {
        for i in 1..=15 {
            for j in 1..=15 {
                inputs[slot(&artifact, &format!("pixels[{i},{j}]"), "P")] =
                    [0., -0., 2.75, f64::from_bits(1), -1e100][(i + j + frame) % 5];
            }
        }
        let output = execution.evaluate(&inputs, frame as f64);
        check_rectangular_outputs(&artifact, &output, &inputs, false);
    }
    let refused =
        crate::native_assignment_api::prepare_native_assignments_impl(source, "RectangularCopy")
            .unwrap_err();
    assert!(
        refused
            .message()
            .contains("copy ABI requires dense targets")
    );
    let edited = source.replacen(
        "result[i,j] = pixels[i,j];",
        "result[i,j] = 2.0*pixels[i,j];",
        1,
    );
    let changed = program_artifact(&edited, "RectangularCopy");
    assert_ne!(changed["source_sha256"], artifact["source_sha256"]);
    assert_ne!(changed["module_sha256"], artifact["module_sha256"]);
    let output = ProgramExecution::new(&changed).evaluate(&inputs, 0.);
    check_rectangular_outputs(&changed, &output, &inputs, true);
}

fn check_rectangular_outputs(
    artifact: &serde_json::Value,
    output: &[f64],
    inputs: &[f64],
    edited: bool,
) {
    for i in 1..=15 {
        for j in 1..=15 {
            let input = inputs[slot(artifact, &format!("pixels[{i},{j}]"), "P")];
            let expected = if edited && j <= 6 { 2.0 * input } else { input };
            assert_eq!(
                output[slot(artifact, &format!("result[{i},{j}]"), "Y")].to_bits(),
                expected.to_bits()
            );
        }
    }
}

fn check_metadata(program: &serde_json::Value, stages: &serde_json::Value, source: &str) {
    assert_eq!(program["profile"], "native-direct-program-f64-v2");
    assert_eq!(program["abi"]["export"], "eval_assignments");
    assert_eq!(
        program["abi"]["arguments"],
        serde_json::json!([
            "yPtr:i32",
            "pPtr:i32",
            "time:f64",
            "reservedSeedPtr:i32",
            "reservedOutputPtr:i32"
        ])
    );
    assert_eq!(program["abi"]["reserved_pointer_value"], 0);
    assert_eq!(
        program["source_sha256"],
        format!("{:x}", Sha256::digest(source.as_bytes()))
    );
    assert_eq!(program["compiler"], stages["compiler"]);
    assert_eq!(
        program["solve_schema_version"],
        stages["solve_schema_version"]
    );
    for name in ["var_layout", "parameters", "input_names"] {
        assert_eq!(program[name], stages[name]);
    }
    let bytes = program["module_bytes"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_u64().unwrap() as u8)
        .collect::<Vec<_>>();
    assert_eq!(
        program["module_sha256"],
        format!("{:x}", Sha256::digest(bytes))
    );
    let issued = stages["stages"]
        .as_array()
        .unwrap()
        .iter()
        .map(|stage| {
            serde_json::json!({
                "source_node": stage["source_node"], "target_start": stage["target_start"],
                "target_count": stage["target_count"],
                "target_stride": 1,
                "target_block_width": 1,
            })
        })
        .collect::<Vec<_>>();
    assert_eq!(program["issued_schedule"], serde_json::json!(issued));
    assert!(program.get("stages").is_none());
    for name in [
        "seed_count",
        "seed_offset",
        "output_capacity",
        "output_offset",
    ] {
        assert!(program["abi"].get(name).is_none());
    }
}

fn compare_changed_frames(program: &serde_json::Value, stages: &serde_json::Value) {
    let mut fused = ProgramExecution::new(program);
    let mut reference = NativeExecution::new(stages);
    let mut parameters = program["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    for frame in 0..8 {
        for index in 1..=48 {
            parameters[slot(program, &format!("rgb[{index}]"), "P")] =
                ((index * 7 + frame * 13) % 255) as f64 / 255.0;
        }
        parameters[slot(program, "shift", "P")] = if frame % 2 == 0 { 0.5 } else { -0.75 };
        let actual = fused.evaluate(&parameters, frame as f64);
        let expected = reference.evaluate(&parameters, frame as f64);
        assert_eq!(actual.len(), expected.len());
        for (slot, (actual, expected)) in actual.iter().zip(expected).enumerate() {
            assert_eq!(
                actual.to_bits(),
                expected.to_bits(),
                "frame{frame}/slot{slot}"
            );
        }
    }
}

#[test]
fn fused_native_modelica_matches_v1_all_outputs_input_edits_and_source_provenance() {
    let _lock = session_test_guard();
    let stages = super::prepare(MIXED, "NativeMixed");
    let program = program_artifact(MIXED, "NativeMixed");
    check_metadata(&program, &stages, MIXED);
    compare_changed_frames(&program, &stages);
    let edited_source = MIXED.replace("+ 2;", "+ 3;");
    let edited_stages = super::prepare(&edited_source, "NativeMixed");
    let edited_program = program_artifact(&edited_source, "NativeMixed");
    check_metadata(&edited_program, &edited_stages, &edited_source);
    compare_changed_frames(&edited_program, &edited_stages);
    assert_ne!(program["source_sha256"], edited_program["source_sha256"]);
    assert_ne!(program["module_sha256"], edited_program["module_sha256"]);
    assert_ne!(program["module_bytes"], edited_program["module_bytes"]);
}

#[test]
fn fused_native_scalar_time_empty_parameters_and_union_math_imports_execute() {
    let _lock = session_test_guard();
    let source =
        "model ProgramTime output Real a; output Real b; equation a=0; b=time+2; end ProgramTime;";
    let artifact = program_artifact(source, "ProgramTime");
    let mut execution = ProgramExecution::new(&artifact);
    for time in [0.0, 1.0, -2.0, 12.5] {
        let actual = execution.evaluate(&[], time);
        assert_eq!(
            actual[slot(&artifact, "a", "Y")].to_bits(),
            0.0_f64.to_bits()
        );
        assert_eq!(
            actual[slot(&artifact, "b", "Y")].to_bits(),
            (time + 2.0).to_bits()
        );
    }
    let source = "model ProgramMath input Real x=0; Real a; output Real b; equation a=sin(x); b=exp(a); end ProgramMath;";
    let artifact = program_artifact(source, "ProgramMath");
    let mut execution = ProgramExecution::new(&artifact);
    for x in [0.0_f64, 1.0, -2.0, 12.5] {
        let actual = execution.evaluate(&[x], 0.0);
        assert_eq!(
            actual[slot(&artifact, "a", "Y")].to_bits(),
            x.sin().to_bits()
        );
        assert_eq!(
            actual[slot(&artifact, "b", "Y")].to_bits(),
            x.sin().exp().to_bits()
        );
    }
}

#[test]
fn fused_native_profile_preserves_state_and_coupled_equation_refusals() {
    let _lock = session_test_guard();
    for (source, name, reason) in [
        (
            "model Coupled output Real y; equation y*y=2; end Coupled;",
            "Coupled",
            "did not issue a complete native direct-assignment schedule",
        ),
        (
            "model Stateful Real x(start=0); equation der(x)=1; end Stateful;",
            "Stateful",
            "reject states",
        ),
    ] {
        let error =
            crate::native_program_api::prepare_native_program_impl(source, name).unwrap_err();
        assert!(error.message().contains(reason), "{name}: {error}");
    }
}
