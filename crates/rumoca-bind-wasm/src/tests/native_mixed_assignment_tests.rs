//! Source-issued scalar boundaries and compact array kernels share one ABI.

mod calls;
mod exact_schedules;
mod execution;
mod program;

use super::*;
use execution::NativeExecution;

const MIXED: &str = r#"
model NativeMixed
  input Real rgb[48] = fill(0.0, 48);
  input Real shift = 0.5;
  Real gray[16];
  Real gain;
  output Real score[16];
equation
  gain = rgb[1] + shift;
  score[1] = 0;
  score[16] = (-0.25);
  for i in 1:8 loop
    gray[i] = (rgb[3*i-2] + rgb[3*i-1] + rgb[3*i])/3;
  end for;
  for i in 9:16 loop
    gray[i] = (rgb[3*i-2] + rgb[3*i-1] + rgb[3*i])/3;
  end for;
  for i in 2:15 loop
    score[i] = gray[i]*gain + 2;
  end for;
end NativeMixed;
"#;

fn prepare(source: &str, name: &str) -> serde_json::Value {
    serde_json::from_str(
        &crate::native_assignment_api::prepare_native_assignments(source, name).unwrap(),
    )
    .unwrap()
}

fn slot(artifact: &serde_json::Value, name: &str, storage: &str) -> usize {
    artifact["var_layout"]["bindings"][name][storage]["index"]
        .as_u64()
        .unwrap() as usize
}

fn verify_frames(artifact: &serde_json::Value, addition: f64) {
    let mut execution = NativeExecution::new(artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let rgb_slots = (1..=48)
        .map(|index| slot(artifact, &format!("rgb[{index}]"), "P"))
        .collect::<Vec<_>>();
    let shift_slot = slot(artifact, "shift", "P");
    let gray_start = slot(artifact, "gray", "Y");
    let gain_slot = slot(artifact, "gain", "Y");
    let score_start = slot(artifact, "score", "Y");
    for frame in 0..8 {
        let rgb = (0..48)
            .map(|index| ((index * 7 + frame * 13) % 255) as f64 / 255.0)
            .collect::<Vec<_>>();
        for (&parameter, &value) in rgb_slots.iter().zip(&rgb) {
            parameters[parameter] = value;
        }
        parameters[shift_slot] = if frame % 2 == 0 { 0.5 } else { -0.75 };
        let values = execution.evaluate(&parameters, frame as f64);
        let gain = rgb[0] + parameters[shift_slot];
        assert_eq!(values[gain_slot].to_bits(), gain.to_bits());
        for index in 0..16 {
            let gray = (rgb[index * 3] + rgb[index * 3 + 1] + rgb[index * 3 + 2]) / 3.0;
            assert_eq!(values[gray_start + index].to_bits(), gray.to_bits());
            let score: f64 = match index {
                0 => 0.0,
                15 => -0.25,
                _ => gray * gain + addition,
            };
            assert_eq!(values[score_start + index].to_bits(), score.to_bits());
        }
    }
}

#[test]
fn mixed_native_values_execute_changed_inputs_and_edited_source() {
    let _lock = session_test_guard();
    let artifact = prepare(MIXED, "NativeMixed");
    assert_eq!(artifact["profile"], "native-direct-assignments-f64-v1");
    assert_eq!(
        artifact["solve_schema_version"],
        rumoca_ir_solve::SOLVE_SCHEMA_VERSION
    );
    assert_eq!(artifact["abi"]["y_count"], 33);
    assert_eq!(artifact["abi"]["p_count"], 49);
    // Every unknown has exactly one issued owner; array values are checked
    // element by element against the source semantics below.
    assert_eq!(
        super::native_assignment_tests::issued_target_count(&artifact),
        33
    );
    verify_frames(&artifact, 2.0);

    let edited = prepare(&MIXED.replace("+ 2;", "+ 3;"), "NativeMixed");
    assert_ne!(edited["source_sha256"], artifact["source_sha256"]);
    assert_ne!(edited["stages"], artifact["stages"]);
    verify_frames(&edited, 3.0);
}

#[test]
fn mixed_wire_decode_reissues_the_issued_schedule() {
    let mut session = Session::default();
    session.update_document("input.mo", MIXED);
    let compilation = compile_requested_model(&mut session, "NativeMixed").unwrap();
    let problem = rumoca_sim::lower_solve_problem(&compilation.dae).unwrap();
    let stages = super::native_assignment_tests::issued_stage_ranges;
    let original = stages(&problem);
    assert_eq!(
        original
            .iter()
            .map(|(_, targets)| targets.len())
            .sum::<usize>(),
        33
    );
    let wire = serde_json::to_string(&problem).unwrap();
    assert!(!wire.contains("native_assignment_schedule"));
    let replay: rumoca_ir_solve::SolveProblem = serde_json::from_str(&wire).unwrap();
    assert_eq!(stages(&replay), original);
}

#[test]
fn scalar_zero_and_time_values_are_portable_but_coupled_scalar_values_remain_refused() {
    let _lock = session_test_guard();
    let source = "model ScalarNative output Real a; output Real b; equation a=0; b=time+2; end ScalarNative;";
    let artifact = prepare(source, "ScalarNative");
    let mut execution = NativeExecution::new(&artifact);
    for time in [0.0, 1.0, -2.0, 12.5] {
        let values = execution.evaluate(&[], time);
        assert_eq!(
            values[slot(&artifact, "a", "Y")].to_bits(),
            0.0_f64.to_bits()
        );
        assert_eq!(
            values[slot(&artifact, "b", "Y")].to_bits(),
            (time + 2.0).to_bits()
        );
    }
    assert!(
        crate::native_assignment_api::prepare_native_assignments(
            "model Coupled output Real y; equation y*y=2; end Coupled;",
            "Coupled"
        )
        .is_err()
    );
}
