//! Compact algebraic families (SOLVE-C70) run through the native program ABI,
//! which links their synthesized call owners; the separate-stage copy ABI has
//! no call table and refuses them.

use super::*;

const IMAGE: &str = "model FamilyImage
  input Real rgb[48] = fill(0.0, 48);
  Real gray[16];
  output Real score[16];
equation
  for i in 1:16 loop
    gray[i] = (rgb[3*i-2] + rgb[3*i-1] + rgb[3*i])/3;
  end for;
  for i in 1:16 loop
    score[i] = gray[i]*gray[i] + 2;
  end for;
end FamilyImage;";

#[test]
fn family_call_owners_execute_bit_exactly_through_the_native_program_abi() {
    let _lock = session_test_guard();
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(IMAGE, "FamilyImage").unwrap(),
    )
    .unwrap();
    assert_eq!(artifact["profile"], "native-direct-program-f64-v3");
    let mut execution = CallExecution::new(&artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    for frame in 0..4 {
        let rgb = (0..48)
            .map(|index| ((index * 7 + frame * 13) % 255) as f64 / 255.0)
            .collect::<Vec<_>>();
        for (index, value) in rgb.iter().enumerate() {
            parameters[slot(&artifact, &format!("rgb[{}]", index + 1), "P")] = *value;
        }
        let values = execution.run(&parameters);
        for index in 0..16 {
            let gray = (rgb[index * 3] + rgb[index * 3 + 1] + rgb[index * 3 + 2]) / 3.0;
            let name = |base: &str| format!("{base}[{}]", index + 1);
            assert_eq!(
                values[slot(&artifact, &name("gray"), "Y")].to_bits(),
                gray.to_bits()
            );
            assert_eq!(
                values[slot(&artifact, &name("score"), "Y")].to_bits(),
                (gray * gray + 2.0).to_bits()
            );
        }
    }
}

#[test]
fn the_separate_stage_abi_refuses_family_call_owners() {
    let _lock = session_test_guard();
    let error =
        crate::native_assignment_api::prepare_native_assignments(IMAGE, "FamilyImage").unwrap_err();
    assert!(
        format!("{error:?}").contains("belong to the native program ABI"),
        "{error:?}"
    );
}
