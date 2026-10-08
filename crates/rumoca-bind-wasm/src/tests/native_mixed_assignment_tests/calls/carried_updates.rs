//! A carried aggregate rewritten in place by a guarded row update (SOLVE-C71)
//! computes what the source computes, and an index fault is raised on an
//! active slot exactly as before while an inactive slot's index is never read.

use super::*;

const SOURCE: &str = r#"
function Accumulate
  input Real w[:,2];
  input Real mask[size(w,1)];
  input Integer idx[size(w,1)];
  output Real y[3,2];
algorithm
  y := zeros(3,2);
  for k in 1:size(w,1) loop
    if mask[k] == 1.0 then
      y[idx[k],:] := y[idx[k],:] + w[k,:];
    end if;
  end for;
end Accumulate;

model AccumulateModel
  input Real w[4,2] = {{1.0,2.0},{3.0,4.0},{5.0,6.0},{7.0,8.0}};
  input Real mask[4] = {1.0,1.0,0.0,1.0};
  input Integer idx[4] = {1,2,1,1};
  output Real y[3,2];
equation
  y = Accumulate(w, mask, idx);
end AccumulateModel;
"#;

fn run(
    execution: &mut CallExecution,
    artifact: &serde_json::Value,
    parameters: &mut [f64],
    mask: [f64; 4],
    idx: [i64; 4],
) -> (i32, Vec<f64>) {
    for k in 0..4 {
        parameters[slot(artifact, &format!("mask[{}]", k + 1), "P")] = mask[k];
        execution.set_input(&format!("idx[{}]", k + 1), idx[k]);
    }
    execution.run_typed(parameters)
}

#[test]
fn guarded_row_updates_carry_one_aggregate_and_fault_only_when_active() {
    let _lock = session_test_guard();
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(SOURCE, "AccumulateModel").unwrap(),
    )
    .unwrap();
    let mut execution = CallExecution::new(&artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let y0 = slot(&artifact, "y", "Y");
    let rows = |y: &[f64]| y[y0..y0 + 6].to_vec();

    let (status, y) = run(
        &mut execution,
        &artifact,
        &mut parameters,
        [1.0, 1.0, 0.0, 1.0],
        [1, 2, 1, 1],
    );
    assert_eq!(status, 0);
    // Rows 1 and 2 accumulate slots 1, 4 and 2; slot 3 is inactive.
    assert_eq!(rows(&y), [8.0, 10.0, 3.0, 4.0, 0.0, 0.0]);

    // An inactive slot's out-of-range index is never evaluated.
    let (status, y) = run(
        &mut execution,
        &artifact,
        &mut parameters,
        [1.0, 0.0, 0.0, 0.0],
        [1, 0, 99, -4],
    );
    assert_eq!(status, 0);
    assert_eq!(rows(&y), [1.0, 2.0, 0.0, 0.0, 0.0, 0.0]);

    // The same index on an active slot is the checked bounds fault.
    for bad in [0, 4, -1] {
        let (status, _) = run(
            &mut execution,
            &artifact,
            &mut parameters,
            [1.0, 1.0, 0.0, 0.0],
            [1, bad, 1, 1],
        );
        assert_ne!(status, 0, "active index {bad} faults");
    }
}
