//! A function passes a large array on to the functions it calls. The callee
//! reads the caller's storage, so a call frame holds an address, never a copy
//! of the array, however deep the call chain.

use super::*;

const SOURCE: &str = r#"
function Edge
  input Real a[:];
  output Real s;
algorithm
  s := a[1] + a[size(a,1)];
end Edge;

function Middle
  input Real a[:];
  output Real s;
algorithm
  s := Edge(a) + 1.0;
end Middle;

function Halves
  input Real a[:];
  output Real low[size(a,1)];
  output Real high[size(a,1)];
  output Real total;
algorithm
  low := 0.5 * a;
  high := 2.0 * a;
  total := low[1] + high[size(a,1)];
end Halves;

function Outer
  input Real a[:];
  output Real s;
protected
  Real low[size(a,1)];
  Real high[size(a,1)];
  Real total;
  Real other;
algorithm
  (low, high, total) := Halves(a);
  other := Edge(high) + Edge(low);
  s := Middle(a) + 2.0 * Middle(a) + total + other;
end Outer;

model Chain
  parameter Integer n = @N@;
  input Real a[n] = zeros(n);
  output Real s;
equation
  s = Outer(a);
end Chain;
"#;

const CELLS: usize = 4096;

fn artifact() -> serde_json::Value {
    let source = SOURCE.replace("@N@", &CELLS.to_string());
    serde_json::from_str(
        &crate::native_program_api::prepare_native_program(&source, "Chain").unwrap(),
    )
    .unwrap()
}

/// `(input_bytes, owner)` of every call site of every owner.
fn call_inputs(artifact: &serde_json::Value) -> Vec<(u64, u64)> {
    artifact["abi"]["scratch_report"]["owners"]
        .as_array()
        .unwrap()
        .iter()
        .flat_map(|owner| owner["frame"]["calls"].as_array().unwrap())
        .map(|call| {
            (
                call["input_bytes"].as_u64().unwrap(),
                call["owner"].as_u64().unwrap(),
            )
        })
        .collect()
}

#[test]
fn nested_calls_pass_a_large_array_by_address_and_compute_the_same_value() {
    let _lock = session_test_guard();
    let artifact = artifact();
    let array_bytes = (CELLS * 8) as u64;
    let calls = call_inputs(&artifact);
    assert!(
        calls.len() >= 3,
        "Outer calls Middle twice, Middle calls Edge: {calls:?}"
    );
    for (input_bytes, owner) in calls {
        assert!(
            input_bytes < array_bytes,
            "a call to owner {owner} packed {input_bytes} input bytes beside {array_bytes} byte arrays"
        );
    }
    let layout: rumoca_ir_solve::VarLayout =
        serde_json::from_value(artifact["var_layout"].clone()).unwrap();
    let binding = |name: &str| match layout.binding(name).unwrap() {
        rumoca_ir_solve::ScalarSlot::Y { index, .. }
        | rumoca_ir_solve::ScalarSlot::P { index, .. } => index,
        _ => panic!("{name} is a scalar slot"),
    };
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    for cell in 0..CELLS {
        parameters[binding(&format!("a[{}]", cell + 1))] = cell as f64 + 0.25;
    }
    let mut execution = CallExecution::new(&artifact);
    let (status, y) = execution.run_typed(&parameters);
    assert_eq!(status, 0);
    let edge = 0.25 + (CELLS - 1) as f64 + 0.25;
    // The arrays a callee returns are the registers its caller reads: Halves
    // writes low and high into its caller's frame.
    let first = 0.25;
    let last = (CELLS - 1) as f64 + 0.25;
    let total = 0.5 * first + 2.0 * last;
    let other = (2.0 * first + 2.0 * last) + (0.5 * first + 0.5 * last);
    assert_eq!(y[binding("s")], (edge + 1.0) * 3.0 + total + other);
}
