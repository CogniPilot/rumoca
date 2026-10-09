//! The prepared artifact reports where its call scratch goes.

use super::*;

const SOURCE: &str = r#"
function Big
  input Real x;
  output Real y;
protected
  Real work[100];
algorithm
  work := fill(x, 100);
  y := work[1];
end Big;
function Twice
  input Real x;
  output Real y;
protected
  Real first;
algorithm
  first := Big(x);
  y := Big(first + 1.0);
end Twice;
model SequentialCalls
  input Real u = 1;
  output Real y;
equation
  y = Twice(u);
end SequentialCalls;
"#;

#[test]
fn prepared_artifact_reports_frame_region_and_call_high_water_marks() {
    let _lock = session_test_guard();
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(SOURCE, "SequentialCalls").unwrap(),
    )
    .unwrap();
    let report = &artifact["abi"]["scratch_report"];
    assert_eq!(report["total_bytes"], artifact["abi"]["scratch_bytes"]);
    for key in [
        "work_y_bytes",
        "call_input_bytes",
        "call_output_bytes",
        "call_scratch_bytes",
        "memo_bytes",
        "typed_lane_bytes",
        "p_copy_bytes",
        "unshared_call_scratch_bytes",
        "widest_owner",
    ] {
        assert!(report.get(key).is_some(), "missing {key}");
    }
    let owners = report["owners"].as_array().unwrap();
    let twice = owners
        .iter()
        .find(|owner| !owner["frame"]["calls"].as_array().unwrap().is_empty())
        .expect("the caller owner reports its call sites");
    let frame = &twice["frame"];
    let calls = frame["calls"].as_array().unwrap();
    assert_eq!(calls.len(), 2);
    assert!(calls[0]["scratch_bytes"].as_u64().unwrap() >= 800);
    for key in [
        "base_bytes",
        "high_water_bytes",
        "unshared_bytes",
        "slot_bytes",
        "register_bytes",
        "register_count",
        "largest_register_bytes",
        "regions",
    ] {
        assert!(frame.get(key).is_some(), "missing {key}");
    }
    // Two sequential calls share one callee frame.
    assert!(
        frame["high_water_bytes"].as_u64().unwrap() < frame["unshared_bytes"].as_u64().unwrap()
    );
    assert!(
        twice["provenance"]["end"].as_u64().unwrap()
            > twice["provenance"]["start"].as_u64().unwrap()
    );
}

/// The 128 node, 256 edge pose-graph optimizer calls its Cholesky, PCG and
/// line-search functions from nested loops and branches; each authored call
/// is one site (SOLVE-C73). Laying every call frame out disjointly needs
/// several MB for its root owner; sequential frames
/// share storage, so the whole program fits well under the 64 MiB cap.
#[test]
fn pose_graph_optimizer_prepares_with_shared_call_frames() {
    let _lock = session_test_guard();
    let source = include_str!("../../fixtures/ModelicaPoseGraph.mo");
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(source, "ModelicaPoseGraph").unwrap(),
    )
    .unwrap();
    let report = &artifact["abi"]["scratch_report"];
    let scratch = artifact["abi"]["scratch_bytes"].as_u64().unwrap();
    assert_eq!(report["total_bytes"].as_u64().unwrap(), scratch);
    assert!(scratch < 4 * 1024 * 1024, "scratch {scratch}");
    let unshared = report["unshared_call_scratch_bytes"].as_u64().unwrap();
    let shared = report["call_scratch_bytes"].as_u64().unwrap();
    assert!(shared < unshared / 2, "shared {shared} unshared {unshared}");
}
