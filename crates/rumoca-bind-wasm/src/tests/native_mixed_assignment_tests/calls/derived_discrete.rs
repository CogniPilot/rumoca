//! Stateless discrete outputs computed by the native program and published
//! through typed output lanes.
use super::*;

const SOURCE: &str = r#"
function Classify
  input Real x;
  output Real y;
  output Boolean valid;
  output Integer reason;
algorithm
  y := 2*x;
  valid := true;
  reason := 0;
  if x > 10 then
    valid := false;
    reason := 9007199254740993;
  elseif x < 0 then
    valid := false;
    reason := 2;
  end if;
end Classify;
model Edge
  input Real x = 1;
  output Real y;
  output Boolean valid;
  output Integer reason;
  output Real gated;
equation
  (y, valid, reason) = Classify(x);
  gated = if valid then y else -1.0;
end Edge;
"#;

fn lane<'a>(artifact: &'a serde_json::Value, name: &str) -> &'a serde_json::Value {
    artifact["derived_outputs"]
        .as_array()
        .unwrap()
        .iter()
        .find(|output| output["name"] == name)
        .unwrap_or_else(|| panic!("{name} is a derived output"))
}

/// The Integer lane keeps 2^53 + 1 exactly (no Real register hop), the
/// Boolean lane holds 0/1, and the continuous row reading `valid` binds to the
/// value computed in this call, never to the stale Solve P slot.
#[test]
fn discrete_outputs_publish_exact_typed_lanes_and_bind_every_reader() {
    let _lock = session_test_guard();
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(SOURCE, "Edge")
            .expect("stateless discrete outputs lower to a native program"),
    )
    .unwrap();
    assert_eq!(artifact["profile"], "native-direct-program-f64-v3");
    assert_eq!(lane(&artifact, "reason")["representation"], "i64");
    assert_eq!(lane(&artifact, "valid")["representation"], "u8");
    // Hosts read discrete outputs only from their lanes.
    let bindings = &artifact["var_layout"]["bindings"];
    assert!(bindings.get("valid").is_none() && bindings.get("reason").is_none());
    let reason = lane(&artifact, "reason")["byte_offset"].as_u64().unwrap() as usize;
    let valid = lane(&artifact, "valid")["byte_offset"].as_u64().unwrap() as usize;
    let mut execution = CallExecution::new(&artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let x = slot(&artifact, "x", "P");
    for (input, accepted, code) in [
        (1.0, 1u8, 0i64),
        (20.0, 0, 9_007_199_254_740_993),
        (-3.0, 0, 2),
        (4.0, 1, 0),
    ] {
        parameters[x] = input;
        let values = execution.run(&parameters);
        let lanes = execution.lanes();
        assert_eq!(values[slot(&artifact, "y", "Y")], 2.0 * input);
        assert_eq!(lanes[valid], accepted);
        assert_eq!(
            i64::from_le_bytes(lanes[reason..reason + 8].try_into().unwrap()),
            code
        );
        let gated = if accepted == 1 { 2.0 * input } else { -1.0 };
        assert_eq!(values[slot(&artifact, "gated", "Y")], gated);
        let expected = if code.unsigned_abs() <= 1 << 53 {
            crate::native_program_api::IntegerLane::Number(code as f64)
        } else {
            crate::native_program_api::IntegerLane::BigInt(code)
        };
        assert_eq!(
            crate::native_program_api::integer_lane(&lanes, reason).unwrap(),
            expected
        );
    }
}

/// Every discrete form the stateless evaluation cannot own is refused at
/// preparation with its typed reason.
#[test]
fn unsupported_discrete_semantics_are_refused_with_their_reason() {
    let _lock = session_test_guard();
    for (equation, reason) in [
        (
            "output Real twice = 2.0*reason;",
            "an Integer output read by a later stage requires typed program registers",
        ),
        (
            "output Integer next = reason + 1;",
            "an Integer output computed by Real register arithmetic has no exact Integer source",
        ),
        (
            "output Boolean positive = x > 0;",
            "a relation outside noEvent generates events",
        ),
    ] {
        let source = SOURCE.replace(
            "  output Real gated;",
            &format!("  output Real gated;\n  {equation}"),
        );
        let refusal =
            crate::native_program_api::prepare_native_program(&source, "Edge").expect_err(equation);
        assert!(
            refusal.message().contains(reason),
            "{equation}: {}",
            refusal.message()
        );
    }
}
