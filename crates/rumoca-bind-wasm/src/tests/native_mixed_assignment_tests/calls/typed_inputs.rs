//! Integer and Boolean inputs written to typed input lanes (SPEC_0040
//! SOLVE-C69): an Integer reaches a pure call or an Integer output without a
//! Binary64 hop, and every Real view of an Integer is exact or refused.
use super::*;

/// A nested record state carried across native entrypoints, with Integer
/// fields beyond 2^53.
const SOURCE: &str = r#"
package Carry
  record Identity
    Integer sequence;
    Boolean valid;
  end Identity;
  record State
    Identity identity;
    Real position[3];
    Integer observations[2];
    Boolean occupied[2];
  end State;
  function Empty
    output State state;
  algorithm
    state.identity.sequence := 0;
    state.identity.valid := false;
    state.position := zeros(3);
    state.observations := {0, 0};
    state.occupied := {false, false};
  end Empty;
  function Advance
    input State previous;
    input Boolean requested;
    input Integer increment;
    output State next;
  algorithm
    next := previous;
    if requested then
      next.identity.sequence := previous.identity.sequence + increment;
      next.identity.valid := not previous.identity.valid;
      next.position := previous.position + {1, 2, 3};
      next.observations := previous.observations + {increment, -increment};
      next.occupied := {previous.occupied[2], previous.occupied[1]};
    end if;
  end Advance;
end Carry;
model Step
  input Carry.State previous = Carry.Empty();
  input Boolean requested = true;
  output Carry.State next;
  output Integer receivedSequence;
equation
  next = Carry.Advance(previous, requested, 1);
  receivedSequence = previous.identity.sequence;
end Step;
model RealView
  input Integer count = 1;
  output Real scaled;
equation
  scaled = 0.5 * count;
end RealView;
"#;

fn named<'a>(artifact: &'a serde_json::Value, table: &str, name: &str) -> &'a serde_json::Value {
    artifact[table]
        .as_array()
        .unwrap()
        .iter()
        .find(|lane| lane["name"] == name)
        .unwrap_or_else(|| panic!("{name} is listed in {table}"))
}

fn offset(artifact: &serde_json::Value, table: &str, name: &str) -> usize {
    named(artifact, table, name)["byte_offset"]
        .as_u64()
        .unwrap() as usize
}

fn prepare(model: &str) -> serde_json::Value {
    serde_json::from_str(
        &crate::native_program_api::prepare_native_program(SOURCE, model)
            .expect("the model prepares natively"),
    )
    .unwrap()
}

/// 2^53 + 1 written to an Integer input lane passes through the pure call
/// and straight to an Integer output exactly; Boolean lanes carry 0/1.
#[test]
fn integer_inputs_cross_a_native_step_without_rounding() {
    let _lock = session_test_guard();
    let artifact = prepare("Step");
    assert_eq!(
        named(&artifact, "input_lanes", "previous.identity.sequence")["representation"],
        "i64"
    );
    assert_eq!(
        named(&artifact, "input_lanes", "requested")["representation"],
        "u8"
    );
    // Hosts write typed inputs only to their lanes.
    let bindings = &artifact["var_layout"]["bindings"];
    assert!(bindings.get("previous.identity.sequence").is_none());
    let large = (1_i64 << 53) + 1;
    let parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let mut execution = CallExecution::new(&artifact);
    for (name, value) in [
        ("previous.identity.sequence", large),
        ("previous.observations[1]", large),
        ("previous.observations[2]", -large),
        ("previous.identity.valid", 1),
        ("previous.occupied[1]", 1),
        ("previous.occupied[2]", 0),
        ("requested", 1),
    ] {
        execution.set_input(name, value);
    }
    let (status, values) = execution.run_typed(&parameters);
    assert_eq!(status, 0);
    assert_eq!(values[slot(&artifact, "next.position[3]", "Y")], 3.0);
    let lanes = execution.lanes();
    let integer = |name: &str| {
        let at = offset(&artifact, "derived_outputs", name);
        i64::from_le_bytes(lanes[at..at + 8].try_into().unwrap())
    };
    assert_eq!(integer("receivedSequence"), large);
    assert_eq!(integer("next.identity.sequence"), large + 1);
    assert_eq!(integer("next.observations[1]"), large + 1);
    assert_eq!(integer("next.observations[2]"), -large - 1);
    let boolean = |name: &str| lanes[offset(&artifact, "derived_outputs", name)];
    assert_eq!(boolean("next.identity.valid"), 0);
    assert_eq!(
        (boolean("next.occupied[1]"), boolean("next.occupied[2]")),
        (0, 1)
    );
}

/// A Real view of an Integer input is IntegerToReal: exact values convert,
/// and a magnitude above 2^53 returns status 2 instead of a rounded Real. A
/// Boolean lane byte other than 0 or 1 is refused the same way.
#[test]
fn real_views_of_integer_inputs_are_exact_or_refused() {
    let _lock = session_test_guard();
    let artifact = prepare("RealView");
    let mut execution = CallExecution::new(&artifact);
    let parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    execution.set_input("count", 1 << 53);
    let (status, values) = execution.run_typed(&parameters);
    assert_eq!(status, 0);
    assert_eq!(values[slot(&artifact, "scaled", "Y")], (1_u64 << 52) as f64);
    execution.set_input("count", (1 << 53) + 1);
    assert_eq!(execution.run_typed(&parameters).0, 2);

    let step = prepare("Step");
    let mut execution = CallExecution::new(&step);
    let parameters = step["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    execution.set_input("requested", 2);
    assert_eq!(execution.run_typed(&parameters).0, 2);
}
