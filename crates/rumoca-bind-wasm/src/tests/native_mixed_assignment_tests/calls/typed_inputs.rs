//! Integer and Boolean inputs written to typed input lanes (SPEC_0040
//! SOLVE-C69): an Integer reaches a pure call or an Integer output without a
//! Binary64 hop, and every Real view of an Integer is exact or refused.
use super::*;

/// A nested record state carried across native entrypoints, with Integer
/// fields beyond 2^53.
pub(super) const SOURCE: &str = r#"
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
function Mixed
  input Integer n;
  input Real x;
  output Real y;
algorithm
  y := x - n;
end Mixed;
model SharedArgument
  input Integer count = 1;
  output Real difference;
equation
  difference = Mixed(count, count);
end SharedArgument;
function NativeTypedInputValue
  input Real value;
  input Integer sequence;
  input Boolean enabled;
  output Real result;
algorithm
  result := 0.0;
  if enabled then
    result := value+sequence;
  end if;
end NativeTypedInputValue;
model NativeTypedInputsProbe
  input Real value = 2.5;
  input Integer sequence = 7;
  input Boolean enabled = true;
  output Real result;
equation
  result = NativeTypedInputValue(value,sequence,enabled);
end NativeTypedInputsProbe;
model RealView
  input Integer count = 1;
  output Real scaled;
equation
  scaled = 0.5 * count;
end RealView;
model RealViewArray
  input Integer counts[4] = {1, 2, 3, 4};
  output Real scaled[4];
equation
  scaled = 0.5 * counts;
end RealViewArray;
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

#[test]
fn native_integer_argument_rejects_positive_i64_overflow_before_call() {
    let _lock = session_test_guard();
    let source = r#"
function ObserveArgument
  input Integer value;
  output Real result;
algorithm
  result := if value == 9223372036854775807 then 1.0 else 2.0;
end ObserveArgument;
model IntegerArgumentOverflow
  input Integer count = 1;
  output Real result;
equation
  result = ObserveArgument(count * 2048);
end IntegerArgumentOverflow;
"#;
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(source, "IntegerArgumentOverflow")
            .expect("the original model has an independent native call argument"),
    )
    .unwrap();
    let parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let mut execution = CallExecution::new(&artifact);
    execution.set_input("count", 1);
    let (status, before) = execution.run_typed(&parameters);
    assert_eq!(status, 0);
    assert_eq!(before[slot(&artifact, "result", "Y")], 2.0);

    execution.set_input("count", -(1_i64 << 52));
    let (status, minimum) = execution.run_typed(&parameters);
    assert_eq!(status, 0, "the signed-i64 minimum remains admissible");
    assert_eq!(minimum, before);

    // The typed input is exactly representable, but its Integer product is
    // +2^63, outside the checked signed-i64 call interface. Saturating the
    // float to i64::MAX must not admit the call or replace the prior result.
    execution.set_input("count", 1_i64 << 52);
    let (status, after) = execution.run_typed(&parameters);
    assert_eq!(status, 2, "an overflowing Integer argument must refuse");
    assert_eq!(after, before, "a refused call must retain published Y");
}

#[test]
fn native_integer_argument_never_publishes_a_rounded_integer_product() {
    let _lock = session_test_guard();
    let source = r#"
function ObserveExactProduct
  input Integer value;
  output Real result;
algorithm
  result := if value == 9007199254740993 then 1.0 else 2.0;
end ObserveExactProduct;
model IntegerArgumentProduct
  input Integer count = 1;
  output Real result;
equation
  result = ObserveExactProduct(count * 3);
end IntegerArgumentProduct;
"#;
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(source, "IntegerArgumentProduct")
            .expect("the original model has an independent native call argument"),
    )
    .unwrap();
    let parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let mut execution = CallExecution::new(&artifact);
    execution.set_input("count", 1);
    let (status, before) = execution.run_typed(&parameters);
    assert_eq!(status, 0);
    assert_eq!(before[slot(&artifact, "result", "Y")], 2.0);

    execution.set_input("count", 3_002_399_751_580_331);
    let (status, after) = execution.run_typed(&parameters);
    if status == 0 {
        assert_eq!(
            after[slot(&artifact, "result", "Y")],
            1.0,
            "successful Integer arithmetic must retain the exact source product"
        );
    } else {
        assert_eq!(status, 2, "an unsupported exact Integer read must refuse");
        assert_eq!(after, before, "refusal cannot replace published Y");
    }
}

#[test]
fn native_integer_input_power_of_two_scaling_retains_its_exact_source_proof() {
    let _lock = session_test_guard();
    for factor in [-1_i64, 1, -2, 2, -(1_i64 << 62), i64::MIN] {
        let factor_source = if factor == i64::MIN {
            "-9223372036854775807 - 1".to_owned()
        } else {
            factor.to_string()
        };
        let source = format!(
            r#"
function ObserveScaledInteger
  input Integer value;
  output Real result;
algorithm
  result := if value == 0 then 0.0 elseif value < 0 then -1.0 else 1.0;
end ObserveScaledInteger;
model ExactInputScaling
  input Integer count = 1;
  output Real result;
equation
  result = ObserveScaledInteger(count * ({factor_source}));
end ExactInputScaling;
"#
        );
        let artifact: serde_json::Value = serde_json::from_str(
            &crate::native_program_api::prepare_native_program(&source, "ExactInputScaling")
                .expect("signed power-of-two input scaling prepares"),
        )
        .unwrap();
        let parameters = artifact["parameters"]
            .as_array()
            .unwrap()
            .iter()
            .map(|value| value.as_f64().unwrap())
            .collect::<Vec<_>>();
        let mut execution = CallExecution::new(&artifact);
        execution.set_input("count", 0);
        let (status, zero) = execution.run_typed(&parameters);
        assert_eq!(status, 0, "zero times {factor}");
        assert_eq!(zero[slot(&artifact, "result", "Y")], 0.0);
        execution.set_input("count", 1);
        let (status, before) = execution.run_typed(&parameters);
        assert_eq!(status, 0, "one times {factor}");
        assert_eq!(
            before[slot(&artifact, "result", "Y")],
            factor.signum() as f64
        );
        execution.set_input("count", -1);
        let (status, after) = execution.run_typed(&parameters);
        if factor == i64::MIN {
            assert_eq!(status, 2, "the opposite of i64MIN overflows");
            assert_eq!(after, before);
        } else {
            assert_eq!(status, 0, "minus one times {factor}");
            assert_eq!(
                after[slot(&artifact, "result", "Y")],
                -factor.signum() as f64
            );
        }
    }
}

#[test]
fn native_real_power_never_publishes_a_rounded_integer_exponent() {
    let _lock = session_test_guard();
    let source = r#"
model IntegerExponentProduct
  input Integer count = 1;
  output Real result;
equation
  result = (-1.0) ^ (count * 3);
end IntegerExponentProduct;
"#;
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(source, "IntegerExponentProduct")
            .expect("the original Real power has an independent Integer exponent"),
    )
    .unwrap();
    let parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let mut execution = CallExecution::new(&artifact);
    execution.set_input("count", 1);
    let (status, before) = execution.run_typed(&parameters);
    assert_eq!(status, 0);
    assert_eq!(before[slot(&artifact, "result", "Y")], -1.0);
    execution.set_input("count", 3_002_399_751_580_331);
    let (status, after) = execution.run_typed(&parameters);
    if status == 0 {
        assert_eq!(
            after[slot(&artifact, "result", "Y")],
            -1.0,
            "a successful power must preserve the exact Integer exponent parity"
        );
    } else {
        assert_eq!(status, 2);
        assert_eq!(
            after, before,
            "a refused exact exponent cannot replace published Y"
        );
    }
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

/// An Integer input that a model also echoes to an Integer output is read
/// back from its input lane, not from the rounded Real view in P, for every
/// magnitude an i64 can carry.
#[test]
fn integer_input_echo_keeps_every_wide_value() {
    let _lock = session_test_guard();
    let artifact = prepare("Step");
    let parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    for value in [(1_i64 << 53) + 1, -((1_i64 << 53) + 1), i64::MIN + 1] {
        let mut execution = CallExecution::new(&artifact);
        execution.set_input("previous.identity.sequence", value);
        let (status, _) = execution.run_typed(&parameters);
        assert_eq!(status, 0);
        let lanes = execution.lanes();
        let at = offset(&artifact, "derived_outputs", "receivedSequence");
        let echoed = i64::from_le_bytes(lanes[at..at + 8].try_into().unwrap());
        assert_eq!(echoed, value);
    }
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

/// Every cell of an Integer array an unbound reader reads is checked: an exact
/// magnitude passes in any cell and one above 2^53 is refused wherever it is.
#[test]
fn each_cell_of_an_integer_array_view_is_exact_or_refused() {
    let _lock = session_test_guard();
    let artifact = prepare("RealViewArray");
    let parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    for cell in 1..=4 {
        let mut execution = CallExecution::new(&artifact);
        for other in 1..=4 {
            execution.set_input(&format!("counts[{other}]"), 1 << 53);
        }
        let (status, values) = execution.run_typed(&parameters);
        assert_eq!(status, 0);
        assert_eq!(
            values[slot(&artifact, "scaled[2]", "Y")],
            (1_u64 << 52) as f64
        );
        execution.set_input(&format!("counts[{cell}]"), (1 << 53) + 1);
        assert_eq!(execution.run_typed(&parameters).0, 2, "counts[{cell}]");
        assert_eq!(execution.host_p(parameters.len()), parameters);
    }
}

/// The stages patch the typed-input slots of P with the Real views of their
/// lanes; the entry restores them, so the host P is unchanged after a call that
/// succeeds and after one that is refused.
#[test]
fn typed_input_views_leave_the_host_p_unchanged() {
    let _lock = session_test_guard();
    let artifact = prepare("RealView");
    let parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let mut execution = CallExecution::new(&artifact);
    execution.set_input("count", 6);
    let (status, values) = execution.run_typed(&parameters);
    assert_eq!(status, 0);
    assert_eq!(values[slot(&artifact, "scaled", "Y")], 3.0);
    assert_eq!(execution.host_p(parameters.len()), parameters);
    execution.set_input("count", (1 << 53) + 1);
    assert_eq!(execution.run_typed(&parameters).0, 2);
    assert_eq!(execution.host_p(parameters.len()), parameters);
}

/// One Integer input passed both as an Integer argument (bound to its lane)
/// and as a Real argument of the same call: the Real cell is the authored
/// coercion of the call, so it rounds (IEEE 754) and the call returns status
/// 0; the Integer cell still reads the exact lane.
#[test]
fn an_integer_input_also_passed_as_a_real_argument_rounds_the_real_cell() {
    let _lock = session_test_guard();
    let artifact = prepare("SharedArgument");
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
    assert_eq!(values[slot(&artifact, "difference", "Y")], 0.0);
    execution.set_input("count", (1 << 53) + 1);
    let (status, values) = execution.run_typed(&parameters);
    assert_eq!(status, 0);
    // The Real cell holds 2^53 (ties to even); the callee converts the exact
    // Integer cell to Real the same way, so the difference is zero.
    assert_eq!(values[slot(&artifact, "difference", "Y")], 0.0);
}

/// Boolean input lanes are one byte each, so the input lane region is
/// padded to 8 bytes: the output lanes after it stay aligned for a host
/// `BigInt64Array` view of an Integer output.
#[test]
fn output_lanes_after_boolean_inputs_stay_eight_byte_aligned() {
    let _lock = session_test_guard();
    let artifact = prepare("Step");
    let abi = &artifact["abi"];
    let typed = abi["typed_lanes_offset"].as_u64().unwrap();
    let inputs = abi["input_lanes_bytes"].as_u64().unwrap();
    let outputs = abi["output_lanes_offset"].as_u64().unwrap();
    assert_eq!(outputs, typed + inputs);
    assert_eq!(outputs % 8, 0, "output lanes at {outputs}");
    assert_eq!(
        offset(&artifact, "derived_outputs", "receivedSequence") % 8,
        0
    );
}

/// An authored Real expression over an Integer input (`value + sequence`) is
/// Modelica Real arithmetic: the Integer converts to Binary64 with IEEE 754
/// rounding above 2^53 (MLS 3.7 has no inexact-conversion error), so the call
/// returns status 0 with the rounded sum. The Integer reaches the callee
/// through its lane, so the Real view of the input is never read and never
/// checked.
#[test]
fn an_authored_real_expression_over_an_integer_input_rounds() {
    let _lock = session_test_guard();
    let artifact = prepare("NativeTypedInputsProbe");
    let parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let mut execution = CallExecution::new(&artifact);
    execution.set_input("enabled", 1);
    execution.set_input("sequence", (1 << 53) + 1);
    let (status, values) = execution.run_typed(&parameters);
    assert_eq!(status, 0);
    // 2^53 + 1 converts to 2^53 (ties to even); 2^53 + 2.5 rounds to 2^53 + 2.
    assert_eq!(
        values[slot(&artifact, "result", "Y")],
        ((1_u64 << 53) + 2) as f64
    );
    execution.set_input("sequence", 7);
    let (status, values) = execution.run_typed(&parameters);
    assert_eq!(status, 0);
    assert_eq!(values[slot(&artifact, "result", "Y")], 9.5);
}

#[test]
fn native_integer_scaling_constant_cannot_borrow_a_rounded_cancellation() {
    let _lock = session_test_guard();
    let source = r#"
function ObserveExactInteger
  input Integer value;
  output Real result;
algorithm
  result := if value == 3 then 3.0 else 0.0;
end ObserveExactInteger;
model ExactCancellationScaling
  input Integer count = 1;
  output Real result;
equation
  result = ObserveExactInteger(count * (9007199254740993 - 9007199254740992));
end ExactCancellationScaling;
"#;
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(source, "ExactCancellationScaling")
            .expect("closed exact Integer cancellation prepares"),
    )
    .unwrap();
    let parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let mut execution = CallExecution::new(&artifact);
    execution.set_input("count", 3);
    let (status, result) = execution.run_typed(&parameters);
    assert_eq!(status, 0, "exact Integer cancellation leaves the factor 1");
    assert_eq!(result[slot(&artifact, "result", "Y")], 3.0);
}
