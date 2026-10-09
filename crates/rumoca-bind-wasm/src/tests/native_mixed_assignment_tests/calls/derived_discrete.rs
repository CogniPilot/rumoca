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

/// Multiple exact Integer fields and Boolean cells of one record call share
/// one native stage; capture every i64 cell before later calls reuse scratch.
#[test]
fn record_output_cells_share_one_stage_and_keep_every_exact_integer() {
    let _lock = session_test_guard();
    let source = r#"
record Result
  Integer codes[2];
  Boolean valid[2];
end Result;
function Make
  input Real x;
  output Result r;
algorithm
  r.codes := {9007199254740993, -9007199254740993};
  r.valid := {x > 0, x < 0};
end Make;
model Shared
  input Real x = 1;
  Result r;
equation
  r = Make(x);
end Shared;
"#;
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(source, "Shared")
            .expect("one shared record call is admitted"),
    )
    .unwrap();
    assert_eq!(artifact["issued_schedule"].as_array().unwrap().len(), 1);
    let mut execution = CallExecution::new(&artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let x = slot(&artifact, "x", "P");
    for input in [1.0, -1.0, 0.0, 1.0] {
        parameters[x] = input;
        execution.run(&parameters);
        let bytes = execution.lanes();
        for (name, expected) in [
            ("r.codes[1]", 9_007_199_254_740_993i64),
            ("r.codes[2]", -9_007_199_254_740_993),
        ] {
            let offset = lane(&artifact, name)["byte_offset"].as_u64().unwrap() as usize;
            assert_eq!(
                i64::from_le_bytes(bytes[offset..offset + 8].try_into().unwrap()),
                expected
            );
        }
        for (name, expected) in [("r.valid[1]", input > 0.0), ("r.valid[2]", input < 0.0)] {
            let offset = lane(&artifact, name)["byte_offset"].as_u64().unwrap() as usize;
            assert_eq!(bytes[offset], u8::from(expected));
        }
    }
}

/// Authored Integer fill stays exact inside the typed function owner, including
/// its native shared outputs; direct Real-register fills retain their refusal.
#[test]
fn integer_input_fill_keeps_every_exact_cell() {
    let _lock = session_test_guard();
    let source = r#"
record Codes
  Integer values[3];
end Codes;
function FillCodes
  input Integer code;
  output Codes result;
algorithm
  result.values := fill(code, 3);
end FillCodes;
model Filled
  input Integer code = 1;
  Codes result;
equation
  result = FillCodes(code);
end Filled;
"#;
    let exact = 9_007_199_254_740_993i64;
    crate::native_assignment_api::with_prepared_native_model(source, "Filled", |model, _, _| {
        let site = model
            .problem
            .discrete
            .rhs
            .programs()
            .iter()
            .flatten()
            .find_map(|op| {
                if let rumoca_ir_solve::LinearOp::PureCall { site, .. } = op {
                    Some(site)
                } else {
                    None
                }
            })
            .expect("the authored function has one canonical call");
        let input = rumoca_eval_solve::TypedValue::construct(
            site.inputs()[0].clone(),
            vec![rumoca_ir_solve::SolveValueKind::Integer(exact)],
        )
        .unwrap();
        let outputs =
            rumoca_eval_solve::eval_pure_call(&model.pure_calls, site.owner(), &[input]).unwrap();
        assert_eq!(
            outputs[0].elements(),
            &[rumoca_ir_solve::SolveValueKind::Integer(exact); 3]
        );
        Ok(String::new())
    })
    .unwrap();
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(source, "Filled").unwrap(),
    )
    .unwrap();
    let parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let mut execution = CallExecution::new(&artifact);
    execution.set_input("code", exact);
    execution.run(&parameters);
    for cell in 1..=3 {
        let offset = lane(&artifact, &format!("result.values[{cell}]"))["byte_offset"]
            .as_u64()
            .unwrap() as usize;
        assert_eq!(
            i64::from_le_bytes(execution.lanes()[offset..offset + 8].try_into().unwrap()),
            exact
        );
    }
    let direct = source.replace(
        "result = FillCodes(code);",
        "result.values = fill(code, 3);",
    );
    let refusal = crate::native_program_api::prepare_native_program(&direct, "Filled").unwrap_err();
    assert!(
        refusal
            .message()
            .contains("native evaluation has no structured discrete owners"),
        "{}",
        refusal.message()
    );
}

#[test]
fn nested_validation_reuses_aliased_tensor_reads_and_skips_disabled_rows() {
    let _lock = session_test_guard();
    let source = r#"
function ValidateWords
  input Real words[2,4];
  input Real enabled[2];
  output Boolean valid;
protected
  Real mean; Real energy;
algorithm
  valid := true; mean := 0.0; energy := 0.0;
  for word in 1:2 loop
    if enabled[word] == 1.0 then
      mean := 0.0; energy := 0.0;
      for component in 1:4 loop
        valid := valid and abs(words[word,component]) <= 8.0;
        mean := mean + words[word,component];
        energy := energy + words[word,component]*words[word,component];
      end for;
      valid := valid and mean <= 10.0 and energy <= 50.0;
    end if;
  end for;
end ValidateWords;
model WordValidation
  input Real words[2,4] = {{1,2,3,4},{5,6,7,8}};
  input Real enabled[2] = {1,0};
  output Boolean valid;
equation
  valid = ValidateWords(words, enabled);
end WordValidation;
"#;
    crate::native_assignment_api::with_prepared_native_model(
        source,
        "WordValidation",
        |model, _, _| {
            let site = model
                .problem
                .discrete
                .rhs
                .programs()
                .iter()
                .flatten()
                .find_map(|op| {
                    if let rumoca_ir_solve::LinearOp::PureCall { site, .. } = op {
                        Some(site)
                    } else {
                        None
                    }
                })
                .unwrap();
            for (enabled, expected) in [([1.0, 0.0], true), ([1.0, 1.0], false), ([0.0, 0.0], true)]
            {
                let inputs = [
                    vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0],
                    enabled.to_vec(),
                ]
                .into_iter()
                .zip(site.inputs())
                .map(|(values, ty)| {
                    rumoca_eval_solve::TypedValue::construct(
                        ty.clone(),
                        values.into_iter().map(fixture_real64).collect(),
                    )
                    .unwrap()
                })
                .collect::<Vec<_>>();
                let output =
                    rumoca_eval_solve::eval_pure_call(&model.pure_calls, site.owner(), &inputs)
                        .unwrap();
                assert_eq!(
                    output[0].elements(),
                    &[rumoca_ir_solve::SolveValueKind::Boolean(expected)]
                );
            }
            Ok(String::new())
        },
    )
    .unwrap();
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(source, "WordValidation").unwrap(),
    )
    .unwrap();
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let valid = lane(&artifact, "valid")["byte_offset"].as_u64().unwrap() as usize;
    let mut execution = CallExecution::new(&artifact);
    for (enabled, expected) in [([1.0, 0.0], 1), ([1.0, 1.0], 0), ([0.0, 0.0], 1)] {
        for (index, value) in enabled.into_iter().enumerate() {
            parameters[slot(&artifact, &format!("enabled[{}]", index + 1), "P")] = value;
        }
        execution.run(&parameters);
        assert_eq!(execution.lanes()[valid], expected);
    }
}

fn fixture_real64(value: f64) -> rumoca_ir_solve::SolveValueKind {
    rumoca_ir_solve::SolveValueKind::Real64(value.to_bits())
}
