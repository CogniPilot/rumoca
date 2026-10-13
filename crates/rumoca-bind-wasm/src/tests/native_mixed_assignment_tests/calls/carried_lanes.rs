//! Integer and Boolean state is published into the input lanes that hold its
//! previous value (SPEC_0040 SOLVE-C101): the host moves nothing between steps.

use super::*;

const SOURCE: &str = r#"
record Tally
  Integer count[600];
  Integer small[2];
  Boolean seen[3];
end Tally;

function Advance
  input Tally previous;
  input Integer bump;
  output Tally next;
algorithm
  for i in 1:600 loop
    next.count[i] := previous.count[i] + bump;
  end for;
  for i in 1:2 loop
    next.small[i] := previous.small[i];
  end for;
  for i in 1:3 loop
    next.seen[i] := if i == 3 then not previous.seen[i] else previous.seen[i];
  end for;
end Advance;

model Counter
  input Tally previous;
  input Integer bump = 1;
  output Tally next;
equation
  next = Advance(previous, bump);
end Counter;
"#;

const PAIRS: &str = r#"[["next.count", "previous.count"], ["next.small", "previous.small"], ["next.seen", "previous.seen"]]"#;

struct Lanes {
    artifact: serde_json::Value,
}

impl Lanes {
    fn new(json: &str) -> Self {
        Self {
            artifact: serde_json::from_str(json).unwrap(),
        }
    }

    /// Byte offset, from the start of its lane region, of the scalar `name` in
    /// `table` (`derived_outputs` or `input_lanes`).
    fn offset(&self, table: &str, name: &str) -> (usize, String) {
        let entry = self.artifact[table]
            .as_array()
            .unwrap()
            .iter()
            .find(|entry| entry["name"] == name)
            .unwrap_or_else(|| panic!("{name} is in {table}"));
        (
            entry["byte_offset"].as_u64().unwrap() as usize,
            entry["representation"].as_str().unwrap().to_owned(),
        )
    }

    fn scalars() -> Vec<String> {
        let mut names = (1..=600).map(|i| format!("count[{i}]")).collect::<Vec<_>>();
        names.extend((1..=2).map(|i| format!("small[{i}]")));
        names.extend((1..=3).map(|i| format!("seen[{i}]")));
        names
    }
}

impl CallExecution {
    /// Take the input lanes the module left as the next call's inputs.
    fn adopt_input_lanes(&mut self) {
        let mut bytes = vec![0; self.inputs.len()];
        self.memory
            .read(&self.store, self.typed, &mut bytes)
            .unwrap();
        self.inputs = bytes;
    }
}

fn start(execution: &mut CallExecution) {
    for (index, name) in Lanes::scalars().iter().enumerate() {
        let name = format!("previous.{name}");
        execution.set_input(
            &name,
            if name.contains("seen") {
                1
            } else {
                index as i64
            },
        );
    }
    execution.set_input("bump", 2);
}

#[test]
fn carried_lanes_publish_the_output_lanes_into_the_input_lanes() {
    let _lock = session_test_guard();
    let plain =
        Lanes::new(&crate::native_program_api::prepare_native_program(SOURCE, "Counter").unwrap());
    let carrying = Lanes::new(
        &crate::native_program_api::prepare_native_program_carrying(SOURCE, "Counter", PAIRS)
            .unwrap(),
    );
    let runs = carrying.artifact["abi"]["carried_lanes"]
        .as_array()
        .unwrap();
    let carried_bytes: u64 = runs.iter().map(|run| run["bytes"].as_u64().unwrap()).sum();
    assert_eq!(carried_bytes, (602 * 8 + 3) as u64);
    assert_eq!(carrying.artifact["abi"]["p_readonly"], false);

    let mut expected = CallExecution::new(&plain.artifact);
    start(&mut expected);
    let (status, _) = expected.run_typed(
        &plain.artifact["parameters"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_f64().unwrap())
            .collect::<Vec<_>>(),
    );
    assert_eq!(status, 0);
    let published = expected.lanes();

    let mut actual = CallExecution::new(&carrying.artifact);
    start(&mut actual);
    let parameters = carrying.artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_f64().unwrap())
        .collect::<Vec<_>>();
    let (status, _) = actual.run_typed(&parameters);
    assert_eq!(status, 0);
    actual.adopt_input_lanes();
    let untouched = actual.lanes();
    for name in Lanes::scalars() {
        let (from, kind) = plain.offset("derived_outputs", &format!("next.{name}"));
        let (to, _) = carrying.offset("input_lanes", &format!("previous.{name}"));
        let (out, _) = carrying.offset("derived_outputs", &format!("next.{name}"));
        let width = if kind == "u8" { 1 } else { 8 };
        assert_eq!(
            &actual.inputs[to..to + width],
            &published[from..from + width],
            "{name}"
        );
        assert!(
            untouched[out..out + width].iter().all(|byte| *byte == 0),
            "carried output lane {name} is not written"
        );
    }
}

#[test]
fn steps_chained_through_the_input_lanes_equal_steps_with_the_state_moved_by_the_host() {
    let _lock = session_test_guard();
    let plain =
        Lanes::new(&crate::native_program_api::prepare_native_program(SOURCE, "Counter").unwrap());
    let carrying = Lanes::new(
        &crate::native_program_api::prepare_native_program_carrying(SOURCE, "Counter", PAIRS)
            .unwrap(),
    );
    let parameters = |lanes: &Lanes| {
        lanes.artifact["parameters"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_f64().unwrap())
            .collect::<Vec<_>>()
    };
    let mut moved = CallExecution::new(&plain.artifact);
    let mut in_place = CallExecution::new(&carrying.artifact);
    start(&mut moved);
    start(&mut in_place);
    for step in 0..3 {
        let (status, _) = moved.run_typed(&parameters(&plain));
        assert_eq!(status, 0);
        let outputs = moved.lanes();
        for name in Lanes::scalars() {
            let (from, kind) = plain.offset("derived_outputs", &format!("next.{name}"));
            let (to, _) = plain.offset("input_lanes", &format!("previous.{name}"));
            let width = if kind == "u8" { 1 } else { 8 };
            moved.inputs[to..to + width].copy_from_slice(&outputs[from..from + width]);
        }
        let (status, _) = in_place.run_typed(&parameters(&carrying));
        assert_eq!(status, 0);
        in_place.adopt_input_lanes();
        for name in Lanes::scalars() {
            let (at, kind) = plain.offset("input_lanes", &format!("previous.{name}"));
            let (here, _) = carrying.offset("input_lanes", &format!("previous.{name}"));
            let width = if kind == "u8" { 1 } else { 8 };
            assert_eq!(
                &in_place.inputs[here..here + width],
                &moved.inputs[at..at + width],
                "step {step} {name}"
            );
        }
    }
}

#[test]
fn a_typed_declaration_that_is_not_a_lane_pair_is_refused() {
    let _lock = session_test_guard();
    let refused = |pairs: &str| {
        crate::native_program_api::prepare_native_program_carrying(SOURCE, "Counter", pairs)
            .map(|_| ())
            .map_err(|error| error.to_string())
    };
    assert!(
        refused(r#"[["next.count", "bump"]]"#)
            .unwrap_err()
            .contains("shape")
    );
    assert!(
        refused(r#"[["next.small", "previous.seen"]]"#)
            .unwrap_err()
            .contains("shape")
    );
    assert!(
        refused(r#"[["next.seen", "previous.seen"], ["next.seen", "previous.seen"]]"#)
            .unwrap_err()
            .contains("overlap")
    );
}

#[test]
fn an_integer_input_only_call_arguments_read_has_no_real_view() {
    let _lock = session_test_guard();
    let views = crate::native_assignment_api::with_prepared_native_model(
        SOURCE,
        "Counter",
        |model, _, _| {
            let schedule = crate::native_assignment_api::checked_native_schedule(model)?;
            Ok(schedule
                .input_runs()
                .iter()
                .map(|run| {
                    format!(
                        "{:?}:{}:{}",
                        run.lane(),
                        run.scalar_count(),
                        run.view_is_read()
                    )
                })
                .collect::<Vec<_>>()
                .join(" "))
        },
    )
    .unwrap();
    assert!(views.contains("Integer:600:false"), "{views}");
    assert!(views.contains("Boolean:3:true"), "{views}");
}
