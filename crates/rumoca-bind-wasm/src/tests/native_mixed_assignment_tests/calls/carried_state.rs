//! A step's next state is published into the cells that hold its previous
//! state (SPEC_0040 SOLVE-C101): the host moves nothing between steps, and a
//! state the step returns unchanged is already in place.

use super::*;

const SOURCE: &str = r#"
record State
  Real x[1024];
  Real k;
end State;

function Advance
  input State previous;
  input Real flag;
  output State next;
algorithm
  next := previous;
  if flag > 0.5 then
    next.x := 2.0 * previous.x;
    next.k := previous.k + 1.0;
  end if;
end Advance;

model Stepper
  input State previous;
  input Real flag = 0;
  output State next;
equation
  next = Advance(previous, flag);
end Stepper;
"#;

const CELLS: usize = 1024;

const PAIRS: &str = r#"[["next.x", "previous.x"], ["next.k", "previous.k"]]"#;

struct Prepared {
    artifact: serde_json::Value,
    layout: rumoca_ir_solve::VarLayout,
}

impl Prepared {
    fn new(json: &str) -> Self {
        let artifact: serde_json::Value = serde_json::from_str(json).unwrap();
        let layout = serde_json::from_value(artifact["var_layout"].clone()).unwrap();
        Self { artifact, layout }
    }

    fn index(&self, name: &str) -> usize {
        match self.layout.binding(name).unwrap() {
            rumoca_ir_solve::ScalarSlot::Y { index, .. }
            | rumoca_ir_solve::ScalarSlot::P { index, .. } => index,
            _ => panic!("{name} is a scalar slot"),
        }
    }

    fn parameters(&self, flag: f64) -> Vec<f64> {
        let mut parameters = self.artifact["parameters"]
            .as_array()
            .unwrap()
            .iter()
            .map(|value| value.as_f64().unwrap())
            .collect::<Vec<_>>();
        parameters[self.index("flag")] = flag;
        parameters[self.index("previous.k")] = 7.0;
        for cell in 0..CELLS {
            parameters[self.index(&format!("previous.x[{}]", cell + 1))] = cell as f64 + 1.0;
        }
        parameters
    }
}

#[test]
fn the_next_state_replaces_the_previous_state_in_p_and_y_is_not_written() {
    let _lock = session_test_guard();
    let plain = Prepared::new(
        &crate::native_program_api::prepare_native_program(SOURCE, "Stepper").unwrap(),
    );
    let carrying = Prepared::new(
        &crate::native_program_api::prepare_native_program_carrying(SOURCE, "Stepper", PAIRS)
            .unwrap(),
    );
    assert_eq!(plain.artifact["abi"]["p_readonly"], true);
    assert_eq!(carrying.artifact["abi"]["p_readonly"], false);
    let runs = carrying.artifact["abi"]["carried"].as_array().unwrap();
    assert_eq!(runs.len(), 2, "next.x and next.k are one run each");
    for flag in [0.0, 1.0] {
        let (status, expected) =
            CallExecution::new(&plain.artifact).run_typed(&plain.parameters(flag));
        assert_eq!(status, 0);
        let mut execution = CallExecution::new(&carrying.artifact);
        let parameters = carrying.parameters(flag);
        let (status, y) = execution.run_typed(&parameters);
        assert_eq!(status, 0);
        let host_p = execution.host_p(parameters.len());
        for cell in 0..CELLS {
            let (next, previous) = (
                format!("next.x[{}]", cell + 1),
                format!("previous.x[{}]", cell + 1),
            );
            assert_eq!(
                host_p[carrying.index(&previous)],
                expected[plain.index(&next)],
                "flag {flag} cell {cell}"
            );
            assert_eq!(y[carrying.index(&next)], 0.0, "carried Y is not written");
        }
        assert_eq!(
            host_p[carrying.index("previous.k")],
            expected[plain.index("next.k")]
        );
        assert_eq!(host_p[carrying.index("flag")], flag);
    }
}

#[test]
fn a_step_chained_through_p_equals_the_same_steps_with_the_state_moved_by_the_host() {
    let _lock = session_test_guard();
    let plain = Prepared::new(
        &crate::native_program_api::prepare_native_program(SOURCE, "Stepper").unwrap(),
    );
    let carrying = Prepared::new(
        &crate::native_program_api::prepare_native_program_carrying(SOURCE, "Stepper", PAIRS)
            .unwrap(),
    );
    let mut moved = plain.parameters(1.0);
    let mut in_place = CallExecution::new(&carrying.artifact);
    let mut p = carrying.parameters(1.0);
    for step in 0..3 {
        let (status, y) = CallExecution::new(&plain.artifact).run_typed(&moved);
        assert_eq!(status, 0);
        for cell in 0..CELLS {
            moved[plain.index(&format!("previous.x[{}]", cell + 1))] =
                y[plain.index(&format!("next.x[{}]", cell + 1))];
        }
        moved[plain.index("previous.k")] = y[plain.index("next.k")];
        let (status, _) = in_place.run_typed(&p);
        assert_eq!(status, 0);
        p = in_place.host_p(p.len());
        for cell in 0..CELLS {
            let name = format!("previous.x[{}]", cell + 1);
            assert_eq!(
                p[carrying.index(&name)],
                moved[plain.index(&name)],
                "step {step}"
            );
        }
        assert_eq!(p[carrying.index("previous.k")], 7.0 + (step + 1) as f64);
    }
}

#[test]
fn a_declaration_naming_other_variables_than_a_real_output_and_input_is_refused() {
    let _lock = session_test_guard();
    let refused = |pairs: &str| {
        crate::native_program_api::prepare_native_program_carrying(SOURCE, "Stepper", pairs)
            .map(|_| ())
            .map_err(|error| error.to_string())
    };
    assert!(
        refused(r#"[["next.x", "previous.k"]]"#)
            .unwrap_err()
            .contains("shape")
    );
    assert!(
        refused(r#"[["previous.x", "next.x"]]"#)
            .unwrap_err()
            .contains("Y output and a P input")
    );
    assert!(
        refused(r#"[["next.k", "nothing"]]"#)
            .unwrap_err()
            .contains("Y output and a P input")
    );
    assert!(
        refused(r#"[["next.k", "flag"], ["next.x", "previous.x"], ["next.k", "previous.k"]]"#)
            .unwrap_err()
            .contains("overlap")
    );
    assert!(refused("next.").unwrap_err().contains("[output, input]"));
}
