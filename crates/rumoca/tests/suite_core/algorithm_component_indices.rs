//! MLS 11.1.2: component-instance indices identify distinct algorithm storage.
use rumoca::Compiler;

#[test]
fn component_array_algorithms_keep_distinct_discrete_coordinates() {
    let source = r#"
package ArrayAlgorithm
  model Cell
    discrete Integer state(start=0, fixed=true);
  algorithm
    when sample(0,1) then
      state := pre(state)+1;
    end when;
  end Cell;
  model Top
    Cell cells[2];
  end Top;
end ArrayAlgorithm;
"#;
    let compiled = Compiler::new()
        .model("ArrayAlgorithm.Top")
        .compile_str(source, "ArrayAlgorithm.mo")
        .expect("each cell algorithm owns its indexed coordinate");
    compiled.dae.inspect(|view| {
        let names = view
            .variables()
            .map(|(_, v)| v.name().as_str().to_owned())
            .collect::<Vec<_>>();
        for name in ["cells[1].state", "cells[2].state"] {
            assert!(
                names.iter().any(|actual| actual == name),
                "missing {name}: {names:?}"
            );
        }
        assert!(!names.iter().any(|name| name == "cells.state"));
    });
}

#[test]
fn nested_components_preserve_the_array_element_subscript() {
    let source = r#"
package NestedAlgorithm
  model Cell
    parameter Integer selected = 2;
    discrete Real state[2](each start=0, each fixed=true);
  algorithm
    when sample(0,1) then
      state := pre(state);
      state[selected] := pre(state[selected])+1;
    end when;
  end Cell;
  model Group
    Cell cells[2];
  end Group;
  model Top
    Group groups[2];
  end Top;
end NestedAlgorithm;
"#;
    let compiled = Compiler::new()
        .model("NestedAlgorithm.Top")
        .compile_str(source, "NestedAlgorithm.mo")
        .expect("component indices are consumed but the array subscript remains");
    let wire = serde_json::to_string(&compiled.dae).unwrap();
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("indexed owners retain their checked wire contract");
    decoded.inspect(|view| {
        let names = view
            .variables()
            .map(|(_, v)| v.name().as_str().to_owned())
            .collect::<Vec<_>>();
        for group in 1..=2 {
            for cell in 1..=2 {
                let name = format!("groups[{group}].cells[{cell}].state");
                assert!(names.contains(&name), "missing {name}: {names:?}");
            }
        }
    });
}

#[test]
fn component_array_call_outputs_keep_their_coordinates() {
    let source = r#"
package CallAlgorithm
  function bump
    input Integer a;
    input Integer b;
    output Integer x;
    output Integer y;
  algorithm
    x := a+1;
    y := b+2;
  end bump;
  model Cell
    discrete Integer x(start=0, fixed=true);
    discrete Integer y(start=0, fixed=true);
  algorithm
    when sample(0,1) then
      (x,y) := bump(pre(x),pre(y));
    end when;
  end Cell;
  model Top
    Cell cells[2];
  end Top;
end CallAlgorithm;
"#;
    let compiled = Compiler::new()
        .model("CallAlgorithm.Top")
        .compile_str(source, "CallAlgorithm.mo")
        .expect("each call writes the outputs of its own cell");
    let wire = serde_json::to_string(&compiled.dae).unwrap();
    let _: rumoca_compile::compile::Dae = serde_json::from_str(&wire).unwrap();
}

const CONDITIONAL_HOLD: &str = r#"
package ConditionalHold
  model Cell
    parameter Integer marker;
    Integer x = if time < 1 then marker else marker+1;
    Integer y(start=-1, fixed=true);
    discrete Real deadline;
  algorithm
    when {initial(), change(x) and not initial()} then
      deadline := if initial() then 1 else time+2;
      if x < 0 then
        y := x;
      end if;
    elsewhen time >= deadline then
      y := x;
    end when;
  end Cell;
  model Top
    Cell cells[3](marker={10,20,30});
  end Top;
end ConditionalHold;
"#;

/// MLS 11.2.7: an active earlier when branch wins even when its nested if
/// retains a target. Distinct cell inputs also check the actual RHS mapping.
#[test]
fn a_conditional_hold_blocks_a_simultaneous_elsewhen_per_cell() {
    check_conditional_hold(CONDITIONAL_HOLD);
}

#[test]
fn a_nested_elsewhen_cannot_overtake_an_earlier_conditional_hold() {
    let source = CONDITIONAL_HOLD.replace(
        "elsewhen time >= deadline then\n      y := x;",
        "elsewhen time >= deadline then\n      if x > 0 then\n        y := x;\n      end if;",
    );
    check_conditional_hold(&source);
}

#[test]
fn an_earlier_branch_without_a_target_write_still_blocks_elsewhen() {
    let source =
        CONDITIONAL_HOLD.replace("      if x < 0 then\n        y := x;\n      end if;\n", "");
    check_conditional_hold(&source);
}

fn check_conditional_hold(source: &str) {
    let compiled = Compiler::new()
        .model("ConditionalHold.Top")
        .compile_str(source, "ConditionalHold.mo")
        .expect("the cell-local conditional holds compile");
    let result = rumoca_sim::simulate_dae(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 4.0,
            ..rumoca_sim::SimOptions::default()
        },
    )
    .expect("the conditional hold model simulates");
    for cell in 1..=3 {
        let name = format!("cells[{cell}].y");
        let column = result
            .names
            .iter()
            .position(|value| value == &name)
            .unwrap();
        let mut checked_hold = false;
        let mut checked_update = false;
        for (time, actual) in result.times.iter().zip(&result.data[column]) {
            if *time > 1.0 + 1e-6 && *time < 3.0 - 1e-6 {
                assert_eq!(*actual, -1.0, "{name} must retain its value at {time}");
                checked_hold = true;
            } else if *time > 3.0 + 1e-6 {
                assert_eq!(*actual, f64::from(cell * 10 + 1), "{name} at {time}");
                checked_update = true;
            }
        }
        assert!(checked_hold && checked_update, "both intervals are sampled");
    }
}
