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
