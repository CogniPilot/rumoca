use rumoca::Compiler;

const TWO_DIMENSIONAL_COMPONENT_ARRAY: &str = r#"
block Source
  parameter Real p;
  output Real y;
equation
  y = p;
end Source;

partial model Icon
  parameter Real display;
end Icon;

partial model BaseCell
  extends Icon(final display=y);
  parameter Real p;
  Source source(p=p);
  output Real y = source.y;
end BaseCell;

model ConcreteCell
  extends BaseCell;
end ConcreteCell;

partial model BaseStack
  parameter Integer n = 2;
  parameter Integer m = 2;
  parameter Real q[n,m] = [1, 2; 3, 4];
  replaceable BaseCell cell[n,m](p=q, y(start=q));
end BaseStack;

model TwoDimensionalComponentArray
  extends BaseStack(redeclare ConcreteCell cell(p=q, y(start=q)));
end TwoDimensionalComponentArray;
"#;

#[test]
fn two_dimensional_component_array_retains_nested_declaration_bindings() {
    let compiled = Compiler::new()
        .model("TwoDimensionalComponentArray")
        .compile_str(
            TWO_DIMENSIONAL_COMPONENT_ARRAY,
            "array_component_binding.mo",
        )
        .expect("two-dimensional component-array declaration bindings must balance");

    for name in ["cell[1,1].y", "cell[1,2].y", "cell[2,1].y", "cell[2,2].y"] {
        let binding = compiled
            .flat
            .variables
            .get(&rumoca_core::VarName::new(name))
            .and_then(|variable| variable.binding.as_ref());
        assert!(binding.is_some(), "missing declaration binding for {name}");
    }
}
