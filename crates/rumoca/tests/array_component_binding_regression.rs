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

const TWO_DIMENSIONAL_COMPONENT_ARRAY_NESTED_EQUATIONS: &str = r#"
partial model BaseDynamicLeaf
  Real x(start=0, fixed=true);
  Real y;
equation
  der(x) = 1;
  y = x;
end BaseDynamicLeaf;

model DynamicLeaf
  extends BaseDynamicLeaf;
end DynamicLeaf;

partial model BaseCellWithNestedEquations
  replaceable BaseDynamicLeaf cell;
end BaseCellWithNestedEquations;

model CellWithNestedEquations
  extends BaseCellWithNestedEquations(redeclare DynamicLeaf cell);
end CellWithNestedEquations;

partial model BaseTwoDimensionalComponentArrayNestedEquations
  replaceable BaseCellWithNestedEquations cell[2,2];
end BaseTwoDimensionalComponentArrayNestedEquations;

model StackWithNestedEquations
  extends BaseTwoDimensionalComponentArrayNestedEquations(
    redeclare CellWithNestedEquations cell);
end StackWithNestedEquations;

model TwoDimensionalComponentArrayNestedEquations
  StackWithNestedEquations stack;
end TwoDimensionalComponentArrayNestedEquations;
"#;

#[test]
fn two_dimensional_component_array_qualifies_nested_equations_per_element() {
    let compiled = Compiler::new()
        .model("TwoDimensionalComponentArrayNestedEquations")
        .compile_str(
            TWO_DIMENSIONAL_COMPONENT_ARRAY_NESTED_EQUATIONS,
            "array_component_nested_equations.mo",
        )
        .expect("nested equations in a component array must bind to each scalar instance");

    for name in [
        "stack.cell[1,1].cell.x",
        "stack.cell[1,2].cell.x",
        "stack.cell[2,1].cell.x",
        "stack.cell[2,2].cell.x",
    ] {
        assert!(
            compiled
                .dae
                .variables
                .states
                .contains_key(&rumoca_core::VarName::new(name)),
            "nested derivative must classify {name} as a state"
        );
    }
}
