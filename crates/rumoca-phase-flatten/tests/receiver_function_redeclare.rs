//! MLS §7.3: a call through a component selects that instance's redeclare.
//! SPEC_0007 Stage 2 requires the selection to be applied before Flat escapes.

use rumoca_core::{DefId, Expression, ExpressionVisitor, Literal, OpBinary, Reference, Statement};
use rumoca_ir_ast as ast;

const SOURCE_NAME: &str = "<receiver_function_redeclare>";
const SOURCE: &str = r#"
package P
  partial function PartialF
    input Real x;
    output Real y;
  end PartialF;
  function Double
    extends PartialF;
  algorithm
    y := 2*x;
  end Double;
  function Triple
    extends PartialF;
  algorithm
    y := 3*x;
  end Triple;
  model World
    replaceable function F = Double constrainedby PartialF;
  end World;
  model Body
    outer World world;
    Real y;
  equation
    y = world.F(time + 1);
  end Body;
  model Direct
    World world(redeclare function F = Triple);
    Real y;
  equation
    y = world.F(time + 1);
  end Direct;
  model ThroughOuter
    inner World world(redeclare function F = Triple);
    Body body;
  end ThroughOuter;
  model Siblings
    World first(redeclare function F = Triple);
    World second;
    Real y1;
    Real y2;
  equation
    y1 = first.F(time + 1);
    y2 = second.F(time + 1);
  end Siblings;
  model Leaf
    World world(redeclare function F = Triple);
    Real y;
  equation
    y = world.F(time + 1);
  end Leaf;
  model Nested
    World world;
    Leaf first;
    Real y;
  equation
    y = world.F(time + 1);
  end Nested;
  model RepeatedPrefix
    Nested first;
  end RepeatedPrefix;
end P;
"#;

fn instantiate(model: &str) -> ast::InstancedTree {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    rumoca_phase_instantiate::instantiate(resolved, model).expect("source instantiates")
}

fn assert_overlay_selection(instanced: &ast::InstancedTree, receiver: &str) -> DefId {
    let triple = instanced.tree.get_def_id_by_name("P.Triple").unwrap();
    let slot = instanced.tree.get_def_id_by_name("P.World.F").unwrap();
    let instance = instanced
        .overlay
        .components
        .values()
        .find(|instance| instance.qualified_name.to_flat_string() == receiver)
        .expect("receiver has a concrete instance");
    let selected = instance
        .class_overrides
        .values()
        .find(|selected| selected.alias_def_id == slot)
        .expect("Instantiate records the exact function slot override");
    assert_eq!(selected.target_def_id, triple);
    triple
}

#[derive(Default)]
struct Calls(Vec<Reference>);

impl ExpressionVisitor for Calls {
    fn visit_function_call(&mut self, name: &Reference, args: &[Expression], constructor: bool) {
        self.0.push(name.clone());
        self.walk_function_call(name, args, constructor);
    }
}

struct FlattenedCalls {
    flat: rumoca_ir_flat::Model,
    calls: Vec<Reference>,
}

fn flatten_calls(mut instanced: ast::InstancedTree, model: &str) -> FlattenedCalls {
    rumoca_phase_typecheck::typecheck_instanced(&instanced.tree, &mut instanced.overlay, model)
        .expect("source typechecks");
    let flat = rumoca_phase_flatten::flatten_ref(&instanced.tree, &instanced.overlay, model)
        .expect("source flattens");
    let mut calls = Calls::default();
    for equation in &flat.equations {
        calls.visit_expression(&equation.residual);
    }
    FlattenedCalls {
        flat,
        calls: calls.0,
    }
}

fn assert_concrete_implementation(
    flat: &rumoca_ir_flat::Model,
    call: &Reference,
    expected_def_id: Option<DefId>,
    expected_multiplier: i64,
) {
    let resolved = call
        .resolved_function()
        .expect("call records its exact collected function instance");
    let implementation = flat
        .get_function_instance(resolved.instance_id)
        .expect("resolved function instance is collected exactly once");
    assert_eq!(
        implementation.def_id, expected_def_id,
        "call resolves to the expected implementation or exposed slot owner"
    );
    assert_eq!(
        implementation.name.as_str(),
        call.as_str(),
        "the collected implementation remains owned by this call occurrence"
    );
    assert!(
        !implementation.body.is_empty(),
        "the selected concrete implementation retains its executable body"
    );
    let [Statement::Assignment { value, .. }] = implementation.body.as_slice() else {
        panic!("the fixture implementation has one executable assignment");
    };
    let Expression::Binary {
        op: OpBinary::Mul,
        lhs,
        rhs,
        ..
    } = value
    else {
        panic!("the fixture implementation multiplies its input");
    };
    assert!(
        matches!(lhs.as_ref(), Expression::Literal { value: Literal::Integer(value), .. } if *value == expected_multiplier)
            || matches!(rhs.as_ref(), Expression::Literal { value: Literal::Integer(value), .. } if *value == expected_multiplier),
        "the selected body has the expected Double/Triple multiplier"
    );
}

#[test]
fn direct_receiver_selects_its_redeclared_implementation() {
    let model = "P.Direct";
    let instanced = instantiate(model);
    let triple = assert_overlay_selection(&instanced, "world");
    let flattened = flatten_calls(instanced, model);
    assert_eq!(flattened.calls.len(), 1);
    assert_eq!(flattened.calls[0].target_def_id(), Some(triple));
    assert_concrete_implementation(&flattened.flat, &flattened.calls[0], Some(triple), 3);
}

#[test]
fn outer_receiver_selects_the_inner_instances_redeclare() {
    let model = "P.ThroughOuter";
    let instanced = instantiate(model);
    let triple = assert_overlay_selection(&instanced, "world");
    let flattened = flatten_calls(instanced, model);
    assert_eq!(flattened.calls.len(), 1);
    assert_eq!(flattened.calls[0].target_def_id(), Some(triple));
    assert_concrete_implementation(&flattened.flat, &flattened.calls[0], Some(triple), 3);
}

#[test]
fn sibling_instances_do_not_share_function_redeclarations() {
    let model = "P.Siblings";
    let instanced = instantiate(model);
    let triple = assert_overlay_selection(&instanced, "first");
    let slot = instanced.tree.get_def_id_by_name("P.World.F").unwrap();
    let flattened = flatten_calls(instanced, model);
    assert_eq!(flattened.calls.len(), 2);
    assert_eq!(flattened.calls[0].target_def_id(), Some(triple));
    assert_eq!(flattened.calls[1].target_def_id(), Some(slot));
    assert_concrete_implementation(&flattened.flat, &flattened.calls[0], Some(triple), 3);
    assert_concrete_implementation(&flattened.flat, &flattened.calls[1], Some(slot), 2);
}

#[test]
fn qualified_receiver_paths_do_not_select_a_same_spelled_nested_receiver() {
    let model = "P.RepeatedPrefix";
    let instanced = instantiate(model);
    let triple = assert_overlay_selection(&instanced, "first.first.world");
    let slot = instanced.tree.get_def_id_by_name("P.World.F").unwrap();
    let flattened = flatten_calls(instanced, model);
    assert_eq!(flattened.calls.len(), 2);
    assert_eq!(
        flattened
            .calls
            .iter()
            .filter(|call| call.target_def_id() == Some(triple))
            .count(),
        1
    );
    assert_eq!(flattened.calls[1].target_def_id(), Some(slot));
    assert_concrete_implementation(&flattened.flat, &flattened.calls[0], Some(triple), 3);
    assert_concrete_implementation(&flattened.flat, &flattened.calls[1], Some(slot), 2);
}
