//! MLS §7.3: a call through a component selects that instance's redeclare.
//! SPEC_0007 Stage 2 requires the selection to be applied before Flat escapes.

use rumoca_core::{DefId, Expression, ExpressionVisitor, Reference};
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

fn flatten_calls(mut instanced: ast::InstancedTree, model: &str) -> Vec<Reference> {
    rumoca_phase_typecheck::typecheck_instanced(&instanced.tree, &mut instanced.overlay, model)
        .expect("source typechecks");
    let flat = rumoca_phase_flatten::flatten_ref(&instanced.tree, &instanced.overlay, model)
        .expect("source flattens");
    let mut calls = Calls::default();
    for equation in &flat.equations {
        calls.visit_expression(&equation.residual);
    }
    calls.0
}

#[test]
fn direct_receiver_selects_its_redeclared_implementation() {
    let model = "P.Direct";
    let instanced = instantiate(model);
    let triple = assert_overlay_selection(&instanced, "world");
    let calls = flatten_calls(instanced, model);
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].target_def_id(), Some(triple));
}

#[test]
fn outer_receiver_selects_the_inner_instances_redeclare() {
    let model = "P.ThroughOuter";
    let instanced = instantiate(model);
    let triple = assert_overlay_selection(&instanced, "world");
    let calls = flatten_calls(instanced, model);
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].target_def_id(), Some(triple));
}

#[test]
fn sibling_instances_do_not_share_function_redeclarations() {
    let model = "P.Siblings";
    let instanced = instantiate(model);
    let triple = assert_overlay_selection(&instanced, "first");
    let double = instanced.tree.get_def_id_by_name("P.Double").unwrap();
    let calls = flatten_calls(instanced, model);
    assert_eq!(calls.len(), 2);
    assert_eq!(calls[0].target_def_id(), Some(triple));
    assert_eq!(calls[1].target_def_id(), Some(double));
}

#[test]
fn qualified_receiver_paths_do_not_select_a_same_spelled_nested_receiver() {
    let model = "P.RepeatedPrefix";
    let instanced = instantiate(model);
    let triple = assert_overlay_selection(&instanced, "first.first.world");
    let double = instanced.tree.get_def_id_by_name("P.Double").unwrap();
    let calls = flatten_calls(instanced, model);
    assert_eq!(calls.len(), 2);
    assert_eq!(
        calls
            .iter()
            .filter(|call| call.target_def_id() == Some(triple))
            .count(),
        1
    );
    assert_eq!(
        calls
            .iter()
            .filter(|call| call.target_def_id() == Some(double))
            .count(),
        1
    );
}
