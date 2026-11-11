//! MLS §7.2/§7.3: inherited component modifications must retain redeclarations.
use crate::instantiate_model;
use rumoca_ir_ast as ast;

const SOURCE: &str = r#"
partial model ValueInterface
  Real x;
end ValueInterface;
model DefaultValue
  extends ValueInterface;
equation
  x=1;
end DefaultValue;
model ReplacementValue
  extends ValueInterface;
equation
  x=2;
end ReplacementValue;
model Host
  replaceable DefaultValue item constrainedby ValueInterface;
  model ReplacementValue
    extends ValueInterface;
  equation
    x=3;
  end ReplacementValue;
end Host;
model Base
  Host h;
end Base;
model Direct
  Host h(redeclare ReplacementValue item);
end Direct;
model Inherited
  extends Base(h(redeclare ReplacementValue item));
end Inherited;
model Grandparent
  extends Base;
end Grandparent;
model GrandInherited
  extends Grandparent(h(redeclare ReplacementValue item));
end GrandInherited;
model ArrayBase
  Host h[2];
end ArrayBase;
model ArrayInherited
  extends ArrayBase(h(redeclare each ReplacementValue item));
end ArrayInherited;
model SelectedBase
  Host h(redeclare ReplacementValue item);
end SelectedBase;
model OverrideSelected
  extends SelectedBase(h(redeclare DefaultValue item));
end OverrideSelected;
model ModifySelected
  extends SelectedBase(h(item(x(start=4))));
end ModifySelected;
model SeparateInstances
  Inherited changed;
  Base unchanged;
end SeparateInstances;
model InvalidValue
  Real other;
end InvalidValue;
model BadConstraint
  extends Base(h(redeclare InvalidValue item));
end BadConstraint;
model FinalHost
  final replaceable DefaultValue item constrainedby ValueInterface;
end FinalHost;
model FinalBase
  FinalHost h;
end FinalBase;
model BadFinal
  extends FinalBase(h(redeclare ReplacementValue item));
end BadFinal;
"#;

fn resolved() -> ast::ClassTree {
    let file = "NestedRedeclare.mo";
    let parsed = rumoca_phase_parse::parse_to_ast(SOURCE, file).unwrap();
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map.add(file, SOURCE);
    rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .unwrap()
        .into_inner()
}

#[test]
fn inherited_nested_component_redeclare_selects_replacement() {
    let tree = resolved();
    let expected = tree
        .get_class_by_qualified_name("ReplacementValue")
        .unwrap()
        .def_id;
    for model in ["Direct", "Inherited", "GrandInherited"] {
        let overlay = instantiate_model(&tree, model).unwrap();
        let selected = overlay
            .classes
            .values()
            .find(|class| class.qualified_name.to_flat_string() == "h.item")
            .expect("the component class occurrence must be instantiated");
        assert_eq!(
            selected.class_def_id, expected,
            "{model}: the replacement class must own the instantiated component"
        );
    }
}

fn assert_selected(tree: &ast::ClassTree, model: &str, occurrence: &str, expected: &str) {
    let overlay = instantiate_model(tree, model).unwrap();
    let expected = tree.get_class_by_qualified_name(expected).unwrap().def_id;
    let selected = overlay
        .classes
        .values()
        .find(|class| class.qualified_name.to_flat_string() == occurrence)
        .unwrap();
    assert_eq!(selected.class_def_id, expected, "{model}: {occurrence}");
}

#[test]
fn inherited_nested_redeclare_overrides_declaration_modifier() {
    assert_selected(&resolved(), "OverrideSelected", "h.item", "DefaultValue");
}

#[test]
fn inherited_nested_redeclare_does_not_leak_to_another_instance() {
    let tree = resolved();
    assert_selected(
        &tree,
        "SeparateInstances",
        "changed.h.item",
        "ReplacementValue",
    );
    assert_selected(
        &tree,
        "SeparateInstances",
        "unchanged.h.item",
        "DefaultValue",
    );
}

#[test]
fn inherited_nested_redeclare_preserves_constraint_rejection() {
    let error = instantiate_model(&resolved(), "BadConstraint")
        .unwrap_err()
        .to_string();
    assert!(error.contains("constrain"), "{error}");
}

#[test]
fn inherited_nested_redeclare_preserves_final_rejection() {
    let error = instantiate_model(&resolved(), "BadFinal")
        .unwrap_err()
        .to_string();
    assert!(error.contains("final"), "{error}");
}

#[test]
fn inherited_value_modifier_keeps_the_selected_type() {
    assert_selected(&resolved(), "ModifySelected", "h.item", "ReplacementValue");
}

#[test]
fn inherited_each_redeclare_selects_each_array_instance() {
    let tree = resolved();
    for occurrence in ["h[1].item", "h[2].item"] {
        assert_selected(&tree, "ArrayInherited", occurrence, "ReplacementValue");
    }
}
