//! MLS §7.2.6: a final modifier and a final declaration retain the same property.

use super::*;

const SOURCE: &str = r#"
model Cell
  parameter Real c = 1;
end Cell;
model Declared
  final parameter Real c = 0;
end Declared;
model Root
  parameter Real gain = 2;
  Declared declared;
  Cell modified(final c = 0);
  Cell ordinary(c = 0);
  Cell dependent(final c = gain);
  Cell cells[2](each final c = 0);
  final Cell frozen(c = 0);
end Root;
"#;

fn instantiate() -> ast::InstanceOverlay {
    let parsed = rumoca_phase_parse::parse_to_ast(SOURCE, "FinalModifier.mo").unwrap();
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map.add("FinalModifier.mo", SOURCE);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).unwrap();
    let tree = resolved.into_inner();
    match instantiate_model_with_outcome(&tree, "Root") {
        InstantiationOutcome::Success(overlay) => overlay,
        _ => panic!("valid final modifiers must instantiate"),
    }
}

fn component<'a>(overlay: &'a ast::InstanceOverlay, name: &str) -> &'a ast::InstanceData {
    overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == name)
        .unwrap_or_else(|| panic!("missing {name}"))
}

#[test]
fn final_binding_modifier_retains_the_declarations_final_property() {
    let overlay = instantiate();
    let declared = component(&overlay, "declared.c");
    let modified = component(&overlay, "modified.c");
    assert!(declared.is_final && declared.evaluate);
    assert!(
        modified.is_final,
        "a final modifier must survive instantiation"
    );
    assert_eq!(modified.evaluate, declared.evaluate);
}

#[test]
fn each_final_modifier_reaches_each_component_instance() {
    let overlay = instantiate();
    for name in ["cells[1].c", "cells[2].c"] {
        let data = component(&overlay, name);
        assert!(data.is_final, "{name} lost its final modifier");
        assert!(data.evaluate);
    }
}

#[test]
fn final_modifier_retains_its_symbolic_parent_dependency() {
    let overlay = instantiate();
    let data = component(&overlay, "dependent.c");
    assert!(data.is_final);
    assert_eq!(data.binding_source.as_ref().unwrap().to_string(), "gain");
}

#[test]
fn ordinary_siblings_do_not_inherit_another_instances_final_modifier() {
    let overlay = instantiate();
    for name in ["ordinary.c", "gain"] {
        let data = component(&overlay, name);
        assert!(!data.is_final);
        assert!(!data.evaluate);
    }
}

#[test]
fn members_of_a_final_component_remain_final() {
    let overlay = instantiate();
    assert!(component(&overlay, "frozen").is_final);
    assert!(component(&overlay, "frozen.c").is_final);
}
