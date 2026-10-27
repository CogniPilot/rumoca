//! MLS 7.2.5/7.2.6: nested extends modifiers retain their each/final prefixes.
use crate::{InstantiationOutcome, instantiate_model_with_outcome};
use rumoca_ir_ast as ast;

const SOURCE: &str = r#"
model Cell
  parameter Real coefficients[3];
end Cell;
model Base
  Cell cells[3];
  Cell distributed[3];
end Base;
model Top
  extends Base(
    cells(each coefficients={1,0,0}),
    distributed(coefficients={{1,2,3},{4,5,6},{7,8,9}}));
  Cell direct[3](each coefficients={1,0,0});
  Cell directDistributed[3](coefficients={{1,2,3},{4,5,6},{7,8,9}});
end Top;
"#;

fn instantiate(source: &str) -> ast::InstanceOverlay {
    let parsed = rumoca_phase_parse::parse_to_ast(source, "NestedPrefixes.mo").unwrap();
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map.add("NestedPrefixes.mo", source);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).unwrap();
    match instantiate_model_with_outcome(&resolved.into_inner(), "Top") {
        InstantiationOutcome::Success(overlay) => overlay,
        outcome => panic!("nested prefixes must instantiate: {outcome:?}"),
    }
}

fn component<'a>(overlay: &'a ast::InstanceOverlay, name: &str) -> &'a ast::InstanceData {
    overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == name)
        .unwrap_or_else(|| panic!("missing {name}"))
}

fn binding(data: &ast::InstanceData) -> String {
    data.binding
        .as_ref()
        .expect("parameter binding")
        .to_string()
}

#[test]
fn nested_extends_each_keeps_the_whole_array_per_instance() {
    let overlay = instantiate(SOURCE);
    for index in 1..=3 {
        let inherited = component(&overlay, &format!("cells[{index}].coefficients"));
        let direct = component(&overlay, &format!("direct[{index}].coefficients"));
        assert!(matches!(
            direct.binding.as_ref(),
            Some(ast::Expression::Array { .. })
        ));
        assert_eq!(binding(inherited), binding(direct), "cell {index}");
    }
}

#[test]
fn nested_extends_without_each_still_distributes_array_rows() {
    let overlay = instantiate(SOURCE);
    for index in 1..=3 {
        let inherited = component(&overlay, &format!("distributed[{index}].coefficients"));
        let direct = component(
            &overlay,
            &format!("directDistributed[{index}].coefficients"),
        );
        assert_eq!(binding(inherited), binding(direct), "row {index}");
    }
}

#[test]
fn nested_extends_each_final_retains_both_prefixes() {
    let overlay = instantiate(&SOURCE.replace("each coefficients=", "each final coefficients="));
    for index in 1..=3 {
        let inherited = component(&overlay, &format!("cells[{index}].coefficients"));
        let direct = component(&overlay, &format!("direct[{index}].coefficients"));
        assert!(inherited.is_final, "cell {index} lost final");
        assert_eq!(binding(inherited), binding(direct), "cell {index}");
    }
}

#[test]
fn enclosing_each_reaches_nested_binding() {
    let source = SOURCE.replace(
        "cells(each coefficients={1,0,0})",
        "each cells(coefficients={1,0,0})",
    );
    let overlay = instantiate(&source);
    for index in 1..=3 {
        assert_eq!(
            binding(component(&overlay, &format!("cells[{index}].coefficients"))),
            binding(component(
                &overlay,
                &format!("direct[{index}].coefficients")
            )),
        );
    }
}

#[test]
fn later_non_each_modifier_replaces_earlier_each_prefix() {
    let source = SOURCE
        .replace("model Top", "model Intermediate")
        .replace("end Top;", "end Intermediate;")
        + r#"
model Top
  extends Intermediate(cells(coefficients={{1,2,3},{4,5,6},{7,8,9}}));
end Top;
"#;
    let overlay = instantiate(&source);
    for index in 1..=3 {
        assert_eq!(
            binding(component(&overlay, &format!("cells[{index}].coefficients"))),
            binding(component(
                &overlay,
                &format!("directDistributed[{index}].coefficients")
            )),
        );
    }
}
