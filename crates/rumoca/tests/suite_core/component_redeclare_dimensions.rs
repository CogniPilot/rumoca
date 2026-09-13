//! Component redeclarations must be applied before structured array expansion.

use rumoca_ir_ast as ast;

fn instantiate(source: &str) -> (ast::ClassTree, ast::InstanceOverlay) {
    let parsed = rumoca_phase_parse::parse_to_ast(source, "component_redeclare_dimensions.mo")
        .expect("parse component redeclaration");
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map
        .add("component_redeclare_dimensions.mo", source);
    let tree = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .expect("resolve component redeclaration")
        .into_inner();
    let overlay = rumoca_phase_instantiate::instantiate_model(&tree, "Root")
        .expect("instantiate component redeclaration");
    (tree, overlay)
}

const TYPES: &str = r#"
record DriveData
  parameter Real wMax = asin(0.5);
end DriveData;
record AlternateDriveData
  extends DriveData;
  parameter Real acceleration = 2;
end AlternateDriveData;
model Holder
  parameter Integer k = 5;
  replaceable parameter DriveData driveData constrainedby DriveData;
end Holder;
"#;

#[test]
fn component_redeclaration_replaces_type_and_shape_before_expansion() {
    let source = format!(
        r#"{TYPES}
model Root
  Holder modified(redeclare AlternateDriveData driveData[2]);
  Holder unchanged;
end Root;
"#
    );
    let (tree, overlay) = instantiate(&source);
    let replacement = tree
        .get_class_by_qualified_name("AlternateDriveData")
        .unwrap()
        .def_id;
    let records: Vec<_> = overlay
        .components
        .values()
        .filter(|data| data.type_def_id == replacement)
        .collect();
    assert_eq!(records.len(), 2);
    let names: Vec<_> = overlay
        .components
        .values()
        .map(|data| data.qualified_name.to_flat_string())
        .collect();
    for expected in [
        "modified.driveData[1].acceleration",
        "modified.driveData[2].acceleration",
        "unchanged.driveData.wMax",
    ] {
        assert!(
            names.iter().any(|name| name == expected),
            "missing {expected}: {names:?}"
        );
    }
    assert!(
        !names
            .iter()
            .any(|name| name == "unchanged.driveData.acceleration")
    );
}

#[test]
fn redeclared_component_dimensions_use_the_enclosing_scope() {
    let source = format!(
        r#"{TYPES}
model Root
  parameter Integer k = 2;
  Holder modified(k=7, redeclare DriveData driveData[k]);
end Root;
"#
    );
    let (tree, overlay) = instantiate(&source);
    let root = tree.get_class_by_qualified_name("Root").unwrap();
    let modifier = &root.components["modified"].source_modifications[1];
    let ast::Expression::Modification { target, .. } = modifier else {
        panic!("redeclare modifier")
    };
    let ast::Subscript::Expression(ast::Expression::ComponentReference(index)) =
        &target.parts[0].subs.as_ref().unwrap()[0]
    else {
        panic!("symbolic dimension")
    };
    assert_eq!(index.target_def_id(), root.components["k"].def_id);
    let record_type = tree
        .get_class_by_qualified_name("DriveData")
        .unwrap()
        .def_id;
    assert_eq!(
        overlay
            .components
            .values()
            .filter(|data| data.type_def_id == record_type)
            .count(),
        2
    );
}

#[test]
fn component_redeclaration_without_subscripts_preserves_declared_dimensions() {
    let source = format!(
        r#"{}
model Root
  Holder modified(redeclare AlternateDriveData driveData);
end Root;
"#,
        TYPES.replace(
            "DriveData driveData constrainedby",
            "DriveData driveData[3] constrainedby"
        )
    );
    let (tree, overlay) = instantiate(&source);
    let replacement = tree
        .get_class_by_qualified_name("AlternateDriveData")
        .unwrap()
        .def_id;
    assert_eq!(
        overlay
            .components
            .values()
            .filter(|data| data.type_def_id == replacement)
            .count(),
        3
    );
}

#[test]
fn redeclared_component_array_preserves_each_and_distributed_field_modifiers() {
    for (modifier, expected) in [("each wMax=3", [3.0, 3.0]), ("wMax={3,4}", [3.0, 4.0])] {
        let source = format!(
            r#"{TYPES}
model Root
  Holder modified(redeclare AlternateDriveData driveData[2]({modifier}));
end Root;
"#
        );
        let (_, overlay) = instantiate(&source);
        for (index, expected) in expected.into_iter().enumerate() {
            let name = format!("modified.driveData[{}].wMax", index + 1);
            let data = overlay
                .components
                .values()
                .find(|data| data.qualified_name.to_flat_string() == name)
                .unwrap();
            let actual = data.binding.as_ref().and_then(|binding| match binding {
                ast::Expression::Terminal { token, .. } => token.text.parse::<f64>().ok(),
                _ => None,
            });
            assert_eq!(
                actual,
                Some(expected),
                "{modifier}: {name}, binding={:?}",
                data.binding
            );
        }
    }
}

#[test]
fn empty_redeclared_arrays_retain_their_declaration_rank() {
    let source = format!(
        r#"{TYPES}
model Root
  Holder empty(redeclare AlternateDriveData driveData[0]);
  Holder scalar;
end Root;
"#
    );
    let (tree, overlay) = instantiate(&source);
    let holder = tree.get_class_by_qualified_name("Holder").unwrap();
    assert!(
        overlay
            .array_component_declarations
            .contains(&holder.components["driveData"].def_id.unwrap())
    );
    assert!(!overlay.components.values().any(|data| {
        data.qualified_name
            .to_flat_string()
            .starts_with("empty.driveData")
    }));
}

#[test]
fn unapplied_nested_redeclarations_mark_descendant_shapes_unknown() {
    let source = format!(
        r#"{TYPES}
record Box
  replaceable DriveData driveData;
end Box;
model Container
  replaceable Box box;
end Container;
model Root
  Container c(redeclare Box box(redeclare AlternateDriveData driveData[2]));
end Root;
"#
    );
    let (_, overlay) = instantiate(&source);
    assert!(!overlay.components.is_empty());
    assert!(
        overlay
            .components
            .values()
            .all(|data| data.has_unapplied_redeclare)
    );
}
