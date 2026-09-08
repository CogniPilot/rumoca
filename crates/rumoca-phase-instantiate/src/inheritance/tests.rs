use super::*;
use rumoca_core::DefId;
use std::sync::Arc;

const TEST_FILE: &str = "inheritance.mo";

fn test_location() -> rumoca_core::Location {
    rumoca_core::Location {
        start_line: 1,
        start_column: 1,
        end_line: 1,
        end_column: 2,
        start: 0,
        end: 1,
        source: rumoca_core::SourceId::from_source_name(TEST_FILE),
    }
}

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(TEST_FILE), 1, 2)
}

/// Create a minimal component for testing.
fn make_component(name: &str, is_replaceable: bool, is_final: bool) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        is_replaceable,
        is_final,
        ..ast::Component::empty_with_span(test_span())
    }
}

/// Create a component reference for testing.
fn make_component_ref(name: &str) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: rumoca_core::Token {
                text: std::sync::Arc::from(name),
                location: rumoca_core::Location::default(),
                token_number: 0,
                token_type: 0,
            },
            subs: None,
            def_id: None,
        }],
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
    }
}

/// Create a token for testing.
fn make_token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: std::sync::Arc::from(text),
        location: rumoca_core::Location::default(),
        token_number: 0,
        token_type: 0,
    }
}

/// Create a Name for testing.
fn make_name(text: &str) -> rumoca_ir_ast::Name {
    rumoca_ir_ast::Name {
        name: rumoca_core::split_path_with_indices(text)
            .into_iter()
            .map(make_token)
            .collect(),
        def_id: None,
    }
}

fn make_resolved_name(text: &str, def_id: DefId) -> rumoca_ir_ast::Name {
    rumoca_ir_ast::Name {
        name: rumoca_core::split_path_with_indices(text)
            .into_iter()
            .map(make_token)
            .collect(),
        def_id: Some(def_id),
    }
}

fn register_predefined_external_object(tree: &mut ast::ClassTree) {
    tree.scope_tree.add_predefined_member(
        rumoca_core::ComponentPath::from_flat_path("ExternalObject"),
        DefId::new(u32::MAX),
    );
}

fn insert_resolved_test_class(
    tree: &mut ast::ClassTree,
    name: &str,
    def_id: DefId,
    mut class: ast::ClassDef,
) {
    class.def_id = Some(def_id);
    tree.name_map.insert(name.to_string(), def_id);
    tree.def_map.insert(def_id, name.to_string());
    tree.definitions.classes.insert(name.to_string(), class);
}

fn insert_user_real_collision(
    tree: &mut ast::ClassTree,
    package_def_id: DefId,
    user_real_def_id: DefId,
) {
    let mut package = ast::ClassDef {
        name: make_token("P"),
        def_id: Some(package_def_id),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    package.classes.insert(
        "Real".to_string(),
        ast::ClassDef {
            name: make_token("Real"),
            def_id: Some(user_real_def_id),
            ..Default::default()
        },
    );
    tree.definitions.classes.insert("P".to_string(), package);
    for (name, def_id) in [("P", package_def_id), ("P.Real", user_real_def_id)] {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }
}

fn make_int_expr(value: &str) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: make_token(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_resolved_ref_expr(name: &str, def_id: DefId) -> ast::Expression {
    let mut reference = make_component_ref(name);
    reference.set_root_def_id(Some(def_id));
    reference.set_target_def_id(Some(def_id));
    ast::Expression::ComponentReference(reference)
}

fn redeclared_type(name: &str, def_id: DefId) -> RedeclaredType {
    RedeclaredType {
        source_name: name.to_string(),
        def_id,
    }
}

fn register_test_predefined(tree: &mut ast::ClassTree, name: &str, def_id: DefId) {
    tree.scope_tree
        .add_predefined_member(rumoca_core::ComponentPath::from_flat_path(name), def_id);
}

#[test]
fn duplicate_inherited_components_require_equivalent_declarations() {
    let type_def_id = DefId::new(100);
    let mut existing = make_component("x", false, false);
    existing.type_def_id = Some(type_def_id);
    existing.type_name = make_name("Real");
    existing.has_explicit_binding = true;
    existing.binding = Some(make_int_expr("1"));

    let mut incoming = existing.clone();
    incoming.def_id = Some(DefId::new(102));
    incoming.binding = Some(make_int_expr("2"));

    assert!(
        !components_are_compatible(&existing, &incoming),
        "a shared resolved type must not hide conflicting bindings"
    );

    incoming.binding = existing.binding.clone();
    incoming.variability = rumoca_core::Variability::Parameter(make_token("parameter"));
    assert!(
        !components_are_compatible(&existing, &incoming),
        "a shared resolved type must not hide conflicting variability"
    );
}

#[test]
fn duplicate_inherited_components_distinguish_resolved_binding_identity() {
    let mut existing = make_component("x", false, false);
    existing.type_name = make_name("Real");
    existing.has_explicit_binding = true;
    existing.binding = Some(make_resolved_ref_expr("p", DefId::new(201)));

    let mut incoming = existing.clone();
    incoming.def_id = Some(DefId::new(202));
    incoming.binding = Some(make_resolved_ref_expr("p", DefId::new(203)));

    assert!(
        !components_are_compatible(&existing, &incoming),
        "equal source spelling must not hide different resolved declarations"
    );
}

#[test]
fn duplicate_inherited_components_from_same_source_keep_diamond_fast_path() {
    let shared_def_id = DefId::new(101);
    let mut existing = make_component("x", false, false);
    existing.def_id = Some(shared_def_id);
    existing.binding = Some(make_int_expr("1"));
    let mut incoming = existing.clone();
    incoming.binding = Some(make_int_expr("2"));

    assert!(
        components_are_compatible(&existing, &incoming),
        "the same source declaration inherited through a diamond contributes once"
    );
}

#[test]
fn duplicate_inherited_components_ignore_documentation_and_annotations() {
    let mut existing = make_component("system", false, false);
    existing.type_def_id = Some(DefId::new(100));
    existing.type_name = make_name("System");
    existing.description = vec![make_token("System wide properties")];
    existing.annotation = vec![make_int_expr("1")];

    let mut incoming = existing.clone();
    incoming.def_id = Some(DefId::new(102));
    incoming.description = vec![make_token("System properties")];
    incoming.annotation = vec![make_int_expr("2")];

    assert!(
        components_are_compatible(&existing, &incoming),
        "documentation and annotations do not change component declaration semantics"
    );
}

#[test]
fn test_apply_extends_modifications_reports_final_override_at_extends_span() {
    let mut tree = ast::ClassTree::default();
    tree.source_map.add(TEST_FILE, "extends Base(x = 2);");
    let mut target = InheritedContent::default();
    target
        .components
        .insert("x".to_string(), make_component("x", false, true));
    let base_class = ast::ClassDef {
        name: make_token("Base"),
        ..Default::default()
    };
    let extend = ast::Extend {
        base_name: make_name("Base"),
        location: test_location(),
        modifications: vec![ast::ExtendModification {
            expr: ast::Expression::Modification {
                target: make_component_ref("x"),
                value: Some(Arc::new(make_int_expr("2"))),
                span: rumoca_core::Span::DUMMY,
            },
            each: false,
            final_: false,
            redeclare: false,
        }],
        ..Default::default()
    };

    let err = apply_extends_modifications(&tree, &mut target, &base_class, &extend)
        .expect_err("extends modification must not override final inherited component");
    assert!(matches!(*err, InstantiateError::RedeclareFinal { .. }));
}

#[test]
fn test_validate_redeclaration_non_replaceable() {
    // A non-replaceable component should fail redeclaration
    let tree = ast::ClassTree::default();
    let comp = make_component("x", false, false);
    let result = validate_redeclaration(&tree, &comp, "x", None, Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("not replaceable"));
}

#[test]
fn test_validate_redeclaration_final() {
    // A final component should fail redeclaration (even if replaceable)
    let tree = ast::ClassTree::default();
    let comp = make_component("x", true, true);
    let result = validate_redeclaration(&tree, &comp, "x", None, Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("final"));
}

#[test]
fn test_validate_redeclaration_replaceable() {
    // A replaceable, non-final component should succeed
    let tree = ast::ClassTree::default();
    let comp = make_component("x", true, false);
    let result = validate_redeclaration(&tree, &comp, "x", None, Span::DUMMY);
    assert!(result.is_ok());
}

#[test]
fn redeclare_replacement_requires_exact_resolved_identity() {
    let expression = ast::Expression::Modification {
        target: make_component_ref("x"),
        value: Some(Arc::new(ast::Expression::ComponentReference(
            make_component_ref("Replacement"),
        ))),
        span: test_span(),
    };

    assert!(extract_redeclared_type(&expression, test_span()).is_err());
}

#[test]
fn exact_extends_identity_does_not_classify_user_real_as_predefined() {
    let predefined_real = DefId::new(120);
    let user_real = DefId::new(121);
    let root_id = DefId::new(122);
    let package_id = DefId::new(123);
    let mut tree = ast::ClassTree::default();
    register_test_predefined(&mut tree, "Real", predefined_real);
    insert_user_real_collision(&mut tree, package_id, user_real);
    insert_resolved_test_class(
        &mut tree,
        "Root",
        root_id,
        ast::ClassDef {
            name: make_token("Root"),
            extends: vec![ast::Extend {
                base_name: make_resolved_name("Real", user_real),
                base_def_id: Some(user_real),
                ..Default::default()
            }],
            ..Default::default()
        },
    );
    let root = tree.get_class_by_def_id(root_id).expect("root identity");

    assert_ne!(user_real, predefined_real);
    assert_eq!(root.extends[0].base_def_id, Some(user_real));

    assert_eq!(
        predefined_extend_name(&tree, &root.extends[0]).expect("extends edge is exact"),
        None
    );
    assert!(class_extends(&tree, root, "P.Real").expect("exact user type is reachable"));
}

#[test]
fn test_validate_redeclaration_constant_rejected() {
    let tree = ast::ClassTree::default();
    let mut comp = make_component("x", true, false);
    comp.variability = rumoca_core::Variability::Constant(make_token("constant"));
    let replacement = redeclared_type("Real", DefId::new(1));
    let result = validate_redeclaration(&tree, &comp, "x", Some(&replacement), Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.to_string()
            .contains("constant elements cannot be redeclared")
    );
}

#[test]
fn test_classes_are_compatible_for_equivalent_declarations() {
    let component = ast::Component {
        name: "k".to_string(),
        name_token: make_token("k"),
        type_name: make_name("Real"),
        variability: rumoca_core::Variability::Parameter(make_token("parameter")),
        start: ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token("1"),
            span: rumoca_core::Span::DUMMY,
        },
        has_explicit_binding: true,
        binding: Some(ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token("1"),
            span: rumoca_core::Span::DUMMY,
        }),
        ..ast::Component::empty_with_span(test_span())
    };

    let helper_a = ast::ClassDef {
        name: make_token("Helper"),
        class_type: rumoca_core::ClassType::Model,
        components: [("k".to_string(), component.clone())].into_iter().collect(),
        ..Default::default()
    };
    let helper_b = ast::ClassDef {
        name: make_token("Helper"),
        class_type: rumoca_core::ClassType::Model,
        components: [("k".to_string(), component)].into_iter().collect(),
        ..Default::default()
    };

    assert!(classes_are_compatible(&helper_a, &helper_b));
}

/// Create a component with constrainedby for testing.
fn make_constrained_component(
    name: &str,
    type_name: &str,
    type_def_id: DefId,
    constrainedby: Option<(&str, DefId)>,
) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        type_name: make_resolved_name(type_name, type_def_id),
        type_def_id: Some(type_def_id),
        is_replaceable: true,
        is_final: false,
        constrainedby: constrainedby.map(|(name, def_id)| make_resolved_name(name, def_id)),
        ..ast::Component::empty_with_span(test_span())
    }
}

#[test]
fn test_constrainedby_exact_match() {
    // Redeclaring with exact same type as constraint should succeed
    let real_id = DefId::new(90);
    let mut tree = ast::ClassTree::default();
    register_test_predefined(&mut tree, "Real", real_id);
    let comp = make_constrained_component("x", "Real", real_id, Some(("Real", real_id)));
    let replacement = redeclared_type("Real", real_id);
    let result = validate_redeclaration(&tree, &comp, "x", Some(&replacement), Span::DUMMY);
    assert!(result.is_ok());
}

#[test]
fn test_constrainedby_violation_builtin() {
    // Redeclaring Real constrained component to Integer should fail
    let real_id = DefId::new(91);
    let integer_id = DefId::new(92);
    let mut tree = ast::ClassTree::default();
    register_test_predefined(&mut tree, "Real", real_id);
    register_test_predefined(&mut tree, "Integer", integer_id);
    let comp = make_constrained_component("x", "Real", real_id, Some(("Real", real_id)));
    let replacement = redeclared_type("Integer", integer_id);
    let result = validate_redeclaration(&tree, &comp, "x", Some(&replacement), Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("violates constrainedby"));
}

#[test]
fn test_constrainedby_default_uses_original_type() {
    // When no constrainedby is specified, the original type is the constraint
    let real_id = DefId::new(93);
    let integer_id = DefId::new(94);
    let mut tree = ast::ClassTree::default();
    register_test_predefined(&mut tree, "Real", real_id);
    register_test_predefined(&mut tree, "Integer", integer_id);
    let comp = make_constrained_component("x", "Real", real_id, None);
    // Redeclaring to Integer should fail (Real is implicit constraint)
    let replacement = redeclared_type("Integer", integer_id);
    let result = validate_redeclaration(&tree, &comp, "x", Some(&replacement), Span::DUMMY);
    assert!(result.is_err());
}

#[test]
fn component_redeclare_distinguishes_user_real_from_predefined_real() {
    let predefined_real = DefId::new(95);
    let user_real = DefId::new(96);
    let package_id = DefId::new(97);
    let mut tree = ast::ClassTree::default();
    register_test_predefined(&mut tree, "Real", predefined_real);
    insert_user_real_collision(&mut tree, package_id, user_real);
    let component = make_constrained_component(
        "x",
        "Real",
        predefined_real,
        Some(("Real", predefined_real)),
    );
    let replacement = redeclared_type("P.Real", user_real);

    assert_ne!(user_real, predefined_real);
    assert_eq!(
        component
            .constrainedby
            .as_ref()
            .and_then(|constraint| constraint.def_id),
        Some(predefined_real)
    );
    assert_eq!(
        tree.def_map.get(&user_real).map(String::as_str),
        Some("P.Real")
    );

    let error = validate_redeclaration(&tree, &component, "x", Some(&replacement), Span::DUMMY)
        .expect_err("same-leaf user P.Real cannot satisfy the predefined Real constraint");

    assert!(matches!(
        *error,
        InstantiateError::RedeclareConstraintViolation { .. }
    ));
}

#[test]
fn component_redeclare_accepts_exact_type_alias_extending_predefined_real() {
    let predefined_real = DefId::new(110);
    let voltage = DefId::new(111);
    let mut tree = ast::ClassTree::default();
    register_test_predefined(&mut tree, "Real", predefined_real);
    insert_resolved_test_class(
        &mut tree,
        "Voltage",
        voltage,
        ast::ClassDef {
            name: make_token("Voltage"),
            extends: vec![ast::Extend {
                base_name: make_resolved_name("Real", predefined_real),
                base_def_id: Some(predefined_real),
                ..Default::default()
            }],
            ..Default::default()
        },
    );
    let component = make_constrained_component(
        "v",
        "Real",
        predefined_real,
        Some(("Real", predefined_real)),
    );
    let replacement = redeclared_type("Voltage", voltage);

    validate_redeclaration(&tree, &component, "v", Some(&replacement), Span::DUMMY)
        .expect("an exact type alias extending predefined Real satisfies the constraint");
}

#[test]
fn component_redeclare_distinguishes_same_leaf_types_in_different_packages() {
    let constraint_id = DefId::new(97);
    let replacement_id = DefId::new(98);
    let mut tree = ast::ClassTree::default();
    for (qualified, def_id) in [("P.Medium", constraint_id), ("Q.Medium", replacement_id)] {
        insert_resolved_test_class(
            &mut tree,
            qualified,
            def_id,
            ast::ClassDef {
                name: make_token("Medium"),
                ..Default::default()
            },
        );
    }
    let component = make_constrained_component(
        "medium",
        "P.Medium",
        constraint_id,
        Some(("P.Medium", constraint_id)),
    );
    let replacement = redeclared_type("Q.Medium", replacement_id);

    assert!(
        validate_redeclaration(&tree, &component, "medium", Some(&replacement), Span::DUMMY,)
            .is_err()
    );
}

#[test]
fn component_redeclare_refuses_unresolved_explicit_constraint() {
    let replacement_id = DefId::new(105);
    let fallback_id = DefId::new(106);
    let mut tree = ast::ClassTree::default();
    for (name, def_id) in [("Replacement", replacement_id), ("Fallback", fallback_id)] {
        insert_resolved_test_class(
            &mut tree,
            name,
            def_id,
            ast::ClassDef {
                name: make_token(name),
                ..Default::default()
            },
        );
    }
    let mut component = make_constrained_component(
        "x",
        "Fallback",
        fallback_id,
        Some(("MissingConstraint", fallback_id)),
    );
    component
        .constrainedby
        .as_mut()
        .expect("explicit constraint")
        .def_id = None;
    let replacement = redeclared_type("Replacement", replacement_id);

    assert!(
        validate_redeclaration(&tree, &component, "x", Some(&replacement), Span::DUMMY,).is_err(),
        "an unresolved explicit constraint must not fall back to the declaration type"
    );
}

#[test]
fn class_redeclare_distinguishes_user_real_from_predefined_real() {
    let predefined_real = DefId::new(99);
    let user_real = DefId::new(100);
    let alias_id = DefId::new(101);
    let mut tree = ast::ClassTree::default();
    register_test_predefined(&mut tree, "Real", predefined_real);
    insert_resolved_test_class(
        &mut tree,
        "P.Real",
        user_real,
        ast::ClassDef {
            name: make_token("Real"),
            ..Default::default()
        },
    );
    let alias = ast::ClassDef {
        name: make_token("T"),
        def_id: Some(alias_id),
        is_replaceable: true,
        constrainedby: Some(make_resolved_name("Real", predefined_real)),
        ..Default::default()
    };
    let replacement = redeclared_type("P.Real", user_real);

    assert!(
        validate_class_redeclaration(&tree, &alias, "T", Some(&replacement), Span::DUMMY,).is_err()
    );
}

#[test]
fn class_redeclare_distinguishes_same_leaf_types_in_different_packages() {
    let constraint_id = DefId::new(102);
    let replacement_id = DefId::new(103);
    let alias_id = DefId::new(104);
    let mut tree = ast::ClassTree::default();
    for (qualified, def_id) in [("P.Medium", constraint_id), ("Q.Medium", replacement_id)] {
        insert_resolved_test_class(
            &mut tree,
            qualified,
            def_id,
            ast::ClassDef {
                name: make_token("Medium"),
                ..Default::default()
            },
        );
    }
    let alias = ast::ClassDef {
        name: make_token("Medium"),
        def_id: Some(alias_id),
        is_replaceable: true,
        constrainedby: Some(make_resolved_name("P.Medium", constraint_id)),
        ..Default::default()
    };
    let replacement = redeclared_type("Q.Medium", replacement_id);

    assert!(
        validate_class_redeclaration(&tree, &alias, "Medium", Some(&replacement), Span::DUMMY,)
            .is_err()
    );
}

#[test]
fn class_redeclare_refuses_unresolved_explicit_constraint() {
    let replacement_id = DefId::new(107);
    let fallback_id = DefId::new(108);
    let alias_id = DefId::new(109);
    let mut tree = ast::ClassTree::default();
    for (name, def_id) in [("Replacement", replacement_id), ("Fallback", fallback_id)] {
        insert_resolved_test_class(
            &mut tree,
            name,
            def_id,
            ast::ClassDef {
                name: make_token(name),
                ..Default::default()
            },
        );
    }
    let alias = ast::ClassDef {
        name: make_token("T"),
        def_id: Some(alias_id),
        is_replaceable: true,
        constrainedby: Some(make_name("MissingConstraint")),
        extends: vec![ast::Extend {
            base_name: make_resolved_name("Fallback", fallback_id),
            base_def_id: Some(fallback_id),
            ..Default::default()
        }],
        ..Default::default()
    };
    let replacement = redeclared_type("Replacement", replacement_id);

    assert!(
        validate_class_redeclaration(&tree, &alias, "T", Some(&replacement), Span::DUMMY,).is_err(),
        "an unresolved explicit constraint must not fall back to the resolved extends edge"
    );
}

#[test]
fn test_constrainedby_subtype_allowed() {
    // Redeclaring to a subtype of the constraint should succeed

    let mut tree = ast::ClassTree::default();
    let base_def_id = DefId::new(100);
    let derived_def_id = DefId::new(101);
    register_predefined_external_object(&mut tree);

    let base = ast::ClassDef {
        name: make_token("BaseConnector"),
        ..Default::default()
    };

    let derived = ast::ClassDef {
        name: make_token("DerivedConnector"),
        extends: vec![ast::Extend {
            base_name: make_resolved_name("BaseConnector", base_def_id),
            base_def_id: Some(base_def_id),
            ..Default::default()
        }],
        ..Default::default()
    };

    insert_resolved_test_class(&mut tree, "BaseConnector", base_def_id, base);
    insert_resolved_test_class(&mut tree, "DerivedConnector", derived_def_id, derived);

    // ast::Component constrained to BaseConnector
    let comp = make_constrained_component(
        "c",
        "BaseConnector",
        base_def_id,
        Some(("BaseConnector", base_def_id)),
    );

    // Redeclaring to DerivedConnector (a subtype) should succeed
    let replacement = redeclared_type("DerivedConnector", derived_def_id);
    let result = validate_redeclaration(&tree, &comp, "c", Some(&replacement), Span::DUMMY);
    assert!(result.is_ok());
}

#[test]
fn test_class_redeclare_constraint_resolves_relative_to_declaration_scope() {
    let (tree, flow_characteristic_id) = relative_class_redeclare_constraint_tree();

    let class = tree
        .get_class_by_def_id(flow_characteristic_id)
        .expect("flowCharacteristic class should exist");

    let quadratic_flow_id = tree
        .get_def_id_by_name("Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.quadraticFlow")
        .expect("quadraticFlow identity");
    let replacement = redeclared_type(
        "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.quadraticFlow",
        quadratic_flow_id,
    );
    let result = validate_class_redeclaration(
        &tree,
        class,
        "flowCharacteristic",
        Some(&replacement),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

fn relative_class_redeclare_constraint_tree() -> (ast::ClassTree, DefId) {
    let ids = RelativeConstraintIds::new();
    let modelica = relative_constraint_modelica_class(&ids);

    let mut tree = ast::ClassTree::default();
    register_predefined_external_object(&mut tree);
    tree.definitions
        .classes
        .insert("Modelica".to_string(), modelica);
    for (name, def_id) in ids.qualified_names() {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }
    (tree, ids.flow_characteristic)
}

struct RelativeConstraintIds {
    base_flow: DefId,
    quadratic_flow: DefId,
    flow_characteristic: DefId,
    pump_characteristics: DefId,
    partial_pump: DefId,
    base_classes: DefId,
    machines: DefId,
    fluid: DefId,
    modelica: DefId,
}

impl RelativeConstraintIds {
    fn new() -> Self {
        Self {
            base_flow: DefId::new(1),
            quadratic_flow: DefId::new(2),
            flow_characteristic: DefId::new(3),
            pump_characteristics: DefId::new(4),
            partial_pump: DefId::new(5),
            base_classes: DefId::new(6),
            machines: DefId::new(7),
            fluid: DefId::new(8),
            modelica: DefId::new(9),
        }
    }

    fn qualified_names(&self) -> [(&'static str, DefId); 9] {
        [
            ("Modelica", self.modelica),
            ("Modelica.Fluid", self.fluid),
            ("Modelica.Fluid.Machines", self.machines),
            ("Modelica.Fluid.Machines.BaseClasses", self.base_classes),
            (
                "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics",
                self.pump_characteristics,
            ),
            (
                "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.baseFlow",
                self.base_flow,
            ),
            (
                "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.quadraticFlow",
                self.quadratic_flow,
            ),
            (
                "Modelica.Fluid.Machines.BaseClasses.PartialPump",
                self.partial_pump,
            ),
            (
                "Modelica.Fluid.Machines.BaseClasses.PartialPump.flowCharacteristic",
                self.flow_characteristic,
            ),
        ]
    }
}

fn relative_constraint_modelica_class(ids: &RelativeConstraintIds) -> ast::ClassDef {
    let base_flow = ast::ClassDef {
        name: make_token("baseFlow"),
        def_id: Some(ids.base_flow),
        class_type: rumoca_core::ClassType::Function,
        ..Default::default()
    };
    let quadratic_flow = ast::ClassDef {
        name: make_token("quadraticFlow"),
        def_id: Some(ids.quadratic_flow),
        class_type: rumoca_core::ClassType::Function,
        extends: vec![ast::Extend {
            base_name: make_resolved_name("baseFlow", ids.base_flow),
            base_def_id: Some(ids.base_flow),
            ..Default::default()
        }],
        ..Default::default()
    };

    let mut pump_characteristics = ast::ClassDef {
        name: make_token("PumpCharacteristics"),
        def_id: Some(ids.pump_characteristics),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    pump_characteristics
        .classes
        .insert("baseFlow".to_string(), base_flow);
    pump_characteristics
        .classes
        .insert("quadraticFlow".to_string(), quadratic_flow);

    let flow_characteristic = ast::ClassDef {
        name: make_token("flowCharacteristic"),
        def_id: Some(ids.flow_characteristic),
        class_type: rumoca_core::ClassType::Function,
        is_replaceable: true,
        extends: vec![ast::Extend {
            base_name: make_resolved_name("PumpCharacteristics.baseFlow", ids.base_flow),
            base_def_id: Some(ids.base_flow),
            ..Default::default()
        }],
        ..Default::default()
    };
    let mut partial_pump = ast::ClassDef {
        name: make_token("PartialPump"),
        def_id: Some(ids.partial_pump),
        ..Default::default()
    };
    partial_pump
        .classes
        .insert("flowCharacteristic".to_string(), flow_characteristic);

    let mut base_classes = ast::ClassDef {
        name: make_token("BaseClasses"),
        def_id: Some(ids.base_classes),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    base_classes
        .classes
        .insert("PumpCharacteristics".to_string(), pump_characteristics);
    base_classes
        .classes
        .insert("PartialPump".to_string(), partial_pump);

    let mut machines = ast::ClassDef {
        name: make_token("Machines"),
        def_id: Some(ids.machines),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    machines
        .classes
        .insert("BaseClasses".to_string(), base_classes);

    let mut fluid = ast::ClassDef {
        name: make_token("Fluid"),
        def_id: Some(ids.fluid),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    fluid.classes.insert("Machines".to_string(), machines);

    let mut modelica = ast::ClassDef {
        name: make_token("Modelica"),
        def_id: Some(ids.modelica),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    modelica.classes.insert("Fluid".to_string(), fluid);
    modelica
}

#[test]
fn test_constrainedby_non_subtype_rejected() {
    // Redeclaring to a non-subtype should fail
    let mut tree = ast::ClassTree::default();

    let class_a_id = DefId::new(110);
    let class_b_id = DefId::new(111);
    insert_resolved_test_class(
        &mut tree,
        "ClassA",
        class_a_id,
        ast::ClassDef {
            name: make_token("ClassA"),
            ..Default::default()
        },
    );
    insert_resolved_test_class(
        &mut tree,
        "ClassB",
        class_b_id,
        ast::ClassDef {
            name: make_token("ClassB"),
            ..Default::default()
        },
    );

    // ast::Component constrained to ClassA
    let comp = make_constrained_component("c", "ClassA", class_a_id, Some(("ClassA", class_a_id)));

    // Redeclaring to ClassB (not a subtype) should fail
    let replacement = redeclared_type("ClassB", class_b_id);
    let result = validate_redeclaration(&tree, &comp, "c", Some(&replacement), Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("violates constrainedby"));
}

#[test]
fn test_class_redeclaration_default_constraint_uses_declared_base() {
    let tree = crate::test_support::resolved_tree(
        "default_class_constraint.mo",
        r"
package PhaseSystems
  partial model PartialPhaseSystem end PartialPhaseSystem;
  model TwoConductor
    extends PartialPhaseSystem;
  end TwoConductor;
end PhaseSystems;
package Interfaces
  replaceable model PhaseSystem = PhaseSystems.PartialPhaseSystem;
end Interfaces;
",
    );
    let alias_phase_system = tree
        .get_class_by_qualified_name("Interfaces.PhaseSystem")
        .expect("resolved replaceable class alias");
    let replacement_id = tree
        .get_class_by_qualified_name("PhaseSystems.TwoConductor")
        .and_then(|class| class.def_id)
        .expect("resolved replacement identity");
    let replacement = redeclared_type("PhaseSystems.TwoConductor", replacement_id);
    let result = validate_class_redeclaration(
        &tree,
        alias_phase_system,
        "PhaseSystem",
        Some(&replacement),
        Span::DUMMY,
    );
    result.expect(
        "replacement extending the exact declared base must satisfy the default constraint",
    );
}

#[test]
fn test_nested_class_redeclaration_replaces_inherited_replaceable_class() {
    let mut tree = ast::ClassTree::default();
    tree.source_map.add(TEST_FILE, "extends Base;");

    let partial_id = DefId::new(10);
    let base_properties_id = DefId::new(11);
    let derived_id = DefId::new(12);
    let redeclared_id = DefId::new(13);

    let base_properties = ast::ClassDef {
        name: make_token("BaseProperties"),
        def_id: Some(base_properties_id),
        class_type: rumoca_core::ClassType::Model,
        is_replaceable: true,
        ..Default::default()
    };
    let mut partial = ast::ClassDef {
        name: make_token("PartialPureSubstance"),
        def_id: Some(partial_id),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    partial
        .classes
        .insert("BaseProperties".to_string(), base_properties);

    let redeclared_base_properties = ast::ClassDef {
        name: make_token("BaseProperties"),
        def_id: Some(redeclared_id),
        class_type: rumoca_core::ClassType::Model,
        is_replaceable: true,
        extends: vec![ast::Extend {
            base_name: make_resolved_name(
                "PartialPureSubstance.BaseProperties",
                base_properties_id,
            ),
            base_def_id: Some(base_properties_id),
            location: test_location(),
            ..Default::default()
        }],
        ..Default::default()
    };

    let mut derived = ast::ClassDef {
        name: make_token("WaterIF97_base"),
        def_id: Some(derived_id),
        class_type: rumoca_core::ClassType::Package,
        extends: vec![ast::Extend {
            base_name: make_resolved_name("PartialPureSubstance", partial_id),
            base_def_id: Some(partial_id),
            location: test_location(),
            ..Default::default()
        }],
        ..Default::default()
    };
    derived
        .classes
        .insert("BaseProperties".to_string(), redeclared_base_properties);

    tree.definitions
        .classes
        .insert("PartialPureSubstance".to_string(), partial);
    tree.definitions
        .classes
        .insert("WaterIF97_base".to_string(), derived);
    for (name, def_id) in [
        ("PartialPureSubstance", partial_id),
        ("PartialPureSubstance.BaseProperties", base_properties_id),
        ("WaterIF97_base", derived_id),
        ("WaterIF97_base.BaseProperties", redeclared_id),
    ] {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }

    let class = tree
        .get_class_by_qualified_name("WaterIF97_base")
        .expect("derived class should exist");
    let result = get_effective_components(&tree, class);
    assert!(
        result.is_ok(),
        "nested replaceable class redeclare should replace the inherited declaration"
    );
}

#[test]
fn test_nested_class_redeclaration_shadows_inherited_replaceable_merged_later() {
    let mut tree = ast::ClassTree::default();
    tree.source_map.add(TEST_FILE, "extends Base;");

    let partial_medium_id = DefId::new(20);
    let partial_state_id = DefId::new(21);
    let simple_medium_id = DefId::new(22);
    let simple_state_id = DefId::new(23);
    let water_id = DefId::new(24);

    let mut partial_medium = ast::ClassDef {
        name: make_token("PartialMedium"),
        def_id: Some(partial_medium_id),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    partial_medium.classes.insert(
        "ThermodynamicState".to_string(),
        ast::ClassDef {
            name: make_token("ThermodynamicState"),
            def_id: Some(partial_state_id),
            class_type: rumoca_core::ClassType::Record,
            is_replaceable: true,
            ..Default::default()
        },
    );

    let mut simple_medium = ast::ClassDef {
        name: make_token("PartialSimpleMedium"),
        def_id: Some(simple_medium_id),
        class_type: rumoca_core::ClassType::Package,
        extends: vec![ast::Extend {
            base_name: make_resolved_name("PartialMedium", partial_medium_id),
            base_def_id: Some(partial_medium_id),
            location: test_location(),
            ..Default::default()
        }],
        ..Default::default()
    };
    simple_medium.classes.insert(
        "ThermodynamicState".to_string(),
        ast::ClassDef {
            name: make_token("ThermodynamicState"),
            def_id: Some(simple_state_id),
            class_type: rumoca_core::ClassType::Record,
            is_replaceable: true,
            ..Default::default()
        },
    );

    let water = ast::ClassDef {
        name: make_token("ConstantPropertyLiquidWater"),
        def_id: Some(water_id),
        class_type: rumoca_core::ClassType::Package,
        extends: vec![ast::Extend {
            base_name: make_resolved_name("PartialSimpleMedium", simple_medium_id),
            base_def_id: Some(simple_medium_id),
            location: test_location(),
            ..Default::default()
        }],
        ..Default::default()
    };

    tree.definitions
        .classes
        .insert("PartialMedium".to_string(), partial_medium);
    tree.definitions
        .classes
        .insert("PartialSimpleMedium".to_string(), simple_medium);
    tree.definitions
        .classes
        .insert("ConstantPropertyLiquidWater".to_string(), water);
    for (name, def_id) in [
        ("PartialMedium", partial_medium_id),
        ("PartialMedium.ThermodynamicState", partial_state_id),
        ("PartialSimpleMedium", simple_medium_id),
        ("PartialSimpleMedium.ThermodynamicState", simple_state_id),
        ("ConstantPropertyLiquidWater", water_id),
    ] {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }

    let class = tree
        .get_class_by_qualified_name("ConstantPropertyLiquidWater")
        .expect("derived class should exist");
    let inherited = process_extends(&tree, class)
        .expect("redeclared nested class should shadow inherited replaceable class");
    let effective_state = inherited
        .classes
        .get("ThermodynamicState")
        .expect("effective nested class should exist");
    assert_eq!(effective_state.def_id, Some(simple_state_id));
}

#[test]
fn test_is_type_subtype_exact_match() {
    let tree = ast::ClassTree::default();
    assert!(is_type_subtype(&tree, "Real", "Real").expect("builtin subtype compares"));
    assert!(is_type_subtype(&tree, "MyClass", "MyClass").is_err());
}

#[test]
fn test_is_type_subtype_builtin_mismatch() {
    let tree = ast::ClassTree::default();
    assert!(!is_type_subtype(&tree, "Real", "Integer").expect("builtin subtype compares"));
    assert!(!is_type_subtype(&tree, "Boolean", "String").expect("builtin subtype compares"));
}

#[test]
fn test_is_type_subtype_via_extends() {
    let mut tree = ast::ClassTree::default();
    let a_def_id = DefId::new(200);
    let b_def_id = DefId::new(201);
    let c_def_id = DefId::new(202);
    register_predefined_external_object(&mut tree);

    // A extends nothing
    let class_a = ast::ClassDef {
        name: make_token("A"),
        ..Default::default()
    };

    // B extends A
    let class_b = ast::ClassDef {
        name: make_token("B"),
        extends: vec![ast::Extend {
            base_name: make_resolved_name("A", a_def_id),
            base_def_id: Some(a_def_id),
            ..Default::default()
        }],
        ..Default::default()
    };

    // C extends B (transitive: C -> B -> A)
    let class_c = ast::ClassDef {
        name: make_token("C"),
        extends: vec![ast::Extend {
            base_name: make_resolved_name("B", b_def_id),
            base_def_id: Some(b_def_id),
            ..Default::default()
        }],
        ..Default::default()
    };

    insert_resolved_test_class(&mut tree, "A", a_def_id, class_a);
    insert_resolved_test_class(&mut tree, "B", b_def_id, class_b);
    insert_resolved_test_class(&mut tree, "C", c_def_id, class_c);

    // B is subtype of A
    assert!(is_type_subtype(&tree, "B", "A").expect("resolved subtype compares"));
    // C is subtype of B
    assert!(is_type_subtype(&tree, "C", "B").expect("resolved subtype compares"));
    // C is subtype of A (transitive)
    assert!(is_type_subtype(&tree, "C", "A").expect("resolved subtype compares"));
    // A is NOT subtype of B
    assert!(!is_type_subtype(&tree, "A", "B").expect("resolved subtype compares"));
}

#[test]
fn test_class_extends_cached_matches_base_def_id_for_relative_extends_name() {
    let tree = crate::test_support::resolved_tree(
        "relative_extends.mo",
        r"
package Root
  package Pkg
    package Interfaces
      model C end C;
    end Interfaces;
    model D
      extends Interfaces.C;
    end D;
  end Pkg;
end Root;
",
    );

    let d_class = tree
        .get_class_by_qualified_name("Root.Pkg.D")
        .expect("Root.Pkg.D class should exist");
    let mut cache = SubtypeCache::default();
    assert!(
        class_extends_cached(&tree, d_class, "Root.Pkg.Interfaces.C", &mut cache)
            .expect("resolved extends graph compares"),
        "relative extends with base_def_id should match the resolved queried supertype"
    );
    let cache_snapshot = cache.clone();
    let error = class_extends_cached(&tree, d_class, "Interfaces.C", &mut cache)
        .expect_err("unresolved short supertype names fail closed");
    assert!(matches!(*error, InstantiateError::ModelNotFound(_)));
    assert_eq!(
        cache, cache_snapshot,
        "failed lookup must not mutate any existing subtype-cache row"
    );
}

#[test]
fn subtype_cache_distinguishes_same_named_classes_by_def_id() {
    let tree = crate::test_support::resolved_tree(
        "subtype_cache_identity.mo",
        r"
model Base end Base;
package P
  model Foo
    extends Base;
  end Foo;
end P;
package Q
  model Foo end Foo;
end Q;
",
    );
    let extending_foo = tree
        .get_class_by_qualified_name("P.Foo")
        .expect("resolved extending Foo");
    let unrelated_foo = tree
        .get_class_by_qualified_name("Q.Foo")
        .expect("resolved unrelated Foo");

    let mut cache = SubtypeCache::default();
    assert!(
        class_extends_cached(&tree, extending_foo, "Base", &mut cache)
            .expect("resolved extends graph compares")
    );
    assert!(
        !class_extends_cached(&tree, unrelated_foo, "Base", &mut cache)
            .expect("resolved extends graph compares")
    );
    assert_eq!(cache.len(), 2, "each resolved subtype owns one memo row");
}

#[test]
fn test_extract_modification_target_modification() {
    // Test extracting target from ast::Expression::Modification
    let expr = ast::Expression::Modification {
        target: make_component_ref("myVar"),
        value: Some(Arc::new(ast::Expression::Empty {
            span: rumoca_core::Span::DUMMY,
        })),
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(
        extract_modification_target(&expr),
        Some("myVar".to_string())
    );
}

#[test]
fn test_extract_modification_target_class_modification() {
    // Test extracting target from ast::Expression::ClassModification
    let expr = ast::Expression::ClassModification {
        target: make_component_ref("myClass"),
        modifications: vec![],
        each_flags: vec![],
        final_flags: vec![],
        redeclare_flags: vec![],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(
        extract_modification_target(&expr),
        Some("myClass".to_string())
    );
}

#[test]
fn test_extract_modification_target_named_argument() {
    // Test extracting target from ast::Expression::NamedArgument
    let expr = ast::Expression::NamedArgument {
        name: make_token("param"),
        value: Arc::new(ast::Expression::Empty {
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(
        extract_modification_target(&expr),
        Some("param".to_string())
    );
}

#[test]
fn test_is_effectively_primitive_transitive_enumeration_chain() {
    // Test that type alias chains leading to enumerations are detected as primitive
    // type Logic = enumeration(...)
    // connector DigitalSignal = Logic
    // connector DigitalInput = input DigitalSignal

    let mut tree = ast::ClassTree::new();
    let logic_id = DefId::new(700);
    let digital_signal_id = DefId::new(701);
    let digital_input_id = DefId::new(702);

    // Logic enumeration
    let mut logic = ast::ClassDef {
        name: make_token("Logic"),
        ..Default::default()
    };
    logic.enum_literals.push(ast::EnumLiteral {
        ident: make_token("U"),
        description: vec![],
    });
    logic.enum_literals.push(ast::EnumLiteral {
        ident: make_token("X"),
        description: vec![],
    });

    // DigitalSignal = Logic
    let digital_signal = ast::ClassDef {
        name: make_token("DigitalSignal"),
        extends: vec![ast::Extend {
            base_name: make_resolved_name("Logic", logic_id),
            base_def_id: Some(logic_id),
            ..Default::default()
        }],
        ..Default::default()
    };

    // DigitalInput = input DigitalSignal
    let digital_input = ast::ClassDef {
        name: make_token("DigitalInput"),
        extends: vec![ast::Extend {
            base_name: make_resolved_name("DigitalSignal", digital_signal_id),
            base_def_id: Some(digital_signal_id),
            ..Default::default()
        }],
        ..Default::default()
    };

    insert_resolved_test_class(&mut tree, "Logic", logic_id, logic);
    insert_resolved_test_class(
        &mut tree,
        "DigitalSignal",
        digital_signal_id,
        digital_signal,
    );
    insert_resolved_test_class(&mut tree, "DigitalInput", digital_input_id, digital_input);

    // DigitalInput should be effectively primitive because it chains to Logic (enumeration)
    let digital_input = tree
        .get_class_by_def_id(digital_input_id)
        .expect("DigitalInput class exists");
    assert!(
        is_effectively_primitive_transitive(&tree, digital_input)
            .expect("resolved primitive ancestry compares")
    );
}

#[test]
fn external_object_descendant_is_not_a_scalar_primitive() {
    let mut tree = ast::ClassTree::new();
    register_predefined_external_object(&mut tree);
    let handle_id = DefId::new(703);
    let handle = ast::ClassDef {
        name: make_token("Handle"),
        extends: vec![ast::Extend {
            base_name: make_resolved_name("ExternalObject", DefId::new(u32::MAX)),
            base_def_id: Some(DefId::new(u32::MAX)),
            ..Default::default()
        }],
        ..Default::default()
    };
    insert_resolved_test_class(&mut tree, "Handle", handle_id, handle);

    let handle = tree
        .get_class_by_def_id(handle_id)
        .expect("Handle class exists");
    assert!(
        !is_effectively_primitive_transitive(&tree, handle)
            .expect("resolved ExternalObject ancestry is classified"),
        "ExternalObject owns lifecycle semantics and is never a scalar predefined type"
    );
}
