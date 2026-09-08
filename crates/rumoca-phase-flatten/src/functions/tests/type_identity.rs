use super::*;

fn long_type_alias_tree(
    terminal: &str,
    terminal_class_type: Option<rumoca_core::ClassType>,
) -> ast::ClassTree {
    const ALIAS_COUNT: u32 = 40;
    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    let terminal_id = terminal_class_type.as_ref().map_or_else(
        || {
            tree.scope_tree
                .predefined_member(&rumoca_core::ComponentPath::from_flat_path(terminal))
                .expect("fixture terminal is a predefined type")
        },
        |_| rumoca_core::DefId::new(90_000 + ALIAS_COUNT),
    );
    if let Some(class_type) = terminal_class_type.as_ref() {
        tree.definitions.classes.insert(
            terminal.to_string(),
            class(terminal, class_type.clone(), terminal_id),
        );
    }
    for index in (0..ALIAS_COUNT).rev() {
        let name = format!("Alias{index}");
        let base_name = if index + 1 == ALIAS_COUNT {
            terminal.to_string()
        } else {
            format!("Alias{}", index + 1)
        };
        let base_id = if index + 1 == ALIAS_COUNT {
            Some(terminal_id)
        } else {
            Some(rumoca_core::DefId::new(90_000 + index + 1))
        };
        let mut alias = class(
            &name,
            rumoca_core::ClassType::Type,
            rumoca_core::DefId::new(90_000 + index),
        );
        alias.extends.push(ast::Extend {
            base_name: ast::Name::from_string(&base_name),
            base_def_id: base_id,
            ..Default::default()
        });
        tree.definitions.classes.insert(name, alias);
    }
    tree
}

#[test]
fn effective_function_type_has_no_arbitrary_alias_depth_limit() {
    let tree = long_type_alias_tree("TerminalModel", Some(rumoca_core::ClassType::Model));
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let first = tree
        .definitions
        .classes
        .get("Alias0")
        .expect("first alias exists");

    assert_eq!(
        effective_function_param_class_type(&class_index, first),
        rumoca_core::ClassType::Model
    );
}

#[test]
fn function_parameter_alias_dimensions_have_no_arbitrary_depth_limit() {
    let mut tree = long_type_alias_tree("Real", None);
    for index in 0..40 {
        tree.definitions
            .classes
            .get_mut(&format!("Alias{index}"))
            .expect("long alias member")
            .array_subscripts = vec![integer_subscript(2)];
    }
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let component = ast::Component {
        name: "value".to_string(),
        type_name: ast::Name {
            name: vec![token("Alias0")],
            def_id: Some(rumoca_core::DefId::new(90_000)),
        },
        type_def_id: Some(rumoca_core::DefId::new(90_000)),
        location: test_location(1, 2),
        ..ast::Component::empty_with_span(test_span())
    };

    let dims = function_param_type_alias_dims(&class_index, &component, &test_source_map())
        .expect("finite alias dimensions must be accumulated to the terminal root");
    assert_eq!(dims, vec![2; 40]);
}

#[test]
fn structural_cache_kind_has_no_arbitrary_alias_depth_limit() {
    let tree = long_type_alias_tree("Real", None);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let component = ast::Component {
        type_name: ast::Name::from_string("Alias0"),
        type_def_id: Some(rumoca_core::DefId::new(90_000)),
        ..ast::Component::empty_with_span(test_span())
    };

    assert_eq!(
        effective_component_constant_kind(&class_index, &component),
        Some(ComponentConstantKind::Real)
    );
}

#[test]
fn structural_cache_kind_rejects_primitive_spelling_without_identity() {
    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let component = ast::Component {
        type_name: ast::Name::from_string("Integer"),
        ..ast::Component::empty_with_span(test_span())
    };

    assert_eq!(
        effective_component_constant_kind(&class_index, &component),
        None
    );
    assert_eq!(
        effective_component_primitive_type(&class_index, &component, "Integer"),
        None,
        "a predefined spelling without its declaration identity is not a primitive type proof"
    );
}

#[test]
fn type_alias_cycle_terminates_without_minting_an_effective_type() {
    let a_id = rumoca_core::DefId::new(91_000);
    let b_id = rumoca_core::DefId::new(91_001);
    let mut a = class("A", rumoca_core::ClassType::Type, a_id);
    a.extends.push(ast::Extend {
        base_name: ast::Name::from_string("B"),
        base_def_id: Some(b_id),
        ..Default::default()
    });
    let mut b = class("B", rumoca_core::ClassType::Type, b_id);
    b.extends.push(ast::Extend {
        base_name: ast::Name::from_string("A"),
        base_def_id: Some(a_id),
        ..Default::default()
    });
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("A".to_string(), a);
    tree.definitions.classes.insert("B".to_string(), b);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let first = class_index.get(a_id).expect("cycle member is indexed");
    let component = ast::Component {
        name: "value".to_string(),
        type_name: ast::Name {
            name: vec![token("A")],
            def_id: Some(a_id),
        },
        type_def_id: Some(a_id),
        location: test_location(1, 2),
        ..ast::Component::empty_with_span(test_span())
    };

    assert_eq!(
        effective_function_param_class_type(&class_index, first),
        rumoca_core::ClassType::Type
    );
    assert_eq!(
        effective_component_constant_kind(&class_index, &component),
        None
    );

    let error = function_param_type_alias_dims(&class_index, &component, &test_source_map())
        .expect_err("a cycle cannot return partially accumulated parameter dimensions");
    assert!(
        matches!(error, FlattenError::MissingResolvedClassMetadata { .. }),
        "cycle refusal must remain a typed flatten diagnostic: {error:?}",
    );
}

#[test]
fn unresolved_function_parameter_alias_continuation_is_rejected() {
    let alias_id = rumoca_core::DefId::new(92_000);
    let missing_id = rumoca_core::DefId::new(92_001);
    let mut alias = class("Alias", rumoca_core::ClassType::Type, alias_id);
    alias.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Missing"),
        base_def_id: Some(missing_id),
        ..Default::default()
    });
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("Alias".to_string(), alias);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let component = ast::Component {
        name: "value".to_string(),
        type_name: ast::Name {
            name: vec![token("Alias")],
            def_id: Some(alias_id),
        },
        type_def_id: Some(alias_id),
        location: test_location(1, 2),
        ..ast::Component::empty_with_span(test_span())
    };

    let error = function_param_type_alias_dims(&class_index, &component, &test_source_map())
        .expect_err("a missing continuation cannot return partial dimensions");
    assert!(
        matches!(error, FlattenError::MissingResolvedClassMetadata { .. }),
        "unresolved continuation must remain a typed flatten diagnostic: {error:?}",
    );
}

#[test]
fn function_parameter_alias_uses_exact_def_id_not_colliding_spelling() {
    let exact_id = rumoca_core::DefId::new(92_100);
    let spelling_id = rumoca_core::DefId::new(92_101);
    let mut exact = class("Exact", rumoca_core::ClassType::Type, exact_id);
    exact.array_subscripts = vec![integer_subscript(2)];
    let mut colliding = class("Collision", rumoca_core::ClassType::Type, spelling_id);
    colliding.array_subscripts = vec![integer_subscript(7)];
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("Exact".to_string(), exact);
    tree.definitions
        .classes
        .insert("Collision".to_string(), colliding);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let component = ast::Component {
        name: "value".to_string(),
        type_name: ast::Name {
            name: vec![token("Collision")],
            def_id: Some(exact_id),
        },
        type_def_id: Some(exact_id),
        location: test_location(1, 2),
        ..ast::Component::empty_with_span(test_span())
    };

    assert_eq!(
        function_param_type_alias_dims(&class_index, &component, &test_source_map())
            .expect("exact declaration identity selects the alias owner"),
        vec![2]
    );
}

#[test]
fn function_parameter_alias_does_not_fallback_from_missing_def_id_to_spelling() {
    let spelling_id = rumoca_core::DefId::new(92_110);
    let colliding = class("Collision", rumoca_core::ClassType::Type, spelling_id);
    let mut tree = ast::ClassTree::new();
    tree.definitions
        .classes
        .insert("Collision".to_string(), colliding);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let component = ast::Component {
        name: "value".to_string(),
        type_name: ast::Name {
            name: vec![token("Collision")],
            def_id: Some(rumoca_core::DefId::new(92_111)),
        },
        type_def_id: Some(rumoca_core::DefId::new(92_111)),
        location: test_location(1, 2),
        ..ast::Component::empty_with_span(test_span())
    };

    let error = function_param_type_alias_dims(&class_index, &component, &test_source_map())
        .expect_err("a missing exact DefId cannot borrow a colliding spelling");
    assert!(matches!(
        error,
        FlattenError::MissingResolvedClassMetadata { .. }
    ));
}

#[test]
fn function_parameter_alias_rejects_multiple_base_continuations() {
    let alias_id = rumoca_core::DefId::new(92_120);
    let first_id = rumoca_core::DefId::new(92_121);
    let second_id = rumoca_core::DefId::new(92_122);
    let mut alias = class("Alias", rumoca_core::ClassType::Type, alias_id);
    for (name, def_id) in [("First", first_id), ("Second", second_id)] {
        alias.extends.push(ast::Extend {
            base_name: ast::Name::from_string(name),
            base_def_id: Some(def_id),
            ..Default::default()
        });
    }
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("Alias".to_string(), alias);
    tree.definitions.classes.insert(
        "First".to_string(),
        class("First", rumoca_core::ClassType::Model, first_id),
    );
    tree.definitions.classes.insert(
        "Second".to_string(),
        class("Second", rumoca_core::ClassType::Model, second_id),
    );
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let component = ast::Component {
        name: "value".to_string(),
        type_name: ast::Name {
            name: vec![token("Alias")],
            def_id: Some(alias_id),
        },
        type_def_id: Some(alias_id),
        location: test_location(1, 2),
        ..ast::Component::empty_with_span(test_span())
    };

    let error = function_param_type_alias_dims(&class_index, &component, &test_source_map())
        .expect_err("a Type wrapper cannot pick the first of multiple bases");
    assert!(matches!(
        error,
        FlattenError::MissingResolvedClassMetadata { .. }
    ));
}

#[test]
fn vectorization_certificate_rejects_replaceable_exposure_parent() {
    let package_id = rumoca_core::DefId::new(80_001);
    let function_id = rumoca_core::DefId::new(80_002);
    let mut package = class("P", rumoca_core::ClassType::Package, package_id);
    package.is_replaceable = true;
    package.classes.insert(
        "f".to_string(),
        class("f", rumoca_core::ClassType::Function, function_id),
    );
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("P".to_string(), package);
    let index = ast::ClassDefIndex::from_tree(&tree);
    let reference = core_structured_comp_ref(&[("P", package_id), ("f", function_id)]);
    let request = FunctionRequest::from_reference(
        &rumoca_core::Reference::from_component_reference(reference),
    );

    assert!(!request_proves_transitive_non_replaceability(
        &index, &request
    ));
}

#[test]
fn vectorization_certificate_ignores_replaceable_nested_sibling() {
    let package_id = rumoca_core::DefId::new(80_011);
    let function_id = rumoca_core::DefId::new(80_012);
    let sibling_id = rumoca_core::DefId::new(80_013);
    let mut package = class("P", rumoca_core::ClassType::Package, package_id);
    package.classes.insert(
        "f".to_string(),
        class("f", rumoca_core::ClassType::Function, function_id),
    );
    let mut sibling = class("Choice", rumoca_core::ClassType::Model, sibling_id);
    sibling.is_replaceable = true;
    package.classes.insert("Choice".to_string(), sibling);
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("P".to_string(), package);
    let index = ast::ClassDefIndex::from_tree(&tree);
    let reference = core_structured_comp_ref(&[("P", package_id), ("f", function_id)]);
    let request = FunctionRequest::from_reference(
        &rumoca_core::Reference::from_component_reference(reference),
    );

    assert!(request_proves_transitive_non_replaceability(
        &index, &request
    ));
}

#[test]
fn vectorization_certificate_rejects_unresolved_short_alias() {
    let function_id = rumoca_core::DefId::new(80_021);
    let mut function = class("f", rumoca_core::ClassType::Function, function_id);
    function.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Missing"),
        base_def_id: None,
        ..ast::Extend::default()
    });
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("f".to_string(), function);
    let index = ast::ClassDefIndex::from_tree(&tree);
    let reference = core_structured_comp_ref(&[("f", function_id)]);
    let request = FunctionRequest::from_reference(
        &rumoca_core::Reference::from_component_reference(reference),
    );

    assert!(!request_proves_transitive_non_replaceability(
        &index, &request
    ));
}
