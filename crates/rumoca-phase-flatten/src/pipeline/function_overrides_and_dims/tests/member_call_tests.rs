use super::*;

#[test]
fn marks_member_function_calls_through_component_type_aliases() {
    let gravity_def = DefId::new(1);
    let mut world = class("World", ClassType::Model);
    let mut gravity = class("gravityAcceleration", ClassType::Function);
    gravity.def_id = Some(gravity_def);
    world
        .classes
        .insert("gravityAcceleration".to_string(), gravity);

    let mut tree = ClassTree::new();
    tree.definitions.classes.insert("World".to_string(), world);
    tree.name_map
        .insert("World.gravityAcceleration".to_string(), gravity_def);

    let mut override_functions = OverrideFunctionMap::default();
    let world_def = DefId::new(2);
    let Some(world) = tree.definitions.classes.get_mut("World") else {
        panic!("expected World class");
    };
    world.def_id = Some(world_def);
    tree.def_map.insert(world_def, "World".to_string());
    override_functions.insert(
        "world".to_string(),
        override_target("World", world_def, ClassType::Model),
    );
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let marker = MemberFunctionCallMarker {
        tree: &tree,
        class_index: &class_index,
        override_functions: &override_functions,
    };

    assert_eq!(
        marker
            .mark_component_function_call(comp_ref(&["world", "gravityAcceleration"]))
            .def_id,
        Some(gravity_def)
    );
}

#[test]
fn root_package_alias_marks_member_function_calls() {
    let (tree, ids) = concrete_override_chain_tree();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let alias_target = override_target("AliasMedium", ids.alias_pkg, ClassType::Package);
    let component_override_map = root_component_override_map(&OverrideTarget {
        alias: "Medium".to_string(),
        ..alias_target
    });
    let (_, override_functions) = override_context_for_scope("", &component_override_map);
    let marker = MemberFunctionCallMarker {
        tree: &tree,
        class_index: &class_index,
        override_functions: &override_functions,
    };

    assert_eq!(
        marker
            .mark_component_function_call(comp_ref(&["Medium", "density"]))
            .def_id,
        Some(ids.concrete_density)
    );
}

#[test]
fn active_package_alias_rewrites_inherited_partial_function_call() {
    let (tree, ids) = concrete_override_chain_tree();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_packages = vec![OverrideTarget {
        alias: "Medium".to_string(),
        ..override_target("AliasMedium", ids.alias_pkg, ClassType::Package)
    }];
    let override_functions = OverrideFunctionMap::default();
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "PartialMedium.density",
            rumoca_core::ComponentReference {
                def_id: Some(ids.partial_density),
                ..core_comp_ref(&["PartialMedium", "density"])
            },
        ),
        args: vec![core_var("state")],
        is_constructor: false,
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression(
        &mut expr,
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    );

    let Expression::FunctionCall { name, .. } = expr else {
        panic!("expected rewritten function call");
    };
    assert_eq!(name.as_str(), "AliasMedium.density");
}

#[test]
fn fully_qualified_partial_function_prefers_local_medium_alias_when_multiple_media_match() {
    let (tree, ids) = concrete_override_chain_tree();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_packages = vec![
        OverrideTarget {
            alias: "Medium".to_string(),
            ..override_target("AliasMedium", ids.alias_pkg, ClassType::Package)
        },
        OverrideTarget {
            alias: "MediumAir".to_string(),
            ..override_target("ConcreteMedium", ids.concrete_pkg, ClassType::Package)
        },
    ];
    let override_functions = OverrideFunctionMap::default();
    let ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    );
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::new("PartialMedium.density"),
        args: vec![core_var("state")],
        is_constructor: false,
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx);

    let Expression::FunctionCall { name, .. } = expr else {
        panic!("expected rewritten function call");
    };
    assert_eq!(name.as_str(), "AliasMedium.density");
}

#[test]
fn leaves_unknown_member_function_calls_unmarked() {
    let tree = ClassTree::new();
    let mut override_functions = OverrideFunctionMap::default();
    override_functions.insert(
        "world".to_string(),
        override_target("World", DefId::new(1), ClassType::Model),
    );
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let marker = MemberFunctionCallMarker {
        tree: &tree,
        class_index: &class_index,
        override_functions: &override_functions,
    };

    assert_eq!(
        marker
            .mark_component_function_call(comp_ref(&["world", "gravityAcceleration"]))
            .def_id,
        None
    );
}
