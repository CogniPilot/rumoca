//! Member function calls through package aliases: marking, rewriting,
//! and concrete override chain preference.

use super::*;

#[test]
fn replaceable_package_function_prefers_concrete_override_chain() {
    let (tree, ids) = concrete_override_chain_tree();

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let mut alias_target = override_target("AliasMedium", ids.alias_pkg, ClassType::Package);
    alias_target.alias = "Medium".to_string();
    let mut override_functions = OverrideFunctionMap::default();
    override_functions.insert("Medium".to_string(), alias_target.clone());
    let marker = MemberFunctionCallMarker {
        tree: &tree,
        class_index: &class_index,
        override_functions: &override_functions,
    };

    assert_eq!(
        marker
            .mark_component_function_call(deferred_member_ref(
                ("Medium", ids.alias_pkg),
                "setState_phX",
            ))
            .target_def_id(),
        Some(ids.concrete_fn)
    );

    let override_packages = vec![alias_target];
    let no_override_functions = OverrideFunctionMap::default();
    let ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &no_override_functions,
    );

    let set_state_ref = rumoca_core::Reference::with_component_reference(
        "PartialMedium.setState_phX",
        core_comp_ref(&[("Medium", ids.alias_pkg), ("setState_phX", ids.concrete_fn)]),
    );
    let set_state = resolve_exact_function_rewrite(&set_state_ref, false, &ctx, test_span())
        .expect("exact selected package function")
        .expect("function selection");
    assert_eq!(set_state.selection.implementation, ids.concrete_fn);
    let concrete_component_ref = rumoca_core::Reference::with_component_reference(
        "PartialMedium.density",
        core_comp_ref(&[
            ("AliasMedium", ids.alias_pkg),
            ("density", ids.concrete_density),
        ]),
    );
    let density = resolve_exact_function_rewrite(&concrete_component_ref, false, &ctx, test_span())
        .expect("exact concrete package function")
        .expect("function selection");
    assert_eq!(density.selection.implementation, ids.concrete_density);
    let unscoped_ctx =
        FunctionOverrideRewriteContext::new(&tree, &class_index, &[], &no_override_functions);
    let unscoped_density =
        resolve_exact_function_rewrite(&concrete_component_ref, false, &unscoped_ctx, test_span())
            .expect("structured target is sufficient without a spelling context")
            .expect("function selection");
    assert_eq!(
        unscoped_density.selection.implementation,
        ids.concrete_density
    );

    let mut flat = flat_with_partial_density_function(ids);
    let component_override_map = root_component_override_map(&override_packages[0]);

    rewrite_function_overrides_in_flat_model(
        &mut flat,
        &tree,
        &class_index,
        &component_override_map,
        &component_member_scope::ComponentMemberScopes::default(),
    )
    .expect("function override rewrite");

    let function_name = rumoca_core::VarName::new("PartialMedium.density_phX");
    let Some(function) = flat.functions.get(&function_name) else {
        panic!("expected function to remain keyed by source name");
    };
    let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
        panic!("expected assignment statement");
    };
    let Expression::FunctionCall { name, .. } = value else {
        panic!("expected function call");
    };
    assert_eq!(name.as_str(), "PartialMedium.density");
    assert_eq!(name.target_def_id(), Some(ids.concrete_density));
}

#[derive(Clone, Copy)]
struct ConcreteOverrideChainIds {
    partial_pkg: DefId,
    partial_fn: DefId,
    concrete_pkg: DefId,
    concrete_fn: DefId,
    alias_pkg: DefId,
    partial_density: DefId,
    concrete_density: DefId,
}

fn concrete_override_chain_tree() -> (ClassTree, ConcreteOverrideChainIds) {
    let ids = ConcreteOverrideChainIds {
        partial_pkg: DefId::new(1),
        partial_fn: DefId::new(2),
        concrete_pkg: DefId::new(3),
        concrete_fn: DefId::new(4),
        alias_pkg: DefId::new(5),
        partial_density: DefId::new(6),
        concrete_density: DefId::new(7),
    };
    let partial_pkg = partial_medium_package(ids);
    let concrete_pkg = concrete_medium_package(ids);
    let alias_pkg = alias_medium_package(ids);
    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("PartialMedium".to_string(), partial_pkg);
    tree.definitions
        .classes
        .insert("ConcreteMedium".to_string(), concrete_pkg);
    tree.definitions
        .classes
        .insert("AliasMedium".to_string(), alias_pkg);
    for (def_id, name) in [
        (ids.partial_pkg, "PartialMedium"),
        (ids.partial_fn, "PartialMedium.setState_phX"),
        (ids.partial_density, "PartialMedium.density"),
        (ids.concrete_pkg, "ConcreteMedium"),
        (ids.concrete_fn, "ConcreteMedium.setState_phX"),
        (ids.concrete_density, "ConcreteMedium.density"),
        (ids.alias_pkg, "AliasMedium"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
    }
    for (name, def_id) in [
        ("PartialMedium.setState_phX", ids.partial_fn),
        ("PartialMedium.density", ids.partial_density),
        ("ConcreteMedium.setState_phX", ids.concrete_fn),
        ("ConcreteMedium.density", ids.concrete_density),
        ("AliasMedium.setState_phX", ids.concrete_fn),
        ("AliasMedium.density", ids.concrete_density),
    ] {
        tree.name_map.insert(name.to_string(), def_id);
    }
    (tree, ids)
}

fn partial_medium_package(ids: ConcreteOverrideChainIds) -> ClassDef {
    let mut partial_fn = class("setState_phX", ClassType::Function);
    partial_fn.def_id = Some(ids.partial_fn);
    let mut partial_density = class("density", ClassType::Function);
    partial_density.def_id = Some(ids.partial_density);
    let mut partial_pkg = class("PartialMedium", ClassType::Package);
    partial_pkg.def_id = Some(ids.partial_pkg);
    partial_pkg
        .classes
        .insert("setState_phX".to_string(), partial_fn);
    partial_pkg
        .classes
        .insert("density".to_string(), partial_density);
    partial_pkg
}

fn concrete_medium_package(ids: ConcreteOverrideChainIds) -> ClassDef {
    let mut concrete_fn = class("setState_phX", ClassType::Function);
    concrete_fn.def_id = Some(ids.concrete_fn);
    concrete_fn
        .algorithms
        .push(vec![rumoca_ir_ast::Statement::Return {
            token: token("return"),
        }]);
    let mut concrete_density = class("density", ClassType::Function);
    concrete_density.def_id = Some(ids.concrete_density);
    concrete_density
        .algorithms
        .push(vec![rumoca_ir_ast::Statement::Return {
            token: token("return"),
        }]);
    let mut concrete_pkg = class("ConcreteMedium", ClassType::Package);
    concrete_pkg.def_id = Some(ids.concrete_pkg);
    concrete_pkg.extends.push(Extend {
        base_name: Name {
            def_id: Some(ids.partial_pkg),
            ..Name::from_string("PartialMedium")
        },
        base_def_id: Some(ids.partial_pkg),
        ..Extend::default()
    });
    concrete_pkg
        .classes
        .insert("setState_phX".to_string(), concrete_fn);
    concrete_pkg
        .classes
        .insert("density".to_string(), concrete_density);
    concrete_pkg
}

fn alias_medium_package(ids: ConcreteOverrideChainIds) -> ClassDef {
    let mut alias_pkg = class("AliasMedium", ClassType::Package);
    alias_pkg.def_id = Some(ids.alias_pkg);
    alias_pkg.extends.push(Extend {
        base_name: Name {
            def_id: Some(ids.concrete_pkg),
            ..Name::from_string("ConcreteMedium")
        },
        base_def_id: Some(ids.concrete_pkg),
        ..Extend::default()
    });
    alias_pkg
}

fn flat_with_partial_density_function(ids: ConcreteOverrideChainIds) -> rumoca_ir_flat::Model {
    let mut flat = rumoca_ir_flat::Model::new();
    let mut function = rumoca_core::Function::new("PartialMedium.density_phX", test_span());
    function.body.push(rumoca_core::Statement::Assignment {
        comp: core_comp_ref(&[("d", DefId::new(8))]),
        value: Expression::FunctionCall {
            name: rumoca_core::Reference::with_component_reference(
                "PartialMedium.density",
                core_comp_ref(&[("Medium", ids.alias_pkg), ("density", ids.concrete_density)]),
            ),
            args: vec![core_var(&[("state", DefId::new(9))])],
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
    });
    flat.add_function(function);
    flat
}

fn root_component_override_map(alias_target: &OverrideTarget) -> ComponentOverrideMap {
    let mut component_override_map = ComponentOverrideMap::default();
    component_override_map.insert(
        ComponentPath::root(),
        [("Medium".to_string(), alias_target.clone())]
            .into_iter()
            .collect(),
    );
    component_override_map
}

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
            .mark_component_function_call(deferred_member_ref(
                ("world", world_def),
                "gravityAcceleration",
            ))
            .target_def_id(),
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
            .mark_component_function_call(
                deferred_member_ref(("Medium", ids.alias_pkg), "density",)
            )
            .target_def_id(),
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
            core_comp_ref(&[
                ("PartialMedium", ids.partial_pkg),
                ("density", ids.partial_density),
            ]),
        ),
        args: vec![core_var(&[("state", DefId::new(8))])],
        is_constructor: false,
        span: test_span(),
    };

    rewrite_function_overrides_in_expression(
        &mut expr,
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    )
    .expect("function override rewrite");

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
            .mark_component_function_call(deferred_member_ref(
                ("world", DefId::new(1)),
                "gravityAcceleration",
            ))
            .target_def_id(),
        None
    );
}
