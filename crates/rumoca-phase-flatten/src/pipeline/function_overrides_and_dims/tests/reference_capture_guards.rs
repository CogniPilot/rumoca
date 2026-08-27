//! Guards keeping non-package references (function locals, instance
//! paths, comprehension binders) out of override capture.

use super::*;

#[test]
fn function_override_rewrite_keeps_function_local_record_fields() {
    let package_def = DefId::new(1);
    let member_def = DefId::new(2);
    let local_def = DefId::new(3);
    let local_owner_def = DefId::new(4);

    let mut member = class("kappa", ClassType::Function);
    member.def_id = Some(member_def);
    let mut package = class("AliasMedium", ClassType::Package);
    package.def_id = Some(package_def);
    package.classes.insert("kappa".to_string(), member);

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("AliasMedium".to_string(), package);
    tree.def_map.insert(package_def, "AliasMedium".to_string());
    tree.def_map
        .insert(member_def, "AliasMedium.kappa".to_string());
    tree.name_map
        .insert("AliasMedium.kappa".to_string(), member_def);
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_packages = vec![override_target(
        "AliasMedium",
        package_def,
        ClassType::Package,
    )];
    let override_functions = OverrideFunctionMap::default();
    let local_ref = rumoca_core::Reference::with_component_reference(
        "sat.kappa",
        core_comp_ref(&[("sat", local_owner_def), ("kappa", local_def)]),
    );
    let expr = Expression::VarRef {
        name: local_ref,
        subscripts: Vec::new(),
        span: test_span(),
    };

    let no_locals_ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    );
    let Expression::VarRef { name, .. } =
        FunctionOverrideExpressionRewriter::new(&no_locals_ctx).rewrite_expression(&expr)
    else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "AliasMedium.kappa");

    let local_ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    )
    .with_local_def_ids(FxHashSet::from_iter([local_def]));
    let Expression::VarRef { name, .. } =
        FunctionOverrideExpressionRewriter::new(&local_ctx).rewrite_expression(&expr)
    else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "sat.kappa");
}

#[test]
fn active_package_member_rewrite_keeps_structured_instance_path() {
    let package_def = DefId::new(1);
    let state_def = DefId::new(2);
    let tank_def = DefId::new(3);
    let medium_def = DefId::new(4);
    let pressure_def = DefId::new(5);
    let mut state = class("state", ClassType::Record);
    state.def_id = Some(state_def);
    let mut package = class("ConcreteMedium", ClassType::Package);
    package.def_id = Some(package_def);
    package.classes.insert("state".to_string(), state);

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("ConcreteMedium".to_string(), package);
    tree.def_map
        .insert(package_def, "ConcreteMedium".to_string());
    tree.def_map
        .insert(state_def, "ConcreteMedium.state".to_string());

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_packages = vec![override_target(
        "ConcreteMedium",
        package_def,
        ClassType::Package,
    )];
    let override_functions = OverrideFunctionMap::default();
    let mut component_members = component_member_scope::ComponentMemberScopes::default();
    component_members
        .insert_component_member_path(&ComponentPath::from_flat_path("tank.medium.state.p"));
    let ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    )
    .with_component_member_scope(&component_members);
    let mut expr = Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(core_comp_ref(&[
            ("tank", tank_def),
            ("medium", medium_def),
            ("state", state_def),
        ])),
        subscripts: vec![],
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("function override rewrite");

    let Expression::VarRef { name, .. } = expr else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "tank.medium.state");

    let mut field_expr = Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(core_comp_ref(&[
            ("tank", tank_def),
            ("medium", medium_def),
            ("state", state_def),
            ("p", pressure_def),
        ])),
        subscripts: vec![],
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut field_expr, &ctx)
        .expect("function override rewrite");

    let Expression::VarRef { name, .. } = field_expr else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "tank.medium.state.p");
}

#[test]
fn concrete_package_component_ref_is_not_canonicalized_to_inherited_partial_name() {
    let partial_package_def = DefId::new(1);
    let partial_function_def = DefId::new(2);
    let concrete_package_def = DefId::new(3);

    let mut partial_function = class("temperature_psX", ClassType::Function);
    partial_function.def_id = Some(partial_function_def);
    let mut partial_package = class("PartialMedium", ClassType::Package);
    partial_package.def_id = Some(partial_package_def);
    partial_package
        .classes
        .insert("temperature_psX".to_string(), partial_function);

    let mut concrete_package = class("Air_pT", ClassType::Package);
    concrete_package.def_id = Some(concrete_package_def);
    concrete_package.extends.push(Extend {
        base_name: Name::from_string("PartialMedium"),
        base_def_id: Some(partial_package_def),
        ..Extend::default()
    });

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("PartialMedium".to_string(), partial_package);
    tree.definitions
        .classes
        .insert("Air_pT".to_string(), concrete_package);
    tree.def_map
        .insert(partial_package_def, "PartialMedium".to_string());
    tree.def_map.insert(
        partial_function_def,
        "PartialMedium.temperature_psX".to_string(),
    );
    tree.def_map
        .insert(concrete_package_def, "Air_pT".to_string());

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_packages = Vec::new();
    let override_functions = OverrideFunctionMap::default();
    let ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    );

    let component_ref = core_comp_ref(&[
        ("Air_pT", concrete_package_def),
        ("temperature_psX", partial_function_def),
    ]);
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "Air_pT.temperature_psX",
            component_ref,
        ),
        args: Vec::new(),
        is_constructor: false,
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("function override rewrite");

    let Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(name.as_str(), "Air_pT.temperature_psX");
    assert_eq!(name.target_def_id(), Some(partial_function_def));
}

#[test]
fn comprehension_binder_is_not_captured_by_active_override() {
    let package_def = DefId::new(1);
    let member_def = DefId::new(2);

    let mut member = component("i", "Integer", DefId::new(3));
    member.def_id = Some(member_def);
    let mut package = class("ConcreteMedium", ClassType::Package);
    package.def_id = Some(package_def);
    package.components.insert("i".to_string(), member);

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("ConcreteMedium".to_string(), package);
    tree.def_map
        .insert(package_def, "ConcreteMedium".to_string());
    tree.def_map
        .insert(member_def, "ConcreteMedium.i".to_string());

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let mut override_package = override_target("ConcreteMedium", package_def, ClassType::Package);
    override_package.alias = "Medium".to_string();
    let override_packages = vec![override_package];
    let override_functions = OverrideFunctionMap::default();
    let ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    );

    let mut expr = Expression::ArrayComprehension {
        expr: Box::new(core_var(&[("i", DefId::new(4))])),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: Expression::Range {
                start: Box::new(Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: test_span(),
                }),
                step: None,
                end: Box::new(Expression::Literal {
                    value: rumoca_core::Literal::Integer(3),
                    span: test_span(),
                }),
                span: test_span(),
            },
        }],
        filter: None,
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("function override rewrite");

    let Expression::ArrayComprehension { expr, .. } = expr else {
        panic!("expected array comprehension");
    };
    let Expression::VarRef { name, .. } = *expr else {
        panic!("expected comprehension body var ref");
    };
    assert_eq!(name.as_str(), "i");
}
