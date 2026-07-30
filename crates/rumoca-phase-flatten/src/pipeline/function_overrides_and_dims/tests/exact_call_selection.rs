//! Exact callable selection: which declaration a call occurrence
//! exposes and which implementation it selects.

use super::*;

#[test]
fn fully_qualified_sibling_package_call_is_not_aliased_to_self() {
    // Regression: a function `A.Quat.inverse` that calls the fully-qualified
    // sibling `B.Quat.inverse` must NOT have that call rewritten to its own
    // package (`A.Quat.inverse`). The two packages share the leaf package name
    // `Quat`; exact prefix and target DefIds prove that the call belongs to
    // `B.Quat`, independent of the caller's equally spelled package exposure.
    let a_def = DefId::new(1);
    let a_quat_def = DefId::new(2);
    let a_inverse_def = DefId::new(3);
    let b_def = DefId::new(4);
    let b_quat_def = DefId::new(5);
    let b_inverse_def = DefId::new(6);

    let mut a_inverse = class("inverse", ClassType::Function);
    a_inverse.def_id = Some(a_inverse_def);
    let mut a_quat = class("Quat", ClassType::Package);
    a_quat.def_id = Some(a_quat_def);
    a_quat.classes.insert("inverse".to_string(), a_inverse);
    let mut a = class("A", ClassType::Package);
    a.def_id = Some(a_def);
    a.classes.insert("Quat".to_string(), a_quat);

    let mut b_inverse = class("inverse", ClassType::Function);
    b_inverse.def_id = Some(b_inverse_def);
    let mut b_quat = class("Quat", ClassType::Package);
    b_quat.def_id = Some(b_quat_def);
    b_quat.classes.insert("inverse".to_string(), b_inverse);
    let mut b = class("B", ClassType::Package);
    b.def_id = Some(b_def);
    b.classes.insert("Quat".to_string(), b_quat);

    let mut tree = ClassTree::new();
    tree.definitions.classes.insert("A".to_string(), a);
    tree.definitions.classes.insert("B".to_string(), b);
    for (def_id, name) in [
        (a_def, "A"),
        (a_quat_def, "A.Quat"),
        (a_inverse_def, "A.Quat.inverse"),
        (b_def, "B"),
        (b_quat_def, "B.Quat"),
        (b_inverse_def, "B.Quat.inverse"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    // The caller's own package (`A.Quat`) is supplied as the (inactive) override
    // package, mirroring `rewrite_function_extends_aliases_in_function`. Its
    // alias is the leaf segment `Quat`, which also matches the call's parent.
    let override_packages = vec![override_target_with_active(
        "A.Quat",
        a_quat_def,
        ClassType::Package,
        false,
    )];
    let override_functions = OverrideFunctionMap::default();
    let ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    );

    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "B.Quat.inverse",
            core_comp_ref(&[
                ("B", b_def),
                ("Quat", b_quat_def),
                ("inverse", b_inverse_def),
            ]),
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
    assert_eq!(name.as_str(), "B.Quat.inverse");
}

#[test]
fn function_retarget_defers_display_canonicalization_until_instance_selection() {
    let package_def = DefId::new(1);
    let function_def = DefId::new(2);
    let mut function = class("specificEnthalpy_pTX", ClassType::Function);
    function.def_id = Some(function_def);
    let mut package = class("Pkg", ClassType::Package);
    package.def_id = Some(package_def);
    package
        .classes
        .insert("specificEnthalpy_pTX".to_string(), function);

    let mut tree = ClassTree::new();
    tree.definitions.classes.insert("Pkg".to_string(), package);
    tree.def_map.insert(package_def, "Pkg".to_string());
    tree.def_map
        .insert(function_def, "Pkg.specificEnthalpy_pTX".to_string());

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_packages = Vec::new();
    let override_functions = OverrideFunctionMap::default();
    let ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    );

    let component_ref = core_comp_ref(&[("specificEnthalpy_pTX", function_def)]);
    let occurrence = rumoca_core::InstanceId::new(17);
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "specificEnthalpy_pTX",
            component_ref,
        )
        .with_instance_id(occurrence),
        args: Vec::new(),
        is_constructor: false,
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("function override rewrite");

    let Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(name.as_str(), "specificEnthalpy_pTX");
    assert_eq!(name.target_def_id(), Some(function_def));
    assert_eq!(name.instance_id(), Some(occurrence));
    assert_eq!(
        name.component_ref()
            .expect("rewritten function reference remains structured")
            .to_var_name()
            .as_str(),
        "specificEnthalpy_pTX"
    );
}

#[test]
fn redeclared_function_retarget_preserves_occurrence_path_and_instance_identity() {
    let package_def = DefId::new(20);
    let concrete_def = DefId::new(21);
    let world_def = DefId::new(22);
    let replaceable_def = DefId::new(23);
    let occurrence = rumoca_core::InstanceId::new(24);

    let mut concrete = class("StandardGravity", ClassType::Function);
    concrete.def_id = Some(concrete_def);
    let mut package = class("P", ClassType::Package);
    package.def_id = Some(package_def);
    package
        .classes
        .insert("StandardGravity".to_string(), concrete);
    let mut tree = ClassTree::new();
    tree.definitions.classes.insert("P".to_string(), package);
    tree.def_map.insert(package_def, "P".to_string());
    tree.def_map
        .insert(concrete_def, "P.StandardGravity".to_string());
    tree.name_map
        .insert("P.StandardGravity".to_string(), concrete_def);
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);

    let original = rumoca_core::Reference::with_component_reference(
        "world.gravity",
        core_comp_ref(&[("world", world_def), ("gravity", replaceable_def)]),
    )
    .with_instance_id(occurrence);
    let rewritten = rewritten_function_reference(
        &original,
        "P.StandardGravity".to_string(),
        &tree,
        &class_index,
    );

    assert_eq!(rewritten.as_str(), "P.StandardGravity");
    assert_eq!(rewritten.instance_id(), Some(occurrence));
    let path = rewritten
        .component_ref()
        .expect("redeclared function occurrence remains structured");
    assert_eq!(path.to_var_name().as_str(), "world.gravity");
    assert_eq!(path.root_def_id(), world_def);
    assert_eq!(path.target_def_id(), concrete_def);
}

#[test]
fn unqualified_partial_package_call_uses_active_component_override_scope() {
    let partial_package_def = DefId::new(1);
    let partial_function_def = DefId::new(2);
    let concrete_package_def = DefId::new(3);
    let concrete_function_def = DefId::new(4);

    let mut partial_function = class("specificEnthalpy", ClassType::Function);
    partial_function.def_id = Some(partial_function_def);
    partial_function.partial = true;
    let mut partial_package = class("PartialMedium", ClassType::Package);
    partial_package.def_id = Some(partial_package_def);
    partial_package
        .classes
        .insert("specificEnthalpy".to_string(), partial_function);

    let mut concrete_function = class("specificEnthalpy", ClassType::Function);
    concrete_function.def_id = Some(concrete_function_def);
    let mut concrete_package = class("ConcreteMedium", ClassType::Package);
    concrete_package.def_id = Some(concrete_package_def);
    concrete_package.extends.push(Extend {
        base_name: Name::from_string("PartialMedium"),
        base_def_id: Some(partial_package_def),
        ..Extend::default()
    });
    concrete_package
        .classes
        .insert("specificEnthalpy".to_string(), concrete_function);

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("PartialMedium".to_string(), partial_package);
    tree.definitions
        .classes
        .insert("ConcreteMedium".to_string(), concrete_package);
    tree.def_map
        .insert(partial_package_def, "PartialMedium".to_string());
    tree.def_map.insert(
        partial_function_def,
        "PartialMedium.specificEnthalpy".to_string(),
    );
    tree.def_map
        .insert(concrete_package_def, "ConcreteMedium".to_string());
    tree.def_map.insert(
        concrete_function_def,
        "ConcreteMedium.specificEnthalpy".to_string(),
    );

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let mut override_package =
        override_target("ConcreteMedium", concrete_package_def, ClassType::Package);
    override_package.alias = "Medium".to_string();
    let override_packages = vec![override_package];
    let override_functions = OverrideFunctionMap::default();
    let ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    );

    let component_ref = core_comp_ref(&[("specificEnthalpy", partial_function_def)]);
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference("specificEnthalpy", component_ref),
        args: Vec::new(),
        is_constructor: false,
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("function override rewrite");

    let Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(name.as_str(), "ConcreteMedium.specificEnthalpy");
    assert_eq!(name.target_def_id(), Some(concrete_function_def));
}

#[test]
fn replaceable_constructor_uses_exact_target_not_same_spelling_override() {
    let package_a = DefId::new(180);
    let record_a = DefId::new(181);
    let package_b = DefId::new(182);
    let record_b = DefId::new(183);
    let mut a_record = class("R", ClassType::Record);
    a_record.def_id = Some(record_a);
    let mut a = class("A", ClassType::Package);
    a.def_id = Some(package_a);
    a.classes.insert("R".to_string(), a_record);
    let mut b_record = class("R", ClassType::Record);
    b_record.def_id = Some(record_b);
    let mut b = class("B", ClassType::Package);
    b.def_id = Some(package_b);
    b.classes.insert("R".to_string(), b_record);
    let mut tree = ClassTree::new();
    tree.definitions.classes.insert("A".to_string(), a);
    tree.definitions.classes.insert("B".to_string(), b);
    for (def_id, name) in [
        (package_a, "A"),
        (record_a, "A.R"),
        (package_b, "B"),
        (record_b, "B.R"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let mut overrides = OverrideFunctionMap::default();
    overrides.insert(
        "R".to_string(),
        override_target("B.R", record_b, ClassType::Record),
    );
    let ctx = FunctionOverrideRewriteContext::new(&tree, &class_index, &[], &overrides);
    let mut expression = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "R",
            core_comp_ref(&[("R", record_a)]),
        ),
        args: Vec::new(),
        is_constructor: true,
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expression, &ctx)
        .expect("exact constructor selection");

    let Expression::FunctionCall { name, .. } = expression else {
        panic!("expected constructor call");
    };
    assert_eq!(name.as_str(), "R");
    assert_eq!(name.target_def_id(), Some(record_a));
}
