//! Package constant/member references resolved through the active
//! component override scope.

use super::*;

#[test]
fn package_constant_member_uses_active_component_override_scope() {
    let partial_package_def = DefId::new(1);
    let fluid_constants_def = DefId::new(2);
    let concrete_package_def = DefId::new(3);
    let source_def = DefId::new(11);
    let medium_def = DefId::new(12);

    let mut fluid_constants = component("fluidConstants", "FluidConstants", DefId::new(10));
    fluid_constants.def_id = Some(fluid_constants_def);
    let mut partial_package = class("PartialMedium", ClassType::Package);
    partial_package.def_id = Some(partial_package_def);
    partial_package
        .components
        .insert("fluidConstants".to_string(), fluid_constants);

    let mut concrete_package = class("ConcreteMedium", ClassType::Package);
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
        .insert("ConcreteMedium".to_string(), concrete_package);
    tree.def_map
        .insert(partial_package_def, "PartialMedium".to_string());
    tree.def_map.insert(
        fluid_constants_def,
        "PartialMedium.fluidConstants".to_string(),
    );
    tree.def_map
        .insert(concrete_package_def, "ConcreteMedium".to_string());
    tree.name_map.insert(
        "PartialMedium.fluidConstants".to_string(),
        fluid_constants_def,
    );
    tree.name_map.insert(
        "ConcreteMedium.fluidConstants".to_string(),
        fluid_constants_def,
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

    let component_ref = core_comp_ref(&[
        ("source", source_def),
        ("medium", medium_def),
        ("fluidConstants", fluid_constants_def),
    ]);
    let mut expr = Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            "source.medium.fluidConstants",
            component_ref,
        ),
        subscripts: Vec::new(),
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("function override rewrite");

    let Expression::VarRef { name, .. } = expr else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "ConcreteMedium.fluidConstants");
}

#[test]
fn package_constant_member_with_exact_identity_uses_active_override_scope() {
    let partial_package_def = DefId::new(1);
    let fluid_constants_def = DefId::new(2);
    let concrete_package_def = DefId::new(3);
    let source_def = DefId::new(11);
    let medium_def = DefId::new(12);

    let mut fluid_constants = component("fluidConstants", "FluidConstants", DefId::new(10));
    fluid_constants.def_id = Some(fluid_constants_def);
    let mut partial_package = class("PartialMedium", ClassType::Package);
    partial_package.def_id = Some(partial_package_def);
    partial_package
        .components
        .insert("fluidConstants".to_string(), fluid_constants);

    let mut concrete_package = class("ConcreteMedium", ClassType::Package);
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
        .insert("ConcreteMedium".to_string(), concrete_package);
    tree.def_map
        .insert(partial_package_def, "PartialMedium".to_string());
    tree.def_map.insert(
        fluid_constants_def,
        "PartialMedium.fluidConstants".to_string(),
    );
    tree.def_map
        .insert(concrete_package_def, "ConcreteMedium".to_string());
    tree.name_map.insert(
        "PartialMedium.fluidConstants".to_string(),
        fluid_constants_def,
    );
    tree.name_map.insert(
        "ConcreteMedium.fluidConstants".to_string(),
        fluid_constants_def,
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

    let component_ref = core_comp_ref(&[
        ("source", source_def),
        ("medium", medium_def),
        ("fluidConstants", fluid_constants_def),
    ]);
    let mut expr = Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            "source.medium.fluidConstants",
            component_ref,
        ),
        subscripts: Vec::new(),
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("function override rewrite");

    let Expression::VarRef { name, .. } = expr else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "ConcreteMedium.fluidConstants");
}

#[test]
fn qualified_package_member_with_exact_identity_is_not_captured_by_active_override() {
    let modelica_def = DefId::new(1);
    let constants_def = DefId::new(2);
    let concrete_package_def = DefId::new(3);
    let epsilon_def = DefId::new(4);

    let mut constants = class("Constants", ClassType::Package);
    constants.def_id = Some(constants_def);
    let mut epsilon = component("eps", "Real", DefId::new(5));
    epsilon.def_id = Some(epsilon_def);
    constants.components.insert("eps".to_string(), epsilon);
    let mut modelica = class("Modelica", ClassType::Package);
    modelica.def_id = Some(modelica_def);
    modelica.classes.insert("Constants".to_string(), constants);
    let mut concrete_package = class("ConcreteMedium", ClassType::Package);
    concrete_package.def_id = Some(concrete_package_def);

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("Modelica".to_string(), modelica);
    tree.definitions
        .classes
        .insert("ConcreteMedium".to_string(), concrete_package);

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

    let component_ref = core_comp_ref(&[
        ("Modelica", modelica_def),
        ("Constants", constants_def),
        ("eps", epsilon_def),
    ]);
    let mut expr = Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            "Modelica.Constants.eps",
            component_ref,
        ),
        subscripts: Vec::new(),
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("function override rewrite");

    let Expression::VarRef { name, .. } = expr else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "Modelica.Constants.eps");
}

#[test]
fn package_constant_field_projection_uses_concrete_package_scope() {
    let partial_package_def = DefId::new(1);
    let data_def = DefId::new(2);
    let concrete_package_def = DefId::new(3);
    let data_type_def = DefId::new(4);
    let molar_mass_def = DefId::new(5);

    let mut data = component("data", "DataRecord", data_type_def);
    data.def_id = Some(data_def);
    let mut molar_mass = component("MM", "Real", DefId::new(6));
    molar_mass.def_id = Some(molar_mass_def);
    let mut data_type = class("DataRecord", ClassType::Record);
    data_type.def_id = Some(data_type_def);
    data_type.components.insert("MM".to_string(), molar_mass);
    let mut partial_package = class("PartialMedium", ClassType::Package);
    partial_package.def_id = Some(partial_package_def);
    partial_package.components.insert("data".to_string(), data);

    let mut concrete_package = class("ConcreteMedium", ClassType::Package);
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
        .insert("DataRecord".to_string(), data_type);
    tree.definitions
        .classes
        .insert("ConcreteMedium".to_string(), concrete_package);
    tree.def_map
        .insert(partial_package_def, "PartialMedium".to_string());
    tree.def_map
        .insert(data_def, "PartialMedium.data".to_string());
    tree.def_map
        .insert(concrete_package_def, "ConcreteMedium".to_string());
    tree.def_map.insert(data_type_def, "DataRecord".to_string());
    tree.def_map
        .insert(molar_mass_def, "DataRecord.MM".to_string());
    tree.name_map
        .insert("ConcreteMedium.data.MM".to_string(), molar_mass_def);

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_packages = vec![override_target_with_active(
        "ConcreteMedium",
        concrete_package_def,
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

    let mut expr = Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            "PartialMedium.data.MM",
            core_comp_ref(&[
                ("PartialMedium", partial_package_def),
                ("data", data_def),
                ("MM", molar_mass_def),
            ]),
        ),
        subscripts: Vec::new(),
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("function override rewrite");

    let Expression::VarRef { name, .. } = expr else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "ConcreteMedium.data.MM");
}
