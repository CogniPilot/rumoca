use super::*;
use rumoca_core::{ClassType, ComponentPath, DefId, Span, Token};
use rumoca_ir_ast::{
    ClassDef, ClassInstanceData, ClassTree, Component, ComponentRefPart, ComponentReference,
    Extend, Name, QualifiedName, ScopeKind,
};
use std::sync::Arc;

fn token(text: &str) -> Token {
    Token {
        text: Arc::from(text),
        ..Token::default()
    }
}

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_overrides_test.mo"),
        1,
        2,
    )
}

fn class(name: &str, class_type: ClassType) -> ClassDef {
    ClassDef {
        name: token(name),
        class_type,
        ..ClassDef::default()
    }
}

fn component(name: &str, type_name: &str, type_def_id: DefId) -> Component {
    Component {
        name: name.to_string(),
        type_name: Name::from_string(type_name),
        type_def_id: Some(type_def_id),
        ..Component::empty_with_span(test_span())
    }
}

fn override_target(name: &str, def_id: DefId, class_type: ClassType) -> OverrideTarget {
    override_target_with_active(name, def_id, class_type, true)
}

fn override_target_with_active(
    name: &str,
    def_id: DefId,
    class_type: ClassType,
    active: bool,
) -> OverrideTarget {
    OverrideTarget {
        alias: leaf_segment(name).to_string(),
        name: name.to_string(),
        def_id,
        class_type,
        active,
        modifier_args: Vec::new(),
    }
}

fn comp_ref(parts: &[&str]) -> ComponentReference {
    ComponentReference {
        local: false,
        parts: parts
            .iter()
            .map(|part| ComponentRefPart {
                ident: token(part),
                subs: None,
            })
            .collect(),
        span: test_span(),
        def_id: None,
    }
}

fn ast_var(name: &str) -> rumoca_ir_ast::Expression {
    rumoca_ir_ast::Expression::ComponentReference(comp_ref(&[name]))
}

fn core_var(name: &str) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: Span::DUMMY,
    }
}

#[test]
fn function_override_rewrite_keeps_function_local_record_fields() {
    let package_def = DefId::new(1);
    let member_def = DefId::new(2);
    let local_def = DefId::new(3);

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
        rumoca_core::ComponentReference {
            def_id: Some(local_def),
            ..core_comp_ref(&["sat", "kappa"])
        },
    );
    let expr = Expression::VarRef {
        name: local_ref,
        subscripts: Vec::new(),
        span: Span::DUMMY,
    };

    let no_locals_ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    );
    let Expression::VarRef { name, .. } = (FunctionOverrideExpressionRewriter {
        ctx: &no_locals_ctx,
    })
    .rewrite_expression(&expr) else {
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
        (FunctionOverrideExpressionRewriter { ctx: &local_ctx }).rewrite_expression(&expr)
    else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "sat.kappa");
}

fn core_comp_ref(parts: &[&str]) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: Span::DUMMY,
        parts: parts
            .iter()
            .map(|part| rumoca_core::ComponentRefPart {
                ident: part.to_string(),
                span: Span::DUMMY,
                subs: Vec::new(),
            })
            .collect(),
        def_id: None,
    }
}

fn named_arg(expr: &Expression) -> Option<(&str, &Expression)> {
    let Expression::FunctionCall { name, args, .. } = expr else {
        return None;
    };
    Some((
        name.as_str().strip_prefix("__rumoca_named_arg__.")?,
        args.first()?,
    ))
}

#[test]
fn active_package_member_rewrite_keeps_structured_instance_path() {
    let package_def = DefId::new(1);
    let state_def = DefId::new(2);
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
            "tank", "medium", "state",
        ])),
        subscripts: vec![],
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx);

    let Expression::VarRef { name, .. } = expr else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "tank.medium.state");

    let mut field_expr = Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(core_comp_ref(&[
            "tank", "medium", "state", "p",
        ])),
        subscripts: vec![],
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut field_expr, &ctx);

    let Expression::VarRef { name, .. } = field_expr else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "tank.medium.state.p");
}

#[test]
fn fully_qualified_sibling_package_call_is_not_aliased_to_self() {
    // Regression: a function `A.Quat.inverse` that calls the fully-qualified
    // sibling `B.Quat.inverse` must NOT have that call rewritten to its own
    // package (`A.Quat.inverse`). The two packages share the leaf package name
    // `Quat`, so the alias-matched fallback in `resolve_override_function_name`
    // previously re-resolved the call's leaf (`inverse`) inside the caller's
    // own package, aliasing the cross-package call to itself.
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
            core_comp_ref(&["B", "Quat", "inverse"]),
        ),
        args: Vec::new(),
        is_constructor: false,
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx);

    let Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(name.as_str(), "B.Quat.inverse");
}

#[test]
fn canonicalizes_single_part_function_call_from_target_def_id() {
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

    let mut component_ref = core_comp_ref(&["specificEnthalpy_pTX"]);
    component_ref.def_id = Some(function_def);
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "specificEnthalpy_pTX",
            component_ref,
        ),
        args: Vec::new(),
        is_constructor: false,
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx);

    let Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(name.as_str(), "Pkg.specificEnthalpy_pTX");
    assert_eq!(name.target_def_id(), Some(function_def));
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

    let mut component_ref = core_comp_ref(&["Air_pT", "temperature_psX"]);
    component_ref.def_id = Some(partial_function_def);
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "Air_pT.temperature_psX",
            component_ref,
        ),
        args: Vec::new(),
        is_constructor: false,
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx);

    let Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(name.as_str(), "Air_pT.temperature_psX");
    assert_eq!(name.target_def_id(), Some(partial_function_def));
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

    let mut component_ref = core_comp_ref(&["specificEnthalpy"]);
    component_ref.def_id = Some(partial_function_def);
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference("specificEnthalpy", component_ref),
        args: Vec::new(),
        is_constructor: false,
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx);

    let Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(name.as_str(), "ConcreteMedium.specificEnthalpy");
    assert_eq!(name.target_def_id(), Some(concrete_function_def));
}

#[test]
fn package_constant_member_uses_active_component_override_scope() {
    let partial_package_def = DefId::new(1);
    let fluid_constants_def = DefId::new(2);
    let concrete_package_def = DefId::new(3);

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

    let mut component_ref = core_comp_ref(&["source", "medium", "fluidConstants"]);
    component_ref.def_id = Some(fluid_constants_def);
    let mut expr = Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            "source.medium.fluidConstants",
            component_ref,
        ),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx);

    let Expression::VarRef { name, .. } = expr else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "ConcreteMedium.fluidConstants");
}

#[test]
fn package_constant_member_without_def_id_uses_unique_active_override_scope() {
    let partial_package_def = DefId::new(1);
    let fluid_constants_def = DefId::new(2);
    let concrete_package_def = DefId::new(3);

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

    let component_ref = core_comp_ref(&["source", "medium", "fluidConstants"]);
    let mut expr = Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            "source.medium.fluidConstants",
            component_ref,
        ),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx);

    let Expression::VarRef { name, .. } = expr else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "ConcreteMedium.fluidConstants");
}

#[test]
fn package_constant_field_projection_uses_concrete_package_scope() {
    let partial_package_def = DefId::new(1);
    let data_def = DefId::new(2);
    let concrete_package_def = DefId::new(3);
    let data_type_def = DefId::new(4);

    let mut data = component("data", "DataRecord", data_type_def);
    data.def_id = Some(data_def);
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
        .insert("ConcreteMedium".to_string(), concrete_package);
    tree.def_map
        .insert(partial_package_def, "PartialMedium".to_string());
    tree.def_map
        .insert(data_def, "PartialMedium.data".to_string());
    tree.def_map
        .insert(concrete_package_def, "ConcreteMedium".to_string());

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
            core_comp_ref(&["PartialMedium", "data", "MM"]),
        ),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx);

    let Expression::VarRef { name, .. } = expr else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "ConcreteMedium.data.MM");
}

#[test]
fn replaceable_function_alias_preserves_modifier_actuals() {
    let gravity_def = DefId::new(1);
    let standard_def = DefId::new(2);
    let world_def = DefId::new(3);
    let mut gravity = class("gravityAcceleration", ClassType::Function);
    gravity.def_id = Some(gravity_def);
    gravity.extends.push(Extend {
        base_name: Name::from_string("Standard"),
        base_def_id: Some(standard_def),
        modifications: vec![rumoca_ir_ast::ExtendModification {
            expr: rumoca_ir_ast::Expression::Modification {
                target: comp_ref(&["gravityType"]),
                value: Arc::new(ast_var("gravityType")),
                span: test_span(),
            },
            each: false,
            final_: false,
            redeclare: false,
        }],
        ..Extend::default()
    });

    let mut world = class("World", ClassType::Model);
    world.def_id = Some(world_def);
    world
        .classes
        .insert("gravityAcceleration".to_string(), gravity);
    let mut tree = ClassTree::new();
    tree.definitions.classes.insert("World".to_string(), world);
    let mut standard = class("Standard", ClassType::Function);
    standard.def_id = Some(standard_def);
    standard
        .algorithms
        .push(vec![rumoca_ir_ast::Statement::Return {
            token: token("return"),
        }]);
    tree.definitions
        .classes
        .insert("Standard".to_string(), standard);
    tree.def_map
        .insert(gravity_def, "World.gravityAcceleration".to_string());
    tree.def_map.insert(standard_def, "Standard".to_string());
    tree.def_map.insert(world_def, "World".to_string());
    tree.name_map
        .insert("World.gravityAcceleration".to_string(), gravity_def);
    tree.name_map.insert("Standard".to_string(), standard_def);
    tree.name_map.insert("World".to_string(), world_def);

    let mut override_functions = OverrideFunctionMap::default();
    override_functions.insert(
        "world".to_string(),
        override_target("World", world_def, ClassType::Model),
    );
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let ctx = FunctionOverrideRewriteContext::new(&tree, &class_index, &[], &override_functions);
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::new("World.gravityAcceleration"),
        args: vec![core_var("r")],
        is_constructor: false,
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx);

    let Expression::FunctionCall { name, args, .. } = expr else {
        panic!("expected rewritten function call");
    };
    assert_eq!(name.as_str(), "Standard");
    assert_eq!(args.len(), 2);
    let Some(("gravityType", Expression::VarRef { name, .. })) = named_arg(&args[1]) else {
        panic!("expected receiver-qualified gravityType named argument");
    };
    assert_eq!(name.as_str(), "world.gravityType");
}

#[test]
fn redeclare_function_assignment_rewrites_call_with_modifier_actuals() {
    let quadratic_def = DefId::new(1);
    let partial_pump_def = DefId::new(2);
    let mut quadratic = class("quadraticFlow", ClassType::Function);
    quadratic.def_id = Some(quadratic_def);
    let mut partial_pump = class("PartialPump", ClassType::Model);
    partial_pump.def_id = Some(partial_pump_def);

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("quadraticFlow".to_string(), quadratic);
    tree.definitions
        .classes
        .insert("PartialPump".to_string(), partial_pump);
    tree.def_map
        .insert(quadratic_def, "quadraticFlow".to_string());
    tree.def_map.insert(
        partial_pump_def,
        "Modelica.Fluid.Machines.BaseClasses.PartialPump".to_string(),
    );
    tree.name_map
        .insert("quadraticFlow".to_string(), quadratic_def);

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let mut override_functions = OverrideFunctionMap::default();
    override_functions.insert(
        "flowCharacteristic".to_string(),
        OverrideTarget {
            alias: "flowCharacteristic".to_string(),
            name: "quadraticFlow".to_string(),
            def_id: quadratic_def,
            class_type: ClassType::Function,
            active: true,
            modifier_args: vec![FunctionModifierArg {
                name: "V_flow_nominal".to_string(),
                value: ast_var("V_flow_op"),
                span: Span::DUMMY,
            }],
        },
    );
    let ctx = FunctionOverrideRewriteContext::new(&tree, &class_index, &[], &override_functions)
        .with_active_scope(ComponentPath::from_flat_path("pump"));
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "Modelica.Fluid.Machines.BaseClasses.PartialPump.flowCharacteristic",
            core_comp_ref(&[
                "Modelica",
                "Fluid",
                "Machines",
                "BaseClasses",
                "PartialPump",
                "flowCharacteristic",
            ]),
        ),
        args: vec![core_var("pump.V_flow_single")],
        is_constructor: false,
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx);

    let Expression::FunctionCall { name, args, .. } = expr else {
        panic!("expected rewritten function call");
    };
    assert_eq!(name.as_str(), "quadraticFlow");
    assert_eq!(args.len(), 2);
    let Some(("V_flow_nominal", Expression::VarRef { name, .. })) = named_arg(&args[1]) else {
        panic!("expected receiver-qualified V_flow_nominal named argument");
    };
    assert_eq!(name.as_str(), "pump.V_flow_op");
}

#[test]
fn inherited_replaceable_function_call_keeps_declaration_modifier_actuals() {
    let (tree, efficiency_characteristic_def) = replaceable_efficiency_fixture();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_functions = OverrideFunctionMap::default();
    let ctx = FunctionOverrideRewriteContext::new(&tree, &class_index, &[], &override_functions)
        .with_active_scope(ComponentPath::from_flat_path("pump"));
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "Modelica.Fluid.Machines.BaseClasses.PartialPump.efficiencyCharacteristic",
            rumoca_core::ComponentReference {
                def_id: Some(efficiency_characteristic_def),
                ..core_comp_ref(&[
                    "Modelica",
                    "Fluid",
                    "Machines",
                    "BaseClasses",
                    "PartialPump",
                    "efficiencyCharacteristic",
                ])
            },
        ),
        args: vec![core_var("pump.V_flow_single")],
        is_constructor: false,
        span: Span::DUMMY,
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx);

    let Expression::FunctionCall { name, args, .. } = expr else {
        panic!("expected rewritten function call");
    };
    assert_eq!(
        name.as_str(),
        "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.constantEfficiency"
    );
    assert_eq!(args.len(), 2);
    let Some(("eta_nominal", Expression::Literal { value, .. })) = named_arg(&args[1]) else {
        panic!("expected eta_nominal declaration modifier argument");
    };
    assert_eq!(*value, rumoca_core::Literal::Real(0.8));
}

fn replaceable_efficiency_fixture() -> (ClassTree, DefId) {
    let base_efficiency_def = DefId::new(1);
    let constant_efficiency_def = DefId::new(2);
    let efficiency_characteristic_def = DefId::new(3);

    let pump_characteristics =
        replaceable_efficiency_pump_characteristics(base_efficiency_def, constant_efficiency_def);
    let partial_pump =
        replaceable_efficiency_partial_pump(efficiency_characteristic_def, constant_efficiency_def);
    let modelica = replaceable_efficiency_modelica_tree(pump_characteristics, partial_pump);
    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("Modelica".to_string(), modelica);
    register_replaceable_efficiency_names(
        &mut tree,
        base_efficiency_def,
        constant_efficiency_def,
        efficiency_characteristic_def,
    );
    (tree, efficiency_characteristic_def)
}

fn replaceable_efficiency_pump_characteristics(
    base_efficiency_def: DefId,
    constant_efficiency_def: DefId,
) -> ClassDef {
    let mut base_efficiency = class("baseEfficiency", ClassType::Function);
    base_efficiency.def_id = Some(base_efficiency_def);
    let mut constant_efficiency = class("constantEfficiency", ClassType::Function);
    constant_efficiency.def_id = Some(constant_efficiency_def);
    constant_efficiency
        .algorithms
        .push(vec![rumoca_ir_ast::Statement::Return {
            token: token("return"),
        }]);
    let mut pump_characteristics = class("PumpCharacteristics", ClassType::Package);
    pump_characteristics
        .classes
        .insert("baseEfficiency".to_string(), base_efficiency);
    pump_characteristics
        .classes
        .insert("constantEfficiency".to_string(), constant_efficiency);
    pump_characteristics
}

fn replaceable_efficiency_partial_pump(
    efficiency_characteristic_def: DefId,
    constant_efficiency_def: DefId,
) -> ClassDef {
    let mut efficiency_characteristic = class("efficiencyCharacteristic", ClassType::Function);
    efficiency_characteristic.def_id = Some(efficiency_characteristic_def);
    efficiency_characteristic.extends.push(Extend {
        base_name: Name {
            def_id: Some(constant_efficiency_def),
            ..Name::from_string("PumpCharacteristics.constantEfficiency")
        },
        base_def_id: Some(constant_efficiency_def),
        modifications: vec![eta_nominal_modifier()],
        ..Extend::default()
    });

    let mut partial_pump = class("PartialPump", ClassType::Model);
    partial_pump.classes.insert(
        "efficiencyCharacteristic".to_string(),
        efficiency_characteristic,
    );
    partial_pump
}

fn eta_nominal_modifier() -> rumoca_ir_ast::ExtendModification {
    rumoca_ir_ast::ExtendModification {
        expr: rumoca_ir_ast::Expression::NamedArgument {
            name: token("eta_nominal"),
            value: Arc::new(rumoca_ir_ast::Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
                token: token("0.8"),
                span: test_span(),
            }),
            span: test_span(),
        },
        each: false,
        final_: false,
        redeclare: false,
    }
}

fn replaceable_efficiency_modelica_tree(
    pump_characteristics: ClassDef,
    partial_pump: ClassDef,
) -> ClassDef {
    let mut base_classes = class("BaseClasses", ClassType::Package);
    base_classes
        .classes
        .insert("PumpCharacteristics".to_string(), pump_characteristics);
    base_classes
        .classes
        .insert("PartialPump".to_string(), partial_pump);
    let mut machines = class("Machines", ClassType::Package);
    machines
        .classes
        .insert("BaseClasses".to_string(), base_classes);
    let mut fluid = class("Fluid", ClassType::Package);
    fluid.classes.insert("Machines".to_string(), machines);
    let mut modelica = class("Modelica", ClassType::Package);
    modelica.classes.insert("Fluid".to_string(), fluid);
    modelica
}

fn register_replaceable_efficiency_names(
    tree: &mut ClassTree,
    base_efficiency_def: DefId,
    constant_efficiency_def: DefId,
    efficiency_characteristic_def: DefId,
) {
    tree.def_map.insert(
        base_efficiency_def,
        "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.baseEfficiency".to_string(),
    );
    tree.def_map.insert(
        constant_efficiency_def,
        "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.constantEfficiency".to_string(),
    );
    tree.def_map.insert(
        efficiency_characteristic_def,
        "Modelica.Fluid.Machines.BaseClasses.PartialPump.efficiencyCharacteristic".to_string(),
    );
    tree.name_map.insert(
        "Modelica.Fluid.Machines.BaseClasses.PumpCharacteristics.constantEfficiency".to_string(),
        constant_efficiency_def,
    );
}

#[test]
fn component_scope_inherits_type_extends_redeclare_function() {
    let partial_def = DefId::new(1);
    let controlled_def = DefId::new(2);
    let quadratic_def = DefId::new(3);

    let mut partial = class("PartialPump", ClassType::Model);
    partial.def_id = Some(partial_def);
    let mut controlled = class("ControlledPump", ClassType::Model);
    controlled.def_id = Some(controlled_def);
    let mut quadratic = class("quadraticFlow", ClassType::Function);
    quadratic.def_id = Some(quadratic_def);

    let mut quadratic_ref = comp_ref(&["quadraticFlow"]);
    quadratic_ref.def_id = Some(quadratic_def);
    controlled.extends.push(Extend {
        base_name: Name {
            def_id: Some(partial_def),
            ..Name::from_string("PartialPump")
        },
        base_def_id: Some(partial_def),
        modifications: vec![rumoca_ir_ast::ExtendModification {
            expr: rumoca_ir_ast::Expression::Binary {
                op: rumoca_core::OpBinary::Assign,
                lhs: Arc::new(rumoca_ir_ast::Expression::ComponentReference(comp_ref(&[
                    "flowCharacteristic",
                ]))),
                rhs: Arc::new(rumoca_ir_ast::Expression::FunctionCall {
                    comp: quadratic_ref,
                    args: vec![rumoca_ir_ast::Expression::NamedArgument {
                        name: token("V_flow_nominal"),
                        value: Arc::new(ast_var("V_flow_op")),
                        span: test_span(),
                    }],
                    span: test_span(),
                }),
                span: test_span(),
            },
            each: false,
            final_: false,
            redeclare: true,
        }],
        ..Extend::default()
    });

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("PartialPump".to_string(), partial);
    tree.definitions
        .classes
        .insert("ControlledPump".to_string(), controlled);
    tree.definitions
        .classes
        .insert("quadraticFlow".to_string(), quadratic);
    tree.def_map.insert(partial_def, "PartialPump".to_string());
    tree.def_map
        .insert(controlled_def, "ControlledPump".to_string());
    tree.def_map
        .insert(quadratic_def, "quadraticFlow".to_string());
    tree.name_map
        .insert("quadraticFlow".to_string(), quadratic_def);

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let mut overlay = InstanceOverlay::new();
    let pump_id = overlay.alloc_id();
    overlay.add_component(rumoca_ir_ast::InstanceData {
        instance_id: pump_id,
        qualified_name: QualifiedName::from_ident("pump"),
        type_def_id: Some(controlled_def),
        ..rumoca_ir_ast::InstanceData::default()
    });

    let override_map =
        build_component_override_map(&overlay, &tree, &class_index, "ControlledPump")
            .expect("component override map");
    let (_, override_functions) = override_context_for_scope("pump", &override_map);
    let target = override_functions
        .get("flowCharacteristic")
        .expect("expected ControlledPump redeclare in pump scope");
    assert_eq!(target.name, "quadraticFlow");
    assert_eq!(target.modifier_args.len(), 1);
    assert_eq!(target.modifier_args[0].name, "V_flow_nominal");
}

#[test]
fn inherited_function_body_rewrites_self_package_calls_to_exposed_package() {
    let (tree, mut function) = inherited_function_alias_rewrite_fixture();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);

    rewrite_function_extends_aliases_in_function(&mut function, &tree, &class_index)
        .expect("function alias rewrite");

    let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
        panic!("expected assignment");
    };
    let Expression::FunctionCall { name, args, .. } = value else {
        panic!("expected outer function call");
    };
    assert_eq!(name.as_str(), "ConcreteMedium.specificEnthalpy");
    let Expression::FunctionCall { name, .. } = &args[0] else {
        panic!("expected nested function call");
    };
    assert_eq!(name.as_str(), "ConcreteMedium.setState_pTX");
}

fn inherited_function_alias_rewrite_fixture() -> (ClassTree, rumoca_core::Function) {
    let partial_pkg_def = DefId::new(1);
    let base_pkg_def = DefId::new(2);
    let concrete_pkg_def = DefId::new(3);
    let partial_specific_def = DefId::new(4);
    let partial_set_state_def = DefId::new(5);
    let base_specific_def = DefId::new(6);
    let base_set_state_def = DefId::new(7);

    let mut partial_specific = class("specificEnthalpy", ClassType::Function);
    partial_specific.def_id = Some(partial_specific_def);
    let mut partial_set_state = class("setState_pTX", ClassType::Function);
    partial_set_state.def_id = Some(partial_set_state_def);
    let mut partial_pkg = class("PartialMedium", ClassType::Package);
    partial_pkg.def_id = Some(partial_pkg_def);
    partial_pkg
        .classes
        .insert("specificEnthalpy".to_string(), partial_specific);
    partial_pkg
        .classes
        .insert("setState_pTX".to_string(), partial_set_state);

    let mut base_specific = class("specificEnthalpy", ClassType::Function);
    base_specific.def_id = Some(base_specific_def);
    let mut base_set_state = class("setState_pTX", ClassType::Function);
    base_set_state.def_id = Some(base_set_state_def);
    let mut base_pkg = class("BaseMedium", ClassType::Package);
    base_pkg.def_id = Some(base_pkg_def);
    base_pkg.extends.push(Extend {
        base_name: Name {
            def_id: Some(partial_pkg_def),
            ..Name::from_string("PartialMedium")
        },
        base_def_id: Some(partial_pkg_def),
        ..Extend::default()
    });
    base_pkg
        .classes
        .insert("specificEnthalpy".to_string(), base_specific);
    base_pkg
        .classes
        .insert("setState_pTX".to_string(), base_set_state);

    let mut concrete_pkg = class("ConcreteMedium", ClassType::Package);
    concrete_pkg.def_id = Some(concrete_pkg_def);
    concrete_pkg.extends.push(Extend {
        base_name: Name {
            def_id: Some(base_pkg_def),
            ..Name::from_string("BaseMedium")
        },
        base_def_id: Some(base_pkg_def),
        ..Extend::default()
    });

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("PartialMedium".to_string(), partial_pkg);
    tree.definitions
        .classes
        .insert("BaseMedium".to_string(), base_pkg);
    tree.definitions
        .classes
        .insert("ConcreteMedium".to_string(), concrete_pkg);
    register_inherited_alias_rewrite_names(
        &mut tree,
        [
            (partial_pkg_def, "PartialMedium", false),
            (base_pkg_def, "BaseMedium", false),
            (concrete_pkg_def, "ConcreteMedium", false),
            (partial_specific_def, "PartialMedium.specificEnthalpy", true),
            (partial_set_state_def, "PartialMedium.setState_pTX", true),
            (base_specific_def, "BaseMedium.specificEnthalpy", true),
            (base_set_state_def, "BaseMedium.setState_pTX", true),
        ],
    );

    let mut partial_specific_ref = core_comp_ref(&["PartialMedium", "specificEnthalpy"]);
    partial_specific_ref.def_id = Some(partial_specific_def);
    let mut partial_set_state_ref = core_comp_ref(&["PartialMedium", "setState_pTX"]);
    partial_set_state_ref.def_id = Some(partial_set_state_def);
    let mut function =
        rumoca_core::Function::new("ConcreteMedium.specificEnthalpy_pTX", Span::DUMMY);
    function.body.push(rumoca_core::Statement::Assignment {
        comp: core_comp_ref(&["h"]),
        value: Expression::FunctionCall {
            name: rumoca_core::Reference::with_component_reference(
                "PartialMedium.specificEnthalpy",
                partial_specific_ref,
            ),
            args: vec![Expression::FunctionCall {
                name: rumoca_core::Reference::with_component_reference(
                    "PartialMedium.setState_pTX",
                    partial_set_state_ref,
                ),
                args: vec![core_var("p")],
                is_constructor: false,
                span: Span::DUMMY,
            }],
            is_constructor: false,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
    });

    (tree, function)
}

fn register_inherited_alias_rewrite_names(tree: &mut ClassTree, names: [(DefId, &str, bool); 7]) {
    for (def_id, name, add_name_map) in names {
        tree.def_map.insert(def_id, name.to_string());
        if add_name_map {
            tree.name_map.insert(name.to_string(), def_id);
        }
    }
}

#[test]
fn root_class_scope_inherits_member_function_receiver_types() {
    let world_def = DefId::new(1);
    let gravity_def = DefId::new(2);
    let base_def = DefId::new(3);
    let alias_def = DefId::new(4);

    let mut tree = ClassTree::new();
    let world_scope = tree
        .scope_tree
        .create_scope(tree.scope_tree.global(), ScopeKind::Class);
    let base_scope = tree
        .scope_tree
        .create_scope(tree.scope_tree.global(), ScopeKind::Class);
    let alias_scope = tree
        .scope_tree
        .create_scope(tree.scope_tree.global(), ScopeKind::Class);
    let gravity_scope = tree
        .scope_tree
        .create_scope(world_scope, ScopeKind::Function);

    tree.scope_tree.add_member(
        tree.scope_tree.global(),
        ComponentPath::from_flat_path("World"),
        world_def,
    );
    tree.scope_tree.add_member(
        tree.scope_tree.global(),
        ComponentPath::from_flat_path("BasePointMass"),
        base_def,
    );
    tree.scope_tree.add_member(
        tree.scope_tree.global(),
        ComponentPath::from_flat_path("AliasPointMass"),
        alias_def,
    );
    tree.scope_tree.add_member(
        world_scope,
        ComponentPath::from_flat_path("gravityAcceleration"),
        gravity_def,
    );

    let mut world = class("World", ClassType::Model);
    world.def_id = Some(world_def);
    world.scope_id = Some(world_scope);
    world.classes.insert(
        "gravityAcceleration".to_string(),
        ClassDef {
            def_id: Some(gravity_def),
            scope_id: Some(gravity_scope),
            ..class("gravityAcceleration", ClassType::Function)
        },
    );

    let mut base_point_mass = class("BasePointMass", ClassType::Model);
    base_point_mass.def_id = Some(base_def);
    base_point_mass.scope_id = Some(base_scope);
    base_point_mass
        .components
        .insert("world".to_string(), component("world", "World", world_def));

    let mut alias_point_mass = class("AliasPointMass", ClassType::Model);
    alias_point_mass.def_id = Some(alias_def);
    alias_point_mass.scope_id = Some(alias_scope);
    alias_point_mass.extends.push(Extend {
        base_name: Name::from_string("BasePointMass"),
        base_def_id: Some(base_def),
        ..Extend::default()
    });

    tree.definitions.classes.insert("World".to_string(), world);
    tree.definitions
        .classes
        .insert("BasePointMass".to_string(), base_point_mass);
    tree.definitions
        .classes
        .insert("AliasPointMass".to_string(), alias_point_mass);
    tree.def_map.insert(world_def, "World".to_string());
    tree.def_map.insert(base_def, "BasePointMass".to_string());
    tree.def_map.insert(alias_def, "AliasPointMass".to_string());
    tree.name_map
        .insert("World.gravityAcceleration".to_string(), gravity_def);

    let mut overlay = InstanceOverlay::new();
    overlay.add_class(ClassInstanceData {
        qualified_name: QualifiedName::new(),
        source_scope: Some(QualifiedName::from_ident("AliasPointMass")),
        source_scope_id: Some(alias_scope),
        ..ClassInstanceData::default()
    });

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_map =
        build_component_override_map(&overlay, &tree, &class_index, "AliasPointMass")
            .expect("component override map");
    let (_, override_functions) = override_context_for_scope("", &override_map);
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
fn active_redeclare_overrides_inherited_default_alias_for_same_name() {
    let concrete_medium = override_target("ConcreteMedium", DefId::new(1), ClassType::Package);
    let inherited_default =
        override_target_with_active("BaseClass.Medium", DefId::new(2), ClassType::Package, false);
    let mut component_override_map = ComponentOverrideMap::default();
    component_override_map.insert(
        ComponentPath::root(),
        [("Medium".to_string(), concrete_medium.clone())]
            .into_iter()
            .collect(),
    );
    component_override_map.insert(
        ComponentPath::from_flat_path("pipe"),
        [("Medium".to_string(), inherited_default)]
            .into_iter()
            .collect(),
    );

    let (override_packages, _) = override_context_for_scope("pipe", &component_override_map);

    assert_eq!(override_packages.len(), 1);
    assert_eq!(override_packages[0].name, concrete_medium.name);
    assert!(override_packages[0].active);
}

#[test]
fn component_class_override_activity_uses_source_redeclare_reference() {
    let medium_def = DefId::new(10);
    let partial_def = DefId::new(1);
    let concrete_def = DefId::new(2);
    let explicit_def = DefId::new(3);
    let forwarding_to_default = rumoca_ir_ast::ClassOverride::new(
        "Medium",
        medium_def,
        partial_def,
        Some(comp_ref(&["Medium"])),
    );
    let forwarding_to_concrete = rumoca_ir_ast::ClassOverride::new(
        "Medium",
        medium_def,
        concrete_def,
        Some(comp_ref(&["Medium"])),
    );
    let explicit_concrete = rumoca_ir_ast::ClassOverride::new(
        "Medium",
        medium_def,
        explicit_def,
        Some(comp_ref(&[
            "Modelica",
            "Media",
            "Water",
            "StandardWaterOnePhase",
        ])),
    );
    let inherited_default =
        override_target_with_active("BaseClass.Medium", partial_def, ClassType::Package, false);
    let concrete_class = class("StandardWaterOnePhase", ClassType::Package);
    let explicit_class = class("StandardWaterOnePhase", ClassType::Package);
    let default_class = class("Medium", ClassType::Package);
    let concrete_target = ResolvedClassRef {
        name: "Modelica.Media.Water.StandardWaterOnePhase".to_string(),
        def_id: concrete_def,
        class_def: &concrete_class,
    };
    let explicit_target = ResolvedClassRef {
        name: "Modelica.Media.Water.StandardWaterOnePhase".to_string(),
        def_id: explicit_def,
        class_def: &explicit_class,
    };
    let default_target = ResolvedClassRef {
        name: "Modelica.Fluid.Interfaces.PartialTwoPort.Medium".to_string(),
        def_id: partial_def,
        class_def: &default_class,
    };

    assert!(!component_class_override_is_active(
        &forwarding_to_default,
        Some(&inherited_default),
        &default_target,
    ));
    assert!(component_class_override_is_active(
        &forwarding_to_concrete,
        Some(&inherited_default),
        &concrete_target,
    ));
    assert!(component_class_override_is_active(
        &explicit_concrete,
        Some(&inherited_default),
        &explicit_target,
    ));
}

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
            .mark_component_function_call(comp_ref(&["Medium", "setState_phX"]))
            .def_id,
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
        core_comp_ref(&["Medium", "setState_phX"]),
    );
    assert_eq!(
        resolve_override_function_name(&set_state_ref, &ctx).as_deref(),
        Some("AliasMedium.setState_phX")
    );
    let concrete_component_ref = rumoca_core::Reference::with_component_reference(
        "PartialMedium.density",
        rumoca_core::ComponentReference {
            def_id: Some(ids.partial_density),
            ..core_comp_ref(&["AliasMedium", "density"])
        },
    );
    assert_eq!(
        resolve_override_function_name(&concrete_component_ref, &ctx).as_deref(),
        Some("AliasMedium.density")
    );
    let unscoped_ctx =
        FunctionOverrideRewriteContext::new(&tree, &class_index, &[], &no_override_functions);
    assert_eq!(
        resolve_function_override_name(&concrete_component_ref, false, &unscoped_ctx).as_deref(),
        Some("AliasMedium.density")
    );

    let mut flat = flat_with_partial_density_function();
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
    assert_eq!(name.as_str(), "AliasMedium.density");
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

fn flat_with_partial_density_function() -> rumoca_ir_flat::Model {
    let mut flat = rumoca_ir_flat::Model::new();
    let mut function = rumoca_core::Function::new("PartialMedium.density_phX", Span::DUMMY);
    function.body.push(rumoca_core::Statement::Assignment {
        comp: core_comp_ref(&["d"]),
        value: Expression::FunctionCall {
            name: rumoca_core::Reference::with_component_reference(
                "PartialMedium.density",
                core_comp_ref(&["Medium", "density"]),
            ),
            args: vec![core_var("state")],
            is_constructor: false,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
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
