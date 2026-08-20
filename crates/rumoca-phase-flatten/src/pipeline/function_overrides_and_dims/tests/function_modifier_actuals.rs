//! Replaceable function aliases carry their modifier actuals to the
//! selected implementation.

use super::*;

struct GravityAliasFixture {
    tree: ClassTree,
    override_functions: OverrideFunctionMap,
    implementation: DefId,
    world_occurrence: DefId,
    radius: DefId,
}

fn gravity_alias_fixture() -> GravityAliasFixture {
    let gravity_def = DefId::new(1);
    let standard_def = DefId::new(2);
    let world_def = DefId::new(3);
    let standard_gravity_type_def = DefId::new(4);
    let world_gravity_type_def = DefId::new(5);
    let radius_def = DefId::new(6);
    let world_occurrence_def = DefId::new(7);
    let owner_def = DefId::new(8);
    let mut gravity = class("gravityAcceleration", ClassType::Function);
    gravity.def_id = Some(gravity_def);
    gravity.extends.push(Extend {
        base_name: Name::from_string("Standard"),
        base_def_id: Some(standard_def),
        modifications: vec![rumoca_ir_ast::ExtendModification {
            expr: rumoca_ir_ast::Expression::Modification {
                target: resolved_comp_ref(&[("gravityType", standard_gravity_type_def)]),
                value: Arc::new(resolved_ast_var(&[("gravityType", world_gravity_type_def)])),
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
    let mut world_gravity_type = component("gravityType", "Integer", rumoca_core::DefId::new(90));
    world_gravity_type.def_id = Some(world_gravity_type_def);
    world
        .components
        .insert("gravityType".to_string(), world_gravity_type);
    world
        .classes
        .insert("gravityAcceleration".to_string(), gravity);
    let mut tree = ClassTree::new();
    tree.definitions.classes.insert("World".to_string(), world);
    let mut standard = class("Standard", ClassType::Function);
    standard.def_id = Some(standard_def);
    let mut standard_gravity_type =
        component("gravityType", "Integer", rumoca_core::DefId::new(90));
    standard_gravity_type.def_id = Some(standard_gravity_type_def);
    standard
        .components
        .insert("gravityType".to_string(), standard_gravity_type);
    standard
        .algorithms
        .push(vec![rumoca_ir_ast::Statement::Return {
            token: token("return"),
        }]);
    tree.definitions
        .classes
        .insert("Standard".to_string(), standard);
    let mut owner = class("Owner", ClassType::Model);
    owner.def_id = Some(owner_def);
    let mut world_occurrence = component("world", "World", world_def);
    world_occurrence.def_id = Some(world_occurrence_def);
    owner
        .components
        .insert("world".to_string(), world_occurrence);
    tree.definitions.classes.insert("Owner".to_string(), owner);
    tree.def_map
        .insert(gravity_def, "World.gravityAcceleration".to_string());
    tree.def_map.insert(standard_def, "Standard".to_string());
    tree.def_map.insert(world_def, "World".to_string());
    tree.def_map.insert(owner_def, "Owner".to_string());
    tree.def_map.insert(
        standard_gravity_type_def,
        "Standard.gravityType".to_string(),
    );
    tree.def_map
        .insert(world_gravity_type_def, "World.gravityType".to_string());
    tree.name_map
        .insert("World.gravityAcceleration".to_string(), gravity_def);
    tree.name_map.insert("Standard".to_string(), standard_def);
    tree.name_map.insert("World".to_string(), world_def);

    let mut override_functions = OverrideFunctionMap::default();
    override_functions.insert(
        "world".to_string(),
        override_target("World", world_def, ClassType::Model),
    );
    GravityAliasFixture {
        tree,
        override_functions,
        implementation: standard_def,
        world_occurrence: world_occurrence_def,
        radius: radius_def,
    }
}

#[test]
fn replaceable_function_alias_preserves_modifier_actuals() {
    let fixture = gravity_alias_fixture();
    let tree = fixture.tree;
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let ctx =
        FunctionOverrideRewriteContext::new(&tree, &class_index, &[], &fixture.override_functions);
    let mut expr = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "world.gravityAcceleration",
            core_comp_ref(&[
                ("world", fixture.world_occurrence),
                ("gravityAcceleration", fixture.implementation),
            ]),
        ),
        args: vec![core_var(&[("r", fixture.radius)])],
        is_constructor: false,
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("function override rewrite");

    let Expression::FunctionCall { name, args, .. } = expr else {
        panic!("expected rewritten function call");
    };
    assert_eq!(name.as_str(), "world.gravityAcceleration");
    assert_eq!(args.len(), 2);
    let Some(("gravityType", Expression::VarRef { name, .. })) = named_arg(&args[1]) else {
        panic!("expected receiver-qualified gravityType named argument");
    };
    assert_eq!(name.as_str(), "world.gravityType");
}

#[test]
fn structured_template_and_scalar_row_keep_the_same_bound_function_inputs() {
    let fixture = gravity_alias_fixture();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&fixture.tree);
    let call = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "world.gravityAcceleration",
            core_comp_ref(&[
                ("world", fixture.world_occurrence),
                ("gravityAcceleration", fixture.implementation),
            ]),
        ),
        args: vec![core_var(&[("r", fixture.radius)])],
        is_constructor: false,
        span: test_span(),
    };
    let origin = rumoca_ir_flat::EquationOrigin::ComponentEquation {
        component: "probe".to_string(),
    };
    let mut flat = rumoca_ir_flat::Model::new();
    flat.add_equation(rumoca_ir_flat::Equation::new(
        call.clone(),
        test_span(),
        origin.clone(),
    ));
    flat.add_initial_equation(rumoca_ir_flat::Equation::new(
        call.clone(),
        test_span(),
        origin.clone(),
    ));
    let family = |body| rumoca_ir_flat::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        first_equation_index: 0,
        equations_per_point: 1,
        span: test_span(),
        origin: origin.clone(),
        regular: None,
        template: Some(rumoca_core::ComprehensionTemplate {
            body: vec![body],
            scalar_view: rumoca_core::ComprehensionScalarView::BinderSubstitution,
        }),
        interiors_materialized: true,
    };
    flat.add_structured_equation(family(call.clone()));
    flat.add_initial_structured_equation(family(call));
    let mut component_overrides = ComponentOverrideMap::default();
    component_overrides.insert(ComponentPath::root(), fixture.override_functions);

    rewrite_function_overrides_in_flat_model(
        &mut flat,
        &fixture.tree,
        &class_index,
        &component_overrides,
        &crate::pipeline::component_member_scope::ComponentMemberScopes::default(),
    )
    .expect("exact function modifier rewrite");

    let argument_names = |expression: &Expression| {
        let Expression::FunctionCall { args, .. } = expression else {
            panic!("expected function call");
        };
        args.iter()
            .filter_map(named_arg)
            .map(|(name, _)| name.to_string())
            .collect::<Vec<_>>()
    };
    let scalar_names = argument_names(&flat.equations[0].residual);
    let initial_scalar_names = argument_names(&flat.initial_equations[0].residual);
    let template_names =
        argument_names(&flat.structured_equations[0].template.as_ref().unwrap().body[0]);
    let initial_template_names = argument_names(
        &flat.initial_structured_equations[0]
            .template
            .as_ref()
            .unwrap()
            .body[0],
    );
    assert_eq!(scalar_names, vec!["gravityType"]);
    assert_eq!(template_names, scalar_names);
    assert_eq!(initial_scalar_names, scalar_names);
    assert_eq!(initial_template_names, scalar_names);
}

fn function_alias_with_real_default(
    name: &str,
    exposure: DefId,
    implementation: DefId,
    value: &str,
) -> ClassDef {
    let mut alias = class(name, ClassType::Function);
    alias.def_id = Some(exposure);
    alias.extends.push(Extend {
        base_name: Name::from_string("Standard"),
        base_def_id: Some(implementation),
        modifications: vec![rumoca_ir_ast::ExtendModification {
            expr: rumoca_ir_ast::Expression::NamedArgument {
                name: token("g0"),
                value: Arc::new(rumoca_ir_ast::Expression::Terminal {
                    terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
                    token: token(value),
                    span: test_span(),
                }),
                span: test_span(),
            },
            each: false,
            final_: false,
            redeclare: false,
        }],
        ..Extend::default()
    });
    alias
}

fn receiver_with_function_alias(
    name: &str,
    receiver: DefId,
    exposure: DefId,
    implementation: DefId,
    value: &str,
) -> ClassDef {
    let mut class_def = class(name, ClassType::Model);
    class_def.def_id = Some(receiver);
    class_def.classes.insert(
        "gravity".to_string(),
        function_alias_with_real_default("gravity", exposure, implementation, value),
    );
    class_def
}

fn rewrite_gravity_call(
    receiver: &str,
    occurrence: DefId,
    implementation: DefId,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Vec<Expression> {
    let mut expression = Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            format!("{receiver}.gravity"),
            core_comp_ref(&[(receiver, occurrence), ("gravity", implementation)]),
        ),
        args: Vec::new(),
        is_constructor: false,
        span: test_span(),
    };
    rewrite_function_overrides_in_expression_with_ctx(&mut expression, ctx)
        .expect("exact function selection rewrites");
    let Expression::FunctionCall { name, args, .. } = expression else {
        panic!("expected rewritten gravity call");
    };
    assert_eq!(name.as_str(), format!("{receiver}.gravity"));
    args
}

#[test]
fn same_leaf_receivers_materialize_defaults_from_exact_exposure_owner() {
    let implementation = DefId::new(130);
    let receiver_a = DefId::new(131);
    let exposure_a = DefId::new(132);
    let receiver_b = DefId::new(133);
    let exposure_b = DefId::new(134);
    let mut standard = class("Standard", ClassType::Function);
    standard.def_id = Some(implementation);
    standard
        .algorithms
        .push(vec![rumoca_ir_ast::Statement::Return {
            token: token("return"),
        }]);
    let world_a =
        receiver_with_function_alias("WorldA", receiver_a, exposure_a, implementation, "1.0");
    let world_b =
        receiver_with_function_alias("WorldB", receiver_b, exposure_b, implementation, "2.0");
    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("Standard".to_string(), standard);
    tree.definitions
        .classes
        .insert("WorldA".to_string(), world_a);
    tree.definitions
        .classes
        .insert("WorldB".to_string(), world_b);
    for (def_id, name) in [
        (implementation, "Standard"),
        (receiver_a, "WorldA"),
        (exposure_a, "WorldA.gravity"),
        (receiver_b, "WorldB"),
        (exposure_b, "WorldB.gravity"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let mut overrides = OverrideFunctionMap::default();
    overrides.insert(
        "a".to_string(),
        override_target("WorldA", receiver_a, ClassType::Model),
    );
    overrides.insert(
        "b".to_string(),
        override_target("WorldB", receiver_b, ClassType::Model),
    );
    let ctx = FunctionOverrideRewriteContext::new(&tree, &class_index, &[], &overrides);

    let a_args = rewrite_gravity_call("a", receiver_a, implementation, &ctx);
    let b_args = rewrite_gravity_call("b", receiver_b, implementation, &ctx);
    let Some(("g0", Expression::Literal { value: a, .. })) = named_arg(&a_args[0]) else {
        panic!("receiver a must materialize its exact declaration default");
    };
    let Some(("g0", Expression::Literal { value: b, .. })) = named_arg(&b_args[0]) else {
        panic!("receiver b must materialize its exact declaration default");
    };
    assert_eq!(*a, rumoca_core::Literal::Real(1.0));
    assert_eq!(*b, rumoca_core::Literal::Real(2.0));
}

#[test]
fn same_spelling_receiver_and_unrelated_package_cannot_replace_exact_prefix_owner() {
    let implementation = DefId::new(170);
    let receiver_a = DefId::new(171);
    let exposure_a = DefId::new(172);
    let receiver_b = DefId::new(173);
    let exposure_b = DefId::new(174);
    let mut standard = class("Standard", ClassType::Function);
    standard.def_id = Some(implementation);
    standard
        .algorithms
        .push(vec![rumoca_ir_ast::Statement::Return {
            token: token("return"),
        }]);
    let world_a =
        receiver_with_function_alias("WorldA", receiver_a, exposure_a, implementation, "1.0");
    let world_b =
        receiver_with_function_alias("WorldB", receiver_b, exposure_b, implementation, "2.0");
    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("Standard".to_string(), standard);
    tree.definitions
        .classes
        .insert("WorldA".to_string(), world_a);
    tree.definitions
        .classes
        .insert("WorldB".to_string(), world_b);
    for (def_id, name) in [
        (implementation, "Standard"),
        (receiver_a, "WorldA"),
        (exposure_a, "WorldA.gravity"),
        (receiver_b, "WorldB"),
        (exposure_b, "WorldB.gravity"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let mut overrides = OverrideFunctionMap::default();
    overrides.insert(
        "world".to_string(),
        override_target("WorldB", receiver_b, ClassType::Model),
    );
    let unrelated_packages = vec![override_target("WorldB", receiver_b, ClassType::Package)];
    let ctx =
        FunctionOverrideRewriteContext::new(&tree, &class_index, &unrelated_packages, &overrides);

    let args = rewrite_gravity_call("world", receiver_a, implementation, &ctx);
    let Some(("g0", Expression::Literal { value, .. })) = named_arg(&args[0]) else {
        panic!("exact WorldA exposure must supply its declaration default");
    };
    assert_eq!(*value, rumoca_core::Literal::Real(1.0));
}

#[test]
fn inherited_exposure_and_explicit_redeclare_use_exact_precedence() {
    let implementation = DefId::new(150);
    let base_receiver = DefId::new(151);
    let derived_receiver = DefId::new(152);
    let exposure = DefId::new(153);
    let mut standard = class("Standard", ClassType::Function);
    standard.def_id = Some(implementation);
    standard
        .algorithms
        .push(vec![rumoca_ir_ast::Statement::Return {
            token: token("return"),
        }]);
    let base =
        receiver_with_function_alias("BaseWorld", base_receiver, exposure, implementation, "1.0");
    let mut derived = class("DerivedWorld", ClassType::Model);
    derived.def_id = Some(derived_receiver);
    derived.extends.push(Extend {
        base_name: Name::from_string("BaseWorld"),
        base_def_id: Some(base_receiver),
        ..Extend::default()
    });
    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("Standard".to_string(), standard);
    tree.definitions
        .classes
        .insert("BaseWorld".to_string(), base);
    tree.definitions
        .classes
        .insert("DerivedWorld".to_string(), derived);
    for (def_id, name) in [
        (implementation, "Standard"),
        (base_receiver, "BaseWorld"),
        (derived_receiver, "DerivedWorld"),
        (exposure, "BaseWorld.gravity"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let mut overrides = OverrideFunctionMap::default();
    overrides.insert(
        "world".to_string(),
        override_target("DerivedWorld", derived_receiver, ClassType::Model),
    );
    overrides.insert(
        "gravity".to_string(),
        OverrideTarget {
            alias: "gravity".to_string(),
            name: "Standard".to_string(),
            def_id: implementation,
            class_type: ClassType::Function,
            active: true,
            modifier_args: vec![FunctionModifierArg {
                name: "g0".to_string(),
                value: rumoca_ir_ast::Expression::Terminal {
                    terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
                    token: token("9.0"),
                    span: test_span(),
                },
                span: test_span(),
            }],
        },
    );
    let ctx = FunctionOverrideRewriteContext::new(&tree, &class_index, &[], &overrides);

    let args = rewrite_gravity_call("world", derived_receiver, implementation, &ctx);
    assert_eq!(args.len(), 1, "the g0 slot must be materialized once");
    let Some(("g0", Expression::Literal { value, .. })) = named_arg(&args[0]) else {
        panic!("explicit redeclare must materialize g0");
    };
    assert_eq!(
        *value,
        rumoca_core::Literal::Real(9.0),
        "explicit redeclare modifier precedes the inherited declaration default"
    );
}
