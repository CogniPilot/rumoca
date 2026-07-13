use super::*;

fn var_ref_with_target_def(name: &str, target_def_id: DefId) -> Expression {
    let parts = crate::path_utils::segments(name)
        .into_iter()
        .map(|segment| rumoca_core::ComponentRefPart {
            ident: segment.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: Vec::new(),
        })
        .collect();
    let component_ref = rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts,
        def_id: Some(target_def_id),
    };
    Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(name, component_ref),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn self_qualified_var_ref_with_source_leaf(
    rendered_name: &str,
    source_leaf: &str,
    target_def_id: DefId,
) -> Expression {
    let component_ref = rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: source_leaf.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: Vec::new(),
        }],
        def_id: Some(target_def_id),
    };
    Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(rendered_name, component_ref),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn reconciled_modified_record_table_shape_beats_stale_default_binding_shape() {
    let mut ctx = Context::new();
    let tree = source_backed_tree();
    ctx.parameter_values.insert("rows".to_string(), 29);
    ctx.parameter_values.insert("columns".to_string(), 2);
    ctx.array_dimensions
        .insert("stack.cellData.OCV_SOC".to_string(), vec![29, 2]);
    let row = || Expression::Array {
        elements: vec![int_lit(0), int_lit(1)],
        is_matrix: false,
        span: test_span(),
    };
    let name = rumoca_core::VarName::new("stack.cellData.OCV_SOC");
    let mut flat = flat::Model::default();
    flat.add_variable(
        name.clone(),
        flat::Variable {
            name: name.clone(),
            dims: vec![2, 2],
            binding: Some(Expression::Array {
                elements: vec![row(), row()],
                is_matrix: true,
                span: test_span(),
            }),
            binding_from_modification: true,
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let mut overlay = InstanceOverlay::default();
    overlay.components.insert(
        InstanceId::new(1),
        symbolic_instance(
            InstanceId::new(1),
            "stack.cellData.OCV_SOC",
            vec![
                ast::Subscript::Expression(component_ref_expr("rows")),
                ast::Subscript::Expression(component_ref_expr("columns")),
            ],
        ),
    );

    assert!(ctx.reconcile_modified_binding_dimensions(&mut flat));
    let recomputed = ctx
        .recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
        .expect("modified table dimensions reconcile");

    assert!(!recomputed);
    assert_eq!(flat.variables.get(&name).unwrap().dims, vec![29, 2]);

    ctx.build_parameter_lookup(&flat, &tree);
    assert!(!ctx.reconcile_modified_binding_dimensions(&mut flat));
    assert!(
        !ctx.recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect("fixed point remains stable")
    );
}

#[test]
fn modified_binding_dimensions_replace_stale_declared_shape() {
    let mut ctx = Context::new();
    let tree = source_backed_tree();
    let mut flat = flat::Model::default();
    let ambiguous_mr_def = DefId::new(42);
    ctx.target_def_names
        .insert(ambiguous_mr_def, "mr".to_string());

    for (name, binding, binding_from_modification) in [
        ("mr", int_lit(3), false),
        ("aimsM.mr", int_lit(5), true),
        (
            "aimsM.rotor.m",
            var_ref_with_target_def("aimsM.mr", ambiguous_mr_def),
            true,
        ),
    ] {
        let var_name = rumoca_core::VarName::new(name);
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(binding),
                binding_from_modification,
                is_discrete_type: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let r_name = rumoca_core::VarName::new("aimsM.rotor.resistor.R");
    flat.add_variable(
        r_name.clone(),
        flat::Variable {
            name: r_name.clone(),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            dims: vec![3],
            binding: Some(Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Fill,
                args: vec![int_lit(1), var_ref("aimsM.rotor.m")],
                span: rumoca_core::Span::DUMMY,
            }),
            binding_from_modification: true,
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let local_m = rumoca_core::VarName::new("aimsM.rotor.resistor.m");
    flat.add_variable(
        local_m.clone(),
        flat::Variable {
            name: local_m,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(var_ref_with_target_def("aimsM.mr", ambiguous_mr_def)),
            binding_from_modification: true,
            is_discrete_type: true,
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let mut overlay = InstanceOverlay::default();
    overlay.components.insert(
        InstanceId::new(1),
        symbolic_instance(
            InstanceId::new(1),
            "aimsM.rotor.resistor.R",
            vec![ast::Subscript::Expression(component_ref_expr(
                "aimsM.rotor.resistor.m",
            ))],
        ),
    );

    ctx.build_parameter_lookup(&flat, &tree);
    assert_eq!(ctx.get_integer_param("aimsM.rotor.m"), Some(5));
    assert_eq!(ctx.get_integer_param("aimsM.rotor.resistor.m"), Some(5));
    assert_eq!(
        ctx.array_dimensions.get("aimsM.rotor.resistor.R"),
        Some(&vec![5])
    );
    assert_eq!(
        flat.variables.get(&r_name).expect("R variable").dims,
        vec![3]
    );

    assert!(ctx.reconcile_modified_binding_dimensions(&mut flat));
    assert_eq!(
        flat.variables.get(&r_name).expect("R variable").dims,
        vec![5]
    );

    flat.variables.get_mut(&r_name).expect("R variable").dims = vec![3];
    assert!(
        ctx.recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect("modified local m dimension is available to symbolic dimensions")
    );
    assert_eq!(
        flat.variables.get(&r_name).expect("R variable").dims,
        vec![5]
    );

    ctx.build_parameter_lookup(&flat, &tree);
    assert_eq!(
        ctx.array_dimensions.get("aimsM.rotor.resistor.R"),
        Some(&vec![5])
    );
}

#[test]
fn modified_integer_unqualified_rhs_prefers_modifier_source_scope_over_target_def() {
    let mut ctx = Context::new();
    let target_def = DefId::new(43);
    ctx.target_def_names
        .insert(target_def, "sensor.block.m".to_string());
    ctx.parameter_values.insert("sensor.m".to_string(), 3);
    ctx.parameter_values.insert("sensor.block.m".to_string(), 6);

    let params = vec![(
        "sensor.block.m".to_string(),
        var_ref_with_target_def("m", target_def),
    )];

    assert!(
        ctx.eval_modified_integer_params(&params),
        "modifier-origin unqualified RHS must be resolved where the modifier was written"
    );
    assert_eq!(ctx.get_integer_param("sensor.block.m"), Some(3));
}

#[test]
fn modified_binding_dimensions_do_not_follow_unrelated_local_m() {
    let mut ctx = Context::new();
    let tree = source_backed_tree();
    let mut flat = flat::Model::default();

    let local_m = rumoca_core::VarName::new("component.m");
    flat.add_variable(
        local_m.clone(),
        flat::Variable {
            name: local_m,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(int_lit(5)),
            binding_from_modification: true,
            is_discrete_type: true,
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let x_name = rumoca_core::VarName::new("component.x");
    flat.add_variable(
        x_name.clone(),
        flat::Variable {
            name: x_name.clone(),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            dims: vec![3],
            binding: Some(Expression::Array {
                elements: vec![int_lit(1), int_lit(2), int_lit(3)],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            }),
            binding_from_modification: true,
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    ctx.build_parameter_lookup(&flat, &tree);
    assert_eq!(ctx.get_integer_param("component.m"), Some(5));
    assert_eq!(ctx.array_dimensions.get("component.x"), Some(&vec![3]));
    assert!(!ctx.reconcile_modified_binding_dimensions(&mut flat));
    assert_eq!(
        flat.variables.get(&x_name).expect("x variable").dims,
        vec![3]
    );
}

#[test]
fn modified_integer_alias_prefers_modifier_scope_over_declared_target_def() {
    let mut ctx = Context::new();
    let tree = source_backed_tree();
    let mut flat = flat::Model::default();
    let class_m_def = DefId::new(77);
    ctx.target_def_names
        .insert(class_m_def, "Pkg.Block.m".to_string());

    for (name, binding, binding_from_modification) in [
        ("component.m", int_lit(3), true),
        ("Pkg.Block.m", int_lit(6), false),
        (
            "component.child.m",
            var_ref_with_target_def("m", class_m_def),
            true,
        ),
    ] {
        let var_name = rumoca_core::VarName::new(name);
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(binding),
                binding_from_modification,
                is_discrete_type: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    ctx.build_parameter_lookup(&flat, &tree);

    assert_eq!(ctx.get_integer_param("component.m"), Some(3));
    assert_eq!(ctx.get_integer_param("Pkg.Block.m"), Some(6));
    assert_eq!(ctx.get_integer_param("component.child.m"), Some(3));
}

#[test]
fn modified_integer_alias_uses_source_leaf_when_flat_name_is_self_qualified() {
    let mut ctx = Context::new();
    let tree = source_backed_tree();
    let mut flat = flat::Model::default();
    let class_m_def = DefId::new(78);
    ctx.target_def_names
        .insert(class_m_def, "Pkg.Block.m".to_string());

    for (name, binding, binding_from_modification) in [
        ("component.m", int_lit(3), true),
        ("Pkg.Block.m", int_lit(6), false),
        (
            "component.child.m",
            self_qualified_var_ref_with_source_leaf("component.child.m", "m", class_m_def),
            true,
        ),
    ] {
        let var_name = rumoca_core::VarName::new(name);
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(binding),
                binding_from_modification,
                is_discrete_type: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    ctx.build_parameter_lookup(&flat, &tree);

    assert_eq!(ctx.get_integer_param("component.child.m"), Some(3));
}

#[test]
fn modified_integer_alias_sync_is_not_limited_to_phase_count_m() {
    let mut ctx = Context::new();
    let tree = source_backed_tree();
    let mut flat = flat::Model::default();

    for (name, binding, binding_from_modification) in [
        ("system.n", int_lit(4), true),
        ("component.nLocal", var_ref("system.n"), true),
    ] {
        let var_name = rumoca_core::VarName::new(name);
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(binding),
                binding_from_modification,
                is_discrete_type: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let x_name = rumoca_core::VarName::new("component.x");
    flat.add_variable(
        x_name.clone(),
        flat::Variable {
            name: x_name.clone(),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            dims: vec![2],
            binding: Some(Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Fill,
                args: vec![int_lit(1), var_ref("component.nLocal")],
                span: rumoca_core::Span::DUMMY,
            }),
            binding_from_modification: true,
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    ctx.build_parameter_lookup(&flat, &tree);

    assert_eq!(ctx.get_integer_param("component.nLocal"), Some(4));
    assert_eq!(ctx.array_dimensions.get("component.x"), Some(&vec![4]));
    assert_eq!(
        flat.variables.get(&x_name).expect("x variable").dims,
        vec![2]
    );
    assert!(ctx.reconcile_modified_binding_dimensions(&mut flat));
    assert_eq!(
        flat.variables.get(&x_name).expect("x variable").dims,
        vec![4]
    );
}
