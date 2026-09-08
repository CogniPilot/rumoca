use super::*;
use rumoca_core::EvalLookup;
use rumoca_ir_ast::Component;

fn add_conditional_enum_root(flat: &mut rumoca_ir_flat::Model) -> rumoca_core::InstanceId {
    let condition_instance = rumoca_core::InstanceId::new(501);
    add_boolean_parameter(
        flat,
        "Medium.singleState",
        &[
            ("Medium", DefId::new(401)),
            ("singleState", DefId::new(402)),
        ],
        condition_instance,
        true,
    );
    let selected_instance = rumoca_core::InstanceId::new(502);
    add_enum_parameter(
        flat,
        "systemMassDynamics",
        &[("systemMassDynamics", DefId::new(403))],
        selected_instance,
        Expression::If {
            branches: vec![(
                parameter_reference_expr(
                    &[
                        ("Medium", DefId::new(401)),
                        ("singleState", DefId::new(402)),
                    ],
                    condition_instance,
                ),
                enum_literal_expr("SteadyState", DefId::new(602)),
            )],
            else_branch: Box::new(enum_literal_expr("SteadyStateInitial", DefId::new(603))),
            span: test_span(),
        },
    );
    selected_instance
}

#[test]
fn test_eval_enum_params_resolves_conditional_enum_binding_from_known_boolean() {
    let mut flat = typed_flat_model();
    let selected_instance = add_conditional_enum_root(&mut flat);
    let system_instance = rumoca_core::InstanceId::new(503);
    add_enum_parameter(
        &mut flat,
        "system.massDynamics",
        &[
            ("system", DefId::new(404)),
            ("massDynamics", DefId::new(405)),
        ],
        system_instance,
        parameter_reference_expr(
            &[("systemMassDynamics", DefId::new(403))],
            selected_instance,
        ),
    );
    let pipe_instance = rumoca_core::InstanceId::new(504);
    add_enum_parameter(
        &mut flat,
        "pipe1.massDynamics",
        &[
            ("pipe1", DefId::new(406)),
            ("massDynamics", DefId::new(407)),
        ],
        pipe_instance,
        parameter_reference_expr(
            &[
                ("system", DefId::new(404)),
                ("massDynamics", DefId::new(405)),
            ],
            system_instance,
        ),
    );
    add_enum_parameter(
        &mut flat,
        "pipe1.traceDynamics",
        &[
            ("pipe1", DefId::new(406)),
            ("traceDynamics", DefId::new(408)),
        ],
        rumoca_core::InstanceId::new(505),
        parameter_reference_expr(
            &[
                ("pipe1", DefId::new(406)),
                ("massDynamics", DefId::new(407)),
            ],
            pipe_instance,
        ),
    );

    let ctx = build_test_parameter_context(&flat);
    assert_eq!(
        enum_display(&ctx, "pipe1.traceDynamics"),
        Some("Dynamics.SteadyState".to_string())
    );
}

#[test]
fn test_typed_enum_lookup_preserves_alias_owner_and_ordinal() {
    let mut ctx = Context::new();
    ctx.enum_parameter_values.insert(
        "Modelica.Electrical.Digital.Tables.L.'U'".to_string(),
        resolved_enum_value(
            rumoca_core::DefId::new(154),
            "Modelica.Electrical.Digital.Interfaces.Logic",
            "'U'",
        ),
    );
    let enum_type = rumoca_core::DefId::new(154);
    let mut expr = component_ref_expr("L.'U'");
    let rumoca_ir_ast::Expression::ComponentReference(source_reference) = &mut expr else {
        panic!("fixture is an enum component reference");
    };
    for part in &mut source_reference.parts {
        part.def_id = Some(enum_type);
    }
    let got =
        try_eval_const_enum_identity_with_scope(&expr, &ctx, "Modelica.Electrical.Digital.Tables")
            .expect("enum alias settles to one resolved value");
    assert_eq!(got.declaration(), enum_type);
    assert_eq!(got.ordinal(), 1);
}

#[test]
fn test_lookup_with_scope_dotted_name_does_not_use_suffix_lookup() {
    let mut values: rustc_hash::FxHashMap<String, i64> = rustc_hash::FxHashMap::default();
    values.insert("source.medium.nXi".to_string(), 3);

    assert_eq!(lookup_with_scope("medium.nXi", "", &values), None);
}

#[test]
fn test_lookup_with_scope_dotted_name_does_not_fallback_to_leaf_segment() {
    let mut values: rustc_hash::FxHashMap<String, i64> = rustc_hash::FxHashMap::default();
    values.insert("a.b.nXi".to_string(), 1);
    values.insert("c.d.nXi".to_string(), 1);

    assert_eq!(lookup_with_scope("x.y.nXi", "", &values), None);
}

#[test]
fn test_lookup_with_scope_simple_name_does_not_use_suffix_lookup() {
    let mut values: rustc_hash::FxHashMap<String, i64> = rustc_hash::FxHashMap::default();
    values.insert("a.b.nXi".to_string(), 2);
    values.insert("c.d.nXi".to_string(), 2);

    assert_eq!(lookup_with_scope("nXi", "", &values), None);
}

#[test]
fn test_eval_lookup_trait_resolves_scoped_values() {
    let mut ctx = Context::new();
    ctx.parameter_values.insert("sys.n".to_string(), 4);
    ctx.real_parameter_values
        .insert("sys.inner.r".to_string(), 2.5);
    ctx.boolean_parameter_values
        .insert("sys.flag".to_string(), true);
    ctx.parameter_values
        .insert("source.medium.nXi".to_string(), 3);
    ctx.record_aliases.insert(
        rumoca_core::ComponentPath::from_flat_path("sys.alias"),
        rumoca_core::ComponentPath::from_flat_path("sys"),
    );

    assert_eq!(ctx.lookup_integer("n", "sys.inner"), Some(4));
    assert_eq!(ctx.lookup_integer("n", "sys.alias.inner"), Some(4));
    assert_eq!(ctx.lookup_real("r", "sys.inner"), Some(2.5));
    assert_eq!(ctx.lookup_boolean("flag", "sys.inner"), Some(true));
    assert_eq!(ctx.lookup_integer("medium.nXi", ""), None);
}

#[test]
fn test_resolve_through_prefix_handles_dot_inside_subscript_expression() {
    let mut aliases: rustc_hash::FxHashMap<rumoca_core::ComponentPath, rumoca_core::ComponentPath> =
        rustc_hash::FxHashMap::default();
    aliases.insert(
        rumoca_core::ComponentPath::from_flat_path("bus[data.medium]"),
        rumoca_core::ComponentPath::from_flat_path("busMedium"),
    );

    let resolved = crate::alias_paths::resolve_component_alias_once(
        &rumoca_core::ComponentPath::from_flat_path("bus[data.medium].pin.v"),
        Some(&rumoca_core::ComponentPath::from_flat_path("source")),
        &aliases,
    );
    assert_eq!(
        resolved.as_ref().map(|path| path.to_flat_string()),
        Some("busMedium.pin.v".to_string())
    );
}

#[test]
fn test_synthesize_intermediate_aliases_handles_dot_inside_subscript_expression() {
    let mut aliases: rustc_hash::FxHashMap<rumoca_core::ComponentPath, rumoca_core::ComponentPath> =
        rustc_hash::FxHashMap::default();
    aliases.insert(
        rumoca_core::ComponentPath::from_flat_path("stack.stackData"),
        rumoca_core::ComponentPath::from_flat_path("stackData"),
    );
    aliases.insert(
        rumoca_core::ComponentPath::from_flat_path("src"),
        rumoca_core::ComponentPath::from_flat_path("stack.cell[data.medium].stackData.cellData"),
    );

    synthesize_intermediate_aliases(&mut aliases);

    assert_eq!(
        aliases.get(&rumoca_core::ComponentPath::from_flat_path(
            "stack.cell[data.medium].stackData"
        )),
        Some(&rumoca_core::ComponentPath::from_flat_path(
            "stack.stackData"
        ))
    );
}

#[test]
fn test_infer_function_call_dims_requires_exact_resolved_name() {
    let mut function_output_dims = DimMap::new();
    function_output_dims.insert("foo[data.medium]".to_string(), vec![2]);
    function_output_dims.insert("model.foo[data.medium]".to_string(), vec![3]);

    assert_eq!(
        infer_function_call_dims("model.foo[data.medium]", &function_output_dims),
        Some(vec![3]),
        "function output dims use the resolved function name, not textual leaf recovery"
    );
    assert_eq!(
        infer_function_call_dims("other.foo[data.medium]", &function_output_dims),
        None
    );
}

#[test]
fn test_infer_expr_dims_handles_array_comprehension() {
    let expr = Expression::ArrayComprehension {
        expr: Box::new(Expression::Array {
            elements: vec![
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: test_span(),
                },
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: test_span(),
                },
            ],
            is_matrix: false,
            span: test_span(),
        }),
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

    assert_eq!(
        infer_expr_dims(&expr, &DimMap::new(), &DimMap::new()).unwrap(),
        Some(vec![3, 2])
    );
}

#[test]
fn test_infer_expr_dims_array_comprehension_with_filter_returns_none() {
    let expr = Expression::ArrayComprehension {
        expr: Box::new(Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span: test_span(),
        }),
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
        filter: Some(Box::new(Expression::VarRef {
            name: "cond".to_string().into(),
            subscripts: Vec::new(),
            span: test_span(),
        })),
        span: test_span(),
    };

    assert_eq!(
        infer_expr_dims(&expr, &DimMap::new(), &DimMap::new()).unwrap(),
        None
    );
}

fn seed_class(tree: &mut ClassTree, name: &str, def_id: DefId, class_type: ClassType) {
    let class = ClassDef {
        name: token(name),
        class_type,
        def_id: Some(def_id),
        ..Default::default()
    };
    tree.definitions.classes.insert(name.to_string(), class);
    tree.def_map.insert(def_id, name.to_string());
    tree.name_map.insert(name.to_string(), def_id);
}

#[test]
fn test_component_overrides_include_replaceable_component_defaults() {
    let mut tree = ClassTree::new();
    let host_def_id = DefId::new(10);
    let default_noise_def_id = DefId::new(11);
    let noise_component_def_id = DefId::new(12);

    seed_class(
        &mut tree,
        "DefaultNoise",
        default_noise_def_id,
        ClassType::Block,
    );
    let mut host = ClassDef {
        name: token("Host"),
        class_type: ClassType::Block,
        def_id: Some(host_def_id),
        ..Default::default()
    };
    host.components.insert(
        "noise".to_string(),
        Component {
            name: "noise".to_string(),
            def_id: Some(noise_component_def_id),
            is_replaceable: true,
            type_def_id: Some(default_noise_def_id),
            ..Component::empty_with_span(test_span())
        },
    );
    tree.definitions.classes.insert("Host".to_string(), host);
    tree.def_map.insert(host_def_id, "Host".to_string());
    tree.name_map.insert("Host".to_string(), host_def_id);

    let instance = InstanceData {
        type_def_id: Some(host_def_id),
        ..Default::default()
    };

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let overrides =
        component_overrides(&instance, &tree, &class_index).expect("component override table");
    assert_eq!(
        overrides
            .get(&noise_component_def_id)
            .map(|target| target.name.as_str()),
        Some("DefaultNoise"),
        "replaceable component defaults should seed constructor/function override aliases"
    );
}

#[test]
fn test_component_overrides_include_non_replaceable_constructor_aliases() {
    let mut tree = ClassTree::new();
    let host_def_id = DefId::new(15);
    let friction_def_id = DefId::new(16);
    let friction_component_def_id = DefId::new(17);

    seed_class(
        &mut tree,
        "FrictionParameters",
        friction_def_id,
        ClassType::Record,
    );
    let mut host = ClassDef {
        name: token("Host"),
        class_type: ClassType::Model,
        def_id: Some(host_def_id),
        ..Default::default()
    };
    host.components.insert(
        "frictionParameters".to_string(),
        Component {
            name: "frictionParameters".to_string(),
            def_id: Some(friction_component_def_id),
            is_replaceable: false,
            type_def_id: Some(friction_def_id),
            ..Component::empty_with_span(test_span())
        },
    );
    tree.definitions.classes.insert("Host".to_string(), host);
    tree.def_map.insert(host_def_id, "Host".to_string());
    tree.name_map.insert("Host".to_string(), host_def_id);

    let instance = InstanceData {
        type_def_id: Some(host_def_id),
        ..Default::default()
    };

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let overrides =
        component_overrides(&instance, &tree, &class_index).expect("component override table");
    assert_eq!(
        overrides
            .get(&friction_component_def_id)
            .map(|target| target.name.as_str()),
        Some("FrictionParameters"),
        "non-replaceable component constructor aliases should be available for rewrite"
    );
}

#[test]
fn test_component_overrides_prefers_explicit_redeclare_over_default() {
    let mut tree = ClassTree::new();
    let host_def_id = DefId::new(20);
    let default_noise_def_id = DefId::new(21);
    let redeclared_noise_def_id = DefId::new(22);
    let noise_component_def_id = DefId::new(23);

    seed_class(
        &mut tree,
        "DefaultNoise",
        default_noise_def_id,
        ClassType::Block,
    );
    seed_class(
        &mut tree,
        "RedeclaredNoise",
        redeclared_noise_def_id,
        ClassType::Block,
    );

    let mut host = ClassDef {
        name: token("Host"),
        class_type: ClassType::Block,
        def_id: Some(host_def_id),
        ..Default::default()
    };
    host.components.insert(
        "noise".to_string(),
        Component {
            name: "noise".to_string(),
            def_id: Some(noise_component_def_id),
            is_replaceable: true,
            type_def_id: Some(default_noise_def_id),
            ..Component::empty_with_span(test_span())
        },
    );
    tree.definitions.classes.insert("Host".to_string(), host);
    tree.def_map.insert(host_def_id, "Host".to_string());
    tree.name_map.insert("Host".to_string(), host_def_id);

    let mut instance = InstanceData {
        type_def_id: Some(host_def_id),
        ..Default::default()
    };
    instance.class_overrides.insert(
        default_noise_def_id,
        rumoca_ir_ast::ClassOverride::new(
            "noise",
            default_noise_def_id,
            redeclared_noise_def_id,
            None,
        ),
    );

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let overrides =
        component_overrides(&instance, &tree, &class_index).expect("component override table");
    assert_eq!(
        overrides
            .get(&default_noise_def_id)
            .map(|target| target.name.as_str()),
        Some("RedeclaredNoise"),
        "explicit class redeclare must override default replaceable binding"
    );
}
