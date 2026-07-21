use super::*;
use rumoca_core::EvalLookup;
use rumoca_ir_ast::Component;

#[test]
fn test_eval_enum_params_resolves_conditional_enum_binding_from_known_boolean() {
    let mut ctx = Context::new();
    ctx.record_aliases.insert(
        rumoca_core::ComponentPath::from_flat_path("pipe1.system"),
        rumoca_core::ComponentPath::from_flat_path("system"),
    );
    ctx.boolean_parameter_values
        .insert("Medium.singleState".to_string(), true);

    let params = vec![
        (
            "systemMassDynamics".to_string(),
            Expression::If {
                branches: vec![(
                    Expression::VarRef {
                        name: rumoca_core::Reference::new("Medium.singleState"),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    },
                    Expression::VarRef {
                        name: rumoca_core::Reference::new("Dynamics.SteadyState"),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    },
                )],
                else_branch: Box::new(Expression::VarRef {
                    name: rumoca_core::Reference::new("Dynamics.SteadyStateInitial"),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
        ),
        (
            "system.massDynamics".to_string(),
            Expression::VarRef {
                name: rumoca_core::Reference::new("systemMassDynamics"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
        ),
        (
            "pipe1.massDynamics".to_string(),
            Expression::VarRef {
                name: rumoca_core::Reference::new("pipe1.system.massDynamics"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
        ),
        (
            "pipe1.traceDynamics".to_string(),
            Expression::VarRef {
                name: rumoca_core::Reference::new("pipe1.massDynamics"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
        ),
    ];

    let progress = ctx.eval_enum_params(&params);
    assert!(
        progress,
        "expected enum parameter pass to resolve enum-if binding"
    );
    assert_eq!(
        ctx.get_enum_param("pipe1.traceDynamics"),
        Some("Dynamics.SteadyState".to_string())
    );
}

#[test]
fn test_try_eval_const_enum_with_scope_rejects_dotted_parameter_like_ref() {
    let ctx = Context::new();
    let expr = component_ref_expr("pipe1.system.energyDynamics");

    assert_eq!(try_eval_const_enum_with_scope(&expr, &ctx, ""), None);
}

#[test]
fn test_try_eval_const_enum_with_scope_accepts_scoped_enum_literal_ref() {
    let ctx = Context::new();
    let expr = component_ref_expr("pipe.Types.ModelStructure.a_v_b");

    assert_eq!(
        try_eval_const_enum_with_scope(&expr, &ctx, ""),
        Some("pipe.Types.ModelStructure.a_v_b".to_string())
    );
}

#[test]
fn test_try_eval_const_flat_expr_with_scope_resolves_enum_alias_component_ref() {
    let mut ctx = Context::new();
    ctx.enum_parameter_values.insert(
        "Modelica.Electrical.Digital.Tables.L.'U'".to_string(),
        "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
    );
    let expr = component_ref_expr("L.'U'");

    let got =
        try_eval_const_flat_expr_with_scope(&expr, &ctx, "Modelica.Electrical.Digital.Tables");
    assert!(matches!(
        got,
        Some(Expression::VarRef { ref name, .. })
            if name.as_str() == "Modelica.Electrical.Digital.Interfaces.Logic.'U'"
    ));
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
    ctx.enum_parameter_values
        .insert("sys.mode".to_string(), "Pkg.Mode.Fast".to_string());
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
    assert_eq!(
        ctx.lookup_enum("mode", "sys.inner").as_deref(),
        Some("Pkg.Mode.Fast")
    );
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
                    span: rumoca_core::Span::DUMMY,
                },
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: Expression::Range {
                start: Box::new(Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                }),
                step: None,
                end: Box::new(Expression::Literal {
                    value: rumoca_core::Literal::Integer(3),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        infer_expr_dims(&expr, &DimMap::new(), &DimMap::new()),
        Some(vec![3, 2])
    );
}

#[test]
fn test_infer_expr_dims_array_comprehension_with_filter_returns_none() {
    let expr = Expression::ArrayComprehension {
        expr: Box::new(Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        }),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: Expression::Range {
                start: Box::new(Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                }),
                step: None,
                end: Box::new(Expression::Literal {
                    value: rumoca_core::Literal::Integer(3),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        filter: Some(Box::new(Expression::VarRef {
            name: "cond".to_string().into(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        })),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(infer_expr_dims(&expr, &DimMap::new(), &DimMap::new()), None);
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
    let overrides = component_overrides(&instance, &tree, &class_index);
    assert_eq!(
        overrides.get("noise").map(|target| target.name.as_str()),
        Some("DefaultNoise"),
        "replaceable component defaults should seed constructor/function override aliases"
    );
}

#[test]
fn test_component_overrides_include_non_replaceable_constructor_aliases() {
    let mut tree = ClassTree::new();
    let host_def_id = DefId::new(15);
    let friction_def_id = DefId::new(16);

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
    let overrides = component_overrides(&instance, &tree, &class_index);
    assert_eq!(
        overrides
            .get("frictionParameters")
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
    let overrides = component_overrides(&instance, &tree, &class_index);
    assert_eq!(
        overrides.get("noise").map(|target| target.name.as_str()),
        Some("RedeclaredNoise"),
        "explicit class redeclare must override default replaceable binding"
    );
}
