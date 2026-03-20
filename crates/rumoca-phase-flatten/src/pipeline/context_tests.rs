use super::*;

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{DefId, EvalLookup};
    use rumoca_ir_ast as ast;
    use rumoca_ir_ast::{ClassDef, ClassTree, ClassType, Component, InstanceData, InstanceId};
    use rumoca_ir_flat as flat;
    use std::sync::Arc;

    fn component_ref_expr(path: &str) -> ast::Expression {
        let parts = crate::path_utils::split_path_with_indices(path)
            .into_iter()
            .map(|segment| ast::ComponentRefPart {
                ident: rumoca_ir_core::Token {
                    text: Arc::from(segment),
                    ..rumoca_ir_core::Token::default()
                },
                subs: None,
            })
            .collect();

        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts,
            def_id: None,
        })
    }

    #[test]
    fn test_flatten_context() {
        let _ctx = Context::new();
    }

    #[test]
    fn test_is_in_disabled_component_handles_dot_inside_bracket_expression() {
        let qn = QualifiedName {
            parts: vec![
                ("bus[data.medium]".to_string(), Vec::new()),
                ("pin".to_string(), Vec::new()),
                ("v".to_string(), Vec::new()),
            ],
        };
        let mut disabled = std::collections::HashSet::new();
        disabled.insert("bus[data.medium].pin".to_string());

        assert!(is_in_disabled_component(&qn, &disabled));
    }

    #[test]
    fn test_is_in_disabled_component_ignores_non_matching_bracket_expression() {
        let qn = QualifiedName {
            parts: vec![
                ("bus[data.medium]".to_string(), Vec::new()),
                ("pin".to_string(), Vec::new()),
                ("v".to_string(), Vec::new()),
            ],
        };
        let mut disabled = std::collections::HashSet::new();
        disabled.insert("bus[data.other].pin".to_string());

        assert!(!is_in_disabled_component(&qn, &disabled));
    }

    #[test]
    fn test_infer_array_dimensions_1d() {
        // {0} -> [1]
        let expr = Expression::Array {
            elements: vec![Expression::Literal(flat::Literal::Integer(0))],
            is_matrix: false,
        };
        assert_eq!(infer_array_dimensions(&expr), Some(vec![1]));

        // {1, 2, 3} -> [3]
        let expr = Expression::Array {
            elements: vec![
                Expression::Literal(flat::Literal::Integer(1)),
                Expression::Literal(flat::Literal::Integer(2)),
                Expression::Literal(flat::Literal::Integer(3)),
            ],
            is_matrix: false,
        };
        assert_eq!(infer_array_dimensions(&expr), Some(vec![3]));

        // {} -> [0]
        let expr = Expression::Array {
            elements: vec![],
            is_matrix: false,
        };
        assert_eq!(infer_array_dimensions(&expr), Some(vec![0]));
    }

    #[test]
    fn test_infer_array_dimensions_2d_matrix() {
        // {{1, 2}, {3, 4}} -> [2, 2]
        let expr = Expression::Array {
            elements: vec![
                Expression::Array {
                    elements: vec![
                        Expression::Literal(flat::Literal::Integer(1)),
                        Expression::Literal(flat::Literal::Integer(2)),
                    ],
                    is_matrix: false,
                },
                Expression::Array {
                    elements: vec![
                        Expression::Literal(flat::Literal::Integer(3)),
                        Expression::Literal(flat::Literal::Integer(4)),
                    ],
                    is_matrix: false,
                },
            ],
            is_matrix: true,
        };
        assert_eq!(infer_array_dimensions(&expr), Some(vec![2, 2]));
    }

    #[test]
    fn test_infer_array_dimensions_single_row_matrix() {
        // [1, 2] -> [1, 2]
        let expr = Expression::Array {
            elements: vec![
                Expression::Literal(flat::Literal::Integer(1)),
                Expression::Literal(flat::Literal::Integer(2)),
            ],
            is_matrix: true,
        };
        assert_eq!(infer_array_dimensions(&expr), Some(vec![1, 2]));
    }

    #[test]
    fn test_infer_array_dimensions_non_array() {
        // Scalar literal -> None
        let expr = Expression::Literal(flat::Literal::Integer(5));
        assert_eq!(infer_array_dimensions(&expr), None);

        // flat::Variable reference -> None
        let expr = Expression::VarRef {
            name: flat::VarName::new("x"),
            subscripts: vec![],
        };
        assert_eq!(infer_array_dimensions(&expr), None);
    }

    #[test]
    fn test_eval_integer_params_overwrites_stale_seed_value() {
        let mut ctx = Context::new();
        ctx.parameter_values.insert("pipe.n".to_string(), 3);
        ctx.parameter_values
            .insert("pipe.flowModel.n".to_string(), 5);

        let params = vec![(
            "pipe.flowModel.n".to_string(),
            Expression::Binary {
                op: flat::OpBinary::Sub(flat::Token::default()),
                lhs: Box::new(Expression::VarRef {
                    name: flat::VarName::new("pipe.n"),
                    subscripts: vec![],
                }),
                rhs: Box::new(Expression::Literal(flat::Literal::Integer(1))),
            },
        )];

        let progress = ctx.eval_integer_params(&params);
        assert!(
            progress,
            "expected reevaluation to replace stale seeded value"
        );
        assert_eq!(ctx.get_integer_param("pipe.flowModel.n"), Some(2));
    }

    #[test]
    fn test_eval_integer_params_reconciles_conflicting_integral_real_value() {
        let mut ctx = Context::new();
        ctx.parameter_values
            .insert("pipe.flowModel.n".to_string(), 5);
        ctx.real_parameter_values
            .insert("pipe.flowModel.n".to_string(), 2.0);

        let progress = ctx.eval_integer_params(&[]);
        assert!(
            progress,
            "expected conflicting seeded integer to be corrected"
        );
        assert_eq!(ctx.get_integer_param("pipe.flowModel.n"), Some(2));
    }

    #[test]
    fn test_get_integer_param_prefers_conflicting_integral_real_value() {
        let mut ctx = Context::new();
        ctx.parameter_values
            .insert("pipe.flowModel.n".to_string(), 5);
        ctx.real_parameter_values
            .insert("pipe.flowModel.n".to_string(), 2.0);

        assert_eq!(ctx.get_integer_param("pipe.flowModel.n"), Some(2));
    }

    #[test]
    fn test_propagate_unexpanded_record_array_dims_prepends_parent_dims_to_field_arrays() {
        let mut flat = flat::Model::default();

        let mut field = flat::Variable {
            name: flat::VarName::new("world.cylinders.R.T"),
            is_primitive: true,
            dims: vec![3, 3],
            ..Default::default()
        };
        flat.add_variable(field.name.clone(), field.clone());

        field.name = flat::VarName::new("world.cylinders.r_shape");
        field.dims = vec![3];
        flat.add_variable(field.name.clone(), field);

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            InstanceData {
                instance_id: InstanceId::new(1),
                qualified_name: QualifiedName::from_dotted("world.cylinders"),
                dims: vec![2],
                is_primitive: false,
                ..Default::default()
            },
        );

        propagate_unexpanded_record_array_dims(&mut flat, &overlay);

        assert_eq!(
            flat.variables
                .get(&flat::VarName::new("world.cylinders.R.T"))
                .expect("missing field R.T")
                .dims,
            vec![2, 3, 3]
        );
        assert_eq!(
            flat.variables
                .get(&flat::VarName::new("world.cylinders.r_shape"))
                .expect("missing field r_shape")
                .dims,
            vec![2, 3]
        );
    }

    #[test]
    fn test_propagate_unexpanded_record_array_dims_does_not_double_prefix_dims() {
        let mut flat = flat::Model::default();
        let var_name = flat::VarName::new("world.cylinders.R.T");
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name.clone(),
                dims: vec![2, 3, 3],
                is_primitive: true,
                ..Default::default()
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            InstanceData {
                instance_id: InstanceId::new(1),
                qualified_name: QualifiedName::from_dotted("world.cylinders"),
                dims: vec![2],
                is_primitive: false,
                ..Default::default()
            },
        );

        propagate_unexpanded_record_array_dims(&mut flat, &overlay);

        assert_eq!(
            flat.variables
                .get(&var_name)
                .expect("missing R.T field")
                .dims,
            vec![2, 3, 3]
        );
    }

    #[test]
    fn test_propagate_unexpanded_record_array_dims_combines_parent_and_child_from_start() {
        let mut flat = flat::Model::default();
        let var_name = flat::VarName::new("world.cylinders.lengthDirection");
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name.clone(),
                is_primitive: true,
                dims: vec![2],
                binding: Some(Expression::Array {
                    elements: vec![
                        Expression::VarRef {
                            name: flat::VarName::new("a"),
                            subscripts: vec![],
                        },
                        Expression::VarRef {
                            name: flat::VarName::new("b"),
                            subscripts: vec![],
                        },
                    ],
                    is_matrix: false,
                }),
                start: Some(Expression::Array {
                    elements: vec![
                        Expression::Literal(flat::Literal::Integer(1)),
                        Expression::Literal(flat::Literal::Integer(0)),
                        Expression::Literal(flat::Literal::Integer(0)),
                    ],
                    is_matrix: false,
                }),
                ..Default::default()
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            InstanceData {
                instance_id: InstanceId::new(1),
                qualified_name: QualifiedName::from_dotted("world.cylinders"),
                dims: vec![2],
                is_primitive: false,
                ..Default::default()
            },
        );

        propagate_unexpanded_record_array_dims(&mut flat, &overlay);

        assert_eq!(
            flat.variables
                .get(&var_name)
                .expect("missing world.cylinders.lengthDirection")
                .dims,
            vec![2, 3]
        );
    }

    #[test]
    fn test_eval_enum_params_resolves_alias_target_reference() {
        let mut ctx = Context::new();
        ctx.record_aliases
            .insert("pipe1.system".to_string(), "system".to_string());

        let params = vec![
            (
                "system.energyDynamics".to_string(),
                Expression::VarRef {
                    name: flat::VarName::new("Modelica.Fluid.Types.Dynamics.SteadyStateInitial"),
                    subscripts: vec![],
                },
            ),
            (
                "pipe1.energyDynamics".to_string(),
                Expression::VarRef {
                    name: flat::VarName::new("pipe1.system.energyDynamics"),
                    subscripts: vec![],
                },
            ),
        ];

        ctx.eval_enum_params(&params);

        assert_eq!(
            ctx.get_enum_param("pipe1.energyDynamics"),
            Some("Modelica.Fluid.Types.Dynamics.SteadyStateInitial".to_string())
        );
    }

    #[test]
    fn test_eval_enum_params_resolves_suffix_reference_without_alias() {
        let mut ctx = Context::new();

        let params = vec![
            (
                "system.energyDynamics".to_string(),
                Expression::VarRef {
                    name: flat::VarName::new("Modelica.Fluid.Types.Dynamics.SteadyStateInitial"),
                    subscripts: vec![],
                },
            ),
            (
                "pipe1.energyDynamics".to_string(),
                Expression::VarRef {
                    name: flat::VarName::new("pipe1.system.energyDynamics"),
                    subscripts: vec![],
                },
            ),
        ];

        ctx.eval_enum_params(&params);

        assert_eq!(
            ctx.get_enum_param("pipe1.energyDynamics"),
            Some("Modelica.Fluid.Types.Dynamics.SteadyStateInitial".to_string())
        );
    }

    #[test]
    fn test_eval_enum_params_resolves_uppercase_scoped_reference() {
        let mut ctx = Context::new();

        let params = vec![
            (
                "system.energyDynamics".to_string(),
                Expression::VarRef {
                    name: flat::VarName::new("Modelica.Fluid.Types.Dynamics.SteadyStateInitial"),
                    subscripts: vec![],
                },
            ),
            (
                "HEX.energyDynamics".to_string(),
                Expression::VarRef {
                    name: flat::VarName::new("HEX.system.energyDynamics"),
                    subscripts: vec![],
                },
            ),
        ];

        ctx.eval_enum_params(&params);

        assert_eq!(
            ctx.get_enum_param("HEX.energyDynamics"),
            Some("Modelica.Fluid.Types.Dynamics.SteadyStateInitial".to_string())
        );
    }

    #[test]
    fn test_eval_enum_params_normalizes_transitive_reference_values() {
        let mut ctx = Context::new();

        let params = vec![
            (
                "system.energyDynamics".to_string(),
                Expression::VarRef {
                    name: flat::VarName::new("Modelica.Fluid.Types.Dynamics.SteadyStateInitial"),
                    subscripts: vec![],
                },
            ),
            (
                "pipe.energyDynamics".to_string(),
                Expression::VarRef {
                    name: flat::VarName::new("system.energyDynamics"),
                    subscripts: vec![],
                },
            ),
            (
                "HEX.energyDynamics".to_string(),
                Expression::VarRef {
                    name: flat::VarName::new("pipe.energyDynamics"),
                    subscripts: vec![],
                },
            ),
        ];

        ctx.eval_enum_params(&params);

        assert_eq!(
            ctx.get_enum_param("HEX.energyDynamics"),
            Some("Modelica.Fluid.Types.Dynamics.SteadyStateInitial".to_string())
        );
    }

    #[test]
    fn test_eval_enum_params_resolves_conditional_enum_binding_from_known_boolean() {
        let mut ctx = Context::new();
        ctx.boolean_parameter_values
            .insert("Medium.singleState".to_string(), true);

        let params = vec![
            (
                "systemMassDynamics".to_string(),
                Expression::If {
                    branches: vec![(
                        Expression::VarRef {
                            name: flat::VarName::new("Medium.singleState"),
                            subscripts: vec![],
                        },
                        Expression::VarRef {
                            name: flat::VarName::new("Dynamics.SteadyState"),
                            subscripts: vec![],
                        },
                    )],
                    else_branch: Box::new(Expression::VarRef {
                        name: flat::VarName::new("Dynamics.SteadyStateInitial"),
                        subscripts: vec![],
                    }),
                },
            ),
            (
                "system.massDynamics".to_string(),
                Expression::VarRef {
                    name: flat::VarName::new("systemMassDynamics"),
                    subscripts: vec![],
                },
            ),
            (
                "pipe1.massDynamics".to_string(),
                Expression::VarRef {
                    name: flat::VarName::new("pipe1.system.massDynamics"),
                    subscripts: vec![],
                },
            ),
            (
                "pipe1.traceDynamics".to_string(),
                Expression::VarRef {
                    name: flat::VarName::new("pipe1.massDynamics"),
                    subscripts: vec![],
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
    fn test_lookup_with_scope_dotted_name_uses_full_suffix() {
        let mut values: rustc_hash::FxHashMap<String, i64> = rustc_hash::FxHashMap::default();
        values.insert("source.medium.nXi".to_string(), 3);

        assert_eq!(lookup_with_scope("medium.nXi", "", &values), Some(3));
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
    fn test_eval_lookup_trait_resolves_scoped_and_suffix_values() {
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
        ctx.record_aliases
            .insert("sys.alias".to_string(), "sys".to_string());

        assert_eq!(ctx.lookup_integer("n", "sys.inner"), Some(4));
        assert_eq!(ctx.lookup_integer("n", "sys.alias.inner"), Some(4));
        assert_eq!(ctx.lookup_real("r", "sys.inner"), Some(2.5));
        assert_eq!(ctx.lookup_boolean("flag", "sys.inner"), Some(true));
        assert_eq!(
            ctx.lookup_enum("mode", "sys.inner").as_deref(),
            Some("Pkg.Mode.Fast")
        );
        assert_eq!(ctx.lookup_integer("medium.nXi", ""), Some(3));
    }

    #[test]
    fn test_resolve_through_prefix_handles_dot_inside_subscript_expression() {
        let mut aliases: rustc_hash::FxHashMap<String, String> = rustc_hash::FxHashMap::default();
        aliases.insert("bus[data.medium]".to_string(), "busMedium".to_string());

        let resolved = resolve_through_prefix("bus[data.medium].pin.v", "source", &aliases);
        assert_eq!(resolved.as_deref(), Some("busMedium.pin.v"));
    }

    #[test]
    fn test_synthesize_intermediate_aliases_handles_dot_inside_subscript_expression() {
        let mut aliases: rustc_hash::FxHashMap<String, String> = rustc_hash::FxHashMap::default();
        aliases.insert("stack.stackData".to_string(), "stackData".to_string());
        aliases.insert(
            "src".to_string(),
            "stack.cell[data.medium].stackData.cellData".to_string(),
        );

        synthesize_intermediate_aliases(&mut aliases);

        assert_eq!(
            aliases.get("stack.cell[data.medium].stackData"),
            Some(&"stack.stackData".to_string())
        );
    }

    #[test]
    fn test_infer_function_call_dims_uses_bracket_aware_leaf_matching() {
        let mut function_output_dims = DimMap::new();
        function_output_dims.insert("foo[data.medium]".to_string(), vec![2]);
        function_output_dims.insert("bar[data.medium]".to_string(), vec![3]);

        assert_eq!(
            infer_function_call_dims("model.foo[data.medium]", &function_output_dims),
            Some(vec![2]),
            "dot-inside-subscript should not create ambiguous leaf matching"
        );
    }

    #[test]
    fn test_infer_expr_dims_handles_array_comprehension() {
        let expr = Expression::ArrayComprehension {
            expr: Box::new(Expression::Array {
                elements: vec![
                    Expression::Literal(flat::Literal::Integer(1)),
                    Expression::Literal(flat::Literal::Integer(2)),
                ],
                is_matrix: false,
            }),
            indices: vec![rumoca_ir_flat::ComprehensionIndex {
                name: "i".to_string(),
                range: Expression::Range {
                    start: Box::new(Expression::Literal(flat::Literal::Integer(1))),
                    step: None,
                    end: Box::new(Expression::Literal(flat::Literal::Integer(3))),
                },
            }],
            filter: None,
        };

        assert_eq!(
            infer_expr_dims(&expr, &DimMap::new(), &DimMap::new()),
            Some(vec![3, 2])
        );
    }

    #[test]
    fn test_infer_expr_dims_array_comprehension_with_filter_returns_none() {
        let expr = Expression::ArrayComprehension {
            expr: Box::new(Expression::Literal(flat::Literal::Integer(1))),
            indices: vec![rumoca_ir_flat::ComprehensionIndex {
                name: "i".to_string(),
                range: Expression::Range {
                    start: Box::new(Expression::Literal(flat::Literal::Integer(1))),
                    step: None,
                    end: Box::new(Expression::Literal(flat::Literal::Integer(3))),
                },
            }],
            filter: Some(Box::new(Expression::VarRef {
                name: "cond".to_string().into(),
                subscripts: Vec::new(),
            })),
        };

        assert_eq!(infer_expr_dims(&expr, &DimMap::new(), &DimMap::new()), None);
    }

    fn token(name: &str) -> rumoca_ir_core::Token {
        rumoca_ir_core::Token {
            text: Arc::from(name.to_string()),
            ..rumoca_ir_core::Token::default()
        }
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
                ..Default::default()
            },
        );
        tree.definitions.classes.insert("Host".to_string(), host);
        tree.def_map.insert(host_def_id, "Host".to_string());
        tree.name_map.insert("Host".to_string(), host_def_id);

        let instance = InstanceData {
            type_def_id: Some(host_def_id),
            ..Default::default()
        };

        let overrides = component_overrides(&instance, &tree);
        assert_eq!(
            overrides.get("noise"),
            Some(&"DefaultNoise".to_string()),
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
                ..Default::default()
            },
        );
        tree.definitions.classes.insert("Host".to_string(), host);
        tree.def_map.insert(host_def_id, "Host".to_string());
        tree.name_map.insert("Host".to_string(), host_def_id);

        let instance = InstanceData {
            type_def_id: Some(host_def_id),
            ..Default::default()
        };

        let overrides = component_overrides(&instance, &tree);
        assert_eq!(
            overrides.get("frictionParameters"),
            Some(&"FrictionParameters".to_string()),
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
                ..Default::default()
            },
        );
        tree.definitions.classes.insert("Host".to_string(), host);
        tree.def_map.insert(host_def_id, "Host".to_string());
        tree.name_map.insert("Host".to_string(), host_def_id);

        let mut instance = InstanceData {
            type_def_id: Some(host_def_id),
            ..Default::default()
        };
        instance
            .class_overrides
            .insert("noise".to_string(), redeclared_noise_def_id);

        let overrides = component_overrides(&instance, &tree);
        assert_eq!(
            overrides.get("noise"),
            Some(&"RedeclaredNoise".to_string()),
            "explicit class redeclare must override default replaceable binding"
        );
    }
}
