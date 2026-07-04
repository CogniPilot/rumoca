// SPEC_0021 file-size exception: pipeline context tests share class-tree and
// lookup fixtures across constants, dimensions, and redeclare paths. split plan:
// move constant-injection, scoped lookup, and redeclare fixtures into modules.
use super::*;
#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::EvalLookup;
    use rumoca_core::{ClassType, DefId};
    use rumoca_ir_ast as ast;
    use rumoca_ir_ast::{ClassDef, ClassTree, Component, InstanceData, InstanceId};
    use rumoca_ir_flat as flat;
    use std::sync::Arc;
    const TEST_FILE: &str = "context_tests.mo";
    #[path = "context_tests_modified_binding_dimensions.rs"]
    mod modified_binding_dimensions;
    fn test_source_location() -> rumoca_core::Location {
        rumoca_core::Location {
            start_line: 1,
            start_column: 1,
            end_line: 1,
            end_column: 2,
            start: 0,
            end: 1,
            file_name: TEST_FILE.to_string(),
        }
    }

    fn add_test_source(tree: &mut ClassTree) {
        tree.source_map.add(TEST_FILE, "component x;");
    }

    fn source_backed_tree() -> ClassTree {
        let mut tree = ClassTree::default();
        add_test_source(&mut tree);
        tree
    }

    fn symbolic_instance(
        instance_id: InstanceId,
        qualified_name: &str,
        dims_expr: Vec<ast::Subscript>,
    ) -> InstanceData {
        InstanceData {
            instance_id,
            component_ref: None,
            qualified_name: QualifiedName::from_dotted(qualified_name),
            source_location: test_source_location(),
            dims: Vec::new(),
            dims_expr,
            type_id: rumoca_core::TypeId::default(),
            type_name: String::new(),
            type_def_id: None,
            declaration_source_scope: None,
            class_overrides: ast::ClassOverrideMap::default(),
            has_forwarding_class_redeclare: false,
            variability: rumoca_core::Variability::Empty,
            causality: rumoca_core::Causality::Empty,
            flow: false,
            stream: false,
            start: None,
            fixed: None,
            min: None,
            max: None,
            nominal: None,
            quantity: None,
            unit: None,
            display_unit: None,
            description: None,
            state_select: rumoca_core::StateSelect::default(),
            binding: None,
            binding_source: None,
            binding_source_scope: None,
            attribute_source_scopes: ast::AstIndexMap::default(),
            binding_from_modification: false,
            is_primitive: true,
            is_discrete_type: false,
            from_expandable_connector: false,
            evaluate: false,
            is_final: false,
            is_overconstrained: false,
            is_protected: false,
            is_connector_type: false,
            oc_record_path: None,
            oc_eq_constraint_size: None,
        }
    }

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("flatten_context_test.mo"),
            8,
            32,
        )
    }

    fn component_ref_expr(path: &str) -> ast::Expression {
        let parts = crate::path_utils::segments(path)
            .into_iter()
            .map(|segment| ast::ComponentRefPart {
                ident: rumoca_core::Token {
                    text: Arc::from(segment),
                    ..rumoca_core::Token::default()
                },
                subs: None,
            })
            .collect();

        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts,
            def_id: None,
            span: test_span(),
        })
    }

    fn component_ref_expr_with_def_id(path: &str, def_id: DefId) -> ast::Expression {
        let mut expr = component_ref_expr(path);
        if let ast::Expression::ComponentReference(reference) = &mut expr {
            reference.def_id = Some(def_id);
        }
        expr
    }

    fn int_lit(value: i64) -> Expression {
        Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn real_lit(value: f64) -> Expression {
        Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn var_ref(name: &str) -> Expression {
        Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn div_expr(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: rumoca_core::OpBinary::Div,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn mul_expr(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn sqrt_expr(arg: Expression) -> Expression {
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sqrt,
            args: vec![arg],
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn int_array(values: &[i64]) -> Expression {
        Expression::Array {
            elements: values.iter().copied().map(int_lit).collect(),
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn fill_expr(value: i64, dims: &[i64]) -> Expression {
        let mut args = vec![int_lit(value)];
        args.extend(dims.iter().copied().map(int_lit));
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn size_dim_expr(name: &str, dim: i64) -> Expression {
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args: vec![
                Expression::VarRef {
                    name: rumoca_core::Reference::new(name),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                },
                int_lit(dim),
            ],
            span: rumoca_core::Span::DUMMY,
        }
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
        let mut disabled = indexmap::IndexSet::new();
        disabled.insert(rumoca_core::ComponentPath::from_parts([
            "bus[data.medium]",
            "pin",
        ]));

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
        let mut disabled = indexmap::IndexSet::new();
        disabled.insert(rumoca_core::ComponentPath::from_parts([
            "bus[data.other]",
            "pin",
        ]));

        assert!(!is_in_disabled_component(&qn, &disabled));
    }

    #[test]
    fn test_infer_array_dimensions_1d() {
        // {0} -> [1]
        let expr = Expression::Array {
            elements: vec![Expression::Literal {
                value: rumoca_core::Literal::Integer(0),
                span: rumoca_core::Span::DUMMY,
            }],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(infer_array_dimensions(&expr), Some(vec![1]));

        // {1, 2, 3} -> [3]
        let expr = Expression::Array {
            elements: vec![
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                },
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: rumoca_core::Span::DUMMY,
                },
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(3),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(infer_array_dimensions(&expr), Some(vec![3]));

        // {} -> [0]
        let expr = Expression::Array {
            elements: vec![],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
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
                },
                Expression::Array {
                    elements: vec![
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(3),
                            span: rumoca_core::Span::DUMMY,
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(4),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: true,
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(infer_array_dimensions(&expr), Some(vec![2, 2]));
    }

    #[test]
    fn test_infer_array_dimensions_single_row_matrix() {
        // [1, 2] -> [1, 2]
        let expr = Expression::Array {
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
            is_matrix: true,
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(infer_array_dimensions(&expr), Some(vec![1, 2]));
    }

    #[test]
    fn test_infer_array_dimensions_non_array() {
        // Scalar literal -> None
        let expr = Expression::Literal {
            value: rumoca_core::Literal::Integer(5),
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(infer_array_dimensions(&expr), None);

        // flat::Variable reference -> None
        let expr = Expression::VarRef {
            name: rumoca_core::Reference::new("x"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(infer_array_dimensions(&expr), None);
    }

    #[test]
    fn concrete_dimensions_replace_unknown_same_rank_dimensions() {
        assert!(dims_are_better(&[4], &[0]));
        assert!(dims_are_better(&[3, 2], &[3, 0]));
        assert!(dims_are_better(&[8, 2], &[1, 2]));
        assert!(!dims_are_better(&[0], &[4]));
        assert!(!dims_are_better(&[1, 2], &[8, 2]));
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
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(Expression::VarRef {
                    name: rumoca_core::Reference::new("pipe.n"),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
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
    fn test_modified_integer_binding_resolves_in_enclosing_scope() {
        let mut ctx = Context::new();
        ctx.parameter_values.insert("order".to_string(), 3);
        ctx.parameter_values.insert("filter.order".to_string(), 2);

        let params = vec![(
            "filter.order".to_string(),
            Expression::VarRef {
                name: rumoca_core::Reference::new("order"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
        )];

        let progress = ctx.eval_modified_integer_params(&params);
        assert!(
            progress,
            "modifier-origin binding should replace component declaration default"
        );
        assert_eq!(ctx.get_integer_param("filter.order"), Some(3));
    }

    #[test]
    fn test_modified_integer_binding_updates_dependent_enum_if_parameter() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();

        for (name, binding, binding_from_modification) in [
            ("order", int_lit(3), false),
            ("filter.order", var_ref("order"), true),
            (
                "filter.analogFilter",
                var_ref("AnalogFilter.CriticalDamping"),
                false,
            ),
            (
                "filter.nr",
                Expression::If {
                    branches: vec![(
                        Expression::Binary {
                            op: rumoca_core::OpBinary::Eq,
                            lhs: Box::new(var_ref("filter.analogFilter")),
                            rhs: Box::new(var_ref("AnalogFilter.CriticalDamping")),
                            span: rumoca_core::Span::DUMMY,
                        },
                        var_ref("filter.order"),
                    )],
                    else_branch: Box::new(Expression::BuiltinCall {
                        function: rumoca_core::BuiltinFunction::Mod,
                        args: vec![var_ref("filter.order"), int_lit(2)],
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                },
                false,
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

        assert_eq!(ctx.get_integer_param("filter.order"), Some(3));
        assert_eq!(ctx.get_integer_param("filter.nr"), Some(3));
    }

    #[test]
    fn real_modifier_bindings_resolve_in_enclosing_scope_for_sibling_instances() {
        let mut ctx = Context::new();
        let tree = ClassTree::default();
        let mut flat = flat::Model::default();

        for (name, binding, from_modification) in [
            ("td", real_lit(0.002), false),
            ("line1.TD", div_expr(var_ref("td"), int_lit(2)), true),
            ("line2.TD", div_expr(var_ref("line2.td"), int_lit(2)), true),
        ] {
            let var_name = rumoca_core::VarName::new(name);
            flat.add_variable(
                var_name.clone(),
                flat::Variable {
                    name: var_name,
                    variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                    binding: Some(binding),
                    binding_from_modification: from_modification,
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(test_span())
                },
            );
        }

        ctx.build_parameter_lookup(&flat, &tree);

        assert_eq!(ctx.real_parameter_values.get("line1.TD"), Some(&0.001));
        assert_eq!(ctx.real_parameter_values.get("line2.TD"), Some(&0.001));
    }

    #[test]
    fn real_modifier_binding_parameter_is_non_structural_even_when_fixed_default() {
        let mut ctx = Context::new();
        let tree = ClassTree::default();
        let mut flat = flat::Model::default();
        let name = rumoca_core::VarName::new("force.v_nominal");
        flat.add_variable(
            name.clone(),
            flat::Variable {
                name,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(real_lit(5.0)),
                binding_from_modification: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        ctx.build_parameter_lookup(&flat, &tree);

        assert!(ctx.non_structural_params.contains("force.v_nominal"));
        assert_eq!(ctx.real_parameter_values.get("force.v_nominal"), Some(&5.0));
    }

    #[test]
    fn real_modifier_bindings_resolve_transmission_line_delay_chain() {
        let mut ctx = Context::new();
        let tree = ClassTree::default();
        let mut flat = flat::Model::default();

        let c0 = div_expr(
            real_lit(1.0),
            sqrt_expr(mul_expr(var_ref("l"), var_ref("c"))),
        );
        let td = div_expr(var_ref("len"), var_ref("c0"));
        for (name, binding, from_modification) in [
            ("l", real_lit(1.0e-6), false),
            ("c", real_lit(15.0e-12), false),
            ("len", real_lit(100.0e3), false),
            ("c0", c0, false),
            ("td", td, false),
            ("line1.TD", div_expr(var_ref("td"), int_lit(2)), true),
            ("line2.TD", div_expr(var_ref("td"), int_lit(2)), true),
        ] {
            let var_name = rumoca_core::VarName::new(name);
            flat.add_variable(
                var_name.clone(),
                flat::Variable {
                    name: var_name,
                    variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                    binding: Some(binding),
                    binding_from_modification: from_modification,
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(test_span())
                },
            );
        }

        ctx.build_parameter_lookup(&flat, &tree);

        let expected = 100.0e3 * (1.0e-6_f64 * 15.0e-12_f64).sqrt() / 2.0;
        for name in ["line1.TD", "line2.TD"] {
            let value = ctx
                .real_parameter_values
                .get(name)
                .copied()
                .expect("line delay should evaluate");
            assert!((value - expected).abs() < 1.0e-15);
        }
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
            name: rumoca_core::VarName::new("world.cylinders.R.T"),
            is_primitive: true,
            dims: vec![3, 3],
            ..flat::Variable::empty_with_span(test_span())
        };
        flat.add_variable(field.name.clone(), field.clone());

        field.name = rumoca_core::VarName::new("world.cylinders.r_shape");
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
                .get(&rumoca_core::VarName::new("world.cylinders.R.T"))
                .expect("missing field R.T")
                .dims,
            vec![2, 3, 3]
        );
        assert_eq!(
            flat.variables
                .get(&rumoca_core::VarName::new("world.cylinders.r_shape"))
                .expect("missing field r_shape")
                .dims,
            vec![2, 3]
        );
    }

    #[test]
    fn test_propagate_unexpanded_component_array_dims_to_model_fields() {
        let mut flat = flat::Model::default();
        let var_name = rumoca_core::VarName::new("sensor.child.y");
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name.clone(),
                is_primitive: true,
                dims: Vec::new(),
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            InstanceData {
                instance_id: InstanceId::new(1),
                qualified_name: QualifiedName::from_dotted("sensor.child"),
                dims: vec![6],
                is_primitive: false,
                ..Default::default()
            },
        );

        propagate_unexpanded_record_array_dims(&mut flat, &overlay);

        assert_eq!(
            flat.variables
                .get(&var_name)
                .expect("missing sensor child field")
                .dims,
            vec![6]
        );
    }

    #[test]
    fn test_propagate_unexpanded_record_array_dims_does_not_double_prefix_dims() {
        let mut flat = flat::Model::default();
        let var_name = rumoca_core::VarName::new("world.cylinders.R.T");
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name.clone(),
                dims: vec![2, 3, 3],
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
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
        let var_name = rumoca_core::VarName::new("world.cylinders.lengthDirection");
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name.clone(),
                is_primitive: true,
                dims: vec![2],
                binding: Some(Expression::Array {
                    elements: vec![
                        Expression::VarRef {
                            name: rumoca_core::Reference::new("a"),
                            subscripts: vec![],
                            span: rumoca_core::Span::DUMMY,
                        },
                        Expression::VarRef {
                            name: rumoca_core::Reference::new("b"),
                            subscripts: vec![],
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                }),
                start: Some(Expression::Array {
                    elements: vec![
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span: rumoca_core::Span::DUMMY,
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(0),
                            span: rumoca_core::Span::DUMMY,
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(0),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                }),
                ..flat::Variable::empty_with_span(test_span())
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
    fn symbolic_component_dimensions_replace_stale_flat_dimensions() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();
        let nf_name = rumoca_core::VarName::new("realFFT.nf");
        flat.add_variable(
            nf_name.clone(),
            flat::Variable {
                name: nf_name,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(Expression::Literal {
                    value: rumoca_core::Literal::Integer(401),
                    span: rumoca_core::Span::DUMMY,
                }),
                is_discrete_type: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let abs_name = rumoca_core::VarName::new("realFFT.abs");
        flat.add_variable(
            abs_name.clone(),
            flat::Variable {
                name: abs_name.clone(),
                dims: vec![4],
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "realFFT.abs",
                vec![ast::Subscript::Expression(component_ref_expr("realFFT.nf"))],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree);
        assert_eq!(ctx.array_dimensions.get("realFFT.abs"), Some(&vec![4]));

        let changed = ctx
            .recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect("symbolic dimensions should resolve");

        assert!(changed);
        assert_eq!(
            flat.variables.get(&abs_name).expect("abs variable").dims,
            vec![401]
        );
        assert_eq!(ctx.array_dimensions.get("realFFT.abs"), Some(&vec![401]));
    }

    #[test]
    fn symbolic_component_dimensions_use_modified_parameter_value() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();

        let root_n = rumoca_core::VarName::new("N");
        flat.add_variable(
            root_n.clone(),
            flat::Variable {
                name: root_n,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(int_lit(7)),
                is_discrete_type: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let local_n = rumoca_core::VarName::new("aD_Converter.N");
        flat.add_variable(
            local_n.clone(),
            flat::Variable {
                name: local_n,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(var_ref("N")),
                binding_from_modification: true,
                start: Some(Expression::Literal {
                    value: rumoca_core::Literal::Real(8.0),
                    span: test_span(),
                }),
                is_discrete_type: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let y_name = rumoca_core::VarName::new("aD_Converter.y");
        flat.add_variable(
            y_name.clone(),
            flat::Variable {
                name: y_name.clone(),
                dims: vec![8],
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "aD_Converter.y",
                vec![ast::Subscript::Expression(component_ref_expr(
                    "aD_Converter.N",
                ))],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree);
        assert_eq!(ctx.get_integer_param("aD_Converter.N"), Some(7));

        let changed = ctx
            .recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect("modified symbolic dimension should resolve");

        assert!(changed);
        assert_eq!(
            flat.variables.get(&y_name).expect("y variable").dims,
            vec![7]
        );
        assert_eq!(ctx.array_dimensions.get("aD_Converter.y"), Some(&vec![7]));
    }

    #[test]
    fn symbolic_component_dimensions_resolve_package_member_target_def_constant() {
        let nstate_def = DefId::new(42);
        let target_name = "Modelica.Math.Random.Generators.Xorshift128plus.nState";
        let mut ctx = Context::new();
        ctx.target_def_names
            .insert(nstate_def, target_name.to_string());
        ctx.parameter_values.insert(target_name.to_string(), 4);

        let mut tree = source_backed_tree();
        tree.def_map.insert(nstate_def, target_name.to_string());
        let mut flat = flat::Model::default();
        let state_name = rumoca_core::VarName::new("motor.uniformNoise.state");
        flat.add_variable(
            state_name.clone(),
            flat::Variable {
                name: state_name.clone(),
                dims: vec![1],
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "motor.uniformNoise.state",
                vec![ast::Subscript::Expression(component_ref_expr_with_def_id(
                    "generator.nState",
                    nstate_def,
                ))],
            ),
        );

        let changed = ctx
            .recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect("dimension target definition constant should resolve");

        assert!(changed);
        assert_eq!(
            flat.variables.get(&state_name).expect("state").dims,
            vec![4]
        );
        assert_eq!(
            ctx.array_dimensions.get("motor.uniformNoise.state"),
            Some(&vec![4])
        );
    }

    #[test]
    fn enum_type_component_dimensions_use_literal_count() {
        let mut ctx = Context::new();
        let mut tree = ClassTree::new();
        add_test_source(&mut tree);
        let logic_def_id = DefId::new(42);
        let mut logic = ClassDef {
            name: token("Logic"),
            class_type: ClassType::Type,
            def_id: Some(logic_def_id),
            ..Default::default()
        };
        for literal in ["U", "X", "0", "1", "Z", "W", "L", "H", "-"] {
            logic.enum_literals.push(ast::EnumLiteral {
                ident: token(literal),
                description: Vec::new(),
            });
        }
        tree.definitions.classes.insert("Logic".to_string(), logic);
        tree.def_map.insert(logic_def_id, "Logic".to_string());
        tree.name_map.insert("Logic".to_string(), logic_def_id);

        let mut flat = flat::Model::default();
        let table_name = rumoca_core::VarName::new("gate.delayTable");
        flat.add_variable(
            table_name.clone(),
            flat::Variable {
                name: table_name.clone(),
                dims: vec![1, 1],
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "gate.delayTable",
                vec![
                    ast::Subscript::Expression(ast::Expression::ComponentReference(
                        ast::ComponentReference {
                            local: false,
                            parts: vec![ast::ComponentRefPart {
                                ident: token("Logic"),
                                subs: None,
                            }],
                            def_id: Some(logic_def_id),
                            span: rumoca_core::Span::DUMMY,
                        },
                    )),
                    ast::Subscript::Expression(ast::Expression::ComponentReference(
                        ast::ComponentReference {
                            local: false,
                            parts: vec![ast::ComponentRefPart {
                                ident: token("Logic"),
                                subs: None,
                            }],
                            def_id: Some(logic_def_id),
                            span: rumoca_core::Span::DUMMY,
                        },
                    )),
                ],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree);

        let changed = ctx
            .recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect("enum type dimensions should resolve from literal count");

        assert!(changed);
        assert_eq!(
            flat.variables
                .get(&table_name)
                .expect("delayTable variable")
                .dims,
            vec![9, 9]
        );
        assert_eq!(
            ctx.array_dimensions.get("gate.delayTable"),
            Some(&vec![9, 9])
        );
    }

    #[test]
    fn colon_component_dimensions_use_enum_range_binding_shape() {
        let mut ctx = Context::new();
        let mut tree = ClassTree::new();
        add_test_source(&mut tree);
        let logic_def_id = DefId::new(42);
        let mut logic = ClassDef {
            name: token("Logic"),
            class_type: ClassType::Type,
            def_id: Some(logic_def_id),
            ..Default::default()
        };
        for literal in ["U", "X", "0", "1", "Z", "W", "L", "H", "-"] {
            logic.enum_literals.push(ast::EnumLiteral {
                ident: token(literal),
                description: Vec::new(),
            });
        }
        tree.definitions.classes.insert("Logic".to_string(), logic);
        tree.def_map.insert(logic_def_id, "Logic".to_string());
        tree.name_map.insert("Logic".to_string(), logic_def_id);

        let enum_ref = |literal: &str| {
            let component_ref = rumoca_core::ComponentReference {
                local: false,
                span: rumoca_core::Span::DUMMY,
                parts: vec![
                    rumoca_core::ComponentRefPart {
                        ident: "Logic".to_string(),
                        span: rumoca_core::Span::DUMMY,
                        subs: Vec::new(),
                    },
                    rumoca_core::ComponentRefPart {
                        ident: literal.to_string(),
                        span: rumoca_core::Span::DUMMY,
                        subs: Vec::new(),
                    },
                ],
                def_id: Some(logic_def_id),
            };
            Expression::VarRef {
                name: rumoca_core::Reference::from_component_reference(component_ref),
                subscripts: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            }
        };

        let mut flat = flat::Model::default();
        let values_name = rumoca_core::VarName::new("delay.LogicValues");
        flat.add_variable(
            values_name.clone(),
            flat::Variable {
                name: values_name.clone(),
                dims: vec![1],
                binding: Some(Expression::Range {
                    start: Box::new(enum_ref("U")),
                    step: None,
                    end: Box::new(enum_ref("-")),
                    span: rumoca_core::Span::DUMMY,
                }),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "delay.LogicValues",
                vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree);

        let changed = ctx
            .recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect("enum range binding should resolve colon dimension");

        assert!(changed);
        assert_eq!(
            flat.variables
                .get(&values_name)
                .expect("LogicValues variable")
                .dims,
            vec![9]
        );
        assert_eq!(
            ctx.array_dimensions.get("delay.LogicValues"),
            Some(&vec![9])
        );
    }

    #[test]
    fn colon_component_dimensions_use_binding_shape() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();
        let x_name = rumoca_core::VarName::new("a.x");
        flat.add_variable(
            x_name.clone(),
            flat::Variable {
                name: x_name.clone(),
                dims: vec![1],
                binding: Some(Expression::Array {
                    elements: vec![
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span: rumoca_core::Span::DUMMY,
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(0),
                            span: rumoca_core::Span::DUMMY,
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span: rumoca_core::Span::DUMMY,
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(0),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                }),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "a.x",
                vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree);

        let changed = ctx
            .recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect("colon dimension should resolve from binding shape");

        assert!(changed);
        assert_eq!(
            flat.variables.get(&x_name).expect("a.x variable").dims,
            vec![4]
        );
        assert_eq!(ctx.array_dimensions.get("a.x"), Some(&vec![4]));
    }

    #[test]
    fn colon_component_dimensions_prefer_binding_shape_over_stale_larger_dims() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();
        let x_name = rumoca_core::VarName::new("a.x");
        flat.add_variable(
            x_name.clone(),
            flat::Variable {
                name: x_name.clone(),
                dims: vec![3],
                binding: Some(Expression::Array {
                    elements: vec![Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY,
                    }],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                }),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "a.x",
                vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree);

        let changed = ctx
            .recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect("colon dimension should prefer binding shape");

        assert!(changed);
        assert_eq!(
            flat.variables.get(&x_name).expect("a.x variable").dims,
            vec![1]
        );
        assert_eq!(ctx.array_dimensions.get("a.x"), Some(&vec![1]));
    }

    #[test]
    fn colon_component_dimensions_accept_zero_sized_binding_shape() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();
        let table_name = rumoca_core::VarName::new("table");
        flat.add_variable(
            table_name.clone(),
            flat::Variable {
                name: table_name.clone(),
                dims: vec![0, 2],
                binding: Some(fill_expr(0, &[0, 2])),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "table",
                vec![
                    ast::Subscript::Range {
                        token: rumoca_core::Token::default(),
                    },
                    ast::Subscript::Range {
                        token: rumoca_core::Token::default(),
                    },
                ],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree);

        let changed = ctx
            .recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect("zero-sized binding shape should satisfy colon dimensions");

        assert!(!changed);
        assert_eq!(
            flat.variables
                .get(&table_name)
                .expect("table variable")
                .dims,
            vec![0, 2]
        );
        assert_eq!(ctx.array_dimensions.get("table"), Some(&vec![0, 2]));
    }

    #[test]
    fn colon_component_dimensions_without_shape_fail_fast() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();
        let x_name = rumoca_core::VarName::new("a.x");
        flat.add_variable(
            x_name,
            flat::Variable {
                name: rumoca_core::VarName::new("a.x"),
                dims: vec![1],
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "a.x",
                vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
            ),
        );

        let err = ctx
            .recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect_err("colon dimension without a known shape should fail");

        assert!(matches!(
            err,
            FlattenError::UnresolvedComponentDimension { .. }
        ));
    }

    #[test]
    fn symbolic_colon_component_dimensions_can_reuse_known_flat_dims() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();
        let table_name = rumoca_core::VarName::new("model.table");
        flat.add_variable(
            table_name.clone(),
            flat::Variable {
                name: rumoca_core::VarName::new("model.table"),
                dims: vec![8, 2],
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "model.table",
                vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
            ),
        );

        let changed = ctx
            .recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect("known flat dimensions should satisfy colon resolution");

        assert!(changed);
        assert_eq!(
            flat.variables
                .get(&table_name)
                .expect("table variable should remain present")
                .dims,
            vec![8]
        );
    }

    #[test]
    fn lookup_infers_array_literal_dims_inside_array_component_element() {
        let mut ctx = Context::new();
        let tree = ClassTree::default();
        let mut flat = flat::Model::default();
        let a_name = rumoca_core::VarName::new("adaptor.filter[1].transferFunction[1].a");
        flat.add_variable(
            a_name.clone(),
            flat::Variable {
                name: a_name,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(int_array(&[-1, 1])),
                binding_from_modification: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let nx_name = rumoca_core::VarName::new("adaptor.filter[1].transferFunction[1].nx");
        flat.add_variable(
            nx_name.clone(),
            flat::Variable {
                name: nx_name,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(Expression::Binary {
                    op: rumoca_core::OpBinary::Sub,
                    lhs: Box::new(size_dim_expr("a", 1)),
                    rhs: Box::new(int_lit(1)),
                    span: rumoca_core::Span::DUMMY,
                }),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        ctx.build_parameter_lookup(&flat, &tree);

        assert_eq!(
            ctx.array_dimensions
                .get("adaptor.filter[1].transferFunction[1].a"),
            Some(&vec![2])
        );
        assert_eq!(
            ctx.parameter_values
                .get("adaptor.filter[1].transferFunction[1].nx"),
            Some(&1_i64)
        );
    }

    #[test]
    fn unresolved_symbolic_component_dimensions_fail_before_flat_ir_is_used() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();
        let y_name = rumoca_core::VarName::new("model.y");
        flat.add_variable(
            y_name,
            flat::Variable {
                name: rumoca_core::VarName::new("model.y"),
                dims: vec![1],
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "model.y",
                vec![ast::Subscript::Expression(component_ref_expr(
                    "model.missing_dim",
                ))],
            ),
        );

        let err = ctx
            .recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect_err("unresolved component dimension should fail");

        assert!(matches!(
            err,
            FlattenError::UnresolvedComponentDimension { .. }
        ));
    }

    #[test]
    fn test_eval_enum_params_resolves_alias_target_reference() {
        let mut ctx = Context::new();
        ctx.record_aliases.insert(
            rumoca_core::ComponentPath::from_flat_path("pipe1.system"),
            rumoca_core::ComponentPath::from_flat_path("system"),
        );

        let params = vec![
            (
                "system.energyDynamics".to_string(),
                Expression::VarRef {
                    name: rumoca_core::Reference::new(
                        "Modelica.Fluid.Types.Dynamics.SteadyStateInitial",
                    ),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            (
                "pipe1.energyDynamics".to_string(),
                Expression::VarRef {
                    name: rumoca_core::Reference::new("pipe1.system.energyDynamics"),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
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
    fn test_eval_enum_params_requires_alias_for_outer_like_reference() {
        let mut ctx = Context::new();

        let params = vec![
            (
                "system.energyDynamics".to_string(),
                Expression::VarRef {
                    name: rumoca_core::Reference::new(
                        "Modelica.Fluid.Types.Dynamics.SteadyStateInitial",
                    ),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            (
                "pipe1.energyDynamics".to_string(),
                Expression::VarRef {
                    name: rumoca_core::Reference::new("pipe1.system.energyDynamics"),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                },
            ),
        ];

        ctx.eval_enum_params(&params);

        assert_eq!(ctx.get_enum_param("pipe1.energyDynamics"), None);
    }

    #[test]
    fn test_eval_enum_params_resolves_uppercase_scoped_reference_through_alias() {
        let mut ctx = Context::new();
        ctx.record_aliases.insert(
            rumoca_core::ComponentPath::from_flat_path("HEX.system"),
            rumoca_core::ComponentPath::from_flat_path("system"),
        );

        let params = vec![
            (
                "system.energyDynamics".to_string(),
                Expression::VarRef {
                    name: rumoca_core::Reference::new(
                        "Modelica.Fluid.Types.Dynamics.SteadyStateInitial",
                    ),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            (
                "HEX.energyDynamics".to_string(),
                Expression::VarRef {
                    name: rumoca_core::Reference::new("HEX.system.energyDynamics"),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
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
                    name: rumoca_core::Reference::new(
                        "Modelica.Fluid.Types.Dynamics.SteadyStateInitial",
                    ),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            (
                "pipe.energyDynamics".to_string(),
                Expression::VarRef {
                    name: rumoca_core::Reference::new("system.energyDynamics"),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            (
                "HEX.energyDynamics".to_string(),
                Expression::VarRef {
                    name: rumoca_core::Reference::new("pipe.energyDynamics"),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
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
        let mut aliases: rustc_hash::FxHashMap<
            rumoca_core::ComponentPath,
            rumoca_core::ComponentPath,
        > = rustc_hash::FxHashMap::default();
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
        let mut aliases: rustc_hash::FxHashMap<
            rumoca_core::ComponentPath,
            rumoca_core::ComponentPath,
        > = rustc_hash::FxHashMap::default();
        aliases.insert(
            rumoca_core::ComponentPath::from_flat_path("stack.stackData"),
            rumoca_core::ComponentPath::from_flat_path("stackData"),
        );
        aliases.insert(
            rumoca_core::ComponentPath::from_flat_path("src"),
            rumoca_core::ComponentPath::from_flat_path(
                "stack.cell[data.medium].stackData.cellData",
            ),
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

    fn token(name: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(name.to_string()),
            ..rumoca_core::Token::default()
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
}
