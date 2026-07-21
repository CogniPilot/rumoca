use super::*;

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{ClassType, DefId};
    use rumoca_ir_ast as ast;
    use rumoca_ir_ast::{ClassDef, ClassTree, InstanceData, InstanceId};
    use rumoca_ir_flat as flat;
    use std::sync::Arc;

    const TEST_FILE: &str = "context_tests.mo";

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

    fn token(name: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(name.to_string()),
            ..rumoca_core::Token::default()
        }
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
        field.binding = Some(Expression::Array {
            elements: vec![
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: test_span(),
                },
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: test_span(),
                },
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: test_span(),
                },
            ],
            is_matrix: false,
            span: test_span(),
        });
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
        let binding = flat
            .variables
            .get(&rumoca_core::VarName::new("world.cylinders.r_shape"))
            .and_then(|variable| variable.binding.as_ref())
            .expect("missing field r_shape binding");
        let Expression::ArrayComprehension { expr, indices, .. } = binding else {
            panic!("declaration binding should repeat over the parent component dimensions");
        };
        assert_eq!(indices.len(), 1);
        assert!(matches!(expr.as_ref(), Expression::Array { elements, .. } if elements.len() == 3));
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
    fn test_propagate_unexpanded_record_array_dims_repeats_each_modifier_binding() {
        let mut flat = flat::Model::default();
        let var_name = rumoca_core::VarName::new("world.cylinders.widthDirection");
        let component_ref = rumoca_core::component_reference_from_flat_name(&var_name, test_span())
            .expect("test variable should have a structured component reference");
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name.clone(),
                component_ref: Some(component_ref.clone()),
                dims: vec![3],
                binding: Some(Expression::Array {
                    elements: vec![
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(0),
                            span: test_span(),
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span: test_span(),
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(0),
                            span: test_span(),
                        },
                    ],
                    is_matrix: false,
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
            InstanceData {
                instance_id: InstanceId::new(1),
                qualified_name: QualifiedName::from_dotted("world.cylinders"),
                dims: vec![2],
                is_primitive: false,
                ..Default::default()
            },
        );
        overlay.each_modifier_bindings.insert(
            rumoca_core::ComponentPath::from_component_reference(&component_ref),
        );

        propagate_unexpanded_record_array_dims(&mut flat, &overlay);

        let variable = flat.variables.get(&var_name).expect("missing field");
        assert_eq!(variable.dims, vec![2, 3]);
        assert!(matches!(
            variable.binding,
            Some(Expression::ArrayComprehension { ref indices, .. }) if indices.len() == 1
        ));
    }

    #[test]
    fn test_propagate_unexpanded_record_array_dims_repeats_record_alias_binding() {
        let mut flat = flat::Model::default();
        let source_name = rumoca_core::VarName::new("world.x_label.R.T");
        flat.add_variable(
            source_name.clone(),
            flat::Variable {
                name: source_name,
                dims: vec![3, 3],
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let target_name = rumoca_core::VarName::new("world.x_label.cylinders.R.T");
        let component_ref =
            rumoca_core::component_reference_from_flat_name(&target_name, test_span())
                .expect("test variable should have a structured component reference");
        flat.add_variable(
            target_name.clone(),
            flat::Variable {
                name: target_name.clone(),
                component_ref: Some(component_ref.clone()),
                dims: vec![3, 3],
                binding: Some(Expression::FieldAccess {
                    base: Box::new(Expression::VarRef {
                        name: rumoca_core::Reference::new("world.x_label.R"),
                        subscripts: Vec::new(),
                        span: test_span(),
                    }),
                    field: "T".to_string(),
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
            InstanceData {
                instance_id: InstanceId::new(1),
                qualified_name: QualifiedName::from_dotted("world.x_label.cylinders"),
                dims: vec![2],
                is_primitive: false,
                ..Default::default()
            },
        );
        overlay.each_modifier_bindings.insert(
            rumoca_core::ComponentPath::from_component_reference(&component_ref),
        );

        propagate_unexpanded_record_array_dims(&mut flat, &overlay);

        let variable = flat.variables.get(&target_name).expect("missing field");
        assert_eq!(variable.dims, vec![2, 3, 3]);
        assert!(matches!(
            variable.binding,
            Some(Expression::ArrayComprehension { ref indices, .. }) if indices.len() == 1
        ));
    }

    #[test]
    fn test_propagate_unexpanded_record_array_dims_does_not_repeat_non_each_modifier() {
        let mut flat = flat::Model::default();
        let target_name = rumoca_core::VarName::new("world.cylinders.R.T");
        let binding = Expression::VarRef {
            name: rumoca_core::Reference::new("world.R.T"),
            subscripts: Vec::new(),
            span: test_span(),
        };
        flat.add_variable(
            target_name.clone(),
            flat::Variable {
                name: target_name.clone(),
                component_ref: rumoca_core::component_reference_from_flat_name(
                    &target_name,
                    test_span(),
                ),
                dims: vec![3, 3],
                binding: Some(binding.clone()),
                binding_from_modification: true,
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

        let variable = flat.variables.get(&target_name).expect("missing field");
        assert_eq!(variable.dims, vec![2, 3, 3]);
        assert_eq!(variable.binding.as_ref(), Some(&binding));
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

    mod enum_and_overrides;
}
