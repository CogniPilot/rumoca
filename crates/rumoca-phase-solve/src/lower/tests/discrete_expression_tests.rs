use super::*;
use crate::lower::normalized_discrete_update_equations;

#[test]
fn lower_solve_problem_scalarizes_early_record_residual_after_state_vars() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("r.a"), scalar_var("r.a"));
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("r.b"), scalar_var("r.b"));

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            source_var("r"),
            rumoca_core::Expression::Array {
                elements: vec![real_lit(1.0), real_lit(2.0)],
                is_matrix: false,
                span: lower_test_span(),
            },
        ),
        span: lower_test_span(),
        origin: "record residual before derivative rows".to_string(),
        scalar_count: 2,
    });
    dae_model
        .continuous
        .equations
        .push(residual(sub(der(var("x")), real_lit(0.0))));

    let problem = lower_solve_problem(&dae_model)
        .expect("filtered residual rows must not infer state-derivative behavior from row index");
    assert_eq!(problem.continuous.residual.len(), Ok(2));
}

#[test]
fn lower_discrete_rhs_recovers_if_residual_assignment_value() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("z"), scalar_var("z"));
    dae_model
        .discrete
        .real_updates
        .push(residual(rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Boolean(true),
                    span: lower_test_span(),
                },
                // MLS §8.3.4: an if-equation branch may contain the same
                // assignment equation written in residual form.
                sub(
                    var("z"),
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(2.0),
                        span: lower_test_span(),
                    },
                ),
            )],
            else_branch: Box::new(sub(
                var("z"),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(3.0),
                    span: lower_test_span(),
                },
            )),
            span: lower_test_span(),
        }));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("conditional residual discrete update should lower");
    let (_, output) = eval_linear_ops(&rows[0], &[], &[0.0], 0.0);

    assert_eq!(output, Some(2.0));
}

#[test]
fn normalized_discrete_updates_orient_residual_alias_chain_once() {
    let mut dae_model = dae::Dae::default();
    for name in ["a", "b", "c", "d"] {
        dae_model
            .variables
            .discrete_reals
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    for (lhs, rhs) in [("a", "b"), ("b", "c"), ("c", "d")] {
        dae_model.discrete.real_updates.push(residual_with_origin(
            sub(var(lhs), var(rhs)),
            &format!("connection equation: {lhs} = {rhs}"),
        ));
    }

    let equations =
        normalized_discrete_update_equations(&dae_model).expect("normalization succeeds");
    let mut lhs_counts = IndexMap::<String, usize>::new();
    let mut aliases = IndexMap::<String, String>::new();
    for equation in &equations {
        let lhs = equation.lhs.as_ref().expect("alias equation has target");
        *lhs_counts.entry(lhs.as_str().to_string()).or_default() += 1;
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = &equation.rhs
        else {
            panic!("alias equation should preserve VarRef RHS");
        };
        assert!(subscripts.is_empty());
        aliases.insert(lhs.as_str().to_string(), name.as_str().to_string());
    }

    assert_eq!(lhs_counts.values().copied().max(), Some(1));
    assert_eq!(aliases.get("b").map(String::as_str), Some("a"));
    assert_eq!(aliases.get("c").map(String::as_str), Some("b"));
    assert_eq!(aliases.get("d").map(String::as_str), Some("c"));
}

#[test]
fn normalized_discrete_updates_preserve_explicit_difference_assignment() {
    let mut dae_model = dae::Dae::default();
    for name in ["a", "b", "x"] {
        dae_model
            .variables
            .discrete_reals
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("x").into()),
        rhs: sub(var("a"), var("b")),
        span: lower_test_span(),
        origin: "x = a - b".to_string(),
        scalar_count: 1,
    });

    let equations =
        normalized_discrete_update_equations(&dae_model).expect("normalization succeeds");

    assert_eq!(equations.len(), 1);
    assert_eq!(equations[0].lhs.as_ref().map(|lhs| lhs.as_str()), Some("x"));
    assert!(
        matches!(
            equations[0].rhs,
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                ..
            }
        ),
        "explicit difference assignments must not be treated as residual aliases"
    );
}

#[test]
fn lower_expression_prunes_unreachable_static_if_branch() {
    let expr = rumoca_core::Expression::If {
        branches: vec![(
            binary(rumoca_core::OpBinary::Eq, int_lit(1), int_lit(1)),
            real_lit(2.0),
        )],
        else_branch: Box::new(var("missing.binding")),
        span: lower_test_span(),
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect("statically unreachable else branch should not be lowered");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 2.0);
}

#[test]
fn lower_discrete_rhs_expands_vectorized_update_rows() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), source_array_var("y", &[2]));
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(2.0),
                    span,
                },
            ],
            is_matrix: false,
            span,
        },
        span,
        // MLS §8.3 and §10.6: array equations update one scalar slot per
        // element after solve-IR row lowering.
        origin: "vectorized discrete update".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("vectorized update should lower");
    let (_, first) = eval_linear_ops(&rows[0], &[], &[0.0, 0.0], 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &[], &[0.0, 0.0], 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(1.0));
    assert_eq!(second, Some(2.0));
}

#[test]
fn lower_discrete_rhs_uses_first_output_for_array_function_expression() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), source_array_var("y", &[2]));
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.memoryLike"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("My.memoryLike"),
            def_id: None,
            inputs: vec![],
            outputs: vec![
                function_param_with_dims("out", &[2]),
                function_param("line"),
                function_param("bit"),
            ],
            locals: vec![],
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref_index("out", 1),
                    value: real_lit(4.0),
                    span: lower_test_span(),
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref_index("out", 2),
                    value: real_lit(5.0),
                    span: lower_test_span(),
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref("line"),
                    value: real_lit(99.0),
                    span: lower_test_span(),
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref("bit"),
                    value: real_lit(100.0),
                    span: lower_test_span(),
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: lower_test_span(),
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.memoryLike").into(),
            args: vec![],
            is_constructor: false,
            span: lower_test_span(),
        },
        span: lower_test_span(),
        // MLS §12.4: an expression-form function call denotes the first
        // function output. Additional outputs are visible only via tuple
        // assignment/projection and must not widen array-valued expressions.
        origin: "array function first output".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("array-valued first function output should lower");
    let (_, first) = eval_linear_ops(&rows[0], &[], &[0.0, 0.0], 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &[], &[0.0, 0.0], 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(4.0));
    assert_eq!(second, Some(5.0));
}

#[test]
fn lower_discrete_rhs_projects_array_function_output_by_position() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), source_array_var("y", &[2]));
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.memoryLike"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("My.memoryLike"),
            def_id: None,
            inputs: vec![],
            outputs: vec![
                function_param_with_dims("out", &[2]),
                function_param("line"),
                function_param("bit"),
            ],
            locals: vec![],
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref_index("out", 1),
                    value: real_lit(6.0),
                    span: lower_test_span(),
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref_index("out", 2),
                    value: real_lit(7.0),
                    span: lower_test_span(),
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref("line"),
                    value: real_lit(98.0),
                    span: lower_test_span(),
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref("bit"),
                    value: real_lit(99.0),
                    span: lower_test_span(),
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: lower_test_span(),
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("My.memoryLike").into(),
                args: vec![],
                is_constructor: false,
                span: lower_test_span(),
            }),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                1,
                lower_test_span(),
            )],
            span: lower_test_span(),
        },
        span: lower_test_span(),
        // MLS §12.4: positional indexing on a multi-output function call is
        // output projection. `f()[1]` selects the first output, not the first
        // scalar element of that output.
        origin: "array function output projection".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("array-valued positional output projection should lower");
    let (_, first) = eval_linear_ops(&rows[0], &[], &[0.0, 0.0], 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &[], &[0.0, 0.0], 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(6.0));
    assert_eq!(second, Some(7.0));
}

#[test]
fn lower_discrete_rhs_recovers_dynamic_function_output_shape_from_assignments() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), source_array_var("y", &[2]));
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.dynamicMemoryLike"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("My.dynamicMemoryLike"),
            def_id: None,
            inputs: vec![],
            outputs: vec![
                function_param_with_dims("out", &[0, 0]),
                function_param("line"),
                function_param("bit"),
            ],
            locals: vec![],
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref_indices("out", &[1, 1]),
                    value: real_lit(8.0),
                    span: lower_test_span(),
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref_indices("out", &[1, 2]),
                    value: real_lit(9.0),
                    span: lower_test_span(),
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref("line"),
                    value: real_lit(98.0),
                    span: lower_test_span(),
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref("bit"),
                    value: real_lit(99.0),
                    span: lower_test_span(),
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: lower_test_span(),
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.dynamicMemoryLike").into(),
            args: vec![],
            is_constructor: false,
            span: lower_test_span(),
        },
        span: lower_test_span(),
        // Some MSL functions, including Digital RAM memory loading, have
        // output dimensions derived from input parameters. If those dimensions
        // are not static in FunctionParam, the indexed output assignments still
        // define the array-valued function result shape.
        origin: "dynamic array function output".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("indexed assignments should define dynamic output shape");
    let (_, first) = eval_linear_ops(&rows[0], &[], &[0.0, 0.0], 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &[], &[0.0, 0.0], 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(8.0));
    assert_eq!(second, Some(9.0));
}

#[test]
fn lower_expression_indexes_array_function_expression_result() {
    let mut dae_model = dae::Dae::default();
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.vectorResult"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("My.vectorResult"),
            def_id: None,
            inputs: vec![],
            outputs: vec![function_param_with_dims("out", &[2])],
            locals: vec![],
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref_index("out", 1),
                    value: real_lit(7.0),
                    span: lower_test_span(),
                },
                rumoca_core::Statement::Assignment {
                    comp: component_ref_index("out", 2),
                    value: real_lit(8.0),
                    span: lower_test_span(),
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: lower_test_span(),
        },
    );

    let span = lower_test_span();
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.vectorResult").into(),
            args: vec![],
            is_constructor: false,
            span,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_index(2, span)],
        span,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("indexed array-valued function expression should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 8.0);
}

#[test]
fn lower_expression_indexes_array_literal_with_dynamic_subscript() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("row"), scalar_var("row"));

    let span = lower_test_span();
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Array {
                    elements: vec![real_lit(10.0), real_lit(20.0)],
                    is_matrix: false,
                    span: lower_test_span(),
                },
                rumoca_core::Expression::Array {
                    elements: vec![real_lit(30.0), real_lit(40.0)],
                    is_matrix: false,
                    span: lower_test_span(),
                },
            ],
            is_matrix: true,
            span: lower_test_span(),
        }),
        subscripts: vec![
            rumoca_core::Subscript::generated_expr(Box::new(var("row")), lower_test_span()),
            rumoca_core::Subscript::generated_expr(Box::new(int_lit(1)), lower_test_span()),
        ],
        span,
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("dynamic array literal lookup should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[2.0], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 30.0);
}

#[test]
fn lower_expression_accepts_singleton_projection_of_scalarized_expression() {
    let span = lower_test_span();
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(real_lit(2.0)),
            rhs: Box::new(real_lit(3.0)),
            span,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_index(1, span)],
        span,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect("singleton projection of scalarized expression should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 5.0);
}

#[test]
fn lower_expression_accepts_singleton_projection_of_scalar_function_call() {
    let span = lower_test_span();
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("sin").into(),
            args: vec![real_lit(std::f64::consts::FRAC_PI_2)],
            is_constructor: false,
            span,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_index(1, span)],
        span,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect("singleton projection of scalar function call should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert!((read_reg(&regs, lowered.result) - 1.0).abs() < 1.0e-12);
}

#[test]
fn lower_residual_scalarizes_indexed_record_array_fields() {
    let span = lower_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("y.re"),
        source_array_var("y.re", &[1]),
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("y.im"),
        source_array_var("y.im", &[1]),
    );
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u.re"), scalar_var("u.re"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u.im"), scalar_var("u.im"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("scale"), scalar_var("scale"));
    dae_model
        .continuous
        .equations
        .push(dae::Equation::explicit_with_scalar_count(
            rumoca_core::VarName::new("y"),
            rumoca_core::Expression::Array {
                elements: vec![mul(source_var("scale"), source_var("u"))],
                is_matrix: false,
                span,
            },
            span,
            "indexed record array residual",
            2,
        ));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout)
        .expect("array-of-record residual fields should lower through field then index");
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_y_value(&layout, &mut y, "y.re[1]", 6.0);
    set_y_value(&layout, &mut y, "y.im[1]", 8.0);
    set_y_value(&layout, &mut y, "u.re", 3.0);
    set_y_value(&layout, &mut y, "u.im", 4.0);
    set_p_value(&layout, &mut p, "scale", 2.0);

    let (_, first) = eval_linear_ops(&rows[0], &y, &p, 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &y, &p, 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(0.0));
    assert_eq!(second, Some(0.0));
}

#[test]
fn lower_expression_indexes_if_array_value_with_dynamic_subscript() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("choose"), scalar_var("choose"));
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("row"), scalar_var("row"));

    let span = lower_test_span();
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::If {
            branches: vec![(
                var("choose"),
                rumoca_core::Expression::Array {
                    elements: vec![
                        rumoca_core::Expression::Array {
                            elements: vec![real_lit(10.0), real_lit(20.0)],
                            is_matrix: false,
                            span: lower_test_span(),
                        },
                        rumoca_core::Expression::Array {
                            elements: vec![real_lit(30.0), real_lit(40.0)],
                            is_matrix: false,
                            span: lower_test_span(),
                        },
                    ],
                    is_matrix: true,
                    span: lower_test_span(),
                },
            )],
            else_branch: Box::new(rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Array {
                        elements: vec![real_lit(50.0), real_lit(60.0)],
                        is_matrix: false,
                        span: lower_test_span(),
                    },
                    rumoca_core::Expression::Array {
                        elements: vec![real_lit(70.0), real_lit(80.0)],
                        is_matrix: false,
                        span: lower_test_span(),
                    },
                ],
                is_matrix: true,
                span: lower_test_span(),
            }),
            span: lower_test_span(),
        }),
        subscripts: vec![
            rumoca_core::Subscript::generated_expr(Box::new(var("row")), lower_test_span()),
            rumoca_core::Subscript::generated_expr(Box::new(int_lit(1)), lower_test_span()),
        ],
        span,
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("dynamic lookup into if-selected array value should lower");
    let (true_regs, _) = eval_linear_ops(&lowered.ops, &[1.0, 2.0], &[], 0.0);
    let (false_regs, _) = eval_linear_ops(&lowered.ops, &[0.0, 1.0], &[], 0.0);

    assert_eq!(read_reg(&true_regs, lowered.result), 30.0);
    assert_eq!(read_reg(&false_regs, lowered.result), 50.0);
}

#[test]
fn lower_discrete_rhs_indexes_if_array_value_with_dynamic_slice() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("choose"), scalar_var("choose"));
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("row"), scalar_var("row"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), source_array_var("y", &[2]));
    let generated_subscript = |expr| rumoca_core::Subscript::generated_expr(Box::new(expr), span);

    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::If {
                branches: vec![(
                    var("choose"),
                    rumoca_core::Expression::Array {
                        elements: vec![
                            rumoca_core::Expression::Array {
                                elements: vec![real_lit(10.0), real_lit(20.0)],
                                is_matrix: false,
                                span,
                            },
                            rumoca_core::Expression::Array {
                                elements: vec![real_lit(30.0), real_lit(40.0)],
                                is_matrix: false,
                                span,
                            },
                        ],
                        is_matrix: true,
                        span,
                    },
                )],
                else_branch: Box::new(rumoca_core::Expression::Array {
                    elements: vec![
                        rumoca_core::Expression::Array {
                            elements: vec![real_lit(50.0), real_lit(60.0)],
                            is_matrix: false,
                            span,
                        },
                        rumoca_core::Expression::Array {
                            elements: vec![real_lit(70.0), real_lit(80.0)],
                            is_matrix: false,
                            span,
                        },
                    ],
                    is_matrix: true,
                    span,
                }),
                span,
            }),
            subscripts: vec![
                generated_subscript(var("row")),
                generated_subscript(rumoca_core::Expression::Range {
                    start: Box::new(int_lit(1)),
                    step: None,
                    end: Box::new(int_lit(2)),
                    span,
                }),
            ],
            span,
        },
        span: lower_test_span(),
        origin: "dynamic if-selected array row slice".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("dynamic slice from if-selected array value should lower");
    let (_, true_first) = eval_linear_ops(&rows[0], &[1.0, 2.0], &[], 0.0);
    let (_, true_second) = eval_linear_ops(&rows[1], &[1.0, 2.0], &[], 0.0);
    let (_, false_first) = eval_linear_ops(&rows[0], &[0.0, 1.0], &[], 0.0);
    let (_, false_second) = eval_linear_ops(&rows[1], &[0.0, 1.0], &[], 0.0);

    assert_eq!(true_first, Some(30.0));
    assert_eq!(true_second, Some(40.0));
    assert_eq!(false_first, Some(50.0));
    assert_eq!(false_second, Some(60.0));

    let nested_index = rumoca_core::Expression::Index {
        base: Box::new(dae_model.discrete.real_updates[0].rhs.clone()),
        subscripts: vec![generated_subscript(int_lit(2))],
        span,
    };
    let lowered = lower_expression(&nested_index, &layout, &dae_model.symbols.functions)
        .expect("outer index over dynamic if-selected slice should lower");
    let (true_regs, _) = eval_linear_ops(&lowered.ops, &[1.0, 2.0], &[], 0.0);
    let (false_regs, _) = eval_linear_ops(&lowered.ops, &[0.0, 1.0], &[], 0.0);
    assert_eq!(read_reg(&true_regs, lowered.result), 40.0);
    assert_eq!(read_reg(&false_regs, lowered.result), 60.0);
}

#[test]
fn lower_discrete_rhs_expands_fill_branch_in_array_if_expression() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), source_array_var("y", &[2]));
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Boolean(true),
                    span: lower_test_span(),
                },
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Fill,
                    args: vec![
                        var("u"),
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Integer(2),
                            span: lower_test_span(),
                        },
                    ],
                    span: lower_test_span(),
                },
            )],
            else_branch: Box::new(rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(1.0),
                        span: lower_test_span(),
                    },
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(2.0),
                        span: lower_test_span(),
                    },
                ],
                is_matrix: false,
                span: lower_test_span(),
            }),
            span: lower_test_span(),
        },
        span,
        // MLS §10.6.2: fill(s, n) constructs an n-element array, so it can
        // participate in array-valued conditional expressions of that shape.
        origin: "fill branch array if".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("fill branch should lower");
    let y = vec![3.5, 0.0, 0.0];
    let (_, first) = eval_linear_ops(&rows[0], &y, &[], 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &y, &[], 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(3.5));
    assert_eq!(second, Some(3.5));
}

#[test]
fn lower_discrete_rhs_expands_previous_range_slice_in_array_if_expression() {
    let dae_model = previous_range_slice_array_if_model();
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("range slice branch should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u", 4.0);
    set_p_value(&layout, &mut p, "u_buffer[1]", 5.0);
    set_p_value(&layout, &mut p, "u_buffer[2]", 6.0);

    let (_, first) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &[], &p, 0.0);
    let (_, third) = eval_linear_ops(&rows[2], &[], &p, 0.0);

    assert_eq!(rows.len(), 3);
    assert_eq!(first, Some(4.0));
    assert_eq!(second, Some(5.0));
    assert_eq!(third, Some(6.0));
}

fn previous_range_slice_array_if_model() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    insert_previous_range_slice_variables(&mut dae_model);
    let previous_slice = previous_range_slice_expr();
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: previous_range_slice_branch_expr(previous_slice),
        span: lower_test_span(),
        // MLS §10.5 and §16.4: array subscripts may select ranges, and
        // previous(v[1:n]) preserves the selected array shape element-wise.
        origin: "previous range slice branch".to_string(),
        scalar_count: 3,
    });

    dae_model
}

fn insert_previous_range_slice_variables(dae_model: &mut dae::Dae) {
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("n"),
        dae::Variable {
            name: rumoca_core::VarName::new("n"),
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: lower_test_span(),
            }),
            is_tunable: false,
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("u_buffer"),
        source_array_var("u_buffer", &[3]),
    );
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), source_array_var("y", &[3]));
}

fn previous_range_slice_expr() -> rumoca_core::Expression {
    let span = lower_test_span();
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("previous").into(),
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("u_buffer").into(),
            subscripts: vec![rumoca_core::Subscript::generated_expr(
                Box::new(rumoca_core::Expression::Range {
                    start: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span,
                    }),
                    step: None,
                    end: Box::new(var("n")),
                    span,
                }),
                span,
            )],
            span,
        }],
        is_constructor: false,
        span,
    }
}

fn previous_range_slice_branch_expr(
    previous_slice: rumoca_core::Expression,
) -> rumoca_core::Expression {
    let span = lower_test_span();
    rumoca_core::Expression::If {
        branches: vec![(
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                span,
            },
            rumoca_core::Expression::Array {
                elements: vec![real_lit(9.0), real_lit(9.0), real_lit(9.0)],
                is_matrix: false,
                span,
            },
        )],
        else_branch: Box::new(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Cat,
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span,
                },
                rumoca_core::Expression::Array {
                    elements: vec![var("u")],
                    is_matrix: false,
                    span,
                },
                previous_slice,
            ],
            span,
        }),
        span,
    }
}

#[test]
fn lower_discrete_rhs_preserves_pre_array_branch_values() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), source_array_var("y", &[2]));
    insert_pre_parameter(&mut dae_model, "y", &[2]);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Boolean(false),
                    span: lower_test_span(),
                },
                rumoca_core::Expression::Array {
                    elements: vec![
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: lower_test_span(),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(2.0),
                            span: lower_test_span(),
                        },
                    ],
                    is_matrix: false,
                    span: lower_test_span(),
                },
            )],
            else_branch: Box::new(pre_var("y")),
            span: lower_test_span(),
        },
        span,
        // DAE lowering rewrites pre(y) to the __pre__.y parameter array before
        // Solve-IR lowering; the branch must preserve scalar element values.
        origin: "vectorized pre branch".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("pre(array) branch should lower");
    let (_, first) = eval_linear_ops(&rows[0], &[], &[5.0, 6.0], 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &[], &[5.0, 6.0], 0.0);

    assert_eq!(rows.len(), 2);
    assert_eq!(first, Some(5.0));
    assert_eq!(second, Some(6.0));
}
