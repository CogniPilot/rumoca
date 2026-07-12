use super::*;

#[test]
fn lower_expression_unrolls_function_for_loop_over_input_size() {
    let mut dae_model = dae::Dae::default();
    let mut sum = rumoca_core::Function {
        name: rumoca_core::VarName::new("My.sum"),
        def_id: None,
        instance_id: None,
        inputs: vec![function_param_with_dims("u", &[0])],
        outputs: vec![function_param("out")],
        locals: vec![],
        body: vec![
            rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: lower_test_span(),
                },

                span: lower_test_span(),
            },
            rumoca_core::Statement::For {
                indices: vec![rumoca_core::ForIndex {
                    ident: "i".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span: lower_test_span(),
                        }),
                        step: None,
                        end: Box::new(rumoca_core::Expression::BuiltinCall {
                            function: rumoca_core::BuiltinFunction::Size,
                            args: vec![
                                var("u"),
                                rumoca_core::Expression::Literal {
                                    value: rumoca_core::Literal::Integer(1),
                                    span: lower_test_span(),
                                },
                            ],
                            span: lower_test_span(),
                        }),
                        span: lower_test_span(),
                    },
                }],
                equations: vec![rumoca_core::Statement::Assignment {
                    comp: component_ref("out"),
                    value: add(
                        var("out"),
                        rumoca_core::Expression::Index {
                            base: Box::new(var("u")),
                            subscripts: vec![rumoca_core::Subscript::generated_expr(
                                Box::new(var("i")),
                                lower_test_span(),
                            )],
                            span: lower_test_span(),
                        },
                    ),

                    span: lower_test_span(),
                }],

                span: lower_test_span(),
            },
        ],
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: lower_test_span(),
    };
    sum.inputs[0].type_name = "Real".to_string();
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.sum"), sum);

    let span = lower_test_span();
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.sum").into(),
        args: vec![rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(2.0),
                    span,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(3.0),
                    span,
                },
            ],
            is_matrix: false,
            span,
        }],
        is_constructor: false,
        span,
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("function input size should be available for loop unrolling");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 6.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_unrolls_function_for_loop_over_local_input_size() {
    let mut dae_model = dae::Dae::default();
    let mut sum = test_function("My.sumViaLocalSize", lower_test_span());
    sum.inputs.push(function_param_with_dims("u", &[0]));
    sum.outputs.push(function_param("out"));
    sum.locals.push(
        rumoca_core::FunctionParam::new("n", "Integer", lower_test_span())
            .with_default(size_expr(var("u"), 1)),
    );
    sum.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: real_lit(0.0),
        span: lower_test_span(),
    });
    sum.body.push(rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            ident: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int_lit(1)),
                step: None,
                end: Box::new(var("n")),
                span: lower_test_span(),
            },
        }],
        equations: vec![rumoca_core::Statement::Assignment {
            comp: component_ref("out"),
            value: add(var("out"), var_index_expr("u", var("i"))),
            span: lower_test_span(),
        }],
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.sumViaLocalSize"), sum);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.sumViaLocalSize").into(),
        args: vec![rumoca_core::Expression::Array {
            elements: vec![real_lit(1.0), real_lit(2.0), real_lit(3.0)],
            is_matrix: false,
            span: lower_test_span(),
        }],
        is_constructor: false,
        span: lower_test_span(),
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("function local size should be available for loop unrolling");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 6.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_unrolls_for_loop_over_assigned_local_integer() {
    let mut dae_model = dae::Dae::default();
    let mut function = test_function("My.assignedLoopBound", lower_test_span());
    function.outputs.push(function_param("out"));
    function.locals.push(rumoca_core::FunctionParam::new(
        "solveRow",
        "Integer",
        lower_test_span(),
    ));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("solveRow"),
        value: int_lit(2),
        span: lower_test_span(),
    });
    function.body.push(rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            ident: "k".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(var("solveRow")),
                step: None,
                end: Box::new(var("solveRow")),
                span: lower_test_span(),
            },
        }],
        equations: vec![rumoca_core::Statement::Assignment {
            comp: component_ref("out"),
            value: var("k"),
            span: lower_test_span(),
        }],
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("My.assignedLoopBound"),
        args: Vec::new(),
        is_constructor: false,
        span: lower_test_span(),
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("assigned local integer should be available to the following loop range");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 2.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_uses_assigned_local_integer_in_comprehension_range() {
    let span = lower_test_span();
    let mut dae_model = dae::Dae::default();
    let mut function = test_function("My.assignedComprehensionBound", span);
    function.outputs.push(function_param("out"));
    function
        .locals
        .push(rumoca_core::FunctionParam::new("pivotRow", "Integer", span));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("pivotRow"),
        value: int_lit(2),
        span,
    });
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sum,
            args: vec![rumoca_core::Expression::ArrayComprehension {
                expr: Box::new(var("column")),
                indices: vec![rumoca_core::ComprehensionIndex {
                    name: "column".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(add(var("pivotRow"), int_lit(1))),
                        step: None,
                        end: Box::new(int_lit(3)),
                        span,
                    },
                }],
                filter: None,
                span,
            }],
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("My.assignedComprehensionBound"),
        args: Vec::new(),
        is_constructor: false,
        span,
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("assigned local Integer should be available to a comprehension range");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_reads_array_value_assigned_in_for_loop() {
    let mut dae_model = dae::Dae::default();
    let mut function = test_function("My.arrayLoopAssignment", lower_test_span());
    function.outputs.push(function_param("out"));
    function
        .locals
        .push(function_param_with_dims("values", &[1]).with_default(
            rumoca_core::Expression::Array {
                elements: vec![real_lit(0.0)],
                is_matrix: false,
                span: lower_test_span(),
            },
        ));
    function.body.push(rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            ident: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int_lit(1)),
                step: None,
                end: Box::new(int_lit(1)),
                span: lower_test_span(),
            },
        }],
        equations: vec![rumoca_core::Statement::Assignment {
            comp: component_ref_index_expr("values", var("i")),
            value: real_lit(2.0),
            span: lower_test_span(),
        }],
        span: lower_test_span(),
    });
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: var_index("values", 1),
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("My.arrayLoopAssignment"),
        args: Vec::new(),
        is_constructor: false,
        span: lower_test_span(),
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("local array assignment in a function loop should lower");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 2.0).abs() <= 1e-12);
}

#[test]
fn runtime_if_inside_for_invalidates_pre_loop_constant_index() {
    let span = lower_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));

    let mut function = test_function("My.loopSelectedIndex", span);
    function.inputs.push(function_param("u"));
    function.outputs.push(function_param("out"));
    function.locals.push(
        rumoca_core::FunctionParam::new("selected", "Integer", span).with_default(int_lit(0)),
    );
    function
        .locals
        .push(function_param_with_dims("values", &[2]).with_default(
            rumoca_core::Expression::Array {
                elements: vec![real_lit(5.0), real_lit(7.0)],
                is_matrix: false,
                span,
            },
        ));
    function.body.push(rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            ident: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int_lit(1)),
                step: None,
                end: Box::new(int_lit(2)),
                span,
            },
        }],
        equations: vec![rumoca_core::Statement::If {
            cond_blocks: vec![rumoca_core::StatementBlock {
                cond: rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Gt,
                    lhs: Box::new(source_var("u")),
                    rhs: Box::new(source_var("i")),
                    span,
                },
                stmts: vec![rumoca_core::Statement::Assignment {
                    comp: component_ref("selected"),
                    value: source_var("i"),
                    span,
                }],
            }],
            else_block: None,
            span,
        }],
        span,
    });
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: var_index_expr("values", source_var("selected")),
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);

    let expression = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::generated("My.loopSelectedIndex"),
        args: vec![source_var("u")],
        is_constructor: false,
        span,
    };
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expression, &layout, &dae_model.symbols.functions)
        .expect("runtime loop branch should invalidate a pre-loop constant index");

    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "u", 3.0);
    let (regs, _) = eval_linear_ops(&lowered.ops, &y, &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 7.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_uses_function_local_size_in_fill_dimension() {
    let mut dae_model = dae::Dae::default();
    let mut count_fill = test_function("My.countFill", lower_test_span());
    count_fill.inputs.push(function_param_with_dims("u", &[0]));
    count_fill.outputs.push(function_param("out"));
    count_fill.locals.push(
        rumoca_core::FunctionParam::new("n", "Integer", lower_test_span())
            .with_default(size_expr(var("u"), 1)),
    );
    count_fill.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("sum").into(),
            args: vec![rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Fill,
                args: vec![real_lit(1.0), var("n")],
                span: lower_test_span(),
            }],
            is_constructor: false,
            span: lower_test_span(),
        },
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.countFill"), count_fill);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.countFill").into(),
        args: vec![rumoca_core::Expression::Array {
            elements: vec![real_lit(1.0), real_lit(2.0), real_lit(3.0)],
            is_matrix: false,
            span: lower_test_span(),
        }],
        is_constructor: false,
        span: lower_test_span(),
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("function local size should be available for fill dimension");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_unrolls_for_loop_over_qualified_real_fft_sample_points() {
    let mut dae_model = dae::Dae::default();
    let mut count = test_function("My.fftPointCount", lower_test_span());
    count.outputs.push(function_param("out"));
    count.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: real_lit(0.0),
        span: lower_test_span(),
    });
    count.body.push(rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            ident: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int_lit(1)),
                step: None,
                end: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new(
                        "Modelica.Math.FastFourierTransform.realFFTsamplePoints",
                    )
                    .into(),
                    args: vec![real_lit(4.0), real_lit(1.0)],
                    is_constructor: false,
                    span: lower_test_span(),
                }),
                span: lower_test_span(),
            },
        }],
        equations: vec![rumoca_core::Statement::Assignment {
            comp: component_ref("out"),
            value: add(var("out"), real_lit(1.0)),
            span: lower_test_span(),
        }],
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.fftPointCount"), count);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.fftPointCount").into(),
        args: vec![],
        is_constructor: false,
        span: lower_test_span(),
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("qualified realFFTsamplePoints should be a structural function");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 40.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_uses_actual_matrix_shape_for_function_input_size() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("sourceTable"),
        dae::Variable {
            fixed: Some(true),
            ..source_array_var("sourceTable", &[7, 2])
        },
    );

    let span = lower_test_span();
    let mut row_count = test_function("My.rowCount", span);
    row_count.inputs.push(
        rumoca_core::FunctionParam::new("table", "Real", span)
            .with_dims(vec![0, 2])
            .with_shape_expr(vec![
                rumoca_core::Subscript::generated_colon(span),
                rumoca_core::Subscript::generated_index(2, span),
            ]),
    );
    row_count.outputs.push(function_param("rows"));
    row_count.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("rows"),
        value: size_expr(var("table"), 1),
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.rowCount"), row_count);

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.rowCount").into(),
        args: vec![source_var("sourceTable")],
        is_constructor: false,
        span,
    };
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("size() should use the actual matrix shape bound to the function input");

    let p = vec![0.0; layout.p_scalars()];
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &p, 0.0);
    assert!((read_reg(&regs, lowered.result) - 7.0).abs() <= 1e-12);
}
