use super::*;

#[test]
fn lower_expression_unrolls_function_for_loop_over_input_size() {
    let mut dae_model = dae::Dae::default();
    let mut sum = rumoca_core::Function {
        name: rumoca_core::VarName::new("My.sum"),
        def_id: None,
        inputs: vec![function_param_with_dims("u", &[0])],
        outputs: vec![function_param("out")],
        locals: vec![],
        body: vec![
            rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: rumoca_core::Span::DUMMY,
                },

                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Statement::For {
                indices: vec![rumoca_core::ForIndex {
                    ident: "i".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span: rumoca_core::Span::DUMMY,
                        }),
                        step: None,
                        end: Box::new(rumoca_core::Expression::BuiltinCall {
                            function: rumoca_core::BuiltinFunction::Size,
                            args: vec![
                                var("u"),
                                rumoca_core::Expression::Literal {
                                    value: rumoca_core::Literal::Integer(1),
                                    span: rumoca_core::Span::DUMMY,
                                },
                            ],
                            span: rumoca_core::Span::DUMMY,
                        }),
                        span: rumoca_core::Span::DUMMY,
                    },
                }],
                equations: vec![rumoca_core::Statement::Assignment {
                    comp: component_ref("out"),
                    value: add(
                        var("out"),
                        rumoca_core::Expression::Index {
                            base: Box::new(var("u")),
                            subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
                                var("i"),
                            ))],
                            span: rumoca_core::Span::DUMMY,
                        },
                    ),

                    span: rumoca_core::Span::DUMMY,
                }],

                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: Default::default(),
    };
    sum.inputs[0].type_name = "Real".to_string();
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.sum"), sum);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.sum").into(),
        args: vec![rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(2.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(3.0),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("function input size should be available for loop unrolling");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 6.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_unrolls_function_for_loop_over_local_input_size() {
    let mut dae_model = dae::Dae::default();
    let mut sum = rumoca_core::Function::new("My.sumViaLocalSize", Default::default());
    sum.inputs.push(function_param_with_dims("u", &[0]));
    sum.outputs.push(function_param("out"));
    sum.locals
        .push(rumoca_core::FunctionParam::new("n", "Integer").with_default(size_expr(var("u"), 1)));
    sum.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: real_lit(0.0),
        span: rumoca_core::Span::DUMMY,
    });
    sum.body.push(rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            ident: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int_lit(1)),
                step: None,
                end: Box::new(var("n")),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        equations: vec![rumoca_core::Statement::Assignment {
            comp: component_ref("out"),
            value: add(var("out"), var_index_expr("u", var("i"))),
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
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
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("function local size should be available for loop unrolling");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 6.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_uses_function_local_size_in_fill_dimension() {
    let mut dae_model = dae::Dae::default();
    let mut count_fill = rumoca_core::Function::new("My.countFill", Default::default());
    count_fill.inputs.push(function_param_with_dims("u", &[0]));
    count_fill.outputs.push(function_param("out"));
    count_fill
        .locals
        .push(rumoca_core::FunctionParam::new("n", "Integer").with_default(size_expr(var("u"), 1)));
    count_fill.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("sum").into(),
            args: vec![rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Fill,
                args: vec![real_lit(1.0), var("n")],
                span: rumoca_core::Span::DUMMY,
            }],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
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
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("function local size should be available for fill dimension");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() <= 1e-12);
}
