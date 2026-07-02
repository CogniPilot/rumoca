use super::*;

pub(super) fn array_arg(values: [f64; 3]) -> rumoca_core::Expression {
    let span = lower_test_span();
    rumoca_core::Expression::Array {
        elements: values.into_iter().map(real_lit).collect(),
        is_matrix: false,
        span,
    }
}

pub(super) fn matrix_arg(values: [[f64; 3]; 3]) -> rumoca_core::Expression {
    let span = lower_test_span();
    rumoca_core::Expression::Array {
        elements: values.into_iter().map(array_arg).collect(),
        is_matrix: true,
        span,
    }
}

fn real_lit(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: lower_test_span(),
    }
}

pub(super) fn size_call(base: rumoca_core::Expression, dim: i64) -> rumoca_core::Expression {
    let span = lower_test_span();
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![
            base,
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(dim),
                span,
            },
        ],
        span,
    }
}

#[test]
fn lower_expression_handles_projected_complex_function_output_field_from_if_constructor() {
    let mut dae_model = dae::Dae::default();
    let power_of_j = build_power_of_j_function(
        vec![
            (
                eq_local("m", 0.0),
                complex_call(
                    vec![
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: lower_test_span(),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(0.0),
                            span: lower_test_span(),
                        },
                    ],
                    true,
                ),
            ),
            (
                eq_local("m", 1.0),
                complex_call(
                    vec![
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(0.0),
                            span: lower_test_span(),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: lower_test_span(),
                        },
                    ],
                    true,
                ),
            ),
        ],
        complex_call(
            vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: lower_test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(-1.0),
                    span: lower_test_span(),
                },
            ],
            true,
        ),
    );
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.powerOfJ"), power_of_j);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.powerOfJ.x.im",
        )),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span: lower_test_span(),
        }],
        is_constructor: false,
        span: lower_test_span(),
    };

    let layout = VarLayout::default();
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("projected complex output field from if constructor should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 1.0).abs() < 1e-12);
}
#[test]
fn lower_expression_handles_projected_complex_output_from_unflagged_constructor_calls() {
    let mut dae_model = dae::Dae::default();
    insert_complex_constructor(&mut dae_model, None);

    let power_of_j = build_power_of_j_function(
        vec![
            (
                eq_local("m", 0.0),
                complex_call(
                    vec![
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: lower_test_span(),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(0.0),
                            span: lower_test_span(),
                        },
                    ],
                    false,
                ),
            ),
            (
                eq_local("m", 1.0),
                complex_call(
                    vec![
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(0.0),
                            span: lower_test_span(),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: lower_test_span(),
                        },
                    ],
                    false,
                ),
            ),
        ],
        complex_call(
            vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: lower_test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(-1.0),
                    span: lower_test_span(),
                },
            ],
            false,
        ),
    );
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.powerOfJ"), power_of_j);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.powerOfJ.x.im",
        )),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span: lower_test_span(),
        }],
        is_constructor: false,
        span: lower_test_span(),
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("projected complex output field from unflagged constructor should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 1.0).abs() < 1e-12);
}
#[test]
fn lower_expression_handles_projected_complex_output_with_constructor_defaults_and_negation() {
    let mut dae_model = dae::Dae::default();
    insert_complex_constructor(
        &mut dae_model,
        Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span: lower_test_span(),
        }),
    );

    let power_of_j = build_power_of_j_function(
        vec![
            (
                eq_local("m", 0.0),
                complex_call(
                    vec![rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: lower_test_span(),
                    }],
                    true,
                ),
            ),
            (
                eq_local("m", 1.0),
                complex_call(
                    vec![
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Integer(0),
                            span: lower_test_span(),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span: lower_test_span(),
                        },
                    ],
                    false,
                ),
            ),
            (
                eq_local("m", 2.0),
                complex_call(
                    vec![rumoca_core::Expression::Unary {
                        op: rumoca_core::OpUnary::Minus,
                        rhs: Box::new(rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span: lower_test_span(),
                        }),
                        span: lower_test_span(),
                    }],
                    true,
                ),
            ),
        ],
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: Box::new(complex_call(
                vec![
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(0),
                        span: lower_test_span(),
                    },
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: lower_test_span(),
                    },
                ],
                false,
            )),
            span: lower_test_span(),
        },
    );
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.powerOfJ"), power_of_j);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.powerOfJ.x.im",
        )),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(3),
            span: lower_test_span(),
        }],
        is_constructor: false,
        span: lower_test_span(),
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("projected complex output with constructor defaults and negation should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) + 1.0).abs() < 1e-12);
}
#[test]
fn lower_residual_applies_state_then_algebraic_sign() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("z"), scalar_var("z"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "x",
            )),
            subscripts: vec![],
            span: lower_test_span(),
        },
        lower_test_span(),
        "state row",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "z",
            )),
            subscripts: vec![],
            span: lower_test_span(),
        },
        lower_test_span(),
        "algebraic row",
    ));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout).expect("lowering residual should succeed");
    assert_eq!(rows.len(), 2);

    let y = vec![2.0, 3.0];
    let p = vec![];

    let (_regs0, out0) = eval_linear_ops(&rows[0], &y, &p, 0.0);
    let (_regs1, out1) = eval_linear_ops(&rows[1], &y, &p, 0.0);
    assert_eq!(out0.expect("state row output"), -2.0);
    assert_eq!(out1.expect("algebraic row output"), 3.0);
}
#[test]
fn lower_expression_inlines_user_function_if_statement() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));

    let abs_like = rumoca_core::Function {
        name: rumoca_core::VarName::new("My.absLike"),
        def_id: None,
        inputs: vec![function_param("u")],
        outputs: vec![function_param("out")],
        locals: vec![],
        body: vec![rumoca_core::Statement::If {
            cond_blocks: vec![rumoca_core::StatementBlock {
                cond: rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Ge,
                    lhs: Box::new(rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::from_component_reference(
                            test_component_ref_from_name("u"),
                        ),
                        subscripts: vec![],
                        span: lower_test_span(),
                    }),
                    rhs: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(0.0),
                        span: lower_test_span(),
                    }),
                    span: lower_test_span(),
                },
                stmts: vec![rumoca_core::Statement::Assignment {
                    comp: component_ref("out"),
                    value: rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::from_component_reference(
                            test_component_ref_from_name("u"),
                        ),
                        subscripts: vec![],
                        span: lower_test_span(),
                    },

                    span: lower_test_span(),
                }],
            }],
            else_block: Some(vec![rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Minus,
                    rhs: Box::new(rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::from_component_reference(
                            test_component_ref_from_name("u"),
                        ),
                        subscripts: vec![],
                        span: lower_test_span(),
                    }),
                    span: lower_test_span(),
                },

                span: lower_test_span(),
            }]),

            span: lower_test_span(),
        }],
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: lower_test_span(),
    };
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.absLike"), abs_like);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.absLike",
        )),
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
                "x",
            )),
            subscripts: vec![],
            span: lower_test_span(),
        }],
        is_constructor: false,
        span: lower_test_span(),
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("if-statement function should lower");

    let (regs_pos, _) = eval_linear_ops(&lowered.ops, &[2.5], &[], 0.0);
    let (regs_neg, _) = eval_linear_ops(&lowered.ops, &[-3.0], &[], 0.0);
    assert!((read_reg(&regs_pos, lowered.result) - 2.5).abs() <= 1e-12);
    assert!((read_reg(&regs_neg, lowered.result) - 3.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_inlines_user_function_break_statement() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("a"), scalar_var("a"));
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("b"), scalar_var("b"));

    let break_like = rumoca_core::Function {
        name: rumoca_core::VarName::new("My.breakLike"),
        def_id: None,
        inputs: vec![function_param_with_dims("bits", &[2])],
        outputs: vec![function_param("out")],
        locals: vec![],
        body: vec![
            rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: real_lit(1.0),
                span: lower_test_span(),
            },
            rumoca_core::Statement::For {
                indices: vec![rumoca_core::ForIndex {
                    ident: "i".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(int_lit(1)),
                        step: None,
                        end: Box::new(int_lit(2)),
                        span: lower_test_span(),
                    },
                }],
                equations: vec![
                    rumoca_core::Statement::If {
                        cond_blocks: vec![rumoca_core::StatementBlock {
                            cond: rumoca_core::Expression::Binary {
                                op: rumoca_core::OpBinary::Eq,
                                lhs: Box::new(var_index_expr("bits", var("i"))),
                                rhs: Box::new(real_lit(2.0)),
                                span: lower_test_span(),
                            },
                            stmts: vec![
                                rumoca_core::Statement::Assignment {
                                    comp: component_ref("out"),
                                    value: real_lit(0.0),
                                    span: lower_test_span(),
                                },
                                rumoca_core::Statement::Break {
                                    span: lower_test_span(),
                                },
                            ],
                        }],
                        else_block: None,
                        span: lower_test_span(),
                    },
                    rumoca_core::Statement::Assignment {
                        comp: component_ref("out"),
                        value: rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Add,
                            lhs: Box::new(var("out")),
                            rhs: Box::new(real_lit(10.0)),
                            span: lower_test_span(),
                        },
                        span: lower_test_span(),
                    },
                ],
                span: lower_test_span(),
            },
        ],
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: lower_test_span(),
    };
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.breakLike"), break_like);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.breakLike",
        )),
        args: vec![rumoca_core::Expression::Array {
            elements: vec![var("a"), var("b")],
            is_matrix: false,
            span: lower_test_span(),
        }],
        is_constructor: false,
        span: lower_test_span(),
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("break-statement function should lower");

    let (regs_break, _) = eval_linear_ops(&lowered.ops, &[2.0, 1.0], &[], 0.0);
    let (regs_no_break, _) = eval_linear_ops(&lowered.ops, &[1.0, 1.0], &[], 0.0);
    assert_eq!(read_reg(&regs_break, lowered.result), 0.0);
    assert!(read_reg(&regs_no_break, lowered.result) > 0.0);
}

#[test]
fn lower_expression_rejects_guarded_assignment_without_prior_binding() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));

    let mut maybe_return = rumoca_core::Function::new("My.maybeReturn", lower_test_span());
    maybe_return.inputs.push(function_param("u"));
    maybe_return.outputs.push(function_param("out"));
    maybe_return.body = vec![
        rumoca_core::Statement::If {
            cond_blocks: vec![rumoca_core::StatementBlock {
                cond: rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Gt,
                    lhs: Box::new(var("u")),
                    rhs: Box::new(real_lit(0.0)),
                    span: lower_test_span(),
                },
                stmts: vec![rumoca_core::Statement::Return {
                    span: lower_test_span(),
                }],
            }],
            else_block: None,
            span: lower_test_span(),
        },
        rumoca_core::Statement::Assignment {
            comp: component_ref("out"),
            value: real_lit(1.0),
            span: lower_test_span(),
        },
    ];
    dae_model
        .symbols
        .functions
        .insert(maybe_return.name.clone(), maybe_return);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.maybeReturn",
        )),
        args: vec![var("u")],
        is_constructor: false,
        span: lower_test_span(),
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let err = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect_err("uninitialized function output must not be synthesized as zero");

    assert!(
        err.reason().contains("requires an existing binding"),
        "unexpected error: {}",
        err.reason()
    );
}

#[test]
fn lower_expression_allows_dead_local_assignment_after_return() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));

    let mut maybe_return = rumoca_core::Function::new("My.localAfterReturn", lower_test_span());
    maybe_return.inputs.push(function_param("u"));
    maybe_return.outputs.push(function_param("out"));
    maybe_return.locals.push(function_param("tmp"));
    maybe_return.body = vec![
        rumoca_core::Statement::If {
            cond_blocks: vec![rumoca_core::StatementBlock {
                cond: rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Gt,
                    lhs: Box::new(var("u")),
                    rhs: Box::new(real_lit(0.0)),
                    span: lower_test_span(),
                },
                stmts: vec![
                    rumoca_core::Statement::Assignment {
                        comp: component_ref("out"),
                        value: real_lit(2.0),
                        span: lower_test_span(),
                    },
                    rumoca_core::Statement::Return {
                        span: lower_test_span(),
                    },
                ],
            }],
            else_block: None,
            span: lower_test_span(),
        },
        rumoca_core::Statement::Assignment {
            comp: component_ref("tmp"),
            value: real_lit(1.0),
            span: lower_test_span(),
        },
        rumoca_core::Statement::Assignment {
            comp: component_ref("out"),
            value: var("tmp"),
            span: lower_test_span(),
        },
    ];
    dae_model
        .symbols
        .functions
        .insert(maybe_return.name.clone(), maybe_return);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.localAfterReturn",
        )),
        args: vec![var("u")],
        is_constructor: false,
        span: lower_test_span(),
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("dead local after return should lower");

    let (positive_regs, _) = eval_linear_ops(&lowered.ops, &[1.0], &[], 0.0);
    let (negative_regs, _) = eval_linear_ops(&lowered.ops, &[-1.0], &[], 0.0);

    assert_eq!(read_reg(&positive_regs, lowered.result), 2.0);
    assert_eq!(read_reg(&negative_regs, lowered.result), 1.0);
}

#[test]
fn lower_expression_inlines_user_function_for_statement() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));

    let repeat_accum = rumoca_core::Function {
        name: rumoca_core::VarName::new("My.repeatAccum"),
        def_id: None,
        inputs: vec![function_param("u")],
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
                        end: Box::new(rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Integer(3),
                            span: lower_test_span(),
                        }),
                        span: lower_test_span(),
                    },
                }],
                equations: vec![rumoca_core::Statement::Assignment {
                    comp: component_ref("out"),
                    value: rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Add,
                        lhs: Box::new(rumoca_core::Expression::VarRef {
                            name: rumoca_core::Reference::from_component_reference(
                                test_component_ref_from_name("out"),
                            ),
                            subscripts: vec![],
                            span: lower_test_span(),
                        }),
                        rhs: Box::new(rumoca_core::Expression::VarRef {
                            name: rumoca_core::Reference::from_component_reference(
                                test_component_ref_from_name("u"),
                            ),
                            subscripts: vec![],
                            span: lower_test_span(),
                        }),
                        span: lower_test_span(),
                    },

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
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.repeatAccum"), repeat_accum);

    let span = lower_test_span();
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.repeatAccum",
        )),
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
                "x",
            )),
            subscripts: vec![],
            span,
        }],
        is_constructor: false,
        span,
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("for-statement function should lower");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[2.0], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 6.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_inlines_function_row_slice_from_scoped_matrix_input() {
    let mut dae_model = dae::Dae::default();
    let mut pick_row = rumoca_core::Function::new("My.pickRow", lower_test_span());
    pick_row.inputs = vec![
        function_param_with_dims("R_T", &[3, 3]),
        function_param_with_dims("sequence", &[3]),
    ];
    pick_row.outputs = vec![function_param("out")];
    pick_row.locals = vec![function_param_with_dims("e3_1", &[3])];
    pick_row.body = vec![
        rumoca_core::Statement::Assignment {
            comp: component_ref("e3_1"),
            value: rumoca_core::Expression::Index {
                base: Box::new(var("R_T")),
                subscripts: vec![
                    rumoca_core::Subscript::generated_expr(
                        Box::new(var("sequence[3]")),
                        lower_test_span(),
                    ),
                    rumoca_core::Subscript::Colon {
                        span: lower_test_span(),
                    },
                ],
                span: lower_test_span(),
            },
            span: lower_test_span(),
        },
        rumoca_core::Statement::Assignment {
            comp: component_ref("out"),
            value: var_index("e3_1", 2),
            span: lower_test_span(),
        },
    ];
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.pickRow"), pick_row);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.pickRow",
        )),
        args: vec![
            rumoca_core::Expression::Array {
                elements: (1..=9)
                    .map(|value| rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(value),
                        span: lower_test_span(),
                    })
                    .collect(),
                is_matrix: true,
                span: lower_test_span(),
            },
            rumoca_core::Expression::Array {
                elements: [1, 3, 2]
                    .into_iter()
                    .map(|value| rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(value),
                        span: lower_test_span(),
                    })
                    .collect(),
                is_matrix: false,
                span: lower_test_span(),
            },
        ],
        is_constructor: false,
        span: lower_test_span(),
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("function-local matrix row slice should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 5.0);
}

#[test]
fn lower_expression_inlines_user_function_while_statement() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));

    let mut repeat_while = rumoca_core::Function::new("My.repeatWhile", lower_test_span());
    repeat_while.inputs.push(function_param("u"));
    repeat_while.outputs.push(function_param("out"));
    repeat_while.locals.push(function_param("i"));
    repeat_while.body = vec![
        rumoca_core::Statement::Assignment {
            comp: component_ref("out"),
            value: real_lit(0.0),
            span: lower_test_span(),
        },
        rumoca_core::Statement::Assignment {
            comp: component_ref("i"),
            value: real_lit(0.0),
            span: lower_test_span(),
        },
        rumoca_core::Statement::While {
            block: rumoca_core::StatementBlock {
                cond: rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Lt,
                    lhs: Box::new(var("i")),
                    rhs: Box::new(real_lit(3.0)),
                    span: lower_test_span(),
                },
                stmts: vec![
                    rumoca_core::Statement::Assignment {
                        comp: component_ref("out"),
                        value: rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Add,
                            lhs: Box::new(var("out")),
                            rhs: Box::new(var("u")),
                            span: lower_test_span(),
                        },
                        span: lower_test_span(),
                    },
                    rumoca_core::Statement::Assignment {
                        comp: component_ref("i"),
                        value: rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Add,
                            lhs: Box::new(var("i")),
                            rhs: Box::new(real_lit(1.0)),
                            span: lower_test_span(),
                        },
                        span: lower_test_span(),
                    },
                ],
            },
            span: lower_test_span(),
        },
    ];
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.repeatWhile"), repeat_while);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.repeatWhile",
        )),
        args: vec![var("x")],
        is_constructor: false,
        span: lower_test_span(),
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("while-statement function should lower");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[2.0], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 6.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_uses_constant_function_input_in_for_range() {
    let mut dae_model = dae::Dae::default();
    let loop_to_n = rumoca_core::Function {
        name: rumoca_core::VarName::new("My.loopToN"),
        def_id: None,
        inputs: vec![function_param("n")],
        outputs: vec![function_param("out")],
        locals: vec![],
        body: vec![
            rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: real_lit(0.0),
                span: lower_test_span(),
            },
            rumoca_core::Statement::For {
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
                    value: rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Add,
                        lhs: Box::new(var("out")),
                        rhs: Box::new(real_lit(1.0)),
                        span: lower_test_span(),
                    },
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
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.loopToN"), loop_to_n);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.loopToN",
        )),
        args: vec![int_lit(4)],
        is_constructor: false,
        span: lower_test_span(),
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("function input constant should lower for-loop range");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 4.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_uses_for_index_in_function_assignment_target() {
    let mut dae_model = dae::Dae::default();
    let fill_array = rumoca_core::Function {
        name: rumoca_core::VarName::new("My.fillArray"),
        def_id: None,
        inputs: vec![],
        outputs: vec![function_param("out")],
        locals: vec![function_param_with_dims("m", &[3])],
        body: vec![
            rumoca_core::Statement::For {
                indices: vec![rumoca_core::ForIndex {
                    ident: "i".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(int_lit(1)),
                        step: None,
                        end: Box::new(int_lit(3)),
                        span: lower_test_span(),
                    },
                }],
                equations: vec![rumoca_core::Statement::Assignment {
                    comp: component_ref_index_expr(
                        "m",
                        rumoca_core::Expression::BuiltinCall {
                            function: rumoca_core::BuiltinFunction::Integer,
                            args: vec![var("i")],
                            span: lower_test_span(),
                        },
                    ),
                    value: var("i"),
                    span: lower_test_span(),
                }],
                span: lower_test_span(),
            },
            rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: var_index("m", 2),
                span: lower_test_span(),
            },
        ],
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: lower_test_span(),
    };
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.fillArray"), fill_array);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.fillArray",
        )),
        args: vec![],
        is_constructor: false,
        span: lower_test_span(),
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("function assignment target with for index should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 2.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_reads_prior_indexed_function_local_matrix_assignment() {
    let mut dae_model = dae::Dae::default();
    let fill_matrix = rumoca_core::Function {
        name: rumoca_core::VarName::new("My.fillMatrix"),
        def_id: None,
        inputs: vec![],
        outputs: vec![function_param("out")],
        locals: vec![function_param_with_dims("m", &[1, 2])],
        body: vec![
            rumoca_core::Statement::For {
                indices: vec![rumoca_core::ForIndex {
                    ident: "i".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(int_lit(1)),
                        step: None,
                        end: Box::new(int_lit(1)),
                        span: lower_test_span(),
                    },
                }],
                equations: vec![
                    rumoca_core::Statement::Assignment {
                        comp: component_ref_matrix_index_expr("m", var("i"), 1),
                        value: real_lit(3.0),
                        span: lower_test_span(),
                    },
                    rumoca_core::Statement::Assignment {
                        comp: component_ref_matrix_index_expr("m", var("i"), 2),
                        value: rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Add,
                            lhs: Box::new(var_matrix_index_expr("m", var("i"), 1)),
                            rhs: Box::new(real_lit(4.0)),
                            span: lower_test_span(),
                        },
                        span: lower_test_span(),
                    },
                ],
                span: lower_test_span(),
            },
            rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: var_matrix_index_expr("m", int_lit(1), 2),
                span: lower_test_span(),
            },
        ],
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: lower_test_span(),
    };
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.fillMatrix"), fill_matrix);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.fillMatrix",
        )),
        args: vec![],
        is_constructor: false,
        span: lower_test_span(),
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("matrix element assignment should be readable in the same function algorithm");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 7.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_ignores_side_effect_function_statement_without_outputs() {
    let mut dae_model = dae::Dae::default();
    let side_effect = rumoca_core::Function {
        name: rumoca_core::VarName::new("My.sideEffect"),
        def_id: None,
        inputs: vec![function_param("u")],
        outputs: vec![function_param("out")],
        locals: vec![],
        body: vec![
            rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: var("u"),
                span: lower_test_span(),
            },
            rumoca_core::Statement::FunctionCall {
                comp: component_ref("close"),
                args: vec![real_lit(0.0)],
                outputs: vec![],
                span: lower_test_span(),
            },
            rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Add,
                    lhs: Box::new(var("out")),
                    rhs: Box::new(real_lit(1.0)),
                    span: lower_test_span(),
                },
                span: lower_test_span(),
            },
        ],
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: lower_test_span(),
    };
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.sideEffect"), side_effect);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.sideEffect",
        )),
        args: vec![real_lit(2.0)],
        is_constructor: false,
        span: lower_test_span(),
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("side-effect-only function statement should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_rejects_unlowered_runtime_special_function_statement_outputs() {
    let mut dae_model = dae::Dae::default();
    let read_line = rumoca_core::Function {
        name: rumoca_core::VarName::new("My.readLineWrapper"),
        def_id: None,
        inputs: vec![],
        outputs: vec![function_param("out")],
        locals: vec![function_param("line"), function_param("endOfFile")],
        body: vec![
            rumoca_core::Statement::FunctionCall {
                comp: component_ref("Modelica.Utilities.Streams.readLine"),
                args: vec![rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::String("memory.txt".to_string()),
                    span: lower_test_span(),
                }],
                outputs: vec![component_ref("line"), component_ref("endOfFile")],
                span: lower_test_span(),
            },
            rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Add,
                    lhs: Box::new(var("endOfFile")),
                    rhs: Box::new(real_lit(1.0)),
                    span: lower_test_span(),
                },
                span: lower_test_span(),
            },
        ],
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: lower_test_span(),
    };
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.readLineWrapper"), read_line);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.readLineWrapper",
        )),
        args: vec![],
        is_constructor: false,
        span: lower_test_span(),
    };

    let err = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect_err("unlowered runtime special statement output should fail");
    assert!(err.reason().contains("readLine must be lowered"), "{err}");
}

#[test]
fn lower_expression_scalar_position_aggregates_follow_spec_0008() {
    let layout = VarLayout::default();
    let array_expr = rumoca_core::Expression::Array {
        elements: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span: lower_test_span(),
        }],
        is_matrix: false,
        span: lower_test_span(),
    };
    let lowered = lower_expression(&array_expr, &layout, &IndexMap::new())
        .expect("array expression should lower to first scalar");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 1.0).abs() < 1e-12);

    let range_expr = rumoca_core::Expression::Range {
        start: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span: lower_test_span(),
        }),
        step: None,
        end: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(3),
            span: lower_test_span(),
        }),
        span: lower_test_span(),
    };
    // SPEC_0008: a range in scalar position must fail loudly instead of
    // lowering to a silent 0.0.
    let err = lower_expression(&range_expr, &layout, &IndexMap::new())
        .expect_err("range in scalar position must not lower");
    assert!(
        err.reason().contains("range expression in scalar position"),
        "{err}"
    );

    let empty_array = rumoca_core::Expression::Array {
        elements: vec![],
        is_matrix: false,
        span: lower_test_span(),
    };
    let err = lower_expression(&empty_array, &layout, &IndexMap::new())
        .expect_err("empty array in scalar position must not lower");
    assert!(
        err.reason().contains("array expression with 0 elements"),
        "{err}"
    );
}

#[test]
fn lower_expression_maps_namespaced_intrinsic_function_call() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "Modelica.Math.sin",
        )),
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "x",
            )),
            subscripts: vec![],
            span: lower_test_span(),
        }],
        is_constructor: false,
        span: lower_test_span(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("Modelica.Math.sin should lower as intrinsic");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[0.5], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 0.5_f64.sin()).abs() < 1e-12);
}

#[test]
fn lower_expression_binds_scalarized_record_function_inputs() {
    let mut inner = rumoca_core::Function::new("My.recordInputSum", lower_test_span());
    inner.inputs.push(rumoca_core::FunctionParam::new(
        "state",
        "Medium.ThermodynamicState",
        lower_test_span(),
    ));
    inner.outputs.push(function_param("y"));
    inner.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: add(var("state.T"), var("state.p")),
        span: lower_test_span(),
    });

    let mut outer = rumoca_core::Function::new("My.forwardRecordInput", lower_test_span());
    outer.inputs.push(rumoca_core::FunctionParam::new(
        "state",
        "Medium.ThermodynamicState",
        lower_test_span(),
    ));
    outer.outputs.push(function_param("y"));
    outer.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.recordInputSum",
            )),
            args: vec![var("state")],
            is_constructor: false,
            span: lower_test_span(),
        },
        span: lower_test_span(),
    });

    let mut dae_model = dae::Dae::default();
    dae_model
        .symbols
        .functions
        .insert(inner.name.clone(), inner);
    dae_model
        .symbols
        .functions
        .insert(outer.name.clone(), outer);
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("state.p"), scalar_var("state.p"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("state.T"), scalar_var("state.T"));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.forwardRecordInput",
        )),
        args: vec![var("state")],
        is_constructor: false,
        span: lower_test_span(),
    };
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("scalarized record input should bind its fields");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "state.p", 2.0);
    set_y_value(&layout, &mut y, "state.T", 5.0);
    let (regs, _) = eval_linear_ops(&lowered.ops, &y, &[], 0.0);

    assert!((read_reg(&regs, lowered.result) - 7.0).abs() < 1e-12);
}
