use super::*;

#[test]
fn lower_expression_handles_projected_field_after_array_literal_index() {
    let span = lower_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("i"), scalar_var("i"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("left.im"), scalar_var("left.im"));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("right.im"),
        scalar_var("right.im"),
    );
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::from_component_reference(
                            source_component_ref_from_name("left"),
                        ),
                        subscripts: vec![],
                        span,
                    },
                    rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::from_component_reference(
                            source_component_ref_from_name("right"),
                        ),
                        subscripts: vec![],
                        span,
                    },
                ],
                is_matrix: false,
                span,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_expr(
                Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::from_component_reference(
                        source_component_ref_from_name("i"),
                    ),
                    subscripts: vec![],
                    span,
                }),
                span,
            )],
            span,
        }),
        field: "im".to_string(),
        span,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected field after array literal index should lower");

    for i in [1.0, 2.0, 3.0] {
        let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[i, 4.0, 9.0], 0.0);
        let compiled = read_reg(&regs, lowered.result);
        let expected = match rounded_index(i) {
            1 => 4.0,
            2 => 9.0,
            _ => 0.0,
        };
        assert!(
            (compiled - expected).abs() < 1e-12,
            "projected field after array literal index mismatch for i={i}: compiled={compiled}, expected={expected}"
        );
    }
}

#[test]
fn lower_expression_collapses_singleton_colon_projection() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("flow"),
        array_var("flow", &[2, 1]),
    );
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let expr = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "flow",
        )),
        subscripts: vec![
            rumoca_core::Subscript::generated_index(1, lower_test_span()),
            rumoca_core::Subscript::Colon {
                span: lower_test_span(),
            },
        ],
        span: lower_test_span(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("singleton colon projection should resolve from layout shape");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[42.0, 7.0], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 42.0);
}

#[test]
fn lower_expression_handles_nested_structural_index_over_array_literal() {
    let layout = VarLayout::default();
    let span = lower_test_span();
    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span,
    };
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Array {
                        elements: vec![lit(1.0), lit(2.0)],
                        is_matrix: false,

                        span,
                    },
                    rumoca_core::Expression::Array {
                        elements: vec![lit(3.0), lit(4.0)],
                        is_matrix: false,

                        span,
                    },
                ],
                is_matrix: true,
                span,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_expr(
                Box::new(lit(2.0)),
                span,
            )],

            span,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_expr(
            Box::new(lit(1.0)),
            span,
        )],
        span,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("nested structural index should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_sum_over_array_comprehension() {
    let layout = VarLayout::default();
    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: lower_test_span(),
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "Modelica.ComplexMath.sum",
            )),
            args: vec![rumoca_core::Expression::ArrayComprehension {
                expr: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::from_component_reference(
                        test_component_ref_from_name("Complex"),
                    ),
                    args: vec![
                        rumoca_core::Expression::VarRef {
                            name: rumoca_core::Reference::from_component_reference(
                                test_component_ref_from_name("k"),
                            ),
                            subscripts: vec![],
                            span: lower_test_span(),
                        },
                        lit(1.0),
                    ],
                    is_constructor: true,

                    span: lower_test_span(),
                }),
                indices: vec![rumoca_core::ComprehensionIndex {
                    name: "k".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(lit(1.0)),
                        step: None,
                        end: Box::new(lit(2.0)),
                        span: lower_test_span(),
                    },
                }],
                filter: None,
                span: lower_test_span(),
            }],
            is_constructor: false,
            span: lower_test_span(),
        }),
        field: "im".to_string(),
        span: lower_test_span(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected complex sum over array comprehension should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 2.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_division_component() {
    let layout = VarLayout::default();
    let span = lower_test_span();
    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Div,
            lhs: Box::new(lit(1.0)),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::from_component_reference(
                    test_component_ref_from_name("Complex"),
                ),
                args: vec![lit(2.0), lit(3.0)],
                is_constructor: true,
                span,
            }),
            span,
        }),
        field: "im".to_string(),
        span,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected complex division component should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) + 3.0 / 13.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_operator_call() {
    let layout = VarLayout::default();
    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: lower_test_span(),
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "Complex.'+'",
            )),
            args: vec![
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::from_component_reference(
                        test_component_ref_from_name("Complex"),
                    ),
                    args: vec![lit(1.0), lit(2.0)],
                    is_constructor: true,

                    span: lower_test_span(),
                },
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::from_component_reference(
                        test_component_ref_from_name("Complex"),
                    ),
                    args: vec![lit(3.0), lit(4.0)],
                    is_constructor: true,

                    span: lower_test_span(),
                },
            ],
            is_constructor: false,
            span: lower_test_span(),
        }),
        field: "im".to_string(),
        span: lower_test_span(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected complex operator call should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 6.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_field_over_array_literal_in_scalar_context() {
    let layout = VarLayout::default();
    let span = lower_test_span();
    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::from_component_reference(
                        test_component_ref_from_name("Complex"),
                    ),
                    args: vec![lit(2.0), lit(3.0)],
                    is_constructor: true,
                    span,
                },
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::from_component_reference(
                        test_component_ref_from_name("Complex"),
                    ),
                    args: vec![lit(5.0), lit(7.0)],
                    is_constructor: true,
                    span,
                },
            ],
            is_matrix: false,
            span,
        }),
        field: "im".to_string(),
        span,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected field over array literal should lower in scalar context");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_division_over_array_literal_in_scalar_context() {
    let layout = VarLayout::default();
    let span = lower_test_span();
    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span,
    };
    let complex_div = |re: f64, im: f64| rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Div,
        lhs: Box::new(lit(1.0)),
        rhs: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "Complex",
            )),
            args: vec![lit(re), lit(im)],
            is_constructor: true,
            span,
        }),
        span,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Array {
            elements: vec![complex_div(2.0, 3.0), complex_div(5.0, 7.0)],
            is_matrix: false,
            span,
        }),
        field: "re".to_string(),
        span,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected complex division over array literal should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - (2.0 / 13.0)).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_output_with_array_literal_field_input() {
    let mut functions = IndexMap::new();
    let span = lower_test_span();
    let mut function = test_function("Pkg.pickFirstComplexField", span);
    function.inputs.push(
        rumoca_core::FunctionParam::new("c", "Modelica.ComplexMath.Complex", lower_test_span())
            .with_dims(vec![1]),
    );
    function.outputs.push(rumoca_core::FunctionParam::new(
        "result",
        "Modelica.ComplexMath.Complex",
        lower_test_span(),
    ));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: source_component_ref_from_name("result"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "Complex",
            )),
            args: vec![
                rumoca_core::Expression::FieldAccess {
                    base: Box::new(rumoca_core::Expression::Index {
                        base: Box::new(rumoca_core::Expression::VarRef {
                            name: rumoca_core::Reference::from_component_reference(
                                source_component_ref_from_name("c"),
                            ),
                            subscripts: vec![],
                            span,
                        }),
                        subscripts: vec![rumoca_core::Subscript::generated_index(1, span)],
                        span,
                    }),
                    field: "re".to_string(),
                    span,
                },
                rumoca_core::Expression::FieldAccess {
                    base: Box::new(rumoca_core::Expression::Index {
                        base: Box::new(rumoca_core::Expression::VarRef {
                            name: rumoca_core::Reference::from_component_reference(
                                source_component_ref_from_name("c"),
                            ),
                            subscripts: vec![],
                            span,
                        }),
                        subscripts: vec![rumoca_core::Subscript::generated_index(1, span)],
                        span,
                    }),
                    field: "im".to_string(),
                    span,
                },
            ],
            is_constructor: true,
            span,
        },

        span,
    });
    functions.insert(function.name.clone(), function);

    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span,
    };
    let arg = rumoca_core::Expression::Array {
        elements: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "Complex",
            )),
            args: vec![lit(2.0), lit(-3.0)],
            is_constructor: true,
            span,
        }],
        is_matrix: false,
        span,
    };
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "Pkg.pickFirstComplexField.result.im",
        )),
        args: vec![arg],
        is_constructor: false,
        span,
    };
    let lowered =
        lower_expression(&expr, &VarLayout::default(), &functions).expect("function should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) + 3.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_complex_sum_with_encoded_slice_varref() {
    let mut functions = IndexMap::new();
    let span = lower_test_span();
    let mut function = test_function("Pkg.sumComplexEncoded", lower_test_span());
    function.inputs.push(
        rumoca_core::FunctionParam::new("v", "Modelica.ComplexMath.Complex", lower_test_span())
            .with_dims(vec![3]),
    );
    function.outputs.push(rumoca_core::FunctionParam::new(
        "result",
        "Modelica.ComplexMath.Complex",
        lower_test_span(),
    ));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("result"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "Complex",
            )),
            args: vec![
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sum,
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::from_component_reference(
                            test_component_ref_from_name("v[:].re"),
                        ),
                        subscripts: vec![],
                        span: lower_test_span(),
                    }],
                    span: lower_test_span(),
                },
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sum,
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::from_component_reference(
                            test_component_ref_from_name("v[:].im"),
                        ),
                        subscripts: vec![],
                        span: lower_test_span(),
                    }],
                    span: lower_test_span(),
                },
            ],
            is_constructor: true,
            span: lower_test_span(),
        },

        span: lower_test_span(),
    });
    functions.insert(function.name.clone(), function);

    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span,
    };
    let arg = rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::from_component_reference(
                    test_component_ref_from_name("Complex"),
                ),
                args: vec![lit(2.0), lit(-3.0)],
                is_constructor: true,
                span,
            },
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::from_component_reference(
                    test_component_ref_from_name("Complex"),
                ),
                args: vec![lit(1.0), lit(4.0)],
                is_constructor: true,
                span,
            },
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::from_component_reference(
                    test_component_ref_from_name("Complex"),
                ),
                args: vec![lit(-5.0), lit(2.0)],
                is_constructor: true,
                span,
            },
        ],
        is_matrix: false,
        span,
    };
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "Pkg.sumComplexEncoded.result.re",
        )),
        args: vec![arg],
        is_constructor: false,
        span,
    };
    let lowered =
        lower_expression(&expr, &VarLayout::default(), &functions).expect("function should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) + 2.0).abs() < 1e-12);
}

#[test]
fn lower_expression_rejects_complex_array_input_width_mismatch_with_span() {
    let mut functions = IndexMap::new();
    let mut function = test_function("Pkg.sumComplexEncoded", lower_test_span());
    function.inputs.push(
        rumoca_core::FunctionParam::new("v", "Modelica.ComplexMath.Complex", lower_test_span())
            .with_dims(vec![3]),
    );
    function.outputs.push(rumoca_core::FunctionParam::new(
        "result",
        "Modelica.ComplexMath.Complex",
        lower_test_span(),
    ));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("result"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "Complex",
            )),
            args: vec![
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sum,
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::from_component_reference(
                            test_component_ref_from_name("v[:].re"),
                        ),
                        subscripts: vec![],
                        span: lower_test_span(),
                    }],
                    span: lower_test_span(),
                },
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sum,
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::from_component_reference(
                            test_component_ref_from_name("v[:].im"),
                        ),
                        subscripts: vec![],
                        span: lower_test_span(),
                    }],
                    span: lower_test_span(),
                },
            ],
            is_constructor: true,
            span: lower_test_span(),
        },
        span: lower_test_span(),
    });
    functions.insert(function.name.clone(), function);

    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: lower_test_span(),
    };
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_tests_projection_runtime_tests_complex_projection_source_61.mo",
        ),
        12,
        48,
    );
    let arg = rumoca_core::Expression::Array {
        elements: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "Complex",
            )),
            args: vec![lit(2.0), lit(-3.0)],
            is_constructor: true,
            span,
        }],
        is_matrix: false,
        span,
    };
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "Pkg.sumComplexEncoded.result.re",
        )),
        args: vec![arg],
        is_constructor: false,
        span,
    };

    let err = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect_err("Complex array input actual width must match declared shape");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "invalid IR contract: function `Pkg.sumComplexEncoded` Complex input `v` expected 3 scalar value(s) for shape [3], got 1"
    );
}

#[test]
fn lower_expression_der_builtin_returns_zero() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "x",
            )),
            subscripts: vec![],
            span: lower_test_span(),
        }],
        span: lower_test_span(),
    };
    let lowered =
        lower_expression(&expr, &layout, &IndexMap::new()).expect("der builtin should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[1.2], &[], 0.0);
    assert!(read_reg(&regs, lowered.result).abs() < 1e-12);
}
