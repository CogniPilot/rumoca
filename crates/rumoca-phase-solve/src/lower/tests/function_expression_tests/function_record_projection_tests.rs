use super::*;

#[test]
fn function_assigns_record_result_to_record_array_element() {
    let span = lower_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("which"), scalar_var("which"));

    let mut candidate_ctor = test_function("My.Candidate", span);
    candidate_ctor.is_constructor = true;
    candidate_ctor.inputs.push(function_param("length"));
    candidate_ctor
        .outputs
        .push(record_param("candidate", "My.Candidate"));
    dae_model
        .symbols
        .functions
        .insert(candidate_ctor.name.clone(), candidate_ctor);

    let mut make_candidate = test_function("My.makeCandidate", span);
    make_candidate.inputs.push(function_param("length"));
    make_candidate
        .outputs
        .push(record_param("candidate", "My.Candidate"));
    make_candidate
        .body
        .push(rumoca_core::Statement::Assignment {
            comp: component_ref("candidate.length"),
            value: source_var("length"),
            span,
        });
    dae_model
        .symbols
        .functions
        .insert(make_candidate.name.clone(), make_candidate);

    let mut select = test_function("My.selectCandidate", span);
    select.inputs.push(function_param("which"));
    select.outputs.push(function_param("out"));
    select
        .locals
        .push(record_param("candidates", "My.Candidate").with_dims(vec![2]));
    select.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref_index("candidates", 1),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::generated("My.makeCandidate"),
            args: vec![real_lit(2.0)],
            is_constructor: false,
            span,
        },
        span,
    });
    select.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref_index("candidates", 2),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::generated("My.makeCandidate"),
            args: vec![real_lit(3.0)],
            is_constructor: false,
            span,
        },
        span,
    });
    select.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(source_var("candidates")),
                subscripts: vec![rumoca_core::Subscript::generated_expr(
                    Box::new(source_var("which")),
                    span,
                )],
                span,
            }),
            field: "length".to_string(),
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(select.name.clone(), select);

    let expression = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::generated("My.selectCandidate"),
        args: vec![source_var("which")],
        is_constructor: false,
        span,
    };
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expression, &layout, &dae_model.symbols.functions)
        .expect("record function result should assign to a record-array element");

    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "which", 2.0);
    let (regs, _) = eval_linear_ops(&lowered.ops, &y, &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_lowers_delay_source_from_pre_slot() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    insert_pre_parameter(&mut dae_model, "x", &[]);
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let x_slot = layout.binding("x").expect("x should be bound");
    let pre_x_slot = layout.binding("__pre__.x").expect("pre(x) should be bound");
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Delay,
        args: vec![
            rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::from_component_reference(
                    test_component_ref_from_name("x"),
                ),
                subscripts: vec![],
                span: lower_test_span(),
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.001),
                span: lower_test_span(),
            },
        ],
        span: lower_test_span(),
    };

    let lowered = lower_expression(&expr, &layout, &IndexMap::new()).expect("delay should lower");

    // SPEC_0007 keeps event-entry memory in explicit `__pre__.*` parameter
    // slots. The current placeholder lowers delay(expr, dt) to pre(expr);
    // introducing a real delay operator belongs in a later, measured change.
    let result = lowered.result;
    if let ScalarSlot::P { index, .. } = pre_x_slot {
        assert!(
            lowered
                .ops
                .iter()
                .any(|op| matches!(op, LinearOp::LoadP { dst, index: i } if *dst == result && *i == index)),
            "delay placeholder should use the event-entry pre slot"
        );
    }
    if let ScalarSlot::P { index, .. } = x_slot {
        assert!(
            !lowered
                .ops
                .iter()
                .any(|op| matches!(op, LinearOp::LoadP { dst, index: i } if *dst == result && *i == index)),
            "delay placeholder must not read the current parameter slot when pre(x) exists"
        );
    }
}
#[test]
fn lower_expression_handles_projected_function_output_array_element() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("th"), scalar_var("th"));
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("LieGroupsSE2.rot2"),
        rot2_function(),
    );

    let expr = projected_rot2_output_expr();

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("projected function output should lower");

    let th = 0.5;
    let (regs, _) = eval_linear_ops(&lowered.ops, &[th], &[], 0.0);
    let compiled = read_reg(&regs, lowered.result);
    let expected = -th.sin();
    assert!((compiled - expected).abs() < 1e-12);
}

fn rot2_function() -> rumoca_core::Function {
    rumoca_core::Function {
        name: rumoca_core::VarName::new("LieGroupsSE2.rot2"),
        def_id: None,
        instance_id: None,
        inputs: vec![function_param("th")],
        outputs: vec![function_param_with_dims("R", &[2, 2])],
        locals: vec![],
        body: vec![rumoca_core::Statement::Assignment {
            comp: component_ref("R"),
            value: rot2_matrix_expr(),
            span: lower_test_span(),
        }],
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: lower_test_span(),
    }
}

fn rot2_matrix_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements: vec![
            array_row(vec![cos_th_expr(), neg_sin_th_expr()]),
            array_row(vec![sin_th_expr(), cos_th_expr()]),
        ],
        is_matrix: true,
        span: lower_test_span(),
    }
}

fn array_row(elements: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements,
        is_matrix: false,
        span: lower_test_span(),
    }
}

fn cos_th_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Cos,
        args: vec![var("th")],
        span: lower_test_span(),
    }
}

fn sin_th_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sin,
        args: vec![var("th")],
        span: lower_test_span(),
    }
}

fn neg_sin_th_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: Box::new(sin_th_expr()),
        span: lower_test_span(),
    }
}

fn projected_rot2_output_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "LieGroupsSE2.rot2.R[1,2]",
        )),
        args: vec![var("th")],
        is_constructor: false,
        span: lower_test_span(),
    }
}

#[test]
fn lower_projected_function_output_skips_synthetic_array_size_actuals() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("actual_q"),
        dae::Variable {
            name: rumoca_core::VarName::new("actual_q"),
            component_ref: Some(test_component_ref_from_name("actual_q")),
            dims: vec![4],
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("actual_omega"),
        dae::Variable {
            name: rumoca_core::VarName::new("actual_omega"),
            component_ref: Some(test_component_ref_from_name("actual_omega")),
            dims: vec![3],
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("gain"), scalar_var("gain"));

    let mut function = test_function("F", lower_test_span());
    function.inputs = vec![
        function_param_with_dims("q", &[4]),
        function_param_with_dims("omega", &[3]),
        function_param("gain"),
    ];
    function.outputs = vec![function_param_with_dims("q_dot", &[4])];
    function.body = vec![rumoca_core::Statement::Assignment {
        comp: component_ref_index("q_dot", 1),
        value: add(add(var("q[1]"), var("omega[2]")), var("gain")),

        span: lower_test_span(),
    }];
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("F"), function);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "F.q_dot[1]",
        )),
        args: vec![
            var("actual_q"),
            size_expr(var("actual_q"), 1),
            var("actual_omega"),
            size_expr(var("actual_omega"), 1),
            var("gain"),
        ],
        is_constructor: false,
        span,
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("projected function output should lower");
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_y_value(&layout, &mut y, "actual_q[1]", 2.0);
    set_y_value(&layout, &mut y, "actual_omega[2]", 5.0);
    set_p_value(&layout, &mut p, "gain", 7.0);
    let (regs, _) = eval_linear_ops(&lowered.ops, &y, &p, 0.0);

    assert!((read_reg(&regs, lowered.result) - 14.0).abs() < 1e-12);
}
#[test]
fn lower_expression_handles_projected_complex_function_output_field() {
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
            "My.powerOfJ.x.re",
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
        .expect("projected complex output field should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!(read_reg(&regs, lowered.result).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_implicit_single_complex_output_field_projection() {
    let mut dae_model = dae::Dae::default();
    let conj_like = conj_like_function();
    dae_model
        .symbols
        .functions
        .insert(conj_like.name.clone(), conj_like);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.conjLike.im",
        )),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "Complex",
            )),
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(2.0),
                    span: lower_test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(3.0),
                    span: lower_test_span(),
                },
            ],
            is_constructor: true,

            span: lower_test_span(),
        }],
        is_constructor: false,
        span: lower_test_span(),
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("MLS §12.4.3 single Complex output projection may omit the declared output name");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert!((read_reg(&regs, lowered.result) + 3.0).abs() < 1e-12);
}

#[test]
fn lower_expression_binds_projected_real_component_to_complex_input() {
    let mut dae_model = dae::Dae::default();
    let conj_like = conj_like_function();
    dae_model
        .symbols
        .functions
        .insert(conj_like.name.clone(), conj_like);
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u.re"), scalar_var("u.re"));

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.conjLike.re",
        )),
        args: vec![source_var("u.re")],
        is_constructor: false,
        span: lower_test_span(),
    };
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("MLS §3.7.2 projected Complex record field is a scalar Real component");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[4.5], &[], 0.0);

    assert!((read_reg(&regs, lowered.result) - 4.5).abs() < 1e-12);
}

#[test]
fn lower_expression_rebinds_flattened_record_input_components() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();

    let mut orientation = test_function("My.Orientation", span);
    orientation.is_constructor = true;
    orientation.inputs.push(
        rumoca_core::FunctionParam::new("T", "Real", lower_test_span()).with_dims(vec![3, 3]),
    );
    orientation
        .inputs
        .push(rumoca_core::FunctionParam::new("w", "Real", lower_test_span()).with_dims(vec![3]));
    dae_model
        .symbols
        .functions
        .insert(orientation.name.clone(), orientation);

    let mut resolve1 = test_function("My.resolve1", span);
    resolve1.add_input(
        rumoca_core::FunctionParam::new("R", "My.Orientation", lower_test_span())
            .with_type_class(rumoca_core::ClassType::Record),
    );
    resolve1.add_input(
        rumoca_core::FunctionParam::new("v2", "Real", lower_test_span()).with_dims(vec![3]),
    );
    resolve1.add_output(
        rumoca_core::FunctionParam::new("v1", "Real", lower_test_span()).with_dims(vec![3]),
    );
    resolve1.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("v1"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Transpose,
                args: vec![var("R.T")],
                span,
            }),
            rhs: Box::new(var("v2")),
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(resolve1.name.clone(), resolve1);

    let t_arg = matrix_arg([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]);
    let w_arg = array_arg([0.0, 0.0, 0.0]);
    let v2_arg = array_arg([2.0, 4.0, 6.0]);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.resolve1.v1[2]",
        )),
        args: vec![t_arg, w_arg, v2_arg.clone(), size_call(v2_arg, 1)],
        is_constructor: false,
        span,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("flattened record input components should bind local record fields");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert!((read_reg(&regs, lowered.result) - 4.0).abs() < 1e-12);
}

#[test]
fn lower_expression_rejects_unknown_record_constructor_input_field_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_tests_function_expression_tests_source_45.mo",
        ),
        10,
        30,
    );
    let mut dae_model = dae::Dae::default();

    let mut orientation = test_function("My.Orientation", span);
    orientation.is_constructor = true;
    orientation.inputs.push(
        rumoca_core::FunctionParam::new("T", "Real", lower_test_span()).with_dims(vec![3, 3]),
    );
    orientation
        .inputs
        .push(rumoca_core::FunctionParam::new("w", "Real", lower_test_span()).with_dims(vec![3]));
    dae_model
        .symbols
        .functions
        .insert(orientation.name.clone(), orientation);

    let mut use_orientation = test_function("My.useOrientation", span);
    use_orientation.add_input(
        rumoca_core::FunctionParam::new("R", "My.Orientation", lower_test_span())
            .with_type_class(rumoca_core::ClassType::Record),
    );
    use_orientation.add_output(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        lower_test_span(),
    ));
    use_orientation
        .body
        .push(rumoca_core::Statement::Assignment {
            comp: component_ref("y"),
            value: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.0),
                span,
            },
            span,
        });
    dae_model
        .symbols
        .functions
        .insert(use_orientation.name.clone(), use_orientation);

    let bad_constructor = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.Orientation",
        )),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("__rumoca_named_arg__.q").into(),
            args: vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span,
            }],
            is_constructor: true,
            span,
        }],
        is_constructor: true,
        span,
    };
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.useOrientation.y",
        )),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("__rumoca_named_arg__.R").into(),
            args: vec![bad_constructor],
            is_constructor: true,
            span,
        }],
        is_constructor: false,
        span,
    };

    let err = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect_err("unknown record constructor input field should fail without panicking");
    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string()
            .contains("record constructor `My.Orientation` does not define field `q`"),
        "unexpected error: {err}"
    );
}
