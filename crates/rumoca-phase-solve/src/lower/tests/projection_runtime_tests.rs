use super::*;

#[test]
fn lower_expression_handles_projected_array_output_with_indexed_assignments() {
    let mut functions = IndexMap::new();
    let random_like = random_like_function();
    functions.insert(random_like.name.clone(), random_like);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Pkg.randomLike.seedOut[2]").into(),
        args: vec![rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(3),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("MLS §11.1.2 indexed array output assignment should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 22.0);
}

#[test]
fn lower_discrete_rhs_expands_tuple_function_assignment_outputs() {
    let mut dae_model = dae::Dae::default();
    let random_like = random_like_function();
    dae_model
        .symbols
        .functions
        .insert(random_like.name.clone(), random_like);
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("noise"), scalar_var("noise"));
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("seedState"),
        dae::Variable {
            name: rumoca_core::VarName::new("seedState"),
            dims: vec![3],
            ..Default::default()
        },
    );
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            sub(
                rumoca_core::Expression::Tuple {
                    elements: vec![var("noise"), var("seedState")],
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Pkg.randomLike").into(),
                    args: vec![rumoca_core::Expression::Array {
                        elements: vec![
                            rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(1),
                                span: rumoca_core::Span::DUMMY,
                            },
                            rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(2),
                                span: rumoca_core::Span::DUMMY,
                            },
                            rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(3),
                                span: rumoca_core::Span::DUMMY,
                            },
                        ],
                        is_matrix: false,
                        span: rumoca_core::Span::DUMMY,
                    }],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            Default::default(),
            // MLS §12.4.3: multiple function outputs may be assigned by tuple;
            // each output component becomes an equivalent scalar update row.
            "tuple function update",
        ));

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("tuple function updates should lower through output projections");

    assert_eq!(rows.len(), 4);
}

#[test]
fn lower_discrete_rhs_expands_tuple_function_assignment_with_real_span() {
    let mut dae_model = dae::Dae::default();
    let random_like = random_like_function();
    dae_model
        .symbols
        .functions
        .insert(random_like.name.clone(), random_like);
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("noise"), scalar_var("noise"));
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("seedState"),
        dae::Variable {
            name: rumoca_core::VarName::new("seedState"),
            dims: vec![3],
            ..Default::default()
        },
    );
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::Tuple {
                    elements: vec![var("noise"), var("seedState")],
                    span: rumoca_core::Span::from_offsets(rumoca_core::SourceId(1), 10, 28),
                }),
                rhs: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Pkg.randomLike").into(),
                    args: vec![rumoca_core::Expression::Array {
                        elements: vec![
                            rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(1),
                                span: rumoca_core::Span::from_offsets(
                                    rumoca_core::SourceId(1),
                                    45,
                                    46,
                                ),
                            },
                            rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(2),
                                span: rumoca_core::Span::from_offsets(
                                    rumoca_core::SourceId(1),
                                    48,
                                    49,
                                ),
                            },
                            rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(3),
                                span: rumoca_core::Span::from_offsets(
                                    rumoca_core::SourceId(1),
                                    51,
                                    52,
                                ),
                            },
                        ],
                        is_matrix: false,
                        span: rumoca_core::Span::from_offsets(rumoca_core::SourceId(1), 44, 53),
                    }],
                    is_constructor: false,
                    span: rumoca_core::Span::from_offsets(rumoca_core::SourceId(1), 30, 54),
                }),
                span: rumoca_core::Span::from_offsets(rumoca_core::SourceId(1), 10, 54),
            },
            rumoca_core::Span::from_offsets(rumoca_core::SourceId(1), 10, 54),
            "tuple function update with source span",
        ));

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("tuple function updates with real spans should lower");

    assert_eq!(rows.len(), 4);
}

#[test]
fn lower_discrete_rhs_expands_external_random_tuple_outputs() {
    let mut dae_model = dae::Dae::default();
    let mut random = rumoca_core::Function::new(
        "Modelica.Math.Random.Generators.Xorshift64star.random",
        Default::default(),
    );
    random.external = Some(Default::default());
    random
        .inputs
        .push(rumoca_core::FunctionParam::new("stateIn", "Integer").with_dims(vec![2]));
    random
        .inputs
        .push(rumoca_core::FunctionParam::new("nState", "Integer"));
    random
        .outputs
        .push(rumoca_core::FunctionParam::new("result", "Real"));
    random
        .outputs
        .push(rumoca_core::FunctionParam::new("stateOut", "Integer").with_dims(vec![0]));
    dae_model
        .symbols
        .functions
        .insert(random.name.clone(), random);
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("r64"), scalar_var("r64"));
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("state64"),
        dae::Variable {
            name: rumoca_core::VarName::new("state64"),
            dims: vec![2],
            ..Default::default()
        },
    );
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            sub(
                rumoca_core::Expression::Tuple {
                    elements: vec![var("r64"), var("state64")],
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new(
                        "Modelica.Math.Random.Generators.Xorshift64star.random",
                    )
                    .into(),
                    args: vec![
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("previous").into(),
                            args: vec![var("state64")],
                            is_constructor: false,

                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Integer(2),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            Default::default(),
            // MLS §12.9 allows external functions. MSL random is a pure external
            // tuple function, so solve-lower represents its result and state outputs
            // explicitly instead of trying to inline an unavailable function body.
            "external random tuple update",
        ));

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("MSL random tuple outputs should lower to solve-IR random ops");

    assert_eq!(rows.len(), 3);
    assert!(rows.iter().flatten().any(|op| matches!(
        op,
        LinearOp::RandomResult {
            generator: rumoca_ir_solve::RandomGenerator::Xorshift64Star,
            ..
        }
    )));
    assert_eq!(
        rows.iter()
            .flatten()
            .filter(|op| matches!(
                op,
                LinearOp::RandomState {
                    generator: rumoca_ir_solve::RandomGenerator::Xorshift64Star,
                    ..
                }
            ))
            .count(),
        2
    );
}

#[test]
fn lower_discrete_rhs_lowers_impure_random_event_call() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("globalSeed.id_impure"),
        scalar_var("globalSeed.id_impure"),
    );
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("y"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Modelica.Math.Random.Utilities.impureRandom")
                    .into(),
                args: vec![var("globalSeed.id_impure")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Default::default(),
            // MLS §12.3: impure calls are legal in event contexts. Lower the
            // event RHS to an explicit solve-IR impure-random op instead of
            // routing through external-function string fallback.
            "impure random event update",
        ));

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("impure random event call should lower to solve-IR");

    assert_eq!(rows.len(), 1);
    assert!(
        rows[0]
            .iter()
            .any(|op| matches!(op, LinearOp::ImpureRandom { .. }))
    );
}

#[test]
fn lower_discrete_rhs_lowers_impure_random_call_inside_scalar_wrapper() {
    let mut dae_model = dae::Dae::default();
    let mut impure_random = rumoca_core::Function::new(
        "Modelica.Math.Random.Utilities.impureRandom",
        Default::default(),
    );
    impure_random.pure = false;
    impure_random.external = Some(Default::default());
    impure_random
        .inputs
        .push(rumoca_core::FunctionParam::new("id", "Integer"));
    impure_random
        .outputs
        .push(rumoca_core::FunctionParam::new("y", "Real"));
    dae_model
        .symbols
        .functions
        .insert(impure_random.name.clone(), impure_random);

    let mut wrapper = rumoca_core::Function::new("Pkg.impureWrapper", Default::default());
    wrapper.pure = false;
    wrapper
        .inputs
        .push(rumoca_core::FunctionParam::new("id", "Integer"));
    wrapper
        .outputs
        .push(rumoca_core::FunctionParam::new("y", "Real"));
    wrapper
        .locals
        .push(rumoca_core::FunctionParam::new("r", "Real"));
    wrapper.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("r"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Modelica.Math.Random.Utilities.impureRandom").into(),
            args: vec![named_arg("id", var("id"))],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    });
    wrapper.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: var("r"),
        span: rumoca_core::Span::DUMMY,
    });
    dae_model
        .symbols
        .functions
        .insert(wrapper.name.clone(), wrapper);

    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("id"), scalar_var("id"));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("y"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Pkg.impureWrapper").into(),
                args: vec![named_arg("id", var("id"))],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Default::default(),
            // MSL random integer wrappers are scalar functions that assign a
            // local from impureRandom before computing their output. The
            // record-assignment fast path must not reject the scalar external
            // impure call before runtime intrinsic lowering sees it.
            "impure random scalar wrapper event update",
        ));

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("impure random scalar wrapper should lower to solve-IR");

    assert_eq!(rows.len(), 1);
    assert!(
        rows[0]
            .iter()
            .any(|op| matches!(op, LinearOp::ImpureRandom { .. }))
    );
}

#[test]
fn lower_discrete_rhs_expands_random_state_out_array_projection() {
    let mut dae_model = dae::Dae::default();
    let mut random = rumoca_core::Function::new(
        "Modelica.Math.Random.Generators.Xorshift64star.random",
        Default::default(),
    );
    random.external = Some(Default::default());
    random
        .inputs
        .push(rumoca_core::FunctionParam::new("stateIn", "Integer").with_dims(vec![2]));
    random
        .outputs
        .push(rumoca_core::FunctionParam::new("result", "Real"));
    random
        .outputs
        .push(rumoca_core::FunctionParam::new("stateOut", "Integer").with_dims(vec![2]));
    dae_model
        .symbols
        .functions
        .insert(random.name.clone(), random);
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("state64"),
        dae::Variable {
            name: rumoca_core::VarName::new("state64"),
            dims: vec![2],
            ..Default::default()
        },
    );
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit_with_scalar_count(
            rumoca_core::VarName::new("state64"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new(
                    "Modelica.Math.Random.Generators.Xorshift64star.random.stateOut",
                )
                .into(),
                args: vec![rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("previous").into(),
                    args: vec![var("state64")],
                    is_constructor: false,

                    span: rumoca_core::Span::DUMMY,
                }],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Default::default(),
            // MLS §12.4.3: selecting one array-valued output from a multi-output
            // function is still array-valued. The solve-IR lowerer must project
            // each stateOut element, not treat `.stateOut` as a separate function.
            "random stateOut array projection",
            2,
        ));

    let layout = build_var_layout(&dae_model);
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("random.stateOut array projection should lower");

    assert_eq!(rows.len(), 2);
    assert!(rows.iter().all(|row| {
        row.iter()
            .any(|op| matches!(op, LinearOp::RandomState { .. }))
    }));
}

fn random_like_function() -> rumoca_core::Function {
    let mut random_like = rumoca_core::Function::new("Pkg.randomLike", Default::default());
    random_like
        .inputs
        .push(rumoca_core::FunctionParam::new("seedIn", "Integer").with_dims(vec![3]));
    random_like
        .outputs
        .push(rumoca_core::FunctionParam::new("x", "Real"));
    random_like
        .outputs
        .push(rumoca_core::FunctionParam::new("seedOut", "Integer").with_dims(vec![3]));
    random_like.body = vec![
        rumoca_core::Statement::Assignment {
            comp: component_ref("x"),
            value: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.25),
                span: rumoca_core::Span::DUMMY,
            },

            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Statement::Assignment {
            comp: component_ref_index("seedOut", 1),
            value: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(11),
                span: rumoca_core::Span::DUMMY,
            },

            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Statement::Assignment {
            comp: component_ref_index("seedOut", 2),
            value: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(22),
                span: rumoca_core::Span::DUMMY,
            },

            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Statement::Assignment {
            comp: component_ref_index("seedOut", 3),
            value: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(33),
                span: rumoca_core::Span::DUMMY,
            },

            span: rumoca_core::Span::DUMMY,
        },
    ];
    random_like
}

#[test]
fn lower_expression_binds_named_function_arguments_by_name() {
    let mut functions = IndexMap::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Default::default());
    function.inputs.push(function_param("a"));
    function.inputs.push(function_param("b"));
    function.outputs.push(function_param("y"));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("a").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("b").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },

        span: rumoca_core::Span::DUMMY,
    });
    functions.insert(rumoca_core::VarName::new("Pkg.f"), function);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Pkg.f").into(),
        args: vec![
            named_arg(
                "b",
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(2.0),
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            named_arg(
                "a",
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(7.0),
                    span: rumoca_core::Span::DUMMY,
                },
            ),
        ],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("named args should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 5.0).abs() < 1.0e-12);
}
#[test]
fn lower_initial_expression_rows_read_lowered_pre_parameter() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    insert_pre_parameter(&mut dae_model, "x", &[]);
    let layout = build_var_layout(&dae_model);
    let expressions = vec![rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(pre_var("x")),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    }];
    let rows =
        lower_initial_expression_rows_from_expressions(&expressions, &layout, &IndexMap::new())
            .expect("lowered initial pre parameter should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "__pre__.x", 2.0);
    let (regs, output) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let value = output.unwrap_or_else(|| read_reg(&regs, 0));
    assert!((value - 3.0).abs() < 1.0e-12);
}
#[test]
fn lower_expression_handles_constructor_field_access_by_signature() {
    let mut dae_model = dae::Dae::default();
    let mut constructor = rumoca_core::Function::new("My.Record", Default::default());
    constructor.inputs.push(function_param("R"));
    constructor.inputs.push(function_param("C"));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.Record"), constructor);

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.Record").into(),
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(2.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(3.0),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        }),
        field: "C".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("constructor field access should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() < 1e-12);
}
#[test]
fn lower_expression_handles_index_projection() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("xs"),
        dae::Variable {
            name: rumoca_core::VarName::new("xs"),
            dims: vec![2],
            ..Default::default()
        },
    );
    let layout = build_var_layout(&dae_model);
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("xs").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            2,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new()).expect("index should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[10.0, 20.0], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 20.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_indexed_record_field_array_binding() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("z.re"),
        dae::Variable {
            name: rumoca_core::VarName::new("z.re"),
            dims: vec![1],
            ..Default::default()
        },
    );
    let layout = build_var_layout(&dae_model);
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Index {
            base: Box::new(var("z")),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                1,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        }),
        field: "re".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("indexed record field array binding should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[42.0], &[], 0.0);

    assert!((read_reg(&regs, lowered.result) - 42.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_dynamic_varref_subscript_expr() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("xs"),
        dae::Variable {
            name: rumoca_core::VarName::new("xs"),
            dims: vec![3],
            ..Default::default()
        },
    );
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("i"), scalar_var("i"));
    let layout = build_var_layout(&dae_model);
    let expr = rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new("xs").into(),
        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
            rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("i").into(),
                subscripts: vec![],

                span: rumoca_core::Span::DUMMY,
            },
        ))],
        span: rumoca_core::Span::DUMMY,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("dynamic varref subscript should lower");

    let y = [10.0, 20.0, 30.0];
    for i in [1.0, 2.0, 2.7, 4.0, -1.0] {
        let (regs, _) = eval_linear_ops(&lowered.ops, &y, &[i], 0.0);
        let compiled = read_reg(&regs, lowered.result);
        let subscript = i.trunc() as i64;
        let expected = match subscript {
            1 => 10.0,
            2 => 20.0,
            3 => 30.0,
            _ => 10.0,
        };
        assert!(
            (compiled - expected).abs() < 1e-12,
            "dynamic varref mismatch for i={i}: compiled={compiled}, expected={expected}"
        );
    }
}
#[test]
fn lower_expression_handles_dynamic_index_subscript_expr() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("xs"),
        dae::Variable {
            name: rumoca_core::VarName::new("xs"),
            dims: vec![3],
            ..Default::default()
        },
    );
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("i"), scalar_var("i"));
    let layout = build_var_layout(&dae_model);
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("xs").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
            rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("i").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
        ))],
        span: rumoca_core::Span::DUMMY,
    };
    let lowered =
        lower_expression(&expr, &layout, &IndexMap::new()).expect("dynamic index should lower");

    let y = [10.0, 20.0, 30.0];
    for i in [1.0, 2.0, 2.7, 4.0, -1.0] {
        let (regs, _) = eval_linear_ops(&lowered.ops, &y, &[i], 0.0);
        let compiled = read_reg(&regs, lowered.result);
        let expected = match rounded_index(i) {
            1 => 10.0,
            2 => 20.0,
            3 => 30.0,
            _ => 0.0,
        };
        assert!(
            (compiled - expected).abs() < 1e-12,
            "dynamic index mismatch for i={i}: compiled={compiled}, expected={expected}"
        );
    }
}
#[test]
fn lower_expression_handles_dynamic_index_over_array_literal() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("i"), scalar_var("i"));
    let layout = build_var_layout(&dae_model);
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(10.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(20.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(30.0),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
            rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("i").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
        ))],
        span: rumoca_core::Span::DUMMY,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("dynamic array literal index should lower");

    for i in [1.0, 2.0, 2.7, 4.0, -1.0] {
        let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[i], 0.0);
        let compiled = read_reg(&regs, lowered.result);
        let expected = match rounded_index(i) {
            1 => 10.0,
            2 => 20.0,
            3 => 30.0,
            _ => 0.0,
        };
        assert!(
            (compiled - expected).abs() < 1e-12,
            "dynamic array literal index mismatch for i={i}: compiled={compiled}, expected={expected}"
        );
    }
}

#[test]
fn lower_expression_handles_nested_enum_table_indexing() {
    let mut dae_model = dae::Dae::default();
    insert_nested_enum_table_bindings(&mut dae_model);
    let layout = build_var_layout(&dae_model);
    let expr = nested_enum_table_expr();
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("nested enum table indexing should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "strength", 1.0);
    set_p_value(&layout, &mut p, "enable", 3.0);
    set_p_value(&layout, &mut p, "x", 1.0);

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &p, 0.0);

    // MLS §10.5: array subscripts are evaluated expressions. Enum-valued
    // conversion tables used as subscripts must feed the outer table lookup,
    // as in the MSL Digital BUF3S truth table.
    assert!((read_reg(&regs, lowered.result) - 5.0).abs() < 1e-12);
}

fn insert_nested_enum_table_bindings(dae_model: &mut dae::Dae) {
    dae_model.symbols.enum_literal_ordinals.extend([
        (nested_logic_literal("U"), 1),
        (nested_logic_literal("X"), 2),
        (nested_logic_literal("0"), 3),
        (nested_logic_literal("1"), 4),
        (nested_logic_literal("Z"), 5),
        (nested_ux01_literal("U"), 1),
        (nested_ux01_literal("X"), 2),
        (nested_ux01_literal("0"), 3),
        (nested_ux01_literal("1"), 4),
    ]);
    for name in ["strength", "enable", "x"] {
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
}

fn nested_logic_literal(name: &str) -> String {
    format!("Modelica.Electrical.Digital.Interfaces.Logic.'{name}'")
}

fn nested_ux01_literal(name: &str) -> String {
    format!("Modelica.Electrical.Digital.Interfaces.UX01.'{name}'")
}

fn nested_logic_expr(name: &str) -> rumoca_core::Expression {
    var(nested_logic_literal(name).as_str())
}

fn nested_ux01_expr(name: &str) -> rumoca_core::Expression {
    var(nested_ux01_literal(name).as_str())
}

fn nested_ux01_conversion(input: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::Array {
            elements: vec![
                nested_ux01_expr("U"),
                nested_ux01_expr("X"),
                nested_ux01_expr("0"),
                nested_ux01_expr("1"),
                nested_ux01_expr("X"),
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(input))],
        span: rumoca_core::Span::DUMMY,
    }
}

fn nested_table_plane(selected: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Array {
                elements: vec![
                    nested_logic_expr("U"),
                    nested_logic_expr("X"),
                    nested_logic_expr("0"),
                ],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Array {
                elements: vec![
                    nested_logic_expr("X"),
                    nested_logic_expr("0"),
                    nested_logic_expr("1"),
                ],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Array {
                elements: vec![selected, nested_logic_expr("1"), nested_logic_expr("U")],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_matrix: true,
        span: rumoca_core::Span::DUMMY,
    }
}

fn nested_enum_table_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::Array {
            elements: vec![
                nested_table_plane(nested_logic_expr("Z")),
                nested_table_plane(nested_logic_expr("X")),
                nested_table_plane(nested_logic_expr("U")),
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![
            rumoca_core::Subscript::generated_expr(Box::new(var("strength"))),
            rumoca_core::Subscript::generated_expr(Box::new(nested_ux01_conversion(var("enable")))),
            rumoca_core::Subscript::generated_expr(Box::new(nested_ux01_conversion(var("x")))),
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

fn insert_msl_buf3s_bindings(dae_model: &mut dae::Dae) {
    dae_model.symbols.enum_literal_ordinals.extend([
        (msl_logic_literal("U"), 1),
        (msl_logic_literal("X"), 2),
        (msl_logic_literal("0"), 3),
        (msl_logic_literal("1"), 4),
        (msl_logic_literal("Z"), 5),
        (msl_ux01_literal("U"), 1),
        (msl_ux01_literal("X"), 2),
        (msl_ux01_literal("0"), 3),
        (msl_ux01_literal("1"), 4),
        (
            "Modelica.Electrical.Digital.Interfaces.Strength.'S_X01'".to_string(),
            1,
        ),
    ]);
    for name in ["strength", "enable", "x"] {
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
}

fn msl_logic_literal(name: &str) -> String {
    format!("Modelica.Electrical.Digital.Interfaces.Logic.'{name}'")
}

fn msl_ux01_literal(name: &str) -> String {
    format!("Modelica.Electrical.Digital.Interfaces.UX01.'{name}'")
}

fn msl_logic_expr(name: &str) -> rumoca_core::Expression {
    var(msl_logic_literal(name).as_str())
}

fn msl_ux01_expr(name: &str) -> rumoca_core::Expression {
    var(msl_ux01_literal(name).as_str())
}

fn msl_buf3s_ux01_conversion(input: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::Array {
            elements: vec![
                msl_ux01_expr("U"),
                msl_ux01_expr("X"),
                msl_ux01_expr("0"),
                msl_ux01_expr("1"),
                msl_ux01_expr("X"),
                msl_ux01_expr("X"),
                msl_ux01_expr("0"),
                msl_ux01_expr("1"),
                msl_ux01_expr("X"),
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(input))],
        span: rumoca_core::Span::DUMMY,
    }
}

fn msl_buf3s_truth_plane() -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Array {
                elements: vec![
                    msl_logic_expr("U"),
                    msl_logic_expr("U"),
                    msl_logic_expr("U"),
                    msl_logic_expr("U"),
                ],
                is_matrix: false,

                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Array {
                elements: vec![
                    msl_logic_expr("U"),
                    msl_logic_expr("X"),
                    msl_logic_expr("X"),
                    msl_logic_expr("X"),
                ],
                is_matrix: false,

                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Array {
                elements: vec![
                    msl_logic_expr("Z"),
                    msl_logic_expr("Z"),
                    msl_logic_expr("Z"),
                    msl_logic_expr("Z"),
                ],
                is_matrix: false,

                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Array {
                elements: vec![
                    msl_logic_expr("U"),
                    msl_logic_expr("X"),
                    msl_logic_expr("0"),
                    msl_logic_expr("1"),
                ],
                is_matrix: false,

                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_matrix: true,
        span: rumoca_core::Span::DUMMY,
    }
}

fn msl_buf3s_truth_expr() -> rumoca_core::Expression {
    let plane = msl_buf3s_truth_plane();
    rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::Array {
            elements: vec![rumoca_core::Expression::Array {
                elements: std::iter::repeat_n(plane, 10).collect(),
                is_matrix: false,

                span: rumoca_core::Span::DUMMY,
            }],
            is_matrix: true,
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![
            rumoca_core::Subscript::generated_expr(Box::new(var("strength"))),
            rumoca_core::Subscript::generated_expr(Box::new(msl_buf3s_ux01_conversion(var(
                "enable",
            )))),
            rumoca_core::Subscript::generated_expr(Box::new(msl_buf3s_ux01_conversion(var("x")))),
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn lower_expression_handles_msl_buf3s_enum_truth_table_indexing() {
    let mut dae_model = dae::Dae::default();
    insert_msl_buf3s_bindings(&mut dae_model);
    let layout = build_var_layout(&dae_model);
    let expr = msl_buf3s_truth_expr();
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("MSL BUF3S truth-table indexing should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "strength", 1.0);
    set_p_value(&layout, &mut p, "enable", 3.0);
    set_p_value(&layout, &mut p, "x", 1.0);

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &p, 0.0);

    // MLS §10.5: each subscript expression is applied to its corresponding
    // array dimension. MSL Digital BUF3S relies on enum conversion subscripts
    // feeding the 10x4x4 truth table without flatten-order drift.
    assert!((read_reg(&regs, lowered.result) - 5.0).abs() < 1e-12);
}

#[test]
fn lower_expression_handles_projected_field_after_array_literal_index() {
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
    let layout = build_var_layout(&dae_model);
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("left").into(),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("right").into(),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("i").into(),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                },
            ))],
            span: rumoca_core::Span::DUMMY,
        }),
        field: "im".to_string(),
        span: rumoca_core::Span::DUMMY,
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
        dae::Variable {
            name: rumoca_core::VarName::new("flow"),
            dims: vec![2, 1],
            ..Default::default()
        },
    );
    let layout = build_var_layout(&dae_model);
    let expr = rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new("flow").into(),
        subscripts: vec![
            rumoca_core::Subscript::generated_index(1, rumoca_core::Span::DUMMY),
            rumoca_core::Subscript::Colon {
                span: rumoca_core::Span::DUMMY,
            },
        ],
        span: rumoca_core::Span::DUMMY,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("singleton colon projection should resolve from layout shape");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[42.0, 7.0], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 42.0);
}

#[test]
fn lower_expression_handles_nested_structural_index_over_array_literal() {
    let layout = VarLayout::default();
    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Array {
                        elements: vec![lit(1.0), lit(2.0)],
                        is_matrix: false,

                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Expression::Array {
                        elements: vec![lit(3.0), lit(4.0)],
                        is_matrix: false,

                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                is_matrix: true,
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(lit(2.0)))],

            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(lit(1.0)))],
        span: rumoca_core::Span::DUMMY,
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
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Modelica.ComplexMath.sum").into(),
            args: vec![rumoca_core::Expression::ArrayComprehension {
                expr: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Complex").into(),
                    args: vec![
                        rumoca_core::Expression::VarRef {
                            name: rumoca_core::VarName::new("k").into(),
                            subscripts: vec![],
                            span: rumoca_core::Span::DUMMY,
                        },
                        lit(1.0),
                    ],
                    is_constructor: true,

                    span: rumoca_core::Span::DUMMY,
                }),
                indices: vec![rumoca_core::ComprehensionIndex {
                    name: "k".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(lit(1.0)),
                        step: None,
                        end: Box::new(lit(2.0)),
                        span: rumoca_core::Span::DUMMY,
                    },
                }],
                filter: None,
                span: rumoca_core::Span::DUMMY,
            }],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        }),
        field: "im".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected complex sum over array comprehension should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 2.0).abs() < 1e-12);
}
#[test]
fn lower_expression_handles_projected_complex_division_component() {
    let layout = VarLayout::default();
    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Div,
            lhs: Box::new(lit(1.0)),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Complex").into(),
                args: vec![lit(2.0), lit(3.0)],
                is_constructor: true,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "im".to_string(),
        span: rumoca_core::Span::DUMMY,
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
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Complex.'+'").into(),
            args: vec![
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Complex").into(),
                    args: vec![lit(1.0), lit(2.0)],
                    is_constructor: true,

                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Complex").into(),
                    args: vec![lit(3.0), lit(4.0)],
                    is_constructor: true,

                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        }),
        field: "im".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected complex operator call should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 6.0).abs() < 1e-12);
}
#[test]
fn lower_expression_handles_projected_field_over_array_literal_in_scalar_context() {
    let layout = VarLayout::default();
    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Complex").into(),
                    args: vec![lit(2.0), lit(3.0)],
                    is_constructor: true,
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Complex").into(),
                    args: vec![lit(5.0), lit(7.0)],
                    is_constructor: true,
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }),
        field: "im".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected field over array literal should lower in scalar context");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() < 1e-12);
}
#[test]
fn lower_expression_handles_projected_complex_division_over_array_literal_in_scalar_context() {
    let layout = VarLayout::default();
    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    };
    let complex_div = |re: f64, im: f64| rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Div,
        lhs: Box::new(lit(1.0)),
        rhs: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Complex").into(),
            args: vec![lit(re), lit(im)],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Array {
            elements: vec![complex_div(2.0, 3.0), complex_div(5.0, 7.0)],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }),
        field: "re".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("projected complex division over array literal should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - (2.0 / 13.0)).abs() < 1e-12);
}
#[test]
fn lower_expression_handles_projected_complex_output_with_array_literal_field_input() {
    let mut functions = IndexMap::new();
    let mut function = rumoca_core::Function::new("Pkg.pickFirstComplexField", Default::default());
    function.inputs.push(
        rumoca_core::FunctionParam::new("c", "Modelica.ComplexMath.Complex").with_dims(vec![1]),
    );
    function.outputs.push(rumoca_core::FunctionParam::new(
        "result",
        "Modelica.ComplexMath.Complex",
    ));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("result"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Complex").into(),
            args: vec![
                rumoca_core::Expression::FieldAccess {
                    base: Box::new(rumoca_core::Expression::Index {
                        base: Box::new(rumoca_core::Expression::VarRef {
                            name: rumoca_core::VarName::new("c").into(),
                            subscripts: vec![],
                            span: rumoca_core::Span::DUMMY,
                        }),
                        subscripts: vec![rumoca_core::Subscript::generated_index(
                            1,
                            rumoca_core::Span::DUMMY,
                        )],
                        span: rumoca_core::Span::DUMMY,
                    }),
                    field: "re".to_string(),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::FieldAccess {
                    base: Box::new(rumoca_core::Expression::Index {
                        base: Box::new(rumoca_core::Expression::VarRef {
                            name: rumoca_core::VarName::new("c").into(),
                            subscripts: vec![],
                            span: rumoca_core::Span::DUMMY,
                        }),
                        subscripts: vec![rumoca_core::Subscript::generated_index(
                            1,
                            rumoca_core::Span::DUMMY,
                        )],
                        span: rumoca_core::Span::DUMMY,
                    }),
                    field: "im".to_string(),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },

        span: rumoca_core::Span::DUMMY,
    });
    functions.insert(function.name.clone(), function);

    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    };
    let arg = rumoca_core::Expression::Array {
        elements: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Complex").into(),
            args: vec![lit(2.0), lit(-3.0)],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        }],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Pkg.pickFirstComplexField.result.im").into(),
        args: vec![arg],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let lowered =
        lower_expression(&expr, &VarLayout::default(), &functions).expect("function should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) + 3.0).abs() < 1e-12);
}
#[test]
fn lower_expression_handles_projected_complex_sum_with_encoded_slice_varref() {
    let mut functions = IndexMap::new();
    let mut function = rumoca_core::Function::new("Pkg.sumComplexEncoded", Default::default());
    function.inputs.push(
        rumoca_core::FunctionParam::new("v", "Modelica.ComplexMath.Complex").with_dims(vec![3]),
    );
    function.outputs.push(rumoca_core::FunctionParam::new(
        "result",
        "Modelica.ComplexMath.Complex",
    ));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("result"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Complex").into(),
            args: vec![
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sum,
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("v[:].re").into(),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    }],
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sum,
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("v[:].im").into(),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    }],
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },

        span: rumoca_core::Span::DUMMY,
    });
    functions.insert(function.name.clone(), function);

    let lit = |value: f64| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    };
    let arg = rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Complex").into(),
                args: vec![lit(2.0), lit(-3.0)],
                is_constructor: true,
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Complex").into(),
                args: vec![lit(1.0), lit(4.0)],
                is_constructor: true,
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Complex").into(),
                args: vec![lit(-5.0), lit(2.0)],
                is_constructor: true,
                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Pkg.sumComplexEncoded.result.re").into(),
        args: vec![arg],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let lowered =
        lower_expression(&expr, &VarLayout::default(), &functions).expect("function should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) + 2.0).abs() < 1e-12);
}
#[test]
fn lower_expression_der_builtin_returns_zero() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    let layout = build_var_layout(&dae_model);
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("x").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    };
    let lowered =
        lower_expression(&expr, &layout, &IndexMap::new()).expect("der builtin should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[1.2], &[], 0.0);
    assert!(read_reg(&regs, lowered.result).abs() < 1e-12);
}
#[test]
fn lower_derivative_rhs_extracts_explicit_state_derivative_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("k"), scalar_var("k"));
    dae_model.continuous.equations.push(residual(sub(
        der(var("x")),
        mul(
            rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs: Box::new(var("k")),
                span: rumoca_core::Span::DUMMY,
            },
            var("x"),
        ),
    )));

    let layout = build_var_layout(&dae_model);
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout).expect("explicit xdot should lower"),
    );
    let (_, output) = eval_linear_ops(&rows.programs[0], &[2.0], &[3.0], 0.0);

    assert!((output.expect("xdot row") + 6.0).abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_extracts_additive_zero_residual_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("q"), scalar_var("q"));
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("qd"), scalar_var("qd"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("wq"), scalar_var("wq"));
    dae_model
        .continuous
        .equations
        .push(residual(sub(var("qd"), der(var("q")))));
    dae_model
        .continuous
        .equations
        .push(residual(add(der(var("qd")), mul(var("wq"), var("q")))));

    let layout = build_var_layout(&dae_model);
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("additive zero residual derivative rows should lower"),
    );
    let (_, q_derivative) = eval_linear_ops(&rows.programs[0], &[2.0, 5.0], &[3.0], 0.0);
    let (_, qd_derivative) = eval_linear_ops(&rows.programs[1], &[2.0, 5.0], &[3.0], 0.0);

    assert!((q_derivative.expect("q derivative row") - 5.0).abs() < 1e-12);
    assert!((qd_derivative.expect("qd derivative row") + 6.0).abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_extracts_piecewise_state_derivative_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("limited"), scalar_var("limited"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .continuous
        .equations
        .push(residual(rumoca_core::Expression::If {
            branches: vec![(var("limited"), sub(der(var("x")), real_lit(0.0)))],
            else_branch: Box::new(sub(der(var("x")), var("u"))),
            span: rumoca_core::Span::DUMMY,
        }));

    let layout = build_var_layout(&dae_model);
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("piecewise derivative equation should lower"),
    );
    let mut p = vec![0.0; 2];
    set_p_value(&layout, &mut p, "limited", 0.0);
    set_p_value(&layout, &mut p, "u", 4.0);
    let (_, unconstrained_output) = eval_linear_ops(&rows.programs[0], &[0.0], &p, 0.0);

    set_p_value(&layout, &mut p, "limited", 1.0);
    let (_, limited_output) = eval_linear_ops(&rows.programs[0], &[0.0], &p, 0.0);

    assert!((unconstrained_output.expect("unconstrained derivative") - 4.0).abs() < 1e-12);
    assert!(limited_output.expect("limited derivative").abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_extracts_nested_piecewise_inductor_row() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("i"), scalar_var("i"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("v"), scalar_var("v"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("L"), scalar_var("L"));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("quasiStatic"),
        scalar_var("quasiStatic"),
    );
    dae_model.continuous.equations.push(residual(sub(
        var("v"),
        rumoca_core::Expression::If {
            branches: vec![(var("quasiStatic"), real_lit(0.0))],
            else_branch: Box::new(mul(var("L"), der(var("i")))),
            span: rumoca_core::Span::DUMMY,
        },
    )));

    let layout = build_var_layout(&dae_model);
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("nested piecewise derivative equation should lower"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_y_value(&layout, &mut y, "v", 6.0);
    set_p_value(&layout, &mut p, "L", 2.0);
    set_p_value(&layout, &mut p, "quasiStatic", 0.0);

    let (_, output) = eval_linear_ops(&rows.programs[0], &y, &p, 0.0);

    assert!((output.expect("inductor derivative") - 3.0).abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_extracts_whole_if_with_derivative_free_branch() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("T"), scalar_var("T"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("flow_a"), scalar_var("flow_a"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("flow_b"), scalar_var("flow_b"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("m"), scalar_var("m"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("cv"), scalar_var("cv"));
    dae_model
        .continuous
        .equations
        .push(residual(rumoca_core::Expression::If {
            branches: vec![(
                binary(rumoca_core::OpBinary::Gt, var("m"), real_lit(0.0)),
                sub(
                    add(var("flow_a"), var("flow_b")),
                    mul(mul(var("m"), var("cv")), der(var("T"))),
                ),
            )],
            else_branch: Box::new(sub(add(var("flow_a"), var("flow_b")), real_lit(0.0))),
            span: rumoca_core::Span::DUMMY,
        }));

    let layout = build_var_layout(&dae_model);
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("whole-if derivative equation should lower"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_y_value(&layout, &mut y, "flow_a", 10.0);
    set_y_value(&layout, &mut y, "flow_b", 20.0);
    set_p_value(&layout, &mut p, "m", 2.0);
    set_p_value(&layout, &mut p, "cv", 5.0);

    let (_, output) = eval_linear_ops(&rows.programs[0], &y, &p, 0.0);

    assert!((output.expect("state derivative") - 3.0).abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_preserves_one_element_array_state_index() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            name: rumoca_core::VarName::new("x"),
            dims: vec![1],
            ..Default::default()
        },
    );
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .continuous
        .equations
        .push(residual(sub(der(indexed_var("x", 1)), var("u"))));

    let layout = build_var_layout(&dae_model);
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("one-element array-state derivative should lower"),
    );
    let (_, output) = eval_linear_ops(&rows.programs[0], &[0.0, 4.0], &[], 0.0);

    assert!((output.expect("x[1] derivative") - 4.0).abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_lowers_coupled_array_state_solve_to_solver_ir() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("omega"),
        dae::Variable {
            name: rumoca_core::VarName::new("omega"),
            dims: vec![2],
            ..Default::default()
        },
    );
    for name in ["J[1,1]", "J[1,2]", "J[2,1]", "J[2,2]", "tau[1]", "tau[2]"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.continuous.equations.push(residual(sub(
        add(
            mul(var("J[1,1]"), der(indexed_var("omega", 1))),
            mul(var("J[1,2]"), der(indexed_var("omega", 2))),
        ),
        var("tau[1]"),
    )));
    dae_model.continuous.equations.push(residual(sub(
        add(
            mul(var("J[2,1]"), der(indexed_var("omega", 1))),
            mul(var("J[2,2]"), der(indexed_var("omega", 2))),
        ),
        var("tau[2]"),
    )));

    let layout = build_var_layout(&dae_model);
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout).expect("coupled xdot should lower"),
    );
    assert!(rows.programs[0].iter().any(|op| {
        matches!(
            op,
            LinearOp::LinearSolveComponent {
                n: 2,
                component: 0,
                ..
            }
        )
    }));
    let y = [0.0, 0.0, 2.0, 0.0, 0.0, 4.0, 8.0, 20.0];
    let (_, first) = eval_linear_ops(&rows.programs[0], &y, &[], 0.0);
    let (_, second) = eval_linear_ops(&rows.programs[1], &y, &[], 0.0);

    assert!((first.expect("omega[1] derivative") - 4.0).abs() < 1e-12);
    assert!((second.expect("omega[2] derivative") - 5.0).abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_scalar_programs_reuse_coupled_state_components() {
    let mut dae_model = dae::Dae::default();
    for name in ["x", "y"] {
        dae_model
            .variables
            .states
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    for name in ["u", "v"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model
        .continuous
        .equations
        .push(residual(sub(add(der(var("x")), der(var("y"))), var("u"))));
    dae_model
        .continuous
        .equations
        .push(residual(sub(der(var("y")), var("v"))));

    let layout = build_var_layout(&dae_model);
    let rows = lower_derivative_rhs_scalar_programs(&dae_model, &layout)
        .expect("scalar derivative rows should preserve coupled derivative groups");

    assert_eq!(rows.len(), 2);
    assert!(rows[0].iter().any(|op| {
        matches!(
            op,
            LinearOp::LinearSolveComponent {
                n: 2,
                component: 0,
                ..
            }
        )
    }));
    assert!(rows[1].iter().any(|op| {
        matches!(
            op,
            LinearOp::LinearSolveComponent {
                n: 2,
                component: 1,
                ..
            }
        )
    }));
}
