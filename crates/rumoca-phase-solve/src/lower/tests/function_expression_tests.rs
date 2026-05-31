use super::*;

mod statement_and_projection_tests;
use statement_and_projection_tests::{array_arg, matrix_arg, size_call};
fn complex_output_param(name: &str) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        def_id: None,
        name: name.to_string(),
        span: rumoca_core::Span::DUMMY,
        type_name: "Complex".to_string(),
        type_class: None,
        dims: vec![],
        shape_expr: Vec::new(),
        default: None,
        description: None,
    }
}

fn insert_complex_constructor(
    dae_model: &mut dae::Dae,
    im_default: Option<rumoca_core::Expression>,
) {
    let mut complex_ctor = rumoca_core::Function::new("Complex", Default::default());
    complex_ctor
        .inputs
        .push(rumoca_core::FunctionParam::new("re", "Real"));
    let imag_input = rumoca_core::FunctionParam::new("im", "Real");
    complex_ctor.inputs.push(match im_default {
        Some(default) => imag_input.with_default(default),
        None => imag_input,
    });
    complex_ctor
        .outputs
        .push(rumoca_core::FunctionParam::new("res", "Complex"));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("Complex"), complex_ctor);
}

fn complex_call(
    args: Vec<rumoca_core::Expression>,
    is_constructor: bool,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Complex").into(),
        args,
        is_constructor,
        span: rumoca_core::Span::DUMMY,
    }
}

fn conj_like_function() -> rumoca_core::Function {
    let mut conj_like = rumoca_core::Function::new("My.conjLike", Default::default());
    conj_like
        .inputs
        .push(rumoca_core::FunctionParam::new("c1", "Complex"));
    conj_like
        .outputs
        .push(rumoca_core::FunctionParam::new("c2", "Complex"));
    conj_like.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("c2"),
        value: complex_call(
            vec![
                var("c1.re"),
                rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Minus,
                    rhs: Box::new(var("c1.im")),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            true,
        ),

        span: rumoca_core::Span::DUMMY,
    });
    conj_like
}

fn eq_local(name: &str, value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(name).into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    }
}

fn array_lit(values: &[f64]) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements: values.iter().copied().map(real_lit).collect(),
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn record_ctor(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    }
}

fn component_ref_matrix_index_expr(
    name: &str,
    row: rumoca_core::Expression,
    column: i64,
) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: vec![
                rumoca_core::Subscript::generated_expr(Box::new(row)),
                rumoca_core::Subscript::generated_index(column, rumoca_core::Span::DUMMY),
            ],
        }],
        def_id: None,
    }
}

fn var_matrix_index_expr(
    name: &str,
    row: rumoca_core::Expression,
    column: i64,
) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![
            rumoca_core::Subscript::generated_expr(Box::new(row)),
            rumoca_core::Subscript::generated_index(column, rumoca_core::Span::DUMMY),
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn lower_expression_binds_function_local_array_defaults() {
    let mut function = rumoca_core::Function::new("Pkg.localArray", Default::default());
    function
        .outputs
        .push(rumoca_core::FunctionParam::new("y", "Real"));
    function.locals.push(
        rumoca_core::FunctionParam::new("a", "Real")
            .with_dims(vec![0])
            .with_default(rumoca_core::Expression::Array {
                elements: vec![real_lit(2.0), real_lit(3.0)],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            }),
    );
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var("a[1]")),
            rhs: Box::new(var("a[2]")),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    });

    let mut functions = IndexMap::new();
    functions.insert(function.name.clone(), function);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Pkg.localArray").into(),
        args: Vec::new(),
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("local array defaults should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 5.0);
}

#[test]
fn lower_expression_binds_named_record_constructor_input_fields() {
    let mut function = rumoca_core::Function::new("Pkg.recordInput", Default::default());
    function.inputs.push(rumoca_core::FunctionParam {
        type_class: Some(rumoca_core::ClassType::Record),
        type_name: "Pkg.Data".to_string(),
        ..rumoca_core::FunctionParam::new("data", "Pkg.Data")
    });
    function
        .outputs
        .push(rumoca_core::FunctionParam::new("y", "Real"));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(var("data")),
                field: "R_s".to_string(),
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(var("data.alow[2]")),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    });

    let mut functions = IndexMap::new();
    functions.insert(function.name.clone(), function);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Pkg.recordInput").into(),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Pkg.Data").into(),
            args: vec![
                named_arg(
                    "name",
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::String("metadata".to_string()),
                        span: rumoca_core::Span::DUMMY,
                    },
                ),
                named_arg("R_s", real_lit(4.0)),
                named_arg(
                    "alow",
                    rumoca_core::Expression::Array {
                        elements: vec![real_lit(2.0), real_lit(3.0)],
                        is_matrix: false,
                        span: rumoca_core::Span::DUMMY,
                    },
                ),
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("named record constructor input fields should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 7.0);
}

#[test]
fn lower_expression_projects_record_output_assigned_from_if_constructor() {
    let mut function = rumoca_core::Function::new("Pkg.recordIf", Default::default());
    function
        .inputs
        .push(rumoca_core::FunctionParam::new("u", "Real"));
    function.outputs.push(rumoca_core::FunctionParam {
        type_class: Some(rumoca_core::ClassType::Record),
        type_name: "Pkg.State".to_string(),
        ..rumoca_core::FunctionParam::new("state", "Pkg.State")
    });
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("state"),
        value: rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Gt,
                    lhs: Box::new(var("u")),
                    rhs: Box::new(real_lit(0.0)),
                    span: rumoca_core::Span::DUMMY,
                },
                record_ctor("Pkg.State", vec![named_arg("X", array_lit(&[2.0, 3.0]))]),
            )],
            else_branch: Box::new(record_ctor(
                "Pkg.State",
                vec![named_arg("X", array_lit(&[5.0, 6.0]))],
            )),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    });

    let mut functions = IndexMap::new();
    functions.insert(function.name.clone(), function);
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Pkg.recordIf").into(),
        args: vec![real_lit(1.0)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(call),
            field: "X".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            2,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("record if output field projection should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 3.0);
}

#[test]
fn lower_expression_projects_only_requested_record_output_field() {
    let mut function = rumoca_core::Function::new("Pkg.recordIf", Default::default());
    function.outputs.push(rumoca_core::FunctionParam {
        type_class: Some(rumoca_core::ClassType::Record),
        type_name: "Pkg.State".to_string(),
        ..rumoca_core::FunctionParam::new("state", "Pkg.State")
    });
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("state"),
        value: rumoca_core::Expression::If {
            branches: vec![(
                real_lit(1.0),
                record_ctor(
                    "Pkg.State",
                    vec![
                        named_arg("p", real_lit(101325.0)),
                        named_arg(
                            "T",
                            rumoca_core::Expression::FunctionCall {
                                name: rumoca_core::VarName::new("Pkg.expensiveTemperature").into(),
                                args: vec![],
                                is_constructor: false,
                                span: rumoca_core::Span::DUMMY,
                            },
                        ),
                    ],
                ),
            )],
            else_branch: Box::new(record_ctor(
                "Pkg.State",
                vec![
                    named_arg("p", real_lit(90000.0)),
                    named_arg("T", real_lit(300.0)),
                ],
            )),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    });

    let mut functions = IndexMap::new();
    functions.insert(function.name.clone(), function);
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Pkg.recordIf").into(),
        args: vec![],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(call),
        field: "p".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("projecting p must not lower unrelated record field T");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 101325.0);
}

fn build_power_of_j_function(
    branches: Vec<(rumoca_core::Expression, rumoca_core::Expression)>,
    else_branch: rumoca_core::Expression,
) -> rumoca_core::Function {
    rumoca_core::Function {
        name: rumoca_core::VarName::new("My.powerOfJ"),
        def_id: None,
        inputs: vec![function_param("k")],
        outputs: vec![complex_output_param("x")],
        locals: vec![function_param("m")],
        body: vec![
            rumoca_core::Statement::Assignment {
                comp: component_ref("m"),
                value: rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Mod,
                    args: vec![
                        rumoca_core::Expression::VarRef {
                            name: rumoca_core::VarName::new("k").into(),
                            subscripts: vec![],
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(4.0),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    span: rumoca_core::Span::DUMMY,
                },

                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Statement::Assignment {
                comp: component_ref("x"),
                value: rumoca_core::Expression::If {
                    branches,
                    else_branch: Box::new(else_branch),
                    span: rumoca_core::Span::DUMMY,
                },

                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: Default::default(),
    }
}
#[test]
fn lower_expression_round_trip_matches_eval_expr() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("z"), scalar_var("z"));
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("p"), scalar_var("p"));
    dae_model.variables.constants.insert(
        rumoca_core::VarName::new("k"),
        dae::Variable {
            name: rumoca_core::VarName::new("k"),
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span: rumoca_core::Span::DUMMY,
            }),
            ..Default::default()
        },
    );

    let expr = rumoca_core::Expression::If {
        branches: vec![(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Gt,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("x").into(),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Add,
                lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sin,
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("x").into(),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    }],
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("p").into(),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
        )],
        else_branch: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("z").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("k").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };

    let layout = build_var_layout(&dae_model);
    let lowered =
        lower_expression(&expr, &layout, &IndexMap::new()).expect("lowering should succeed");

    let y = vec![0.25, 1.5, 0.0];
    let p = vec![3.0];
    let (regs, _output) = eval_linear_ops(&lowered.ops, &y, &p, 0.4);
    let compiled = read_reg(&regs, lowered.result);

    let expected = 0.25_f64.sin() + 3.0;
    assert!((compiled - expected).abs() <= 1e-12);
}

#[test]
fn lower_expression_binds_singleton_array_actual_to_scalar_formal_lane() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("diameters"),
        dae::Variable {
            dims: vec![1],
            ..scalar_var("diameters")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("residual"),
        scalar_var("residual"),
    );

    let mut pressure_loss = rumoca_core::Function::new("pressureLoss", Default::default());
    pressure_loss
        .inputs
        .push(rumoca_core::FunctionParam::new("diameter", "Real"));
    pressure_loss
        .outputs
        .push(rumoca_core::FunctionParam::new("loss", "Real"));
    pressure_loss.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("loss"),
        value: mul(
            binary(rumoca_core::OpBinary::Div, real_lit(4.0), var("diameter")),
            real_lit(2.0),
        ),
        span: rumoca_core::Span::DUMMY,
    });
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("pressureLoss"), pressure_loss);

    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("pressureLoss").into(),
        args: vec![var("diameters")],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let sum_call = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sum,
        args: vec![call],
        span: rumoca_core::Span::DUMMY,
    };
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(var("residual"), sum_call),
        span: rumoca_core::Span::DUMMY,
        origin: "singleton vectorized scalar function call".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model);
    let rows = lower_residual(&dae_model, &layout)
        .expect("singleton array actuals should bind scalar function formals by lane");
    let y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "diameters[1]", 2.0);

    let actual = eval_linear_ops(&rows[0], &y, &p, 0.0)
        .1
        .expect("residual output");
    assert_eq!(actual, -4.0);
}
#[test]
fn lower_expression_inlines_user_function_call() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));

    let square_add_one = rumoca_core::Function {
        name: rumoca_core::VarName::new("My.squareAddOne"),
        def_id: None,
        inputs: vec![function_param("u")],
        outputs: vec![function_param("out")],
        locals: vec![],
        body: vec![rumoca_core::Statement::Assignment {
            comp: component_ref("out"),
            value: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Add,
                lhs: Box::new(rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Mul,
                    lhs: Box::new(rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("u").into(),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    }),
                    rhs: Box::new(rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("u").into(),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: Default::default(),
    };
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.squareAddOne"), square_add_one);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.squareAddOne").into(),
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("x").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    let layout = build_var_layout(&dae_model);
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("lowering should succeed");
    let y = vec![3.0];
    let p = vec![];
    let (regs, _output) = eval_linear_ops(&lowered.ops, &y, &p, 0.0);
    let compiled = read_reg(&regs, lowered.result);
    assert!((compiled - 10.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_lowers_delay_source_from_pre_slot() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    insert_pre_parameter(&mut dae_model, "x", &[]);
    let layout = build_var_layout(&dae_model);
    let x_slot = layout.binding("x").expect("x should be bound");
    let pre_x_slot = layout.binding("__pre__.x").expect("pre(x) should be bound");
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Delay,
        args: vec![
            rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("x").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.001),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        span: rumoca_core::Span::DUMMY,
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

    let layout = build_var_layout(&dae_model);
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
        inputs: vec![function_param("th")],
        outputs: vec![function_param_with_dims("R", &[2, 2])],
        locals: vec![],
        body: vec![rumoca_core::Statement::Assignment {
            comp: component_ref("R"),
            value: rot2_matrix_expr(),
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: Default::default(),
    }
}

fn rot2_matrix_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements: vec![
            array_row(vec![cos_th_expr(), neg_sin_th_expr()]),
            array_row(vec![sin_th_expr(), cos_th_expr()]),
        ],
        is_matrix: true,
        span: rumoca_core::Span::DUMMY,
    }
}

fn array_row(elements: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements,
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn cos_th_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Cos,
        args: vec![var("th")],
        span: rumoca_core::Span::DUMMY,
    }
}

fn sin_th_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sin,
        args: vec![var("th")],
        span: rumoca_core::Span::DUMMY,
    }
}

fn neg_sin_th_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: Box::new(sin_th_expr()),
        span: rumoca_core::Span::DUMMY,
    }
}

fn projected_rot2_output_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("LieGroupsSE2.rot2.R[1,2]").into(),
        args: vec![var("th")],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn lower_projected_function_output_skips_synthetic_array_size_actuals() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("actual_q"),
        dae::Variable {
            name: rumoca_core::VarName::new("actual_q"),
            dims: vec![4],
            ..Default::default()
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("actual_omega"),
        dae::Variable {
            name: rumoca_core::VarName::new("actual_omega"),
            dims: vec![3],
            ..Default::default()
        },
    );
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("gain"), scalar_var("gain"));

    let mut function = rumoca_core::Function::new("F", rumoca_core::Span::DUMMY);
    function.inputs = vec![
        function_param_with_dims("q", &[4]),
        function_param_with_dims("omega", &[3]),
        function_param("gain"),
    ];
    function.outputs = vec![function_param_with_dims("q_dot", &[4])];
    function.body = vec![rumoca_core::Statement::Assignment {
        comp: component_ref_index("q_dot", 1),
        value: add(add(var("q[1]"), var("omega[2]")), var("gain")),

        span: rumoca_core::Span::DUMMY,
    }];
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("F"), function);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("F.q_dot[1]").into(),
        args: vec![
            var("actual_q"),
            size_expr(var("actual_q"), 1),
            var("actual_omega"),
            size_expr(var("actual_omega"), 1),
            var("gain"),
        ],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    let layout = build_var_layout(&dae_model);
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
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(0.0),
                            span: rumoca_core::Span::DUMMY,
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
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: rumoca_core::Span::DUMMY,
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
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(-1.0),
                    span: rumoca_core::Span::DUMMY,
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
        name: rumoca_core::VarName::new("My.powerOfJ.x.re").into(),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
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
        name: rumoca_core::VarName::new("My.conjLike.im").into(),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Complex").into(),
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
        }],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
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
        name: rumoca_core::VarName::new("My.conjLike.re").into(),
        args: vec![var("u.re")],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let layout = build_var_layout(&dae_model);
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("MLS §3.7.2 projected Complex record field is a scalar Real component");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[4.5], &[], 0.0);

    assert!((read_reg(&regs, lowered.result) - 4.5).abs() < 1e-12);
}

#[test]
fn lower_expression_rebinds_flattened_record_input_components() {
    let mut dae_model = dae::Dae::default();

    let mut orientation = rumoca_core::Function::new("My.Orientation", Default::default());
    orientation.is_constructor = true;
    orientation
        .inputs
        .push(rumoca_core::FunctionParam::new("T", "Real").with_dims(vec![3, 3]));
    orientation
        .inputs
        .push(rumoca_core::FunctionParam::new("w", "Real").with_dims(vec![3]));
    dae_model
        .symbols
        .functions
        .insert(orientation.name.clone(), orientation);

    let mut resolve1 = rumoca_core::Function::new("My.resolve1", Default::default());
    resolve1.add_input(
        rumoca_core::FunctionParam::new("R", "My.Orientation")
            .with_type_class(rumoca_core::ClassType::Record),
    );
    resolve1.add_input(rumoca_core::FunctionParam::new("v2", "Real").with_dims(vec![3]));
    resolve1.add_output(rumoca_core::FunctionParam::new("v1", "Real").with_dims(vec![3]));
    resolve1.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("v1"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Transpose,
                args: vec![var("R.T")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(var("v2")),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    });
    dae_model
        .symbols
        .functions
        .insert(resolve1.name.clone(), resolve1);

    let t_arg = matrix_arg([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]);
    let w_arg = array_arg([0.0, 0.0, 0.0]);
    let v2_arg = array_arg([2.0, 4.0, 6.0]);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.resolve1.v1[2]").into(),
        args: vec![t_arg, w_arg, v2_arg.clone(), size_call(v2_arg, 1)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("flattened record input components should bind local record fields");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert!((read_reg(&regs, lowered.result) - 4.0).abs() < 1e-12);
}
