//! SPEC_0021 file-size exception: function-expression regression fixtures share
//! common function/DAE builders; split plan: continue moving behavioral groups
//! into the existing focused sibling modules.

use super::*;

mod shape_diagnostic_tests;
mod statement_and_projection_tests;
use statement_and_projection_tests::{array_arg, matrix_arg, size_call};
fn complex_output_param(name: &str) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        def_id: None,
        type_def_id: None,
        name: name.to_string(),
        span: lower_test_span(),
        type_name: "Complex".to_string(),
        type_class: None,
        dims: vec![],
        shape_expr: Vec::new(),
        default: None,
        min: None,
        max: None,
        description: None,
    }
}

fn record_param(name: &str, type_name: &str) -> rumoca_core::FunctionParam {
    let mut param = rumoca_core::FunctionParam::new(name, type_name, lower_test_span());
    param.type_class = Some(rumoca_core::ClassType::Record);
    param
}

fn insert_complex_constructor(
    dae_model: &mut dae::Dae,
    im_default: Option<rumoca_core::Expression>,
) {
    let mut complex_ctor = test_function("Complex", lower_test_span());
    complex_ctor.inputs.push(rumoca_core::FunctionParam::new(
        "re",
        "Real",
        lower_test_span(),
    ));
    let imag_input = rumoca_core::FunctionParam::new("im", "Real", lower_test_span());
    complex_ctor.inputs.push(match im_default {
        Some(default) => imag_input.with_default(default),
        None => imag_input,
    });
    complex_ctor.outputs.push(rumoca_core::FunctionParam::new(
        "res",
        "Complex",
        lower_test_span(),
    ));
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
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "Complex",
        )),
        args,
        is_constructor,
        span: lower_test_span(),
    }
}

fn conj_like_function() -> rumoca_core::Function {
    let mut conj_like = test_function("My.conjLike", lower_test_span());
    conj_like.inputs.push(rumoca_core::FunctionParam::new(
        "c1",
        "Complex",
        lower_test_span(),
    ));
    conj_like.outputs.push(rumoca_core::FunctionParam::new(
        "c2",
        "Complex",
        lower_test_span(),
    ));
    conj_like.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("c2"),
        value: complex_call(
            vec![
                var("c1.re"),
                rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Minus,
                    rhs: Box::new(var("c1.im")),
                    span: lower_test_span(),
                },
            ],
            true,
        ),

        span: lower_test_span(),
    });
    conj_like
}

fn eq_local(name: &str, value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(name).into(),
            subscripts: vec![],
            span: lower_test_span(),
        }),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: lower_test_span(),
        }),
        span: lower_test_span(),
    }
}

fn array_lit(values: &[f64]) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements: values.iter().copied().map(real_lit).collect(),
        is_matrix: false,
        span: lower_test_span(),
    }
}

#[test]
fn lower_function_call_does_not_fold_self_referential_start_metadata() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .metadata
        .variable_starts
        .insert("x".to_string(), var("x"));

    let mut identity = test_function("My.identity", lower_test_span());
    identity.inputs.push(function_param("u"));
    identity.outputs.push(function_param("y"));
    identity.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: var("u"),
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(identity.name.clone(), identity);

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_expression_rows_from_expressions_with_runtime_metadata(
        &[rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.identity",
            )),
            args: vec![var("x")],
            is_constructor: false,
            span: lower_test_span(),
        }],
        &layout,
        &dae_model.symbols.functions,
        &dae_model.clocks.intervals,
        &dae_model.clocks.timings,
        &dae_model.metadata.variable_starts,
    )
    .expect("self-referential start metadata should not recurse during constant folding");

    assert_eq!(rows.len(), 1);
    assert!(
        rows[0]
            .iter()
            .any(|op| matches!(op, LinearOp::LoadY { .. }))
    );
}

fn record_ctor(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor: true,
        span: lower_test_span(),
    }
}

fn component_ref_matrix_index_expr(
    name: &str,
    row: rumoca_core::Expression,
    column: i64,
) -> rumoca_core::ComponentReference {
    let span = row.span().unwrap_or_else(lower_test_span);
    rumoca_core::ComponentReference {
        local: false,
        span,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span,
            subs: vec![
                rumoca_core::Subscript::generated_expr(Box::new(row), span),
                rumoca_core::Subscript::generated_index(column, span),
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
    let span = row.span().unwrap_or_else(lower_test_span);
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
            name,
        )),
        subscripts: vec![
            rumoca_core::Subscript::generated_expr(Box::new(row), span),
            rumoca_core::Subscript::generated_index(column, span),
        ],
        span,
    }
}

#[test]
fn lower_expression_binds_function_local_array_defaults() {
    let mut function = test_function("Pkg.localArray", lower_test_span());
    function.outputs.push(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        lower_test_span(),
    ));
    function.locals.push(
        rumoca_core::FunctionParam::new("a", "Real", lower_test_span())
            .with_dims(vec![0])
            .with_default(rumoca_core::Expression::Array {
                elements: vec![real_lit(2.0), real_lit(3.0)],
                is_matrix: false,
                span: lower_test_span(),
            }),
    );
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var("a[1]")),
            rhs: Box::new(var("a[2]")),
            span: lower_test_span(),
        },
        span: lower_test_span(),
    });

    let mut functions = IndexMap::new();
    functions.insert(function.name.clone(), function);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "Pkg.localArray",
        )),
        args: Vec::new(),
        is_constructor: false,
        span: lower_test_span(),
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("local array defaults should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 5.0);
}

#[test]
fn lower_expression_binds_named_record_constructor_input_fields() {
    let mut function = test_function("Pkg.recordInput", lower_test_span());
    function.inputs.push(rumoca_core::FunctionParam {
        type_class: Some(rumoca_core::ClassType::Record),
        type_name: "Pkg.Data".to_string(),
        ..rumoca_core::FunctionParam::new("data", "Pkg.Data", lower_test_span())
    });
    function.outputs.push(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        lower_test_span(),
    ));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(var("data")),
                field: "R_s".to_string(),
                span: lower_test_span(),
            }),
            rhs: Box::new(var("data.alow[2]")),
            span: lower_test_span(),
        },
        span: lower_test_span(),
    });

    // The record constructor must be registered: solve refuses to fabricate
    // field metadata for unregistered constructors (SPEC_0008).
    let mut constructor = test_function("Pkg.Data", lower_test_span());
    constructor.is_constructor = true;
    constructor.inputs.push(rumoca_core::FunctionParam::new(
        "name",
        "String",
        lower_test_span(),
    ));
    constructor.inputs.push(rumoca_core::FunctionParam::new(
        "R_s",
        "Real",
        lower_test_span(),
    ));
    constructor.inputs.push(rumoca_core::FunctionParam {
        dims: vec![2],
        ..rumoca_core::FunctionParam::new("alow", "Real", lower_test_span())
    });

    let mut functions = IndexMap::new();
    functions.insert(function.name.clone(), function);
    functions.insert(constructor.name.clone(), constructor);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "Pkg.recordInput",
        )),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "Pkg.Data",
            )),
            args: vec![
                named_arg(
                    "name",
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::String("metadata".to_string()),
                        span: lower_test_span(),
                    },
                ),
                named_arg("R_s", real_lit(4.0)),
                named_arg(
                    "alow",
                    rumoca_core::Expression::Array {
                        elements: vec![real_lit(2.0), real_lit(3.0)],
                        is_matrix: false,
                        span: lower_test_span(),
                    },
                ),
            ],
            is_constructor: true,
            span: lower_test_span(),
        }],
        is_constructor: false,
        span: lower_test_span(),
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("named record constructor input fields should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 7.0);
}

#[test]
fn lower_expression_projects_record_output_assigned_from_if_constructor() {
    let span = lower_test_span();
    let mut function = test_function("Pkg.recordIf", lower_test_span());
    function.inputs.push(rumoca_core::FunctionParam::new(
        "u",
        "Real",
        lower_test_span(),
    ));
    function.outputs.push(rumoca_core::FunctionParam {
        type_class: Some(rumoca_core::ClassType::Record),
        type_name: "Pkg.State".to_string(),
        ..rumoca_core::FunctionParam::new("state", "Pkg.State", lower_test_span())
    });
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("state"),
        value: rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Gt,
                    lhs: Box::new(var("u")),
                    rhs: Box::new(real_lit(0.0)),
                    span,
                },
                record_ctor("Pkg.State", vec![named_arg("X", array_lit(&[2.0, 3.0]))]),
            )],
            else_branch: Box::new(record_ctor(
                "Pkg.State",
                vec![named_arg("X", array_lit(&[5.0, 6.0]))],
            )),
            span,
        },
        span,
    });

    let mut functions = IndexMap::new();
    functions.insert(function.name.clone(), function);
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "Pkg.recordIf",
        )),
        args: vec![real_lit(1.0)],
        is_constructor: false,
        span,
    };
    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(call),
            field: "X".to_string(),
            span,
        }),
        subscripts: vec![rumoca_core::Subscript::generated_index(2, span)],
        span,
    };

    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("record if output field projection should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 3.0);
}

#[test]
fn lower_expression_projects_only_requested_record_output_field() {
    let mut function = test_function("Pkg.recordIf", lower_test_span());
    let span = lower_test_span();
    function.outputs.push(rumoca_core::FunctionParam {
        type_class: Some(rumoca_core::ClassType::Record),
        type_name: "Pkg.State".to_string(),
        ..rumoca_core::FunctionParam::new("state", "Pkg.State", lower_test_span())
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
                                name: rumoca_core::Reference::from_component_reference(
                                    test_component_ref_from_name("Pkg.expensiveTemperature"),
                                ),
                                args: vec![],
                                is_constructor: false,
                                span,
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
            span,
        },
        span,
    });

    let mut functions = IndexMap::new();
    functions.insert(function.name.clone(), function);
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "Pkg.recordIf",
        )),
        args: vec![],
        is_constructor: false,
        span,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(call),
        field: "p".to_string(),
        span,
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
    let span = lower_test_span();
    rumoca_core::Function {
        name: rumoca_core::VarName::new("My.powerOfJ"),
        def_id: None,
        instance_id: None,
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
                            name: rumoca_core::Reference::from_component_reference(
                                test_component_ref_from_name("k"),
                            ),
                            subscripts: vec![],
                            span,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(4.0),
                            span,
                        },
                    ],
                    span,
                },

                span,
            },
            rumoca_core::Statement::Assignment {
                comp: component_ref("x"),
                value: rumoca_core::Expression::If {
                    branches,
                    else_branch: Box::new(else_branch),
                    span,
                },

                span,
            },
        ],
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span,
    }
}
#[test]
fn lower_expression_round_trip_matches_eval_expr() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
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
                span: lower_test_span(),
            }),
            ..rumoca_ir_dae::Variable::empty_with_span(span)
        },
    );

    let expr = rumoca_core::Expression::If {
        branches: vec![(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Gt,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::from_component_reference(
                        test_component_ref_from_name("x"),
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
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Add,
                lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sin,
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::from_component_reference(
                            test_component_ref_from_name("x"),
                        ),
                        subscripts: vec![],
                        span: lower_test_span(),
                    }],
                    span: lower_test_span(),
                }),
                rhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::from_component_reference(
                        test_component_ref_from_name("p"),
                    ),
                    subscripts: vec![],
                    span: lower_test_span(),
                }),
                span: lower_test_span(),
            },
        )],
        else_branch: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::from_component_reference(
                    test_component_ref_from_name("z"),
                ),
                subscripts: vec![],
                span: lower_test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::from_component_reference(
                    test_component_ref_from_name("k"),
                ),
                subscripts: vec![],
                span: lower_test_span(),
            }),
            span: lower_test_span(),
        }),
        span,
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
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

    let mut pressure_loss = test_function("pressureLoss", lower_test_span());
    pressure_loss.inputs.push(rumoca_core::FunctionParam::new(
        "diameter",
        "Real",
        lower_test_span(),
    ));
    pressure_loss.outputs.push(rumoca_core::FunctionParam::new(
        "loss",
        "Real",
        lower_test_span(),
    ));
    pressure_loss.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("loss"),
        value: mul(
            binary(rumoca_core::OpBinary::Div, real_lit(4.0), var("diameter")),
            real_lit(2.0),
        ),
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("pressureLoss"), pressure_loss);

    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "pressureLoss",
        )),
        args: vec![var("diameters")],
        is_constructor: false,
        span: lower_test_span(),
    };
    let sum_call = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sum,
        args: vec![call],
        span: lower_test_span(),
    };
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(var("residual"), sum_call),
        span: lower_test_span(),
        origin: "singleton vectorized scalar function call".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
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
    let span = lower_test_span();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));

    let square_add_one = rumoca_core::Function {
        name: rumoca_core::VarName::new("My.squareAddOne"),
        def_id: None,
        instance_id: None,
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
                        name: rumoca_core::Reference::from_component_reference(
                            test_component_ref_from_name("u"),
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
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span: lower_test_span(),
                }),
                span: lower_test_span(),
            },
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
        .insert(rumoca_core::VarName::new("My.squareAddOne"), square_add_one);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.squareAddOne",
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
        .expect("lowering should succeed");
    let y = vec![3.0];
    let p = vec![];
    let (regs, _output) = eval_linear_ops(&lowered.ops, &y, &p, 0.0);
    let compiled = read_reg(&regs, lowered.result);
    assert!((compiled - 10.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_binds_record_function_result_to_record_input() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("p"), scalar_var("p"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("temp"), scalar_var("temp"));

    let mut state_ctor = test_function("My.State", lower_test_span());
    state_ctor.is_constructor = true;
    state_ctor.inputs.push(function_param("p"));
    state_ctor.inputs.push(function_param("T"));
    state_ctor.outputs.push(record_param("state", "My.State"));
    dae_model
        .symbols
        .functions
        .insert(state_ctor.name.clone(), state_ctor);

    let mut make_state = test_function("My.makeState", lower_test_span());
    make_state.inputs.push(function_param("p"));
    make_state.inputs.push(function_param("T"));
    make_state.outputs.push(record_param("state", "My.State"));
    make_state.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("state"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.State",
            )),
            args: vec![var("p"), var("T")],
            is_constructor: true,
            span: lower_test_span(),
        },
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(make_state.name.clone(), make_state);

    let mut temperature = test_function("My.temperature", lower_test_span());
    temperature.inputs.push(record_param("state", "My.State"));
    temperature.outputs.push(function_param("T"));
    temperature.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("T"),
        value: rumoca_core::Expression::FieldAccess {
            base: Box::new(var("state")),
            field: "T".to_string(),
            span: lower_test_span(),
        },
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(temperature.name.clone(), temperature);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.temperature",
        )),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.makeState",
            )),
            args: vec![var("p"), var("temp")],
            is_constructor: false,
            span: lower_test_span(),
        }],
        is_constructor: false,
        span: lower_test_span(),
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("record-valued function actual should bind record input components");
    let mut y = vec![0.0; layout.y_scalars()];
    let p = vec![];
    set_y_value(&layout, &mut y, "p", 101325.0);
    set_y_value(&layout, &mut y, "temp", 350.0);

    let (regs, _output) = eval_linear_ops(&lowered.ops, &y, &p, 0.0);
    let compiled = read_reg(&regs, lowered.result);
    assert!((compiled - 350.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_binds_record_function_result_to_flattened_record_inputs() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("p"), scalar_var("p"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("temp"), scalar_var("temp"));

    let mut state_ctor = test_function("My.State", lower_test_span());
    state_ctor.is_constructor = true;
    state_ctor.inputs.push(function_param("p"));
    state_ctor.inputs.push(function_param("T"));
    state_ctor.outputs.push(record_param("state", "My.State"));
    dae_model
        .symbols
        .functions
        .insert(state_ctor.name.clone(), state_ctor);

    let mut make_state = test_function("My.makeState", lower_test_span());
    make_state.inputs.push(function_param("p"));
    make_state.inputs.push(function_param("T"));
    make_state.outputs.push(record_param("state", "My.State"));
    make_state.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("state"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.State",
            )),
            args: vec![var("p"), var("T")],
            is_constructor: true,
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(make_state.name.clone(), make_state);

    let mut enthalpy = test_function("My.specificEnthalpy", lower_test_span());
    enthalpy.inputs.push(function_param("state_p"));
    enthalpy.inputs.push(function_param("state_T"));
    enthalpy.outputs.push(function_param("h"));
    enthalpy.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("h"),
        value: var("state_T"),
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(enthalpy.name.clone(), enthalpy);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
            "My.specificEnthalpy",
        )),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.makeState",
            )),
            args: vec![var("p"), var("temp")],
            is_constructor: false,
            span,
        }],
        is_constructor: false,
        span,
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("record-valued actual should bind flattened record inputs");
    let mut y = vec![0.0; layout.y_scalars()];
    let p = vec![];
    set_y_value(&layout, &mut y, "p", 101325.0);
    set_y_value(&layout, &mut y, "temp", 380.0);

    let (regs, _output) = eval_linear_ops(&lowered.ops, &y, &p, 0.0);
    let compiled = read_reg(&regs, lowered.result);
    assert!((compiled - 380.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_binds_same_named_local_record_actual_to_record_input() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));

    let mut make_local = test_function("My.makeLocal", span);
    make_local.inputs.push(function_param("u"));
    make_local
        .outputs
        .push(record_param("local", "My.LocalRecord"));
    make_local.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("local.d"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var("u")),
            rhs: Box::new(real_lit(10.0)),
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(make_local.name.clone(), make_local);

    let mut use_local = test_function("My.useLocal", span);
    use_local.inputs.push(record_param("f", "My.LocalRecord"));
    use_local.outputs.push(record_param("aux", "My.AuxRecord"));
    use_local.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("aux.rho"),
        value: rumoca_core::Expression::FieldAccess {
            base: Box::new(var("f")),
            field: "d".to_string(),
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(use_local.name.clone(), use_local);

    let mut build_aux = test_function("My.buildAux", span);
    build_aux.inputs.push(function_param("u"));
    build_aux.outputs.push(record_param("aux", "My.AuxRecord"));
    build_aux.locals.push(record_param("f", "My.LocalRecord"));
    build_aux.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("f"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.makeLocal",
            )),
            args: vec![var("u")],
            is_constructor: false,
            span,
        },
        span,
    });
    build_aux.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("aux"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.useLocal",
            )),
            args: vec![var("f")],
            is_constructor: false,
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(build_aux.name.clone(), build_aux);

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.buildAux",
            )),
            args: vec![var("u")],
            is_constructor: false,
            span,
        }),
        field: "rho".to_string(),
        span,
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("same-named record actual should bind components into callee input scope");
    let mut y = vec![0.0; layout.y_scalars()];
    let p = vec![];
    set_y_value(&layout, &mut y, "u", 5.0);

    let (regs, _output) = eval_linear_ops(&lowered.ops, &y, &p, 0.0);
    let compiled = read_reg(&regs, lowered.result);
    assert!((compiled - 15.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_projects_record_field_from_function_result() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("p"), scalar_var("p"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("temp"), scalar_var("temp"));

    let mut state_ctor = test_function("My.State", lower_test_span());
    state_ctor.is_constructor = true;
    state_ctor.inputs.push(function_param("p"));
    state_ctor.inputs.push(function_param("T"));
    state_ctor.outputs.push(record_param("state", "My.State"));
    dae_model
        .symbols
        .functions
        .insert(state_ctor.name.clone(), state_ctor);

    let mut make_state = test_function("My.makeState", lower_test_span());
    make_state.inputs.push(function_param("p"));
    make_state.inputs.push(function_param("T"));
    make_state.outputs.push(record_param("state", "My.State"));
    make_state.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("state"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.State",
            )),
            args: vec![var("p"), var("T")],
            is_constructor: true,
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(make_state.name.clone(), make_state);

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.makeState",
            )),
            args: vec![var("p"), var("temp")],
            is_constructor: false,
            span,
        }),
        field: "T".to_string(),
        span,
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("record-valued function field projection should lower");
    let mut y = vec![0.0; layout.y_scalars()];
    let p = vec![];
    set_y_value(&layout, &mut y, "p", 101325.0);
    set_y_value(&layout, &mut y, "temp", 360.0);

    let (regs, _output) = eval_linear_ops(&lowered.ops, &y, &p, 0.0);
    let compiled = read_reg(&regs, lowered.result);
    assert!((compiled - 360.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_projects_single_output_function_by_output_name() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));

    let mut temperature = test_function("My.temperature", lower_test_span());
    temperature.inputs.push(function_param("u"));
    temperature.outputs.push(function_param("T"));
    temperature.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("T"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var("u")),
            rhs: Box::new(real_lit(10.0)),
            span: lower_test_span(),
        },
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(temperature.name.clone(), temperature);

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.temperature",
            )),
            args: vec![var("u")],
            is_constructor: false,
            span: lower_test_span(),
        }),
        field: "T".to_string(),
        span: lower_test_span(),
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("single-output function output-name projection should lower");
    let mut y = vec![0.0; layout.y_scalars()];
    let p = vec![];
    set_y_value(&layout, &mut y, "u", 273.15);

    let (regs, _output) = eval_linear_ops(&lowered.ops, &y, &p, 0.0);
    let compiled = read_reg(&regs, lowered.result);
    assert!((compiled - 283.15).abs() <= 1e-12);
}

#[test]
fn lower_expression_projects_multi_output_scalar_inside_binary() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));

    let mut pair = test_function("My.pair", lower_test_span());
    pair.inputs.push(function_param("u"));
    pair.outputs.push(function_param("first"));
    pair.outputs.push(function_param("second"));
    pair.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("first"),
        value: real_lit(0.0),
        span: lower_test_span(),
    });
    pair.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("second"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("u")),
            rhs: Box::new(real_lit(1.0)),
            span: lower_test_span(),
        },
        span: lower_test_span(),
    });
    pair.body.push(rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Gt,
                lhs: Box::new(var("u")),
                rhs: Box::new(real_lit(0.0)),
                span: lower_test_span(),
            },
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: component_ref("second"),
                value: rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Add,
                    lhs: Box::new(var("u")),
                    rhs: Box::new(real_lit(1.0)),
                    span: lower_test_span(),
                },
                span: lower_test_span(),
            }],
        }],
        else_block: Some(vec![rumoca_core::Statement::Assignment {
            comp: component_ref("second"),
            value: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("u")),
                rhs: Box::new(real_lit(2.0)),
                span: lower_test_span(),
            },
            span: lower_test_span(),
        }]),
        span: lower_test_span(),
    });
    dae_model.symbols.functions.insert(pair.name.clone(), pair);

    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.pair.second",
            )),
            args: vec![var("u")],
            is_constructor: false,
            span: lower_test_span(),
        }),
        rhs: Box::new(real_lit(10.0)),
        span: lower_test_span(),
    };
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("projected multi-output scalar should lower inside binary expressions");

    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "u", 1.0);
    let (regs, _output) = eval_linear_ops(&lowered.ops, &y, &[], 0.0);
    let compiled = read_reg(&regs, lowered.result);
    assert!(
        (compiled - 20.0).abs() <= 1e-12,
        "compiled projected output was {compiled}"
    );
}

#[test]
fn projected_function_output_reads_array_assigned_in_if_branches() {
    fn indexed(name: &str, index: i64) -> rumoca_core::Expression {
        let span = lower_test_span();
        rumoca_core::Expression::Index {
            base: Box::new(source_var(name)),
            subscripts: vec![rumoca_core::Subscript::Index { value: index, span }],
            span,
        }
    }

    let span = lower_test_span();
    let mut dae_model = dae::Dae::default();
    let mut from_quat = test_function("My.fromQuat", span);
    from_quat.inputs.push(function_param_with_dims("q", &[4]));
    from_quat.outputs.push(function_param_with_dims("r", &[3]));
    from_quat.locals.push(function_param_with_dims("q_n", &[4]));
    from_quat.locals.push(function_param("den"));
    from_quat.body.push(rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Lt,
                lhs: Box::new(indexed("q", 1)),
                rhs: Box::new(real_lit(0.0)),
                span,
            },
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: component_ref("q_n"),
                value: rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Minus,
                    rhs: Box::new(source_var("q")),
                    span,
                },
                span,
            }],
        }],
        else_block: Some(vec![rumoca_core::Statement::Assignment {
            comp: component_ref("q_n"),
            value: source_var("q"),
            span,
        }]),
        span,
    });
    from_quat.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("den"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(real_lit(1.0)),
            rhs: Box::new(indexed("q_n", 1)),
            span,
        },
        span,
    });
    from_quat.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref_index("r", 1),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Div,
            lhs: Box::new(indexed("q_n", 2)),
            rhs: Box::new(source_var("den")),
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(from_quat.name.clone(), from_quat);
    let expression = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::generated_component_reference(test_component_ref_from_name(
            "My.fromQuat.r[1]",
        )),
        args: vec![array_lit(&[1.0, 2.0, 3.0, 4.0])],
        is_constructor: false,
        span,
    };
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");

    let lowered = lower_expression(&expression, &layout, &dae_model.symbols.functions)
        .expect("projected function output should retain branch-assigned local array elements");
    let (regs, _output) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert!((read_reg(&regs, lowered.result) - 1.0).abs() <= 1e-12);
}

#[test]
fn function_reads_initialized_array_element_reassigned_in_runtime_if() {
    let span = lower_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));

    let mut choose = test_function("My.chooseArrayElement", span);
    choose.inputs.push(function_param("u"));
    choose.outputs.push(function_param("out"));
    choose
        .locals
        .push(function_param_with_dims("values", &[1]).with_default(
            rumoca_core::Expression::Array {
                elements: vec![real_lit(0.0)],
                is_matrix: false,
                span,
            },
        ));
    choose.body.push(rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Gt,
                lhs: Box::new(source_var("u")),
                rhs: Box::new(real_lit(0.0)),
                span,
            },
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: component_ref_index("values", 1),
                value: real_lit(2.0),
                span,
            }],
        }],
        else_block: Some(vec![rumoca_core::Statement::Assignment {
            comp: component_ref_index("values", 1),
            value: real_lit(3.0),
            span,
        }]),
        span,
    });
    choose.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: var_index("values", 1),
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(choose.name.clone(), choose);

    let expression = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::generated("My.chooseArrayElement"),
        args: vec![source_var("u")],
        is_constructor: false,
        span,
    };
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expression, &layout, &dae_model.symbols.functions)
        .expect("runtime branch array assignment should lower");

    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "u", 1.0);
    let (regs, _) = eval_linear_ops(&lowered.ops, &y, &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 2.0).abs() <= 1e-12);
}

#[test]
fn runtime_if_assignment_invalidates_stale_constant_index() {
    let span = lower_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));

    let mut choose = test_function("My.chooseDynamicIndex", span);
    choose.inputs.push(function_param("u"));
    choose.outputs.push(function_param("out"));
    choose.locals.push(
        rumoca_core::FunctionParam::new("selected", "Integer", span).with_default(int_lit(0)),
    );
    choose
        .locals
        .push(function_param_with_dims("values", &[2]).with_default(
            rumoca_core::Expression::Array {
                elements: vec![real_lit(5.0), real_lit(7.0)],
                is_matrix: false,
                span,
            },
        ));
    choose.body.push(rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Gt,
                lhs: Box::new(source_var("u")),
                rhs: Box::new(real_lit(0.0)),
                span,
            },
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: component_ref("selected"),
                value: int_lit(1),
                span,
            }],
        }],
        else_block: Some(vec![rumoca_core::Statement::Assignment {
            comp: component_ref("selected"),
            value: int_lit(2),
            span,
        }]),
        span,
    });
    choose.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: var_index_expr("values", source_var("selected")),
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(choose.name.clone(), choose);

    let expression = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::generated("My.chooseDynamicIndex"),
        args: vec![source_var("u")],
        is_constructor: false,
        span,
    };
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expression, &layout, &dae_model.symbols.functions)
        .expect("runtime branch assignment should invalidate the initial constant index");

    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "u", 1.0);
    let (regs, _) = eval_linear_ops(&lowered.ops, &y, &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 5.0).abs() <= 1e-12);
}

#[test]
fn function_selects_runtime_local_matrix_row_slice() {
    let span = lower_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("row"), scalar_var("row"));

    let matrix_row = rumoca_core::Expression::Index {
        base: Box::new(source_var("matrix")),
        subscripts: vec![
            rumoca_core::Subscript::generated_expr(Box::new(source_var("row")), span),
            rumoca_core::Subscript::generated_colon(span),
        ],
        span,
    };
    let mut row_sum = test_function("My.runtimeRowSum", span);
    row_sum.inputs.push(function_param("row"));
    row_sum.outputs.push(function_param("out"));
    row_sum
        .locals
        .push(function_param_with_dims("matrix", &[2, 2]).with_default(
            rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Array {
                        elements: vec![real_lit(1.0), real_lit(2.0)],
                        is_matrix: false,
                        span,
                    },
                    rumoca_core::Expression::Array {
                        elements: vec![real_lit(3.0), real_lit(4.0)],
                        is_matrix: false,
                        span,
                    },
                ],
                is_matrix: true,
                span,
            },
        ));
    row_sum.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sum,
            args: vec![matrix_row],
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(row_sum.name.clone(), row_sum);

    let expression = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::generated("My.runtimeRowSum"),
        args: vec![source_var("row")],
        is_constructor: false,
        span,
    };
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expression, &layout, &dae_model.symbols.functions)
        .expect("runtime local matrix row slice should lower through dynamic selection");

    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "row", 2.0);
    let (regs, _) = eval_linear_ops(&lowered.ops, &y, &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 7.0).abs() <= 1e-12);
}

#[test]
fn function_assigns_runtime_local_matrix_row_slice() {
    let span = lower_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("row"), scalar_var("row"));

    let matrix_literal = rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Array {
                elements: vec![real_lit(1.0), real_lit(2.0)],
                is_matrix: false,
                span,
            },
            rumoca_core::Expression::Array {
                elements: vec![real_lit(3.0), real_lit(4.0)],
                is_matrix: false,
                span,
            },
        ],
        is_matrix: true,
        span,
    };
    let mut target = source_component_ref_from_name("matrix");
    target.parts.last_mut().unwrap().subs = vec![
        rumoca_core::Subscript::generated_expr(Box::new(source_var("row")), span),
        rumoca_core::Subscript::generated_colon(span),
    ];
    let second_row = rumoca_core::Expression::Index {
        base: Box::new(source_var("matrix")),
        subscripts: vec![
            rumoca_core::Subscript::generated_index(2, span),
            rumoca_core::Subscript::generated_colon(span),
        ],
        span,
    };

    let mut row_update = test_function("My.runtimeRowUpdate", span);
    row_update.inputs.push(function_param("row"));
    row_update.outputs.push(function_param("out"));
    row_update
        .locals
        .push(function_param_with_dims("matrix", &[2, 2]).with_default(matrix_literal));
    row_update.body.push(rumoca_core::Statement::Assignment {
        comp: target,
        value: rumoca_core::Expression::Array {
            elements: vec![real_lit(9.0), real_lit(8.0)],
            is_matrix: false,
            span,
        },
        span,
    });
    row_update.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sum,
            args: vec![second_row],
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(row_update.name.clone(), row_update);

    let expression = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::generated("My.runtimeRowUpdate"),
        args: vec![source_var("row")],
        is_constructor: false,
        span,
    };
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expression, &layout, &dae_model.symbols.functions)
        .expect("runtime local matrix row slice assignment should lower");

    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "row", 2.0);
    let (regs, _) = eval_linear_ops(&lowered.ops, &y, &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 17.0).abs() <= 1e-12);
}

#[test]
fn function_output_shape_uses_scalar_actual_for_callee_input_name() {
    let span = lower_test_span();
    let mut dae_model = dae::Dae::default();

    let mut square = test_function("My.dynamicSquare", span);
    square
        .inputs
        .push(rumoca_core::FunctionParam::new("n", "Integer", span));
    let mut output = function_param_with_dims("matrix", &[0, 0]);
    output.shape_expr = vec![
        rumoca_core::Subscript::generated_expr(Box::new(source_var("n")), span),
        rumoca_core::Subscript::generated_expr(Box::new(source_var("n")), span),
    ];
    square.outputs.push(output);
    square.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("matrix"),
        value: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Zeros,
            args: vec![source_var("n"), source_var("n")],
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(square.name.clone(), square);

    let mut wrapper = test_function("My.squareSum", span);
    wrapper.outputs.push(function_param("out"));
    wrapper.locals.push(
        rumoca_core::FunctionParam::new("dimension", "Integer", span).with_default(int_lit(2)),
    );
    wrapper.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("out"),
        value: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sum,
            args: vec![rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::generated("My.dynamicSquare"),
                args: vec![source_var("dimension")],
                is_constructor: false,
                span,
            }],
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(wrapper.name.clone(), wrapper);

    let expression = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::generated("My.squareSum"),
        args: Vec::new(),
        is_constructor: false,
        span,
    };
    let lowered = lower_expression(
        &expression,
        &VarLayout::default(),
        &dae_model.symbols.functions,
    )
    .expect("callee output shape should bind `n` to caller actual `dimension`");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!(read_reg(&regs, lowered.result).abs() <= 1e-12);
}

#[test]
fn return_guard_preserves_prior_multidimensional_output_assignment() {
    let span = lower_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));

    let matrix = |values: [f64; 4]| rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Array {
                elements: vec![real_lit(values[0]), real_lit(values[1])],
                is_matrix: false,
                span,
            },
            rumoca_core::Expression::Array {
                elements: vec![real_lit(values[2]), real_lit(values[3])],
                is_matrix: false,
                span,
            },
        ],
        is_matrix: true,
        span,
    };
    let mut choose = test_function("My.matrixReturn", span);
    choose.inputs.push(function_param("u"));
    choose
        .outputs
        .push(function_param_with_dims("matrix", &[2, 2]));
    choose.body.push(rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Gt,
                lhs: Box::new(source_var("u")),
                rhs: Box::new(real_lit(0.0)),
                span,
            },
            stmts: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref("matrix"),
                    value: matrix([1.0, 2.0, 3.0, 4.0]),
                    span,
                },
                rumoca_core::Statement::Return { span },
            ],
        }],
        else_block: None,
        span,
    });
    choose.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("matrix"),
        value: matrix([5.0, 6.0, 7.0, 8.0]),
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(choose.name.clone(), choose);

    let expression = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::generated("My.matrixReturn"),
            args: vec![source_var("u")],
            is_constructor: false,
            span,
        }),
        subscripts: vec![
            rumoca_core::Subscript::generated_index(1, span),
            rumoca_core::Subscript::generated_index(1, span),
        ],
        span,
    };
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expression, &layout, &dae_model.symbols.functions)
        .expect("return guard should preserve a prior matrix output assignment by matrix indices");

    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "u", 1.0);
    let (regs, _) = eval_linear_ops(&lowered.ops, &y, &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 1.0).abs() <= 1e-12);
}

#[test]
fn lower_expression_projects_record_field_from_forwarded_function_result() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("p"), scalar_var("p"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("temp"), scalar_var("temp"));

    let mut state_ctor = test_function("My.State", lower_test_span());
    state_ctor.is_constructor = true;
    state_ctor.inputs.push(function_param("p"));
    state_ctor.inputs.push(function_param("T"));
    state_ctor.outputs.push(record_param("state", "My.State"));
    dae_model
        .symbols
        .functions
        .insert(state_ctor.name.clone(), state_ctor);

    let mut make_state = test_function("My.makeState", lower_test_span());
    make_state.inputs.push(function_param("p"));
    make_state.inputs.push(function_param("T"));
    make_state.outputs.push(record_param("state", "My.State"));
    make_state.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("state"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.State",
            )),
            args: vec![var("p"), var("T")],
            is_constructor: true,
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(make_state.name.clone(), make_state);

    let mut forward_state = test_function("My.forwardState", lower_test_span());
    forward_state.inputs.push(function_param("p"));
    forward_state.inputs.push(function_param("T"));
    forward_state
        .outputs
        .push(record_param("state", "My.State"));
    forward_state.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("state"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.makeState",
            )),
            args: vec![var("p"), var("T")],
            is_constructor: false,
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(forward_state.name.clone(), forward_state);

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(
                "My.forwardState",
            )),
            args: vec![var("p"), var("temp")],
            is_constructor: false,
            span,
        }),
        field: "T".to_string(),
        span,
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("forwarded record-valued function field projection should lower");
    let mut y = vec![0.0; layout.y_scalars()];
    let p = vec![];
    set_y_value(&layout, &mut y, "p", 101325.0);
    set_y_value(&layout, &mut y, "temp", 370.0);

    let (regs, _output) = eval_linear_ops(&lowered.ops, &y, &p, 0.0);
    let compiled = read_reg(&regs, lowered.result);
    assert!((compiled - 370.0).abs() <= 1e-12);
}

#[test]
fn record_function_output_field_preserves_declared_matrix_shape() {
    let span = lower_test_span();
    let mut dae_model = dae::Dae::default();

    let mut state_ctor = test_function("My.MatrixState", span);
    state_ctor.is_constructor = true;
    state_ctor
        .inputs
        .push(function_param_with_dims("covariance", &[2, 2]));
    state_ctor
        .outputs
        .push(record_param("state", "My.MatrixState"));
    dae_model
        .symbols
        .functions
        .insert(state_ctor.name.clone(), state_ctor);

    let mut make_state = test_function("My.makeMatrixState", span);
    make_state
        .outputs
        .push(record_param("state", "My.MatrixState"));
    make_state.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("state.covariance"),
        value: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Zeros,
            args: vec![int_lit(2), int_lit(2)],
            span,
        },
        span,
    });
    make_state.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref_indices("state.covariance", &[1, 1]),
        value: real_lit(2.0),
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(make_state.name.clone(), make_state);

    let expression = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::generated("My.makeMatrixState"),
                args: Vec::new(),
                is_constructor: false,
                span,
            }),
            field: "covariance".to_string(),
            span,
        }),
        subscripts: vec![
            rumoca_core::Subscript::generated_index(1, span),
            rumoca_core::Subscript::generated_index(1, span),
        ],
        span,
    };
    let lowered = lower_expression(
        &expression,
        &VarLayout::default(),
        &dae_model.symbols.functions,
    )
    .expect("record output matrix field should retain its constructor-declared shape");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 2.0).abs() <= 1e-12);
}

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
