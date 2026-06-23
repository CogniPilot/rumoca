use super::*;
use crate::LowerError;
use crate::lower::expression_rows;

fn discrete_array_source_span(source: u64, start: usize, end: usize) -> rumoca_core::Span {
    let source_name = format!(
        "phase_solve_lower_tests_array_operator_tests_discrete_array_tests_source_{source}.mo"
    );
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(&source_name),
        start,
        end,
    )
}

#[test]
fn lower_discrete_rhs_lowers_function_matrix_output_for_matrix_vector_product() {
    let mut dae_model = dae::Dae::default();
    let mut matrix_fn = rumoca_core::Function::new("Pkg.matrix", lower_test_span());
    matrix_fn
        .outputs
        .push(function_param_with_dims("R", &[2, 2]));
    for (row, col, value) in [(1, 1, 1.0), (1, 2, 2.0), (2, 1, 3.0), (2, 2, 4.0)] {
        matrix_fn.body.push(rumoca_core::Statement::Assignment {
            comp: matrix_component_ref("R", row, col),
            value: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                span: lower_test_span(),
            },

            span: lower_test_span(),
        });
    }
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("Pkg.matrix"), matrix_fn);
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("v"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("v")
        },
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("y")
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        // MLS §12.4.5 and §10.6.5: a function may return an array output,
        // and that returned matrix participates in matrix-vector products.
        rhs: mul(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Pkg.matrix").into(),
                args: vec![],
                is_constructor: false,
                span: lower_test_span(),
            },
            var("v"),
        ),
        span: lower_test_span(),
        origin: "function matrix-vector assignment".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("function matrix output should lower as array values");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "v[1]", 10.0);
    set_p_value(&layout, &mut p, "v[2]", 100.0);

    let outputs = eval_programs_all_outputs(&rows, &[], &p, 0.0);

    assert_eq!(outputs[0], 210.0);
    assert_eq!(outputs[1], 430.0);
}

#[test]
fn lower_expression_lowers_projected_function_matrix_output_component() {
    let mut functions = IndexMap::new();
    let span = lower_test_span();
    let mut matrix_fn = rumoca_core::Function::new("Pkg.matrix", span);
    matrix_fn
        .outputs
        .push(function_param_with_dims("R", &[2, 2]));
    for (row, col, value) in [(1, 1, 1.0), (1, 2, 2.0), (2, 1, 3.0), (2, 2, 4.0)] {
        matrix_fn.body.push(rumoca_core::Statement::Assignment {
            comp: matrix_component_ref("R", row, col),
            value: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                span,
            },

            span,
        });
    }
    functions.insert(matrix_fn.name.clone(), matrix_fn);

    let expr = rumoca_core::Expression::FunctionCall {
        // MLS §12.4.5: projected calls to array-valued function outputs select
        // the declared array component, not a flattened pseudo-variable.
        name: rumoca_core::VarName::new("Pkg.matrix.R[2,2]").into(),
        args: vec![],
        is_constructor: false,
        span,
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("projected matrix output should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 4.0);
}

#[test]
fn lower_residual_scalarizes_function_returned_record_fields() {
    let span = discrete_array_source_span(44, 10, 20);
    let dae_model = function_returned_record_fields_dae(span);

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout)
        .expect("function-returned record fields should scalarize through field projection");
    assert_eq!(rows.len(), 12);
    let y = vec![0.0; layout.y_scalars()];
    let p = vec![0.0; layout.p_scalars()];
    let actual = rows
        .iter()
        .map(|row| eval_linear_ops(row, &y, &p, 0.0).1.expect("row output"))
        .collect::<Vec<_>>();
    assert_eq!(
        actual,
        vec![
            -1.0, 0.0, 0.0, 0.0, -1.0, 0.0, 0.0, 0.0, -1.0, 0.0, 0.0, 0.0
        ]
    );
}

fn function_returned_record_fields_dae(span: rumoca_core::Span) -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    let orientation = orientation_constructor_for_record_fields(span);
    dae_model
        .symbols
        .functions
        .insert(orientation.name.clone(), orientation);
    let null_rotation = null_rotation_record_function(span);
    dae_model
        .symbols
        .functions
        .insert(null_rotation.name.clone(), null_rotation);
    for (name, dims) in [("frame.R.T", vec![3, 3]), ("frame.R.w", vec![3])] {
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new(name),
            dae::Variable {
                dims,
                ..scalar_var(name)
            },
        );
    }
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: binary(
            rumoca_core::OpBinary::Sub,
            source_var("frame.R"),
            function_call_with_span("Pkg.nullRotation", Vec::new(), false, span),
        ),
        span,
        origin: "function returned record residual".to_string(),
        scalar_count: 12,
    });
    dae_model
}

fn orientation_constructor_for_record_fields(span: rumoca_core::Span) -> rumoca_core::Function {
    let mut orientation = rumoca_core::Function::new("Pkg.Orientation", span);
    orientation.is_constructor = true;
    orientation
        .inputs
        .push(function_param_with_type_span("T", "Real", &[3, 3], span));
    orientation
        .inputs
        .push(function_param_with_type_span("w", "Real", &[3], span));
    orientation
}

fn null_rotation_record_function(span: rumoca_core::Span) -> rumoca_core::Function {
    let mut null_rotation = rumoca_core::Function::new("Pkg.nullRotation", span);
    let mut output = function_param_with_type_span("R", "Pkg.Orientation", &[], span);
    output.type_class = Some(rumoca_core::ClassType::Record);
    null_rotation.outputs.push(output);
    null_rotation.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref_with_span("R", span),
        value: null_rotation_orientation_call(span),
        span,
    });
    null_rotation
}

fn null_rotation_orientation_call(span: rumoca_core::Span) -> rumoca_core::Expression {
    function_call_with_span(
        "Pkg.Orientation",
        vec![
            named_arg_call("__rumoca_named_arg__.T", identity_3_expr(span), span),
            named_arg_call("__rumoca_named_arg__.w", zeros_3_expr(span), span),
        ],
        true,
        span,
    )
}

fn function_param_with_type_span(
    name: &str,
    type_name: &str,
    dims: &[i64],
    span: rumoca_core::Span,
) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        def_id: None,
        name: name.to_string(),
        span,
        type_name: type_name.to_string(),
        type_class: None,
        dims: dims.to_vec(),
        shape_expr: Vec::new(),
        default: None,
        description: None,
    }
}

fn component_ref_with_span(name: &str, span: rumoca_core::Span) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span,
            subs: Vec::new(),
        }],
        def_id: None,
    }
}

fn function_call_with_span(
    name: &str,
    args: Vec<rumoca_core::Expression>,
    is_constructor: bool,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor,
        span,
    }
}

fn named_arg_call(
    name: &str,
    value: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    function_call_with_span(name, vec![value], true, span)
}

fn identity_3_expr(span: rumoca_core::Span) -> rumoca_core::Expression {
    builtin_with_span(
        rumoca_core::BuiltinFunction::Identity,
        vec![int_lit_with_span(3, span)],
        span,
    )
}

fn zeros_3_expr(span: rumoca_core::Span) -> rumoca_core::Expression {
    builtin_with_span(
        rumoca_core::BuiltinFunction::Zeros,
        vec![int_lit_with_span(3, span)],
        span,
    )
}

fn int_lit_with_span(value: i64, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span,
    }
}

fn builtin_with_span(
    function: rumoca_core::BuiltinFunction,
    args: Vec<rumoca_core::Expression>,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function,
        args,
        span,
    }
}

#[test]
fn lower_residual_preserves_function_record_matrix_field_shape() {
    let span = discrete_array_source_span(44, 30, 90);
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("frame.R.T"),
        dae::Variable {
            dims: vec![3, 3],
            ..scalar_var("frame.R.T")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("Q"),
        dae::Variable {
            dims: vec![4],
            ..scalar_var("Q")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("w"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("w")
        },
    );

    let mut orientation = rumoca_core::Function::new("Pkg.Orientation", span);
    orientation.is_constructor = true;
    orientation
        .inputs
        .push(function_param_with_type_span("T", "Real", &[3, 3], span));
    orientation
        .inputs
        .push(function_param_with_type_span("w", "Real", &[3], span));
    dae_model
        .symbols
        .functions
        .insert(orientation.name.clone(), orientation);

    let mut from_q = rumoca_core::Function::new("Pkg.from_Q", span);
    from_q
        .inputs
        .push(function_param_with_type_span("Q", "Real", &[4], span));
    from_q
        .inputs
        .push(function_param_with_type_span("w", "Real", &[3], span));
    from_q.outputs.push(
        function_param_with_type_span("R", "Pkg.Orientation", &[], span)
            .with_type_class(rumoca_core::ClassType::Record),
    );
    from_q.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("R"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Pkg.Orientation").into(),
            args: vec![
                builtin(rumoca_core::BuiltinFunction::Identity, vec![int_lit(3)]),
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("__rumoca_named_arg__.w").into(),
                    args: vec![var("w")],
                    is_constructor: true,
                    span,
                },
            ],
            is_constructor: false,
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(from_q.name.clone(), from_q);

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("frame.R.T"),
            field_access(
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Pkg.from_Q").into(),
                    args: vec![var("Q"), var("w")],
                    is_constructor: false,
                    span,
                },
                "T",
            ),
        ),
        span,
        origin: "matrix field residual".to_string(),
        scalar_count: 9,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout)
        .expect("record function matrix field should lower to every scalar row");
    assert_eq!(rows.len(), 9);

    let y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "Q[4]", 1.0);
    let actual = rows
        .iter()
        .map(|row| eval_linear_ops(row, &y, &p, 0.0).1.expect("row output"))
        .collect::<Vec<_>>();
    assert_eq!(actual, vec![-1.0, 0.0, 0.0, 0.0, -1.0, 0.0, 0.0, 0.0, -1.0]);
}

#[test]
fn lower_residual_rejects_function_record_matrix_field_shape_mismatch() {
    let span = discrete_array_source_span(44, 100, 140);
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("frame.R.T"),
        dae::Variable {
            dims: vec![3, 3],
            ..scalar_var("frame.R.T")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("w"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("w")
        },
    );

    let mut orientation = rumoca_core::Function::new("Pkg.Orientation", span);
    orientation.is_constructor = true;
    orientation
        .inputs
        .push(function_param_with_type_span("T", "Real", &[3, 3], span));
    orientation
        .inputs
        .push(function_param_with_type_span("w", "Real", &[3], span));
    dae_model
        .symbols
        .functions
        .insert(orientation.name.clone(), orientation);

    let mut bad_rotation = rumoca_core::Function::new("Pkg.badRotation", span);
    bad_rotation.outputs.push(
        function_param_with_type_span("R", "Pkg.Orientation", &[], span)
            .with_type_class(rumoca_core::ClassType::Record),
    );
    bad_rotation.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("R"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Pkg.Orientation").into(),
            args: vec![
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("__rumoca_named_arg__.T").into(),
                    args: vec![var("w")],
                    is_constructor: true,
                    span,
                },
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("__rumoca_named_arg__.w").into(),
                    args: vec![var("w")],
                    is_constructor: true,
                    span,
                },
            ],
            is_constructor: false,
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(bad_rotation.name.clone(), bad_rotation);

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("frame.R.T"),
            field_access(
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Pkg.badRotation").into(),
                    args: vec![],
                    is_constructor: false,
                    span,
                },
                "T",
            ),
        ),
        span,
        origin: "bad matrix field residual".to_string(),
        scalar_count: 9,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let err = lower_residual(&dae_model, &layout)
        .expect_err("record matrix field width mismatch should fail before solve IR exists");
    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string()
            .contains("array expression shape [3, 3] requires 9 scalar values, got 3"),
        "unexpected error: {err}"
    );
}

#[test]
fn lower_expression_lowers_structural_singleton_subscript_as_scalar_value() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("n"),
        dae::Variable {
            name: rumoca_core::VarName::new("n"),
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
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
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("u_buffer"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("u_buffer")
        },
    );

    let selected = rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new("u_buffer").into(),
        subscripts: vec![rumoca_core::Subscript::generated_expr(
            Box::new(add(
                var("n"),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: lower_test_span(),
                },
            )),
            lower_test_span(),
        )],
        span: lower_test_span(),
    };
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("shiftSample").into(),
            args: vec![
                selected,
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: lower_test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: lower_test_span(),
                },
            ],
            is_constructor: false,
            span: lower_test_span(),
        },
        span: lower_test_span(),
        // MLS §10.5: scalar array subscripts select one element, even when the
        // index is a structural expression flowing through a clocked intrinsic.
        origin: "structural singleton subscript".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("MLS §10.5 scalar array subscript should lower through clocked intrinsic");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u_buffer[1]", 4.0);
    set_p_value(&layout, &mut p, "u_buffer[2]", 7.0);

    let (_, output) = eval_linear_ops(&rows[0], &[], &p, 0.0);

    assert_eq!(output, Some(7.0));
}

#[test]
fn lower_discrete_rhs_lowers_matrix_matrix_multiply_as_array_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("a"),
        dae::Variable {
            dims: vec![2, 2],
            ..scalar_var("a")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("b"),
        dae::Variable {
            dims: vec![2, 2],
            ..scalar_var("b")
        },
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![2, 2],
            ..scalar_var("y")
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: mul(var("a"), var("b")),
        span: lower_test_span(),
        // MLS §10.6.5: matrix * matrix yields one scalar equation per
        // resulting matrix element when lowered to solve-IR rows.
        origin: "matrix product discrete update".to_string(),
        scalar_count: 4,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("matrix product should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "a[1,1]", 1.0);
    set_p_value(&layout, &mut p, "a[1,2]", 2.0);
    set_p_value(&layout, &mut p, "a[2,1]", 3.0);
    set_p_value(&layout, &mut p, "a[2,2]", 4.0);
    set_p_value(&layout, &mut p, "b[1,1]", 5.0);
    set_p_value(&layout, &mut p, "b[1,2]", 6.0);
    set_p_value(&layout, &mut p, "b[2,1]", 7.0);
    set_p_value(&layout, &mut p, "b[2,2]", 8.0);

    let actual = eval_programs_all_outputs(&rows, &[], &p, 0.0);

    assert_eq!(actual, vec![19.0, 22.0, 43.0, 50.0]);
}

#[test]
fn lower_discrete_rhs_respects_cat_dimension_for_matrix_columns() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![2, 3],
            ..scalar_var("y")
        },
    );
    let int = |value| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span,
    };
    let real = |value| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span,
    };
    let matrix = |rows: Vec<Vec<rumoca_core::Expression>>| rumoca_core::Expression::Array {
        elements: rows
            .into_iter()
            .map(|row| rumoca_core::Expression::Array {
                elements: row,
                is_matrix: false,
                span,
            })
            .collect(),
        is_matrix: true,
        span,
    };
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Cat,
            args: vec![
                int(2),
                matrix(vec![vec![real(1.0), real(2.0)], vec![real(3.0), real(4.0)]]),
                matrix(vec![vec![real(5.0)], vec![real(6.0)]]),
            ],
            span,
        },
        span,
        // MLS §10.4.2.1: cat(2, A, B) concatenates matrix columns, not rows.
        origin: "matrix column cat discrete update".to_string(),
        scalar_count: 6,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows =
        lower_discrete_rhs(&dae_model, &layout).expect("MLS §10.4.2.1 cat dimension should lower");
    let actual = rows
        .iter()
        .map(|row| eval_linear_ops(row, &[], &[], 0.0).1.expect("row output"))
        .collect::<Vec<_>>();

    assert_eq!(actual, vec![1.0, 2.0, 5.0, 3.0, 4.0, 6.0]);
}

#[test]
fn lower_discrete_rhs_preserves_singleton_vector_rank_for_cat() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("X"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("X")
        },
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("y")
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: builtin(
            rumoca_core::BuiltinFunction::Cat,
            vec![
                int_lit(1),
                var("X"),
                rumoca_core::Expression::Array {
                    elements: vec![sub(
                        real_lit(1.0),
                        source_builtin(rumoca_core::BuiltinFunction::Sum, vec![var("X")]),
                    )],
                    is_matrix: false,
                    span,
                },
            ],
        ),
        span,
        origin: "cat with singleton vector tail".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("singleton vector array literal should keep rank for cat");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "X[1]", 0.25);
    set_p_value(&layout, &mut p, "X[2]", 0.50);
    let actual = eval_programs_all_outputs(&rows, &[], &p, 0.0);

    assert_eq!(actual, vec![0.25, 0.50, 0.25]);
}

#[test]
fn lower_discrete_rhs_selects_compile_time_if_array_branch_before_width_check() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("X"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("X")
        },
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("y")
        },
    );
    let full_x = builtin(
        rumoca_core::BuiltinFunction::Cat,
        vec![
            int_lit(1),
            var("X"),
            rumoca_core::Expression::Array {
                elements: vec![sub(
                    real_lit(1.0),
                    source_builtin(rumoca_core::BuiltinFunction::Sum, vec![var("X")]),
                )],
                is_matrix: false,
                span,
            },
        ],
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Eq,
                    lhs: Box::new(builtin(
                        rumoca_core::BuiltinFunction::Size,
                        vec![var("X"), int_lit(1)],
                    )),
                    rhs: Box::new(int_lit(3)),
                    span,
                },
                var("X"),
            )],
            else_branch: Box::new(full_x),
            span,
        },
        span,
        origin: "compile-time array if branch".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("compile-time array condition should select shape-compatible branch");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "X[1]", 0.25);
    set_p_value(&layout, &mut p, "X[2]", 0.50);
    let actual = eval_programs_all_outputs(&rows, &[], &p, 0.0);

    assert_eq!(actual, vec![0.25, 0.50, 0.25]);
}

#[test]
fn lower_discrete_rhs_lowers_scalar_array_multiply_as_broadcast_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("gain"),
        dae::Variable {
            ..scalar_var("gain")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("u")
        },
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("y")
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: mul(var("gain"), var("u")),
        span: lower_test_span(),
        // MLS §10.6.5: scalar * array scales every array element.
        origin: "scalar vector product discrete update".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("scalar array product should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "gain", 3.0);
    set_p_value(&layout, &mut p, "u[1]", 2.0);
    set_p_value(&layout, &mut p, "u[2]", 4.0);

    let actual = eval_programs_all_outputs(&rows, &[], &p, 0.0);

    assert_eq!(actual, vec![6.0, 12.0]);
}

#[test]
fn lower_discrete_rhs_lowers_array_constructor_with_vector_elements_as_matrix() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("phi"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("phi")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("x")
        },
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("y")
        },
    );
    let unary_builtin = |function| rumoca_core::Expression::BuiltinCall {
        function,
        args: vec![var("phi")],
        span: lower_test_span(),
    };
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: mul(
            rumoca_core::Expression::Array {
                elements: vec![
                    unary_builtin(rumoca_core::BuiltinFunction::Cos),
                    unary_builtin(rumoca_core::BuiltinFunction::Sin),
                ],
                is_matrix: false,
                span: lower_test_span(),
            },
            var("x"),
        ),
        span: lower_test_span(),
        // MLS §10.4.1: an array constructor with array-valued elements
        // prefixes the element shape, so {cos(phi), sin(phi)} is 2 x size(phi).
        origin: "array-valued constructor matrix-vector update".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("array-valued constructor should lower as a matrix-vector product");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "phi[1]", 0.0);
    set_p_value(&layout, &mut p, "phi[2]", std::f64::consts::FRAC_PI_2);
    set_p_value(&layout, &mut p, "phi[3]", std::f64::consts::PI);
    set_p_value(&layout, &mut p, "x[1]", 1.0);
    set_p_value(&layout, &mut p, "x[2]", 2.0);
    set_p_value(&layout, &mut p, "x[3]", 3.0);

    let actual = eval_programs_all_outputs(&rows, &[], &p, 0.0);

    assert!((actual[0] + 2.0).abs() < 1e-12, "{actual:?}");
    assert!((actual[1] - 2.0).abs() < 1e-12, "{actual:?}");
}

#[test]
fn lower_discrete_rhs_resolves_single_dynamic_function_local_matrix_dimension() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("x")
        },
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("y")
        },
    );
    let row = |values: [f64; 3]| rumoca_core::Expression::Array {
        elements: values.into_iter().map(real_lit).collect(),
        is_matrix: false,
        span: lower_test_span(),
    };
    let mut matrix_fn = rumoca_core::Function::new("Pkg.dynamicMatrixProduct", lower_test_span());
    matrix_fn.inputs.push(function_param_with_dims("x", &[3]));
    matrix_fn.outputs.push(function_param_with_dims("y", &[2]));
    matrix_fn.locals.push(rumoca_core::FunctionParam {
        def_id: None,
        name: "A".to_string(),
        span: lower_test_span(),
        type_name: "Real".to_string(),
        type_class: None,
        dims: vec![2, 0],
        shape_expr: vec![
            rumoca_core::Subscript::generated_index(2, lower_test_span()),
            rumoca_core::Subscript::colon(lower_test_span()),
        ],
        default: Some(rumoca_core::Expression::Array {
            elements: vec![row([1.0, 2.0, 3.0]), row([4.0, 5.0, 6.0])],
            is_matrix: true,
            span: lower_test_span(),
        }),
        description: None,
    });
    matrix_fn.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: mul(var("A"), var("x")),
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(matrix_fn.name.clone(), matrix_fn);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Pkg.dynamicMatrixProduct").into(),
            args: vec![var("x")],
            is_constructor: false,
            span: lower_test_span(),
        },
        span: lower_test_span(),
        // MLS §10.4.1 and §12.4: function-local array declarations may use
        // dimensions inferred from structural inputs; once values are lowered,
        // a single unknown dimension can be recovered from the value count.
        origin: "dynamic local matrix product".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("single dynamic local matrix dimension should be inferred");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "x[1]", 1.0);
    set_p_value(&layout, &mut p, "x[2]", 1.0);
    set_p_value(&layout, &mut p, "x[3]", 1.0);

    let actual = eval_programs_all_outputs(&rows, &[], &p, 0.0);

    assert_eq!(actual, vec![6.0, 15.0]);
}

#[test]
fn lower_discrete_rhs_uses_assigned_width_for_unknown_function_output_dims() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("x")
        },
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("y")
        },
    );
    let mut copy_fn = rumoca_core::Function::new("Pkg.copy", lower_test_span());
    copy_fn.inputs.push(function_param_with_dims("x", &[3]));
    copy_fn.outputs.push(function_param_with_dims("y", &[0]));
    copy_fn.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: var("x"),
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(copy_fn.name.clone(), copy_fn);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Pkg.copy").into(),
            args: vec![var("x")],
            is_constructor: false,
            span: lower_test_span(),
        },
        span: lower_test_span(),
        origin: "unknown output dims copy".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("unknown-size function output should take assigned width");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "x[1]", 1.0);
    set_p_value(&layout, &mut p, "x[2]", 2.0);
    set_p_value(&layout, &mut p, "x[3]", 3.0);
    let actual = eval_programs_all_outputs(&rows, &[], &p, 0.0);

    assert_eq!(actual, vec![1.0, 2.0, 3.0]);
}

#[test]
fn lower_expression_reduces_min_max_over_array_ir_values() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("u")
        },
    );
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let array_ref = var("u");
    let max_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Max,
        args: vec![array_ref.clone()],
        span,
    };
    let min_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Min,
        args: vec![array_ref],
        span,
    };

    let max_lowered = lower_expression(&max_expr, &layout, &IndexMap::new())
        .expect("array max reduction should lower");
    let min_lowered = lower_expression(&min_expr, &layout, &IndexMap::new())
        .expect("array min reduction should lower");
    let p = vec![0.0, 1.0, 0.0];
    let (regs, _) = eval_linear_ops(&max_lowered.ops, &[], &p, 0.0);
    assert_eq!(read_reg(&regs, max_lowered.result), 1.0);
    let (regs, _) = eval_linear_ops(&min_lowered.ops, &[], &p, 0.0);
    assert_eq!(read_reg(&regs, min_lowered.result), 0.0);
}

#[test]
fn lower_expression_inlines_boolean_vector_helpers_with_array_reductions() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("u")
        },
    );

    let mut any_true = rumoca_core::Function::new("Modelica.Math.BooleanVectors.anyTrue", span);
    any_true.inputs.push(rumoca_core::FunctionParam {
        def_id: None,
        name: "b".to_string(),
        span,
        type_name: "Boolean".to_string(),
        type_class: None,
        dims: vec![0],
        shape_expr: Vec::new(),
        default: None,
        description: None,
    });
    any_true.outputs.push(rumoca_core::FunctionParam {
        def_id: None,
        name: "result".to_string(),
        span,
        type_name: "Boolean".to_string(),
        type_class: None,
        dims: vec![],
        shape_expr: Vec::new(),
        default: None,
        description: None,
    });
    any_true.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref_with_span("result", span),
        value: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Max,
            args: vec![var("b")],
            span,
        },

        span,
    });
    dae_model
        .symbols
        .functions
        .insert(any_true.name.clone(), any_true);

    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Modelica.Math.BooleanVectors.anyTrue").into(),
        args: vec![var("u")],
        is_constructor: false,
        span,
    };
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&call, &layout, &dae_model.symbols.functions)
        .expect("Boolean vector helper should lower through array reductions");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[0.0, 1.0], 0.0);

    // MLS §12.4 function calls preserve the semantics of the inlined function
    // body; MLS §10.3.4 makes max(b) an array reduction, not a scalar read.
    assert_eq!(read_reg(&regs, lowered.result), 1.0);
}

#[test]
fn lower_expression_rows_emits_matmul_node_for_matrix_matrix_multiply() {
    // Verify that a top-level matrix-matrix multiply (A [2×3] * B [3×2]) in an
    // array equation is preserved as ComputeNode::MatMul rather than
    // immediately scalarized, and that scalarization produces correct results.
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("A"),
        dae::Variable {
            dims: vec![2, 3],
            ..scalar_var("A")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("B"),
        dae::Variable {
            dims: vec![3, 2],
            ..scalar_var("B")
        },
    );

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");

    // Equation: C = A * B  (scalar_count = 2*2 = 4)
    let equation = dae::Equation {
        lhs: None,
        rhs: mul(var("A"), var("B")),
        span: lower_test_span(),
        origin: "matrix-matrix multiply".to_string(),
        scalar_count: 4,
    };

    let clock_intervals = IndexMap::new();
    let clock_timings = IndexMap::new();
    let triggered_clock_conditions = Vec::new();
    let variable_starts = IndexMap::new();
    let block = lower_expression_rows_with_mode(
        std::iter::once(&equation),
        &layout,
        &dae_model.symbols.functions,
        expression_rows::RuntimeRowMetadata {
            clock_intervals: &clock_intervals,
            clock_timings: &clock_timings,
            triggered_clock_conditions: &triggered_clock_conditions,
            discrete_valued_names: &dae_model.variables.discrete_valued,
            variable_starts: &variable_starts,
            dae_variables: Some(&dae_model.variables),
            structural_bindings: None,
            guard_target_start_before_first_clock_tick: false,
        },
        false,
    )
    .expect("matrix-matrix multiply should lower to a compute block");

    // Structural check: exactly one MatMul node, not ScalarPrograms.
    assert_eq!(block.nodes.len(), 1, "expected exactly one compute node");
    assert!(
        matches!(
            block.nodes[0],
            ComputeNode::MatMul {
                m: 2,
                k: 3,
                n: 2,
                ..
            }
        ),
        "expected ComputeNode::MatMul {{ m: 2, k: 3, n: 2 }}, got {:?}",
        block.nodes[0]
    );

    // Numerical check via scalarization: C = A * B should match manual product.
    // A = [[1, 2, 3], [4, 5, 6]], B = [[7, 8], [9, 10], [11, 12]]
    // C = [[58, 64], [139, 154]]
    let scalar =
        rumoca_eval_solve::to_scalar_program_block(&block).expect("matmul should scalarize");
    assert_eq!(
        scalar.output_count(),
        4,
        "expected 4 scalar outputs for 2×2 result"
    );

    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "A[1,1]", 1.0);
    set_p_value(&layout, &mut p, "A[1,2]", 2.0);
    set_p_value(&layout, &mut p, "A[1,3]", 3.0);
    set_p_value(&layout, &mut p, "A[2,1]", 4.0);
    set_p_value(&layout, &mut p, "A[2,2]", 5.0);
    set_p_value(&layout, &mut p, "A[2,3]", 6.0);
    set_p_value(&layout, &mut p, "B[1,1]", 7.0);
    set_p_value(&layout, &mut p, "B[1,2]", 8.0);
    set_p_value(&layout, &mut p, "B[2,1]", 9.0);
    set_p_value(&layout, &mut p, "B[2,2]", 10.0);
    set_p_value(&layout, &mut p, "B[3,1]", 11.0);
    set_p_value(&layout, &mut p, "B[3,2]", 12.0);

    let c = eval_programs_all_outputs(&scalar.programs, &[], &p, 0.0);

    assert_eq!(c[0], 58.0, "C[1,1]");
    assert_eq!(c[1], 64.0, "C[1,2]");
    assert_eq!(c[2], 139.0, "C[2,1]");
    assert_eq!(c[3], 154.0, "C[2,2]");
}

#[test]
fn lower_expression_rows_rejects_unspanned_matmul_node() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("A"),
        dae::Variable {
            dims: vec![2, 3],
            ..scalar_var("A")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("B"),
        dae::Variable {
            dims: vec![3, 2],
            ..scalar_var("B")
        },
    );
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let equation = dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(var("A")),
            rhs: Box::new(var("B")),
            span: unspanned_test_span(),
        },
        span: unspanned_test_span(),
        origin: "unspanned matrix multiply".to_string(),
        scalar_count: 4,
    };
    let clock_intervals = IndexMap::new();
    let clock_timings = IndexMap::new();
    let triggered_clock_conditions = Vec::new();
    let variable_starts = IndexMap::new();

    let err = lower_expression_rows_with_mode(
        std::iter::once(&equation),
        &layout,
        &dae_model.symbols.functions,
        expression_rows::RuntimeRowMetadata {
            clock_intervals: &clock_intervals,
            clock_timings: &clock_timings,
            triggered_clock_conditions: &triggered_clock_conditions,
            discrete_valued_names: &dae_model.variables.discrete_valued,
            variable_starts: &variable_starts,
            dae_variables: Some(&dae_model.variables),
            structural_bindings: None,
            guard_target_start_before_first_clock_tick: false,
        },
        false,
    )
    .expect_err("unspanned MatMul row lowering must fail");

    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason()
            .contains("MatMul expression row lowering requires a source span"),
        "unexpected error: {err}"
    );
}

#[test]
fn lower_expression_rows_preserves_vector_matrix_products_as_matmul_nodes() {
    for (lhs_dims, rhs_dims, scalar_count, expected_shape) in [
        (vec![2, 3], vec![3], 2, (2, 3, 1)),
        (vec![3], vec![3, 2], 2, (1, 3, 2)),
        (vec![3], vec![3], 1, (1, 3, 1)),
    ] {
        let mut dae_model = dae::Dae::default();
        dae_model.variables.parameters.insert(
            rumoca_core::VarName::new("A"),
            dae::Variable {
                dims: lhs_dims.clone(),
                ..scalar_var("A")
            },
        );
        dae_model.variables.parameters.insert(
            rumoca_core::VarName::new("B"),
            dae::Variable {
                dims: rhs_dims.clone(),
                ..scalar_var("B")
            },
        );

        let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
        let equation = dae::Equation {
            lhs: None,
            rhs: mul(var("A"), var("B")),
            span: lower_test_span(),
            origin: format!("multiply {:?} by {:?}", lhs_dims, rhs_dims),
            scalar_count,
        };

        let clock_intervals = IndexMap::new();
        let clock_timings = IndexMap::new();
        let triggered_clock_conditions = Vec::new();
        let variable_starts = IndexMap::new();
        let block = lower_expression_rows_with_mode(
            std::iter::once(&equation),
            &layout,
            &dae_model.symbols.functions,
            expression_rows::RuntimeRowMetadata {
                clock_intervals: &clock_intervals,
                clock_timings: &clock_timings,
                triggered_clock_conditions: &triggered_clock_conditions,
                discrete_valued_names: &dae_model.variables.discrete_valued,
                variable_starts: &variable_starts,
                dae_variables: Some(&dae_model.variables),
                structural_bindings: None,
                guard_target_start_before_first_clock_tick: false,
            },
            false,
        )
        .expect("top-level vector/matrix multiply should lower");

        let (expected_m, expected_k, expected_n) = expected_shape;
        assert!(
            matches!(
                block.nodes.as_slice(),
                [ComputeNode::MatMul {
                    m,
                    k,
                    n,
                    ..
                }] if (*m, *k, *n) == (expected_m, expected_k, expected_n)
            ),
            "expected MatMul shape {:?} for {:?} * {:?}, got {:?}",
            expected_shape,
            lhs_dims,
            rhs_dims,
            block.nodes
        );
        assert_eq!(
            rumoca_eval_solve::to_scalar_program_block(&block)
                .expect("multiply block should scalarize")
                .output_count(),
            scalar_count
        );
    }
}

#[test]
fn lower_discrete_rhs_lowers_cross_builtin_as_vector_rows() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    for name in ["a", "b"] {
        dae_model.variables.parameters.insert(
            rumoca_core::VarName::new(name),
            dae::Variable {
                dims: vec![3],
                ..scalar_var(name)
            },
        );
    }
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("y")
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: builtin(
            rumoca_core::BuiltinFunction::Cross,
            vec![var("a"), var("b")],
        ),
        span,
        origin: "cross vector update".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("cross() should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    for (name, value) in [
        ("a[1]", 1.0),
        ("a[2]", 2.0),
        ("a[3]", 3.0),
        ("b[1]", 4.0),
        ("b[2]", 5.0),
        ("b[3]", 6.0),
    ] {
        set_p_value(&layout, &mut p, name, value);
    }

    let actual = eval_programs_all_outputs(&rows, &[], &p, 0.0);

    assert_eq!(actual, vec![-3.0, 6.0, -3.0]);
}

#[test]
fn lower_residual_shape_error_reports_equation_source_span() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("A"),
        dae::Variable {
            dims: vec![3, 3],
            ..scalar_var("A")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("b"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("b")
        },
    );

    let equation_span = discrete_array_source_span(12, 120, 139);
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("A").into()),
        rhs: var("b"),
        span: equation_span,
        origin: "shape mismatch".to_string(),
        scalar_count: 9,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let err = lower_residual(&dae_model, &layout)
        .expect_err("matrix/vector residual mismatch should fail");
    assert_eq!(err.source_span(), Some(equation_span));
    assert!(
        err.to_string()
            .contains("array operands have incompatible shapes [3, 3] and [2]"),
        "unexpected error: {err}"
    );
}

#[test]
fn lower_residual_shape_error_prefers_operation_source_span() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("A"),
        dae::Variable {
            dims: vec![3, 3],
            ..scalar_var("A")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("b"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("b")
        },
    );

    let equation_span = discrete_array_source_span(12, 120, 139);
    let operation_span = discrete_array_source_span(12, 126, 131);
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("A")),
            rhs: Box::new(var("b")),
            span: operation_span,
        },
        span: equation_span,
        origin: "shape mismatch".to_string(),
        scalar_count: 9,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let err = lower_residual(&dae_model, &layout)
        .expect_err("matrix/vector residual mismatch should fail");
    assert_eq!(err.source_span(), Some(operation_span));
}

#[test]
fn lower_residual_tuple_shape_counts_flattened_element_widths() {
    let mut dae_model = dae::Dae::default();
    for (name, dims) in [
        ("r", vec![3]),
        ("a", vec![0]),
        ("b", vec![0]),
        ("ku", vec![0]),
        ("source", vec![3]),
    ] {
        dae_model.variables.parameters.insert(
            rumoca_core::VarName::new(name),
            dae::Variable {
                dims,
                ..scalar_var(name)
            },
        );
    }
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            rumoca_core::Expression::Tuple {
                elements: vec![var("r"), var("a"), var("b"), var("ku")],
                span: lower_test_span(),
            },
            var("source"),
        ),
        span: lower_test_span(),
        origin: "tuple with empty array elements".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout).expect("tuple shape should use flat width");

    assert_eq!(rows.len(), 3);
}

#[test]
fn lower_residual_flattens_all_outputs_of_tuple_function_call() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("y")
        },
    );
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("k"), scalar_var("k"));

    let mut function = rumoca_core::Function::new("Pkg.outputs", lower_test_span());
    function.outputs.push(function_param_with_dims("r", &[2]));
    function.outputs.push(function_param_with_dims("gain", &[]));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("r"),
        value: rumoca_core::Expression::Array {
            elements: vec![real_lit(1.0), real_lit(2.0)],
            is_matrix: false,
            span: lower_test_span(),
        },
        span: lower_test_span(),
    });
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("gain"),
        value: real_lit(3.0),
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            rumoca_core::Expression::Tuple {
                elements: vec![var("y"), var("k")],
                span: lower_test_span(),
            },
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Pkg.outputs").into(),
                args: Vec::new(),
                is_constructor: false,
                span: lower_test_span(),
            },
        ),
        span: lower_test_span(),
        origin: "tuple function output residual".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout)
        .expect("multi-output function call should flatten all outputs");
    let p = vec![0.0; layout.p_scalars()];
    let actual = eval_programs_all_outputs(&rows, &[], &p, 0.0);

    assert_eq!(actual, vec![-1.0, -2.0, -3.0]);
}

#[test]
fn lower_discrete_rhs_lowers_easy_array_builtins() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("v"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("v")
        },
    );
    for (name, dims) in [
        ("skew_v", vec![3, 3]),
        ("outer_v", vec![3, 2]),
        ("identity_2", vec![2, 2]),
    ] {
        dae_model.variables.discrete_reals.insert(
            rumoca_core::VarName::new(name),
            dae::Variable {
                dims,
                ..scalar_var(name)
            },
        );
    }
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("skew_v").into()),
        rhs: builtin(rumoca_core::BuiltinFunction::Skew, vec![var("v")]),
        span,
        origin: "skew vector update".to_string(),
        scalar_count: 9,
    });
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("outer_v").into()),
        rhs: builtin(
            rumoca_core::BuiltinFunction::OuterProduct,
            vec![
                var("v"),
                rumoca_core::Expression::Array {
                    elements: vec![real_lit(10.0), real_lit(20.0)],
                    is_matrix: false,
                    span: lower_test_span(),
                },
            ],
        ),
        span,
        origin: "outer product update".to_string(),
        scalar_count: 6,
    });
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("identity_2").into()),
        rhs: builtin(rumoca_core::BuiltinFunction::Identity, vec![int_lit(2)]),
        span,
        origin: "identity update".to_string(),
        scalar_count: 4,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows =
        lower_discrete_rhs(&dae_model, &layout).expect("skew/outerProduct/identity should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "v[1]", 1.0);
    set_p_value(&layout, &mut p, "v[2]", 2.0);
    set_p_value(&layout, &mut p, "v[3]", 3.0);

    let actual = eval_programs_all_outputs(&rows, &[], &p, 0.0);

    assert_eq!(
        actual,
        vec![
            0.0, -3.0, 2.0, 3.0, 0.0, -1.0, -2.0, 1.0, 0.0, 10.0, 20.0, 20.0, 40.0, 30.0, 60.0,
            1.0, 0.0, 0.0, 1.0,
        ]
    );
}
