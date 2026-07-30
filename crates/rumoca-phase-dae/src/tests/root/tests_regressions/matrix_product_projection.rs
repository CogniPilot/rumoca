use super::*;

fn add_array_variable(flat: &mut Model, name: &str, dims: Vec<i64>) {
    flat.add_variable(
        VarName::new(name),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new(name),
            dims,
            is_primitive: true,
            ..flat::Variable::empty_with_span(crate::test_support::test_span())
        }),
    );
}

fn colon_slice(name: &str) -> Expression {
    all_colon_slice(name, 1)
}

fn all_colon_slice(name: &str, rank: usize) -> Expression {
    Expression::Index {
        base: Box::new(make_structured_var_ref(name)),
        subscripts: (0..rank)
            .map(|_| rumoca_core::Subscript::Colon {
                span: crate::test_support::test_span(),
            })
            .collect(),
        span: crate::test_support::test_span(),
    }
}

fn row_slice(name: &str, row: i64) -> Expression {
    Expression::Index {
        base: Box::new(make_structured_var_ref(name)),
        subscripts: vec![
            rumoca_core::Subscript::Index {
                value: row,
                span: crate::test_support::test_span(),
            },
            rumoca_core::Subscript::Colon {
                span: crate::test_support::test_span(),
            },
        ],
        span: crate::test_support::test_span(),
    }
}

fn expr_row_slice(name: &str, expr: Expression) -> Expression {
    Expression::Index {
        base: Box::new(make_structured_var_ref(name)),
        subscripts: vec![
            rumoca_core::Subscript::Expr {
                expr: Box::new(expr),
                span: crate::test_support::test_span(),
            },
            rumoca_core::Subscript::Colon {
                span: crate::test_support::test_span(),
            },
        ],
        span: crate::test_support::test_span(),
    }
}

fn binary(op: rumoca_core::OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: crate::test_support::test_span(),
    }
}

fn add_array_equation(flat: &mut Model, lhs: Expression, rhs: Expression, scalar_count: usize) {
    flat.add_equation(flat::Equation {
        residual: binary(rumoca_core::OpBinary::Sub, lhs, rhs),
        span: crate::test_support::test_span(),
        origin: flat::EquationOrigin::ComponentEquation {
            component: "MatrixProductProjectionControl".to_string(),
        },
        scalar_count,
    });
}

fn literal_indices(subscripts: &[rumoca_core::Subscript]) -> Option<Vec<i64>> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } => Some(*value),
            rumoca_core::Subscript::Expr { expr, .. } => match expr.as_ref() {
                Expression::Literal {
                    value: Literal::Integer(value),
                    ..
                } => Some(*value),
                _ => None,
            },
            rumoca_core::Subscript::Colon { .. } => None,
        })
        .collect()
}

fn var_ref_lane(expr: &Expression) -> Option<(String, Vec<i64>)> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    Some((name.as_str().to_string(), literal_indices(subscripts)?))
}

fn collect_product_terms(
    expr: &Expression,
    terms: &mut Vec<((String, Vec<i64>), (String, Vec<i64>))>,
) -> bool {
    match expr {
        Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs,
            rhs,
            ..
        } => collect_product_terms(lhs, terms) && collect_product_terms(rhs, terms),
        Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs,
            rhs,
            ..
        } => {
            let (Some(lhs), Some(rhs)) = (var_ref_lane(lhs), var_ref_lane(rhs)) else {
                return false;
            };
            terms.push((lhs, rhs));
            true
        }
        _ => false,
    }
}

fn collect_var_ref_lanes(expr: &Expression, refs: &mut Vec<(String, Vec<i64>)>) {
    match expr {
        Expression::VarRef { .. } => refs.push(var_ref_lane(expr).expect("literal subscripts")),
        Expression::Binary { lhs, rhs, .. } => {
            collect_var_ref_lanes(lhs, refs);
            collect_var_ref_lanes(rhs, refs);
        }
        Expression::Unary { rhs, .. }
        | Expression::Index { base: rhs, .. }
        | Expression::FieldAccess { base: rhs, .. } => collect_var_ref_lanes(rhs, refs),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_var_ref_lanes(arg, refs);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                collect_var_ref_lanes(condition, refs);
                collect_var_ref_lanes(value, refs);
            }
            collect_var_ref_lanes(else_branch, refs);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            for element in elements {
                collect_var_ref_lanes(element, refs);
            }
        }
        _ => {}
    }
}

fn assert_spanned_contract_error(error: ToDaeError, expected: &str) {
    assert!(
        error.to_string().contains(expected),
        "expected `{expected}` in error, got {error}"
    );
    assert_eq!(
        error.source_span(),
        Some(crate::test_support::test_span()),
        "shape failure must retain the matrix-product source span"
    );
}

fn residual_sides(equation: &rumoca_ir_dae::Equation) -> (&Expression, &Expression) {
    let Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.rhs
    else {
        panic!("expected scalar residual, got {:?}", equation.rhs);
    };
    (lhs, rhs)
}

#[test]
fn test_todae_projects_transposed_matrix_vector_rows_as_three_term_dots() {
    let mut flat = Model::new();
    add_array_variable(&mut flat, "A", vec![3, 3]);
    add_array_variable(&mut flat, "x", vec![3]);
    add_array_variable(&mut flat, "y", vec![3]);

    // Minimal source equation: y[:] = transpose(A) * x[:].
    flat.add_equation(flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(colon_slice("y")),
            rhs: Box::new(Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Transpose,
                    args: vec![make_structured_var_ref("A")],
                    span: crate::test_support::test_span(),
                }),
                rhs: Box::new(colon_slice("x")),
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: flat::EquationOrigin::ComponentEquation {
            component: "MatrixProductProjection".to_string(),
        },
        scalar_count: 3,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("minimal matrix product should lower to DAE");

    assert_eq!(dae.continuous.equations.len(), 3);
    for (lane, equation) in dae.continuous.equations.iter().enumerate() {
        let one_based_lane = i64::try_from(lane + 1).unwrap();
        let (lhs, rhs) = residual_sides(equation);
        assert_eq!(
            var_ref_lane(lhs),
            Some(("y".to_string(), vec![one_based_lane]))
        );

        let mut terms = Vec::new();
        assert!(
            collect_product_terms(rhs, &mut terms),
            "DAE lane {} must be a complete dot product, got {rhs:?}",
            lane + 1
        );
        assert_eq!(
            terms,
            (1_i64..=3)
                .map(|row| {
                    (
                        ("A".to_string(), vec![row, one_based_lane]),
                        ("x".to_string(), vec![row]),
                    )
                })
                .collect::<Vec<_>>(),
            "DAE lane {} must use the complete row of transpose(A)",
            lane + 1
        );
    }
}

#[test]
fn test_todae_projects_indexed_vector_matrix_columns_as_three_term_dots() {
    let mut flat = Model::new();
    add_array_variable(&mut flat, "source", vec![2, 3]);
    add_array_variable(&mut flat, "B", vec![3, 2]);
    add_array_variable(&mut flat, "y", vec![2]);

    // Minimal indexed-slice equation: y[:] = source[1, :] * B.
    flat.add_equation(flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(colon_slice("y")),
            rhs: Box::new(Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(row_slice("source", 1)),
                rhs: Box::new(make_structured_var_ref("B")),
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: flat::EquationOrigin::ComponentEquation {
            component: "VectorMatrixProjection".to_string(),
        },
        scalar_count: 2,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("indexed vector-matrix product should lower to DAE");

    assert_eq!(dae.continuous.equations.len(), 2);
    for (lane, equation) in dae.continuous.equations.iter().enumerate() {
        let column = i64::try_from(lane + 1).unwrap();
        let (lhs, rhs) = residual_sides(equation);
        assert_eq!(var_ref_lane(lhs), Some(("y".to_string(), vec![column])));
        let mut terms = Vec::new();
        assert!(
            collect_product_terms(rhs, &mut terms),
            "DAE lane {} must be a complete vector-matrix dot, got {rhs:?}",
            lane + 1
        );
        assert_eq!(
            terms,
            (1_i64..=3)
                .map(|inner| {
                    (
                        ("source".to_string(), vec![1, inner]),
                        ("B".to_string(), vec![inner, column]),
                    )
                })
                .collect::<Vec<_>>()
        );
    }
}

#[test]
fn test_todae_projects_matrix_matrix_cells_as_three_term_dots() {
    let mut flat = Model::new();
    add_array_variable(&mut flat, "A", vec![2, 3]);
    add_array_variable(&mut flat, "B", vec![3, 2]);
    add_array_variable(&mut flat, "C", vec![2, 2]);

    // Minimal matrix equation: C[:, :] = A[:, :] * B[:, :].
    flat.add_equation(flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(all_colon_slice("C", 2)),
            rhs: Box::new(Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(all_colon_slice("A", 2)),
                rhs: Box::new(all_colon_slice("B", 2)),
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: flat::EquationOrigin::ComponentEquation {
            component: "MatrixMatrixProjection".to_string(),
        },
        scalar_count: 4,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("matrix-matrix product should lower to DAE");

    assert_eq!(dae.continuous.equations.len(), 4);
    for (lane, equation) in dae.continuous.equations.iter().enumerate() {
        let row = i64::try_from(lane / 2 + 1).unwrap();
        let column = i64::try_from(lane % 2 + 1).unwrap();
        let (lhs, rhs) = residual_sides(equation);
        assert_eq!(
            var_ref_lane(lhs),
            Some(("C".to_string(), vec![row, column]))
        );
        let mut terms = Vec::new();
        assert!(
            collect_product_terms(rhs, &mut terms),
            "DAE cell ({row}, {column}) must be a complete matrix dot, got {rhs:?}"
        );
        assert_eq!(
            terms,
            (1_i64..=3)
                .map(|inner| {
                    (
                        ("A".to_string(), vec![row, inner]),
                        ("B".to_string(), vec![inner, column]),
                    )
                })
                .collect::<Vec<_>>()
        );
    }
}

#[test]
fn test_todae_preserves_proven_scalar_array_scaling_forms() {
    let mut flat = Model::new();
    add_array_variable(&mut flat, "x", vec![3]);
    add_array_variable(&mut flat, "gain", vec![]);
    add_array_variable(&mut flat, "flag", vec![]);
    for output in [
        "literal",
        "declared",
        "compound",
        "function",
        "conditional",
        "right",
    ] {
        add_array_variable(&mut flat, output, vec![3]);
    }
    let mut scalar_function =
        rumoca_core::Function::new("scalarFunction", crate::test_support::test_span());
    scalar_function.add_input(rumoca_core::FunctionParam::new(
        "u",
        "Real",
        crate::test_support::test_span(),
    ));
    scalar_function.add_output(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        crate::test_support::test_span(),
    ));
    scalar_function.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("scalar_function".to_string()),
        output_name: Some("y".to_string()),
        ..Default::default()
    });
    flat.add_function(scalar_function);

    let literal = Expression::Literal {
        value: Literal::Real(2.0),
        span: crate::test_support::test_span(),
    };
    let gain = make_structured_var_ref("gain");
    let scalar_compound = binary(rumoca_core::OpBinary::Add, gain.clone(), literal.clone());
    let scalar_call = Expression::FunctionCall {
        name: VarName::new("scalarFunction").into(),
        args: vec![gain.clone()],
        is_constructor: false,
        span: crate::test_support::test_span(),
    };
    let scalar_conditional = Expression::If {
        branches: vec![(make_structured_var_ref("flag"), gain.clone())],
        else_branch: Box::new(literal.clone()),
        span: crate::test_support::test_span(),
    };
    let cases = [
        (
            "literal",
            binary(rumoca_core::OpBinary::Mul, literal, colon_slice("x")),
        ),
        (
            "declared",
            binary(rumoca_core::OpBinary::Mul, gain.clone(), colon_slice("x")),
        ),
        (
            "compound",
            binary(
                rumoca_core::OpBinary::Mul,
                scalar_compound,
                colon_slice("x"),
            ),
        ),
        (
            "function",
            binary(rumoca_core::OpBinary::Mul, scalar_call, colon_slice("x")),
        ),
        (
            "conditional",
            binary(
                rumoca_core::OpBinary::Mul,
                scalar_conditional,
                colon_slice("x"),
            ),
        ),
        (
            "right",
            binary(rumoca_core::OpBinary::Mul, colon_slice("x"), gain),
        ),
    ];
    for (output, rhs) in cases {
        add_array_equation(&mut flat, colon_slice(output), rhs, 3);
    }

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("all proven scalar-array scaling forms should lower");

    assert_eq!(dae.continuous.equations.len(), 18);
    for (index, equation) in dae.continuous.equations.iter().enumerate() {
        let lane = i64::try_from(index % 3 + 1).unwrap();
        let (_, rhs) = residual_sides(equation);
        assert!(matches!(
            rhs,
            Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                ..
            }
        ));
        let mut refs = Vec::new();
        collect_var_ref_lanes(rhs, &mut refs);
        assert!(refs.contains(&("x".to_string(), vec![lane])));
        assert!(
            refs.iter()
                .filter(|(name, _)| name != "x")
                .all(|(_, subscripts)| subscripts.is_empty()),
            "scalar factors must remain scalar: {refs:?}"
        );
    }
}

#[test]
fn test_todae_rejects_scalar_array_scaling_target_width_mismatch() {
    let mut flat = Model::new();
    add_array_variable(&mut flat, "gain", vec![]);
    add_array_variable(&mut flat, "x", vec![3]);
    add_array_variable(&mut flat, "y", vec![2]);
    add_array_equation(
        &mut flat,
        colon_slice("y"),
        binary(
            rumoca_core::OpBinary::Mul,
            make_structured_var_ref("gain"),
            colon_slice("x"),
        ),
        2,
    );

    let error = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("scalar-array scaling must not truncate to the target width");
    assert_spanned_contract_error(error, "result shape");
}

#[test]
fn test_todae_keeps_mulelem_same_lane_and_rejects_mismatched_shapes() {
    let mut valid = Model::new();
    for name in ["a", "b", "y"] {
        add_array_variable(&mut valid, name, vec![3]);
    }
    add_array_equation(
        &mut valid,
        colon_slice("y"),
        binary(
            rumoca_core::OpBinary::MulElem,
            colon_slice("a"),
            colon_slice("b"),
        ),
        3,
    );
    let dae = to_dae_with_options(
        &valid,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("equal-shape MulElem should lower elementwise");
    for (lane, equation) in dae.continuous.equations.iter().enumerate() {
        let (_, rhs) = residual_sides(equation);
        let Expression::Binary {
            op: rumoca_core::OpBinary::MulElem,
            lhs,
            rhs,
            ..
        } = rhs
        else {
            panic!("MulElem must remain elementwise, got {rhs:?}");
        };
        let lane = i64::try_from(lane + 1).unwrap();
        assert_eq!(var_ref_lane(lhs), Some(("a".to_string(), vec![lane])));
        assert_eq!(var_ref_lane(rhs), Some(("b".to_string(), vec![lane])));
    }

    let mut mismatch = Model::new();
    add_array_variable(&mut mismatch, "a", vec![3]);
    add_array_variable(&mut mismatch, "b", vec![2]);
    add_array_variable(&mut mismatch, "y", vec![3]);
    add_array_equation(
        &mut mismatch,
        colon_slice("y"),
        binary(
            rumoca_core::OpBinary::MulElem,
            colon_slice("a"),
            colon_slice("b"),
        ),
        3,
    );
    let error = to_dae_with_options(
        &mismatch,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("MulElem must reject mismatched vector shapes");
    assert_spanned_contract_error(error, "elementwise shape mismatch");
}

#[test]
fn test_todae_projects_ordinary_vector_mul_as_dot_only_for_scalar_target() {
    let mut flat = Model::new();
    add_array_variable(&mut flat, "a", vec![3]);
    add_array_variable(&mut flat, "b", vec![3]);
    add_array_variable(&mut flat, "s", vec![]);
    add_array_equation(
        &mut flat,
        make_structured_var_ref("s"),
        binary(
            rumoca_core::OpBinary::Mul,
            make_structured_var_ref("a"),
            make_structured_var_ref("b"),
        ),
        1,
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("ordinary vector Mul in a scalar target should lower as a dot");
    let (_, rhs) = residual_sides(&dae.continuous.equations[0]);
    let mut terms = Vec::new();
    assert!(collect_product_terms(rhs, &mut terms));
    assert_eq!(
        terms,
        (1_i64..=3)
            .map(|lane| { (("a".to_string(), vec![lane]), ("b".to_string(), vec![lane]),) })
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_todae_preserves_colon_vector_dot_for_scalar_target() {
    let mut flat = Model::new();
    add_array_variable(&mut flat, "a", vec![3]);
    add_array_variable(&mut flat, "b", vec![3]);
    add_array_variable(&mut flat, "z", vec![]);
    add_array_equation(
        &mut flat,
        make_structured_var_ref("z"),
        binary(
            rumoca_core::OpBinary::Mul,
            colon_slice("a"),
            colon_slice("b"),
        ),
        1,
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("existing colon-vector dot lowering must remain intact");
    let (_, rhs) = residual_sides(&dae.continuous.equations[0]);
    let mut terms = Vec::new();
    assert!(collect_product_terms(rhs, &mut terms));
    assert_eq!(terms.len(), 3, "colon-vector dot must retain every term");
}

#[test]
fn test_todae_projects_nested_matrix_vector_scaling_in_both_orders() {
    let mut flat = Model::new();
    add_array_variable(&mut flat, "A", vec![2, 2]);
    add_array_variable(&mut flat, "x", vec![2]);
    add_array_variable(&mut flat, "left", vec![2]);
    add_array_variable(&mut flat, "right", vec![2]);
    let scale = Expression::Literal {
        value: Literal::Real(2.0),
        span: crate::test_support::test_span(),
    };
    let product = || {
        binary(
            rumoca_core::OpBinary::Mul,
            make_structured_var_ref("A"),
            colon_slice("x"),
        )
    };
    add_array_equation(
        &mut flat,
        colon_slice("left"),
        binary(rumoca_core::OpBinary::Mul, scale.clone(), product()),
        2,
    );
    add_array_equation(
        &mut flat,
        colon_slice("right"),
        binary(rumoca_core::OpBinary::Mul, product(), scale),
        2,
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("nested matrix-vector scaling must project the array-valued product");
    assert_eq!(dae.continuous.equations.len(), 4);
    for equation in &dae.continuous.equations {
        let (_, rhs) = residual_sides(equation);
        let Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs,
            rhs,
            ..
        } = rhs
        else {
            panic!("outer scalar scaling must remain multiplication: {rhs:?}");
        };
        let product = if matches!(lhs.as_ref(), Expression::Literal { .. }) {
            rhs.as_ref()
        } else {
            lhs.as_ref()
        };
        let mut terms = Vec::new();
        assert!(collect_product_terms(product, &mut terms));
        assert_eq!(
            terms.len(),
            2,
            "nested matrix-vector lane must be a full dot"
        );
    }
}

#[test]
fn test_todae_rejects_matrix_product_inner_and_target_shape_mismatches() {
    let mut inner = Model::new();
    add_array_variable(&mut inner, "A", vec![2, 3]);
    add_array_variable(&mut inner, "x", vec![2]);
    add_array_variable(&mut inner, "y", vec![2]);
    add_array_equation(
        &mut inner,
        colon_slice("y"),
        binary(
            rumoca_core::OpBinary::Mul,
            make_structured_var_ref("A"),
            colon_slice("x"),
        ),
        2,
    );
    let error = to_dae_with_options(
        &inner,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("matrix-vector inner-dimension mismatch must fail closed");
    assert_spanned_contract_error(error, "inner dimension mismatch");

    let mut target = Model::new();
    add_array_variable(&mut target, "A", vec![3, 3]);
    add_array_variable(&mut target, "x", vec![3]);
    add_array_variable(&mut target, "y", vec![2]);
    add_array_equation(
        &mut target,
        colon_slice("y"),
        binary(
            rumoca_core::OpBinary::Mul,
            make_structured_var_ref("A"),
            colon_slice("x"),
        ),
        2,
    );
    let error = to_dae_with_options(
        &target,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("matrix-vector result must match the declared target shape");
    assert_spanned_contract_error(error, "result shape");
}

#[test]
fn test_todae_rejects_nonscalar_matrix_result_in_scalar_context_and_rank_three_operand() {
    let mut scalar_target = Model::new();
    add_array_variable(&mut scalar_target, "A", vec![2, 2]);
    add_array_variable(&mut scalar_target, "x", vec![2]);
    add_array_variable(&mut scalar_target, "s", vec![]);
    add_array_equation(
        &mut scalar_target,
        make_structured_var_ref("s"),
        binary(
            rumoca_core::OpBinary::Mul,
            make_structured_var_ref("A"),
            make_structured_var_ref("x"),
        ),
        1,
    );
    let error = to_dae_with_options(
        &scalar_target,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("non-scalar matrix product must not survive in scalar context");
    assert_spanned_contract_error(error, "non-scalar result in scalar context");

    let mut unsupported = Model::new();
    add_array_variable(&mut unsupported, "T", vec![2, 2, 2]);
    add_array_variable(&mut unsupported, "x", vec![2]);
    add_array_variable(&mut unsupported, "y", vec![2]);
    add_array_equation(
        &mut unsupported,
        colon_slice("y"),
        binary(
            rumoca_core::OpBinary::Mul,
            make_structured_var_ref("T"),
            colon_slice("x"),
        ),
        2,
    );
    let error = to_dae_with_options(
        &unsupported,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("rank-three matrix product operands must fail closed");
    assert_spanned_contract_error(error, "unsupported rank");
}

#[test]
fn test_todae_rejects_unproven_expr_subscripts_in_matrix_products() {
    let range = Expression::Range {
        start: Box::new(Expression::Literal {
            value: Literal::Integer(1),
            span: crate::test_support::test_span(),
        }),
        step: None,
        end: Box::new(Expression::Literal {
            value: Literal::Integer(2),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };
    let dynamic = make_structured_var_ref("i");
    for (label, lhs, scalar_target) in [
        ("range", expr_row_slice("A", range), true),
        ("dynamic", expr_row_slice("A", dynamic), true),
        (
            "unknown base",
            Expression::VarRef {
                name: VarName::new("unknown").into(),
                subscripts: vec![rumoca_core::Subscript::Expr {
                    expr: Box::new(Expression::Literal {
                        value: Literal::Integer(1),
                        span: crate::test_support::test_span(),
                    }),
                    span: crate::test_support::test_span(),
                }],
                span: crate::test_support::test_span(),
            },
            false,
        ),
    ] {
        let mut flat = Model::new();
        add_array_variable(&mut flat, "A", vec![2, 3]);
        add_array_variable(&mut flat, "x", vec![3]);
        add_array_variable(&mut flat, "y", vec![3]);
        add_array_variable(&mut flat, "z", vec![]);
        add_array_variable(&mut flat, "i", vec![]);
        add_array_equation(
            &mut flat,
            if scalar_target {
                make_structured_var_ref("z")
            } else {
                colon_slice("y")
            },
            binary(rumoca_core::OpBinary::Mul, lhs, colon_slice("x")),
            if scalar_target { 1 } else { 3 },
        );

        let result = to_dae_with_options(
            &flat,
            ToDaeOptions {
                error_on_unbalanced: false,
            },
        );
        let error = match result {
            Ok(dae) => panic!("{label} subscript must fail closed, got {dae:?}"),
            Err(error) => error,
        };
        assert_spanned_contract_error(error, "unknown operand shape");
    }
}

#[test]
fn test_todae_rejects_array_valued_function_product_operand() {
    let mut flat = Model::new();
    add_array_variable(&mut flat, "x", vec![3]);
    add_array_variable(&mut flat, "z", vec![]);
    let mut function =
        rumoca_core::Function::new("arrayFunction", crate::test_support::test_span());
    function.add_output(
        rumoca_core::FunctionParam::new("y", "Real", crate::test_support::test_span())
            .with_dims(vec![3]),
    );
    function.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("array_function".to_string()),
        output_name: Some("y".to_string()),
        ..Default::default()
    });
    flat.add_function(function);
    add_array_equation(
        &mut flat,
        make_structured_var_ref("z"),
        binary(
            rumoca_core::OpBinary::Mul,
            Expression::FunctionCall {
                name: VarName::new("arrayFunction").into(),
                args: vec![],
                is_constructor: false,
                span: crate::test_support::test_span(),
            },
            colon_slice("x"),
        ),
        1,
    );

    let error = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("array-valued function output cannot be lane-projected");
    assert_spanned_contract_error(error, "unknown operand shape");
}
