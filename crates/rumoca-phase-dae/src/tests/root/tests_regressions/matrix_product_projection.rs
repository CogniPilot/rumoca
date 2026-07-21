use super::*;

fn declare_array(flat: &mut Model, name: &str, dims: &[i64]) {
    flat.add_variable(
        VarName::new(name),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new(name),
            dims: dims.to_vec(),
            is_primitive: true,
            ..flat::Variable::empty_with_span(crate::test_support::test_span())
        }),
    );
}

fn colon_vector(name: &str) -> Expression {
    colon_array(name, 1)
}

fn colon_array(name: &str, rank: usize) -> Expression {
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

fn multiply(lhs: Expression, rhs: Expression) -> Expression {
    binary(rumoca_core::OpBinary::Mul, lhs, rhs)
}

fn binary(op: rumoca_core::OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: crate::test_support::test_span(),
    }
}

fn real(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: crate::test_support::test_span(),
    }
}

fn add_equation(flat: &mut Model, lhs: Expression, rhs: Expression, scalar_count: usize) {
    flat.add_equation(flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: flat::EquationOrigin::ComponentEquation {
            component: "MatrixProductProjection".to_string(),
        },
        scalar_count,
    });
}

fn literal_subscripts(expr: &Expression) -> Option<(&str, Vec<i64>)> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    let indices = subscripts
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
        .collect::<Option<Vec<_>>>()?;
    Some((name.as_str(), indices))
}

type ProductTerm<'a> = ((&'a str, Vec<i64>), (&'a str, Vec<i64>));

fn flatten_dot_terms<'a>(expr: &'a Expression, terms: &mut Vec<ProductTerm<'a>>) -> bool {
    match expr {
        Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs,
            rhs,
            ..
        } => flatten_dot_terms(lhs, terms) && flatten_dot_terms(rhs, terms),
        Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs,
            rhs,
            ..
        } => {
            let (Some(lhs), Some(rhs)) = (literal_subscripts(lhs), literal_subscripts(rhs)) else {
                return false;
            };
            terms.push((lhs, rhs));
            true
        }
        _ => false,
    }
}

fn residual_rhs(equation: &rumoca_ir_dae::Equation) -> &Expression {
    let Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        rhs,
        ..
    } = &equation.rhs
    else {
        panic!("expected scalar residual, got {:?}", equation.rhs);
    };
    rhs
}

fn collect_refs<'a>(expr: &'a Expression, refs: &mut Vec<(&'a str, Vec<i64>)>) {
    match expr {
        Expression::VarRef { .. } => refs.push(literal_subscripts(expr).expect("literal lanes")),
        Expression::Binary { lhs, rhs, .. } => {
            collect_refs(lhs, refs);
            collect_refs(rhs, refs);
        }
        Expression::Unary { rhs, .. } => collect_refs(rhs, refs),
        Expression::FunctionCall { args, .. } | Expression::BuiltinCall { args, .. } => {
            for arg in args {
                collect_refs(arg, refs);
            }
        }
        _ => {}
    }
}

fn assert_projection_error(flat: &Model, expected: &str) {
    let error = to_dae_with_options(
        flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("invalid matrix-product projection must fail closed");
    assert!(
        error.to_string().contains(expected),
        "expected `{expected}` in {error}"
    );
    assert_eq!(error.source_span(), Some(crate::test_support::test_span()));
}

fn expression_row_slice(name: &str, selector: Expression) -> Expression {
    Expression::Index {
        base: Box::new(make_structured_var_ref(name)),
        subscripts: vec![
            rumoca_core::Subscript::Expr {
                expr: Box::new(selector),
                span: crate::test_support::test_span(),
            },
            rumoca_core::Subscript::Colon {
                span: crate::test_support::test_span(),
            },
        ],
        span: crate::test_support::test_span(),
    }
}

#[test]
fn test_todae_projects_transposed_matrix_vector_rows_as_three_term_dots() {
    let mut flat = Model::new();
    declare_array(&mut flat, "A", &[3, 3]);
    declare_array(&mut flat, "x", &[3]);
    declare_array(&mut flat, "y", &[3]);

    let transpose_a = Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Transpose,
        args: vec![make_structured_var_ref("A")],
        span: crate::test_support::test_span(),
    };
    add_equation(
        &mut flat,
        colon_vector("y"),
        multiply(transpose_a, colon_vector("x")),
        3,
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("shape-correct matrix-vector product should reach finalized DAE");

    assert_eq!(dae.continuous.equations.len(), 3);
    for (lane, equation) in dae.continuous.equations.iter().enumerate() {
        let Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } = &equation.rhs
        else {
            panic!("expected scalar residual, got {:?}", equation.rhs);
        };
        let output_index = i64::try_from(lane + 1).expect("three lanes fit i64");
        assert_eq!(literal_subscripts(lhs), Some(("y", vec![output_index])));

        let mut terms = Vec::new();
        assert!(
            flatten_dot_terms(rhs, &mut terms),
            "DAE lane {} must be a complete dot product, got {rhs:?}",
            lane + 1
        );
        let expected = (1_i64..=3)
            .map(|row| (("A", vec![row, output_index]), ("x", vec![row])))
            .collect::<Vec<_>>();
        assert_eq!(
            terms,
            expected,
            "DAE lane {} must contain every inner-dimension term",
            lane + 1
        );
    }
}

#[test]
fn test_todae_projects_indexed_vector_matrix_columns_as_three_term_dots() {
    let mut flat = Model::new();
    declare_array(&mut flat, "source", &[2, 3]);
    declare_array(&mut flat, "B", &[3, 2]);
    declare_array(&mut flat, "y", &[2]);
    add_equation(
        &mut flat,
        colon_vector("y"),
        multiply(row_slice("source", 1), make_structured_var_ref("B")),
        2,
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("indexed vector-matrix product should lower");

    for (lane, equation) in dae.continuous.equations.iter().enumerate() {
        let column = i64::try_from(lane + 1).expect("two lanes fit i64");
        let Expression::Binary { lhs, rhs, .. } = &equation.rhs else {
            panic!("expected residual");
        };
        assert_eq!(literal_subscripts(lhs), Some(("y", vec![column])));
        let mut terms = Vec::new();
        assert!(flatten_dot_terms(rhs, &mut terms), "got {rhs:?}");
        assert_eq!(
            terms,
            (1_i64..=3)
                .map(|inner| (("source", vec![1, inner]), ("B", vec![inner, column])))
                .collect::<Vec<_>>()
        );
    }
}

#[test]
fn test_todae_projects_matrix_matrix_cells_as_three_term_dots() {
    let mut flat = Model::new();
    declare_array(&mut flat, "A", &[2, 3]);
    declare_array(&mut flat, "B", &[3, 2]);
    declare_array(&mut flat, "C", &[2, 2]);
    add_equation(
        &mut flat,
        colon_array("C", 2),
        multiply(colon_array("A", 2), colon_array("B", 2)),
        4,
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("matrix-matrix product should lower");

    for (lane, equation) in dae.continuous.equations.iter().enumerate() {
        let row = i64::try_from(lane / 2 + 1).expect("row fits i64");
        let column = i64::try_from(lane % 2 + 1).expect("column fits i64");
        let Expression::Binary { lhs, rhs, .. } = &equation.rhs else {
            panic!("expected residual");
        };
        assert_eq!(literal_subscripts(lhs), Some(("C", vec![row, column])));
        let mut terms = Vec::new();
        assert!(flatten_dot_terms(rhs, &mut terms), "got {rhs:?}");
        assert_eq!(
            terms,
            (1_i64..=3)
                .map(|inner| (("A", vec![row, inner]), ("B", vec![inner, column])))
                .collect::<Vec<_>>()
        );
    }
}

#[test]
fn test_todae_projects_proven_scalar_scaling_forms() {
    let mut flat = Model::new();
    declare_array(&mut flat, "x", &[3]);
    declare_array(&mut flat, "gain", &[]);
    for name in ["literal", "declared", "compound", "function", "right"] {
        declare_array(&mut flat, name, &[3]);
    }
    let mut function =
        rumoca_core::Function::new("scalarFunction", crate::test_support::test_span());
    function.add_input(rumoca_core::FunctionParam::new(
        "u",
        "Real",
        crate::test_support::test_span(),
    ));
    function.add_output(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        crate::test_support::test_span(),
    ));
    function.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("scalar_function".to_string()),
        output_name: Some("y".to_string()),
        ..Default::default()
    });
    flat.add_function(function);

    let gain = make_structured_var_ref("gain");
    let cases = [
        ("literal", multiply(real(2.0), colon_vector("x"))),
        ("declared", multiply(gain.clone(), colon_vector("x"))),
        (
            "compound",
            multiply(
                binary(rumoca_core::OpBinary::Add, gain.clone(), real(1.0)),
                colon_vector("x"),
            ),
        ),
        (
            "function",
            multiply(
                Expression::FunctionCall {
                    name: VarName::new("scalarFunction").into(),
                    args: vec![gain.clone()],
                    is_constructor: false,
                    span: crate::test_support::test_span(),
                },
                colon_vector("x"),
            ),
        ),
        ("right", multiply(colon_vector("x"), gain)),
    ];
    for (name, rhs) in cases {
        add_equation(&mut flat, colon_vector(name), rhs, 3);
    }

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("proven scalar factors should remain scalar");

    assert_eq!(dae.continuous.equations.len(), 15);
    for (index, equation) in dae.continuous.equations.iter().enumerate() {
        let lane = i64::try_from(index % 3 + 1).expect("lane fits i64");
        let mut refs = Vec::new();
        collect_refs(residual_rhs(equation), &mut refs);
        assert!(refs.contains(&("x", vec![lane])), "refs were {refs:?}");
        assert!(
            refs.iter()
                .filter(|(name, _)| *name != "x")
                .all(|(_, indices)| indices.is_empty()),
            "scalar factor was lane-projected: {refs:?}"
        );
    }
}

#[test]
fn test_todae_keeps_same_shape_mulelem_on_the_same_lane() {
    let mut flat = Model::new();
    for name in ["a", "b", "y"] {
        declare_array(&mut flat, name, &[3]);
    }
    add_equation(
        &mut flat,
        colon_vector("y"),
        binary(
            rumoca_core::OpBinary::MulElem,
            colon_vector("a"),
            colon_vector("b"),
        ),
        3,
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("same-shape MulElem should lower elementwise");
    for (lane, equation) in dae.continuous.equations.iter().enumerate() {
        let Expression::Binary {
            op: rumoca_core::OpBinary::MulElem,
            lhs,
            rhs,
            ..
        } = residual_rhs(equation)
        else {
            panic!("expected MulElem, got {:?}", equation.rhs);
        };
        let lane = i64::try_from(lane + 1).expect("lane fits i64");
        assert_eq!(literal_subscripts(lhs), Some(("a", vec![lane])));
        assert_eq!(literal_subscripts(rhs), Some(("b", vec![lane])));
    }
}

#[test]
fn test_todae_lowers_vector_mul_to_dot_only_for_scalar_targets() {
    let mut flat = Model::new();
    declare_array(&mut flat, "a", &[3]);
    declare_array(&mut flat, "b", &[3]);
    declare_array(&mut flat, "colonDot", &[]);
    declare_array(&mut flat, "bareDot", &[]);
    add_equation(
        &mut flat,
        make_structured_var_ref("colonDot"),
        multiply(colon_vector("a"), colon_vector("b")),
        1,
    );
    add_equation(
        &mut flat,
        make_structured_var_ref("bareDot"),
        multiply(make_structured_var_ref("a"), make_structured_var_ref("b")),
        1,
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("scalar-target vector products should become dots");
    for equation in &dae.continuous.equations {
        let mut terms = Vec::new();
        assert!(flatten_dot_terms(residual_rhs(equation), &mut terms));
        assert_eq!(terms.len(), 3);
    }
}

#[test]
fn test_todae_projects_nested_matrix_vector_scaling_in_both_orders() {
    let mut flat = Model::new();
    declare_array(&mut flat, "A", &[2, 2]);
    declare_array(&mut flat, "x", &[2]);
    declare_array(&mut flat, "left", &[2]);
    declare_array(&mut flat, "right", &[2]);
    let product = || multiply(make_structured_var_ref("A"), colon_vector("x"));
    add_equation(
        &mut flat,
        colon_vector("left"),
        multiply(real(2.0), product()),
        2,
    );
    add_equation(
        &mut flat,
        colon_vector("right"),
        multiply(product(), real(2.0)),
        2,
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("nested scaling should project only the array side");
    for equation in &dae.continuous.equations {
        let Expression::Binary { lhs, rhs, .. } = residual_rhs(equation) else {
            panic!("expected outer multiplication");
        };
        let product = if matches!(lhs.as_ref(), Expression::Literal { .. }) {
            rhs
        } else {
            lhs
        };
        let mut terms = Vec::new();
        assert!(flatten_dot_terms(product, &mut terms), "got {product:?}");
        assert_eq!(terms.len(), 2);
    }
}

#[test]
fn test_todae_rejects_inner_and_target_shape_mismatches() {
    let mut inner = Model::new();
    declare_array(&mut inner, "A", &[2, 3]);
    declare_array(&mut inner, "x", &[2]);
    declare_array(&mut inner, "y", &[2]);
    add_equation(
        &mut inner,
        colon_vector("y"),
        multiply(make_structured_var_ref("A"), colon_vector("x")),
        2,
    );
    assert_projection_error(&inner, "inner dimension mismatch");

    let mut target = Model::new();
    declare_array(&mut target, "A", &[3, 3]);
    declare_array(&mut target, "x", &[3]);
    declare_array(&mut target, "y", &[2]);
    add_equation(
        &mut target,
        colon_vector("y"),
        multiply(make_structured_var_ref("A"), colon_vector("x")),
        2,
    );
    assert_projection_error(&target, "result shape mismatch");
}

#[test]
fn test_todae_rejects_matrix_result_in_scalar_context_and_rank_three() {
    let mut scalar = Model::new();
    declare_array(&mut scalar, "A", &[2, 2]);
    declare_array(&mut scalar, "x", &[2]);
    declare_array(&mut scalar, "s", &[]);
    add_equation(
        &mut scalar,
        make_structured_var_ref("s"),
        multiply(make_structured_var_ref("A"), make_structured_var_ref("x")),
        1,
    );
    assert_projection_error(&scalar, "non-scalar result in scalar context");

    let mut rank_three = Model::new();
    declare_array(&mut rank_three, "T", &[2, 2, 2]);
    declare_array(&mut rank_three, "x", &[2]);
    declare_array(&mut rank_three, "y", &[2]);
    add_equation(
        &mut rank_three,
        colon_vector("y"),
        multiply(make_structured_var_ref("T"), colon_vector("x")),
        2,
    );
    assert_projection_error(&rank_three, "unsupported rank");
}

#[test]
fn test_todae_rejects_mulelem_shape_mismatch() {
    let mut flat = Model::new();
    declare_array(&mut flat, "a", &[3]);
    declare_array(&mut flat, "b", &[2]);
    declare_array(&mut flat, "y", &[3]);
    add_equation(
        &mut flat,
        colon_vector("y"),
        binary(
            rumoca_core::OpBinary::MulElem,
            colon_vector("a"),
            colon_vector("b"),
        ),
        3,
    );
    assert_projection_error(&flat, "elementwise shape mismatch");
}

#[test]
fn test_todae_rejects_dynamic_range_and_unknown_product_operands() {
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
    let operands = [
        expression_row_slice("A", make_structured_var_ref("i")),
        expression_row_slice("A", range),
        Expression::VarRef {
            name: VarName::new("unknown").into(),
            subscripts: vec![rumoca_core::Subscript::Index {
                value: 1,
                span: crate::test_support::test_span(),
            }],
            span: crate::test_support::test_span(),
        },
    ];
    for operand in operands {
        let mut flat = Model::new();
        declare_array(&mut flat, "A", &[2, 3]);
        declare_array(&mut flat, "x", &[3]);
        declare_array(&mut flat, "z", &[]);
        declare_array(&mut flat, "i", &[]);
        add_equation(
            &mut flat,
            make_structured_var_ref("z"),
            multiply(operand, colon_vector("x")),
            1,
        );
        assert_projection_error(&flat, "unknown operand shape");
    }
}

#[test]
fn test_todae_rejects_array_valued_function_product_operand() {
    let mut flat = Model::new();
    declare_array(&mut flat, "x", &[3]);
    declare_array(&mut flat, "z", &[]);
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
    add_equation(
        &mut flat,
        make_structured_var_ref("z"),
        multiply(
            Expression::FunctionCall {
                name: VarName::new("arrayFunction").into(),
                args: Vec::new(),
                is_constructor: false,
                span: crate::test_support::test_span(),
            },
            colon_vector("x"),
        ),
        1,
    );
    assert_projection_error(&flat, "array-valued function output cannot be projected");
}
