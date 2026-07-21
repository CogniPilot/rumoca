use super::matrix_product_projection::{binary, declare_array, literal_subscripts};
use super::*;

fn assert_compound_dot_term(expr: &Expression, row: i64, inner: i64) {
    let Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs,
        rhs,
        ..
    } = expr
    else {
        panic!("expected compound dot term, got {expr:?}");
    };
    let Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: lhs_a,
        rhs: lhs_b,
        ..
    } = lhs.as_ref()
    else {
        panic!("expected matrix inner sum, got {lhs:?}");
    };
    assert_eq!(literal_subscripts(lhs_a), Some(("A", vec![row, inner])));
    assert_eq!(literal_subscripts(lhs_b), Some(("B", vec![row, inner])));
    let Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: rhs_c,
        rhs: rhs_d,
        ..
    } = rhs.as_ref()
    else {
        panic!("expected vector inner sum, got {rhs:?}");
    };
    assert_eq!(literal_subscripts(rhs_c), Some(("C", vec![inner])));
    assert_eq!(literal_subscripts(rhs_d), Some(("D", vec![inner])));
}

#[test]
fn test_todae_projects_bare_compound_array_operands_as_complete_dots() {
    let mut flat = Model::new();
    for (name, dims) in [
        ("A", [2, 2].as_slice()),
        ("B", [2, 2].as_slice()),
        ("C", [2].as_slice()),
        ("D", [2].as_slice()),
        ("Y", [2].as_slice()),
    ] {
        declare_array(&mut flat, name, dims);
    }
    let add = |lhs, rhs| binary(rumoca_core::OpBinary::Add, lhs, rhs);
    let product = binary(
        rumoca_core::OpBinary::Mul,
        add(make_structured_var_ref("A"), make_structured_var_ref("B")),
        add(make_structured_var_ref("C"), make_structured_var_ref("D")),
    );
    flat.add_equation(flat::Equation {
        residual: binary(
            rumoca_core::OpBinary::Sub,
            Expression::Index {
                base: Box::new(make_structured_var_ref("Y")),
                subscripts: vec![rumoca_core::Subscript::Colon {
                    span: crate::test_support::test_span(),
                }],
                span: crate::test_support::test_span(),
            },
            product,
        ),
        span: crate::test_support::test_span(),
        origin: flat::EquationOrigin::ComponentEquation {
            component: "CompoundArrayProduct".to_string(),
        },
        scalar_count: 2,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("bare compound array operands must project as a matrix-vector product");

    assert_eq!(dae.continuous.equations.len(), 2);
    for (lane, equation) in dae.continuous.equations.iter().enumerate() {
        let row = i64::try_from(lane + 1).expect("two lanes fit i64");
        let Expression::Binary { lhs, rhs, .. } = &equation.rhs else {
            panic!("expected scalar residual");
        };
        assert_eq!(literal_subscripts(lhs), Some(("Y", vec![row])));
        let Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: first,
            rhs: second,
            ..
        } = rhs.as_ref()
        else {
            panic!("lane {row} must contain the complete two-term dot, got {rhs:?}");
        };
        assert_compound_dot_term(first, row, 1);
        assert_compound_dot_term(second, row, 2);
    }
}

#[test]
fn test_todae_preserves_scalar_scaling_of_vector_scalar_division() {
    for scalar_on_left in [true, false] {
        let mut flat = Model::new();
        declare_array(&mut flat, "V", &[2]);
        declare_array(&mut flat, "s", &[]);
        declare_array(&mut flat, "Y", &[2]);
        let vector = Expression::Index {
            base: Box::new(make_structured_var_ref("V")),
            subscripts: vec![rumoca_core::Subscript::Colon {
                span: crate::test_support::test_span(),
            }],
            span: crate::test_support::test_span(),
        };
        let quotient = binary(
            rumoca_core::OpBinary::Div,
            vector,
            make_structured_var_ref("s"),
        );
        let scalar = Expression::Literal {
            value: Literal::Real(2.0),
            span: crate::test_support::test_span(),
        };
        let product = if scalar_on_left {
            binary(rumoca_core::OpBinary::Mul, scalar, quotient)
        } else {
            binary(rumoca_core::OpBinary::Mul, quotient, scalar)
        };
        flat.add_equation(flat::Equation {
            residual: binary(
                rumoca_core::OpBinary::Sub,
                Expression::Index {
                    base: Box::new(make_structured_var_ref("Y")),
                    subscripts: vec![rumoca_core::Subscript::Colon {
                        span: crate::test_support::test_span(),
                    }],
                    span: crate::test_support::test_span(),
                },
                product,
            ),
            span: crate::test_support::test_span(),
            origin: flat::EquationOrigin::ComponentEquation {
                component: "ArrayScalarDivision".to_string(),
            },
            scalar_count: 2,
        });

        let dae = to_dae_with_options(
            &flat,
            ToDaeOptions {
                error_on_unbalanced: false,
            },
        )
        .expect("scalar scaling of vector/scalar division is legal ARR-030 input");

        assert_eq!(dae.continuous.equations.len(), 2);
        for (lane, equation) in dae.continuous.equations.iter().enumerate() {
            let index = i64::try_from(lane + 1).expect("two lanes fit i64");
            let Expression::Binary { lhs, rhs, .. } = &equation.rhs else {
                panic!("expected scalar residual");
            };
            assert_eq!(literal_subscripts(lhs), Some(("Y", vec![index])));
            let Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs,
                rhs,
                ..
            } = rhs.as_ref()
            else {
                panic!("expected preserved scalar product, got {rhs:?}");
            };
            let (scalar, quotient) = if scalar_on_left {
                (lhs.as_ref(), rhs.as_ref())
            } else {
                (rhs.as_ref(), lhs.as_ref())
            };
            assert!(matches!(
                scalar,
                Expression::Literal {
                    value: Literal::Real(2.0),
                    ..
                }
            ));
            let Expression::Binary {
                op: rumoca_core::OpBinary::Div,
                lhs: dividend,
                rhs: divisor,
                ..
            } = quotient
            else {
                panic!("expected indexed vector/scalar quotient, got {quotient:?}");
            };
            assert_eq!(literal_subscripts(dividend), Some(("V", vec![index])));
            assert_eq!(literal_subscripts(divisor), Some(("s", vec![])));
        }
    }
}

#[test]
fn test_todae_rejects_bare_divided_array_operands_in_matrix_product() {
    let mut flat = Model::new();
    for (name, dims) in [
        ("A", [2, 2].as_slice()),
        ("x", [2].as_slice()),
        ("s", [].as_slice()),
        ("t", [].as_slice()),
        ("Y", [2].as_slice()),
    ] {
        declare_array(&mut flat, name, dims);
    }
    let divided = |array, scalar| {
        binary(
            rumoca_core::OpBinary::Div,
            make_structured_var_ref(array),
            make_structured_var_ref(scalar),
        )
    };
    flat.add_equation(flat::Equation {
        residual: binary(
            rumoca_core::OpBinary::Sub,
            Expression::Index {
                base: Box::new(make_structured_var_ref("Y")),
                subscripts: vec![rumoca_core::Subscript::Colon {
                    span: crate::test_support::test_span(),
                }],
                span: crate::test_support::test_span(),
            },
            binary(
                rumoca_core::OpBinary::Mul,
                divided("A", "s"),
                divided("x", "t"),
            ),
        ),
        span: crate::test_support::test_span(),
        origin: flat::EquationOrigin::ComponentEquation {
            component: "BareDividedArrayProduct".to_string(),
        },
        scalar_count: 2,
    });

    let error = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("bare divided array operands must not be scalarized lane-wise");

    assert!(error.to_string().contains("unknown operand shape"));
    assert_eq!(error.source_span(), Some(crate::test_support::test_span()));
}
