//! Regression coverage for scalar-count inference of array, vector, and record-constructor residuals.

use super::*;

#[test]
fn test_infer_scalar_count_single_element_array_lhs_is_scalar() {
    // Reproduces `{0} = Frames.Quaternions.orientationConstraint(body.Q)`.
    // The argument `body.Q` is Real[4], but the equation is scalar.
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("body.Q"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("body.Q"),
            dims: vec![4],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::Array {
            elements: vec![Expression::Literal {
                value: Literal::Integer(0),
                span: crate::test_support::test_span(),
            }],
            is_matrix: false,
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::FunctionCall {
            name: VarName::new("Frames.Quaternions.orientationConstraint").into(),
            args: vec![Expression::VarRef {
                name: VarName::new("body.Q").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }],
            is_constructor: false,
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 1,
        "single-element array LHS should force scalar count 1"
    );
}

#[test]
fn test_infer_scalar_count_array_lhs_der_array_plus_scalar() {
    // Reproduces equations like:
    // {{der(x)}, {xn}} = {{x1dot}, {x}}
    // where x is Real[10], so scalar count is 10 + 1 = 11.
    let mut flat = Model::new();
    let span = crate::test_support::test_span();
    flat.add_variable(
        VarName::new("x"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("x"),
            dims: vec![10],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(span)
        }),
    );
    flat.add_variable(
        VarName::new("xn"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("xn"),
            dims: vec![],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(span)
        }),
    );
    flat.add_variable(
        VarName::new("x1dot"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("x1dot"),
            dims: vec![],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(span)
        }),
    );

    let lhs = Expression::Array {
        elements: vec![
            Expression::Array {
                elements: vec![Expression::BuiltinCall {
                    function: BuiltinFunction::Der,
                    args: vec![Expression::VarRef {
                        name: VarName::new("x").into(),
                        subscripts: vec![],
                        span,
                    }],
                    span,
                }],
                is_matrix: false,
                span,
            },
            Expression::Array {
                elements: vec![Expression::VarRef {
                    name: VarName::new("xn").into(),
                    subscripts: vec![],
                    span,
                }],
                is_matrix: false,
                span,
            },
        ],
        is_matrix: true,
        span,
    };
    let rhs = Expression::Array {
        elements: vec![
            Expression::Array {
                elements: vec![Expression::VarRef {
                    name: VarName::new("x1dot").into(),
                    subscripts: vec![],
                    span,
                }],
                is_matrix: false,
                span,
            },
            Expression::Array {
                elements: vec![Expression::VarRef {
                    name: VarName::new("x").into(),
                    subscripts: vec![],
                    span,
                }],
                is_matrix: false,
                span,
            },
        ],
        is_matrix: true,
        span,
    };

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 11,
        "der(array) inside array LHS should contribute the array scalar size"
    );
}

#[test]
fn test_infer_scalar_count_vector_dot_residual_is_scalar() {
    let mut flat = Model::new();
    for name in ["a", "b", "s"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                dims: if name == "s" { vec![] } else { vec![3] },
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // Residual for equation: 0 = a*b - s
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new("a").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                rhs: Box::new(Expression::VarRef {
                    name: VarName::new("b").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(Expression::VarRef {
                name: VarName::new("s").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 1,
        "vector dot-product residual should count as scalar equation"
    );
}

#[test]
fn test_infer_scalar_count_vector_matrix_vector_residual_is_scalar() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("constraint.ex_a"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("constraint.ex_a"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("constraint.R_rel.T"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("constraint.R_rel.T"),
            dims: vec![3, 3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("constraint.e"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("constraint.e"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Residual for equation:
    // 0 = ((constraint.ex_a * constraint.R_rel.T) * constraint.e)
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new("constraint.ex_a").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                rhs: Box::new(Expression::VarRef {
                    name: VarName::new("constraint.R_rel.T").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(Expression::VarRef {
                name: VarName::new("constraint.e").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 1,
        "vector*matrix*vector residual should count as a scalar equation"
    );
}

#[test]
fn test_infer_scalar_count_zero_equals_vector_stays_vector() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("v"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("v"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Residual for equation: 0 = v
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("v").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "vector residual with scalar zero lhs should remain vector-sized"
    );
}

#[test]
fn test_infer_scalar_count_record_constructor_lhs_uses_constructor_fields() {
    let mut flat = Model::new();
    let mut complex = rumoca_core::Function::new("Complex", crate::test_support::test_span());
    complex.is_constructor = true;
    complex.add_input(rumoca_core::FunctionParam::new(
        "re",
        "Real",
        crate::test_support::test_span(),
    ));
    complex.add_input(rumoca_core::FunctionParam::new(
        "im",
        "Real",
        crate::test_support::test_span(),
    ));
    flat.add_function(complex);

    for name in ["pin_p.i.re", "pin_p.i.im", "pin_n.i.re", "pin_n.i.im"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let zero_complex = Expression::FunctionCall {
        name: VarName::new("Complex").into(),
        args: vec![
            Expression::Literal {
                value: Literal::Integer(0),
                span: crate::test_support::test_span(),
            },
            Expression::Literal {
                value: Literal::Integer(0),
                span: crate::test_support::test_span(),
            },
        ],
        is_constructor: true,
        span: crate::test_support::test_span(),
    };
    let current_sum = Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(make_var_ref("pin_p.i")),
        rhs: Box::new(make_var_ref("pin_n.i")),
        span: crate::test_support::test_span(),
    };
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(zero_complex),
        rhs: Box::new(current_sum),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 2,
        "record constructor equations should count constructor field lanes"
    );
}

#[test]
fn test_infer_scalar_count_elementwise_mul_residual_is_vector() {
    let mut flat = Model::new();
    for name in ["a", "b", "c"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                dims: vec![3],
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // Residual for equation: 0 = a .* b - c
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(Expression::Binary {
                op: rumoca_core::OpBinary::MulElem,
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new("a").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                rhs: Box::new(Expression::VarRef {
                    name: VarName::new("b").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(Expression::VarRef {
                name: VarName::new("c").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "element-wise vector multiply should remain vector-sized"
    );
}

#[test]
fn test_infer_scalar_count_subscripted_derivative_lhs_is_scalar() {
    // Reproduces `Modelica.Blocks.Continuous.TransferFunction`:
    //   der(x_scaled[1]) = (-a[2:na]*x_scaled + a_end*u)/a[1];
    // The derivative target selects one element of a Real[2] state, so the row
    // is one scalar equation, not `size(x_scaled)` equations.
    let mut flat = Model::new();
    let span = crate::test_support::test_span();
    flat.add_variable(
        VarName::new("Hw.x_scaled"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("Hw.x_scaled"),
            dims: vec![2],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(span)
        }),
    );
    flat.add_variable(
        VarName::new("Hw.u"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("Hw.u"),
            dims: vec![],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(span)
        }),
    );

    // Residual for equation: der(Hw.x_scaled[1]) - Hw.u = 0
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args: vec![Expression::VarRef {
                name: VarName::new("Hw.x_scaled").into(),
                subscripts: vec![rumoca_core::Subscript::Index { value: 1, span }],
                span,
            }],
            span,
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("Hw.u").into(),
            subscripts: vec![],
            span,
        }),
        span,
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 1,
        "der(x[1]) selects one element, so the equation is a single scalar row"
    );
}

#[test]
fn test_infer_scalar_count_range_subscripted_derivative_lhs_uses_range_length() {
    // `der(x_scaled[2:3])` over a Real[4] state is two scalar equations.
    let mut flat = Model::new();
    let span = crate::test_support::test_span();
    flat.add_variable(
        VarName::new("f.x"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("f.x"),
            dims: vec![4],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(span)
        }),
    );

    let range = Expression::Range {
        start: Box::new(Expression::Literal {
            value: Literal::Integer(2),
            span,
        }),
        step: None,
        end: Box::new(Expression::Literal {
            value: Literal::Integer(3),
            span,
        }),
        span,
    };
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args: vec![Expression::VarRef {
                name: VarName::new("f.x").into(),
                subscripts: vec![rumoca_core::Subscript::Expr {
                    expr: Box::new(range),
                    span,
                }],
                span,
            }],
            span,
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span,
        }),
        span,
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 2,
        "der(x[2:3]) constrains exactly the two selected elements"
    );
}
