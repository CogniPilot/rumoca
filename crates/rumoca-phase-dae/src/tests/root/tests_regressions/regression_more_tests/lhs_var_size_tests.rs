//! Regression coverage for left-hand-side variable size extraction, including conditional residual branches.

use super::*;

#[test]
fn test_extract_lhs_var_size_keeps_symbolic_tail_subscript_scalar_equation() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("medium_T[1].X"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("medium_T[1].X"),
            dims: vec![2],
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
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("medium_T[1].X[medium_T[1].nX]").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    assert_eq!(
        extract_lhs_var_size(&residual, &flat, &prefix_counts),
        Some(1)
    );
    assert_eq!(
        infer_equation_scalar_count(&residual, &flat, &prefix_counts),
        1
    );
}

#[test]
fn test_extract_lhs_var_size_multilayer_subscript_fallback_is_scalar() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("bus.signal"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("bus.signal"),
            dims: vec![3],
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
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("bus[1].signal[2]").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    assert_eq!(
        extract_lhs_var_size(&residual, &flat, &prefix_counts),
        Some(1)
    );
    assert_eq!(
        infer_equation_scalar_count(&residual, &flat, &prefix_counts),
        1
    );
}

#[test]
// SPEC_0021: Exception - single regression fixture for conditional residual branch sizing.
#[allow(clippy::too_many_lines)]
fn test_extract_lhs_var_size_conditional_residual_uses_branch_lhs_size() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("add.y"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("add.y"),
            dims: vec![],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("add.k"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("add.k"),
            dims: vec![2],
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("add.u"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("add.u"),
            dims: vec![2],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let residual = Expression::If {
        branches: vec![(
            Expression::Binary {
                op: rumoca_core::OpBinary::Gt,
                lhs: Box::new(Expression::BuiltinCall {
                    function: BuiltinFunction::Size,
                    args: vec![
                        Expression::VarRef {
                            name: VarName::new("add.u").into(),
                            subscripts: vec![],
                            span: crate::test_support::test_span(),
                        },
                        Expression::Literal {
                            value: Literal::Integer(1),
                            span: crate::test_support::test_span(),
                        },
                    ],
                    span: crate::test_support::test_span(),
                }),
                rhs: Box::new(Expression::Literal {
                    value: Literal::Integer(0),
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            },
            Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new("add.y").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                }),
                rhs: Box::new(Expression::Binary {
                    op: rumoca_core::OpBinary::Mul,
                    lhs: Box::new(Expression::VarRef {
                        name: VarName::new("add.k").into(),
                        subscripts: vec![],
                        span: crate::test_support::test_span(),
                    }),
                    rhs: Box::new(Expression::VarRef {
                        name: VarName::new("add.u").into(),
                        subscripts: vec![],
                        span: crate::test_support::test_span(),
                    }),
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            },
        )],
        else_branch: Box::new(Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: VarName::new("add.y").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(Expression::Literal {
                value: Literal::Integer(0),
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    assert_eq!(
        extract_lhs_var_size(&residual, &flat, &prefix_counts),
        Some(1),
        "conditional residual should keep scalar size from branch residual LHS"
    );
    assert_eq!(
        infer_equation_scalar_count(&residual, &flat, &prefix_counts),
        1,
        "conditional residual with vector dot-product branch should stay scalar"
    );
}
