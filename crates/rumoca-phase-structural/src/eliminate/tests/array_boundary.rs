use super::*;

#[test]
fn test_boundary_keeps_array_slice_unknowns_before_scalarization() {
    let mut dae = Dae::new();
    let mut matrix = component_var("M");
    matrix.dims = vec![3, 4];
    dae.variables.algebraics.insert(VarName::new("M"), matrix);
    let mut rhs = component_var("rhs");
    rhs.dims = vec![3];
    dae.variables.algebraics.insert(VarName::new("rhs"), rhs);
    dae.continuous.equations.push(dae::Equation::residual_array(
        Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::VarRef {
                name: rumoca_core::Reference::new("M"),
                subscripts: vec![
                    rumoca_core::Subscript::generated_colon(rumoca_core::Span::DUMMY),
                    rumoca_core::Subscript::generated_index(2, rumoca_core::Span::DUMMY),
                ],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(var_ref("rhs")),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "array slice residual",
        3,
    ));

    eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert_eq!(
        dae.continuous.equations.len(),
        1,
        "array slice row must remain for scalarization"
    );
}

#[test]
fn test_boundary_resolves_constant_expression_subscript_to_scalarized_unknown() {
    let mut dae = Dae::new();
    let mut n = component_var("N");
    n.start = Some(Expression::Literal {
        value: Literal::Integer(10),
        span: test_span(),
    });
    dae.variables.parameters.insert(VarName::new("N"), n);
    dae.variables
        .algebraics
        .insert(VarName::new("Q[11]"), component_var("Q[11]"));

    let subscript_expr = Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(var_ref("N")),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(1),
            span: test_span(),
        }),
        span: test_span(),
    };
    dae.continuous.equations.push(residual(
        var_ref_with_subscript_expr("Q", subscript_expr),
        real(0.0),
        1,
        "Q boundary",
    ));

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert_eq!(result.n_eliminated, 1);
    assert!(dae.continuous.equations.is_empty());
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("Q[11]"))
    );
}

fn var_ref_with_subscript_expr(name: &str, expr: Expression) -> Expression {
    Expression::VarRef {
        name: reference(name),
        subscripts: vec![rumoca_core::Subscript::Expr {
            expr: Box::new(expr),
            span: test_span(),
        }],
        span: test_span(),
    }
}
