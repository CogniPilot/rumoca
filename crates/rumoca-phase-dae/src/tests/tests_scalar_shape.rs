use super::*;

#[test]
fn test_classify_equations_prefers_lhs_scalar_shape_over_flat_scalar_count() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("x"),
        flat::Variable {
            name: VarName::new("x"),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("a"),
        flat::Variable {
            name: VarName::new("a"),
            dims: vec![4],
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("b"),
        flat::Variable {
            name: VarName::new("b"),
            dims: vec![4],
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(make_var_ref("x")),
            rhs: Box::new(flat::Expression::Binary {
                op: ast::OpBinary::Mul(ast::Token::default()),
                lhs: Box::new(make_var_ref("a")),
                rhs: Box::new(make_var_ref("b")),
            }),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
        // Simulate conservative flatten scalarization from array varrefs.
        scalar_count: 4,
    });

    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts);

    assert_eq!(dae.f_x.len(), 1);
    assert_eq!(
        dae.f_x[0].scalar_count, 1,
        "scalar LHS should force a single scalar equation even when flat carried array-sized scalar_count"
    );
}
