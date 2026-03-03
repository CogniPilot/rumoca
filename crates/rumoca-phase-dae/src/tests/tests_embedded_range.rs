use super::*;

#[test]
fn test_classify_equations_embedded_range_zero_dimension_is_elided() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("ports_mXi_flow"),
        flat::Variable {
            name: VarName::new("ports_mXi_flow"),
            dims: vec![1, 0],
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(make_var_ref("ports_mXi_flow[1,:]")),
            rhs: Box::new(flat::Expression::Literal(Literal::Integer(0))),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
        // Flatten may conservatively carry scalar_count=1 for this form.
        scalar_count: 1,
    });

    let mut dae = Dae::new();
    dae.algebraics.insert(
        VarName::new("ports_mXi_flow"),
        Variable::new(VarName::new("ports_mXi_flow")),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts);

    assert!(
        dae.f_x.is_empty(),
        "embedded range over a zero-sized dimension must contribute zero scalar equations"
    );
}
