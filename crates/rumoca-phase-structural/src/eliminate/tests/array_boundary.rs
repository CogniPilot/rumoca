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
