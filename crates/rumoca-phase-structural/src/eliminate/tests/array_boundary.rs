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
                name: reference("M"),
                subscripts: vec![
                    rumoca_core::Subscript::generated_colon(test_span()),
                    rumoca_core::Subscript::generated_index(2, test_span()),
                ],
                span: test_span(),
            }),
            rhs: Box::new(var_ref("rhs")),
            span: test_span(),
        },
        test_span(),
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
fn test_boundary_keeps_index_projection_unknowns_before_scalarization() {
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
            lhs: Box::new(Expression::Index {
                base: Box::new(var_ref("M")),
                subscripts: vec![
                    rumoca_core::Subscript::generated_colon(test_span()),
                    rumoca_core::Subscript::generated_index(2, test_span()),
                ],
                span: test_span(),
            }),
            rhs: Box::new(var_ref("rhs")),
            span: test_span(),
        },
        test_span(),
        "index projection residual",
        3,
    ));

    eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert_eq!(
        dae.continuous.equations.len(),
        1,
        "index projections must remain for scalarization"
    );
}

#[test]
fn test_orphan_cleanup_keeps_sliced_aggregate_unknown() {
    let mut dae = Dae::new();
    let mut matrix = component_var("M");
    matrix.dims = vec![3, 4];
    dae.variables.algebraics.insert(VarName::new("M"), matrix);
    dae.continuous.equations.push(dae::Equation::residual_array(
        Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::VarRef {
                name: reference("M"),
                subscripts: vec![
                    rumoca_core::Subscript::generated_colon(test_span()),
                    rumoca_core::Subscript::generated_index(2, test_span()),
                ],
                span: test_span(),
            }),
            rhs: Box::new(array(vec![real(1.0), real(2.0), real(3.0)])),
            span: test_span(),
        },
        test_span(),
        "sliced aggregate definition",
        3,
    ));

    structural_ok(drop_unreferenced_continuous_unknowns(&mut dae));

    assert!(dae.variables.algebraics.contains_key(&VarName::new("M")));
}
