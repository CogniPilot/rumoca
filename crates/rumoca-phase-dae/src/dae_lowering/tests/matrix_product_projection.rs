use super::*;

#[test]
fn bare_negative_dimensions_propagate_from_projection_entry() {
    let dimensions = HashMap::from([("A".to_string(), vec![-1]), ("B".to_string(), vec![-1])]);
    let expression = mul(var_ref("A"), var_ref("B"));
    let error = Projector(&dimensions, &IndexMap::new())
        .project(&expression, 0, &[])
        .expect_err("invalid bare operands must fail closed during projection entry");

    assert!(error.to_string().contains("negative dimension"));
    assert_eq!(error.source_span(), Some(test_span()));
}

#[test]
fn scalar_product_sum_ignores_surrounding_equation_lane() {
    let dimensions = HashMap::from([
        ("p".to_string(), vec![3]),
        ("R".to_string(), vec![3, 3]),
        ("leg_r_b".to_string(), vec![3, 4]),
    ]);
    let indexed = |name, subscripts| rumoca_core::Expression::Index {
        base: Box::new(var_ref(name)),
        subscripts,
        span: test_span(),
    };
    let index = |value| rumoca_core::Subscript::Index {
        value,
        span: test_span(),
    };
    let colon = || rumoca_core::Subscript::Colon { span: test_span() };
    let expression = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(indexed("p", vec![index(3)])),
        rhs: Box::new(mul(
            indexed("R", vec![index(3), colon()]),
            indexed("leg_r_b", vec![colon(), index(2)]),
        )),
        span: test_span(),
    };

    let projected = Projector(&dimensions, &IndexMap::new())
        .project(&expression, 1, &[])
        .expect("a scalar product result must not consume the surrounding equation lane")
        .expect("the nested vector product must be projected");

    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        rhs,
        ..
    } = projected
    else {
        panic!("expected scalar sum projection");
    };
    assert_eq!(
        all_var_names(&rhs),
        [
            "R[3,1]",
            "leg_r_b[1,2]",
            "R[3,2]",
            "leg_r_b[2,2]",
            "R[3,3]",
            "leg_r_b[3,2]",
        ]
    );
}
