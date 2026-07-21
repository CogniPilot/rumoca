use super::*;

fn column_slice(name: &str, column: i64) -> rumoca_core::Expression {
    let span = test_span();
    rumoca_core::Expression::Index {
        base: Box::new(var_ref(name)),
        subscripts: vec![
            rumoca_core::Subscript::generated_colon(span),
            rumoca_core::Subscript::generated_index(column, span),
        ],
        span,
    }
}

fn rotation_column_slice() -> rumoca_core::Expression {
    column_slice("rotation", 3)
}

fn booster_dot_names() -> Vec<String> {
    (1..=3)
        .flat_map(|lane| {
            [
                format!("rotation[{lane},3]"),
                format!("deck_normal_w[{lane}]"),
            ]
        })
        .collect()
}

#[test]
fn colon_slice_times_plain_vector_lowers_to_scalar_dot_product() {
    let array_dims = HashMap::from([
        ("rotation".to_string(), vec![3, 3]),
        ("deck_normal_w".to_string(), vec![3]),
        ("normal_basis".to_string(), vec![3, 2]),
    ]);

    let lowered = lower_colon_slice_dot_products(
        &mul(rotation_column_slice(), var_ref("deck_normal_w")),
        &array_dims,
    )
    .expect("proven vector product should lower");
    assert!(!matches!(lowered, rumoca_core::Expression::Array { .. }));
    assert_eq!(all_var_names(&lowered), booster_dot_names());

    let symmetric = lower_colon_slice_dot_products(
        &mul(var_ref("deck_normal_w"), rotation_column_slice()),
        &array_dims,
    )
    .expect("symmetric proven vector product should lower");
    let mut symmetric_names = all_var_names(&symmetric);
    let mut lowered_names = all_var_names(&lowered);
    symmetric_names.sort();
    lowered_names.sort();
    assert_eq!(symmetric_names, lowered_names);

    let slice_dot = lower_colon_slice_dot_products(
        &mul(rotation_column_slice(), column_slice("normal_basis", 2)),
        &array_dims,
    )
    .expect("two proven slices should lower");
    assert_eq!(
        all_var_names(&slice_dot),
        vec![
            "rotation[1,3]",
            "normal_basis[1,2]",
            "rotation[2,3]",
            "normal_basis[2,2]",
            "rotation[3,3]",
            "normal_basis[3,2]",
        ]
    );
}

#[test]
fn colon_slice_dot_product_requires_two_proven_equal_rank_one_vectors() {
    let span = test_span();
    let slice = |name: &str, subscripts| rumoca_core::Expression::Index {
        base: Box::new(var_ref(name)),
        subscripts,
        span,
    };
    let array_dims = HashMap::from([
        ("rotation".to_string(), vec![3, 3]),
        ("short".to_string(), vec![2]),
        ("matrix".to_string(), vec![3, 3]),
        ("cube".to_string(), vec![2, 2, 2]),
        ("vector4".to_string(), vec![4]),
        ("short_basis".to_string(), vec![2, 2]),
        ("gain".to_string(), vec![]),
    ]);

    for invalid in [
        mul(rotation_column_slice(), var_ref("short")),
        mul(rotation_column_slice(), var_ref("unknown")),
        mul(rotation_column_slice(), var_ref("matrix")),
        mul(
            rotation_column_slice(),
            function_call("unknownVector", vec![]),
        ),
        mul(rotation_column_slice(), column_slice("short_basis", 1)),
        mul(
            slice(
                "cube",
                vec![
                    rumoca_core::Subscript::generated_colon(span),
                    rumoca_core::Subscript::generated_colon(span),
                    rumoca_core::Subscript::generated_index(1, span),
                ],
            ),
            var_ref("vector4"),
        ),
        mul(
            rotation_column_slice(),
            rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Array {
                        elements: vec![int_lit(1), int_lit(2)],
                        is_matrix: false,
                        span,
                    },
                    rumoca_core::Expression::Array {
                        elements: vec![int_lit(3), int_lit(4)],
                        is_matrix: false,
                        span,
                    },
                ],
                is_matrix: true,
                span,
            },
        ),
        mul(
            rotation_column_slice(),
            rumoca_core::Expression::Array {
                elements: vec![rumoca_core::Expression::Array {
                    elements: vec![int_lit(1), int_lit(2), int_lit(3)],
                    is_matrix: false,
                    span,
                }],
                is_matrix: false,
                span,
            },
        ),
    ] {
        let lowered = lower_colon_slice_dot_products(&invalid, &array_dims)
            .expect("unsupported dot-product shape should remain representable");
        assert!(
            matches!(lowered, rumoca_core::Expression::Binary { .. }),
            "unsupported product lowered to {lowered:?}"
        );
    }

    for scalar in [int_lit(2), var_ref("gain")] {
        let scaled =
            lower_colon_slice_dot_products(&mul(rotation_column_slice(), scalar), &array_dims)
                .expect("slice scaling should remain elementwise");
        assert!(matches!(
            scaled,
            rumoca_core::Expression::Array { ref elements, .. } if elements.len() == 3
        ));
    }

    let elementwise = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::MulElem,
        lhs: Box::new(rotation_column_slice()),
        rhs: Box::new(var_ref("short")),
        span,
    };
    let lowered = lower_colon_slice_dot_products(&elementwise, &array_dims)
        .expect("MulElem must not become a dot product");
    assert!(matches!(
        lowered,
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::MulElem,
            ..
        }
    ));
}

#[test]
fn booster_nested_min_max_colon_slice_vector_product_is_scalar() {
    let mut dae = Dae::new();
    let span = test_span();
    for (name, dims) in [("rotation", vec![3, 3]), ("deck_normal_w", vec![3])] {
        let mut variable = dae::Variable::new(rumoca_core::VarName::new(name), span);
        variable.dims = dims;
        dae.variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), variable);
    }
    let clamp = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Min,
        args: vec![
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Max,
                args: vec![
                    mul(rotation_column_slice(), var_ref("deck_normal_w")),
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(-1.0),
                        span,
                    },
                ],
                span,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span,
            },
        ],
        span,
    };
    dae.continuous.equations.push(dae::Equation::residual(
        clamp,
        span,
        "body_up_surface_cosine = min(max(rotation[:, 3] * deck_normal_w, -1.0), 1.0)",
    ));

    scalarize_phantom_vector_equations(&mut dae).unwrap();

    let rumoca_core::Expression::BuiltinCall { args: min_args, .. } =
        &dae.continuous.equations[0].rhs
    else {
        panic!("expected outer min");
    };
    let rumoca_core::Expression::BuiltinCall { args: max_args, .. } = &min_args[0] else {
        panic!("expected nested max");
    };
    assert!(matches!(
        max_args[0],
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            ..
        }
    ));
    assert_eq!(all_var_names(&max_args[0]), booster_dot_names());
}
