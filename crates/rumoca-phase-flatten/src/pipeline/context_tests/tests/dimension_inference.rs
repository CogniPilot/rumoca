use super::*;

#[test]
fn test_infer_array_dimensions_1d() {
    // {0} -> [1]
    let expr = Expression::Array {
        elements: vec![Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: test_span(),
        }],
        is_matrix: false,
        span: test_span(),
    };
    assert_eq!(
        infer_array_dimensions_checked(&expr).unwrap(),
        Some(vec![1])
    );

    // {1, 2, 3} -> [3]
    let expr = Expression::Array {
        elements: vec![
            Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: test_span(),
            },
            Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: test_span(),
            },
            Expression::Literal {
                value: rumoca_core::Literal::Integer(3),
                span: test_span(),
            },
        ],
        is_matrix: false,
        span: test_span(),
    };
    assert_eq!(
        infer_array_dimensions_checked(&expr).unwrap(),
        Some(vec![3])
    );

    // {} -> [0]
    let expr = Expression::Array {
        elements: vec![],
        is_matrix: false,
        span: test_span(),
    };
    assert_eq!(
        infer_array_dimensions_checked(&expr).unwrap(),
        Some(vec![0])
    );
}

#[test]
fn test_infer_array_dimensions_2d_matrix() {
    // {{1, 2}, {3, 4}} -> [2, 2]
    let expr = Expression::Array {
        elements: vec![
            Expression::Array {
                elements: vec![
                    Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: test_span(),
                    },
                    Expression::Literal {
                        value: rumoca_core::Literal::Integer(2),
                        span: test_span(),
                    },
                ],
                is_matrix: false,
                span: test_span(),
            },
            Expression::Array {
                elements: vec![
                    Expression::Literal {
                        value: rumoca_core::Literal::Integer(3),
                        span: test_span(),
                    },
                    Expression::Literal {
                        value: rumoca_core::Literal::Integer(4),
                        span: test_span(),
                    },
                ],
                is_matrix: false,
                span: test_span(),
            },
        ],
        is_matrix: true,
        span: test_span(),
    };
    assert_eq!(
        infer_array_dimensions_checked(&expr).unwrap(),
        Some(vec![2, 2])
    );
}

#[test]
fn test_infer_array_dimensions_single_row_matrix() {
    // [1, 2] -> [1, 2]
    let expr = Expression::Array {
        elements: vec![
            Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: test_span(),
            },
            Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: test_span(),
            },
        ],
        is_matrix: true,
        span: test_span(),
    };
    assert_eq!(
        infer_array_dimensions_checked(&expr).unwrap(),
        Some(vec![1, 2])
    );
}

#[test]
fn test_infer_array_dimensions_non_array() {
    // Scalar literal -> None
    let expr = Expression::Literal {
        value: rumoca_core::Literal::Integer(5),
        span: test_span(),
    };
    assert_eq!(infer_array_dimensions_checked(&expr).unwrap(), None);

    // flat::Variable reference -> None
    let expr = Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            "x",
            core_component_ref(&[("x", DefId::new(1))]),
        ),
        subscripts: vec![],
        span: test_span(),
    };
    assert_eq!(infer_array_dimensions_checked(&expr).unwrap(), None);
}

#[test]
fn malformed_dimension_ir_returns_a_typed_evaluation_error() {
    let mut flat = flat::Model::default();
    let child_name = rumoca_core::VarName::new("parent.field");
    flat.add_variable(
        child_name.clone(),
        flat::Variable {
            name: child_name,
            binding: Some(Expression::Empty { span: test_span() }),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let mut overlay = InstanceOverlay::default();
    overlay.components.insert(
        InstanceId::new(1),
        InstanceData {
            instance_id: InstanceId::new(1),
            qualified_name: QualifiedName::from_dotted("parent"),
            dims: vec![2],
            is_primitive: false,
            ..Default::default()
        },
    );

    let error = propagate_unexpanded_record_array_dims(&mut flat, &overlay)
        .expect_err("malformed child binding must fail dimension recovery");

    assert!(matches!(
        error,
        rumoca_eval_flat::constant::EvalError::InvalidSemanticIr { span, .. }
            if span == test_span()
    ));
}

#[test]
fn concrete_dimensions_replace_unknown_same_rank_dimensions() {
    assert!(dims_are_better(&[4], &[0]));
    assert!(dims_are_better(&[3, 2], &[3, 0]));
    assert!(dims_are_better(&[8, 2], &[1, 2]));
    assert!(!dims_are_better(&[0], &[4]));
    assert!(!dims_are_better(&[1, 2], &[8, 2]));
}
