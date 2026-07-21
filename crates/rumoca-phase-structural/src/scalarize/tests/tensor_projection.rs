use super::*;

#[test]
fn scalarize_array_field_of_untyped_function_result_selects_each_component() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(VarName::new("y"), variable("y", &[2]));

    let mut function = rumoca_core::Function::new("Path.sample", test_span());
    set_function_instance(&mut function, 5);
    function.add_output(rumoca_core::FunctionParam::new(
        "state",
        "Path.State",
        test_span(),
    ));
    dae_model
        .symbols
        .functions
        .insert(VarName::new("Path.sample"), function);

    let call = Expression::FunctionCall {
        name: structured_function_reference("Path.sample", test_span(), 5),
        args: vec![],
        is_constructor: false,
        span: test_span(),
    };
    let field = Expression::FieldAccess {
        base: Box::new(call.clone()),
        field: "firstDerivative".to_string(),
        span: test_span(),
    };
    dae_model
        .continuous
        .equations
        .push(eq("y", field.clone(), 2));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 2);
    for (offset, equation) in dae_model.continuous.equations.iter().enumerate() {
        assert_eq!(equation.lhs, lhs(&format!("y[{}]", offset + 1)));
        assert_eq!(
            equation.rhs,
            Expression::Index {
                base: Box::new(field.clone()),
                subscripts: vec![Subscript::generated_index(offset as i64 + 1, test_span())],
                span: test_span(),
            }
        );
    }
}

#[test]
fn scalarize_known_record_array_field_uses_selected_function_output_paths() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(VarName::new("y"), variable("y", &[2]));

    let mut constructor = rumoca_core::Function::new("Path.State", test_span());
    set_function_instance(&mut constructor, 6);
    constructor.def_id = Some(rumoca_core::DefId::new(60));
    constructor.is_constructor = true;
    constructor.add_input(
        rumoca_core::FunctionParam::new("firstDerivative", "Real", test_span()).with_dims(vec![2]),
    );
    dae_model
        .symbols
        .functions
        .insert(VarName::new("Path.State"), constructor);

    let mut function = rumoca_core::Function::new("Path.sample", test_span());
    set_function_instance(&mut function, 7);
    let mut state = rumoca_core::FunctionParam::new("state", "Path.State", test_span());
    state.type_class = Some(rumoca_core::ClassType::Record);
    state.type_def_id = Some(rumoca_core::DefId::new(60));
    function.add_output(state);
    dae_model
        .symbols
        .functions
        .insert(VarName::new("Path.sample"), function);

    let call = Expression::FunctionCall {
        name: structured_function_reference("Path.sample", test_span(), 7),
        args: vec![],
        is_constructor: false,
        span: test_span(),
    };
    dae_model.continuous.equations.push(eq(
        "y",
        Expression::FieldAccess {
            base: Box::new(call),
            field: "firstDerivative".to_string(),
            span: test_span(),
        },
        2,
    ));

    scalarize_equations(&mut dae_model).unwrap();

    for (offset, equation) in dae_model.continuous.equations.iter().enumerate() {
        assert_eq!(
            equation.rhs,
            Expression::FunctionCall {
                name: projected_function_reference(
                    &format!("Path.sample.state.firstDerivative[{}]", offset + 1),
                    test_span(),
                    7,
                    2,
                ),
                args: vec![],
                is_constructor: false,
                span: test_span(),
            }
        );
    }
}

#[test]
fn scalarize_vector_matrix_product_uses_column_dot_product() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("v"), variable("v", &[2]));
    dae_model
        .variables
        .parameters
        .insert(VarName::new("R"), variable("R", &[2, 3]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("y"), variable("y", &[3]));
    dae_model
        .continuous
        .equations
        .push(eq("y", mul_expr(var("v"), var("R")), 3));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 3);
    assert_eq!(dae_model.continuous.equations[0].lhs, lhs("y[1]"));
    assert_eq!(
        dae_model.continuous.equations[0].rhs,
        product_sum(&[("v", &[1], "R", &[1, 1]), ("v", &[2], "R", &[2, 1])])
    );
    assert_eq!(dae_model.continuous.equations[1].lhs, lhs("y[2]"));
    assert_eq!(
        dae_model.continuous.equations[1].rhs,
        product_sum(&[("v", &[1], "R", &[1, 2]), ("v", &[2], "R", &[2, 2])])
    );
    assert_eq!(dae_model.continuous.equations[2].lhs, lhs("y[3]"));
    assert_eq!(
        dae_model.continuous.equations[2].rhs,
        product_sum(&[("v", &[1], "R", &[1, 3]), ("v", &[2], "R", &[2, 3])])
    );
}

#[test]
fn scalarize_matrix_matrix_product_uses_row_column_dot_product() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("A"), variable("A", &[2, 2]));
    dae_model
        .variables
        .parameters
        .insert(VarName::new("B"), variable("B", &[2, 2]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("C"), variable("C", &[2, 2]));
    dae_model
        .continuous
        .equations
        .push(eq("C", mul_expr(var("A"), var("B")), 4));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 4);
    assert_eq!(dae_model.continuous.equations[0].lhs, lhs("C[1,1]"));
    assert_eq!(
        dae_model.continuous.equations[0].rhs,
        product_sum(&[("A", &[1, 1], "B", &[1, 1]), ("A", &[1, 2], "B", &[2, 1])])
    );
    assert_eq!(dae_model.continuous.equations[1].lhs, lhs("C[1,2]"));
    assert_eq!(
        dae_model.continuous.equations[1].rhs,
        product_sum(&[("A", &[1, 1], "B", &[1, 2]), ("A", &[1, 2], "B", &[2, 2])])
    );
    assert_eq!(dae_model.continuous.equations[2].lhs, lhs("C[2,1]"));
    assert_eq!(
        dae_model.continuous.equations[2].rhs,
        product_sum(&[("A", &[2, 1], "B", &[1, 1]), ("A", &[2, 2], "B", &[2, 1])])
    );
    assert_eq!(dae_model.continuous.equations[3].lhs, lhs("C[2,2]"));
    assert_eq!(
        dae_model.continuous.equations[3].rhs,
        product_sum(&[("A", &[2, 1], "B", &[1, 2]), ("A", &[2, 2], "B", &[2, 2])])
    );
}

#[test]
fn scalarize_transpose_matrix_vector_product_swaps_indices() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("A"), variable("A", &[2, 3]));
    dae_model
        .variables
        .parameters
        .insert(VarName::new("v"), variable("v", &[2]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("y"), variable("y", &[3]));
    dae_model
        .continuous
        .equations
        .push(eq("y", mul_expr(transpose(var("A")), var("v")), 3));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 3);
    assert_eq!(dae_model.continuous.equations[0].lhs, lhs("y[1]"));
    assert_eq!(
        dae_model.continuous.equations[0].rhs,
        product_sum(&[("A", &[1, 1], "v", &[1]), ("A", &[2, 1], "v", &[2])])
    );
    assert_eq!(dae_model.continuous.equations[1].lhs, lhs("y[2]"));
    assert_eq!(
        dae_model.continuous.equations[1].rhs,
        product_sum(&[("A", &[1, 2], "v", &[1]), ("A", &[2, 2], "v", &[2])])
    );
    assert_eq!(dae_model.continuous.equations[2].lhs, lhs("y[3]"));
    assert_eq!(
        dae_model.continuous.equations[2].rhs,
        product_sum(&[("A", &[1, 3], "v", &[1]), ("A", &[2, 3], "v", &[2])])
    );
}

#[test]
fn scalarize_transpose_as_array_equation_swaps_indices() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("A"), variable("A", &[2, 3]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("C"), variable("C", &[3, 2]));
    dae_model
        .continuous
        .equations
        .push(eq("C", transpose(var("A")), 6));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 6);
    assert_eq!(dae_model.continuous.equations[0].lhs, lhs("C[1,1]"));
    assert_eq!(dae_model.continuous.equations[0].rhs, var_idx("A", &[1, 1]));
    assert_eq!(dae_model.continuous.equations[1].lhs, lhs("C[1,2]"));
    assert_eq!(dae_model.continuous.equations[1].rhs, var_idx("A", &[2, 1]));
    assert_eq!(dae_model.continuous.equations[2].lhs, lhs("C[2,1]"));
    assert_eq!(dae_model.continuous.equations[2].rhs, var_idx("A", &[1, 2]));
    assert_eq!(dae_model.continuous.equations[3].lhs, lhs("C[2,2]"));
    assert_eq!(dae_model.continuous.equations[3].rhs, var_idx("A", &[2, 2]));
    assert_eq!(dae_model.continuous.equations[4].lhs, lhs("C[3,1]"));
    assert_eq!(dae_model.continuous.equations[4].rhs, var_idx("A", &[1, 3]));
    assert_eq!(dae_model.continuous.equations[5].lhs, lhs("C[3,2]"));
    assert_eq!(dae_model.continuous.equations[5].rhs, var_idx("A", &[2, 3]));
}

#[test]
fn scalarize_vector_dot_product_in_scalar_equation() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("a"), variable("a", &[3]));
    dae_model
        .variables
        .parameters
        .insert(VarName::new("b"), variable("b", &[3]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("s"), variable("s", &[]));
    dae_model
        .continuous
        .equations
        .push(eq("s", mul_expr(var("a"), var("b")), 1));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 1);
    assert_eq!(dae_model.continuous.equations[0].lhs, lhs("s"));
    assert_eq!(
        dae_model.continuous.equations[0].rhs,
        product_sum(&[
            ("a", &[1], "b", &[1]),
            ("a", &[2], "b", &[2]),
            ("a", &[3], "b", &[3]),
        ])
    );
}

#[test]
fn scalarize_column_slice_var_ref_projects_colon_subscript() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("A"), variable("A", &[3, 4]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("y"), variable("y", &[3]));
    dae_model.continuous.equations.push(eq(
        "y",
        var_sub(
            "A",
            vec![
                Subscript::generated_colon(rumoca_core::Span::DUMMY),
                Subscript::generated_index(2, rumoca_core::Span::DUMMY),
            ],
        ),
        3,
    ));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 3);
    assert_eq!(dae_model.continuous.equations[0].lhs, lhs("y[1]"));
    assert_eq!(dae_model.continuous.equations[0].rhs, var_idx("A", &[1, 2]));
    assert_eq!(dae_model.continuous.equations[1].lhs, lhs("y[2]"));
    assert_eq!(dae_model.continuous.equations[1].rhs, var_idx("A", &[2, 2]));
    assert_eq!(dae_model.continuous.equations[2].lhs, lhs("y[3]"));
    assert_eq!(dae_model.continuous.equations[2].rhs, var_idx("A", &[3, 2]));
}

#[test]
fn scalarize_sliced_vector_dot_product_in_scalar_equation() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("A"), variable("A", &[4, 3]));
    dae_model
        .variables
        .parameters
        .insert(VarName::new("B"), variable("B", &[3, 4]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("s"), variable("s", &[]));
    dae_model.continuous.equations.push(eq(
        "s",
        mul_expr(
            var_sub(
                "A",
                vec![
                    Subscript::generated_index(2, rumoca_core::Span::DUMMY),
                    Subscript::generated_colon(rumoca_core::Span::DUMMY),
                ],
            ),
            var_sub(
                "B",
                vec![
                    Subscript::generated_colon(rumoca_core::Span::DUMMY),
                    Subscript::generated_index(3, rumoca_core::Span::DUMMY),
                ],
            ),
        ),
        1,
    ));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 1);
    assert_eq!(dae_model.continuous.equations[0].lhs, lhs("s"));
    assert_eq!(
        dae_model.continuous.equations[0].rhs,
        product_sum(&[
            ("A", &[2, 1], "B", &[1, 3]),
            ("A", &[2, 2], "B", &[2, 3]),
            ("A", &[2, 3], "B", &[3, 3]),
        ])
    );
}

#[test]
fn scalarize_structural_range_slice_dot_product_in_scalar_equation() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("a"), variable("a", &[3]));
    let mut n = variable("n", &[]);
    n.start = Some(Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![
            var("a"),
            Expression::Literal {
                value: Literal::Integer(1),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        span: rumoca_core::Span::DUMMY,
    });
    n.is_tunable = false;
    dae_model.variables.parameters.insert(VarName::new("n"), n);
    dae_model
        .variables
        .states
        .insert(VarName::new("x"), variable("x", &[2]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("s"), variable("s", &[]));
    dae_model.continuous.equations.push(eq(
        "s",
        mul_expr(
            var_sub(
                "a",
                vec![Subscript::generated_expr(
                    Box::new(Expression::Range {
                        start: Box::new(Expression::Literal {
                            value: Literal::Integer(2),
                            span: rumoca_core::Span::DUMMY,
                        }),
                        step: None,
                        end: Box::new(var("n")),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    rumoca_core::Span::DUMMY,
                )],
            ),
            var("x"),
        ),
        1,
    ));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 1);
    assert_eq!(dae_model.continuous.equations[0].lhs, lhs("s"));
    // MLS §10.5 and §10.6.5: a structural range subscript selects an array
    // slice, and vector * vector is a scalar product.
    assert_eq!(
        dae_model.continuous.equations[0].rhs,
        product_sum(&[("a", &[2], "x", &[1]), ("a", &[3], "x", &[2])])
    );
}

#[test]
fn scalarize_singleton_structural_range_derivative_residual_projects_element() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(VarName::new("x"), variable("x", &[2]));
    let mut nx = variable("nx", &[]);
    nx.start = Some(Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![
            var("x"),
            Expression::Literal {
                value: Literal::Integer(1),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        span: rumoca_core::Span::DUMMY,
    });
    nx.is_tunable = false;
    dae_model
        .variables
        .parameters
        .insert(VarName::new("nx"), nx);
    dae_model.continuous.equations.push(Equation {
        lhs: None,
        rhs: sub_expr(
            der(var_sub(
                "x",
                vec![Subscript::generated_expr(
                    Box::new(Expression::Range {
                        start: Box::new(Expression::Literal {
                            value: Literal::Integer(2),
                            span: rumoca_core::Span::DUMMY,
                        }),
                        step: None,
                        end: Box::new(var("nx")),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    rumoca_core::Span::DUMMY,
                )],
            )),
            var_sub(
                "x",
                vec![Subscript::generated_expr(
                    Box::new(Expression::Range {
                        start: Box::new(Expression::Literal {
                            value: Literal::Integer(1),
                            span: rumoca_core::Span::DUMMY,
                        }),
                        step: None,
                        end: Box::new(sub_expr(
                            var("nx"),
                            Expression::Literal {
                                value: Literal::Integer(1),
                                span: rumoca_core::Span::DUMMY,
                            },
                        )),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    rumoca_core::Span::DUMMY,
                )],
            ),
        ),
        span: test_span(),
        origin: "singleton derivative slice".to_string(),
        scalar_count: 1,
    });

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 1);
    // MLS §8.3 and §10.5: a one-element array equation is still one scalar
    // equation over the selected element.
    assert_eq!(
        dae_model.continuous.equations[0].rhs,
        sub_expr(der(var_idx("x", &[2])), var_idx("x", &[1]))
    );
}

#[test]
fn scalarize_sliced_dot_product_in_synthetic_root_condition() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("A"), variable("A", &[4, 3]));
    dae_model
        .variables
        .parameters
        .insert(VarName::new("B"), variable("B", &[3, 4]));
    dae_model
        .events
        .synthetic_root_conditions
        .push(Expression::Binary {
            op: OpBinary::Lt,
            lhs: Box::new(mul_expr(
                var_sub(
                    "A",
                    vec![
                        Subscript::generated_index(2, rumoca_core::Span::DUMMY),
                        Subscript::generated_colon(rumoca_core::Span::DUMMY),
                    ],
                ),
                var_sub(
                    "B",
                    vec![
                        Subscript::generated_colon(rumoca_core::Span::DUMMY),
                        Subscript::generated_index(3, rumoca_core::Span::DUMMY),
                    ],
                ),
            )),
            rhs: Box::new(real(0.0)),
            span: rumoca_core::Span::DUMMY,
        });

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.events.synthetic_root_conditions.len(), 1);
    assert_eq!(
        dae_model.events.synthetic_root_conditions[0],
        Expression::Binary {
            op: OpBinary::Lt,
            lhs: Box::new(product_sum(&[
                ("A", &[2, 1], "B", &[1, 3]),
                ("A", &[2, 2], "B", &[2, 3]),
                ("A", &[2, 3], "B", &[3, 3]),
            ])),
            rhs: Box::new(real(0.0)),
            span: rumoca_core::Span::DUMMY,
        }
    );
}

#[test]
fn scalarize_cross_product_uses_vector_components() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("a"), variable("a", &[3]));
    dae_model
        .variables
        .parameters
        .insert(VarName::new("b"), variable("b", &[3]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("c"), variable("c", &[3]));
    dae_model
        .continuous
        .equations
        .push(eq("c", cross(var("a"), var("b")), 3));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 3);
    assert_eq!(dae_model.continuous.equations[0].lhs, lhs("c[1]"));
    assert_eq!(
        dae_model.continuous.equations[0].rhs,
        sub_expr(
            mul_expr(var_idx("a", &[2]), var_idx("b", &[3])),
            mul_expr(var_idx("a", &[3]), var_idx("b", &[2])),
        )
    );
    assert_eq!(dae_model.continuous.equations[1].lhs, lhs("c[2]"));
    assert_eq!(
        dae_model.continuous.equations[1].rhs,
        sub_expr(
            mul_expr(var_idx("a", &[3]), var_idx("b", &[1])),
            mul_expr(var_idx("a", &[1]), var_idx("b", &[3])),
        )
    );
    assert_eq!(dae_model.continuous.equations[2].lhs, lhs("c[3]"));
    assert_eq!(
        dae_model.continuous.equations[2].rhs,
        sub_expr(
            mul_expr(var_idx("a", &[1]), var_idx("b", &[2])),
            mul_expr(var_idx("a", &[2]), var_idx("b", &[1])),
        )
    );
}

#[test]
fn scalarize_residual_column_slice_lhs_infers_targets_from_array_ir() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(VarName::new("M"), variable("M", &[3, 4]));
    dae_model
        .variables
        .parameters
        .insert(VarName::new("r"), variable("r", &[3, 4]));
    dae_model
        .variables
        .parameters
        .insert(VarName::new("f"), variable("f", &[3, 4]));
    dae_model.continuous.equations.push(Equation {
        lhs: None,
        rhs: sub_expr(
            var_sub(
                "M",
                vec![
                    Subscript::generated_colon(rumoca_core::Span::DUMMY),
                    Subscript::generated_index(2, rumoca_core::Span::DUMMY),
                ],
            ),
            cross(
                var_sub(
                    "r",
                    vec![
                        Subscript::generated_colon(rumoca_core::Span::DUMMY),
                        Subscript::generated_index(2, rumoca_core::Span::DUMMY),
                    ],
                ),
                var_sub(
                    "f",
                    vec![
                        Subscript::generated_colon(rumoca_core::Span::DUMMY),
                        Subscript::generated_index(2, rumoca_core::Span::DUMMY),
                    ],
                ),
            ),
        ),
        span: test_span(),
        origin: "slice residual".to_string(),
        scalar_count: 4,
    });

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 3);
    assert_eq!(
        dae_model.continuous.equations[0].rhs,
        sub_expr(
            var_idx("M", &[1, 2]),
            sub_expr(
                mul_expr(var_idx("r", &[2, 2]), var_idx("f", &[3, 2])),
                mul_expr(var_idx("r", &[3, 2]), var_idx("f", &[2, 2])),
            ),
        )
    );
    assert_eq!(
        dae_model.continuous.equations[1].rhs,
        sub_expr(
            var_idx("M", &[2, 2]),
            sub_expr(
                mul_expr(var_idx("r", &[3, 2]), var_idx("f", &[1, 2])),
                mul_expr(var_idx("r", &[1, 2]), var_idx("f", &[3, 2])),
            ),
        )
    );
    assert_eq!(
        dae_model.continuous.equations[2].rhs,
        sub_expr(
            var_idx("M", &[3, 2]),
            sub_expr(
                mul_expr(var_idx("r", &[1, 2]), var_idx("f", &[2, 2])),
                mul_expr(var_idx("r", &[2, 2]), var_idx("f", &[1, 2])),
            ),
        )
    );
}

#[test]
fn scalarize_matrix_vector_derivative_residual_uses_expression_shape() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("J"), variable("J", &[3, 3]));
    dae_model
        .variables
        .states
        .insert(VarName::new("omega"), variable("omega", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(VarName::new("M_body"), variable("M_body", &[3]));

    // MLS §10.6 / SPEC_0019: array equations scalarize by expression shape.
    // A residual `J * der(omega) - M_body` must produce one derivative row per
    // vector component even if stale metadata says there is only one row.
    dae_model.continuous.equations.push(Equation {
        lhs: None,
        rhs: sub_expr(mul_expr(var("J"), der(var("omega"))), var("M_body")),
        span: Span::DUMMY,
        origin: "matrix derivative residual".to_string(),
        scalar_count: 1,
    });

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 3);
    for idx in 1..=3 {
        assert!(
            expr_contains_der_var_idx(
                &dae_model.continuous.equations[idx as usize - 1].rhs,
                "omega",
                idx
            ),
            "row {idx} should contain der(omega[{idx}])"
        );
    }
}

#[test]
fn scalarize_matrix_vector_derivative_residual_preserves_qualified_state_name() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("vehicle.J"), variable("vehicle.J", &[3, 3]));
    dae_model.variables.states.insert(
        VarName::new("vehicle.omega"),
        variable("vehicle.omega", &[3]),
    );
    dae_model.variables.algebraics.insert(
        VarName::new("vehicle.M_body"),
        variable("vehicle.M_body", &[3]),
    );

    // MLS §10.6 / SPEC_0019: scalarizing an array equation must project the
    // existing IR variable reference. Qualified component paths are part of
    // the Modelica name and must not be stripped while projecting der().
    dae_model.continuous.equations.push(Equation {
        lhs: None,
        rhs: sub_expr(
            mul_expr(var("vehicle.J"), der(var("vehicle.omega"))),
            var("vehicle.M_body"),
        ),
        span: Span::DUMMY,
        origin: "qualified matrix derivative residual".to_string(),
        scalar_count: 1,
    });

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 3);
    for idx in 1..=3 {
        assert!(
            expr_contains_der_var_idx(
                &dae_model.continuous.equations[idx as usize - 1].rhs,
                "vehicle.omega",
                idx
            ),
            "row {idx} should contain der(vehicle.omega[{idx}])"
        );
    }
}

#[test]
fn scalarize_matrix_matrix_derivative_residual_preserves_derivative_lhs_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(VarName::new("R"), variable("R", &[3, 3]));
    dae_model
        .variables
        .parameters
        .insert(VarName::new("skew"), variable("skew", &[3, 3]));

    // MLS §10.6: `der(R) = R * skew` is an array equation. Scalarization must
    // keep each residual row owned by the corresponding state derivative.
    dae_model.continuous.equations.push(Equation {
        lhs: None,
        rhs: sub_expr(der(var("R")), mul_expr(var("R"), var("skew"))),
        span: Span::DUMMY,
        origin: "matrix derivative product residual".to_string(),
        scalar_count: 1,
    });

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 9);
    for row in 1..=3 {
        for col in 1..=3 {
            let eq_idx = (row - 1) * 3 + (col - 1);
            assert_eq!(
                dae_model.continuous.equations[eq_idx].rhs,
                sub_expr(
                    der(var_idx("R", &[row as i64, col as i64])),
                    product_sum(&[
                        ("R", &[row as i64, 1], "skew", &[1, col as i64]),
                        ("R", &[row as i64, 2], "skew", &[2, col as i64]),
                        ("R", &[row as i64, 3], "skew", &[3, col as i64])
                    ])
                ),
                "row {row}, col {col} should preserve der(R[{row},{col}])"
            );
        }
    }
}

#[test]
fn scalarize_lowers_indexed_matrix_vector_product_base() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("R"), variable("R", &[3, 3]));
    dae_model
        .variables
        .states
        .insert(VarName::new("omega"), variable("omega", &[3]));

    dae_model
        .continuous
        .equations
        .push(eq("y", index(mul_expr(var("R"), var("omega")), &[2]), 1));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 1);
    assert_eq!(
        dae_model.continuous.equations[0].rhs,
        product_sum(&[
            ("R", &[2, 1], "omega", &[1]),
            ("R", &[2, 2], "omega", &[2]),
            ("R", &[2, 3], "omega", &[3])
        ])
    );
}
