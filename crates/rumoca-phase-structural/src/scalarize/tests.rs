use super::*;
use rumoca_core::Span;

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: VarName::new(name),
        subscripts: Vec::new(),
    }
}

fn var_idx(name: &str, indices: &[i64]) -> Expression {
    Expression::VarRef {
        name: VarName::new(name),
        subscripts: indices.iter().copied().map(Subscript::Index).collect(),
    }
}

fn var_sub(name: &str, subscripts: Vec<Subscript>) -> Expression {
    Expression::VarRef {
        name: VarName::new(name),
        subscripts,
    }
}

fn real(value: f64) -> Expression {
    Expression::Literal(Literal::Real(value))
}

fn variable(name: &str, dims: &[i64]) -> dae::Variable {
    let mut variable = dae::Variable::new(VarName::new(name));
    variable.dims = dims.to_vec();
    variable
}

fn eq(lhs: &str, rhs: Expression, scalar_count: usize) -> Equation {
    Equation::explicit_with_scalar_count(
        VarName::new(lhs),
        rhs,
        Span::DUMMY,
        "scalarize test",
        scalar_count,
    )
}

fn product_sum(terms: &[(&str, &[i64], &str, &[i64])]) -> Expression {
    sum_terms(
        terms.iter().map(|(lhs, lhs_idx, rhs, rhs_idx)| {
            mul_expr(var_idx(lhs, lhs_idx), var_idx(rhs, rhs_idx))
        }),
    )
}

fn transpose(expr: Expression) -> Expression {
    Expression::BuiltinCall {
        function: dae::BuiltinFunction::Transpose,
        args: vec![expr],
    }
}

fn cross(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BuiltinCall {
        function: dae::BuiltinFunction::Cross,
        args: vec![lhs, rhs],
    }
}

#[test]
fn build_output_names_orders_states_algebraics_outputs_and_expands_arrays() {
    let mut dae_model = dae::Dae::default();

    let mut x = dae::Variable::new(dae::VarName::new("x"));
    x.dims = vec![2];
    dae_model.states.insert(dae::VarName::new("x"), x);
    dae_model.states.insert(
        dae::VarName::new("v"),
        dae::Variable::new(dae::VarName::new("v")),
    );

    dae_model.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );

    let mut y = dae::Variable::new(dae::VarName::new("y"));
    y.dims = vec![2];
    dae_model.outputs.insert(dae::VarName::new("y"), y);

    let names = build_output_names(&dae_model);
    assert_eq!(
        names,
        vec![
            "x[1]".to_string(),
            "x[2]".to_string(),
            "v".to_string(),
            "z".to_string(),
            "y[1]".to_string(),
            "y[2]".to_string(),
        ]
    );
}

#[test]
fn build_output_names_expands_matrix_indices_row_major() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .outputs
        .insert(VarName::new("C"), variable("C", &[2, 2]));

    let names = build_output_names(&dae_model);
    assert_eq!(
        names,
        vec![
            "C[1,1]".to_string(),
            "C[1,2]".to_string(),
            "C[2,1]".to_string(),
            "C[2,2]".to_string(),
        ]
    );
}

#[test]
fn scalarize_expression_rows_flattens_matrix_literals_row_major() {
    let ctx = ExpressionScalarizationContext {
        var_dims: HashMap::new(),
        complex_fields: HashMap::new(),
        component_index_map: HashMap::new(),
        function_output_index_map: HashMap::new(),
    };
    let expr = Expression::Array {
        elements: vec![
            Expression::Array {
                elements: vec![
                    Expression::Literal(Literal::Integer(1)),
                    Expression::Literal(Literal::Integer(2)),
                ],
                is_matrix: true,
            },
            Expression::Array {
                elements: vec![
                    Expression::Literal(Literal::Integer(3)),
                    Expression::Literal(Literal::Integer(4)),
                ],
                is_matrix: true,
            },
        ],
        is_matrix: true,
    };

    let rows = scalarize_expression_rows(&expr, 4, &ctx);
    assert_eq!(
        rows,
        vec![
            Expression::Literal(Literal::Integer(1)),
            Expression::Literal(Literal::Integer(2)),
            Expression::Literal(Literal::Integer(3)),
            Expression::Literal(Literal::Integer(4)),
        ]
    );
}

#[test]
fn scalarize_matrix_vector_product_uses_row_dot_product() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .parameters
        .insert(VarName::new("R"), variable("R", &[2, 3]));
    dae_model
        .parameters
        .insert(VarName::new("v"), variable("v", &[3]));
    dae_model
        .outputs
        .insert(VarName::new("y"), variable("y", &[2]));
    dae_model.f_x.push(eq("y", mul_expr(var("R"), var("v")), 2));

    scalarize_equations(&mut dae_model);

    assert_eq!(dae_model.f_x.len(), 2);
    assert_eq!(dae_model.f_x[0].lhs, Some(VarName::new("y[1]")));
    assert_eq!(
        dae_model.f_x[0].rhs,
        product_sum(&[
            ("R", &[1, 1], "v", &[1]),
            ("R", &[1, 2], "v", &[2]),
            ("R", &[1, 3], "v", &[3]),
        ])
    );
    assert_eq!(dae_model.f_x[1].lhs, Some(VarName::new("y[2]")));
    assert_eq!(
        dae_model.f_x[1].rhs,
        product_sum(&[
            ("R", &[2, 1], "v", &[1]),
            ("R", &[2, 2], "v", &[2]),
            ("R", &[2, 3], "v", &[3]),
        ])
    );
}

#[test]
fn scalarize_projected_function_output_keeps_array_argument_whole() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .parameters
        .insert(VarName::new("q"), variable("q", &[4]));
    dae_model
        .outputs
        .insert(VarName::new("R"), variable("R", &[3]));

    let mut function = dae::Function::new("LieGroup.SO3.rotationMatrix", Span::DUMMY);
    function.add_input(dae::FunctionParam::new("q", "Real").with_dims(vec![4]));
    function.add_output(dae::FunctionParam::new("R", "Real").with_dims(vec![3]));
    dae_model
        .functions
        .insert(VarName::new("LieGroup.SO3.rotationMatrix"), function);

    dae_model.f_x.push(eq(
        "R",
        Expression::FunctionCall {
            name: VarName::new("LieGroup.SO3.rotationMatrix"),
            args: vec![var("q")],
            is_constructor: false,
        },
        3,
    ));

    scalarize_equations(&mut dae_model);

    assert_eq!(dae_model.f_x.len(), 3);
    assert_eq!(
        dae_model.f_x[0].rhs,
        Expression::FunctionCall {
            name: VarName::new("LieGroup.SO3.rotationMatrix.R[1]"),
            args: vec![var("q")],
            is_constructor: false,
        }
    );
}

#[test]
fn scalarize_vector_matrix_product_uses_column_dot_product() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .parameters
        .insert(VarName::new("v"), variable("v", &[2]));
    dae_model
        .parameters
        .insert(VarName::new("R"), variable("R", &[2, 3]));
    dae_model
        .outputs
        .insert(VarName::new("y"), variable("y", &[3]));
    dae_model.f_x.push(eq("y", mul_expr(var("v"), var("R")), 3));

    scalarize_equations(&mut dae_model);

    assert_eq!(dae_model.f_x.len(), 3);
    assert_eq!(dae_model.f_x[0].lhs, Some(VarName::new("y[1]")));
    assert_eq!(
        dae_model.f_x[0].rhs,
        product_sum(&[("v", &[1], "R", &[1, 1]), ("v", &[2], "R", &[2, 1])])
    );
    assert_eq!(dae_model.f_x[1].lhs, Some(VarName::new("y[2]")));
    assert_eq!(
        dae_model.f_x[1].rhs,
        product_sum(&[("v", &[1], "R", &[1, 2]), ("v", &[2], "R", &[2, 2])])
    );
    assert_eq!(dae_model.f_x[2].lhs, Some(VarName::new("y[3]")));
    assert_eq!(
        dae_model.f_x[2].rhs,
        product_sum(&[("v", &[1], "R", &[1, 3]), ("v", &[2], "R", &[2, 3])])
    );
}

#[test]
fn scalarize_matrix_matrix_product_uses_row_column_dot_product() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .parameters
        .insert(VarName::new("A"), variable("A", &[2, 2]));
    dae_model
        .parameters
        .insert(VarName::new("B"), variable("B", &[2, 2]));
    dae_model
        .outputs
        .insert(VarName::new("C"), variable("C", &[2, 2]));
    dae_model.f_x.push(eq("C", mul_expr(var("A"), var("B")), 4));

    scalarize_equations(&mut dae_model);

    assert_eq!(dae_model.f_x.len(), 4);
    assert_eq!(dae_model.f_x[0].lhs, Some(VarName::new("C[1,1]")));
    assert_eq!(
        dae_model.f_x[0].rhs,
        product_sum(&[("A", &[1, 1], "B", &[1, 1]), ("A", &[1, 2], "B", &[2, 1])])
    );
    assert_eq!(dae_model.f_x[1].lhs, Some(VarName::new("C[1,2]")));
    assert_eq!(
        dae_model.f_x[1].rhs,
        product_sum(&[("A", &[1, 1], "B", &[1, 2]), ("A", &[1, 2], "B", &[2, 2])])
    );
    assert_eq!(dae_model.f_x[2].lhs, Some(VarName::new("C[2,1]")));
    assert_eq!(
        dae_model.f_x[2].rhs,
        product_sum(&[("A", &[2, 1], "B", &[1, 1]), ("A", &[2, 2], "B", &[2, 1])])
    );
    assert_eq!(dae_model.f_x[3].lhs, Some(VarName::new("C[2,2]")));
    assert_eq!(
        dae_model.f_x[3].rhs,
        product_sum(&[("A", &[2, 1], "B", &[1, 2]), ("A", &[2, 2], "B", &[2, 2])])
    );
}

#[test]
fn scalarize_transpose_matrix_vector_product_swaps_indices() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .parameters
        .insert(VarName::new("A"), variable("A", &[2, 3]));
    dae_model
        .parameters
        .insert(VarName::new("v"), variable("v", &[2]));
    dae_model
        .outputs
        .insert(VarName::new("y"), variable("y", &[3]));
    dae_model
        .f_x
        .push(eq("y", mul_expr(transpose(var("A")), var("v")), 3));

    scalarize_equations(&mut dae_model);

    assert_eq!(dae_model.f_x.len(), 3);
    assert_eq!(dae_model.f_x[0].lhs, Some(VarName::new("y[1]")));
    assert_eq!(
        dae_model.f_x[0].rhs,
        product_sum(&[("A", &[1, 1], "v", &[1]), ("A", &[2, 1], "v", &[2])])
    );
    assert_eq!(dae_model.f_x[1].lhs, Some(VarName::new("y[2]")));
    assert_eq!(
        dae_model.f_x[1].rhs,
        product_sum(&[("A", &[1, 2], "v", &[1]), ("A", &[2, 2], "v", &[2])])
    );
    assert_eq!(dae_model.f_x[2].lhs, Some(VarName::new("y[3]")));
    assert_eq!(
        dae_model.f_x[2].rhs,
        product_sum(&[("A", &[1, 3], "v", &[1]), ("A", &[2, 3], "v", &[2])])
    );
}

#[test]
fn scalarize_transpose_as_array_equation_swaps_indices() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .parameters
        .insert(VarName::new("A"), variable("A", &[2, 3]));
    dae_model
        .outputs
        .insert(VarName::new("C"), variable("C", &[3, 2]));
    dae_model.f_x.push(eq("C", transpose(var("A")), 6));

    scalarize_equations(&mut dae_model);

    assert_eq!(dae_model.f_x.len(), 6);
    assert_eq!(dae_model.f_x[0].lhs, Some(VarName::new("C[1,1]")));
    assert_eq!(dae_model.f_x[0].rhs, var_idx("A", &[1, 1]));
    assert_eq!(dae_model.f_x[1].lhs, Some(VarName::new("C[1,2]")));
    assert_eq!(dae_model.f_x[1].rhs, var_idx("A", &[2, 1]));
    assert_eq!(dae_model.f_x[2].lhs, Some(VarName::new("C[2,1]")));
    assert_eq!(dae_model.f_x[2].rhs, var_idx("A", &[1, 2]));
    assert_eq!(dae_model.f_x[3].lhs, Some(VarName::new("C[2,2]")));
    assert_eq!(dae_model.f_x[3].rhs, var_idx("A", &[2, 2]));
    assert_eq!(dae_model.f_x[4].lhs, Some(VarName::new("C[3,1]")));
    assert_eq!(dae_model.f_x[4].rhs, var_idx("A", &[1, 3]));
    assert_eq!(dae_model.f_x[5].lhs, Some(VarName::new("C[3,2]")));
    assert_eq!(dae_model.f_x[5].rhs, var_idx("A", &[2, 3]));
}

#[test]
fn scalarize_vector_dot_product_in_scalar_equation() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .parameters
        .insert(VarName::new("a"), variable("a", &[3]));
    dae_model
        .parameters
        .insert(VarName::new("b"), variable("b", &[3]));
    dae_model
        .outputs
        .insert(VarName::new("s"), variable("s", &[]));
    dae_model.f_x.push(eq("s", mul_expr(var("a"), var("b")), 1));

    scalarize_equations(&mut dae_model);

    assert_eq!(dae_model.f_x.len(), 1);
    assert_eq!(dae_model.f_x[0].lhs, Some(VarName::new("s")));
    assert_eq!(
        dae_model.f_x[0].rhs,
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
        .parameters
        .insert(VarName::new("A"), variable("A", &[3, 4]));
    dae_model
        .outputs
        .insert(VarName::new("y"), variable("y", &[3]));
    dae_model.f_x.push(eq(
        "y",
        var_sub("A", vec![Subscript::Colon, Subscript::Index(2)]),
        3,
    ));

    scalarize_equations(&mut dae_model);

    assert_eq!(dae_model.f_x.len(), 3);
    assert_eq!(dae_model.f_x[0].lhs, Some(VarName::new("y[1]")));
    assert_eq!(dae_model.f_x[0].rhs, var_idx("A", &[1, 2]));
    assert_eq!(dae_model.f_x[1].lhs, Some(VarName::new("y[2]")));
    assert_eq!(dae_model.f_x[1].rhs, var_idx("A", &[2, 2]));
    assert_eq!(dae_model.f_x[2].lhs, Some(VarName::new("y[3]")));
    assert_eq!(dae_model.f_x[2].rhs, var_idx("A", &[3, 2]));
}

#[test]
fn scalarize_sliced_vector_dot_product_in_scalar_equation() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .parameters
        .insert(VarName::new("A"), variable("A", &[4, 3]));
    dae_model
        .parameters
        .insert(VarName::new("B"), variable("B", &[3, 4]));
    dae_model
        .outputs
        .insert(VarName::new("s"), variable("s", &[]));
    dae_model.f_x.push(eq(
        "s",
        mul_expr(
            var_sub("A", vec![Subscript::Index(2), Subscript::Colon]),
            var_sub("B", vec![Subscript::Colon, Subscript::Index(3)]),
        ),
        1,
    ));

    scalarize_equations(&mut dae_model);

    assert_eq!(dae_model.f_x.len(), 1);
    assert_eq!(dae_model.f_x[0].lhs, Some(VarName::new("s")));
    assert_eq!(
        dae_model.f_x[0].rhs,
        product_sum(&[
            ("A", &[2, 1], "B", &[1, 3]),
            ("A", &[2, 2], "B", &[2, 3]),
            ("A", &[2, 3], "B", &[3, 3]),
        ])
    );
}

#[test]
fn scalarize_sliced_dot_product_in_synthetic_root_condition() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .parameters
        .insert(VarName::new("A"), variable("A", &[4, 3]));
    dae_model
        .parameters
        .insert(VarName::new("B"), variable("B", &[3, 4]));
    dae_model
        .synthetic_root_conditions
        .push(Expression::Binary {
            op: OpBinary::Lt(Default::default()),
            lhs: Box::new(mul_expr(
                var_sub("A", vec![Subscript::Index(2), Subscript::Colon]),
                var_sub("B", vec![Subscript::Colon, Subscript::Index(3)]),
            )),
            rhs: Box::new(real(0.0)),
        });

    scalarize_equations(&mut dae_model);

    assert_eq!(dae_model.synthetic_root_conditions.len(), 1);
    assert_eq!(
        dae_model.synthetic_root_conditions[0],
        Expression::Binary {
            op: OpBinary::Lt(Default::default()),
            lhs: Box::new(product_sum(&[
                ("A", &[2, 1], "B", &[1, 3]),
                ("A", &[2, 2], "B", &[2, 3]),
                ("A", &[2, 3], "B", &[3, 3]),
            ])),
            rhs: Box::new(real(0.0)),
        }
    );
}

#[test]
fn scalarize_cross_product_uses_vector_components() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .parameters
        .insert(VarName::new("a"), variable("a", &[3]));
    dae_model
        .parameters
        .insert(VarName::new("b"), variable("b", &[3]));
    dae_model
        .outputs
        .insert(VarName::new("c"), variable("c", &[3]));
    dae_model.f_x.push(eq("c", cross(var("a"), var("b")), 3));

    scalarize_equations(&mut dae_model);

    assert_eq!(dae_model.f_x.len(), 3);
    assert_eq!(dae_model.f_x[0].lhs, Some(VarName::new("c[1]")));
    assert_eq!(
        dae_model.f_x[0].rhs,
        sub_expr(
            mul_expr(var_idx("a", &[2]), var_idx("b", &[3])),
            mul_expr(var_idx("a", &[3]), var_idx("b", &[2])),
        )
    );
    assert_eq!(dae_model.f_x[1].lhs, Some(VarName::new("c[2]")));
    assert_eq!(
        dae_model.f_x[1].rhs,
        sub_expr(
            mul_expr(var_idx("a", &[3]), var_idx("b", &[1])),
            mul_expr(var_idx("a", &[1]), var_idx("b", &[3])),
        )
    );
    assert_eq!(dae_model.f_x[2].lhs, Some(VarName::new("c[3]")));
    assert_eq!(
        dae_model.f_x[2].rhs,
        sub_expr(
            mul_expr(var_idx("a", &[1]), var_idx("b", &[2])),
            mul_expr(var_idx("a", &[2]), var_idx("b", &[1])),
        )
    );
}
