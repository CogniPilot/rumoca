use super::*;
use rumoca_core::Span;
use std::collections::HashMap;

fn test_span() -> Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_structural_scalarize_tests_source_1.mo"),
        1,
        2,
    )
}

fn var(name: &str) -> Expression {
    let span = test_span();
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span,
    }
}

fn var_idx(name: &str, indices: &[i64]) -> Expression {
    let span = test_span();
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: indices
            .iter()
            .copied()
            .map(|index| Subscript::generated_index(index, span))
            .collect(),
        span,
    }
}

fn var_sub(name: &str, subscripts: Vec<Subscript>) -> Expression {
    let span = test_span();
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts,
        span,
    }
}

fn real(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: test_span(),
    }
}

fn variable(name: &str, dims: &[i64]) -> dae::Variable {
    let mut variable = dae::Variable::new(
        VarName::new(name),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    variable.dims = dims.to_vec();
    variable.source_span = test_span();
    variable
}

fn lhs(name: &str) -> Option<rumoca_core::Reference> {
    // Explicit-equation targets are structured at construction
    // (`Equation::explicit` normalizes bare names).
    let component_ref = rumoca_core::component_reference_from_flat_name(
        &rumoca_core::VarName::new(name),
        test_span(),
    );
    Some(match component_ref {
        Some(component_ref) => {
            rumoca_core::Reference::with_component_reference(name, component_ref)
        }
        None => rumoca_core::Reference::new(name),
    })
}

#[test]
fn complex_field_scalar_name_requires_top_level_segment_boundary() {
    assert!(is_complex_field_scalar_name("z.re", "re"));
    assert!(is_complex_field_scalar_name("pkg.z.im", "im"));
    assert!(!is_complex_field_scalar_name("z.real", "re"));
    assert!(!is_complex_field_scalar_name("z.imag", "im"));
    assert!(!is_complex_field_scalar_name("z[index.re]", "re"));
}

#[test]
fn scalar_target_projection_parsers_use_structured_scalar_names() -> Result<(), StructuralError> {
    assert_eq!(parse_complex_field_selector("re[2]"), Some((1, Some(2))));
    assert_eq!(parse_complex_field_selector("im"), Some((2, None)));
    assert_eq!(parse_complex_field_selector("re[0]"), None);
    assert_eq!(parse_complex_field_selector("im[-1]"), None);
    assert_eq!(parse_complex_field_selector("re[1].tail"), None);
    assert_eq!(parse_complex_field_selector("re[index.with.dot]"), None);
    assert_eq!(
        parse_scalar_target_projection("a.b", "a.b[2].re"),
        Some((Some(2), Some(1)))
    );
    assert_eq!(
        parse_scalar_target_projection("a.b", "a.b.im[3]"),
        Some((Some(3), Some(2)))
    );
    assert_eq!(parse_scalar_target_projection("a.b", "a.b[0].re"), None);
    assert_eq!(parse_scalar_target_projection("a.b", "a.b[2]tail"), None);
    assert_eq!(parse_scalar_target_projection("a.b", "a.b[2.re"), None);

    let Expression::VarRef {
        name, subscripts, ..
    } = target_var_ref_expr_with_span("a[index.with.dot].x[2, 3]", test_span())?
    else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "a[index.with.dot].x");
    assert!(matches!(
        subscripts.as_slice(),
        [
            Subscript::Index { value: 2, .. },
            Subscript::Index { value: 3, .. },
        ]
    ));

    let Expression::VarRef {
        name, subscripts, ..
    } = target_var_ref_expr_with_span("a[index.with.dot]", test_span())?
    else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "a[index.with.dot]");
    assert!(subscripts.is_empty());
    Ok(())
}

#[test]
fn scalarized_equation_lhs_rejects_unspanned_generated_index() {
    let eq = Equation {
        lhs: Some(rumoca_core::Reference::new("x")),
        rhs: var("x"),
        span: Span::DUMMY,
        origin: "unspanned scalarization fixture".to_string(),
        scalar_count: 2,
    };

    let err = scalarized_equation_lhs(&eq, None, 1, Span::DUMMY)
        .expect_err("scalarized LHS index requires reference or equation provenance");

    assert!(matches!(
        err,
        StructuralError::UnspannedContractViolation { .. }
    ));
    assert!(
        err.to_string()
            .contains("cannot append scalarized index to `x`"),
        "error should identify missing LHS provenance: {err}"
    );
}

#[test]
fn scalar_lhs_targets_use_variable_span_when_equation_span_is_generated() {
    let span = test_span();
    let mut var_dims = HashMap::new();
    var_dims.insert("x".to_string(), vec![2]);
    let mut var_spans = HashMap::new();
    var_spans.insert("x".to_string(), span);
    let scalar_names = vec!["x[1]".to_string(), "x[2]".to_string()];

    let (targets, has_residual_targets) = scalar_lhs_targets_for_equation(
        Vec::new(),
        Some("x"),
        Some(&rumoca_core::Reference::new("x")),
        Span::DUMMY,
        &scalar_names,
        &var_dims,
        &var_spans,
    )
    .expect("variable source span should provide scalar target provenance");

    assert!(!has_residual_targets);
    assert_eq!(targets.len(), 2);
    assert_eq!(targets[0].expr.span(), Some(span));
}

#[test]
fn scalar_lhs_targets_use_base_variable_span_for_scalarized_target_name() {
    let span = test_span();
    let var_dims = HashMap::new();
    let mut var_spans = HashMap::new();
    var_spans.insert("x".to_string(), span);
    let scalar_names = vec!["x[2]".to_string()];

    let (targets, has_residual_targets) = scalar_lhs_targets_for_equation(
        Vec::new(),
        Some("x[2]"),
        Some(&rumoca_core::Reference::new("x[2]")),
        Span::DUMMY,
        &scalar_names,
        &var_dims,
        &var_spans,
    )
    .expect("base variable source span should provide scalar target provenance");

    assert!(!has_residual_targets);
    assert_eq!(targets.len(), 1);
    assert_eq!(targets[0].expr.span(), Some(span));
}

#[test]
fn scalar_lhs_targets_reject_generated_equation_without_target_provenance() {
    let var_dims = HashMap::new();
    let var_spans = HashMap::new();
    let scalar_names = vec!["x[1]".to_string()];

    let err = scalar_lhs_targets_for_equation(
        Vec::new(),
        Some("x"),
        Some(&rumoca_core::Reference::new("x")),
        Span::DUMMY,
        &scalar_names,
        &var_dims,
        &var_spans,
    )
    .expect_err("source-derived scalar target must have provenance");

    assert!(matches!(
        err,
        StructuralError::UnspannedContractViolation { .. }
    ));
    assert!(
        err.to_string()
            .contains("cannot scalarize target `x` without source provenance"),
        "error should identify missing scalar target provenance: {err}"
    );
}

fn complex(re: Expression, im: Expression) -> Expression {
    Expression::FunctionCall {
        name: rumoca_core::Reference::new("Complex"),
        args: vec![re, im],
        is_constructor: true,
        span: test_span(),
    }
}

fn array(elements: Vec<Expression>) -> Expression {
    Expression::Array {
        elements,
        is_matrix: false,
        span: test_span(),
    }
}

fn residual_with_binary_span(
    lhs: Expression,
    rhs: Expression,
    scalar_count: usize,
    binary_span: rumoca_core::Span,
) -> Equation {
    Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: binary_span,
        },
        span: test_span(),
        origin: "scalarize residual test".to_string(),
        scalar_count,
    }
}

fn residual_lhs_ref(eq: &Equation) -> Option<(&str, Vec<i64>)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        ..
    } = &eq.rhs
    else {
        return None;
    };
    let Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    else {
        return None;
    };
    let indices = subscripts
        .iter()
        .map(|subscript| match subscript {
            Subscript::Index { value, .. } => Some(*value),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    Some((name.as_str(), indices))
}

#[test]
fn scalarize_complex_record_array_residual_targets_each_field_element() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(VarName::new("aw.re"), variable("aw.re", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(VarName::new("aw.im"), variable("aw.im", &[3]));
    dae_model
        .variables
        .parameters
        .insert(VarName::new("a"), variable("a", &[3]));

    dae_model
        .continuous
        .equations
        .push(residual_with_binary_span(
            var("aw"),
            array(vec![
                complex(var_idx("a", &[1]), real(0.0)),
                complex(var_idx("a", &[2]), real(0.0)),
                complex(var_idx("a", &[3]), real(0.0)),
            ]),
            6,
            Span::from_offsets(
                rumoca_core::SourceId::from_source_name(
                    "phase_structural_scalarize_tests_source_0.mo",
                ),
                10,
                20,
            ),
        ));

    scalarize_equations(&mut dae_model).unwrap();

    let targets = dae_model
        .continuous
        .equations
        .iter()
        .map(residual_lhs_ref)
        .collect::<Option<Vec<_>>>()
        .expect("all scalarized rows should remain residual assignments");
    assert_eq!(
        targets,
        vec![
            ("aw.re", vec![1]),
            ("aw.re", vec![2]),
            ("aw.re", vec![3]),
            ("aw.im", vec![1]),
            ("aw.im", vec![2]),
            ("aw.im", vec![3]),
        ]
    );
    crate::sort_dae(&dae_model).expect("scalarized record-array residual should be matchable");
}

#[test]
fn scalarize_matrix_binding_residual_targets_each_element() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(VarName::new("skew"), variable("skew", &[3, 3]));

    dae_model
        .continuous
        .equations
        .push(residual_with_binary_span(
            var("skew"),
            array(vec![
                array(vec![real(0.0), real(-1.0), real(0.0)]),
                array(vec![real(1.0), real(0.0), real(0.0)]),
                array(vec![real(0.0), real(0.0), real(0.0)]),
            ]),
            9,
            Span::from_offsets(
                rumoca_core::SourceId::from_source_name(
                    "phase_structural_scalarize_tests_source_0.mo",
                ),
                10,
                20,
            ),
        ));

    scalarize_equations(&mut dae_model).unwrap();

    let targets = dae_model
        .continuous
        .equations
        .iter()
        .map(residual_lhs_ref)
        .collect::<Option<Vec<_>>>()
        .expect("all scalarized rows should remain residual assignments");
    assert_eq!(
        targets,
        vec![
            ("skew", vec![1, 1]),
            ("skew", vec![1, 2]),
            ("skew", vec![1, 3]),
            ("skew", vec![2, 1]),
            ("skew", vec![2, 2]),
            ("skew", vec![2, 3]),
            ("skew", vec![3, 1]),
            ("skew", vec![3, 2]),
            ("skew", vec![3, 3]),
        ]
    );
    crate::sort_dae(&dae_model).expect("scalarized matrix binding should be matchable");
}

fn eq(lhs: &str, rhs: Expression, scalar_count: usize) -> Equation {
    Equation::explicit_with_scalar_count(
        VarName::new(lhs),
        rhs,
        test_span(),
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
        function: rumoca_core::BuiltinFunction::Transpose,
        args: vec![expr],
        span: test_span(),
    }
}

fn cross(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Cross,
        args: vec![lhs, rhs],
        span: test_span(),
    }
}

fn index(expr: Expression, indices: &[i64]) -> Expression {
    let span = test_span();
    Expression::Index {
        base: Box::new(expr),
        subscripts: indices
            .iter()
            .copied()
            .map(|index| Subscript::generated_index(index, span))
            .collect(),
        span,
    }
}

fn der(expr: Expression) -> Expression {
    Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![expr],
        span: test_span(),
    }
}

fn expr_contains_der_var_idx(expr: &Expression, target: &str, idx: i64) -> bool {
    match expr {
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args,
            ..
        } => args.first().is_some_and(|arg| match arg {
            Expression::VarRef {
                name, subscripts, ..
            } => {
                name.as_str() == target
                    && matches!(subscripts.as_slice(), [Subscript::Index { value: i, .. }] if *i == idx)
            }
            _ => false,
        }),
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_der_var_idx(lhs, target, idx)
                || expr_contains_der_var_idx(rhs, target, idx)
        }
        Expression::Unary { rhs, .. } => expr_contains_der_var_idx(rhs, target, idx),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => args
            .iter()
            .any(|arg| expr_contains_der_var_idx(arg, target, idx)),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_der_var_idx(condition, target, idx)
                    || expr_contains_der_var_idx(value, target, idx)
            }) || expr_contains_der_var_idx(else_branch, target, idx)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => elements
            .iter()
            .any(|element| expr_contains_der_var_idx(element, target, idx)),
        Expression::Range {
            start, step, end, ..
        } => {
            expr_contains_der_var_idx(start, target, idx)
                || step
                    .as_deref()
                    .is_some_and(|value| expr_contains_der_var_idx(value, target, idx))
                || expr_contains_der_var_idx(end, target, idx)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            expr_contains_der_var_idx(expr, target, idx)
                || indices
                    .iter()
                    .any(|index| expr_contains_der_var_idx(&index.range, target, idx))
                || filter
                    .as_deref()
                    .is_some_and(|value| expr_contains_der_var_idx(value, target, idx))
        }
        Expression::Index { base, .. } | Expression::FieldAccess { base, .. } => {
            expr_contains_der_var_idx(base, target, idx)
        }
        Expression::VarRef { .. }
        | Expression::Literal { value: _, .. }
        | Expression::Empty { .. } => false,
    }
}

#[test]
fn build_output_names_orders_states_algebraics_outputs_and_expands_arrays() {
    let mut dae_model = dae::Dae::default();

    let mut x = dae::Variable::new(
        rumoca_core::VarName::new("x"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    x.dims = vec![2];
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), x);
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("v"),
        dae::Variable::new(
            rumoca_core::VarName::new("v"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("z"),
        dae::Variable::new(
            rumoca_core::VarName::new("z"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let mut y = dae::Variable::new(
        rumoca_core::VarName::new("y"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    y.dims = vec![2];
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("y"), y);

    let names = build_output_names(&dae_model).unwrap();
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
        .variables
        .outputs
        .insert(VarName::new("C"), variable("C", &[2, 2]));

    let names = build_output_names(&dae_model).unwrap();
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
        var_spans: HashMap::new(),
        structural_values: HashMap::new(),
        complex_fields: HashMap::new(),
        component_index_map: HashMap::new(),
        function_output_index_map: HashMap::new(),
    };
    let expr = Expression::Array {
        elements: vec![
            Expression::Array {
                elements: vec![
                    Expression::Literal {
                        value: Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY,
                    },
                    Expression::Literal {
                        value: Literal::Integer(2),
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                is_matrix: true,
                span: rumoca_core::Span::DUMMY,
            },
            Expression::Array {
                elements: vec![
                    Expression::Literal {
                        value: Literal::Integer(3),
                        span: rumoca_core::Span::DUMMY,
                    },
                    Expression::Literal {
                        value: Literal::Integer(4),
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                is_matrix: true,
                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_matrix: true,
        span: rumoca_core::Span::DUMMY,
    };

    let rows = scalarize_expression_rows(&expr, 4, &ctx).unwrap();
    assert_eq!(
        rows,
        vec![
            Expression::Literal {
                value: Literal::Integer(1),
                span: rumoca_core::Span::DUMMY
            },
            Expression::Literal {
                value: Literal::Integer(2),
                span: rumoca_core::Span::DUMMY
            },
            Expression::Literal {
                value: Literal::Integer(3),
                span: rumoca_core::Span::DUMMY
            },
            Expression::Literal {
                value: Literal::Integer(4),
                span: rumoca_core::Span::DUMMY
            },
        ]
    );
}

#[test]
fn scalarize_expression_rows_flattens_nested_array_matrix_literals() {
    let ctx = ExpressionScalarizationContext {
        var_dims: HashMap::new(),
        var_spans: HashMap::new(),
        structural_values: HashMap::new(),
        complex_fields: HashMap::new(),
        component_index_map: HashMap::new(),
        function_output_index_map: HashMap::new(),
    };
    let expr = Expression::Array {
        elements: vec![
            Expression::Array {
                elements: vec![real(1.0), real(0.0), real(0.0)],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
            Expression::Array {
                elements: vec![real(0.0), real(1.0), real(0.0)],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
            Expression::Array {
                elements: vec![real(0.0), real(0.0), real(1.0)],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };

    let rows = scalarize_expression_rows(&expr, 9, &ctx).unwrap();

    assert_eq!(
        rows,
        vec![
            real(1.0),
            real(0.0),
            real(0.0),
            real(0.0),
            real(1.0),
            real(0.0),
            real(0.0),
            real(0.0),
            real(1.0),
        ]
    );
}

#[test]
fn scalarize_matrix_vector_product_uses_row_dot_product() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("R"), variable("R", &[2, 3]));
    dae_model
        .variables
        .parameters
        .insert(VarName::new("v"), variable("v", &[3]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("y"), variable("y", &[2]));
    dae_model
        .continuous
        .equations
        .push(eq("y", mul_expr(var("R"), var("v")), 2));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 2);
    assert_eq!(dae_model.continuous.equations[0].lhs, lhs("y[1]"));
    assert_eq!(
        dae_model.continuous.equations[0].rhs,
        product_sum(&[
            ("R", &[1, 1], "v", &[1]),
            ("R", &[1, 2], "v", &[2]),
            ("R", &[1, 3], "v", &[3]),
        ])
    );
    assert_eq!(dae_model.continuous.equations[1].lhs, lhs("y[2]"));
    assert_eq!(
        dae_model.continuous.equations[1].rhs,
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
        .variables
        .parameters
        .insert(VarName::new("q"), variable("q", &[4]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("R"), variable("R", &[3]));

    let mut function = rumoca_core::Function::new("LieGroup.SO3.rotationMatrix", test_span());
    function
        .add_input(rumoca_core::FunctionParam::new("q", "Real", test_span()).with_dims(vec![4]));
    function
        .add_output(rumoca_core::FunctionParam::new("R", "Real", test_span()).with_dims(vec![3]));
    dae_model
        .symbols
        .functions
        .insert(VarName::new("LieGroup.SO3.rotationMatrix"), function);

    dae_model.continuous.equations.push(eq(
        "R",
        Expression::FunctionCall {
            name: rumoca_core::Reference::new("LieGroup.SO3.rotationMatrix"),
            args: vec![var("q")],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        3,
    ));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 3);
    assert_eq!(
        dae_model.continuous.equations[0].rhs,
        Expression::FunctionCall {
            name: rumoca_core::Reference::new("LieGroup.SO3.rotationMatrix.R[1]"),
            args: vec![var("q")],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        }
    );
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
