use super::*;
use rumoca_core::Span;
use std::collections::HashMap;

mod review_regression_tests;

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

fn structured_reference(name: &str, span: Span) -> rumoca_core::Reference {
    rumoca_core::Reference::from_component_reference(
        rumoca_core::component_reference_from_flat_name(&VarName::new(name), span)
            .expect("valid structured test reference"),
    )
}

fn structured_function_reference(
    name: &str,
    span: Span,
    instance_id: u32,
) -> rumoca_core::Reference {
    let reference = structured_reference(name, span);
    let base_part_count = reference.parts().len();
    reference.with_resolved_function(rumoca_core::ResolvedFunctionReference {
        instance_id: rumoca_core::FunctionInstanceId::new(instance_id),
        base_part_count,
    })
}

fn set_function_instance(function: &mut rumoca_core::Function, instance_id: u32) {
    function.instance_id = Some(rumoca_core::FunctionInstanceId::new(instance_id));
}

fn projected_function_reference(
    name: &str,
    span: Span,
    instance_id: u32,
    base_part_count: usize,
) -> rumoca_core::Reference {
    structured_reference(name, span).with_resolved_function(
        rumoca_core::ResolvedFunctionReference {
            instance_id: rumoca_core::FunctionInstanceId::new(instance_id),
            base_part_count,
        },
    )
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

const COMPLEX_CONSTRUCTOR_INSTANCE: u32 = 41;

fn add_complex_constructor(dae_model: &mut dae::Dae) {
    let mut constructor = rumoca_core::Function::new("Complex", test_span());
    set_function_instance(&mut constructor, COMPLEX_CONSTRUCTOR_INSTANCE);
    constructor.is_constructor = true;
    constructor.add_input(rumoca_core::FunctionParam::new("re", "Real", test_span()));
    let mut im = rumoca_core::FunctionParam::new("im", "Real", test_span());
    im.default = Some(Expression::Literal {
        value: Literal::Integer(0),
        span: test_span(),
    });
    constructor.add_input(im);
    dae_model
        .symbols
        .functions
        .insert(VarName::new("Complex"), constructor);
}

fn complex_args(args: Vec<Expression>) -> Expression {
    Expression::FunctionCall {
        name: structured_function_reference("Complex", test_span(), COMPLEX_CONSTRUCTOR_INSTANCE),
        args,
        is_constructor: true,
        span: test_span(),
    }
}

fn complex(re: Expression, im: Expression) -> Expression {
    complex_args(vec![re, im])
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
    add_complex_constructor(&mut dae_model);
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
fn scalarize_record_constructor_uses_declared_default_for_omitted_field() {
    let mut dae_model = dae::Dae::default();
    add_complex_constructor(&mut dae_model);
    for name in ["z1.re", "z1.im", "z2.re", "z2.im"] {
        dae_model
            .variables
            .algebraics
            .insert(VarName::new(name), variable(name, &[]));
    }
    dae_model
        .continuous
        .equations
        .push(residual_with_binary_span(
            complex_args(vec![real(0.0)]),
            Expression::Binary {
                op: OpBinary::Add,
                lhs: Box::new(var("z1")),
                rhs: Box::new(var("z2")),
                span: test_span(),
            },
            2,
            test_span(),
        ));

    scalarize_equations(&mut dae_model).unwrap();

    let rows = &dae_model.continuous.equations;
    assert_eq!(rows.len(), 2);
    for (row, expected_lhs, expected_fields) in [
        (&rows[0], Literal::Real(0.0), ["z1.re", "z2.re"]),
        (&rows[1], Literal::Integer(0), ["z1.im", "z2.im"]),
    ] {
        let Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } = &row.rhs
        else {
            panic!("expected scalar residual row");
        };
        assert!(
            matches!(lhs.as_ref(), Expression::Literal { value, .. } if *value == expected_lhs)
        );
        let Expression::Binary {
            op: OpBinary::Add,
            lhs: first,
            rhs: second,
            ..
        } = rhs.as_ref()
        else {
            panic!("expected projected additive fields");
        };
        for (field, expected) in [first, second].into_iter().zip(expected_fields) {
            assert!(
                matches!(field.as_ref(), Expression::VarRef { name, .. } if name.as_str() == expected)
            );
        }
    }
}

#[test]
fn scalarize_reconstructed_complex_residual_uses_record_components() {
    let mut dae_model = dae::Dae::default();
    add_complex_constructor(&mut dae_model);
    dae_model
        .continuous
        .equations
        .push(residual_with_binary_span(
            complex(var("lhs_re"), var("lhs_im")),
            complex(var("rhs_re"), var("rhs_im")),
            2,
            test_span(),
        ));

    scalarize_equations(&mut dae_model).unwrap();

    let rows = &dae_model.continuous.equations;
    assert_eq!(rows.len(), 2);
    for (row, expected_lhs, expected_rhs) in [
        (&rows[0], "lhs_re", "rhs_re"),
        (&rows[1], "lhs_im", "rhs_im"),
    ] {
        let Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } = &row.rhs
        else {
            panic!("expected projected complex residual");
        };
        assert!(
            matches!(lhs.as_ref(), Expression::VarRef { name, .. } if name.as_str() == expected_lhs)
        );
        assert!(
            matches!(rhs.as_ref(), Expression::VarRef { name, .. } if name.as_str() == expected_rhs)
        );
    }
}

#[test]
fn scalarize_complex_field_arithmetic_nested_in_scalar_function_arguments() {
    let mut dae_model = dae::Dae::default();
    add_complex_constructor(&mut dae_model);
    let product = Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(complex(var("n_re"), var("n_im"))),
        rhs: Box::new(complex(der(var("phi_re")), der(var("phi_im")))),
        span: test_span(),
    };
    let field = |name: &str| Expression::FieldAccess {
        base: Box::new(product.clone()),
        field: name.to_string(),
        span: test_span(),
    };
    let projected_scalar_call = Expression::FunctionCall {
        name: structured_reference("selectScalar", test_span()),
        args: vec![field("re"), field("im")],
        is_constructor: false,
        span: test_span(),
    };
    dae_model
        .continuous
        .equations
        .push(residual_with_binary_span(
            var("result"),
            projected_scalar_call,
            1,
            test_span(),
        ));

    scalarize_equations(&mut dae_model).expect("nested Complex fields should scalarize");

    let residual = &dae_model.continuous.equations[0].rhs;
    assert!(!expr_contains_field_access(residual));
    assert!(rumoca_ir_dae::expr_contains_der_of(
        residual,
        &VarName::new("phi_re")
    ));
    assert!(rumoca_ir_dae::expr_contains_der_of(
        residual,
        &VarName::new("phi_im")
    ));
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

fn expr_contains_field_access(expr: &Expression) -> bool {
    match expr {
        Expression::FieldAccess { .. } => true,
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_field_access(lhs) || expr_contains_field_access(rhs)
        }
        Expression::Unary { rhs, .. } | Expression::Index { base: rhs, .. } => {
            expr_contains_field_access(rhs)
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_contains_field_access)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_field_access(condition) || expr_contains_field_access(value)
            }) || expr_contains_field_access(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            elements.iter().any(expr_contains_field_access)
        }
        Expression::Range {
            start, step, end, ..
        } => {
            expr_contains_field_access(start)
                || step.as_deref().is_some_and(expr_contains_field_access)
                || expr_contains_field_access(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            expr_contains_field_access(expr)
                || indices
                    .iter()
                    .any(|index| expr_contains_field_access(&index.range))
                || filter.as_deref().is_some_and(expr_contains_field_access)
        }
        Expression::VarRef { .. } | Expression::Literal { .. } | Expression::Empty { .. } => false,
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
fn scalarize_scalar_index_of_array_literal_selects_the_indexed_element() {
    let selected = index(array(vec![var("first"), var("second")]), &[2]);
    let projected = index_into_expr(
        &selected,
        1,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    )
    .unwrap();
    assert_eq!(projected, var("second"));

    let mut dae_model = dae::Dae::default();
    for name in ["selected", "first", "second"] {
        dae_model
            .variables
            .algebraics
            .insert(VarName::new(name), variable(name, &[]));
    }
    dae_model
        .continuous
        .equations
        .push(residual_with_binary_span(
            var("selected"),
            selected,
            1,
            test_span(),
        ));

    scalarize_equations(&mut dae_model).unwrap();

    let Expression::Binary { rhs, .. } = &dae_model.continuous.equations[0].rhs else {
        panic!("expected residual equation");
    };
    assert_eq!(rhs.as_ref(), &var("second"));
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
        function_output_dims_map: HashMap::new(),
        dynamic_function_output_map: HashMap::new(),
        record_field_projection_map: HashMap::new(),
        constructor_input_map: HashMap::new(),
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
fn scalarize_record_function_field_uses_record_relative_projection_key() {
    let span = test_span();
    let instance_id = rumoca_core::FunctionInstanceId::new(41);
    let selector = vec![
        rumoca_core::ComponentRefPart {
            ident: "R".to_string(),
            span,
            subs: Vec::new(),
        },
        rumoca_core::ComponentRefPart {
            ident: "T".to_string(),
            span,
            subs: vec![
                Subscript::generated_index(1, span),
                Subscript::generated_index(1, span),
            ],
        },
    ];
    let mut by_index = HashMap::new();
    by_index.insert(1, selector);
    let mut fields = HashMap::new();
    fields.insert("T".to_string(), by_index);
    let mut record_field_projection_map = HashMap::new();
    record_field_projection_map.insert(
        instance_id,
        crate::projection_maps::RecordFieldProjection {
            output_name: "R".to_string(),
            fields,
        },
    );
    let ctx = ExpressionScalarizationContext {
        var_dims: HashMap::new(),
        var_spans: HashMap::new(),
        structural_values: HashMap::new(),
        complex_fields: HashMap::new(),
        component_index_map: HashMap::new(),
        function_output_index_map: HashMap::new(),
        function_output_dims_map: HashMap::new(),
        dynamic_function_output_map: HashMap::new(),
        record_field_projection_map,
        constructor_input_map: HashMap::new(),
    };
    let call = Expression::FunctionCall {
        name: structured_function_reference("Pkg.nullRotation", span, 41),
        args: Vec::new(),
        is_constructor: false,
        span,
    };
    let output = Expression::FieldAccess {
        base: Box::new(call),
        field: "R".to_string(),
        span,
    };
    let field = Expression::FieldAccess {
        base: Box::new(output),
        field: "T".to_string(),
        span,
    };

    let rows = scalarize_expression_rows(&field, 2, &ctx).expect("record field should scalarize");
    let Expression::FunctionCall { name, .. } = &rows[0] else {
        panic!("expected projected function call");
    };
    assert_eq!(name.as_str(), "Pkg.nullRotation.R.T[1,1]");
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
        function_output_dims_map: HashMap::new(),
        dynamic_function_output_map: HashMap::new(),
        record_field_projection_map: HashMap::new(),
        constructor_input_map: HashMap::new(),
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
fn scalarize_scalar_times_rank_three_literal_projects_each_scalar() {
    let ctx = ExpressionScalarizationContext {
        var_dims: HashMap::new(),
        var_spans: HashMap::new(),
        structural_values: HashMap::new(),
        complex_fields: HashMap::new(),
        component_index_map: HashMap::new(),
        function_output_index_map: HashMap::new(),
        function_output_dims_map: HashMap::new(),
        dynamic_function_output_map: HashMap::new(),
        record_field_projection_map: HashMap::new(),
        constructor_input_map: HashMap::new(),
    };
    let pair = |first, second| array(vec![real(first), real(second)]);
    let tensor = array(vec![
        array(vec![pair(0.0, 0.0), pair(1.0, 1.0)]),
        array(vec![pair(0.0, 1.0), pair(1.0, 0.0)]),
    ]);
    let expr = mul_expr(real(1.5), tensor);

    let rows = scalarize_expression_rows(&expr, 8, &ctx).unwrap();

    assert_eq!(
        rows,
        [0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0]
            .into_iter()
            .map(|value| mul_expr(real(1.5), real(value)))
            .collect::<Vec<_>>()
    );
}

#[test]
fn scalarize_array_of_vectors_projects_each_child_component() {
    let ctx = ExpressionScalarizationContext {
        var_dims: HashMap::from([
            ("first".to_string(), vec![3]),
            ("second".to_string(), vec![3]),
        ]),
        var_spans: HashMap::from([
            ("first".to_string(), test_span()),
            ("second".to_string(), test_span()),
        ]),
        structural_values: HashMap::new(),
        complex_fields: HashMap::new(),
        component_index_map: HashMap::new(),
        function_output_index_map: HashMap::new(),
        function_output_dims_map: HashMap::new(),
        dynamic_function_output_map: HashMap::new(),
        record_field_projection_map: HashMap::new(),
        constructor_input_map: HashMap::new(),
    };
    let expr = array(vec![var("first"), var("second")]);

    let rows = scalarize_expression_rows(&expr, 6, &ctx).unwrap();

    assert_eq!(
        rows,
        vec![
            var_idx("first", &[1]),
            var_idx("first", &[2]),
            var_idx("first", &[3]),
            var_idx("second", &[1]),
            var_idx("second", &[2]),
            var_idx("second", &[3]),
        ]
    );
}

#[test]
fn scalarize_static_comprehension_projects_iteration_and_body_indices() {
    let ctx = ExpressionScalarizationContext {
        var_dims: HashMap::new(),
        var_spans: HashMap::new(),
        structural_values: HashMap::new(),
        complex_fields: HashMap::new(),
        component_index_map: HashMap::new(),
        function_output_index_map: HashMap::new(),
        function_output_dims_map: HashMap::new(),
        dynamic_function_output_map: HashMap::new(),
        record_field_projection_map: HashMap::new(),
        constructor_input_map: HashMap::new(),
    };
    let expr = Expression::ArrayComprehension {
        expr: Box::new(array(vec![var("i"), real(0.0)])),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: Expression::Range {
                start: Box::new(Expression::Literal {
                    value: Literal::Integer(1),
                    span: test_span(),
                }),
                step: Some(Box::new(Expression::Literal {
                    value: Literal::Integer(1),
                    span: test_span(),
                })),
                end: Box::new(Expression::Literal {
                    value: Literal::Integer(3),
                    span: test_span(),
                }),
                span: test_span(),
            },
        }],
        filter: None,
        span: test_span(),
    };

    let rows = scalarize_expression_rows(&expr, 6, &ctx).unwrap();

    assert_eq!(
        rows,
        vec![
            Expression::Literal {
                value: Literal::Integer(1),
                span: test_span(),
            },
            real(0.0),
            Expression::Literal {
                value: Literal::Integer(2),
                span: test_span(),
            },
            real(0.0),
            Expression::Literal {
                value: Literal::Integer(3),
                span: test_span(),
            },
            real(0.0),
        ]
    );
}

#[test]
fn scalarize_nested_reduction_preserves_inner_comprehension_domain() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("matrix"), variable("matrix", &[3, 4]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("totals"), variable("totals", &[3]));
    let range = |end| Expression::Range {
        start: Box::new(Expression::Literal {
            value: Literal::Integer(1),
            span: test_span(),
        }),
        step: None,
        end: Box::new(Expression::Literal {
            value: Literal::Integer(end),
            span: test_span(),
        }),
        span: test_span(),
    };
    let inner = Expression::ArrayComprehension {
        expr: Box::new(var_sub(
            "matrix",
            vec![
                Subscript::generated_expr(Box::new(var("axis")), test_span()),
                Subscript::generated_expr(Box::new(var("foot")), test_span()),
            ],
        )),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "foot".to_string(),
            range: range(4),
        }],
        filter: None,
        span: test_span(),
    };
    let outer = Expression::ArrayComprehension {
        expr: Box::new(Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sum,
            args: vec![inner],
            span: test_span(),
        }),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "axis".to_string(),
            range: range(3),
        }],
        filter: None,
        span: test_span(),
    };
    dae_model
        .continuous
        .equations
        .push(residual_with_binary_span(
            var("totals"),
            outer,
            3,
            test_span(),
        ));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 3);
    for (axis, equation) in (1..=3).zip(&dae_model.continuous.equations) {
        let Expression::Binary { rhs, .. } = &equation.rhs else {
            panic!("expected scalar residual");
        };
        let Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sum,
            args,
            ..
        } = rhs.as_ref()
        else {
            panic!("expected sum reduction, got {rhs:?}");
        };
        let [Expression::ArrayComprehension { expr, indices, .. }] = args.as_slice() else {
            panic!("sum must retain its inner comprehension domain: {args:?}");
        };
        assert_eq!(indices[0].name, "foot");
        assert!(matches!(
            expr.as_ref(),
            Expression::VarRef { subscripts, .. }
                if matches!(
                    subscripts.as_slice(),
                    [
                        Subscript::Expr { expr, .. },
                        Subscript::Expr { expr: foot, .. },
                    ] if matches!(expr.as_ref(), Expression::Literal { value: Literal::Integer(value), .. } if *value == axis)
                        && matches!(foot.as_ref(), Expression::VarRef { name, .. } if name.as_str() == "foot")
                )
        ));
    }
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
fn scalarize_skew_vector_product_projects_builtin_result_not_argument() {
    let mut dae_model = dae::Dae::default();
    for name in ["v", "w", "y"] {
        dae_model
            .variables
            .algebraics
            .insert(VarName::new(name), variable(name, &[3]));
    }
    let skew = Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Skew,
        args: vec![var("v")],
        span: test_span(),
    };
    dae_model
        .continuous
        .equations
        .push(eq("y", mul_expr(skew, var("w")), 3));

    scalarize_equations(&mut dae_model).unwrap();

    let neg = |expr| Expression::Unary {
        op: OpUnary::Minus,
        rhs: Box::new(expr),
        span: test_span(),
    };
    let expected = [
        [real(0.0), neg(var_idx("v", &[3])), var_idx("v", &[2])],
        [var_idx("v", &[3]), real(0.0), neg(var_idx("v", &[1]))],
        [neg(var_idx("v", &[2])), var_idx("v", &[1]), real(0.0)],
    ];
    for (row, coefficients) in dae_model.continuous.equations.iter().zip(expected) {
        assert_eq!(
            row.rhs,
            sum_terms(
                coefficients
                    .into_iter()
                    .zip(1..=3)
                    .map(|(coefficient, index)| { mul_expr(coefficient, var_idx("w", &[index])) })
            )
        );
    }
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
    set_function_instance(&mut function, 1);
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
            name: structured_function_reference("LieGroup.SO3.rotationMatrix", test_span(), 1),
            args: vec![var("q")],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        3,
    ));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 3);
    let projected_name =
        structured_function_reference("LieGroup.SO3.rotationMatrix", test_span(), 1)
            .with_appended_parts(
                &[rumoca_core::ComponentRefPart {
                    ident: "R".to_string(),
                    span: test_span(),
                    subs: vec![Subscript::generated_index(1, test_span())],
                }],
                rumoca_core::Span::DUMMY,
            )
            .expect("structured projection");
    assert_eq!(
        dae_model.continuous.equations[0].rhs,
        Expression::FunctionCall {
            name: projected_name,
            args: vec![var("q")],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        }
    );
}

#[test]
fn scalarize_function_output_row_slice_projects_selected_elements() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("q"), variable("q", &[4]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("up_body"), variable("up_body", &[3]));

    let mut function = rumoca_core::Function::new("LieGroup.SO3.to_DCM", test_span());
    set_function_instance(&mut function, 42);
    function
        .add_input(rumoca_core::FunctionParam::new("q", "Real", test_span()).with_dims(vec![4]));
    function.add_output(
        rumoca_core::FunctionParam::new("R", "Real", test_span()).with_dims(vec![3, 3]),
    );
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);

    let matrix = Expression::FunctionCall {
        name: structured_function_reference("LieGroup.SO3.to_DCM", test_span(), 42),
        args: vec![var("q")],
        is_constructor: false,
        span: test_span(),
    };
    let row = Expression::Index {
        base: Box::new(matrix),
        subscripts: vec![
            Subscript::generated_index(3, test_span()),
            Subscript::generated_colon(test_span()),
        ],
        span: test_span(),
    };
    dae_model
        .continuous
        .equations
        .push(residual_with_binary_span(
            var("up_body"),
            row,
            3,
            test_span(),
        ));

    scalarize_equations(&mut dae_model).unwrap();

    let projected_names = dae_model
        .continuous
        .equations
        .iter()
        .map(|equation| {
            let Expression::Binary { rhs, .. } = &equation.rhs else {
                panic!("expected scalar residual");
            };
            let Expression::FunctionCall { name, .. } = rhs.as_ref() else {
                panic!("expected projected function output, got {rhs:?}");
            };
            name.as_str().to_string()
        })
        .collect::<Vec<_>>();
    assert_eq!(
        projected_names,
        [
            "LieGroup.SO3.to_DCM.R[3,1]",
            "LieGroup.SO3.to_DCM.R[3,2]",
            "LieGroup.SO3.to_DCM.R[3,3]",
        ]
    );
}

#[test]
fn scalarize_function_output_column_slice_preserves_dot_product_indices() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("q"), variable("q", &[4]));
    dae_model
        .variables
        .parameters
        .insert(VarName::new("normal"), variable("normal", &[3]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("cosine"), variable("cosine", &[]));

    let mut function = rumoca_core::Function::new("LieGroup.SO3.to_DCM", test_span());
    set_function_instance(&mut function, 42);
    function
        .add_input(rumoca_core::FunctionParam::new("q", "Real", test_span()).with_dims(vec![4]));
    function.add_output(
        rumoca_core::FunctionParam::new("R", "Real", test_span()).with_dims(vec![3, 3]),
    );
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);

    let column = Expression::Index {
        base: Box::new(Expression::FunctionCall {
            name: structured_function_reference("LieGroup.SO3.to_DCM", test_span(), 42),
            args: vec![var("q")],
            is_constructor: false,
            span: test_span(),
        }),
        subscripts: vec![
            Subscript::generated_colon(test_span()),
            Subscript::generated_index(3, test_span()),
        ],
        span: test_span(),
    };
    dae_model
        .continuous
        .equations
        .push(residual_with_binary_span(
            var("cosine"),
            Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(column),
                rhs: Box::new(var("normal")),
                span: test_span(),
            },
            1,
            test_span(),
        ));

    scalarize_equations(&mut dae_model).unwrap();

    struct FunctionNameCollector(Vec<String>);
    impl rumoca_core::ExpressionVisitor for FunctionNameCollector {
        fn visit_expression(&mut self, expr: &Expression) {
            if let Expression::FunctionCall { name, .. } = expr {
                self.0.push(name.as_str().to_string());
            }
            self.walk_expression(expr);
        }
    }
    let mut calls = FunctionNameCollector(Vec::new());
    rumoca_core::ExpressionVisitor::visit_expression(
        &mut calls,
        &dae_model.continuous.equations[0].rhs,
    );
    assert_eq!(
        calls.0,
        [
            "LieGroup.SO3.to_DCM.R[1,3]",
            "LieGroup.SO3.to_DCM.R[2,3]",
            "LieGroup.SO3.to_DCM.R[3,3]",
        ]
    );
}

#[test]
fn scalarize_does_not_reproject_an_already_selected_function_output() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("q"), variable("q", &[4]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("xhat"), variable("xhat", &[2]));

    let mut function = rumoca_core::Function::new("ekfUpdate", test_span());
    set_function_instance(&mut function, 5);
    function.add_output(rumoca_core::FunctionParam::new(
        "valid",
        "Boolean",
        test_span(),
    ));
    function.add_output(
        rumoca_core::FunctionParam::new("xhat", "Real", test_span()).with_dims(vec![2]),
    );
    function
        .add_input(rumoca_core::FunctionParam::new("q", "Real", test_span()).with_dims(vec![4]));
    dae_model
        .symbols
        .functions
        .insert(VarName::new("ekfUpdate"), function);
    let selected_name = projected_function_reference("ekfUpdate.xhat", test_span(), 5, 1);
    dae_model.continuous.equations.push(eq(
        "xhat",
        Expression::FunctionCall {
            name: selected_name.clone(),
            args: vec![var("q")],
            is_constructor: false,
            span: test_span(),
        },
        2,
    ));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 2);
    for equation in &dae_model.continuous.equations {
        assert!(matches!(
            &equation.rhs,
            Expression::FunctionCall { name, args, .. }
                if name == &selected_name && args == &[var("q")]
        ));
    }
}

#[test]
fn scalarize_matrix_product_uses_declared_function_output_shapes() {
    fn call(name: &str, instance_id: u32) -> Expression {
        Expression::FunctionCall {
            name: structured_function_reference(name, test_span(), instance_id),
            args: Vec::new(),
            is_constructor: false,
            span: test_span(),
        }
    }

    fn collect_call_names(expr: &Expression, names: &mut Vec<String>) {
        match expr {
            Expression::FunctionCall { name, .. } => names.push(name.as_str().to_string()),
            Expression::Binary { lhs, rhs, .. } => {
                collect_call_names(lhs, names);
                collect_call_names(rhs, names);
            }
            _ => {}
        }
    }

    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(VarName::new("P"), variable("P", &[2, 2]));
    for (instance_id, name) in [(2, "F"), (3, "G")] {
        let mut function = rumoca_core::Function::new(name, test_span());
        set_function_instance(&mut function, instance_id);
        function.add_output(
            rumoca_core::FunctionParam::new("Y", "Real", test_span()).with_dims(vec![2, 2]),
        );
        dae_model
            .symbols
            .functions
            .insert(function.name.clone(), function);
    }
    dae_model.continuous.equations.push(eq(
        "P",
        Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(call("F", 2)),
            rhs: Box::new(call("G", 3)),
            span: test_span(),
        },
        4,
    ));

    scalarize_equations(&mut dae_model).unwrap();

    let mut names = Vec::new();
    collect_call_names(&dae_model.continuous.equations[0].rhs, &mut names);
    assert_eq!(names, ["F.Y[1,1]", "G.Y[1,1]", "F.Y[1,2]", "G.Y[2,1]"]);
}

#[test]
fn scalarize_dynamic_function_output_resolves_shape_from_array_argument() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("A"), variable("A", &[2, 2]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("R"), variable("R", &[2, 2]));

    let mut function = rumoca_core::Function::new("My.symmetrize", test_span());
    set_function_instance(&mut function, 4);
    function
        .add_input(rumoca_core::FunctionParam::new("A", "Real", test_span()).with_dims(vec![0, 0]));
    let mut output =
        rumoca_core::FunctionParam::new("symmetricA", "Real", test_span()).with_dims(vec![0, 0]);
    output.shape_expr = [1, 2]
        .into_iter()
        .map(|dim| {
            rumoca_core::Subscript::generated_expr(
                Box::new(Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Size,
                    args: vec![
                        var("A"),
                        Expression::Literal {
                            value: Literal::Integer(dim),
                            span: test_span(),
                        },
                    ],
                    span: test_span(),
                }),
                test_span(),
            )
        })
        .collect();
    function.add_output(output);
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);

    dae_model.continuous.equations.push(eq(
        "R",
        Expression::FunctionCall {
            name: structured_function_reference("My.symmetrize", test_span(), 4),
            args: vec![var("A")],
            is_constructor: false,
            span: test_span(),
        },
        4,
    ));

    scalarize_equations(&mut dae_model).unwrap();

    assert_eq!(dae_model.continuous.equations.len(), 4);
    for (offset, equation) in dae_model.continuous.equations.iter().enumerate() {
        let row = offset / 2 + 1;
        let column = offset % 2 + 1;
        assert_eq!(
            equation.rhs,
            Expression::FunctionCall {
                name: projected_function_reference(
                    &format!("My.symmetrize.symmetricA[{row},{column}]"),
                    test_span(),
                    4,
                    2,
                ),
                args: vec![var("A")],
                is_constructor: false,
                span: test_span(),
            }
        );
    }
}

mod tensor_projection;
