use super::*;

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_projection_test.mo"),
        0,
        1,
    )
}

fn real(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(value),
        span: test_span(),
    }
}

fn array(elements: Vec<rumoca_core::Expression>, is_matrix: bool) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements,
        is_matrix,
        span: test_span(),
    }
}

fn binary(
    op: rumoca_core::OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn component_reference(parts: Vec<rumoca_core::ComponentRefPart>) -> rumoca_core::Reference {
    rumoca_core::Reference::from_component_reference(rumoca_core::ComponentReference {
        local: false,
        span: test_span(),
        parts,
        def_id: None,
    })
}

#[test]
fn flatten_array_elements_flattens_matrix_rows() -> Result<(), LowerError> {
    let row1 = array(vec![real(1.0), real(2.0)], false);
    let row2 = array(vec![real(3.0), real(4.0)], false);

    let flattened = flatten_array_elements(&[row1, row2], test_span())?;
    let values = flattened
        .iter()
        .map(|expr| match expr {
            rumoca_core::Expression::Literal {
                value: Literal::Real(value),
                ..
            } => *value,
            other => panic!("expected scalar literal, got {other:?}"),
        })
        .collect::<Vec<_>>();

    assert_eq!(values, vec![1.0, 2.0, 3.0, 4.0]);
    Ok(())
}

#[test]
fn scoped_single_scalar_value_has_scalar_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope
        .scalars
        .insert("tau_inv".to_string(), vec![real(17.0)]);
    let expr = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("tau_inv"),
        subscripts: Vec::new(),
        span: test_span(),
    };

    assert_eq!(
        analysis.expr_dims(&expr, &scope, 0, test_span()),
        Ok(Some(Vec::new()))
    );
}

#[test]
fn literal_binary_operand_has_known_scalar_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let expr = binary(
        rumoca_core::OpBinary::Add,
        real(1.0),
        array(vec![real(2.0), real(3.0)], false),
        test_span(),
    );

    assert_eq!(
        analysis.expr_dims(&expr, &scope, 0, test_span()),
        Ok(Some(vec![2]))
    );
}

#[test]
fn scoped_full_binding_has_substituted_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope.full.insert("gain".to_string(), real(5.0));
    let expr = local_var("gain");

    assert_eq!(
        analysis.expr_dims(&expr, &scope, 0, test_span()),
        Ok(Some(Vec::new()))
    );
}

#[test]
fn projected_scope_dimensions_override_full_binding_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope.full.insert("x".to_string(), real(5.0));
    scope.dims.insert("x".to_string(), vec![2]);
    let expr = local_var("x");

    assert_eq!(
        analysis.expr_dims(&expr, &scope, 0, test_span()),
        Ok(Some(vec![2]))
    );
}

#[test]
fn dae_scalar_variable_has_known_scalar_dimensions() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("angle"),
        dae::Variable {
            name: rumoca_core::VarName::new("angle"),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let expr = local_var("angle");

    assert_eq!(
        analysis.expr_dims(&expr, &scope, 0, test_span()),
        Ok(Some(Vec::new()))
    );
}

#[test]
fn projected_function_field_outputs_infer_dense_selector_dimensions() -> Result<(), LowerError> {
    let outputs = vec![
        ProjectedFunctionOutput {
            field_path: vec!["w".to_string()],
            selector_indices: vec![1],
            expr: real(1.0),
        },
        ProjectedFunctionOutput {
            field_path: vec!["w".to_string()],
            selector_indices: vec![2],
            expr: real(2.0),
        },
        ProjectedFunctionOutput {
            field_path: vec!["w".to_string()],
            selector_indices: vec![3],
            expr: real(3.0),
        },
    ];

    let dims = projected_field_output_dims(&outputs, "w", test_span())?;

    assert_eq!(dims, Some(vec![3]));
    Ok(())
}

#[test]
fn repeated_scalar_field_outputs_have_unknown_dimensions() -> Result<(), LowerError> {
    let outputs = vec![
        ProjectedFunctionOutput {
            field_path: vec!["record".to_string()],
            selector_indices: Vec::new(),
            expr: real(1.0),
        },
        ProjectedFunctionOutput {
            field_path: vec!["record".to_string()],
            selector_indices: Vec::new(),
            expr: real(2.0),
        },
    ];

    let dims = projected_field_output_dims(&outputs, "record", test_span())?;

    assert_eq!(dims, None);
    Ok(())
}

#[test]
fn array_binary_projection_rejects_unknown_operand_dimensions_with_span() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_51.mo",
        ),
        4,
        17,
    );
    let expr = binary(
        rumoca_core::OpBinary::Add,
        local_var("runtime_value"),
        array(vec![real(2.0), real(3.0)], false),
        span,
    );
    let ctx = ProjectionValueCtx {
        dims: &[2],
        flat_index: 0,
        scope: &scope,
        depth: 0,
        span,
    };

    let rumoca_core::Expression::Binary { lhs, rhs, op, .. } = &expr else {
        panic!("test expression must be binary");
    };
    let err = analysis
        .project_binary_value(op, lhs, rhs, &ctx)
        .expect_err("unknown operand dimensions must bubble a typed error");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "binary lhs has unknown dimensions".to_string()
    );
}

#[test]
fn checked_usize_dimension_rejects_i64_overflow_with_span() {
    let Some(dim) = usize::try_from(i64::MAX)
        .ok()
        .and_then(|value| value.checked_add(1))
    else {
        return;
    };
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_47.mo",
        ),
        8,
        19,
    );

    let err = checked_usize_dims_to_i64(&[dim], "array expression dimension", span)
        .expect_err("dimension must fit in Modelica integer range");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        format!("invalid IR contract: array expression dimension {dim} exceeds i64 range")
    );
}

#[test]
fn checked_projection_offset_rejects_host_index_overflow_with_span() -> Result<(), String> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_48.mo",
        ),
        3,
        11,
    );

    let Err(mul_err) =
        checked_projection_offset(usize::MAX, 2, 0, "matrix product flat index", span)
    else {
        return Err("overflowing projection offset multiplication succeeded".to_string());
    };
    assert_eq!(mul_err.source_span(), Some(span));
    assert_eq!(
        mul_err.reason(),
        "invalid IR contract: matrix product flat index multiplication overflows host index range"
            .to_string()
    );

    let Err(add_err) =
        checked_projection_offset(usize::MAX, 1, 1, "matrix product flat index", span)
    else {
        return Err("overflowing projection offset addition succeeded".to_string());
    };
    assert_eq!(add_err.source_span(), Some(span));
    assert_eq!(
        add_err.reason(),
        "invalid IR contract: matrix product flat index addition overflows host index range"
            .to_string()
    );

    Ok(())
}

#[test]
fn checked_projection_offset_dummy_span_stays_unspanned() {
    let err = checked_projection_offset(
        usize::MAX,
        2,
        0,
        "matrix product flat index",
        rumoca_core::Span::DUMMY,
    )
    .expect_err("overflowing projection offset multiplication must fail");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy projection offset span should not be fabricated into a source span: {err:?}"
    );
    assert!(err.reason().contains("multiplication overflows"));
}

#[test]
fn checked_usize_dims_to_i64_dummy_span_stays_unspanned() {
    let err = checked_usize_dims_to_i64(
        &[usize::MAX],
        "array expression dimension",
        rumoca_core::Span::DUMMY,
    )
    .expect_err("dimension must fit in Modelica integer range");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy dimension span should not be fabricated into a source span: {err:?}"
    );
    assert!(err.reason().contains("exceeds i64 range"));
}

#[test]
fn reserve_projection_capacity_dummy_span_stays_unspanned() {
    let mut values = Vec::<ProjectedFunctionOutput>::new();
    let err = reserve_projection_capacity(
        &mut values,
        usize::MAX,
        "projected output count",
        rumoca_core::Span::DUMMY,
    )
    .expect_err("impossible projection capacity must be rejected");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy projection capacity span should not be fabricated into a source span: {err:?}"
    );
    assert!(err.reason().contains("capacity exceeds host memory limits"));
}

#[test]
fn scalar_count_rejects_host_index_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_52.mo",
        ),
        1,
        9,
    );

    let err = scalar_count_for_dims(&[i64::MAX, i64::MAX], "projected value dimensions", span)
        .expect_err("overflowing scalar count must fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(err.reason().contains("projected value dimensions"));
}

#[test]
fn scalar_count_dummy_span_stays_unspanned() {
    let err = scalar_count_for_dims(
        &[i64::MAX, i64::MAX],
        "projected value dimensions",
        rumoca_core::Span::DUMMY,
    )
    .expect_err("overflowing scalar count must fail");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy scalar-count span should not be fabricated into a source span: {err:?}"
    );
    assert!(err.reason().contains("projected value dimensions"));
}

#[test]
fn flat_index_rejects_host_index_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_53.mo",
        ),
        1,
        9,
    );

    let err = flat_index_from_indices(
        &[i64::MAX, i64::MAX],
        &[i64::MAX, i64::MAX],
        span,
        "projected scalar selection flat index",
    )
    .expect_err("overflowing flat index must fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason()
            .contains("projected scalar selection flat index")
    );
}

#[test]
fn matrix_matrix_projection_with_zero_columns_declines() -> Result<(), String> {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_49.mo",
        ),
        1,
        9,
    );
    let ctx = ProjectionValueCtx {
        dims: &[],
        flat_index: 0,
        scope: &scope,
        depth: 0,
        span,
    };

    let projected = analysis
        .project_matrix_matrix_product(&real(1.0), &real(1.0), &[1, 1], &[1, 0], &ctx, 0)
        .map_err(|err| format!("zero-column matrix projection failed: {err:?}"))?;
    if projected.is_some() {
        return Err("zero-column matrix projection produced a scalar value".to_string());
    }

    Ok(())
}

#[test]
fn project_reference_indices_preserves_indexed_component_parts() {
    let reference = component_reference(vec![
        rumoca_core::ComponentRefPart {
            ident: "vehicle".to_string(),
            span: test_span(),
            subs: Vec::new(),
        },
        rumoca_core::ComponentRefPart {
            ident: "motor".to_string(),
            span: test_span(),
            subs: vec![rumoca_core::Subscript::generated_index(1, test_span())],
        },
        rumoca_core::ComponentRefPart {
            ident: "history".to_string(),
            span: test_span(),
            subs: Vec::new(),
        },
    ]);

    let projected = project_reference_field_path_and_indices(&reference, &[], &[2], test_span())
        .expect("structured reference projection should succeed");

    let component_ref = projected
        .component_ref()
        .expect("projected reference should preserve component-reference structure");
    assert_eq!(projected.as_str(), "vehicle.motor[1].history[2]");
    assert_eq!(component_ref.parts[1].ident, "motor");
    assert_eq!(component_ref.parts[1].subs.len(), 1);
    assert_eq!(component_ref.parts[2].ident, "history");
    assert_eq!(component_ref.parts[2].subs.len(), 1);
}

#[test]
fn project_reference_indices_rejects_i64_overflow_with_span() {
    let Some(index) = usize::try_from(i64::MAX)
        .ok()
        .and_then(|value| value.checked_add(1))
    else {
        return;
    };
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_46.mo",
        ),
        6,
        14,
    );
    let reference = component_reference(vec![rumoca_core::ComponentRefPart {
        ident: "x".to_string(),
        span,
        subs: Vec::new(),
    }]);

    let err = project_reference_field_path_and_indices(&reference, &[], &[index], span)
        .expect_err("projected reference index must fit in Modelica integer range");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        format!(
            "invalid IR contract: function output projection subscript index {index} exceeds i64 range"
        )
    );
}

fn scalar_function_param(name: &str) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        def_id: None,
        name: name.to_string(),
        span: test_span(),
        type_name: "Real".to_string(),
        type_class: None,
        dims: vec![],
        shape_expr: Vec::new(),
        default: None,
        description: None,
    }
}

fn function_param_with_dims(name: &str, dims: &[i64]) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        dims: dims.to_vec(),
        ..scalar_function_param(name)
    }
}

fn real_with_span(value: f64, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(value),
        span,
    }
}

fn function_param_with_type(name: &str, type_name: &str) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        type_name: type_name.to_string(),
        ..scalar_function_param(name)
    }
}

#[test]
fn vector_function_input_rejects_scalar_actual_with_span() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.needsVector", test_span());
    function.inputs.push(function_param_with_dims("u", &[2]));
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_52.mo",
        ),
        3,
        8,
    );

    let err = match analysis.bind_inputs(&function, &[real_with_span(1.0, span)], 0, span) {
        Ok(_) => panic!("scalar actual must not be projected as vector input"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "function `My.needsVector` input `u` expects dimensions [2], got []"
    );
}

#[test]
fn dynamic_vector_function_input_uses_actual_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.needsDynamicVector", test_span());
    function.inputs.push(function_param_with_dims("u", &[0]));

    let scope = analysis
        .bind_inputs(
            &function,
            &[array(vec![real(1.0), real(2.0), real(3.0)], false)],
            0,
            test_span(),
        )
        .expect("dynamic vector input projection should not fail")
        .expect("dynamic vector input should bind");

    assert_eq!(scope.dims.get("u"), Some(&vec![3]));
    assert_eq!(scope.scalars.get("u").map(Vec::len), Some(3));
}

#[test]
fn scalar_real_function_input_rejects_vector_actual_with_span() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.needsScalar", test_span());
    function.inputs.push(scalar_function_param("u"));
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_54.mo",
        ),
        7,
        13,
    );

    let err = match analysis.bind_inputs(
        &function,
        &[rumoca_core::Expression::Array {
            elements: vec![real(1.0), real(2.0)],
            is_matrix: false,
            span,
        }],
        0,
        span,
    ) {
        Ok(_) => panic!("vector actual must not be projected as scalar Real input"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "function `My.needsScalar` input `u` expects dimensions [], got [2]"
    );
}

#[test]
fn generated_function_call_projection_errors_use_owner_span() {
    let owner_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_53.mo",
        ),
        3,
        8,
    );
    let mut dae_model = dae::Dae::default();
    let mut function = rumoca_core::Function::new("My.needsVector", test_span());
    function.inputs.push(rumoca_core::FunctionParam {
        span: owner_span,
        ..function_param_with_dims("u", &[2])
    });
    function.outputs.push(scalar_function_param("y"));
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);
    let structural_bindings = IndexMap::new();
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.needsVector").into(),
        args: vec![rumoca_core::Expression::Literal {
            value: Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    let err = function_call_projected_scalars_with_owner(
        &call,
        &dae_model,
        &structural_bindings,
        owner_span,
    )
    .expect_err("generated invalid projection must report an error");

    assert_eq!(err.source_span(), Some(owner_span));
    assert_eq!(
        err.reason(),
        "function `My.needsVector` input `u` expects dimensions [2], got []"
    );
}

#[test]
fn record_like_function_input_accepts_structured_actual_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.needsRecord", test_span());
    function
        .inputs
        .push(function_param_with_type("q", "Pkg.Quaternion"));

    let scope = analysis
        .bind_inputs(
            &function,
            &[array(
                vec![real(1.0), real(2.0), real(3.0), real(4.0)],
                false,
            )],
            0,
            test_span(),
        )
        .expect("record-like input binding should not fail")
        .expect("record-like input should bind");

    assert_eq!(scope.dims.get("q"), Some(&vec![4]));
    assert_eq!(scope.scalars.get("q").map(Vec::len), Some(4));
}

#[test]
fn vector_constructor_input_rejects_scalar_actual_with_span() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let input = function_param_with_dims("u", &[2]);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_53.mo",
        ),
        5,
        11,
    );
    let actual = real_with_span(1.0, span);

    let err = analysis
        .optional_constructor_input_scalars(&actual, &input, &scope, 0, span)
        .expect_err("scalar actual must not be projected as vector constructor input");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "record constructor input `u` expects dimensions [2], got []"
    );
}

#[test]
fn scalar_constructor_input_uses_formal_scalar_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let input = scalar_function_param("u");
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_57.mo",
        ),
        5,
        11,
    );
    let actual = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("runtime_scalar"),
        subscripts: Vec::new(),
        span,
    };

    let scalars = analysis
        .optional_constructor_input_scalars(&actual, &input, &scope, 0, span)
        .expect("scalar constructor input projection should not fail")
        .expect("scalar primitive input should project as a scalar");

    assert_eq!(scalars.len(), 1);
}

#[test]
fn record_like_constructor_input_declines_unknown_actual_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let input = function_param_with_type("q", "Pkg.Quaternion");
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_58.mo",
        ),
        6,
        14,
    );
    let actual = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("runtime_record"),
        subscripts: Vec::new(),
        span,
    };

    let scalars = analysis
        .optional_constructor_input_scalars(&actual, &input, &scope, 0, span)
        .expect("unknown record-like constructor input dimensions should decline");

    assert!(scalars.is_none());
}

#[test]
fn if_projection_rejects_scalar_values_without_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let entry_scope = FunctionProjectionScope::default();
    let mut else_scope = FunctionProjectionScope::default();
    else_scope.scalars.insert("x".to_string(), vec![real(0.0)]);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_59.mo",
        ),
        7,
        18,
    );

    let err = match analysis.merged_if_scope(&entry_scope, &[], &[], &else_scope, span) {
        Ok(_) => panic!("merged scalar projection without dimensions must fail"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "invalid IR contract: if-statement projection for `x` has scalar values but no dimensions"
    );
}

#[test]
fn if_projection_rejects_conflicting_branch_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut entry_scope = FunctionProjectionScope::default();
    entry_scope
        .scalars
        .insert("x".to_string(), vec![real(0.0), real(0.0)]);
    entry_scope.dims.insert("x".to_string(), vec![2]);
    let mut branch_scope = entry_scope.clone();
    branch_scope.dims.insert("x".to_string(), vec![1, 2]);
    let condition = real(1.0);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_60.mo",
        ),
        8,
        21,
    );

    let err = match analysis.merged_if_scope(
        &entry_scope,
        &[condition],
        &[branch_scope],
        &entry_scope,
        span,
    ) {
        Ok(_) => panic!("merged scalar projection with conflicting dimensions must fail"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "invalid IR contract: if-statement projection for `x` has mismatched dimensions: [2] and [1, 2]"
    );
}

fn scalar_assignment(target: &str, value: rumoca_core::Expression) -> rumoca_core::Statement {
    assignment_with_span(target, value, test_span())
}

fn assignment_with_span(
    target: &str,
    value: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: target.to_string(),
                span,
                subs: Vec::new(),
            }],
            def_id: None,
        },
        value,
        span,
    }
}

#[test]
fn vector_assignment_rejects_scalar_value_with_span() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.badAssign", test_span());
    function.locals.push(function_param_with_dims("x", &[2]));
    let mut scope = FunctionProjectionScope::default();
    let mut projected = Vec::new();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_55.mo",
        ),
        2,
        9,
    );
    let statement = assignment_with_span("x", real_with_span(1.0, span), span);

    let err = analysis
        .apply_assignment(&function, &statement, &mut scope, &mut projected, 0, span)
        .expect_err("scalar assignment to vector local must fail");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "function `My.badAssign` assignment to `x` expects dimensions [2], got []"
    );
}

#[test]
fn scalar_assignment_rejects_vector_value_with_span() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.badAssign", test_span());
    function.locals.push(scalar_function_param("x"));
    let mut scope = FunctionProjectionScope::default();
    let mut projected = Vec::new();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_56.mo",
        ),
        4,
        12,
    );
    let statement = assignment_with_span(
        "x",
        rumoca_core::Expression::Array {
            elements: vec![real(1.0), real(2.0)],
            is_matrix: false,
            span,
        },
        span,
    );

    let err = analysis
        .apply_assignment(&function, &statement, &mut scope, &mut projected, 0, span)
        .expect_err("vector assignment to scalar local must fail");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "function `My.badAssign` assignment to `x` expects dimensions [], got [2]"
    );
}

#[test]
fn unassigned_projected_scalar_reports_projection_span() -> Result<(), String> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_57.mo",
        ),
        6,
        15,
    );
    let values = vec![rumoca_core::Expression::Empty { span }];

    let Err(err) = assigned_projected_scalar_value("x", &[1], &values, 0, span) else {
        return Err("unassigned projected scalar slot succeeded".to_string());
    };

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "projected local component `x[1]` is unassigned"
    );
    Ok(())
}

#[test]
fn scalar_selector_rejects_colon_with_subscript_span() -> Result<(), String> {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_58.mo",
        ),
        2,
        3,
    );
    let subscript = rumoca_core::Subscript::colon(span);

    let Err(err) = subscript_selector_expr(&subscript, &analysis, &scope, 0) else {
        return Err("colon scalar selector succeeded".to_string());
    };

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "colon subscript cannot select a scalar projected value"
    );
    Ok(())
}

#[test]
fn guarded_assignment_without_base_reports_assignment_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_59.mo",
        ),
        9,
        21,
    );

    let err = guarded_assignment_without_base("y", span);

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "if-statement assignment to `y` requires an existing binding or an else assignment"
    );
}

fn local_var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span: test_span(),
    }
}

/// A pure function whose projected output doubles in size per statement,
/// crossing `MAX_FUNCTION_PROJECTION_NODES` long before it finishes.
fn over_budget_function() -> rumoca_core::Function {
    let mut body = vec![scalar_assignment(
        "y",
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(local_var("x")),
            rhs: Box::new(local_var("x")),
            span: test_span(),
        },
    )];
    for _ in 0..16 {
        body.push(scalar_assignment(
            "y",
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(local_var("y")),
                rhs: Box::new(local_var("y")),
                span: test_span(),
            },
        ));
    }
    rumoca_core::Function {
        name: rumoca_core::VarName::new("My.explode"),
        def_id: None,
        inputs: vec![scalar_function_param("x")],
        outputs: vec![scalar_function_param("y")],
        locals: vec![],
        body,
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: test_span(),
    }
}

#[test]
fn over_budget_projection_is_a_typed_error_and_declines_at_the_boundary() {
    let mut dae_model = dae::Dae::default();
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.explode"),
        over_budget_function(),
    );
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.explode").into(),
        args: vec![real(2.0)],
        is_constructor: false,
        span: test_span(),
    };

    let err = analysis
        .function_call_outputs_with_owner(&call, 0, test_span())
        .expect_err("over-budget projection must surface as a typed error");
    assert!(err.is_projection_budget_exceeded(), "got: {err:?}");
    assert!(err.reason().contains("My.explode"), "got: {}", err.reason());

    // The outermost boundary resolves the decline by keeping the runtime
    // call; the memoized decline must answer follow-up probes identically.
    for _ in 0..2 {
        let outputs = analysis
            .top_level_function_call_outputs(&call, test_span())
            .expect("budget decline must not fail the outer lowering");
        assert!(outputs.is_none());
    }
}
