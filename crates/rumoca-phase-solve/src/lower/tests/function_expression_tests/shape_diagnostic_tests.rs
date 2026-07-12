use super::*;

fn function_with_array_input(name: &str, dims: Vec<i64>) -> rumoca_core::Function {
    let mut function = test_function(name, lower_test_span());
    function.inputs.push(
        rumoca_core::FunctionParam::new("u", "Real", lower_test_span())
            .with_dims(dims)
            .with_span(lower_test_span()),
    );
    function.outputs.push(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        lower_test_span(),
    ));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: real_lit(0.0),
        span: lower_test_span(),
    });
    function
}

fn call_function_with_arg(name: &str, arg: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(name)),
        args: vec![arg],
        is_constructor: false,
        span: lower_test_span(),
    }
}

fn call_function_without_args(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(test_component_ref_from_name(name)),
        args: Vec::new(),
        is_constructor: false,
        span: lower_test_span(),
    }
}

#[test]
fn lower_expression_reports_function_input_shape_value_count_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_tests_function_expression_tests_shape_diagnostic_tests_source_46.mo",
        ),
        4,
        16,
    );
    let mut functions = IndexMap::new();
    let function = function_with_array_input("Pkg.shapeCount", vec![2, 2]);
    functions.insert(function.name.clone(), function);

    let arg = rumoca_core::Expression::Array {
        elements: vec![real_lit(1.0), real_lit(2.0)],
        is_matrix: false,
        span,
    };
    let err = lower_expression(
        &call_function_with_arg("Pkg.shapeCount", arg),
        &VarLayout::default(),
        &functions,
    )
    .expect_err("declared function-input shape should reject wrong scalar count");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string().contains(
            "function `Pkg.shapeCount` input `u` expected 4 scalar value(s) for shape [2, 2], got 2"
        ),
        "unexpected error: {err}"
    );
}

#[test]
fn lower_expression_reports_negative_local_array_shape_with_subscript_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_tests_function_expression_tests_shape_diagnostic_tests_source_48.mo",
        ),
        12,
        18,
    );
    let mut functions = IndexMap::new();
    let mut function = test_function("Pkg.localNegativeShape", lower_test_span());
    function.outputs.push(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        lower_test_span(),
    ));
    function.locals.push(
        rumoca_core::FunctionParam::new("a", "Real", lower_test_span())
            .with_dims(vec![-1, -1])
            .with_default(rumoca_core::Expression::Array {
                elements: vec![real_lit(1.0), real_lit(2.0)],
                is_matrix: false,
                span: lower_test_span(),
            }),
    );
    function.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("a").into(),
            subscripts: vec![
                rumoca_core::Subscript::generated_index(1, span),
                rumoca_core::Subscript::generated_index(1, span),
            ],
            span,
        },
        span,
    });
    functions.insert(function.name.clone(), function);

    let err = lower_expression(
        &call_function_without_args("Pkg.localNegativeShape"),
        &VarLayout::default(),
        &functions,
    )
    .expect_err("negative local array dimensions should fail with the subscript span");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string()
            .contains("subscripted local array `a` has negative dimensions [-1, -1]"),
        "unexpected error: {err}"
    );
}

#[test]
fn lower_expression_reports_negative_function_input_shape_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_tests_function_expression_tests_shape_diagnostic_tests_source_47.mo",
        ),
        8,
        20,
    );
    let mut functions = IndexMap::new();
    let function = function_with_array_input("Pkg.negativeShape", vec![-1]);
    functions.insert(function.name.clone(), function);

    let arg = rumoca_core::Expression::Array {
        elements: vec![real_lit(1.0)],
        is_matrix: false,
        span,
    };
    let err = lower_expression(
        &call_function_with_arg("Pkg.negativeShape", arg),
        &VarLayout::default(),
        &functions,
    )
    .expect_err("negative declared function-input shape should fail without panicking");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string().contains(
            "function `Pkg.negativeShape` input `u` has negative dimension in declared shape [-1]"
        ),
        "unexpected error: {err}"
    );
}
