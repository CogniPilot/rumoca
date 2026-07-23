use super::*;

fn evaluate_constant(expr: &rumoca_core::Expression) -> Option<f64> {
    match expr {
        rumoca_core::Expression::Literal {
            value: Literal::Real(value),
            ..
        } => Some(*value),
        rumoca_core::Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value as f64),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs,
            rhs,
            ..
        } => Some(evaluate_constant(lhs)? + evaluate_constant(rhs)?),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs,
            rhs,
            ..
        } => Some(evaluate_constant(lhs)? * evaluate_constant(rhs)?),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sqrt,
            args,
            ..
        } if args.len() == 1 => Some(evaluate_constant(&args[0])?.sqrt()),
        _ => None,
    }
}

fn vector_norm_expr(name: &str) -> rumoca_core::Expression {
    builtin(
        rumoca_core::BuiltinFunction::Sqrt,
        vec![binary(
            rumoca_core::OpBinary::Mul,
            local_var(name),
            local_var(name),
            test_span(),
        )],
    )
}

fn projected_scalar(
    functions: Vec<rumoca_core::Function>,
    call_name: &str,
    args: Vec<rumoca_core::Expression>,
) -> Result<rumoca_core::Expression, LowerError> {
    let mut dae_model = dae::Dae::default();
    for function in functions {
        dae_model
            .symbols
            .functions
            .insert(function.name.clone(), function);
    }
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(call_name),
        args,
        is_constructor: false,
        span: test_span(),
    };
    let values = function_call_projected_scalars_with_owner(
        &call,
        &dae_model,
        &IndexMap::new(),
        test_span(),
    )?
    .expect("scalar function output should project");
    let [value] = values.as_slice() else {
        panic!("expected one projected scalar, got {values:?}");
    };
    Ok(value.clone())
}

#[test]
fn array_output_bound_to_function_local_uses_complete_vector_dot_product() -> Result<(), LowerError>
{
    let mut source = rumoca_core::Function::new("My.vectorSource", test_span());
    source.outputs.push(function_param_with_dims("v", &[3]));
    source.body.push(scalar_assignment(
        "v",
        array(vec![real(3.0), real(4.0), real(0.0)], false),
    ));

    let mut norm = rumoca_core::Function::new("My.localVectorNorm", test_span());
    norm.locals.push(function_param_with_dims("v", &[3]));
    norm.outputs.push(scalar_function_param("y"));
    norm.body.push(scalar_assignment(
        "v",
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("My.vectorSource"),
            args: Vec::new(),
            is_constructor: false,
            span: test_span(),
        },
    ));
    norm.body
        .push(scalar_assignment("y", vector_norm_expr("v")));

    let value = projected_scalar(vec![source, norm], "My.localVectorNorm", Vec::new())?;
    assert_eq!(evaluate_constant(&value), Some(5.0));
    Ok(())
}

#[test]
fn function_local_scalar_dot_product_controls_branch() -> Result<(), LowerError> {
    let mut function = rumoca_core::Function::new("My.localScalarNorm", test_span());
    function.inputs.push(function_param_with_dims("v", &[3]));
    function.locals.push(scalar_function_param("n"));
    function.outputs.push(scalar_function_param("y"));
    function
        .body
        .push(scalar_assignment("n", vector_norm_expr("v")));
    function.body.push(rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: binary(
                rumoca_core::OpBinary::Gt,
                local_var("n"),
                real(4.5),
                test_span(),
            ),
            stmts: vec![scalar_assignment("y", real(5.0))],
        }],
        else_block: Some(vec![scalar_assignment("y", real(0.0))]),
        span: test_span(),
    });

    let value = projected_scalar(
        vec![function],
        "My.localScalarNorm",
        vec![array(vec![real(3.0), real(4.0), real(0.0)], false)],
    )?;
    assert_eq!(evaluate_constant(&value), Some(5.0));
    Ok(())
}

#[test]
fn previous_quat_shape_uses_all_four_dot_product_terms() -> Result<(), LowerError> {
    let mut function = rumoca_core::Function::new("My.previousQuatNorm", test_span());
    function
        .inputs
        .push(function_param_with_dims("previous_quat", &[4]));
    function.outputs.push(scalar_function_param("y"));
    function
        .body
        .push(scalar_assignment("y", vector_norm_expr("previous_quat")));

    let value = projected_scalar(
        vec![function],
        "My.previousQuatNorm",
        vec![array(
            vec![real(0.5), real(0.5), real(0.5), real(0.5)],
            false,
        )],
    )?;
    assert_eq!(evaluate_constant(&value), Some(1.0));
    Ok(())
}

#[test]
fn tangential_vector_norm_sums_three_squared_components() -> Result<(), LowerError> {
    let mut function = rumoca_core::Function::new("My.tangentialNorm", test_span());
    function
        .inputs
        .push(function_param_with_dims("tangential", &[3]));
    function.outputs.push(scalar_function_param("y"));
    function
        .body
        .push(scalar_assignment("y", vector_norm_expr("tangential")));

    let value = projected_scalar(
        vec![function],
        "My.tangentialNorm",
        vec![array(vec![real(3.0), real(4.0), real(0.0)], false)],
    )?;
    assert_eq!(evaluate_constant(&value), Some(5.0));
    Ok(())
}

#[test]
fn vector_dot_projection_rejects_unequal_unknown_and_invalid_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let expr = binary(
        rumoca_core::OpBinary::Mul,
        local_var("lhs"),
        local_var("rhs"),
        test_span(),
    );

    for (lhs_dims, rhs_dims, expected) in [
        (Some(vec![2]), Some(vec![3]), "incompatible"),
        (None, Some(vec![3]), "unknown dimensions"),
        (Some(vec![0]), Some(vec![0]), "positive"),
        (Some(vec![-1]), Some(vec![-1]), "invalid dimension"),
    ] {
        let mut scope = FunctionProjectionScope::default();
        if let Some(dims) = lhs_dims {
            scope.dims.insert("lhs".to_string(), dims);
        }
        if let Some(dims) = rhs_dims {
            scope.dims.insert("rhs".to_string(), dims);
        }
        let ctx = projection_value_ctx(&[], 0, &scope, 0, test_span());
        let rumoca_core::Expression::Binary { lhs, rhs, .. } = &expr else {
            unreachable!();
        };
        let err = analysis
            .project_binary_value(&rumoca_core::OpBinary::Mul, lhs, rhs, &ctx)
            .expect_err("invalid vector dot dimensions must fail closed");
        assert!(
            err.reason().contains(expected),
            "expected `{expected}` in `{}`",
            err.reason()
        );
    }
}
