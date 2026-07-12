use super::*;

#[test]
fn scalarize_indexes_shape_changing_wrapper_around_dynamic_output_as_a_whole() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(VarName::new("A"), variable("A", &[3, 2]));
    dae_model
        .variables
        .outputs
        .insert(VarName::new("T"), variable("T", &[2, 3]));

    let mut function = rumoca_core::Function::new("My.dynamicMatrix", test_span());
    set_function_instance(&mut function, 40);
    function
        .add_input(rumoca_core::FunctionParam::new("A", "Real", test_span()).with_dims(vec![0, 0]));
    let mut output =
        rumoca_core::FunctionParam::new("Y", "Real", test_span()).with_dims(vec![0, 0]);
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
    let call = Expression::FunctionCall {
        name: structured_function_reference("My.dynamicMatrix", test_span(), 40),
        args: vec![var("A")],
        is_constructor: false,
        span: test_span(),
    };
    let transpose = Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Transpose,
        args: vec![call],
        span: test_span(),
    };
    dae_model
        .continuous
        .equations
        .push(eq("T", transpose.clone(), 6));

    scalarize_equations(&mut dae_model).unwrap();

    let Expression::Index {
        base, subscripts, ..
    } = &dae_model.continuous.equations[1].rhs
    else {
        panic!("the wrapper result should be indexed as a whole");
    };
    assert_eq!(base.as_ref(), &transpose);
    assert!(matches!(
        subscripts.as_slice(),
        [
            Subscript::Index { value: 1, .. },
            Subscript::Index { value: 2, .. }
        ]
    ));
}
