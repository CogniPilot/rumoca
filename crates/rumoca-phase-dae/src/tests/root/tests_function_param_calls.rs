use super::*;

#[test]
fn test_todae_accepts_function_typed_parameter_calls_in_function_body() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let partial =
        rumoca_core::Function::new("Modelica.Math.Nonlinear.partialScalarFunction", Span::DUMMY);
    flat.add_function(partial);

    let mut nonlinear = rumoca_core::Function::new(
        "Modelica.Math.Nonlinear.solveOneNonlinearEquation",
        Span::DUMMY,
    );
    nonlinear.inputs.push(
        rumoca_core::FunctionParam::new(
            "f",
            "Modelica.Math.Nonlinear.partialScalarFunction",
            crate::test_support::test_span(),
        )
        .with_type_class(rumoca_core::ClassType::Function),
    );
    nonlinear.inputs.push(rumoca_core::FunctionParam::new(
        "u",
        "Real",
        crate::test_support::test_span(),
    ));
    nonlinear.outputs.push(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        crate::test_support::test_span(),
    ));
    nonlinear.body.push(rumoca_core::Statement::Assignment {
        comp: make_comp_ref("y"),
        value: rumoca_core::Expression::FunctionCall {
            name: VarName::new("Modelica.Math.Nonlinear.solveOneNonlinearEquation.f").into(),
            args: vec![make_var_ref("u")],
            is_constructor: false,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
    });
    flat.add_function(nonlinear);

    add_scalar_ode_with_rhs_call(
        &mut flat,
        "x",
        "Modelica.Math.Nonlinear.solveOneNonlinearEquation",
    );

    to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("function-typed function parameters should be callable in function bodies");
}

#[test]
fn test_todae_accepts_function_typed_parameter_without_flat_function_entry() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let mut nonlinear = rumoca_core::Function::new(
        "Modelica.Math.Nonlinear.solveOneNonlinearEquation",
        Span::DUMMY,
    );
    nonlinear.inputs.push(
        rumoca_core::FunctionParam::new(
            "f",
            "Modelica.Math.Nonlinear.Interfaces.partialScalarFunction",
            crate::test_support::test_span(),
        )
        .with_type_class(rumoca_core::ClassType::Function),
    );
    nonlinear.inputs.push(rumoca_core::FunctionParam::new(
        "u",
        "Real",
        crate::test_support::test_span(),
    ));
    nonlinear.outputs.push(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        crate::test_support::test_span(),
    ));
    nonlinear.body.push(rumoca_core::Statement::Assignment {
        comp: make_comp_ref("y"),
        value: rumoca_core::Expression::FunctionCall {
            name: VarName::new("Modelica.Math.Nonlinear.solveOneNonlinearEquation.f").into(),
            args: vec![make_var_ref("u")],
            is_constructor: false,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
    });
    flat.add_function(nonlinear);

    add_scalar_ode_with_rhs_call(
        &mut flat,
        "x",
        "Modelica.Math.Nonlinear.solveOneNonlinearEquation",
    );

    to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("function-typed formals should not require executable flat function entries");
}
