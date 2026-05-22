use super::*;

#[test]
fn test_todae_accepts_function_typed_parameter_calls_in_function_body() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let mut partial =
        rumoca_ir_flat::Function::new("Modelica.Math.Nonlinear.partialScalarFunction", Span::DUMMY);
    partial.partial = true;
    flat.add_function(partial);

    let mut nonlinear = rumoca_ir_flat::Function::new(
        "Modelica.Math.Nonlinear.solveOneNonlinearEquation",
        Span::DUMMY,
    );
    nonlinear.inputs.push(rumoca_ir_flat::FunctionParam {
        name: "f".to_string(),
        type_name: "Modelica.Math.Nonlinear.partialScalarFunction".to_string(),
        dims: vec![],
        default: None,
        description: None,
    });
    nonlinear.inputs.push(rumoca_ir_flat::FunctionParam {
        name: "u".to_string(),
        type_name: "Real".to_string(),
        dims: vec![],
        default: None,
        description: None,
    });
    nonlinear.outputs.push(rumoca_ir_flat::FunctionParam {
        name: "y".to_string(),
        type_name: "Real".to_string(),
        dims: vec![],
        default: None,
        description: None,
    });
    nonlinear.body.push(rumoca_ir_flat::Statement::Assignment {
        comp: make_comp_ref("y"),
        value: flat::Expression::FunctionCall {
            name: VarName::new("Modelica.Math.Nonlinear.solveOneNonlinearEquation.f"),
            args: vec![make_var_ref("u")],
            is_constructor: false,
        },
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
