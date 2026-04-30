use super::*;

#[test]
fn test_eval_function_call_projected_complex_output_with_array_literal_input() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = Function::new("Pkg.pickFirstComplex", Default::default());
    f.add_input(FunctionParam::new("c", "Modelica.ComplexMath.Complex").with_dims(vec![1]));
    f.add_output(FunctionParam::new("result", "Modelica.ComplexMath.Complex"));
    f.body = vec![Statement::Assignment {
        comp: comp_ref("result"),
        value: Expression::VarRef {
            name: VarName::new("c"),
            subscripts: vec![Subscript::Index(1)],
        },
    }];
    funcs.insert("Pkg.pickFirstComplex".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    let arg = arr(
        vec![Expression::FunctionCall {
            name: VarName::new("Complex"),
            args: vec![lit(2.0), lit(-3.0)],
            is_constructor: true,
        }],
        false,
    );
    assert_eq!(
        eval_expr::<f64>(
            &fn_call("Pkg.pickFirstComplex.result.re", vec![arg.clone()]),
            &env
        ),
        2.0
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Pkg.pickFirstComplex.result.im", vec![arg]), &env),
        -3.0
    );
}

#[test]
fn test_eval_function_call_projected_complex_sum_with_slice_field_access() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = Function::new("Pkg.sumComplex", Default::default());
    f.add_input(FunctionParam::new("v", "Modelica.ComplexMath.Complex").with_dims(vec![1]));
    f.add_output(FunctionParam::new("result", "Modelica.ComplexMath.Complex"));
    f.body = vec![Statement::Assignment {
        comp: comp_ref("result"),
        value: Expression::FunctionCall {
            name: VarName::new("Complex"),
            args: vec![
                Expression::BuiltinCall {
                    function: BuiltinFunction::Sum,
                    args: vec![Expression::FieldAccess {
                        base: Box::new(Expression::VarRef {
                            name: VarName::new("v"),
                            subscripts: vec![Subscript::Colon],
                        }),
                        field: "re".to_string(),
                    }],
                },
                Expression::BuiltinCall {
                    function: BuiltinFunction::Sum,
                    args: vec![Expression::FieldAccess {
                        base: Box::new(Expression::VarRef {
                            name: VarName::new("v"),
                            subscripts: vec![Subscript::Colon],
                        }),
                        field: "im".to_string(),
                    }],
                },
            ],
            is_constructor: true,
        },
    }];
    funcs.insert("Pkg.sumComplex".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    let arg = arr(
        vec![Expression::FunctionCall {
            name: VarName::new("Complex"),
            args: vec![lit(2.0), lit(-3.0)],
            is_constructor: true,
        }],
        false,
    );
    assert_eq!(
        eval_expr::<f64>(
            &fn_call("Pkg.sumComplex.result.re", vec![arg.clone()]),
            &env
        ),
        2.0
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Pkg.sumComplex.result.im", vec![arg]), &env),
        -3.0
    );
}

#[test]
fn test_eval_builtin_sum_with_encoded_slice_field_varref_name() {
    let mut env = VarEnv::<f64>::new();
    std::sync::Arc::make_mut(&mut env.dims).insert("v".to_string(), vec![3]);
    env.set("v[1]", 2.0);
    env.set("v[2]", 1.0);
    env.set("v[3]", -5.0);
    env.set("v[1].re", 2.0);
    env.set("v[2].re", 1.0);
    env.set("v[3].re", -5.0);

    let expr = fn_call("sum", vec![var("v[:].re")]);
    assert_eq!(eval_expr::<f64>(&expr, &env), -2.0);
}

#[test]
fn test_eval_array_values_record_field_varref_reads_indexed_record_elements() {
    let mut env = VarEnv::<f64>::new();
    std::sync::Arc::make_mut(&mut env.dims).insert("cellData.rcData.R".to_string(), vec![2]);
    env.set("cellData.rcData[1].R", 0.2);
    env.set("cellData.rcData[2].R", 0.1);

    let values = eval_array_values::<f64>(&var("cellData.rcData.R"), &env);
    assert_eq!(values.len(), 2);
    assert!((values[0] - 0.2).abs() < 1.0e-12);
    assert!((values[1] - 0.1).abs() < 1.0e-12);
}

#[test]
fn test_eval_builtin_sum_record_field_varref_reads_indexed_record_elements() {
    let mut env = VarEnv::<f64>::new();
    std::sync::Arc::make_mut(&mut env.dims).insert("cellData.rcData.R".to_string(), vec![2]);
    env.set("cellData.rcData[1].R", 0.2);
    env.set("cellData.rcData[2].R", 0.1);

    let expr = fn_call("sum", vec![var("cellData.rcData.R")]);
    assert!((eval_expr::<f64>(&expr, &env) - 0.3).abs() < 1.0e-12);
}

#[test]
fn test_eval_function_call_projected_complex_sum_with_encoded_slice_varref() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = Function::new("Pkg.sumComplexEncoded", Default::default());
    f.add_input(FunctionParam::new("v", "Modelica.ComplexMath.Complex").with_dims(vec![3]));
    f.add_output(FunctionParam::new("result", "Modelica.ComplexMath.Complex"));
    f.body = vec![Statement::Assignment {
        comp: comp_ref("result"),
        value: Expression::FunctionCall {
            name: VarName::new("Complex"),
            args: vec![
                fn_call("Modelica.ComplexMath.sum", vec![var("v[:].re")]),
                fn_call("Modelica.ComplexMath.sum", vec![var("v[:].im")]),
            ],
            is_constructor: true,
        },
    }];
    funcs.insert("Pkg.sumComplexEncoded".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    let arg = arr(
        vec![
            Expression::FunctionCall {
                name: VarName::new("Complex"),
                args: vec![lit(2.0), lit(-3.0)],
                is_constructor: true,
            },
            Expression::FunctionCall {
                name: VarName::new("Complex"),
                args: vec![lit(1.0), lit(4.0)],
                is_constructor: true,
            },
            Expression::FunctionCall {
                name: VarName::new("Complex"),
                args: vec![lit(-5.0), lit(2.0)],
                is_constructor: true,
            },
        ],
        false,
    );
    assert!(
        (eval_expr::<f64>(
            &fn_call("Pkg.sumComplexEncoded.result.re", vec![arg.clone()]),
            &env
        ) + 2.0)
            .abs()
            < 1.0e-12
    );
    assert!(
        (eval_expr::<f64>(&fn_call("Pkg.sumComplexEncoded.result.im", vec![arg]), &env) - 3.0)
            .abs()
            < 1.0e-12
    );
}
