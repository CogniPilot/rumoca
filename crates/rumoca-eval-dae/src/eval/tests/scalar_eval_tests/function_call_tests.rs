use super::*;

fn function_param(name: &str, type_name: &str) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam::new(
        name,
        type_name,
        rumoca_core::Span::source_free_serde_default(),
    )
}

fn shape_expr_param(
    name: &str,
    type_name: &str,
    shape_expr: rumoca_core::Subscript,
) -> rumoca_core::FunctionParam {
    function_param(name, type_name)
        .with_dims(vec![0])
        .with_shape_expr(vec![shape_expr])
}

fn pkg_record_shape_env() -> VarEnv<f64> {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut state_ctor = rumoca_core::Function::new("Pkg.State", rumoca_core::Span::DUMMY);
    state_ctor.add_input(function_param("p", "Real"));
    state_ctor.add_input(shape_expr_param(
        "X",
        "Real",
        rumoca_core::Subscript::generated_expr(Box::new(var("nX")), rumoca_core::Span::DUMMY),
    ));
    state_ctor.is_constructor = true;
    funcs.insert("Pkg.State".to_string(), state_ctor);

    let mut set_state = rumoca_core::Function::new("Pkg.setState", rumoca_core::Span::DUMMY);
    set_state.add_input(shape_expr_param(
        "X",
        "Real",
        rumoca_core::Subscript::generated_colon(rumoca_core::Span::DUMMY),
    ));
    set_state.add_output(
        function_param("state", "State").with_type_class(rumoca_core::ClassType::Record),
    );
    set_state.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("state"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.State"),
            args: vec![
                named_ctor_arg("p", lit(101325.0)),
                named_ctor_arg("X", var("X")),
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.setState".to_string(), set_state);

    let mut density = rumoca_core::Function::new("Pkg.density", rumoca_core::Span::DUMMY);
    density.add_input(shape_expr_param(
        "state_X",
        "Real",
        rumoca_core::Subscript::generated_expr(Box::new(var("nX")), rumoca_core::Span::DUMMY),
    ));
    density.add_output(function_param("d", "Real").with_default(var("nX")));
    density.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.density".to_string(), density);

    let mut outer = rumoca_core::Function::new("Pkg.outer", rumoca_core::Span::DUMMY);
    outer.add_input(shape_expr_param(
        "X",
        "Real",
        rumoca_core::Subscript::generated_colon(rumoca_core::Span::DUMMY),
    ));
    outer.add_output(function_param("d", "Real").with_default(fn_call(
        "Pkg.density",
        vec![field(fn_call("Pkg.setState", vec![var("X")]), "X")],
    )));
    outer.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.outer".to_string(), outer);

    env.functions = Arc::new(funcs);
    env
}

fn buildings_air_state_ctor() -> rumoca_core::Function {
    let mut state_ctor = rumoca_core::Function::new(
        "Buildings.Media.Air.ThermodynamicState",
        rumoca_core::Span::DUMMY,
    );
    state_ctor.add_input(function_param("p", "Real"));
    state_ctor.add_input(function_param("T", "Real"));
    state_ctor.add_input(shape_expr_param(
        "X",
        "Real",
        rumoca_core::Subscript::generated_expr(Box::new(var("nX")), rumoca_core::Span::DUMMY),
    ));
    state_ctor.is_constructor = true;
    state_ctor
}

fn thermodynamic_state_call(x: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Buildings.Media.Air.ThermodynamicState"),
        args: vec![
            named_ctor_arg("p", var("p")),
            named_ctor_arg("T", var("T")),
            named_ctor_arg("X", x),
        ],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    }
}

fn padded_mass_fraction_expr() -> rumoca_core::Expression {
    builtin(
        rumoca_core::BuiltinFunction::Cat,
        vec![
            int_lit(1),
            var("X"),
            arr(
                vec![binop(
                    rumoca_core::OpBinary::Sub,
                    lit(1.0),
                    builtin(rumoca_core::BuiltinFunction::Sum, vec![var("X")]),
                )],
                false,
            ),
        ],
    )
}

fn buildings_air_set_state_ptx_with_x_padding() -> rumoca_core::Function {
    let mut set_state =
        rumoca_core::Function::new("Buildings.Media.Air.setState_pTX", rumoca_core::Span::DUMMY);
    set_state.add_input(function_param("p", "Real"));
    set_state.add_input(function_param("T", "Real"));
    set_state.add_input(shape_expr_param(
        "X",
        "Real",
        rumoca_core::Subscript::generated_colon(rumoca_core::Span::DUMMY),
    ));
    set_state.add_output(
        function_param("state", "ThermodynamicState")
            .with_type_class(rumoca_core::ClassType::Record),
    );
    set_state.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("state"),
        value: rumoca_core::Expression::If {
            branches: vec![(
                binop(
                    rumoca_core::OpBinary::Eq,
                    builtin(
                        rumoca_core::BuiltinFunction::Size,
                        vec![var("X"), int_lit(1)],
                    ),
                    int_lit(2),
                ),
                thermodynamic_state_call(var("X")),
            )],
            else_branch: Box::new(thermodynamic_state_call(padded_mass_fraction_expr())),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    set_state
}

#[test]
fn test_eval_user_function_binds_string_array_input_shape_for_size() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut f = rumoca_core::Function::new("Pkg.nSubstances", rumoca_core::Span::DUMMY);
    f.add_input(
        rumoca_core::FunctionParam::new(
            "substanceNames",
            "String",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_dims(vec![0]),
    );
    f.add_output(
        rumoca_core::FunctionParam::new(
            "n",
            "Integer",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(builtin(
            rumoca_core::BuiltinFunction::Size,
            vec![var("substanceNames"), int_lit(1)],
        )),
    );
    f.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.nSubstances".to_string(), f);
    env.functions = Arc::new(funcs);

    let names = arr(
        vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("N2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("O2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("H2O".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("CO2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        false,
    );

    let expr = fn_call("Pkg.nSubstances", vec![names]);

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(4.0));
}

#[test]
fn test_eval_user_function_binds_shape_expr_variable_from_array_input() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut f = rumoca_core::Function::new("Pkg.massFractionCount", rumoca_core::Span::DUMMY);
    f.add_input(
        rumoca_core::FunctionParam::new(
            "X",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_dims(vec![0])
        .with_shape_expr(vec![rumoca_core::Subscript::generated_expr(
            Box::new(var("nX")),
            rumoca_core::Span::DUMMY,
        )]),
    );
    f.add_output(
        rumoca_core::FunctionParam::new(
            "n",
            "Integer",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(var("nX")),
    );
    f.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.massFractionCount".to_string(), f);
    env.functions = Arc::new(funcs);

    let expr = fn_call(
        "Pkg.massFractionCount",
        vec![arr(vec![lit(0.7), lit(0.3)], false)],
    );

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(2.0));
}

#[test]
fn test_eval_nested_user_function_preserves_shape_expr_variable_from_caller_input() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut inner = rumoca_core::Function::new("Pkg.inner", rumoca_core::Span::DUMMY);
    inner.add_input(
        rumoca_core::FunctionParam::new(
            "state_X",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_dims(vec![0])
        .with_shape_expr(vec![rumoca_core::Subscript::generated_expr(
            Box::new(var("nX")),
            rumoca_core::Span::DUMMY,
        )]),
    );
    inner.add_output(
        rumoca_core::FunctionParam::new(
            "d",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(var("nX")),
    );
    inner.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.inner".to_string(), inner);

    let mut outer = rumoca_core::Function::new("Pkg.outer", rumoca_core::Span::DUMMY);
    outer.add_input(
        rumoca_core::FunctionParam::new(
            "X",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_dims(vec![0])
        .with_shape_expr(vec![rumoca_core::Subscript::generated_colon(
            rumoca_core::Span::DUMMY,
        )]),
    );
    outer.add_output(
        rumoca_core::FunctionParam::new(
            "d",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(fn_call("Pkg.inner", vec![var("X")])),
    );
    outer.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.outer".to_string(), outer);
    env.functions = Arc::new(funcs);

    let expr = fn_call("Pkg.outer", vec![arr(vec![lit(0.7), lit(0.3)], false)]);

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(2.0));
}

#[test]
fn test_eval_record_function_array_field_preserves_shape_expr_variable() {
    let env = pkg_record_shape_env();
    let expr = fn_call("Pkg.outer", vec![arr(vec![lit(0.7), lit(0.3)], false)]);

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(2.0));
}

#[test]
fn test_eval_set_state_ptx_x_field_binds_shape_expr_variable() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut density =
        rumoca_core::Function::new("Buildings.Media.Air.density", rumoca_core::Span::DUMMY);
    density.add_input(
        rumoca_core::FunctionParam::new(
            "state_X",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_dims(vec![0])
        .with_shape_expr(vec![rumoca_core::Subscript::generated_expr(
            Box::new(var("nX")),
            rumoca_core::Span::DUMMY,
        )]),
    );
    density.add_output(
        rumoca_core::FunctionParam::new(
            "d",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(var("nX")),
    );
    density.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Buildings.Media.Air.density".to_string(), density);
    env.functions = Arc::new(funcs);

    let state_x = field(
        fn_call(
            "Buildings.Media.Air.setState_pTX",
            vec![
                lit(101325.0),
                lit(293.15),
                arr(vec![lit(0.01), lit(0.99)], false),
            ],
        ),
        "X",
    );
    let expr = fn_call("Buildings.Media.Air.density", vec![state_x]);

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(2.0));
}

#[test]
fn test_eval_set_state_ptx_x_field_uses_user_function_body_before_accessor_fallback() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    funcs.insert(
        "Buildings.Media.Air.ThermodynamicState".to_string(),
        buildings_air_state_ctor(),
    );
    funcs.insert(
        "Buildings.Media.Air.setState_pTX".to_string(),
        buildings_air_set_state_ptx_with_x_padding(),
    );
    env.functions = Arc::new(funcs);

    let expr = field(
        fn_call(
            "Buildings.Media.Air.setState_pTX",
            vec![lit(101325.0), lit(293.15), arr(vec![lit(0.01)], false)],
        ),
        "X",
    );

    assert_eq!(eval_array_values::<f64>(&expr, &env), Ok(vec![0.01, 0.99]));
}

#[test]
fn test_eval_user_function_binds_record_input_fields_from_varref_argument() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut f = rumoca_core::Function::new("Pkg.stateMetric", rumoca_core::Span::DUMMY);
    f.add_input(rumoca_core::FunctionParam::new(
        "st",
        "State",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_output(
        rumoca_core::FunctionParam::new(
            "y",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(binop(rumoca_core::OpBinary::Add, var("st.p"), var("st.T"))),
    );
    f.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.stateMetric".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    env.set("arg.p", 101325.0);
    env.set("arg.T", 350.0);

    let expr = fn_call("Pkg.stateMetric", vec![var("arg")]);
    assert!((eval_expr_value::<f64>(&expr, &env) - 101675.0).abs() < 1e-9);
    assert!((eval_expr::<f64>(&expr, &env).expect("strict eval") - 101675.0).abs() < 1e-9);
}

#[test]
fn test_eval_user_function_binds_record_input_fields_from_start_exprs() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut f = rumoca_core::Function::new("Pkg.stateMetric", rumoca_core::Span::DUMMY);
    f.add_input(rumoca_core::FunctionParam::new(
        "st",
        "State",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_output(
        rumoca_core::FunctionParam::new(
            "y",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(binop(rumoca_core::OpBinary::Add, var("st.p"), var("st.T"))),
    );
    f.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.stateMetric".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    let mut starts = IndexMap::new();
    starts.insert("arg.p".to_string(), lit(101325.0));
    starts.insert("arg.T".to_string(), lit(350.0));
    env.start_exprs = std::sync::Arc::new(starts);

    let expr = fn_call("Pkg.stateMetric", vec![var("arg")]);
    assert!((eval_expr::<f64>(&expr, &env).expect("strict eval") - 101675.0).abs() < 1e-9);
}

#[test]
fn test_eval_user_function_binds_record_input_fields_from_constructor_argument() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut f = rumoca_core::Function::new("Pkg.brushVoltageDrop", rumoca_core::Span::DUMMY);
    f.add_input(
        rumoca_core::FunctionParam::new(
            "brushParameters",
            "BrushParameters",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_type_class(rumoca_core::ClassType::Record),
    );
    f.add_input(rumoca_core::FunctionParam::new(
        "i",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_output(rumoca_core::FunctionParam::new(
        "v",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("v"),
        value: rumoca_core::Expression::If {
            branches: vec![(
                binop(
                    rumoca_core::OpBinary::Le,
                    field(var("brushParameters"), "V"),
                    lit(0.0),
                ),
                lit(0.0),
            )],
            else_branch: Box::new(binop(
                rumoca_core::OpBinary::Mul,
                field(var("brushParameters"), "V"),
                var("i"),
            )),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.brushVoltageDrop".to_string(), f);
    env.functions = Arc::new(funcs);

    let brush_parameters = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.BrushParameters"),
        args: vec![
            named_ctor_arg("V", lit(0.5)),
            named_ctor_arg("ILinear", lit(1.0)),
        ],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };
    let expr = fn_call("Pkg.brushVoltageDrop", vec![brush_parameters, lit(4.0)]);

    assert!((eval_expr::<f64>(&expr, &env).expect("strict eval") - 2.0).abs() < 1e-9);
}

#[test]
fn test_eval_user_function_binds_omitted_record_constructor_fields_from_start_exprs() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut f = rumoca_core::Function::new("Pkg.brushVoltageDrop", rumoca_core::Span::DUMMY);
    f.add_input(
        rumoca_core::FunctionParam::new(
            "brushParameters",
            "BrushParameters",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_type_class(rumoca_core::ClassType::Record),
    );
    f.add_input(rumoca_core::FunctionParam::new(
        "i",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_output(
        rumoca_core::FunctionParam::new(
            "v",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(binop(
            rumoca_core::OpBinary::Mul,
            field(var("brushParameters"), "V"),
            var("i"),
        )),
    );
    f.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.brushVoltageDrop".to_string(), f);
    env.functions = Arc::new(funcs);

    let mut starts = IndexMap::new();
    starts.insert("dcpm.brushParameters.V".to_string(), lit(0.5));
    starts.insert("dcpm.brushParameters.ILinear".to_string(), lit(1.0));
    env.start_exprs = Arc::new(starts);

    let brush_parameters = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.BrushParameters"),
        args: vec![named_ctor_arg("ILinear", lit(1.0))],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };
    let expr = fn_call("Pkg.brushVoltageDrop", vec![brush_parameters, lit(4.0)]);

    assert!((eval_expr::<f64>(&expr, &env).expect("strict eval") - 2.0).abs() < 1e-9);
}

#[test]
fn test_eval_user_function_binds_record_input_fields_from_record_function_output() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut state = rumoca_core::Function::new("Pkg.State", rumoca_core::Span::DUMMY);
    state.is_constructor = true;
    state.add_input(rumoca_core::FunctionParam::new(
        "p",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    state.add_input(rumoca_core::FunctionParam::new(
        "T",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    funcs.insert("Pkg.State".to_string(), state);

    let mut make_state = rumoca_core::Function::new("Pkg.makeState", rumoca_core::Span::DUMMY);
    make_state.add_output(
        rumoca_core::FunctionParam::new(
            "out",
            "State",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_type_class(rumoca_core::ClassType::Record),
    );
    make_state.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("out"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.State"),
            args: vec![
                named_ctor_arg("p", lit(101325.0)),
                named_ctor_arg("T", lit(350.0)),
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.makeState".to_string(), make_state);

    let mut metric = rumoca_core::Function::new("Pkg.stateMetric", rumoca_core::Span::DUMMY);
    metric.add_input(
        rumoca_core::FunctionParam::new(
            "st",
            "State",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_type_class(rumoca_core::ClassType::Record),
    );
    metric.add_output(
        rumoca_core::FunctionParam::new(
            "y",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(binop(rumoca_core::OpBinary::Add, var("st.p"), var("st.T"))),
    );
    metric.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.stateMetric".to_string(), metric);
    env.functions = std::sync::Arc::new(funcs);

    let expr = fn_call("Pkg.stateMetric", vec![fn_call("Pkg.makeState", vec![])]);
    assert!((eval_expr::<f64>(&expr, &env).expect("strict eval") - 101675.0).abs() < 1e-9);
}

#[test]
fn test_eval_user_function_binds_record_input_fields_from_field_access_argument() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut f = rumoca_core::Function::new("Pkg.stateMetric", rumoca_core::Span::DUMMY);
    f.add_input(rumoca_core::FunctionParam::new(
        "st",
        "State",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.add_output(
        rumoca_core::FunctionParam::new(
            "y",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(binop(rumoca_core::OpBinary::Add, var("st.p"), var("st.T"))),
    );
    f.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.stateMetric".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    env.set("container.state.p", 200000.0);
    env.set("container.state.T", 500.0);

    let expr = fn_call(
        "Pkg.stateMetric",
        vec![rumoca_core::Expression::FieldAccess {
            base: Box::new(var("container")),
            field: "state".to_string(),
            span: rumoca_core::Span::DUMMY,
        }],
    );
    assert!((eval_expr_value::<f64>(&expr, &env) - 200500.0).abs() < 1e-9);
}

#[test]
fn test_eval_function_record_field_array_uses_first_element_in_scalar_context() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut state = rumoca_core::Function::new("Pkg.State", rumoca_core::Span::DUMMY);
    state.is_constructor = true;
    state.add_input(rumoca_core::FunctionParam::new(
        "p",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    state.add_input(
        rumoca_core::FunctionParam::new(
            "X",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_dims(vec![2]),
    );
    funcs.insert("Pkg.State".to_string(), state);

    let mut make_state = rumoca_core::Function::new("Pkg.makeState", rumoca_core::Span::DUMMY);
    make_state.add_output(
        rumoca_core::FunctionParam::new(
            "out",
            "State",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_type_class(rumoca_core::ClassType::Record),
    );
    make_state.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("out"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.State"),
            args: vec![
                named_ctor_arg("p", lit(101325.0)),
                named_ctor_arg("X", arr(vec![lit(0.25), lit(0.75)], false)),
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.makeState".to_string(), make_state);
    env.functions = std::sync::Arc::new(funcs);

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(fn_call("Pkg.makeState", vec![])),
        field: "X".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert!((eval_expr_value::<f64>(&expr, &env) - 0.25).abs() < 1e-9);
}

#[test]
fn test_eval_function_record_field_array_infers_unknown_shape_from_named_arg() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut state = rumoca_core::Function::new("Pkg.State", rumoca_core::Span::DUMMY);
    state.is_constructor = true;
    state.add_input(
        rumoca_core::FunctionParam::new(
            "X",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_dims(vec![0]),
    );
    funcs.insert("Pkg.State".to_string(), state);

    let mut make_state = rumoca_core::Function::new("Pkg.makeState", rumoca_core::Span::DUMMY);
    make_state.add_input(
        rumoca_core::FunctionParam::new(
            "X",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_dims(vec![0]),
    );
    make_state.add_output(
        rumoca_core::FunctionParam::new(
            "out",
            "State",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_type_class(rumoca_core::ClassType::Record),
    );
    make_state.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("out"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.State"),
            args: vec![named_ctor_arg("X", var("X"))],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.makeState".to_string(), make_state);
    env.functions = std::sync::Arc::new(funcs);

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(fn_call(
            "Pkg.makeState",
            vec![arr(vec![lit(0.2), lit(0.8)], false)],
        )),
        field: "X".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_array_values::<f64>(&expr, &env), Ok(vec![0.2, 0.8]));
}

#[test]
fn test_eval_function_call_unknown_user_function_returns_error() {
    let env = VarEnv::<f64>::new();
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("exlin"),
        args: vec![lit(2.0), lit(50.0)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(
        eval_expr::<f64>(&expr, &env),
        Err(EvalError::MissingFunction {
            name: "exlin".to_string()
        })
    );
}

#[test]
fn test_eval_function_call_unassigned_output_rejects_instead_of_defaulting_zero() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = rumoca_core::Function::new("Pkg.unassigned", rumoca_core::Span::DUMMY);
    f.add_output(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    f.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.unassigned".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    assert_eq!(
        eval_expr::<f64>(&fn_call("Pkg.unassigned", vec![]), &env),
        Err(EvalError::MissingBinding {
            name: "y".to_string()
        })
    );
}

#[test]
fn test_eval_function_call_external_stub_falls_back_to_special_handler() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut and_true_stub = rumoca_core::Function::new(
        "Modelica.Math.BooleanVectors.andTrue",
        rumoca_core::Span::DUMMY,
    );
    and_true_stub.external = Some(rumoca_core::ExternalFunction::default());
    funcs.insert(
        "Modelica.Math.BooleanVectors.andTrue".to_string(),
        and_true_stub,
    );
    env.functions = std::sync::Arc::new(funcs);

    let all_true = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Modelica.Math.BooleanVectors.andTrue"),
        args: vec![arr(vec![bool_lit(true), bool_lit(true)], false)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_value::<f64>(&all_true, &env), 1.0);

    let one_false = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Modelica.Math.BooleanVectors.andTrue"),
        args: vec![arr(vec![bool_lit(true), bool_lit(false)], false)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_value::<f64>(&one_false, &env), 0.0);
}

#[test]
fn test_runtime_special_function_precedence_over_user_body() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut first_true = rumoca_core::Function::new(
        "Modelica.Math.BooleanVectors.firstTrueIndex",
        rumoca_core::Span::DUMMY,
    );
    first_true.add_input(
        rumoca_core::FunctionParam::new(
            "u",
            "Boolean",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_dims(vec![0]),
    );
    first_true.add_input(rumoca_core::FunctionParam::new(
        "nu",
        "Integer",
        rumoca_core::Span::source_free_serde_default(),
    ));
    first_true.add_output(
        rumoca_core::FunctionParam::new(
            "index",
            "Integer",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_default(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(2),
            span: rumoca_core::Span::DUMMY,
        }),
    );
    funcs.insert(
        "Modelica.Math.BooleanVectors.firstTrueIndex".to_string(),
        first_true,
    );
    env.functions = std::sync::Arc::new(funcs);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Modelica.Math.BooleanVectors.firstTrueIndex"),
        args: vec![
            arr(vec![bool_lit(true), bool_lit(true)], false),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr_value::<f64>(&expr, &env), 1.0);
}
