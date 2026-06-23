use super::*;

#[test]
fn lower_derivative_rhs_extracts_dot_product_with_vector_function_derivative() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("r"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("r")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("y")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("T"),
        dae::Variable {
            dims: vec![3, 3],
            ..source_scalar_var("T")
        },
    );

    let call =
        |name: &str, args: Vec<rumoca_core::Expression>| rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new(name).into(),
            args,
            is_constructor: false,
            span: lower_test_span(),
        };
    let array =
        |elements: Vec<rumoca_core::Expression>, is_matrix: bool| rumoca_core::Expression::Array {
            elements,
            is_matrix,
            span: lower_test_span(),
        };
    let unit = |idx: usize| {
        array(
            (0..3)
                .map(|current| real_lit(f64::from((current == idx) as u8)))
                .collect(),
            false,
        )
    };

    let mut resolve2 = rumoca_core::Function::new("Pkg.resolve2", lower_test_span());
    resolve2.inputs.push(function_param_with_dims("T", &[3, 3]));
    resolve2.inputs.push(function_param_with_dims("v1", &[3]));
    resolve2.outputs.push(function_param_with_dims("v2", &[3]));
    resolve2.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("v2"),
        value: mul(var("T"), var("v1")),
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(resolve2.name.clone(), resolve2);

    for idx in 0..3usize {
        dae_model.continuous.equations.push(dae::Equation {
            lhs: None,
            rhs: sub(
                indexed_var("y", (idx + 1) as i64),
                mul(
                    unit(idx),
                    call("Pkg.resolve2", vec![var("T"), der(var("r"))]),
                ),
            ),
            span: lower_test_span(),
            origin: "dot product with vector function derivative".to_string(),
            scalar_count: 1,
        });
    }

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("dot product function derivative rows should lower");
    assert!(matches!(
        block.nodes.as_slice(),
        [rumoca_ir_solve::ComputeNode::LinSolve { n: 3, .. }]
    ));

    let rows = scalar_program_block_fixture(&block);
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    for idx in 1..=3 {
        set_y_value(&layout, &mut y, &format!("y[{idx}]"), idx as f64);
        for col in 1..=3 {
            let value = f64::from((idx == col) as u8);
            set_p_value(&layout, &mut p, &format!("T[{idx},{col}]"), value);
        }
    }
    let actual = eval_block_all_outputs(&rows, &y, &p, 0.0);

    assert_eq!(actual, vec![1.0, 2.0, 3.0]);
}

#[test]
fn lower_derivative_rhs_projects_transposed_matrix_function_derivative() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("r"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("r")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("y")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("T"),
        dae::Variable {
            dims: vec![3, 3],
            ..source_scalar_var("T")
        },
    );

    let call =
        |name: &str, args: Vec<rumoca_core::Expression>| rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new(name).into(),
            args,
            is_constructor: false,
            span: lower_test_span(),
        };
    let array =
        |elements: Vec<rumoca_core::Expression>, is_matrix: bool| rumoca_core::Expression::Array {
            elements,
            is_matrix,
            span: lower_test_span(),
        };
    let unit = |idx: usize| {
        array(
            (0..3)
                .map(|current| real_lit(f64::from((current == idx) as u8)))
                .collect(),
            false,
        )
    };
    let transpose = |expr: rumoca_core::Expression| rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Transpose,
        args: vec![expr],
        span: lower_test_span(),
    };

    let mut resolve1 = rumoca_core::Function::new("Pkg.resolve1", lower_test_span());
    resolve1.inputs.push(function_param_with_dims("T", &[3, 3]));
    resolve1.inputs.push(function_param_with_dims("v2", &[3]));
    resolve1.outputs.push(function_param_with_dims("v1", &[3]));
    resolve1.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("v1"),
        value: mul(transpose(var("T")), var("v2")),
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(resolve1.name.clone(), resolve1);

    for idx in 0..3usize {
        dae_model.continuous.equations.push(dae::Equation {
            lhs: None,
            rhs: sub(
                indexed_var("y", (idx + 1) as i64),
                mul(
                    unit(idx),
                    call("Pkg.resolve1", vec![var("T"), der(var("r"))]),
                ),
            ),
            span: lower_test_span(),
            origin: "dot product with transposed vector function derivative".to_string(),
            scalar_count: 1,
        });
    }

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("transposed matrix function derivative rows should lower");
    assert!(matches!(
        block.nodes.as_slice(),
        [rumoca_ir_solve::ComputeNode::LinSolve { n: 3, .. }]
    ));
}

#[test]
fn lower_derivative_rhs_projects_vector_function_derivative_divided_by_scalar() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("r"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("r")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("a"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("a")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("T"),
        dae::Variable {
            dims: vec![3, 3],
            ..source_scalar_var("T")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("c"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("c")
        },
    );
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("length"), scalar_var("length"));

    let call =
        |name: &str, args: Vec<rumoca_core::Expression>| rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new(name).into(),
            args,
            is_constructor: false,
            span: lower_test_span(),
        };
    let div = |lhs: rumoca_core::Expression, rhs: rumoca_core::Expression| {
        binary(rumoca_core::OpBinary::Div, lhs, rhs)
    };

    let mut resolve2 = rumoca_core::Function::new("Pkg.resolve2", lower_test_span());
    resolve2.inputs.push(function_param_with_dims("T", &[3, 3]));
    resolve2.inputs.push(function_param_with_dims("v1", &[3]));
    resolve2.outputs.push(function_param_with_dims("v2", &[3]));
    resolve2.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("v2"),
        value: mul(var("T"), var("v1")),
        span: lower_test_span(),
    });
    dae_model
        .symbols
        .functions
        .insert(resolve2.name.clone(), resolve2);

    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("a"),
            div(
                sub(
                    call("Pkg.resolve2", vec![var("T"), der(var("r"))]),
                    var("c"),
                ),
                var("length"),
            ),
        ),
        span: lower_test_span(),
        origin: "vector function derivative divided by scalar".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("vector function derivative divided by scalar should lower");
    assert_vector_derivative_divided_outputs(&layout, &block);
}

fn assert_vector_derivative_divided_outputs(
    layout: &rumoca_ir_solve::VarLayout,
    block: &rumoca_ir_solve::ComputeBlock,
) {
    assert!(matches!(
        block.nodes.as_slice(),
        [rumoca_ir_solve::ComputeNode::LinSolve { n: 3, .. }]
    ));
    let rows = scalar_program_block_fixture(block);
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    for idx in 1..=3 {
        set_y_value(layout, &mut y, &format!("a[{idx}]"), idx as f64);
        set_p_value(layout, &mut p, &format!("c[{idx}]"), 0.0);
        for col in 1..=3 {
            set_p_value(
                layout,
                &mut p,
                &format!("T[{idx},{col}]"),
                f64::from((idx == col) as u8),
            );
        }
    }
    set_p_value(layout, &mut p, "length", 2.0);
    let actual = eval_block_all_outputs(&rows, &y, &p, 0.0);
    assert_eq!(actual, vec![2.0, 4.0, 6.0]);
}

#[test]
fn lower_derivative_rhs_projects_vector_if_derivative_with_zeros_else() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("r"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("r")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("v"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("v")
        },
    );
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("enabled"), scalar_var("enabled"));

    let zeros3 = builtin(rumoca_core::BuiltinFunction::Zeros, vec![int_lit(3)]);
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::If {
            branches: vec![(var("enabled"), sub(var("v"), der(var("r"))))],
            else_branch: Box::new(sub(var("v"), zeros3)),
            span: lower_test_span(),
        },
        span: lower_test_span(),
        origin: "vector if derivative with zeros else".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("vector if derivative with zeros else should lower");
    let rows = scalar_program_block_fixture(&block);
    assert_eq!(rows.programs.len(), 3);
}

#[test]
fn lower_derivative_rhs_projects_structural_if_vector_function_in_vector_if() {
    let dae_model = structural_if_vector_function_dae();

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("structural vector function branch should project scalar derivative rows");
    let rows = scalar_program_block_fixture(&block);
    assert_eq!(rows.programs.len(), 3);
}

fn structural_if_vector_function_dae() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    insert_structural_if_vector_variables(&mut dae_model);
    register_structural_if_vector_functions(&mut dae_model);
    push_structural_if_vector_equation(&mut dae_model);
    dae_model
}

fn insert_structural_if_vector_variables(dae_model: &mut dae::Dae) {
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("v"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("v")
        },
    );
    for name in ["a", "n"] {
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new(name),
            dae::Variable {
                component_ref: Some(source_component_ref_from_name(name)),
                dims: vec![3],
                ..scalar_var(name)
            },
        );
    }
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("mode"),
        dae::Variable {
            start: Some(real_lit(2.0)),
            is_tunable: false,
            ..scalar_var("mode")
        },
    );
    dae_model
        .symbols
        .enum_literal_ordinals
        .insert("Pkg.Mode.Uniform".to_string(), 2);
}

fn register_structural_if_vector_functions(dae_model: &mut dae::Dae) {
    for function in [
        length_function(),
        normalize_with_assert_function(),
        gravity_function(),
    ] {
        dae_model
            .symbols
            .functions
            .insert(function.name.clone(), function);
    }
}

fn length_function() -> rumoca_core::Function {
    let mut length = rumoca_core::Function::new("Pkg.length", lower_test_span());
    length.inputs.push(function_param_with_dims("v", &[3]));
    length.outputs.push(function_param("result"));
    length.body.push(projection_assignment(
        "result",
        builtin(
            rumoca_core::BuiltinFunction::Sqrt,
            vec![mul(var("v"), var("v"))],
        ),
    ));
    length
}

fn normalize_with_assert_function() -> rumoca_core::Function {
    let mut normalize = rumoca_core::Function::new("Pkg.normalizeWithAssert", lower_test_span());
    normalize.inputs.push(function_param_with_dims("v", &[3]));
    normalize
        .outputs
        .push(function_param_with_dims("result", &[3]));
    normalize.body.push(rumoca_core::Statement::FunctionCall {
        comp: component_ref("assert"),
        args: vec![binary(
            rumoca_core::OpBinary::Gt,
            projection_call("Pkg.length", vec![var("v")], false),
            real_lit(0.0),
        )],
        outputs: Vec::new(),
        span: lower_test_span(),
    });
    normalize.body.push(projection_assignment(
        "result",
        binary(
            rumoca_core::OpBinary::Div,
            var("v"),
            projection_call("Pkg.length", vec![var("v")], false),
        ),
    ));
    normalize
}

fn gravity_function() -> rumoca_core::Function {
    let mut gravity = rumoca_core::Function::new("Pkg.gravity", lower_test_span());
    gravity.inputs.push(function_param_with_dims("r", &[3]));
    gravity.inputs.push(function_param("mode"));
    gravity.inputs.push(function_param_with_dims("g", &[3]));
    gravity.outputs.push(function_param_with_dims("out", &[3]));
    gravity.body.push(projection_assignment(
        "out",
        rumoca_core::Expression::If {
            branches: vec![(
                binary(
                    rumoca_core::OpBinary::Eq,
                    var("mode"),
                    var("Pkg.Mode.Uniform"),
                ),
                var("g"),
            )],
            else_branch: Box::new(projection_call("Pkg.unprojected", vec![var("r")], false)),
            span: lower_test_span(),
        },
    ));
    gravity
}

fn push_structural_if_vector_equation(dae_model: &mut dae::Dae) {
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::If {
            branches: vec![(real_lit(1.0), structural_if_vector_then_branch())],
            else_branch: Box::new(sub(
                var("a"),
                builtin(rumoca_core::BuiltinFunction::Zeros, vec![int_lit(3)]),
            )),
            span: lower_test_span(),
        },
        span: lower_test_span(),
        origin: "structural vector function in vector derivative if".to_string(),
        scalar_count: 3,
    });
}

fn structural_if_vector_then_branch() -> rumoca_core::Expression {
    sub(var("a"), sub(der(var("v")), structural_if_gravity_call()))
}

fn structural_if_gravity_call() -> rumoca_core::Expression {
    projection_call(
        "Pkg.gravity",
        vec![
            var("v"),
            projection_call("__rumoca_named_arg__.mode", vec![var("mode")], true),
            projection_call(
                "__rumoca_named_arg__.g",
                vec![mul(real_lit(9.81), var("n"))],
                true,
            ),
        ],
        false,
    )
}

#[test]
fn lower_derivative_rhs_lowers_scalar_times_vector_state_derivative() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("i"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("i")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("v"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("v")
        },
    );
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("R"), scalar_var("R"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("L"), scalar_var("L"));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("v"),
            add(mul(var("R"), var("i")), mul(var("L"), der(var("i")))),
        ),
        span: lower_test_span(),
        origin: "compact scalar-vector dynamics".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = scalar_program_block_fixture(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("scalar-vector derivative equation should lower"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_y_value(&layout, &mut y, "i[1]", 2.0);
    set_y_value(&layout, &mut y, "i[2]", 5.0);
    set_y_value(&layout, &mut y, "v[1]", 14.0);
    set_y_value(&layout, &mut y, "v[2]", 26.0);
    set_p_value(&layout, &mut p, "R", 2.0);
    set_p_value(&layout, &mut p, "L", 5.0);

    let outputs = rows
        .programs
        .iter()
        .map(|row| {
            eval_linear_ops(row, &y, &p, 0.0)
                .1
                .expect("derivative output")
        })
        .collect::<Vec<_>>();
    assert_eq!(outputs, vec![2.0, 3.2]);
}
