use super::*;

#[test]
fn lower_derivative_rhs_inlines_direct_matrix_vector_assignment() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("p"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("p")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("v_b"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("v_b")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("R"),
        dae::Variable {
            dims: vec![3, 3],
            ..scalar_var("R")
        },
    );
    dae_model.variables.outputs.insert(
        rumoca_core::VarName::new("v_w"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("v_w")
        },
    );

    for idx in 1..=3 {
        dae_model.continuous.equations.push(residual(sub(
            der(indexed_var("p", idx)),
            indexed_var("v_w", idx),
        )));
    }
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("v_w").into()),
        // MLS §10.6.5: matrix * vector is a vector expression. ODE RHS
        // extraction must preserve that direct algebraic relation instead of
        // reading a stale scalarized solver slot.
        rhs: mul(var("R"), var("v_b")),
        span: Default::default(),
        origin: "direct matrix-vector assignment".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("direct matrix-vector assignment should lower into derivative RHS"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "v_b[3]", 24.0);
    set_y_value(&layout, &mut y, "R[3,3]", 1.0);
    set_y_value(&layout, &mut y, "v_w[3]", 0.0);

    let (_, output) = eval_block_output(&rows, 2, &y, &[], 0.0);

    assert_eq!(output, Some(24.0));
}

#[test]
fn lower_derivative_rhs_indexes_builtin_array_rhs() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("A"),
        dae::Variable {
            dims: vec![2, 2],
            ..scalar_var("A")
        },
    );
    let rhs = rumoca_core::Expression::Index {
        base: Box::new(builtin(
            rumoca_core::BuiltinFunction::Transpose,
            vec![var("A")],
        )),
        subscripts: vec![
            rumoca_core::Subscript::generated_index(2, rumoca_core::Span::DUMMY),
            rumoca_core::Subscript::generated_index(1, rumoca_core::Span::DUMMY),
        ],
        span: rumoca_core::Span::DUMMY,
    };
    dae_model
        .continuous
        .equations
        .push(residual(sub(der(var("x")), rhs)));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("indexed builtin array RHS should lower in derivative rows"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "A[1,2]", 42.0);

    let (_, output) = eval_block_output(&rows, 0, &y, &[], 0.0);

    assert_eq!(output, Some(42.0));
}

#[test]
fn lower_derivative_rhs_projects_scalarized_vector_function_output_with_vector_alias_arg() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("q"),
        dae::Variable {
            dims: vec![4],
            ..scalar_var("q")
        },
    );
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("omega"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("omega")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("alias_omega"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("alias_omega")
        },
    );

    let mut quat_derivative =
        rumoca_core::Function::new("Pkg.quatDerivative", rumoca_core::Span::DUMMY);
    quat_derivative
        .inputs
        .push(function_param_with_dims("q", &[4]));
    quat_derivative
        .inputs
        .push(function_param_with_dims("omega", &[3]));
    quat_derivative
        .outputs
        .push(function_param_with_dims("q_dot", &[4]));
    quat_derivative
        .body
        .push(rumoca_core::Statement::Assignment {
            comp: component_ref_index("q_dot", 1),
            value: indexed_var("omega", 1),
            span: rumoca_core::Span::DUMMY,
        });
    dae_model
        .symbols
        .functions
        .insert(quat_derivative.name.clone(), quat_derivative);

    dae_model.continuous.equations.push(residual(sub(
        der(indexed_var("q", 1)),
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.quatDerivative.q_dot[1]"),
            args: vec![var("q"), var("alias_omega")],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
    )));
    for idx in 2..=4 {
        dae_model
            .continuous
            .equations
            .push(residual(sub(der(indexed_var("q", idx)), real_lit(0.0))));
    }
    for idx in 1..=3 {
        dae_model
            .continuous
            .equations
            .push(residual(sub(der(indexed_var("omega", idx)), real_lit(0.0))));
    }
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("alias_omega[1]").into()),
        rhs: indexed_var("omega", 1),
        span: rumoca_core::Span::DUMMY,
        origin: "scalarized vector alias".to_string(),
        scalar_count: 1,
    });
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("alias_omega[2]").into()),
        rhs: indexed_var("omega", 2),
        span: rumoca_core::Span::DUMMY,
        origin: "scalarized vector alias".to_string(),
        scalar_count: 1,
    });
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("alias_omega[3]").into()),
        rhs: indexed_var("omega", 3),
        span: rumoca_core::Span::DUMMY,
        origin: "scalarized vector alias".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("projected vector function output should bind vector alias argument"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "omega[1]", 12.0);

    let (_, output) = eval_block_output(&rows, 0, &y, &[], 0.0);

    assert_eq!(output, Some(12.0));
}

#[test]
fn lower_derivative_rhs_lowers_compact_matrix_times_state_derivative() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("omega"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("omega")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("J"),
        dae::Variable {
            dims: vec![3, 3],
            ..scalar_var("J")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("M_body"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("M_body")
        },
    );
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(mul(var("J"), der(var("omega"))), var("M_body")),
        span: rumoca_core::Span::DUMMY,
        origin: "compact angular dynamics".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    lower_residual(&dae_model, &layout)
        .expect("array der() should preserve vector shape in residual lowering");
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("compact matrix-vector derivative equation should lower"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    for idx in 1..=3 {
        set_p_value(&layout, &mut p, &format!("J[{idx},{idx}]"), idx as f64);
        set_y_value(&layout, &mut y, &format!("M_body[{idx}]"), 6.0);
    }

    let outputs = eval_block_all_outputs(&rows, &y, &p, 0.0);
    assert_eq!(outputs, vec![6.0, 3.0, 2.0]);
}

#[test]
fn lower_derivative_rhs_projects_implicit_record_constructor_coupled_rows() {
    let dae_model = implicit_record_constructor_coupled_rows_dae();

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("implicit record constructor projection should expose coupled rows");
    assert!(matches!(
        block.nodes.as_slice(),
        [rumoca_ir_solve::ComputeNode::LinSolve { n: 4, .. }]
    ));
}

fn implicit_record_constructor_coupled_rows_dae() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    insert_implicit_record_constructor_variables(&mut dae_model);
    register_implicit_record_constructor_functions(&mut dae_model);
    push_implicit_record_constructor_equations(&mut dae_model);
    dae_model
}

fn insert_implicit_record_constructor_variables(dae_model: &mut dae::Dae) {
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("Q"),
        dae::Variable {
            dims: vec![4],
            ..scalar_var("Q")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("frame.R.T"),
        dae::Variable {
            dims: vec![3, 3],
            ..scalar_var("frame.R.T")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("frame.R.w"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("frame.R.w")
        },
    );
}

fn register_implicit_record_constructor_functions(dae_model: &mut dae::Dae) {
    let orientation = implicit_orientation_constructor_function();
    dae_model
        .symbols
        .functions
        .insert(orientation.name.clone(), orientation);
    let angular_velocity = angular_velocity2_function();
    dae_model
        .symbols
        .functions
        .insert(angular_velocity.name.clone(), angular_velocity);
    let from_q = from_q_function();
    dae_model
        .symbols
        .functions
        .insert(from_q.name.clone(), from_q);
}

fn implicit_orientation_constructor_function() -> rumoca_core::Function {
    let mut orientation = rumoca_core::Function::new("Pkg.Orientation", Default::default());
    orientation
        .inputs
        .push(projection_function_param("T", "Real", &[3, 3]));
    orientation
        .inputs
        .push(projection_function_param("w", "Real", &[3]));
    orientation
}

fn angular_velocity2_function() -> rumoca_core::Function {
    let mut angular_velocity =
        rumoca_core::Function::new("Pkg.angularVelocity2", Default::default());
    angular_velocity
        .inputs
        .push(projection_function_param("Q", "Pkg.Quaternion", &[]));
    angular_velocity.inputs.push(projection_function_param(
        "der_Q",
        "Pkg.der_Quaternion",
        &[],
    ));
    angular_velocity
        .outputs
        .push(projection_function_param("w", "Real", &[3]));
    angular_velocity
        .body
        .push(projection_assignment("w", angular_velocity2_rhs()));
    angular_velocity
}

fn angular_velocity2_rhs() -> rumoca_core::Expression {
    let rows = vec![
        vec![
            var("Q[4]"),
            var("Q[3]"),
            mul(real_lit(-1.0), var("Q[2]")),
            mul(real_lit(-1.0), var("Q[1]")),
        ],
        vec![
            mul(real_lit(-1.0), var("Q[3]")),
            var("Q[4]"),
            var("Q[1]"),
            mul(real_lit(-1.0), var("Q[2]")),
        ],
        vec![
            var("Q[2]"),
            mul(real_lit(-1.0), var("Q[1]")),
            var("Q[4]"),
            mul(real_lit(-1.0), var("Q[3]")),
        ],
    ];
    mul(
        real_lit(2.0),
        mul(
            projection_array(
                rows.into_iter()
                    .map(|row| projection_array(row, true))
                    .collect(),
                true,
            ),
            var("der_Q"),
        ),
    )
}

fn from_q_function() -> rumoca_core::Function {
    let mut from_q = rumoca_core::Function::new("Pkg.from_Q", Default::default());
    from_q
        .inputs
        .push(projection_function_param("Q", "Pkg.Quaternion", &[]));
    from_q
        .inputs
        .push(projection_function_param("w", "Real", &[3]));
    from_q
        .outputs
        .push(projection_function_param("R", "Pkg.Orientation", &[]));
    from_q
        .body
        .push(projection_assignment("R", from_q_orientation_call()));
    from_q
}

fn from_q_orientation_call() -> rumoca_core::Expression {
    projection_call(
        "Pkg.Orientation",
        vec![
            projection_identity_matrix_3(),
            projection_call("__rumoca_named_arg__.w", vec![var("w")], true),
        ],
        false,
    )
}

fn push_implicit_record_constructor_equations(dae_model: &mut dae::Dae) {
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("frame.R"),
            projection_call(
                "Pkg.from_Q",
                vec![
                    var("Q"),
                    projection_call("Pkg.angularVelocity2", vec![var("Q"), der(var("Q"))], false),
                ],
                false,
            ),
        ),
        span: rumoca_core::Span::DUMMY,
        origin: "implicit record constructor quaternion dynamics".to_string(),
        scalar_count: 12,
    });
    dae_model.continuous.equations.push(residual(sub(
        quaternion_unit_derivative_constraint(),
        real_lit(0.0),
    )));
}

fn quaternion_unit_derivative_constraint() -> rumoca_core::Expression {
    add(
        add(
            mul(var("Q[1]"), der(indexed_var("Q", 1))),
            mul(var("Q[2]"), der(indexed_var("Q", 2))),
        ),
        add(
            mul(var("Q[3]"), der(indexed_var("Q", 3))),
            mul(var("Q[4]"), der(indexed_var("Q", 4))),
        ),
    )
}

fn projection_identity_matrix_3() -> rumoca_core::Expression {
    projection_array(
        (0..3)
            .map(|row| {
                projection_array(
                    (0..3)
                        .map(|col| real_lit(if row == col { 1.0 } else { 0.0 }))
                        .collect(),
                    true,
                )
            })
            .collect(),
        true,
    )
}

fn projection_assignment(target: &str, value: rumoca_core::Expression) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: projection_component_ref(target),
        value,
        span: rumoca_core::Span::DUMMY,
    }
}

fn projection_component_ref(name: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: Vec::new(),
        }],
        def_id: None,
    }
}

fn projection_function_param(
    name: &str,
    type_name: &str,
    dims: &[i64],
) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        def_id: None,
        name: name.to_string(),
        span: rumoca_core::Span::DUMMY,
        type_name: type_name.to_string(),
        type_class: None,
        dims: dims.to_vec(),
        shape_expr: Vec::new(),
        default: None,
        description: None,
    }
}

fn projection_array(
    elements: Vec<rumoca_core::Expression>,
    is_matrix: bool,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements,
        is_matrix,
        span: rumoca_core::Span::DUMMY,
    }
}

fn projection_call(
    name: &str,
    args: Vec<rumoca_core::Expression>,
    is_constructor: bool,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor,
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn lower_derivative_rhs_projects_matrix_function_output_inside_record_constructor() {
    let dae_model = matrix_function_output_record_constructor_dae();

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("matrix-valued function output should keep rank through derivative projection");
    assert!(matches!(
        block.nodes.as_slice(),
        [rumoca_ir_solve::ComputeNode::LinSolve { n: 3, .. }]
    ));

    let rows = rumoca_eval_solve::to_scalar_program_block(&block);
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "frame.R.w[1]", 711.0);
    set_y_value(&layout, &mut y, "frame.R.w[2]", 1602.0);
    set_y_value(&layout, &mut y, "frame.R.w[3]", 2652.0);
    let actual = eval_block_all_outputs(&rows, &y, &[], 0.0);

    assert!(
        actual
            .iter()
            .zip([1.0, 2.0, 3.0])
            .all(|(actual, expected)| (actual - expected).abs() < 1e-12),
        "{actual:?}"
    );
}

fn matrix_function_output_record_constructor_dae() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    insert_matrix_record_constructor_variables(&mut dae_model);
    register_matrix_record_constructor_functions(&mut dae_model);
    push_matrix_record_constructor_equation(&mut dae_model);
    dae_model
}

fn insert_matrix_record_constructor_variables(dae_model: &mut dae::Dae) {
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("angle"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("angle")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("frame.R.T"),
        dae::Variable {
            dims: vec![3, 3],
            ..scalar_var("frame.R.T")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("frame.R.w"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("frame.R.w")
        },
    );
}

fn register_matrix_record_constructor_functions(dae_model: &mut dae::Dae) {
    for function in [
        matrix_orientation_constructor_function(),
        rotation_matrix_function(),
        resolve2_function(),
        axes_function(),
    ] {
        dae_model
            .symbols
            .functions
            .insert(function.name.clone(), function);
    }
}

fn matrix_orientation_constructor_function() -> rumoca_core::Function {
    let mut orientation = rumoca_core::Function::new("Pkg.Orientation", Default::default());
    orientation
        .inputs
        .push(function_param_with_dims("T", &[3, 3]));
    orientation.inputs.push(function_param_with_dims("w", &[3]));
    orientation
}

fn rotation_matrix_function() -> rumoca_core::Function {
    let mut rotation = rumoca_core::Function::new("Pkg.rotation", Default::default());
    rotation
        .outputs
        .push(function_param_with_dims("T", &[3, 3]));
    rotation.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("T"),
        value: projection_matrix(vec![
            vec![1.0, 2.0, 3.0],
            vec![4.0, 5.0, 6.0],
            vec![7.0, 8.0, 10.0],
        ]),
        span: rumoca_core::Span::DUMMY,
    });
    rotation
}

fn resolve2_function() -> rumoca_core::Function {
    let mut resolve2 = rumoca_core::Function::new("Pkg.resolve2", Default::default());
    resolve2.inputs.push(function_param_with_dims("T", &[3, 3]));
    resolve2.inputs.push(function_param_with_dims("v1", &[3]));
    resolve2.outputs.push(function_param_with_dims("v2", &[3]));
    resolve2.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("v2"),
        value: mul(var("T"), var("v1")),
        span: rumoca_core::Span::DUMMY,
    });
    resolve2
}

fn axes_function() -> rumoca_core::Function {
    let mut axes = rumoca_core::Function::new("Pkg.axes", Default::default());
    axes.inputs
        .push(function_param_with_dims("der_angles", &[3]));
    axes.outputs.push(rumoca_core::FunctionParam {
        type_name: "Pkg.Orientation".to_string(),
        ..function_param("R")
    });
    axes.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("R"),
        value: axes_orientation_call(),
        span: rumoca_core::Span::DUMMY,
    });
    axes
}

fn axes_orientation_call() -> rumoca_core::Expression {
    projection_call(
        "Pkg.Orientation",
        vec![
            projection_call("Pkg.rotation", Vec::new(), false),
            add(
                add(axes_resolved_velocity(), axes_resolved_velocity()),
                axes_resolved_velocity(),
            ),
        ],
        true,
    )
}

fn axes_resolved_velocity() -> rumoca_core::Expression {
    projection_call(
        "Pkg.resolve2",
        vec![rotation_product_call(), var("der_angles")],
        false,
    )
}

fn rotation_product_call() -> rumoca_core::Expression {
    mul(
        projection_call("Pkg.rotation", Vec::new(), false),
        projection_call("Pkg.rotation", Vec::new(), false),
    )
}

fn push_matrix_record_constructor_equation(dae_model: &mut dae::Dae) {
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("frame.R"),
            projection_call(
                "Pkg.axes",
                vec![projection_array(
                    (1..=3).map(|idx| der(indexed_var("angle", idx))).collect(),
                    false,
                )],
                false,
            ),
        ),
        span: rumoca_core::Span::DUMMY,
        origin: "record constructor with nested matrix function output".to_string(),
        scalar_count: 12,
    });
}

fn projection_matrix(rows: Vec<Vec<f64>>) -> rumoca_core::Expression {
    projection_array(
        rows.into_iter()
            .map(|row| projection_array(row.into_iter().map(real_lit).collect(), false))
            .collect(),
        true,
    )
}

#[test]
fn lower_derivative_rhs_projects_record_field_with_derivative_when_sibling_field_is_unprojected() {
    let dae_model = planar_rotation_record_dae(PlanarRotationEquation::WholeRecord);

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("derivative-bearing record field should project independently");
    let rows = rumoca_eval_solve::to_scalar_program_block(&block);
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_y_value(&layout, &mut y, "frame.R.w[3]", 6.0);
    set_p_value(&layout, &mut p, "e[3]", 2.0);

    let actual = rows.programs.iter().find_map(|row| {
        let (_regs, output) = eval_linear_ops(row, &y, &p, 0.0);
        output.filter(|value| value.is_finite() && value.abs() > 0.0)
    });
    assert_eq!(actual, Some(3.0));
}

#[test]
fn lower_derivative_rhs_projects_record_function_field_access() {
    let dae_model = planar_rotation_record_dae(PlanarRotationEquation::FieldAccess);

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("derivative-bearing record function field should project");
    let rows = rumoca_eval_solve::to_scalar_program_block(&block);
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_y_value(&layout, &mut y, "frame.R.w[3]", 6.0);
    set_p_value(&layout, &mut p, "e[3]", 2.0);

    let actual = rows.programs.iter().find_map(|row| {
        let (_regs, output) = eval_linear_ops(row, &y, &p, 0.0);
        output.filter(|value| value.is_finite() && value.abs() > 0.0)
    });
    assert_eq!(actual, Some(3.0));
}

enum PlanarRotationEquation {
    WholeRecord,
    FieldAccess,
}

fn planar_rotation_record_dae(equation: PlanarRotationEquation) -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    insert_planar_rotation_variables(&mut dae_model, &equation);
    register_planar_rotation_functions(&mut dae_model);
    push_planar_rotation_equation(&mut dae_model, equation);
    dae_model
}

fn insert_planar_rotation_variables(dae_model: &mut dae::Dae, equation: &PlanarRotationEquation) {
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("angle"), scalar_var("angle"));
    if matches!(equation, PlanarRotationEquation::WholeRecord) {
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new("frame.R.T"),
            dae::Variable {
                dims: vec![3, 3],
                ..scalar_var("frame.R.T")
            },
        );
    }
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("frame.R.w"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("frame.R.w")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("e"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("e")
        },
    );
}

fn register_planar_rotation_functions(dae_model: &mut dae::Dae) {
    let orientation = matrix_orientation_constructor_function();
    dae_model
        .symbols
        .functions
        .insert(orientation.name.clone(), orientation);
    let planar = planar_rotation_function();
    dae_model
        .symbols
        .functions
        .insert(planar.name.clone(), planar);
}

fn planar_rotation_function() -> rumoca_core::Function {
    let mut planar = rumoca_core::Function::new("Pkg.planarRotation", Default::default());
    planar.inputs.push(function_param_with_dims("e", &[3]));
    planar.inputs.push(function_param("angle"));
    planar.inputs.push(function_param("der_angle"));
    planar.outputs.push(rumoca_core::FunctionParam {
        type_name: "Pkg.Orientation".to_string(),
        ..function_param("R")
    });
    planar
        .body
        .push(projection_assignment("R", planar_orientation_call()));
    planar
}

fn planar_orientation_call() -> rumoca_core::Expression {
    projection_call(
        "Pkg.Orientation",
        vec![
            projection_call(
                "__rumoca_named_arg__.T",
                vec![builtin(
                    rumoca_core::BuiltinFunction::Identity,
                    vec![int_lit(3)],
                )],
                true,
            ),
            projection_call(
                "__rumoca_named_arg__.w",
                vec![mul(var("e"), var("der_angle"))],
                true,
            ),
        ],
        true,
    )
}

fn push_planar_rotation_equation(dae_model: &mut dae::Dae, equation: PlanarRotationEquation) {
    let call = projection_call(
        "Pkg.planarRotation",
        vec![var("e"), var("angle"), der(var("angle"))],
        false,
    );
    let (lhs, rhs, scalar_count, origin) = match equation {
        PlanarRotationEquation::WholeRecord => (
            var("frame.R"),
            call,
            12,
            "record field derivative with unprojected sibling field",
        ),
        PlanarRotationEquation::FieldAccess => (
            var("frame.R.w"),
            field_access(call, "w"),
            3,
            "record function field derivative",
        ),
    };
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(lhs, rhs),
        span: rumoca_core::Span::DUMMY,
        origin: origin.to_string(),
        scalar_count,
    });
}

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
            ..scalar_var("T")
        },
    );

    let call =
        |name: &str, args: Vec<rumoca_core::Expression>| rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new(name).into(),
            args,
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        };
    let array =
        |elements: Vec<rumoca_core::Expression>, is_matrix: bool| rumoca_core::Expression::Array {
            elements,
            is_matrix,
            span: rumoca_core::Span::DUMMY,
        };
    let unit = |idx: usize| {
        array(
            (0..3)
                .map(|current| real_lit(f64::from((current == idx) as u8)))
                .collect(),
            false,
        )
    };

    let mut resolve2 = rumoca_core::Function::new("Pkg.resolve2", Default::default());
    resolve2.inputs.push(function_param_with_dims("T", &[3, 3]));
    resolve2.inputs.push(function_param_with_dims("v1", &[3]));
    resolve2.outputs.push(function_param_with_dims("v2", &[3]));
    resolve2.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("v2"),
        value: mul(var("T"), var("v1")),
        span: rumoca_core::Span::DUMMY,
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
            span: rumoca_core::Span::DUMMY,
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

    let rows = rumoca_eval_solve::to_scalar_program_block(&block);
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
            ..scalar_var("T")
        },
    );

    let call =
        |name: &str, args: Vec<rumoca_core::Expression>| rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new(name).into(),
            args,
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        };
    let array =
        |elements: Vec<rumoca_core::Expression>, is_matrix: bool| rumoca_core::Expression::Array {
            elements,
            is_matrix,
            span: rumoca_core::Span::DUMMY,
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
        span: rumoca_core::Span::DUMMY,
    };

    let mut resolve1 = rumoca_core::Function::new("Pkg.resolve1", Default::default());
    resolve1.inputs.push(function_param_with_dims("T", &[3, 3]));
    resolve1.inputs.push(function_param_with_dims("v2", &[3]));
    resolve1.outputs.push(function_param_with_dims("v1", &[3]));
    resolve1.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("v1"),
        value: mul(transpose(var("T")), var("v2")),
        span: rumoca_core::Span::DUMMY,
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
            span: rumoca_core::Span::DUMMY,
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
            ..scalar_var("T")
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
            span: rumoca_core::Span::DUMMY,
        };
    let div = |lhs: rumoca_core::Expression, rhs: rumoca_core::Expression| {
        binary(rumoca_core::OpBinary::Div, lhs, rhs)
    };

    let mut resolve2 = rumoca_core::Function::new("Pkg.resolve2", Default::default());
    resolve2.inputs.push(function_param_with_dims("T", &[3, 3]));
    resolve2.inputs.push(function_param_with_dims("v1", &[3]));
    resolve2.outputs.push(function_param_with_dims("v2", &[3]));
    resolve2.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("v2"),
        value: mul(var("T"), var("v1")),
        span: rumoca_core::Span::DUMMY,
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
        span: rumoca_core::Span::DUMMY,
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
    let rows = rumoca_eval_solve::to_scalar_program_block(block);
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
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
        origin: "vector if derivative with zeros else".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("vector if derivative with zeros else should lower");
    let rows = rumoca_eval_solve::to_scalar_program_block(&block);
    assert_eq!(rows.programs.len(), 3);
}

#[test]
fn lower_derivative_rhs_projects_structural_if_vector_function_in_vector_if() {
    let dae_model = structural_if_vector_function_dae();

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("structural vector function branch should project scalar derivative rows");
    let rows = rumoca_eval_solve::to_scalar_program_block(&block);
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
    let mut length = rumoca_core::Function::new("Pkg.length", Default::default());
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
    let mut normalize = rumoca_core::Function::new("Pkg.normalizeWithAssert", Default::default());
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
        span: rumoca_core::Span::DUMMY,
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
    let mut gravity = rumoca_core::Function::new("Pkg.gravity", Default::default());
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
            span: rumoca_core::Span::DUMMY,
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
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
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
                vec![mul(
                    real_lit(9.81),
                    projection_call("Pkg.normalizeWithAssert", vec![var("n")], false),
                )],
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
        span: rumoca_core::Span::DUMMY,
        origin: "compact scalar-vector dynamics".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = rumoca_eval_solve::to_scalar_program_block(
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
