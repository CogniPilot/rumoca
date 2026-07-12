use super::*;

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
            component_ref: Some(source_component_ref_from_name("Q")),
            dims: vec![4],
            ..scalar_var("Q")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("frame.R.T"),
        dae::Variable {
            component_ref: Some(source_component_ref_from_name("frame.R.T")),
            dims: vec![3, 3],
            ..scalar_var("frame.R.T")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("frame.R.w"),
        dae::Variable {
            component_ref: Some(source_component_ref_from_name("frame.R.w")),
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
    let mut orientation = rumoca_core::Function::new("Pkg.Orientation", lower_test_span());
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
        rumoca_core::Function::new("Pkg.angularVelocity2", lower_test_span());
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
    let mut from_q = rumoca_core::Function::new("Pkg.from_Q", lower_test_span());
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
        span: lower_test_span(),
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

pub(super) fn projection_assignment(
    target: &str,
    value: rumoca_core::Expression,
) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: projection_component_ref(target),
        value,
        span: lower_test_span(),
    }
}

fn projection_component_ref(name: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: lower_test_span(),
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: lower_test_span(),
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
        type_def_id: None,
        name: name.to_string(),
        span: lower_test_span(),
        type_name: type_name.to_string(),
        type_class: None,
        dims: dims.to_vec(),
        shape_expr: Vec::new(),
        default: None,
        min: None,
        max: None,
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
        span: lower_test_span(),
    }
}

pub(super) fn projection_call(
    name: &str,
    args: Vec<rumoca_core::Expression>,
    is_constructor: bool,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor,
        span: lower_test_span(),
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

    let rows = scalar_program_block_fixture(&block);
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
            component_ref: Some(source_component_ref_from_name("frame.R.T")),
            dims: vec![3, 3],
            ..scalar_var("frame.R.T")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("frame.R.w"),
        dae::Variable {
            component_ref: Some(source_component_ref_from_name("frame.R.w")),
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
    let mut orientation = rumoca_core::Function::new("Pkg.Orientation", lower_test_span());
    orientation
        .inputs
        .push(function_param_with_dims("T", &[3, 3]));
    orientation.inputs.push(function_param_with_dims("w", &[3]));
    orientation
}

fn rotation_matrix_function() -> rumoca_core::Function {
    let mut rotation = rumoca_core::Function::new("Pkg.rotation", lower_test_span());
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
        span: lower_test_span(),
    });
    rotation
}

fn resolve2_function() -> rumoca_core::Function {
    let mut resolve2 = rumoca_core::Function::new("Pkg.resolve2", lower_test_span());
    resolve2.inputs.push(function_param_with_dims("T", &[3, 3]));
    resolve2.inputs.push(function_param_with_dims("v1", &[3]));
    resolve2.outputs.push(function_param_with_dims("v2", &[3]));
    resolve2.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("v2"),
        value: mul(var("T"), var("v1")),
        span: lower_test_span(),
    });
    resolve2
}

fn axes_function() -> rumoca_core::Function {
    let mut axes = rumoca_core::Function::new("Pkg.axes", lower_test_span());
    axes.inputs
        .push(function_param_with_dims("der_angles", &[3]));
    axes.outputs.push(rumoca_core::FunctionParam {
        type_name: "Pkg.Orientation".to_string(),
        ..function_param("R")
    });
    axes.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("R"),
        value: axes_orientation_call(),
        span: lower_test_span(),
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
        span: lower_test_span(),
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
    let rows = scalar_program_block_fixture(&block);
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
    let rows = scalar_program_block_fixture(&block);
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
                component_ref: Some(source_component_ref_from_name("frame.R.T")),
                dims: vec![3, 3],
                ..scalar_var("frame.R.T")
            },
        );
    }
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("frame.R.w"),
        dae::Variable {
            component_ref: Some(source_component_ref_from_name("frame.R.w")),
            dims: vec![3],
            ..scalar_var("frame.R.w")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("e"),
        dae::Variable {
            component_ref: Some(source_component_ref_from_name("e")),
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
    let mut planar = rumoca_core::Function::new("Pkg.planarRotation", lower_test_span());
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
        span: lower_test_span(),
        origin: origin.to_string(),
        scalar_count,
    });
}
