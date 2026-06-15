use super::*;

mod derivative_projection_tests;
mod discrete_array_tests;
fn matrix_component_ref(name: &str, row: i64, col: i64) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: vec![
                rumoca_core::Subscript::generated_index(row, rumoca_core::Span::DUMMY),
                rumoca_core::Subscript::generated_index(col, rumoca_core::Span::DUMMY),
            ],
        }],
        def_id: None,
    }
}

fn builtin(
    function: rumoca_core::BuiltinFunction,
    args: Vec<rumoca_core::Expression>,
) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function,
        args,
        span: rumoca_core::Span::DUMMY,
    }
}

fn field_access(base: rumoca_core::Expression, field: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::FieldAccess {
        base: Box::new(base),
        field: field.to_string(),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn lower_expression_lowers_vector_vector_multiply_as_scalar_product() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("k"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("k")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("u")
        },
    );

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let expr = mul(var("k"), var("u"));
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("MLS §10.6.5 vector product should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "k[1]", 10.0);
    set_p_value(&layout, &mut p, "k[2]", 15.0);
    set_p_value(&layout, &mut p, "u[1]", 2.0);
    set_p_value(&layout, &mut p, "u[2]", 3.0);

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &p, 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 65.0);
}

#[test]
fn lower_residual_projects_scalarized_record_refs_in_array_row() {
    let mut dae_model = dae::Dae::default();
    for name in ["a.re", "a.im", "b.re", "b.im"] {
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: add(var("a"), var("b")),
        span: rumoca_core::Span::DUMMY,
        origin: "scalarized record sum".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout).expect("scalarized record refs should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "a.re", 1.0);
    set_p_value(&layout, &mut p, "a.im", 2.0);
    set_p_value(&layout, &mut p, "b.re", 3.0);
    set_p_value(&layout, &mut p, "b.im", 4.0);

    let (_, re) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let (_, im) = eval_linear_ops(&rows[1], &[], &p, 0.0);

    assert_eq!(re, Some(4.0));
    assert_eq!(im, Some(6.0));
}

#[test]
fn lower_complex_field_access_projects_parent_refs_from_scalarized_bindings() {
    let mut dae_model = dae::Dae::default();
    for name in ["a.re", "a.im", "b.re", "b.im"] {
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(mul(var("a"), var("b"))),
        field: "re".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("complex field access should project scalarized parent refs");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "a.re", 2.0);
    set_p_value(&layout, &mut p, "a.im", 3.0);
    set_p_value(&layout, &mut p, "b.re", 5.0);
    set_p_value(&layout, &mut p, "b.im", 7.0);

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &p, 0.0);

    assert_eq!(read_reg(&regs, lowered.result), -11.0);
}

#[test]
fn lower_residual_keeps_real_scalar_binary_factor_unprojected_in_complex_product() {
    let mut dae_model = dae::Dae::default();
    for name in ["y.re", "y.im", "u.re", "u.im", "gain1", "gain2"] {
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(var("y"), mul(add(var("gain1"), var("gain2")), var("u"))),
        span: rumoca_core::Span::DUMMY,
        origin: "complex product with real scalar expression".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout)
        .expect("Real scalar arithmetic should not be projected as Complex fields");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "y.re", 11.0);
    set_p_value(&layout, &mut p, "y.im", 17.0);
    set_p_value(&layout, &mut p, "u.re", 2.0);
    set_p_value(&layout, &mut p, "u.im", 3.0);
    set_p_value(&layout, &mut p, "gain1", 5.0);
    set_p_value(&layout, &mut p, "gain2", 7.0);

    let (_, re) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let (_, im) = eval_linear_ops(&rows[1], &[], &p, 0.0);

    assert_eq!(re, Some(-13.0));
    assert_eq!(im, Some(-19.0));
}

#[test]
fn lower_expression_selects_dynamic_scalarized_record_field_index() {
    let mut dae_model = dae::Dae::default();
    for name in [
        "plugToPin_n.plug_n.pin[1].v",
        "plugToPin_n.plug_n.pin[2].v",
        "plugToPin_n.plug_n.pin[3].v",
        "plugToPin_n.k",
    ] {
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    let pin_k = rumoca_core::Expression::Index {
        base: Box::new(var("plugToPin_n.plug_n.pin")),
        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(var(
            "plugToPin_n.k",
        )))],
        span: rumoca_core::Span::DUMMY,
    };
    let expr = field_access(pin_k, "v");

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("dynamic record field index should lower through structured selectors");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "plugToPin_n.plug_n.pin[1].v", 12.0);
    set_p_value(&layout, &mut p, "plugToPin_n.plug_n.pin[2].v", 34.0);
    set_p_value(&layout, &mut p, "plugToPin_n.plug_n.pin[3].v", 56.0);
    set_p_value(&layout, &mut p, "plugToPin_n.k", 2.0);

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &p, 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 34.0);

    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("plugToPin_n.pin_n.v"),
        scalar_var("plugToPin_n.pin_n.v"),
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("plugToPin_n.pin_n.v"), expr),
        rumoca_core::Span::DUMMY,
        "connector field projection",
    ));
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout)
        .expect("residual row should select dynamic scalarized record field");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "plugToPin_n.pin_n.v", 40.0);
    set_p_value(&layout, &mut p, "plugToPin_n.plug_n.pin[1].v", 12.0);
    set_p_value(&layout, &mut p, "plugToPin_n.plug_n.pin[2].v", 34.0);
    set_p_value(&layout, &mut p, "plugToPin_n.plug_n.pin[3].v", 56.0);
    set_p_value(&layout, &mut p, "plugToPin_n.k", 2.0);
    let (_, residual) = eval_linear_ops(&rows[0], &[], &p, 0.0);

    assert_eq!(residual, Some(6.0));
}

#[test]
fn lower_residual_projects_unflagged_complex_constructor_in_scalarized_record_row() {
    let mut dae_model = dae::Dae::default();
    let mut complex_ctor = rumoca_core::Function::new("Complex", rumoca_core::Span::DUMMY);
    complex_ctor.is_constructor = true;
    complex_ctor
        .inputs
        .push(rumoca_core::FunctionParam::new("re", "Real"));
    complex_ctor
        .inputs
        .push(rumoca_core::FunctionParam::new("im", "Real"));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("Complex"), complex_ctor);
    for name in [
        "v1.re", "v1.im", "i1.re", "i1.im", "i2.re", "i2.im", "omega", "l1", "m",
    ] {
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }

    let j = || rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Complex").into(),
        args: vec![real_lit(0.0), real_lit(1.0)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let scaled = |factor: rumoca_core::Expression, current: rumoca_core::Expression| {
        mul(mul(mul(j(), var("omega")), factor), current)
    };
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var("v1"),
            add(scaled(var("l1"), var("i1")), scaled(var("m"), var("i2"))),
        ),
        span: rumoca_core::Span::DUMMY,
        origin: "scalarized transformer row".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout)
        .expect("unflagged Complex constructor should still project as record constructor");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "v1.re", 10.0);
    set_p_value(&layout, &mut p, "v1.im", 20.0);
    set_p_value(&layout, &mut p, "omega", 2.0);
    set_p_value(&layout, &mut p, "l1", 3.0);
    set_p_value(&layout, &mut p, "m", 5.0);
    set_p_value(&layout, &mut p, "i1.re", 7.0);
    set_p_value(&layout, &mut p, "i1.im", 11.0);
    set_p_value(&layout, &mut p, "i2.re", 13.0);
    set_p_value(&layout, &mut p, "i2.im", 17.0);

    let (_, re) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let (_, im) = eval_linear_ops(&rows[1], &[], &p, 0.0);

    assert_eq!(re, Some(246.0));
    assert_eq!(im, Some(-152.0));
}

#[test]
fn lower_residual_preserves_row_matrix_literal_shape_for_matrix_vector_product() {
    let row = |values: [f64; 4]| rumoca_core::Expression::Array {
        elements: values.into_iter().map(real_lit).collect(),
        is_matrix: true,
        span: rumoca_core::Span::DUMMY,
    };
    let matrix = rumoca_core::Expression::Array {
        elements: vec![
            row([1.0, 2.0, 3.0, 4.0]),
            row([5.0, 6.0, 7.0, 8.0]),
            row([9.0, 10.0, 11.0, 12.0]),
        ],
        is_matrix: true,
        span: rumoca_core::Span::DUMMY,
    };

    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("q"),
        dae::Variable {
            dims: vec![4],
            ..scalar_var("q")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("y")
        },
    );
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(mul(matrix, var("q")), var("y")),
        span: rumoca_core::Span::DUMMY,
        origin: "row matrix literal product".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout)
        .expect("row matrix literals should keep matrix shape through Solve lowering");
    let mut p = vec![0.0; layout.p_scalars()];
    let y = vec![0.0; layout.y_scalars()];
    for idx in 1..=4 {
        set_p_value(&layout, &mut p, &format!("q[{idx}]"), 1.0);
    }

    let actual = rows
        .iter()
        .map(|row| eval_linear_ops(row, &y, &p, 0.0).1.expect("row output"))
        .collect::<Vec<_>>();
    assert_eq!(actual, vec![10.0, 26.0, 42.0]);
}

#[test]
fn lower_residual_lowers_slice_of_scalarized_record_field_array() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("vAC"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("vAC")
        },
    );
    for idx in 1..=3 {
        let name = format!("pin[{idx}].v");
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(&name), scalar_var(&name));
    }

    let pin_v = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Index {
            base: Box::new(var("pin")),
            subscripts: vec![rumoca_core::Subscript::generated_colon(
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        }),
        field: "v".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(var("vAC"), pin_v),
        span: rumoca_core::Span::DUMMY,
        origin: "record field slice residual".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout)
        .expect("scalarized record field slice should lower as vector residual");
    let mut y = vec![0.0; layout.y_scalars()];
    for idx in 1..=3 {
        set_y_value(&layout, &mut y, &format!("pin[{idx}].v"), idx as f64 * 10.0);
    }

    let outputs = rows
        .iter()
        .map(|row| eval_linear_ops(row, &y, &[], 0.0).1.expect("row output"))
        .collect::<Vec<_>>();

    assert_eq!(outputs, vec![-10.0, -20.0, -30.0]);
}

#[test]
fn lower_expression_lowers_sum_of_scalarized_record_field_array() {
    let mut dae_model = dae::Dae::default();
    for idx in 1..=3 {
        let name = format!("pin[{idx}].LossPower");
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(&name), scalar_var(&name));
    }

    let expr = builtin(
        rumoca_core::BuiltinFunction::Sum,
        vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("pin.LossPower").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }],
    );
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("sum over scalarized record field array should lower");
    let mut y = vec![0.0; layout.y_scalars()];
    for idx in 1..=3 {
        set_y_value(
            &layout,
            &mut y,
            &format!("pin[{idx}].LossPower"),
            idx as f64 * 10.0,
        );
    }

    let (regs, _) = eval_linear_ops(&lowered.ops, &y, &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 60.0);
}

#[test]
fn lower_residual_lowers_sum_of_nested_scalarized_record_field_array() {
    let mut dae_model = dae::Dae::default();
    for idx in 1..=3 {
        let name = format!("imc.rs.resistor[{idx}].LossPower");
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(&name), scalar_var(&name));
    }
    dae_model.continuous.equations.push(residual(sub(
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(60.0),
            span: rumoca_core::Span::DUMMY,
        },
        builtin(
            rumoca_core::BuiltinFunction::Sum,
            vec![rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("imc.rs.resistor.LossPower").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }],
        ),
    )));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout)
        .expect("sum over nested scalarized record field array should lower");
    let mut y = vec![0.0; layout.y_scalars()];
    for idx in 1..=3 {
        set_y_value(
            &layout,
            &mut y,
            &format!("imc.rs.resistor[{idx}].LossPower"),
            idx as f64 * 10.0,
        );
    }

    let (_, output) = eval_linear_ops(
        rows.last().expect("nested record field sum row"),
        &y,
        &[],
        0.0,
    );

    assert_eq!(output, Some(0.0));
}

#[test]
fn lower_residual_lowers_sum_of_assignment_only_scalarized_record_field_array() {
    let mut dae_model = dae::Dae::default();
    for idx in 1..=3 {
        let name = format!("pin[{idx}].LossPower");
        let v_name = format!("pin[{idx}].v");
        let i_name = format!("pin[{idx}].i");
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(&v_name), scalar_var(&v_name));
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(&i_name), scalar_var(&i_name));
        dae_model.continuous.equations.push(dae::Equation {
            lhs: Some(rumoca_core::VarName::new(&name).into()),
            rhs: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(var(&v_name)),
                rhs: Box::new(var(&i_name)),
                span: rumoca_core::Span::DUMMY,
            },
            span: rumoca_core::Span::DUMMY,
            origin: "assignment-only record field".to_string(),
            scalar_count: 1,
        });
    }
    dae_model.continuous.equations.push(residual(sub(
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(60.0),
            span: rumoca_core::Span::DUMMY,
        },
        builtin(
            rumoca_core::BuiltinFunction::Sum,
            vec![rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("pin.LossPower").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }],
        ),
    )));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout)
        .expect("sum over assignment-only scalarized record field array should lower");
    let mut y = vec![0.0; layout.y_scalars()];
    for idx in 1..=3 {
        set_y_value(&layout, &mut y, &format!("pin[{idx}].v"), idx as f64);
        set_y_value(&layout, &mut y, &format!("pin[{idx}].i"), 10.0);
    }

    let (_, output) = eval_linear_ops(
        rows.last().expect("assignment-only sum residual row"),
        &y,
        &[],
        0.0,
    );

    assert_eq!(output, Some(0.0));
}

#[test]
fn lower_residual_keeps_slot_backed_assignment_targets_slot_backed() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("x").into()),
        rhs: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(99.0),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
        origin: "slot-backed assignment".to_string(),
        scalar_count: 1,
    });
    dae_model.continuous.equations.push(residual(sub(
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(3.0),
            span: rumoca_core::Span::DUMMY,
        },
        var("x"),
    )));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout)
        .expect("slot-backed assignment target should lower through layout slot");
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "x", 3.0);

    let (_, output) = eval_linear_ops(rows.last().expect("slot-backed residual row"), &y, &[], 0.0);

    assert_eq!(output, Some(0.0));
}

#[test]
fn lower_residual_lowers_structural_slice_times_state_vector_as_scalar_product() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("n"),
        dae::Variable {
            name: rumoca_core::VarName::new("n"),
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(3),
                span: rumoca_core::Span::DUMMY,
            }),
            is_tunable: false,
            ..Default::default()
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("a"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("a")
        },
    );
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("x")
        },
    );
    let slice = rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new("a").into(),
        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
            rumoca_core::Expression::Range {
                start: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: rumoca_core::Span::DUMMY,
                }),
                step: None,
                end: Box::new(var("n")),
                span: rumoca_core::Span::DUMMY,
            },
        ))],
        span: rumoca_core::Span::DUMMY,
    };
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("der(x[1])").into()),
        rhs: mul(slice, var("x")),
        span: Default::default(),
        // MLS §10.6.5: vector * vector is a scalar product even when one
        // operand is a structural slice and the other is a state vector.
        origin: "state vector scalar product residual".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout)
        .expect("MLS §10.6.5 vector scalar product residual should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "a[1]", 10.0);
    set_p_value(&layout, &mut p, "a[2]", 15.0);
    set_p_value(&layout, &mut p, "a[3]", 20.0);

    let (_, output) = eval_linear_ops(&rows[0], &[2.0, 3.0], &p, 0.0);

    assert_eq!(output, Some(-90.0));
}

fn record_array_member_slice_expr(base: &str, field: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::from_component_reference(
                    test_component_ref_from_name(base),
                ),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![rumoca_core::Subscript::Colon {
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        }),
        field: field.to_string(),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn record_array_member_slice_loads_dense_elements() {
    let mut dae_model = dae::Dae::default();
    for name in ["pin[1].v", "pin[2].v"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let expr = builtin(
        rumoca_core::BuiltinFunction::Sum,
        vec![record_array_member_slice_expr("pin", "v")],
    );

    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("dense member slice should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[10.0, 20.0], &[], 0.0);
    assert_eq!(read_reg(&regs, lowered.result), 30.0);
}

#[test]
fn record_array_member_slice_with_numbering_gap_is_a_contract_violation() {
    // The slice probe loads `pin[1].v`, `pin[2].v`, ... until the first
    // missing element, so a gap in the scalarized numbering must fail
    // loudly instead of silently truncating the slice at the gap.
    let mut dae_model = dae::Dae::default();
    for name in ["pin[1].v", "pin[3].v"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let expr = builtin(
        rumoca_core::BuiltinFunction::Sum,
        vec![record_array_member_slice_expr("pin", "v")],
    );

    lower_expression(&expr, &layout, &IndexMap::new())
        .expect_err("a gapped member slice must not lower");

    // The probe-path backstop itself: a probe that stopped after element 1
    // while element 3 exists in the layout is a contract violation.
    let functions = IndexMap::new();
    let builder = crate::lower::LowerBuilder::new(&layout, &functions);
    let err = builder
        .ensure_dense_record_array_slice("pin", "v", 1, rumoca_core::Span::DUMMY)
        .expect_err("a gap behind the probe stopping point must be rejected");
    assert!(
        err.to_string().contains("not densely scalarized"),
        "unexpected error: {err}"
    );
    builder
        .ensure_dense_record_array_slice("pin", "v", 3, rumoca_core::Span::DUMMY)
        .expect("a probe that covered every element is dense");
}
