use super::*;

#[test]
fn refresh_env_solver_and_parameter_values_updates_runtime_slots_only() {
    let mut dae = dae::Dae::default();
    dae.variables.states.insert(
        VarName::new("x"),
        dae::Variable::new(
            VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.outputs.insert(
        VarName::new("y"),
        dae::Variable::new(
            VarName::new("y"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.parameters.insert(
        VarName::new("p"),
        dae::Variable::new(
            VarName::new("p"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.discrete_reals.insert(
        VarName::new("d"),
        dae::Variable::new(
            VarName::new("d"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let mut env = build_env(&dae, &[1.0, 2.0], &[3.0], 0.25).expect("test env should build");
    env.set("d", 9.0);

    refresh_env_solver_and_parameter_values(&mut env, &dae, &[4.0, 5.0], &[6.0], 0.75)
        .expect("refresh should succeed");

    assert_eq!(env_value(&env, "time"), 0.75);
    assert_eq!(env_value(&env, "x"), 4.0);
    assert_eq!(env_value(&env, "y"), 5.0);
    assert_eq!(env_value(&env, "p"), 6.0);
    assert_eq!(env_value(&env, "d"), 9.0);
}

#[test]
fn build_env_rejects_short_solver_vector() {
    let mut dae = dae::Dae::default();
    dae.variables.states.insert(
        VarName::new("x"),
        dae::Variable::new(
            VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.outputs.insert(
        VarName::new("y"),
        dae::Variable::new(
            VarName::new("y"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    assert_eq!(
        build_env(&dae, &[1.0], &[], 0.0).expect_err("short y should fail"),
        EvalError::ShortRuntimeVector {
            vector: "y",
            expected: 2,
            actual: 1,
        }
    );
}

#[test]
fn build_env_rejects_short_parameter_vector() {
    let mut dae = dae::Dae::default();
    dae.variables.parameters.insert(
        VarName::new("p"),
        dae::Variable::new(
            VarName::new("p"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    assert_eq!(
        build_env(&dae, &[], &[], 0.0).expect_err("short p should fail"),
        EvalError::ShortRuntimeVector {
            vector: "p",
            expected: 1,
            actual: 0,
        }
    );
}

#[test]
fn refresh_env_solver_and_parameter_values_rejects_short_vectors_before_mutation() {
    let mut dae = dae::Dae::default();
    dae.variables.states.insert(
        VarName::new("x"),
        dae::Variable::new(
            VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.parameters.insert(
        VarName::new("p"),
        dae::Variable::new(
            VarName::new("p"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let mut env = build_env(&dae, &[1.0], &[2.0], 0.25).expect("initial env");
    let err = refresh_env_solver_and_parameter_values(&mut env, &dae, &[], &[3.0], 0.5)
        .expect_err("short y should fail");

    assert_eq!(
        err,
        EvalError::ShortRuntimeVector {
            vector: "y",
            expected: 1,
            actual: 0,
        }
    );
    assert_eq!(env_value(&env, "time"), 0.25);
    assert_eq!(env_value(&env, "x"), 1.0);
    assert_eq!(env_value(&env, "p"), 2.0);
}

#[test]
fn build_runtime_parameter_tail_env_populates_inputs_and_discretes_without_solver_slots() {
    let mut dae = dae::Dae::default();
    dae.variables.states.insert(
        VarName::new("x"),
        dae::Variable::new(
            VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let mut p = dae::Variable::new(
        VarName::new("p"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    p.start = Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(0.0),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.parameters.insert(VarName::new("p"), p);

    let mut u = dae::Variable::new(
        VarName::new("u"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    u.start = Some(rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(rumoca_core::Expression::VarRef {
            name: Reference::new("p"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.inputs.insert(VarName::new("u"), u);

    let mut d = dae::Variable::new(
        VarName::new("d"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    d.start = Some(rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(rumoca_core::Expression::VarRef {
            name: Reference::new("u"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(2.0),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.discrete_reals.insert(VarName::new("d"), d);

    let env = build_runtime_parameter_tail_env(&dae, &[3.0], 0.25).expect("test env should build");

    assert_eq!(env_value(&env, "time"), 0.25);
    assert_eq!(env_value(&env, "p"), 3.0);
    assert_eq!(env_value(&env, "u"), 4.0);
    assert_eq!(env_value(&env, "d"), 6.0);
    assert!(!env.vars.contains_key("x"));
}

#[test]
fn build_runtime_parameter_tail_env_skips_string_parameter_alias_chain() {
    let mut dae = dae::Dae::default();

    let mut shape_type = dae::Variable::new(
        VarName::new("fixedTranslation.shapeType"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    shape_type.start = Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String("cylinder".to_string()),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("fixedTranslation.shapeType"), shape_type);

    let mut shape_shape_type = dae::Variable::new(
        VarName::new("fixedTranslation.shape.shapeType"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    shape_shape_type.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("fixedTranslation.shapeType"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.parameters.insert(
        VarName::new("fixedTranslation.shape.shapeType"),
        shape_shape_type,
    );

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("string parameter aliases should stay out of numeric env");
    assert!(!env.vars.contains_key("fixedTranslation.shapeType"));
    assert!(!env.vars.contains_key("fixedTranslation.shape.shapeType"));
}

#[test]
fn build_runtime_parameter_tail_env_skips_get_instance_name_string_alias_chain() {
    let mut dae = dae::Dae::default();

    let mut building_name = dae::Variable::new(
        VarName::new("building.modelicaNameBuilding"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    building_name.start = Some(rumoca_core::Expression::FunctionCall {
        name: Reference::new("getInstanceName"),
        args: vec![],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .constants
        .insert(VarName::new("building.modelicaNameBuilding"), building_name);

    let mut local_name = dae::Variable::new(
        VarName::new("zone.modelicaNameBuilding"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    local_name.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("building.modelicaNameBuilding"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .constants
        .insert(VarName::new("zone.modelicaNameBuilding"), local_name);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("getInstanceName string aliases should stay out of numeric env");

    assert!(!env.vars.contains_key("building.modelicaNameBuilding"));
    assert!(!env.vars.contains_key("zone.modelicaNameBuilding"));
}

#[test]
fn build_runtime_parameter_tail_env_skips_string_field_access_alias_chain() {
    let mut dae = dae::Dae::default();
    dae.metadata
        .nonnumeric_variable_names
        .push("building.modelicaNameBuilding".to_string());

    let mut building_name = dae::Variable::new(
        VarName::new("building.modelicaNameBuilding"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    building_name.start = Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String("Root.building".to_string()),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .constants
        .insert(VarName::new("building.modelicaNameBuilding"), building_name);

    let mut zone_name = dae::Variable::new(
        VarName::new("zone.modelicaNameBuilding"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    zone_name.start = Some(rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::VarRef {
                name: Reference::new("zone"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            field: "building".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "modelicaNameBuilding".to_string(),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .constants
        .insert(VarName::new("zone.modelicaNameBuilding"), zone_name);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("string field-access aliases should stay out of numeric env");

    assert!(!env.vars.contains_key("building.modelicaNameBuilding"));
    assert!(!env.vars.contains_key("zone.modelicaNameBuilding"));
}

#[test]
fn declared_slot_runtime_tail_env_advances_over_string_parameter_slots() {
    let mut dae = dae::Dae::default();

    let mut shape_type = dae::Variable::new(
        VarName::new("body.shapeType"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    shape_type.start = Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String("cylinder".to_string()),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("body.shapeType"), shape_type);

    let mut q_start = dae::Variable::new(
        VarName::new("body.Q_start"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    q_start.dims = vec![4];
    q_start.start = Some(arr(vec![lit(0.0), lit(0.0), lit(0.0), lit(1.0)], false));
    dae.variables
        .parameters
        .insert(VarName::new("body.Q_start"), q_start);

    let env = build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        &dae,
        &[0.0, 0.0, 0.0, 0.0, 1.0],
        0.0,
        std::sync::Arc::new(EvalRuntimeState::new()),
    )
    .expect("declared-slot parameter env should consume string slots");

    assert!(!env.vars.contains_key("body.shapeType"));
    assert_eq!(
        eval_shaped_array_values::<f64>(&var("body.Q_start"), &env, 4),
        Ok(vec![0.0, 0.0, 0.0, 1.0])
    );
}

#[test]
fn build_runtime_parameter_tail_env_rejects_short_parameter_vector() {
    let mut dae = dae::Dae::default();
    dae.variables.parameters.insert(
        VarName::new("p"),
        dae::Variable::new(
            VarName::new("p"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    assert_eq!(
        build_runtime_parameter_tail_env(&dae, &[], 0.0).expect_err("short p should fail"),
        EvalError::ShortRuntimeVector {
            vector: "p",
            expected: 1,
            actual: 0,
        }
    );
}

#[test]
fn build_runtime_parameter_tail_env_rejects_bad_array_start_shape() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("eval_dae_env_refresh_source_17.mo"),
        5,
        13,
    );
    let mut dae = dae::Dae::default();
    let mut constant = dae::Variable::new(
        VarName::new("c"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    constant.source_span = span;
    constant.dims = vec![3];
    constant.start = Some(arr(vec![lit(1.0), lit(2.0)], false));
    dae.variables.constants.insert(VarName::new("c"), constant);

    let err = build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect_err("array start shape mismatch should fail env construction");
    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err,
        EvalError::ShapeMismatch {
            context: "shaped array value",
            expected: 3,
            actual: 2,
        }
        .with_span_if_missing(span)
    );
}

#[test]
fn build_runtime_parameter_tail_env_binds_vector_builtin_array_start() {
    let mut dae = dae::Dae::default();
    let mut discrete = dae::Variable::new(
        VarName::new("buf"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    discrete.dims = vec![3];
    discrete.start = Some(builtin(
        BuiltinFunction::Vector,
        vec![arr(
            vec![
                arr(vec![lit(6.5)], true),
                arr(
                    vec![builtin(BuiltinFunction::Fill, vec![lit(0.0), int_lit(2)])],
                    true,
                ),
            ],
            true,
        )],
    ));
    dae.variables
        .discrete_reals
        .insert(VarName::new("buf"), discrete);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("vector(array) start should populate discrete array values");

    assert_eq!(env_value(&env, "buf[1]"), 6.5);
    assert_eq!(env_value(&env, "buf[2]"), 0.0);
    assert_eq!(env_value(&env, "buf[3]"), 0.0);
}

#[test]
fn build_runtime_parameter_tail_env_uses_default_discrete_start_dependencies() {
    let mut dae = dae::Dae::default();
    dae.variables.discrete_valued.insert(
        VarName::new("condition"),
        dae::Variable::new(
            VarName::new("condition"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let mut local_condition = dae::Variable::new(
        VarName::new("localCondition"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    local_condition.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("condition"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .discrete_valued
        .insert(VarName::new("localCondition"), local_condition);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("discrete defaults should be available to dependent starts");

    assert_eq!(env_value(&env, "condition"), 0.0);
    assert_eq!(env_value(&env, "localCondition"), 0.0);
}

#[test]
fn build_runtime_parameter_tail_env_binds_constants_before_parameter_starts() {
    let mut dae = dae::Dae::default();

    let mut table = dae::Variable::new(
        VarName::new("clock.conversionTable"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    table.dims = vec![8];
    table.start = Some(arr(
        vec![
            lit(31_536_000.0),
            lit(86_400.0),
            lit(3_600.0),
            lit(60.0),
            lit(1.0),
            lit(1_000.0),
            lit(1_000_000.0),
            lit(1_000_000_000.0),
        ],
        false,
    ));
    dae.variables
        .constants
        .insert(VarName::new("clock.conversionTable"), table);

    let mut resolution = dae::Variable::new(
        VarName::new("clock.resolution"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    resolution.start = Some(lit(6.0));
    dae.variables
        .parameters
        .insert(VarName::new("clock.resolution"), resolution);

    let mut factor = dae::Variable::new(
        VarName::new("clock.resolutionFactor"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    factor.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("clock.conversionTable"),
        subscripts: vec![rumoca_core::Subscript::Expr {
            expr: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Integer,
                args: vec![rumoca_core::Expression::VarRef {
                    name: Reference::new("clock.resolution"),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("clock.resolutionFactor"), factor);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0).expect("test env should build");

    assert_eq!(env_value(&env, "clock.conversionTable[6]"), 1_000.0);
    assert_eq!(env_value(&env, "clock.resolutionFactor"), 1_000.0);
}

#[test]
fn build_runtime_parameter_tail_env_resolves_forward_parameter_array_dependency() {
    let mut dae = dae::Dae::default();

    let mut selected = dae::Variable::new(
        VarName::new("body.I_11"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    selected.start = Some(indexed_var("body.I", &[1, 1]));
    dae.variables
        .parameters
        .insert(VarName::new("body.I_11"), selected);

    let mut inertia = dae::Variable::new(
        VarName::new("body.I"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    inertia.dims = vec![2, 2];
    inertia.start = Some(arr(
        vec![
            arr(vec![lit(1.0), lit(2.0)], false),
            arr(vec![lit(3.0), lit(4.0)], false),
        ],
        true,
    ));
    dae.variables
        .parameters
        .insert(VarName::new("body.I"), inertia);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0).expect("test env should build");

    assert_eq!(env_value(&env, "body.I[1,1]"), 1.0);
    assert_eq!(env_value(&env, "body.I_11"), 1.0);
}

#[test]
fn build_runtime_parameter_tail_env_prefers_pre_store_for_lowered_pre_parameters() {
    clear_pre_values();

    let mut dae = dae::Dae::default();
    let mut pre = dae::Variable::new(
        VarName::new("__pre__.reset"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    pre.start = Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(0.0),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("__pre__.reset"), pre);

    let runtime = std::sync::Arc::new(EvalRuntimeState::new());
    let seed_env = VarEnv::<f64> {
        runtime: runtime.clone(),
        ..VarEnv::new()
    };
    set_pre_value_in_env(&seed_env, "reset", 1.0);
    let env = build_runtime_parameter_tail_env_with_runtime(&dae, &[0.0], 0.0, runtime)
        .expect("test env should build");

    assert_eq!(env_value(&env, "__pre__.reset"), 1.0);

    clear_pre_values();
}

#[test]
fn build_runtime_parameter_tail_env_keeps_pre_store_runtime_local() {
    clear_pre_values();

    let mut dae = dae::Dae::default();
    dae.variables.parameters.insert(
        VarName::new("__pre__.reset"),
        dae::Variable::new(
            VarName::new("__pre__.reset"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let runtime_a = std::sync::Arc::new(EvalRuntimeState::new());
    let runtime_b = std::sync::Arc::new(EvalRuntimeState::new());
    let env_a = VarEnv::<f64> {
        runtime: runtime_a.clone(),
        ..VarEnv::new()
    };
    let env_b = VarEnv::<f64> {
        runtime: runtime_b.clone(),
        ..VarEnv::new()
    };
    set_pre_value_in_env(&env_a, "reset", 1.0);
    set_pre_value_in_env(&env_b, "reset", 2.0);

    let built_a = build_runtime_parameter_tail_env_with_runtime(&dae, &[0.0], 0.0, runtime_a)
        .expect("test env should build");
    let built_b = build_runtime_parameter_tail_env_with_runtime(&dae, &[0.0], 0.0, runtime_b)
        .expect("test env should build");

    assert_eq!(env_value(&built_a, "__pre__.reset"), 1.0);
    assert_eq!(env_value(&built_b, "__pre__.reset"), 2.0);

    clear_pre_values();
}

#[test]
fn request_local_pre_store_snapshot_restore_does_not_touch_global_runtime() {
    clear_pre_values();

    let runtime_a = std::sync::Arc::new(EvalRuntimeState::new());
    let runtime_b = std::sync::Arc::new(EvalRuntimeState::new());
    let env_a = VarEnv::<f64> {
        runtime: runtime_a,
        ..VarEnv::new()
    };
    let env_b = VarEnv::<f64> {
        runtime: runtime_b,
        ..VarEnv::new()
    };

    set_pre_value_in_env(&env_a, "x", 1.0);
    set_pre_value_in_env(&env_b, "x", 2.0);

    let snapshot_a = snapshot_pre_values_from_env(&env_a);
    restore_pre_values_in_env_runtime(&env_b, snapshot_a);

    assert_eq!(get_pre_value_from_env(&env_a, "x"), Some(1.0));
    assert_eq!(get_pre_value_from_env(&env_b, "x"), Some(1.0));
    assert_eq!(
        get_pre_value("x"),
        None,
        "request-local restore must not write the default compatibility runtime"
    );

    clear_runtime_state_in_env_runtime(&env_a);
    assert_eq!(get_pre_value_from_env(&env_a, "x"), None);
    assert_eq!(get_pre_value_from_env(&env_b, "x"), Some(1.0));

    clear_pre_values();
}

#[test]
fn refresh_env_solver_and_parameter_values_refreshes_lowered_pre_parameters_from_pre_store() {
    clear_pre_values();

    let mut dae = dae::Dae::default();
    dae.variables.states.insert(
        VarName::new("x"),
        dae::Variable::new(
            VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.parameters.insert(
        VarName::new("__pre__.reset"),
        dae::Variable::new(
            VarName::new("__pre__.reset"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let mut env = build_env(&dae, &[0.0], &[0.0], 0.0).expect("test env should build");
    set_pre_value_in_env(&env, "reset", 2.0);
    refresh_env_solver_and_parameter_values(&mut env, &dae, &[1.0], &[0.0], 0.5)
        .expect("refresh should succeed");

    assert_eq!(env_value(&env, "x"), 1.0);
    assert_eq!(env_value(&env, "__pre__.reset"), 2.0);

    clear_pre_values();
}

#[test]
fn build_runtime_parameter_tail_env_skips_zero_sized_parameter_slots() {
    let mut dae = dae::Dae::default();

    let mut dyn_arr = dae::Variable::new(
        VarName::new("dyn_arr"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    dyn_arr.dims = vec![0];
    dae.variables
        .parameters
        .insert(VarName::new("dyn_arr"), dyn_arr);

    dae.variables.parameters.insert(
        VarName::new("table_id"),
        dae::Variable::new(
            VarName::new("table_id"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let env = build_runtime_parameter_tail_env(&dae, &[4.0], 0.0).expect("test env should build");

    assert_eq!(env_value(&env, "table_id"), 4.0);
    assert!(!env.vars.contains_key("dyn_arr"));
}

#[test]
fn build_runtime_parameter_tail_env_binds_enum_parameters_without_numeric_slots() {
    let mut dae = dae::Dae::default();
    dae.symbols.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'X'".to_string(),
        2,
    );
    dae.symbols.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
        3,
    );

    let mut before = dae::Variable::new(
        VarName::new("before"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    before.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Modelica.Electrical.Digital.Interfaces.Logic.'X'"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("before"), before);

    let mut after = dae::Variable::new(
        VarName::new("after"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    after.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("after"), after);

    let mut u = dae::Variable::new(
        VarName::new("u"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    u.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("before"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.inputs.insert(VarName::new("u"), u);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0).expect("test env should build");

    assert_eq!(env_value(&env, "before"), 2.0);
    assert_eq!(env_value(&env, "after"), 3.0);
    assert_eq!(env_value(&env, "u"), 2.0);
}

#[test]
fn build_runtime_parameter_tail_env_binds_qualified_enum_parameters_without_numeric_slots() {
    let mut dae = dae::Dae::default();
    dae.symbols.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
        3,
    );
    dae.symbols.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
        4,
    );

    let mut before = dae::Variable::new(
        VarName::new("Enable.before"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    before.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("Enable.before"), before);

    let mut after = dae::Variable::new(
        VarName::new("Enable.after"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    after.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Modelica.Electrical.Digital.Interfaces.Logic.'1'"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("Enable.after"), after);

    let mut step_time = dae::Variable::new(
        VarName::new("Enable.stepTime"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    step_time.start = Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(1.0),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("Enable.stepTime"), step_time);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0).expect("test env should build");

    assert_eq!(env_value(&env, "Enable.before"), 3.0);
    assert_eq!(env_value(&env, "Enable.after"), 4.0);
    assert_eq!(env_value(&env, "Enable.stepTime"), 1.0);
}

#[test]
fn build_runtime_parameter_tail_env_binds_counter_like_enum_parameter_chain() {
    let mut dae = dae::Dae::default();
    dae.symbols.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
        3,
    );

    let mut q0 = dae::Variable::new(
        VarName::new("Counter.q0"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    q0.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("Counter.q0"), q0);

    let mut ff_q0 = dae::Variable::new(
        VarName::new("Counter.FF[1].q0"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    ff_q0.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Counter.q0"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("Counter.FF[1].q0"), ff_q0);

    let mut td_y0 = dae::Variable::new(
        VarName::new("Counter.FF[1].RS1.TD1.y0"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    td_y0.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Counter.q0"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("Counter.FF[1].RS1.TD1.y0"), td_y0);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0).expect("test env should build");

    assert_eq!(env_value(&env, "Counter.q0"), 3.0);
    assert_eq!(env_value(&env, "Counter.FF[1].q0"), 3.0);
    assert_eq!(env_value(&env, "Counter.FF[1].RS1.TD1.y0"), 3.0);
}

#[test]
fn build_runtime_parameter_tail_env_broadcasts_enum_literal_to_discrete_array_start() {
    let mut dae = dae::Dae::default();
    dae.symbols.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
        1,
    );

    let mut auxiliary = dae::Variable::new(
        VarName::new("auxiliary"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    auxiliary.dims = vec![3];
    auxiliary.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Modelica.Electrical.Digital.Interfaces.Logic.'U'"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .discrete_valued
        .insert(VarName::new("auxiliary"), auxiliary);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0).expect("test env should build");

    assert_eq!(env_value(&env, "auxiliary[1]"), 1.0);
    assert_eq!(env_value(&env, "auxiliary[2]"), 1.0);
    assert_eq!(env_value(&env, "auxiliary[3]"), 1.0);
}

#[test]
fn build_runtime_parameter_tail_env_binds_singleton_parameter_array_index_entries() {
    let mut dae = dae::Dae::default();

    let mut t_param = dae::Variable::new(
        VarName::new("a.t"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    t_param.dims = vec![1];
    t_param.start = Some(rumoca_core::Expression::Array {
        elements: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("a.t"), t_param);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0).expect("test env should build");

    assert_eq!(env_value(&env, "a.t"), 1.0);
    assert_eq!(env_value(&env, "a.t[1]"), 1.0);
}

#[test]
fn build_env_skips_zero_sized_solver_slots() {
    let mut dae = dae::Dae::default();

    let mut dyn_out = dae::Variable::new(
        VarName::new("dyn_out"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    dyn_out.dims = vec![0];
    dae.variables
        .outputs
        .insert(VarName::new("dyn_out"), dyn_out);
    dae.variables.outputs.insert(
        VarName::new("y"),
        dae::Variable::new(
            VarName::new("y"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let env = build_env(&dae, &[7.0], &[], 0.0).expect("test env should build");

    assert_eq!(env_value(&env, "y"), 7.0);
    assert!(!env.vars.contains_key("dyn_out"));
}
