use super::*;

#[test]
fn refresh_env_solver_and_parameter_values_updates_runtime_slots_only() {
    let mut dae = dae::Dae::default();
    dae.variables
        .states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.variables
        .outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
    dae.variables
        .parameters
        .insert(VarName::new("p"), dae::Variable::new(VarName::new("p")));
    dae.variables
        .discrete_reals
        .insert(VarName::new("d"), dae::Variable::new(VarName::new("d")));

    let mut env = build_env(&dae, &[1.0, 2.0], &[3.0], 0.25);
    env.set("d", 9.0);

    refresh_env_solver_and_parameter_values(&mut env, &dae, &[4.0, 5.0], &[6.0], 0.75);

    assert_eq!(env.get("time"), 0.75);
    assert_eq!(env.get("x"), 4.0);
    assert_eq!(env.get("y"), 5.0);
    assert_eq!(env.get("p"), 6.0);
    assert_eq!(env.get("d"), 9.0);
}

#[test]
fn try_build_env_rejects_short_solver_vector() {
    let mut dae = dae::Dae::default();
    dae.variables
        .states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.variables
        .outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    assert_eq!(
        try_build_env(&dae, &[1.0], &[], 0.0).expect_err("short y should fail"),
        EvalError::ShortRuntimeVector {
            vector: "y",
            expected: 2,
            actual: 1,
        }
    );
}

#[test]
fn try_build_env_rejects_short_parameter_vector() {
    let mut dae = dae::Dae::default();
    dae.variables
        .parameters
        .insert(VarName::new("p"), dae::Variable::new(VarName::new("p")));

    assert_eq!(
        try_build_env(&dae, &[], &[], 0.0).expect_err("short p should fail"),
        EvalError::ShortRuntimeVector {
            vector: "p",
            expected: 1,
            actual: 0,
        }
    );
}

#[test]
fn try_refresh_env_solver_and_parameter_values_rejects_short_vectors_before_mutation() {
    let mut dae = dae::Dae::default();
    dae.variables
        .states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.variables
        .parameters
        .insert(VarName::new("p"), dae::Variable::new(VarName::new("p")));

    let mut env = try_build_env(&dae, &[1.0], &[2.0], 0.25).expect("initial env");
    let err = try_refresh_env_solver_and_parameter_values(&mut env, &dae, &[], &[3.0], 0.5)
        .expect_err("short y should fail");

    assert_eq!(
        err,
        EvalError::ShortRuntimeVector {
            vector: "y",
            expected: 1,
            actual: 0,
        }
    );
    assert_eq!(env.get("time"), 0.25);
    assert_eq!(env.get("x"), 1.0);
    assert_eq!(env.get("p"), 2.0);
}

#[test]
fn build_runtime_parameter_tail_env_populates_inputs_and_discretes_without_solver_slots() {
    let mut dae = dae::Dae::default();
    dae.variables
        .states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));

    let mut p = dae::Variable::new(VarName::new("p"));
    p.start = Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(0.0),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.parameters.insert(VarName::new("p"), p);

    let mut u = dae::Variable::new(VarName::new("u"));
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

    let mut d = dae::Variable::new(VarName::new("d"));
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

    let env = build_runtime_parameter_tail_env(&dae, &[3.0], 0.25);

    assert_eq!(env.get("time"), 0.25);
    assert_eq!(env.get("p"), 3.0);
    assert_eq!(env.get("u"), 4.0);
    assert_eq!(env.get("d"), 6.0);
    assert!(!env.vars.contains_key("x"));
}

#[test]
fn build_runtime_parameter_tail_env_skips_string_parameter_alias_chain() {
    let mut dae = dae::Dae::default();

    let mut shape_type = dae::Variable::new(VarName::new("fixedTranslation.shapeType"));
    shape_type.start = Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String("cylinder".to_string()),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("fixedTranslation.shapeType"), shape_type);

    let mut shape_shape_type = dae::Variable::new(VarName::new("fixedTranslation.shape.shapeType"));
    shape_shape_type.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("fixedTranslation.shapeType"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.parameters.insert(
        VarName::new("fixedTranslation.shape.shapeType"),
        shape_shape_type,
    );

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("string parameter aliases should stay out of numeric env");
    assert!(!env.vars.contains_key("fixedTranslation.shapeType"));
    assert!(!env.vars.contains_key("fixedTranslation.shape.shapeType"));
}

#[test]
fn build_runtime_parameter_tail_env_skips_string_record_field_alias_chain() {
    let mut dae = dae::Dae::default();

    let mut source = dae::Variable::new(VarName::new("zone.building.modelicaNameBuilding"));
    source.start = Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String("building".to_string()),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("zone.building.modelicaNameBuilding"), source);

    let mut alias = dae::Variable::new(VarName::new("adapter.modelicaNameBuilding"));
    alias.start = Some(rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(var("zone")),
            field: "building".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "modelicaNameBuilding".to_string(),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("adapter.modelicaNameBuilding"), alias);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("string record field aliases should stay out of numeric env");
    assert!(!env.vars.contains_key("zone.building.modelicaNameBuilding"));
    assert!(!env.vars.contains_key("adapter.modelicaNameBuilding"));
}

#[test]
fn build_runtime_parameter_tail_env_skips_indexed_string_array_alias_with_colon() {
    let mut dae = dae::Dae::default();

    let mut floor_index = dae::Variable::new(VarName::new("floor.floorIndex"));
    floor_index.start = Some(lit(1.0));
    dae.variables
        .parameters
        .insert(VarName::new("floor.floorIndex"), floor_index);

    let mut zone_name = dae::Variable::new(VarName::new("floor.zoneName"));
    zone_name.dims = vec![3, 5];
    zone_name.start = Some(rumoca_core::Expression::Array {
        elements: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("core".to_string()),
            span: rumoca_core::Span::DUMMY,
        }],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("floor.zoneName"), zone_name);

    let mut alias = dae::Variable::new(VarName::new("floor.vav.zoneName"));
    alias.dims = vec![5];
    alias.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("floor.zoneName"),
        subscripts: vec![
            Subscript::Expr {
                expr: Box::new(var("floor.floorIndex")),
                span: rumoca_core::Span::DUMMY,
            },
            Subscript::Colon {
                span: rumoca_core::Span::DUMMY,
            },
        ],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("floor.vav.zoneName"), alias);

    let env = try_build_runtime_parameter_tail_env(&dae, &[1.0], 0.0)
        .expect("indexed string array aliases should stay out of numeric env");
    assert_eq!(
        env.vars.get("floor.floorIndex").map(|value| value.real()),
        Some(1.0)
    );
    assert!(!env.vars.contains_key("floor.zoneName"));
    assert!(!env.vars.contains_key("floor.vav.zoneName"));
}

#[test]
fn build_runtime_parameter_tail_env_preserves_zero_dim_array_alias_chain() {
    let mut dae = dae::Dae::default();

    let mut source = dae::Variable::new(VarName::new("junSup1.C_start"));
    source.dims = vec![0];
    source.start = Some(builtin(
        BuiltinFunction::Fill,
        vec![
            lit(0.0),
            builtin(
                BuiltinFunction::Size,
                vec![builtin(
                    BuiltinFunction::Fill,
                    vec![
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::String(String::new()),
                            span: rumoca_core::Span::DUMMY,
                        },
                        int_lit(0),
                    ],
                )],
            ),
        ],
    ));
    dae.variables
        .parameters
        .insert(VarName::new("junSup1.C_start"), source);

    let mut alias = dae::Variable::new(VarName::new("junSup1.vol.C_start"));
    alias.dims = vec![0];
    alias.start = Some(var("junSup1.C_start"));
    dae.variables
        .parameters
        .insert(VarName::new("junSup1.vol.C_start"), alias);

    let mut nested_alias = dae::Variable::new(VarName::new("junSup1.vol.dynBal.C_start"));
    nested_alias.dims = vec![0];
    nested_alias.start = Some(var("junSup1.C_start"));
    dae.variables
        .parameters
        .insert(VarName::new("junSup1.vol.dynBal.C_start"), nested_alias);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("zero-dimension array aliases should resolve as empty arrays");

    assert_eq!(
        eval_array_values::<f64>(&var("junSup1.C_start"), &env),
        Vec::<f64>::new()
    );
    assert_eq!(
        eval_shaped_array_values::<f64>(&var("junSup1.vol.C_start"), &env, 0),
        Ok(Vec::new())
    );
    assert_eq!(
        eval_shaped_array_values::<f64>(&var("junSup1.vol.dynBal.C_start"), &env, 0),
        Ok(Vec::new())
    );
}

#[test]
fn build_runtime_parameter_tail_env_uses_zero_extent_fill_dims_for_table() {
    let mut dae = dae::Dae::default();

    let mut table = dae::Variable::new(VarName::new("building.weaDat.datRea.table"));
    table.dims = vec![1, 2];
    table.start = Some(builtin(
        BuiltinFunction::Fill,
        vec![lit(0.0), int_lit(0), int_lit(2)],
    ));
    dae.variables
        .parameters
        .insert(VarName::new("building.weaDat.datRea.table"), table);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("zero-row tables should keep runtime fill dimensions");

    assert_eq!(
        env.dims.get("building.weaDat.datRea.table"),
        Some(&vec![0, 2])
    );
    assert_eq!(
        eval_shaped_array_values::<f64>(&var("building.weaDat.datRea.table"), &env, 0),
        Ok(Vec::new())
    );
}

#[test]
fn build_runtime_parameter_tail_env_binds_nested_start_from_enclosing_scope_start() {
    let mut dae = dae::Dae::default();

    let mut log_level = dae::Variable::new(VarName::new("floor.zon[1].logLevel"));
    log_level.start = Some(var("floor.zon[1].fmuZon.AFlo"));
    dae.variables
        .constants
        .insert(VarName::new("floor.zon[1].logLevel"), log_level);

    let mut area = dae::Variable::new(VarName::new("floor.zon[1].AFlo"));
    area.start = Some(lit(48.0));
    dae.variables
        .constants
        .insert(VarName::new("floor.zon[1].AFlo"), area);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("nested component start should resolve from enclosing scope start");

    assert_eq!(
        env.vars.get("floor.zon[1].AFlo").map(|value| value.real()),
        Some(48.0)
    );
    assert_eq!(
        env.vars
            .get("floor.zon[1].fmuZon.AFlo")
            .map(|value| value.real()),
        Some(48.0)
    );
    assert_eq!(
        env.vars
            .get("floor.zon[1].logLevel")
            .map(|value| value.real()),
        Some(48.0)
    );
}

#[test]
fn build_runtime_parameter_tail_env_binds_nested_start_alias_for_existing_parameter_slot() {
    let mut dae = dae::Dae::default();

    let mut area = dae::Variable::new(VarName::new("floor.zon[1].AFlo"));
    area.start = Some(lit(48.0));
    dae.variables
        .constants
        .insert(VarName::new("floor.zon[1].AFlo"), area);

    let mut log_level = dae::Variable::new(VarName::new("floor.zon[1].logLevel"));
    log_level.start = Some(var("floor.zon[1].fmuZon.AFlo"));
    dae.variables
        .parameters
        .insert(VarName::new("floor.zon[1].logLevel"), log_level);

    let env = try_build_runtime_parameter_tail_env(&dae, &[7.0], 0.0)
        .expect("existing parameter slots should still seed start-expression aliases");

    assert_eq!(
        env.vars
            .get("floor.zon[1].fmuZon.AFlo")
            .map(|value| value.real()),
        Some(48.0)
    );
    assert_eq!(
        env.vars
            .get("floor.zon[1].logLevel")
            .map(|value| value.real()),
        Some(7.0)
    );
}

#[test]
fn build_runtime_parameter_tail_env_binds_nested_field_access_start_alias_for_existing_parameter_slot()
 {
    let mut dae = dae::Dae::default();

    let mut area = dae::Variable::new(VarName::new("floor.zon[1].AFlo"));
    area.start = Some(lit(48.0));
    dae.variables
        .constants
        .insert(VarName::new("floor.zon[1].AFlo"), area);

    let mut log_level = dae::Variable::new(VarName::new("floor.zon[1].logLevel"));
    log_level.start = Some(rumoca_core::Expression::FieldAccess {
        base: Box::new(var("floor.zon[1].fmuZon")),
        field: "AFlo".to_string(),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("floor.zon[1].logLevel"), log_level);

    let env = try_build_runtime_parameter_tail_env(&dae, &[7.0], 0.0)
        .expect("existing parameter slots should seed structured field-access aliases");

    assert_eq!(
        env.vars
            .get("floor.zon[1].fmuZon.AFlo")
            .map(|value| value.real()),
        Some(48.0)
    );
    assert_eq!(
        env.vars
            .get("floor.zon[1].logLevel")
            .map(|value| value.real()),
        Some(7.0)
    );
}

#[test]
fn partial_declared_runtime_tail_env_skips_start_depending_on_fixed_false_parameter() {
    let mut dae = dae::Dae::default();

    let mut external_area = dae::Variable::new(VarName::new("zone.fmuZon.AFlo"));
    external_area.fixed = Some(false);
    dae.variables
        .parameters
        .insert(VarName::new("zone.fmuZon.AFlo"), external_area);

    let mut area = dae::Variable::new(VarName::new("zone.AFlo"));
    area.start = Some(rumoca_core::Expression::FieldAccess {
        base: Box::new(var("zone.fmuZon")),
        field: "AFlo".to_string(),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("zone.AFlo"), area);

    let env = try_build_partial_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        &dae,
        &[],
        0.0,
        std::sync::Arc::new(EvalRuntimeState::new()),
    )
    .expect("fixed=false parameter dependencies are initialization unknowns");

    assert_eq!(
        env.vars.get("zone.AFlo").map(|value| value.real()),
        Some(0.0)
    );
    assert_eq!(
        env.vars.get("zone.fmuZon.AFlo").map(|value| value.real()),
        Some(0.0)
    );
}

#[test]
fn partial_declared_runtime_tail_env_uses_global_fixed_false_metadata_for_missing_start() {
    let mut dae = dae::Dae::default();

    let mut external_start = dae::Variable::new(VarName::new("zone.fmuZon.startTime"));
    external_start.fixed = Some(false);
    dae.variables
        .algebraics
        .insert(VarName::new("zone.fmuZon.startTime"), external_start);

    let mut event_time = dae::Variable::new(VarName::new("zone.nextEventTime"));
    event_time.start = Some(var("zone.fmuZon.startTime"));
    dae.variables
        .parameters
        .insert(VarName::new("zone.nextEventTime"), event_time);

    let env = try_build_partial_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        &dae,
        &[],
        0.0,
        std::sync::Arc::new(EvalRuntimeState::new()),
    )
    .expect("fixed=false metadata should cover missing initialization dependencies globally");

    assert_eq!(
        env.vars.get("zone.nextEventTime").map(|value| value.real()),
        Some(0.0)
    );
    assert_eq!(
        env.vars
            .get("zone.fmuZon.startTime")
            .map(|value| value.real()),
        Some(0.0)
    );
}

#[test]
fn partial_declared_runtime_tail_env_seeds_indexed_parameter_default_from_base_array() {
    let mut dae = dae::Dae::default();

    let mut time_span = dae::Variable::new(VarName::new("weather.timeSpan"));
    time_span.dims = vec![2];
    dae.variables
        .parameters
        .insert(VarName::new("weather.timeSpan"), time_span);

    let mut start_time = dae::Variable::new(VarName::new("weather.converter.weaDatStaTim"));
    start_time.start = Some(rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("weather.timeSpan"),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            1,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("weather.converter.weaDatStaTim"), start_time);

    let env = try_build_partial_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        &dae,
        &[],
        0.0,
        std::sync::Arc::new(EvalRuntimeState::new()),
    )
    .expect("indexed parameter references should seed the base array default");

    assert_eq!(
        env.vars
            .get("weather.timeSpan[1]")
            .map(|value| value.real()),
        Some(0.0)
    );
    assert_eq!(
        env.vars
            .get("weather.converter.weaDatStaTim")
            .map(|value| value.real()),
        Some(0.0)
    );
}

#[test]
fn declared_slot_runtime_tail_env_advances_over_string_parameter_slots() {
    let mut dae = dae::Dae::default();

    let mut shape_type = dae::Variable::new(VarName::new("body.shapeType"));
    shape_type.start = Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String("cylinder".to_string()),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("body.shapeType"), shape_type);

    let mut q_start = dae::Variable::new(VarName::new("body.Q_start"));
    q_start.dims = vec![4];
    q_start.start = Some(arr(vec![lit(0.0), lit(0.0), lit(0.0), lit(1.0)], false));
    dae.variables
        .parameters
        .insert(VarName::new("body.Q_start"), q_start);

    let env = try_build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
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
fn build_runtime_parameter_tail_env_projects_matrix_modifier_row_for_component_array_member() {
    let mut dae = dae::Dae::default();

    let mut curve = dae::Variable::new(VarName::new("plant.curve"));
    curve.dims = vec![3, 3];
    curve.start = Some(arr(
        vec![
            arr(vec![lit(0.1), lit(0.2), lit(0.3)], false),
            arr(vec![lit(0.4), lit(0.5), lit(0.6)], false),
            arr(vec![lit(0.7), lit(0.8), lit(0.9)], false),
        ],
        false,
    ));
    dae.variables
        .constants
        .insert(VarName::new("plant.curve"), curve);

    let mut member = dae::Variable::new(VarName::new("plant.ct[2].v_flow_rate"));
    member.dims = vec![3];
    member.start = Some(var("plant.curve"));
    dae.variables
        .constants
        .insert(VarName::new("plant.ct[2].v_flow_rate"), member);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("component array member should project its row from matrix modifier");

    assert_eq!(
        eval_shaped_array_values::<f64>(&var("plant.ct[2].v_flow_rate"), &env, 3),
        Ok(vec![0.4, 0.5, 0.6])
    );
}

#[test]
fn build_runtime_parameter_tail_env_projects_indexed_matrix_parameter_row() {
    let mut dae = dae::Dae::default();

    let mut curve = dae::Variable::new(VarName::new("plant.curve"));
    curve.dims = vec![2, 3];
    curve.start = Some(arr(
        vec![
            arr(vec![lit(1.0), lit(2.0), lit(3.0)], false),
            arr(vec![lit(4.0), lit(5.0), lit(6.0)], false),
        ],
        false,
    ));
    dae.variables
        .constants
        .insert(VarName::new("plant.curve"), curve);

    let mut member = dae::Variable::new(VarName::new("plant.pump[1].VolFloCur"));
    member.dims = vec![3];
    member.start = Some(rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("plant.curve"),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            1,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .constants
        .insert(VarName::new("plant.pump[1].VolFloCur"), member);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("indexed matrix parameter should project a row");

    assert_eq!(
        eval_shaped_array_values::<f64>(&var("plant.pump[1].VolFloCur"), &env, 3),
        Ok(vec![1.0, 2.0, 3.0])
    );
}

#[test]
fn build_runtime_parameter_tail_env_projects_repeated_aggregate_literal_for_component_member() {
    let mut dae = dae::Dae::default();

    let mut eta = dae::Variable::new(VarName::new("plant.Motor_eta"));
    eta.dims = vec![3, 1];
    eta.start = Some(arr(
        vec![
            arr(vec![lit(0.7)], false),
            arr(vec![lit(0.8)], false),
            arr(vec![lit(0.9)], false),
        ],
        false,
    ));
    dae.variables
        .constants
        .insert(VarName::new("plant.Motor_eta"), eta);

    let mut member = dae::Variable::new(VarName::new("plant.pump[2].per.motorEfficiency.eta"));
    member.dims = vec![1];
    member.start = Some(arr(
        vec![
            var("plant.Motor_eta"),
            var("plant.Motor_eta"),
            var("plant.Motor_eta"),
        ],
        false,
    ));
    dae.variables.constants.insert(
        VarName::new("plant.pump[2].per.motorEfficiency.eta"),
        member,
    );

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("component member should project repeated aggregate literal");

    assert_eq!(
        eval_expr::<f64>(&var("plant.pump[2].per.motorEfficiency.eta"), &env),
        Ok(0.8)
    );
}

#[test]
fn build_runtime_parameter_tail_env_broadcasts_strict_scalar_start_expression() {
    let mut dae = dae::Dae::default();

    let mut eta = dae::Variable::new(VarName::new("plant.eta"));
    eta.dims = vec![2, 1];
    eta.start = Some(arr(
        vec![arr(vec![lit(0.8)], false), arr(vec![lit(0.9)], false)],
        false,
    ));
    dae.variables
        .constants
        .insert(VarName::new("plant.eta"), eta);

    let mut member = dae::Variable::new(VarName::new("plant.boiler[1].eta"));
    member.dims = vec![2];
    member.start = Some(rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("plant.eta"),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            1,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .constants
        .insert(VarName::new("plant.boiler[1].eta"), member);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("strict scalar start expression should broadcast to target array");

    assert_eq!(
        eval_shaped_array_values::<f64>(&var("plant.boiler[1].eta"), &env, 2),
        Ok(vec![0.8, 0.8])
    );
}

#[test]
fn build_runtime_parameter_tail_env_uses_sibling_n_for_dynamic_record_vector() {
    let mut dae = dae::Dae::default();

    let mut n = dae::Variable::new(VarName::new("pump.eff.pCur1.n"));
    n.start = Some(int_lit(4));
    dae.variables
        .constants
        .insert(VarName::new("pump.eff.pCur1.n"), n);

    let mut curve = dae::Variable::new(VarName::new("pump.eff.per.pressure.V_flow"));
    curve.dims = vec![4];
    curve.start = Some(arr(vec![lit(0.0), lit(0.1), lit(0.2), lit(0.3)], false));
    dae.variables
        .constants
        .insert(VarName::new("pump.eff.per.pressure.V_flow"), curve);

    let mut v_flow = dae::Variable::new(VarName::new("pump.eff.pCur1.V_flow"));
    v_flow.dims = vec![2];
    v_flow.start = Some(rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pump.eff.per.pressure.V_flow"),
            subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(var("i")))],
            span: rumoca_core::Span::DUMMY,
        }),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int_lit(1)),
                step: None,
                end: Box::new(var("pump.eff.pCur1.n")),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .constants
        .insert(VarName::new("pump.eff.pCur1.V_flow"), v_flow);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("record vector should use sibling n as runtime dimension");

    assert_eq!(
        eval_shaped_array_values::<f64>(&var("pump.eff.pCur1.V_flow"), &env, 4),
        Ok(vec![0.0, 0.1, 0.2, 0.3])
    );
    assert_eq!(
        eval_expr::<f64>(
            &builtin(BuiltinFunction::Size, vec![var("pump.eff.pCur1.V_flow")]),
            &env
        ),
        Ok(4.0)
    );
}

#[test]
fn build_runtime_parameter_tail_env_accepts_singleton_range_scalar_start() {
    let mut dae = dae::Dae::default();

    let mut scalar = dae::Variable::new(VarName::new("medium.state_default.X"));
    scalar.start = Some(rumoca_core::Expression::Range {
        start: Box::new(lit(0.42)),
        step: None,
        end: Box::new(lit(0.42)),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .constants
        .insert(VarName::new("medium.state_default.X"), scalar);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("singleton array-like range starts may initialize scalar slots");

    assert_eq!(
        env.vars
            .get("medium.state_default.X")
            .map(|value| value.real()),
        Some(0.42)
    );
}

#[test]
fn build_runtime_parameter_tail_env_keeps_declared_vector_when_start_len_matches_static_dim() {
    let mut dae = dae::Dae::default();

    let mut n = dae::Variable::new(VarName::new("plant.stage.n"));
    n.start = Some(int_lit(3));
    dae.variables
        .constants
        .insert(VarName::new("plant.stage.n"), n);

    let mut thresholds = dae::Variable::new(VarName::new("plant.stage.thehol"));
    thresholds.dims = vec![2];
    thresholds.start = Some(arr(vec![lit(0.5), lit(0.75)], false));
    dae.variables
        .constants
        .insert(VarName::new("plant.stage.thehol"), thresholds);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("declared vector length should win when start expression already matches it");

    assert_eq!(
        eval_shaped_array_values::<f64>(&var("plant.stage.thehol"), &env, 2),
        Ok(vec![0.5, 0.75])
    );
    assert_eq!(
        eval_expr::<f64>(
            &builtin(BuiltinFunction::Size, vec![var("plant.stage.thehol")]),
            &env
        ),
        Ok(2.0)
    );
}

#[test]
fn build_runtime_parameter_tail_env_projects_record_array_field_from_target_index() {
    let mut dae = dae::Dae::default();

    let mut source_1 = dae::Variable::new(VarName::new("system.dat[1].capacity"));
    source_1.start = Some(lit(10.0));
    dae.variables
        .constants
        .insert(VarName::new("system.dat[1].capacity"), source_1);

    let mut source_2 = dae::Variable::new(VarName::new("system.dat[2].capacity"));
    source_2.start = Some(lit(20.0));
    dae.variables
        .constants
        .insert(VarName::new("system.dat[2].capacity"), source_2);

    let mut member = dae::Variable::new(VarName::new("system.wrapper.unit[2].per.capacity"));
    member.start = Some(rumoca_core::Expression::FieldAccess {
        base: Box::new(var("system.wrapper.dat")),
        field: "capacity".to_string(),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .constants
        .insert(VarName::new("system.wrapper.unit[2].per.capacity"), member);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("record array field access should project using target component index");

    assert_eq!(
        eval_expr::<f64>(&var("system.wrapper.unit[2].per.capacity"), &env),
        Ok(20.0)
    );
}

#[test]
fn build_runtime_parameter_tail_env_projects_record_array_field_through_forward_source_start() {
    let mut dae = dae::Dae::default();

    let mut member = dae::Variable::new(VarName::new("plant.wrapper.unit[1].per.capacity"));
    member.start = Some(rumoca_core::Expression::FieldAccess {
        base: Box::new(var("plant.wrapper.dat")),
        field: "capacity".to_string(),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .constants
        .insert(VarName::new("plant.wrapper.unit[1].per.capacity"), member);

    let mut wrapper_per = dae::Variable::new(VarName::new("plant.wrapper.per[1].capacity"));
    wrapper_per.start = Some(var("plant.dat[1].capacity"));
    dae.variables
        .constants
        .insert(VarName::new("plant.wrapper.per[1].capacity"), wrapper_per);

    let mut source = dae::Variable::new(VarName::new("plant.dat[1].capacity"));
    source.start = Some(var("root.dat[1].capacity"));
    dae.variables
        .constants
        .insert(VarName::new("plant.dat[1].capacity"), source);

    let mut root = dae::Variable::new(VarName::new("root.dat[1].capacity"));
    root.start = Some(lit(42.0));
    dae.variables
        .constants
        .insert(VarName::new("root.dat[1].capacity"), root);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("record array field projection should resolve forward source starts");

    assert_eq!(
        eval_expr::<f64>(&var("plant.wrapper.unit[1].per.capacity"), &env),
        Ok(42.0)
    );
}

#[test]
fn build_runtime_parameter_tail_env_binds_numbered_volume_nominal_flow_alias_without_start() {
    let mut dae = dae::Dae::default();

    let mut side_flow = dae::Variable::new(VarName::new("plant.hex.m1_flow_nominal"));
    side_flow.start = Some(lit(12.5));
    dae.variables
        .constants
        .insert(VarName::new("plant.hex.m1_flow_nominal"), side_flow);

    let mut tau = dae::Variable::new(VarName::new("plant.hex.tau1"));
    tau.start = Some(lit(30.0));
    dae.variables
        .constants
        .insert(VarName::new("plant.hex.tau1"), tau);

    let mut rho = dae::Variable::new(VarName::new("plant.hex.rho1_nominal"));
    rho.start = Some(lit(1000.0));
    dae.variables
        .constants
        .insert(VarName::new("plant.hex.rho1_nominal"), rho);

    let volume_flow = dae::Variable::new(VarName::new("plant.hex.vol1.m_flow_nominal"));
    dae.variables
        .constants
        .insert(VarName::new("plant.hex.vol1.m_flow_nominal"), volume_flow);

    let volume = dae::Variable::new(VarName::new("plant.hex.vol1.V"));
    dae.variables
        .constants
        .insert(VarName::new("plant.hex.vol1.V"), volume);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("numbered volume modifiers should bind from same-side parent parameters");

    assert_eq!(
        eval_expr::<f64>(&var("plant.hex.vol1.m_flow_nominal"), &env),
        Ok(12.5)
    );
    assert_eq!(eval_expr::<f64>(&var("plant.hex.vol1.V"), &env), Ok(0.375));
}

#[test]
fn build_runtime_parameter_tail_env_uses_trace_substance_nominal_source_default() {
    let mut dae = dae::Dae::default();

    let mut child = dae::Variable::new(VarName::new("floor.zone.C_nominal"));
    child.start = Some(var("floor.C_nominal"));
    dae.variables
        .constants
        .insert(VarName::new("floor.zone.C_nominal"), child);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("trace-substance nominal default should seed missing parent alias");

    assert_eq!(
        eval_expr::<f64>(&var("floor.zone.C_nominal"), &env),
        Ok(1.0e-2)
    );
}

#[test]
fn build_runtime_parameter_tail_env_skips_zero_length_array_comprehension_before_scalar_validation()
{
    let mut dae = dae::Dae::default();

    let mut substance = dae::Variable::new(VarName::new("volume.dynBal.s"));
    substance.dims = vec![0];
    substance.start = Some(rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::new("Modelica.Utilities.Strings.isEqual"),
                    args: vec![
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::String("trace".to_string()),
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::String("mass".to_string()),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                },
                var("i"),
            )],
            else_branch: Box::new(lit(0.0)),
            span: rumoca_core::Span::DUMMY,
        }),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int_lit(1)),
                step: None,
                end: Box::new(int_lit(0)),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .constants
        .insert(VarName::new("volume.dynBal.s"), substance);

    try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("zero-length array comprehension should not be scalar-validated");
}

#[test]
fn build_runtime_parameter_tail_env_binds_scalar_times_array_comprehension_start() {
    let mut dae = dae::Dae::default();

    let mut nominal = dae::Variable::new(VarName::new("pump.m_flow_nominal"));
    nominal.start = Some(lit(12.0));
    dae.variables
        .constants
        .insert(VarName::new("pump.m_flow_nominal"), nominal);

    let mut speeds = dae::Variable::new(VarName::new("pump.per.speeds"));
    speeds.dims = vec![3];
    speeds.start = Some(arr(vec![lit(0.5), lit(0.75), lit(1.0)], false));
    dae.variables
        .constants
        .insert(VarName::new("pump.per.speeds"), speeds);

    let mut mass_flow_rates = dae::Variable::new(VarName::new("pump.massFlowRates"));
    mass_flow_rates.dims = vec![3];
    mass_flow_rates.start = Some(binop(
        rumoca_core::OpBinary::Mul,
        var("pump.m_flow_nominal"),
        rumoca_core::Expression::ArrayComprehension {
            expr: Box::new(binop(
                rumoca_core::OpBinary::Div,
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("pump.per.speeds"),
                    subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(var("i")))],
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("pump.per.speeds"),
                    subscripts: vec![rumoca_core::Subscript::generated_index(
                        1,
                        rumoca_core::Span::DUMMY,
                    )],
                    span: rumoca_core::Span::DUMMY,
                },
            )),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: "i".to_string(),
                range: rumoca_core::Expression::Range {
                    start: Box::new(int_lit(1)),
                    step: None,
                    end: Box::new(int_lit(3)),
                    span: rumoca_core::Span::DUMMY,
                },
            }],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        },
    ));
    dae.variables
        .constants
        .insert(VarName::new("pump.massFlowRates"), mass_flow_rates);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("scalar times array comprehension should bind vector start");

    assert_eq!(
        eval_shaped_array_values::<f64>(&var("pump.massFlowRates"), &env, 3),
        Ok(vec![12.0, 18.0, 24.0])
    );
}

#[test]
fn build_runtime_parameter_tail_env_uses_array_comprehension_length_and_end_subscript() {
    let mut dae = dae::Dae::default();

    let mut nominal = dae::Variable::new(VarName::new("pump.m_flow_nominal"));
    nominal.start = Some(lit(12.0));
    dae.variables
        .constants
        .insert(VarName::new("pump.m_flow_nominal"), nominal);

    let mut speeds = dae::Variable::new(VarName::new("pump.per.speeds"));
    speeds.dims = vec![1];
    speeds.start = Some(arr(vec![lit(1.0)], false));
    dae.variables
        .constants
        .insert(VarName::new("pump.per.speeds"), speeds);

    let mut mass_flow_rates = dae::Variable::new(VarName::new("pump.massFlowRates"));
    mass_flow_rates.dims = vec![3];
    mass_flow_rates.start = Some(binop(
        rumoca_core::OpBinary::Mul,
        var("pump.m_flow_nominal"),
        rumoca_core::Expression::ArrayComprehension {
            expr: Box::new(binop(
                rumoca_core::OpBinary::Div,
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("pump.per.speeds"),
                    subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(var("i")))],
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("pump.per.speeds"),
                    subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(int_lit(0)))],
                    span: rumoca_core::Span::DUMMY,
                },
            )),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: "i".to_string(),
                range: rumoca_core::Expression::Range {
                    start: Box::new(int_lit(1)),
                    step: None,
                    end: Box::new(builtin(
                        rumoca_core::BuiltinFunction::Size,
                        vec![var("pump.per.speeds"), int_lit(1)],
                    )),
                    span: rumoca_core::Span::DUMMY,
                },
            }],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        },
    ));
    dae.variables
        .constants
        .insert(VarName::new("pump.massFlowRates"), mass_flow_rates);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("array-like start length should update dynamic vector dimension");

    assert_eq!(
        eval_shaped_array_values::<f64>(&var("pump.massFlowRates"), &env, 1),
        Ok(vec![12.0])
    );
    assert_eq!(
        eval_expr::<f64>(
            &builtin(BuiltinFunction::Size, vec![var("pump.massFlowRates")]),
            &env
        ),
        Ok(1.0)
    );
}

#[test]
fn try_build_runtime_parameter_tail_env_rejects_short_parameter_vector() {
    let mut dae = dae::Dae::default();
    dae.variables
        .parameters
        .insert(VarName::new("p"), dae::Variable::new(VarName::new("p")));

    assert_eq!(
        try_build_runtime_parameter_tail_env(&dae, &[], 0.0).expect_err("short p should fail"),
        EvalError::ShortRuntimeVector {
            vector: "p",
            expected: 1,
            actual: 0,
        }
    );
}

#[test]
fn try_build_runtime_parameter_tail_env_rejects_bad_array_start_shape() {
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(17), 5, 13);
    let mut dae = dae::Dae::default();
    let mut constant = dae::Variable::new(VarName::new("c"));
    constant.source_span = span;
    constant.dims = vec![3];
    constant.start = Some(arr(vec![lit(1.0), lit(2.0)], false));
    dae.variables.constants.insert(VarName::new("c"), constant);

    let err = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect_err("array start shape mismatch should fail env construction");
    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err,
        EvalError::ShapeMismatch {
            context: "shaped array value",
            expected: 3,
            actual: 2,
        }
        .with_fallback_span(span)
    );
}

#[test]
fn build_runtime_parameter_tail_env_binds_vector_builtin_array_start() {
    let mut dae = dae::Dae::default();
    let mut discrete = dae::Variable::new(VarName::new("buf"));
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

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("vector(array) start should populate discrete array values");

    assert_eq!(env.get("buf[1]"), 6.5);
    assert_eq!(env.get("buf[2]"), 0.0);
    assert_eq!(env.get("buf[3]"), 0.0);
}

#[test]
fn build_runtime_parameter_tail_env_uses_default_discrete_start_dependencies() {
    let mut dae = dae::Dae::default();
    dae.variables.discrete_valued.insert(
        VarName::new("condition"),
        dae::Variable::new(VarName::new("condition")),
    );

    let mut local_condition = dae::Variable::new(VarName::new("localCondition"));
    local_condition.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("condition"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .discrete_valued
        .insert(VarName::new("localCondition"), local_condition);

    let env = try_build_runtime_parameter_tail_env(&dae, &[], 0.0)
        .expect("discrete defaults should be available to dependent starts");

    assert_eq!(env.get("condition"), 0.0);
    assert_eq!(env.get("localCondition"), 0.0);
}

#[test]
fn build_runtime_parameter_tail_env_binds_constants_before_parameter_starts() {
    let mut dae = dae::Dae::default();

    let mut table = dae::Variable::new(VarName::new("clock.conversionTable"));
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

    let mut resolution = dae::Variable::new(VarName::new("clock.resolution"));
    resolution.start = Some(lit(6.0));
    dae.variables
        .parameters
        .insert(VarName::new("clock.resolution"), resolution);

    let mut factor = dae::Variable::new(VarName::new("clock.resolutionFactor"));
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

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0);

    assert_eq!(env.get("clock.conversionTable[6]"), 1_000.0);
    assert_eq!(env.get("clock.resolutionFactor"), 1_000.0);
}

#[test]
fn build_runtime_parameter_tail_env_resolves_forward_parameter_array_dependency() {
    let mut dae = dae::Dae::default();

    let mut selected = dae::Variable::new(VarName::new("body.I_11"));
    selected.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("body.I[1,1]"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("body.I_11"), selected);

    let mut inertia = dae::Variable::new(VarName::new("body.I"));
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

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0);

    assert_eq!(env.get("body.I[1,1]"), 1.0);
    assert_eq!(env.get("body.I_11"), 1.0);
}

#[test]
fn build_runtime_parameter_tail_env_prefers_pre_store_for_lowered_pre_parameters() {
    clear_pre_values();

    let mut dae = dae::Dae::default();
    let mut pre = dae::Variable::new(VarName::new("__pre__.reset"));
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
    let env = build_runtime_parameter_tail_env_with_runtime(&dae, &[0.0], 0.0, runtime);

    assert_eq!(env.get("__pre__.reset"), 1.0);

    clear_pre_values();
}

#[test]
fn build_runtime_parameter_tail_env_keeps_pre_store_runtime_local() {
    clear_pre_values();

    let mut dae = dae::Dae::default();
    dae.variables.parameters.insert(
        VarName::new("__pre__.reset"),
        dae::Variable::new(VarName::new("__pre__.reset")),
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

    let built_a = build_runtime_parameter_tail_env_with_runtime(&dae, &[0.0], 0.0, runtime_a);
    let built_b = build_runtime_parameter_tail_env_with_runtime(&dae, &[0.0], 0.0, runtime_b);

    assert_eq!(built_a.get("__pre__.reset"), 1.0);
    assert_eq!(built_b.get("__pre__.reset"), 2.0);

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
    dae.variables
        .states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.variables.parameters.insert(
        VarName::new("__pre__.reset"),
        dae::Variable::new(VarName::new("__pre__.reset")),
    );

    let mut env = build_env(&dae, &[0.0], &[0.0], 0.0);
    set_pre_value_in_env(&env, "reset", 2.0);
    refresh_env_solver_and_parameter_values(&mut env, &dae, &[1.0], &[0.0], 0.5);

    assert_eq!(env.get("x"), 1.0);
    assert_eq!(env.get("__pre__.reset"), 2.0);

    clear_pre_values();
}

#[test]
fn build_runtime_parameter_tail_env_skips_zero_sized_parameter_slots() {
    let mut dae = dae::Dae::default();

    let mut dyn_arr = dae::Variable::new(VarName::new("dyn_arr"));
    dyn_arr.dims = vec![0];
    dae.variables
        .parameters
        .insert(VarName::new("dyn_arr"), dyn_arr);

    dae.variables.parameters.insert(
        VarName::new("table_id"),
        dae::Variable::new(VarName::new("table_id")),
    );

    let env = build_runtime_parameter_tail_env(&dae, &[4.0], 0.0);

    assert_eq!(env.get("table_id"), 4.0);
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

    let mut before = dae::Variable::new(VarName::new("before"));
    before.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Modelica.Electrical.Digital.Interfaces.Logic.'X'"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("before"), before);

    let mut after = dae::Variable::new(VarName::new("after"));
    after.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("after"), after);

    let mut u = dae::Variable::new(VarName::new("u"));
    u.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("before"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.inputs.insert(VarName::new("u"), u);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0);

    assert_eq!(env.get("before"), 2.0);
    assert_eq!(env.get("after"), 3.0);
    assert_eq!(env.get("u"), 2.0);
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

    let mut before = dae::Variable::new(VarName::new("Enable.before"));
    before.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("Enable.before"), before);

    let mut after = dae::Variable::new(VarName::new("Enable.after"));
    after.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Modelica.Electrical.Digital.Interfaces.Logic.'1'"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("Enable.after"), after);

    let mut step_time = dae::Variable::new(VarName::new("Enable.stepTime"));
    step_time.start = Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(1.0),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("Enable.stepTime"), step_time);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0);

    assert_eq!(env.get("Enable.before"), 3.0);
    assert_eq!(env.get("Enable.after"), 4.0);
    assert_eq!(env.get("Enable.stepTime"), 1.0);
}

#[test]
fn build_runtime_parameter_tail_env_binds_counter_like_enum_parameter_chain() {
    let mut dae = dae::Dae::default();
    dae.symbols.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
        3,
    );

    let mut q0 = dae::Variable::new(VarName::new("Counter.q0"));
    q0.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("Counter.q0"), q0);

    let mut ff_q0 = dae::Variable::new(VarName::new("Counter.FF[1].q0"));
    ff_q0.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Counter.q0"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("Counter.FF[1].q0"), ff_q0);

    let mut td_y0 = dae::Variable::new(VarName::new("Counter.FF[1].RS1.TD1.y0"));
    td_y0.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Counter.q0"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .parameters
        .insert(VarName::new("Counter.FF[1].RS1.TD1.y0"), td_y0);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0);

    assert_eq!(env.get("Counter.q0"), 3.0);
    assert_eq!(env.get("Counter.FF[1].q0"), 3.0);
    assert_eq!(env.get("Counter.FF[1].RS1.TD1.y0"), 3.0);
}

#[test]
fn build_runtime_parameter_tail_env_broadcasts_enum_literal_to_discrete_array_start() {
    let mut dae = dae::Dae::default();
    dae.symbols.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
        1,
    );

    let mut auxiliary = dae::Variable::new(VarName::new("auxiliary"));
    auxiliary.dims = vec![3];
    auxiliary.start = Some(rumoca_core::Expression::VarRef {
        name: Reference::new("Modelica.Electrical.Digital.Interfaces.Logic.'U'"),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .discrete_valued
        .insert(VarName::new("auxiliary"), auxiliary);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0);

    assert_eq!(env.get("auxiliary[1]"), 1.0);
    assert_eq!(env.get("auxiliary[2]"), 1.0);
    assert_eq!(env.get("auxiliary[3]"), 1.0);
}

#[test]
fn build_runtime_parameter_tail_env_binds_singleton_parameter_array_index_entries() {
    let mut dae = dae::Dae::default();

    let mut t_param = dae::Variable::new(VarName::new("a.t"));
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

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0);

    assert_eq!(env.get("a.t"), 1.0);
    assert_eq!(env.get("a.t[1]"), 1.0);
}

#[test]
fn build_env_skips_zero_sized_solver_slots() {
    let mut dae = dae::Dae::default();

    let mut dyn_out = dae::Variable::new(VarName::new("dyn_out"));
    dyn_out.dims = vec![0];
    dae.variables
        .outputs
        .insert(VarName::new("dyn_out"), dyn_out);
    dae.variables
        .outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    let env = build_env(&dae, &[7.0], &[], 0.0);

    assert_eq!(env.get("y"), 7.0);
    assert!(!env.vars.contains_key("dyn_out"));
}
