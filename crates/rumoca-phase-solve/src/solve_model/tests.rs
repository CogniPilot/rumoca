use super::*;

fn scalar_var(name: &str) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        ..Default::default()
    }
}

fn var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn component_ref(name: &str) -> rumoca_core::ComponentReference {
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

fn indexed_var(name: &str, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(int_expr(
            index,
        )))],
        span: rumoca_core::Span::DUMMY,
    }
}

fn field_access_expr(base: rumoca_core::Expression, field: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::FieldAccess {
        base: Box::new(base),
        field: field.to_string(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn slice_expr(base: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Index {
        base: Box::new(base),
        subscripts: vec![rumoca_core::Subscript::generated_colon(
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    }
}

fn int_expr(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn string_expr(value: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String(value.to_string()),
        span: rumoca_core::Span::DUMMY,
    }
}

fn array_expr(elements: Vec<rumoca_core::Expression>, is_matrix: bool) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements,
        is_matrix,
        span: rumoca_core::Span::DUMMY,
    }
}

fn real_expr(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn call_expr(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn constructor_call_expr(
    name: &str,
    args: Vec<rumoca_core::Expression>,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    }
}

fn sub(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn binary_expr(
    op: rumoca_core::OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn der(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![expr],
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn visible_expressions_use_direct_output_definition_rhs() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model
        .variables
        .inputs
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: binary_expr(rumoca_core::OpBinary::Mul, var("u"), real_expr(2.0)),
        scalar_count: 1,
        span: rumoca_core::Span::DUMMY,
        origin: "direct output equation".to_string(),
    });

    let visible = visible_expressions_for_dae(&dae_model);
    let y_visible = visible
        .iter()
        .find(|entry| entry.name == "y")
        .expect("output y should be visible");

    assert!(
        !matches!(
            &y_visible.expr,
            rumoca_core::Expression::VarRef { name, subscripts, .. }
                if subscripts.is_empty() && name.as_str() == "y"
        ),
        "causal output observations must use the direct output definition RHS, not an identity binding"
    );
}

#[test]
fn lower_expands_visible_observation_for_scalarized_record_field_slice() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    for index in 1..=3 {
        let name = format!("pin[{index}].v");
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new(name.as_str()),
            scalar_var(name.as_str()),
        );
        dae_model.continuous.equations.push(dae::Equation::residual(
            sub(var(name.as_str()), real_expr(index as f64)),
            Default::default(),
            format!("{name} row"),
        ));
    }
    dae_model.continuous.equations.insert(
        0,
        dae::Equation::residual(
            sub(der(var("x")), real_expr(0.0)),
            Default::default(),
            "x row",
        ),
    );
    let visible = vec![VisibleExpression {
        name: "pin[:].v".to_string(),
        expr: field_access_expr(slice_expr(var("pin")), "v"),
    }];

    let prepared = lower_dae_to_solve_model_owned_with_visible_expressions(dae_model, visible)
        .expect("scalarized record field slice observation should lower");

    assert_eq!(
        prepared.visible_names,
        ["pin[:].v[1]", "pin[:].v[2]", "pin[:].v[3]"]
    );
    assert_eq!(prepared.visible_value_rows.programs.len(), 3);
}

#[test]
fn seed_var_values_rejects_empty_scalar_seed() {
    let var = scalar_var("p");
    let mut env = rumoca_eval_dae::VarEnv::<f64>::new();

    let err = seed_var_values(&mut env, "p", &var, &[])
        .expect_err("empty scalar seed must fail instead of defaulting to zero");

    let message = err.to_string();
    assert!(message.contains("start value for `p`"));
    assert!(message.contains("empty scalar start value"));
}

#[test]
fn initial_solver_values_preserve_vector_start_reference() {
    let mut dae_model = dae::Dae::default();
    let mut shape_type = scalar_var("body1.shapeType");
    shape_type.start = Some(string_expr("cylinder"));
    dae_model
        .variables
        .parameters
        .insert(shape_type.name.clone(), shape_type);

    let mut q_start = scalar_var("body1.Q_start");
    q_start.dims = vec![4];
    q_start.start = Some(array_expr(
        vec![
            real_expr(0.0),
            real_expr(0.0),
            real_expr(0.0),
            real_expr(1.0),
        ],
        false,
    ));
    dae_model
        .variables
        .parameters
        .insert(q_start.name.clone(), q_start);

    let mut q = scalar_var("body1.Q");
    q.dims = vec![4];
    q.start = Some(var("body1.Q_start"));
    dae_model.variables.states.insert(q.name.clone(), q);

    let params = default_parameter_values(&dae_model, None, Arc::new(EvalRuntimeState::default()))
        .expect("parameter vector should lower");
    assert_eq!(params, vec![0.0, 0.0, 0.0, 0.0, 1.0]);

    let initial_y = initial_solver_values(
        &dae_model,
        None,
        &params,
        4,
        Arc::new(EvalRuntimeState::default()),
    )
    .expect("state starts should lower");

    assert_eq!(initial_y, vec![0.0, 0.0, 0.0, 1.0]);
}

#[test]
fn lower_uses_default_numeric_slot_for_string_parameter_start() {
    let mut dae_model = dae::Dae::default();
    let mut method = scalar_var("periodicClock.solverMethod");
    method.start = Some(rumoca_core::Expression::Literal {
        value: Literal::String("ExplicitEuler".to_string()),
        span: rumoca_core::Span::DUMMY,
    });
    dae_model
        .variables
        .parameters
        .insert(method.name.clone(), method);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("string-valued parameter starts should not enter numeric evaluation");
    let Some(solve::ScalarSlot::P { index, .. }) = prepared
        .problem
        .layout
        .binding("periodicClock.solverMethod")
    else {
        panic!("expected string-valued parameter to keep a numeric P slot");
    };

    assert_eq!(prepared.parameters[index], 0.0);
}

#[test]
fn lower_uses_default_numeric_slot_for_nested_string_parameter_start() {
    let mut dae_model = dae::Dae::default();
    let mut terminal = scalar_var("settings.terminalConnection");
    terminal.start = Some(rumoca_core::Expression::If {
        branches: vec![(
            rumoca_core::Expression::Literal {
                value: Literal::Boolean(true),
                span: rumoca_core::Span::DUMMY,
            },
            string_expr("TerminalBox"),
        )],
        else_branch: Box::new(string_expr("NoTerminalBox")),
        span: rumoca_core::Span::DUMMY,
    });
    dae_model
        .variables
        .parameters
        .insert(terminal.name.clone(), terminal);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("nested string-valued parameter starts should use numeric defaults");
    let Some(solve::ScalarSlot::P { index, .. }) = prepared
        .problem
        .layout
        .binding("settings.terminalConnection")
    else {
        panic!("expected string-valued parameter to keep a numeric P slot");
    };

    assert_eq!(prepared.parameters[index], 0.0);
}

#[test]
fn lower_binds_nested_parameter_start_alias_before_parent_parameter_order() {
    let mut dae_model = dae::Dae::default();

    let mut log_level = scalar_var("floor.zon[1].logLevel");
    log_level.start = Some(field_access_expr(var("floor.zon[1].fmuZon"), "AFlo"));
    dae_model
        .variables
        .parameters
        .insert(log_level.name.clone(), log_level);

    let mut area = scalar_var("floor.zon[1].AFlo");
    area.start = Some(real_expr(48.0));
    dae_model
        .variables
        .parameters
        .insert(area.name.clone(), area);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("nested parameter start aliases should be available during solve lowering");

    let Some(solve::ScalarSlot::P { index, .. }) =
        prepared.problem.layout.binding("floor.zon[1].logLevel")
    else {
        panic!("expected logLevel parameter slot");
    };
    assert_eq!(prepared.parameters[index], 48.0);
}

#[test]
fn lower_uses_default_for_parameter_start_depending_on_fixed_false_parameter() {
    let mut dae_model = dae::Dae::default();

    let mut external_area = scalar_var("zone.fmuZon.AFlo");
    external_area.fixed = Some(false);
    dae_model
        .variables
        .parameters
        .insert(external_area.name.clone(), external_area);

    let mut area = scalar_var("zone.AFlo");
    area.start = Some(field_access_expr(var("zone.fmuZon"), "AFlo"));
    dae_model
        .variables
        .parameters
        .insert(area.name.clone(), area);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("fixed=false parameter dependencies should be initialized at runtime");

    let Some(solve::ScalarSlot::P { index, .. }) = prepared.problem.layout.binding("zone.AFlo")
    else {
        panic!("expected AFlo parameter slot");
    };
    assert_eq!(prepared.parameters[index], 0.0);
}

#[test]
fn lower_accepts_singleton_range_scalar_start_guess() {
    let mut dae_model = dae::Dae::default();

    let mut scalar = scalar_var("medium.state_default.X");
    scalar.start = Some(rumoca_core::Expression::Range {
        start: Box::new(real_expr(0.42)),
        step: None,
        end: Box::new(real_expr(0.42)),
        span: rumoca_core::Span::DUMMY,
    });
    dae_model
        .variables
        .parameters
        .insert(scalar.name.clone(), scalar);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("singleton array-like range starts may initialize scalar slots");

    let Some(solve::ScalarSlot::P { index, .. }) =
        prepared.problem.layout.binding("medium.state_default.X")
    else {
        panic!("expected scalar parameter slot");
    };
    assert_eq!(prepared.parameters[index], 0.42);
}

#[test]
fn lower_sizes_initial_vector_from_final_solve_layout() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("v"), scalar_var("v"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("v"), real_expr(1.0)),
        Default::default(),
        "visible algebraic residual",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        real_expr(0.0),
        Default::default(),
        "extra template residual",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("lowering should tolerate extra non-state residual rows");

    assert_eq!(prepared.problem.solve_layout.solver_scalar_count(), 1);
    assert_eq!(prepared.initial_y.len(), 1);
}

#[test]
fn lower_keeps_algebraic_binding_needed_by_derivative_rhs() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert("x".into(), scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("a"), scalar_var("a"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(der(var("x")), var("a")),
        Default::default(),
        // MLS Appendix B B.1a: derivative rows may read algebraics from
        // the same continuous implicit system even when residual rows are
        // reduced before solve-IR lowering.
        "der(x) = a",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("algebraic dependency in derivative RHS should remain bound");

    assert_eq!(prepared.problem.solve_layout.solver_scalar_count(), 2);
    assert_eq!(
        prepared.problem.layout.binding("a"),
        Some(solve::scalar_slot_y(1))
    );
    assert_eq!(prepared.initial_y.len(), 2);
}

#[test]
fn lower_replaces_nonfinite_start_guess_with_type_default() {
    let mut dae_model = dae::Dae::default();
    let mut v = scalar_var("v");
    v.start = Some(binary_expr(div_op(), real_expr(0.0), real_expr(0.0)));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("v"), v);
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("v"), real_expr(1.0)),
        Default::default(),
        // MLS §4.4/§8.6: start is only an initialization guess; a
        // non-finite guess must not prevent equations from solving `v`.
        "non-finite start guess for algebraic",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("non-finite start guesses should be sanitized during Solve IR lowering");

    assert_eq!(prepared.initial_y, vec![0.0]);
}

#[test]
fn lower_seeds_solver_visible_start_dependencies_from_default_guesses() {
    let mut dae_model = dae::Dae::default();
    let mut source = scalar_var("source");
    source.start = Some(real_expr(2.0));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("source"), source);

    let mut dependent = scalar_var("dependent");
    dependent.start = Some(binary_expr(OpBinary::Add, var("source"), real_expr(1.0)));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("dependent"), dependent);

    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("source"), real_expr(0.0)),
        Default::default(),
        "source algebraic residual",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("dependent"), real_expr(0.0)),
        Default::default(),
        "dependent algebraic residual",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("solver-visible start dependencies should be seeded");

    assert_eq!(prepared.initial_y, vec![2.0, 3.0]);
}

#[test]
fn lower_seeds_start_dependencies_removed_by_structural_metadata() {
    let mut dae_model = dae::Dae::default();
    let mut dependent = scalar_var("dependent");
    dependent.start = Some(var("alias.source"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("dependent"), dependent);
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("dependent"), real_expr(0.0)),
        Default::default(),
        "dependent algebraic residual",
    ));

    let mut metadata_dae_model = dae_model.clone();
    metadata_dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("alias.source"),
        scalar_var("alias.source"),
    );

    let prepared = lower_dae_to_solve_model_owned_with_visible_expressions_and_metadata(
        dae_model,
        Vec::new(),
        &metadata_dae_model,
    )
    .expect("metadata defaults should seed structurally removed start aliases");

    assert_eq!(prepared.initial_y, vec![0.0]);
}

#[test]
fn lower_rejects_missing_binding_in_explicit_start_guess() {
    let start_span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(7), 11, 19);
    let mut dae_model = dae::Dae::default();
    let mut v = scalar_var("v");
    v.start = Some(rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new("missing").into(),
        subscripts: Vec::new(),
        span: start_span,
    });
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("v"), v);
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("v"), real_expr(1.0)),
        Default::default(),
        "explicit start references a missing binding",
    ));

    let err = lower_dae_to_solve_model(&dae_model)
        .expect_err("malformed explicit start expression should fail lowering");

    assert!(matches!(
        err,
        SolveModelLowerError::Evaluation {
            source: EvalError::MissingBinding { ref name },
            ..
        } if name == "missing"
    ));
    assert_eq!(err.source_span(), Some(start_span));
}

#[test]
fn lower_projects_record_array_field_start_from_forward_source() {
    let mut dae_model = dae::Dae::default();

    let mut target = scalar_var("plant.wrapper.unit[1].per.capacity");
    target.start = Some(field_access_expr(var("plant.wrapper.dat"), "capacity"));
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("plant.wrapper.unit[1].per.capacity"),
        target,
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("plant.wrapper.unit[1].per.capacity"), real_expr(42.0)),
        Default::default(),
        "record array field projection target",
    ));

    let mut wrapper_per = scalar_var("plant.wrapper.per[1].capacity");
    wrapper_per.start = Some(var("plant.dat[1].capacity"));
    dae_model.variables.constants.insert(
        rumoca_core::VarName::new("plant.wrapper.per[1].capacity"),
        wrapper_per,
    );

    let mut source = scalar_var("plant.dat[1].capacity");
    source.start = Some(var("root.dat[1].capacity"));
    dae_model
        .variables
        .constants
        .insert(rumoca_core::VarName::new("plant.dat[1].capacity"), source);

    let mut root = scalar_var("root.dat[1].capacity");
    root.start = Some(real_expr(42.0));
    dae_model
        .variables
        .constants
        .insert(rumoca_core::VarName::new("root.dat[1].capacity"), root);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("record array field start should resolve through forward source starts");

    assert_eq!(prepared.initial_y, vec![42.0]);
}

#[test]
fn lower_projects_component_array_member_start_from_matrix_row() {
    let mut dae_model = dae::Dae::default();

    let mut source = scalar_var("plant.flow_curve");
    source.dims = vec![3, 3];
    source.start = Some(array_expr(
        vec![
            array_expr(vec![real_expr(0.1), real_expr(0.2), real_expr(0.3)], false),
            array_expr(vec![real_expr(0.4), real_expr(0.5), real_expr(0.6)], false),
            array_expr(vec![real_expr(0.7), real_expr(0.8), real_expr(0.9)], false),
        ],
        false,
    ));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("plant.flow_curve"), source);

    let mut target = scalar_var("plant.ct[2].v_flow_rate");
    target.dims = vec![3];
    target.start = Some(var("plant.flow_curve"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("plant.ct[2].v_flow_rate"), target);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("component array member start should project matrix row");

    assert_eq!(prepared.initial_y, vec![0.4, 0.5, 0.6]);
}

#[test]
fn lower_projects_indexed_matrix_start_row_to_vector() {
    let mut dae_model = dae::Dae::default();

    let mut source = scalar_var("plant.flow_curve");
    source.dims = vec![2, 3];
    source.start = Some(array_expr(
        vec![
            array_expr(vec![real_expr(0.1), real_expr(0.2), real_expr(0.3)], false),
            array_expr(vec![real_expr(0.4), real_expr(0.5), real_expr(0.6)], false),
        ],
        false,
    ));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("plant.flow_curve"), source);

    let mut target = scalar_var("plant.pump[1].VolFloCur");
    target.dims = vec![3];
    target.start = Some(indexed_var("plant.flow_curve", 2));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("plant.pump[1].VolFloCur"), target);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("indexed matrix start should project the selected row");

    assert_eq!(prepared.initial_y, vec![0.4, 0.5, 0.6]);
}

#[test]
fn lower_projects_repeated_aggregate_literal_member_start() {
    let mut dae_model = dae::Dae::default();

    let mut source = scalar_var("plant.Motor_eta");
    source.dims = vec![3, 1];
    source.start = Some(array_expr(
        vec![
            array_expr(vec![real_expr(0.7)], false),
            array_expr(vec![real_expr(0.8)], false),
            array_expr(vec![real_expr(0.9)], false),
        ],
        false,
    ));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("plant.Motor_eta"), source);

    let mut target = scalar_var("plant.pump[2].per.motorEfficiency.eta");
    target.dims = vec![1];
    target.start = Some(array_expr(
        vec![
            var("plant.Motor_eta"),
            var("plant.Motor_eta"),
            var("plant.Motor_eta"),
        ],
        false,
    ));
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("plant.pump[2].per.motorEfficiency.eta"),
        target,
    );

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("repeated aggregate literal should project the target component member");

    assert_eq!(prepared.initial_y, vec![0.8]);
}

#[test]
fn lower_broadcasts_indexed_scalar_matrix_row_start_to_vector() {
    let mut dae_model = dae::Dae::default();

    let mut source = scalar_var("plant.eta");
    source.dims = vec![2, 1];
    source.start = Some(array_expr(
        vec![
            array_expr(vec![real_expr(0.82)], false),
            array_expr(vec![real_expr(0.91)], false),
        ],
        false,
    ));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("plant.eta"), source);

    let mut target = scalar_var("plant.boiler[1].eta");
    target.dims = vec![2];
    target.start = Some(indexed_var("plant.eta", 1));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("plant.boiler[1].eta"), target);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("scalar matrix row start should broadcast to target vector");

    assert_eq!(prepared.initial_y, vec![0.82, 0.82]);
}

#[test]
fn lower_accepts_dynamic_vector_start_when_sibling_n_matches_length() {
    let mut dae_model = dae::Dae::default();

    let mut n = scalar_var("pump.curve.n");
    n.start = Some(real_expr(3.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("pump.curve.n"), n);

    let mut target = scalar_var("pump.curve.V_flow");
    target.dims = vec![2];
    target.start = Some(array_expr(
        vec![real_expr(0.1), real_expr(0.2), real_expr(0.3)],
        false,
    ));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("pump.curve.V_flow"), target);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("dynamic vector start should be accepted when sibling n matches");

    assert_eq!(prepared.initial_y, vec![0.1, 0.2]);
}

#[test]
fn lower_preserves_zero_dim_array_start_alias_as_empty_initial_values() {
    let mut dae_model = dae::Dae::default();

    let mut source = scalar_var("junSup1.C_start");
    source.dims = vec![0];
    source.start = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: vec![real_expr(0.0), int_expr(0)],
        span: rumoca_core::Span::DUMMY,
    });
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("junSup1.C_start"), source);

    let mut target = scalar_var("junSup1.vol.dynBal.C_start");
    target.dims = vec![0];
    target.start = Some(var("junSup1.C_start"));
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("junSup1.vol.dynBal.C_start"),
        target,
    );

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("zero-dimension array start aliases should lower as empty values");

    assert!(prepared.initial_y.is_empty());
}

#[test]
fn lower_uses_runtime_zero_extent_dims_for_start_values() {
    let mut dae_model = dae::Dae::default();

    let mut table = scalar_var("building.weaDat.datRea.table");
    table.dims = vec![1, 2];
    table.start = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: vec![real_expr(0.0), int_expr(0), int_expr(2)],
        span: rumoca_core::Span::DUMMY,
    });
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("building.weaDat.datRea.table"),
        table,
    );

    lower_dae_to_solve_model(&dae_model).expect(
        "runtime zero-extent table dimensions should not require static placeholder starts",
    );
}

#[test]
fn lower_evaluates_size_start_from_dae_dims_without_array_binding() {
    let mut dae_model = dae::Dae::default();
    let mut lines = scalar_var("world.x_label.lines");
    lines.dims = vec![2, 2, 2];
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("world.x_label.lines"), lines);

    let mut n = scalar_var("world.x_label.n");
    n.start = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![var("world.x_label.lines"), int_expr(1)],
        span: rumoca_core::Span::DUMMY,
    });
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("world.x_label.n"), n);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("size start should use DAE dimensions for known array variables");

    let Some(solve::ScalarSlot::P { index, .. }) =
        prepared.problem.layout.binding("world.x_label.n")
    else {
        panic!("world.x_label.n should lower to a parameter slot");
    };
    assert_eq!(prepared.parameters[index], 2.0);
}

#[test]
fn lower_propagates_fixed_start_across_alias_to_state_guess() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    let mut y = scalar_var("y");
    y.start = Some(real_expr(0.1));
    y.fixed = Some(true);
    dae_model.variables.outputs.insert("y".into(), y);
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(der(var("x")), real_expr(0.0)),
        Default::default(),
        "derivative row",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("y"), var("x")),
        Default::default(),
        "output y aliases state x",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model).expect("lowering");

    assert_eq!(prepared.initial_y, vec![0.1, 0.1]);
}

#[test]
fn lower_propagates_fixed_array_start_across_alias_to_solver_guess() {
    let mut dae_model = dae::Dae::default();
    let mut source = scalar_var("source");
    source.dims = vec![2];
    source.start = Some(array_expr(vec![real_expr(0.25), real_expr(0.75)], false));
    source.fixed = Some(true);
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("source"), source);

    let mut target = scalar_var("target");
    target.dims = vec![2];
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("target"), target);

    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("target"), var("source")),
        Default::default(),
        "target aliases source",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("array alias starts should seed scalar solver guesses");

    assert_eq!(prepared.initial_y, vec![0.25, 0.75, 0.25, 0.75]);
}

#[test]
fn lower_seeds_evaluable_continuous_assignment_to_initial_guess() {
    let mut dae_model = dae::Dae::default();
    let mut parameter = scalar_var("parameter");
    parameter.start = Some(real_expr(2.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("parameter"), parameter);

    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("computed"),
        scalar_var("computed"),
    );
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("alias"), scalar_var("alias"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(
            var("computed"),
            binary_expr(OpBinary::Add, var("parameter"), real_expr(1.0)),
        ),
        Default::default(),
        "evaluable continuous assignment",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("alias"), var("computed")),
        Default::default(),
        "alias of computed continuous assignment",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model).expect("continuous assignment should seed");

    assert_eq!(prepared.initial_y, vec![3.0, 3.0]);
}

#[test]
fn lower_applies_singleton_array_initial_assignment_to_scalar_target() {
    let mut dae_model = dae::Dae::default();

    let mut target = scalar_var("target");
    target.dims = vec![1];
    target.start = Some(array_expr(vec![real_expr(5.0)], false));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("target"), target);

    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            sub(var("target"), array_expr(vec![real_expr(0.0)], false)),
            Default::default(),
            "singleton array initial assignment",
        ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("singleton array initial assignment should seed scalar target");

    assert_eq!(prepared.initial_y, vec![0.0]);
}

#[test]
fn lower_does_not_overwrite_fixed_start_with_continuous_assignment_seed() {
    let mut dae_model = dae::Dae::default();
    let mut parameter = scalar_var("parameter");
    parameter.start = Some(real_expr(2.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("parameter"), parameter);

    let mut computed = scalar_var("computed");
    computed.start = Some(real_expr(5.0));
    computed.fixed = Some(true);
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("computed"), computed);
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(
            var("computed"),
            binary_expr(OpBinary::Add, var("parameter"), real_expr(1.0)),
        ),
        Default::default(),
        "continuous assignment must not replace fixed start seed",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model).expect("fixed start should remain seed");

    assert_eq!(prepared.initial_y, vec![5.0]);
}

#[test]
fn lower_skips_nonfinite_continuous_assignment_seed() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("computed"),
        scalar_var("computed"),
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(
            var("computed"),
            binary_expr(OpBinary::Div, real_expr(0.0), real_expr(0.0)),
        ),
        Default::default(),
        "non-finite continuous assignment seed",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model).expect("non-finite seed should be skipped");

    assert_eq!(prepared.initial_y, vec![0.0]);
}

#[test]
fn lower_refines_parameter_start_from_array_max_dependency() {
    let mut dae_model = dae::Dae::default();
    let mut a = scalar_var("a");
    a.start = Some(real_expr(0.1));
    let mut b = scalar_var("b");
    b.start = Some(real_expr(10.0));
    let mut c = scalar_var("c");
    c.start = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Max,
        args: vec![array_expr(
            vec![
                binary_expr(div_op(), var("a"), var("b")),
                real_expr(1.0e-12),
            ],
            true,
        )],
        span: rumoca_core::Span::DUMMY,
    });
    dae_model.variables.parameters.insert("a".into(), a);
    dae_model.variables.parameters.insert("b".into(), b);
    dae_model.variables.parameters.insert("c".into(), c);

    let prepared =
        lower_dae_to_solve_model(&dae_model).expect("parameter start dependencies should lower");
    let Some(rumoca_ir_solve::ScalarSlot::P { index, .. }) = prepared.problem.layout.binding("c")
    else {
        panic!("expected c parameter binding");
    };

    assert!((prepared.parameters[index] - 0.01).abs() <= 1.0e-12);
}

#[test]
fn lower_keeps_runtime_tail_names_visible_for_traces() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .inputs
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("z"), scalar_var("z"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("m"), scalar_var("m"));

    let prepared =
        lower_dae_to_solve_model(&dae_model).expect("no-state runtime-tail model should lower");

    assert_eq!(prepared.initial_y.len(), 0);
    assert_eq!(prepared.visible_names, ["u", "z", "m"]);
    assert_eq!(
        prepared
            .variable_meta
            .iter()
            .map(|meta| meta.role.as_str())
            .collect::<Vec<_>>(),
        ["input", "discrete-real", "discrete-valued"]
    );
}

#[test]
fn lower_skips_visible_observation_with_dynamic_subscript() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("a"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("a")
        },
    );
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("k"), scalar_var("k"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(der(var("x")), real_expr(0.0)),
        Default::default(),
        "derivative row",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(indexed_var("a", 1), real_expr(1.0)),
        Default::default(),
        "a[1] row",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(indexed_var("a", 2), real_expr(2.0)),
        Default::default(),
        "a[2] row",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("k"), real_expr(1.0)),
        Default::default(),
        "dynamic index row",
    ));
    let visible = vec![VisibleExpression {
        name: "fn_dynamic".to_string(),
        expr: rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("pick").into(),
                args: Vec::new(),
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(var("k")))],
            span: rumoca_core::Span::DUMMY,
        },
    }];

    let prepared = lower_dae_to_solve_model_owned_with_visible_expressions(dae_model, visible)
        .expect("dynamic observation should not block solve lowering");

    assert!(!prepared.visible_names.contains(&"fn_dynamic".to_string()));
}

#[test]
fn variable_meta_marks_relation_driven_real_output_event_discontinuous() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("y")),
            rhs: Box::new(rumoca_core::Expression::If {
                branches: vec![(
                    rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Lt,
                        lhs: Box::new(var("time")),
                        rhs: Box::new(real_expr(0.5)),
                        span: rumoca_core::Span::DUMMY,
                    },
                    real_expr(1.0),
                )],
                else_branch: Box::new(real_expr(0.0)),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
        origin: "relation driven output".to_string(),
        scalar_count: 1,
    });

    let meta = build_variable_meta(&dae_model, &["y".to_string()]);

    assert_eq!(meta.len(), 1);
    assert_eq!(meta[0].variability.as_deref(), Some("continuous"));
    assert_eq!(meta[0].time_domain.as_deref(), Some("event-discontinuous"));
}

#[test]
fn variable_meta_marks_guarded_residual_output_event_discontinuous() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("c"),
        dae::Variable {
            name: rumoca_core::VarName::new("c"),
            dims: vec![5],
            ..Default::default()
        },
    );
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::If {
            branches: vec![(indexed_var("c", 5), sub(var("y"), real_expr(1.0)))],
            else_branch: Box::new(sub(var("y"), real_expr(0.0))),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
        origin: "event guarded residual".to_string(),
        scalar_count: 1,
    });

    let meta = build_variable_meta(&dae_model, &["y".to_string()]);

    assert_eq!(meta.len(), 1);
    assert_eq!(meta[0].variability.as_deref(), Some("continuous"));
    assert_eq!(meta[0].time_domain.as_deref(), Some("event-discontinuous"));
}

#[test]
fn variable_meta_propagates_event_discontinuity_through_algebraic_dependency() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("a"), scalar_var("a"));
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("a")),
        rhs: rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Ge,
                    lhs: Box::new(var("time")),
                    rhs: Box::new(real_expr(0.0)),
                    span: rumoca_core::Span::DUMMY,
                },
                real_expr(1.0),
            )],
            else_branch: Box::new(real_expr(0.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
        origin: "relation driven algebraic".to_string(),
        scalar_count: 1,
    });
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: var("a"),
        span: rumoca_core::Span::DUMMY,
        origin: "dependent output".to_string(),
        scalar_count: 1,
    });

    let meta = build_variable_meta(&dae_model, &["y".to_string()]);

    assert_eq!(meta.len(), 1);
    assert_eq!(meta[0].time_domain.as_deref(), Some("event-discontinuous"));
}

#[test]
fn lower_supports_visible_observation_with_array_literal_size_range() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.badSizeRange"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("My.badSizeRange"),
            def_id: None,
            inputs: vec![],
            outputs: vec![rumoca_core::FunctionParam::new("out", "Real")],
            locals: vec![],
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref("out"),
                    value: real_expr(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Statement::For {
                    indices: vec![rumoca_core::ForIndex {
                        ident: "i".to_string(),
                        range: rumoca_core::Expression::Range {
                            start: Box::new(int_expr(1)),
                            step: None,
                            end: Box::new(rumoca_core::Expression::BuiltinCall {
                                function: rumoca_core::BuiltinFunction::Size,
                                args: vec![array_expr(vec![real_expr(1.0), real_expr(2.0)], false)],
                                span: rumoca_core::Span::DUMMY,
                            }),
                            span: rumoca_core::Span::DUMMY,
                        },
                    }],
                    equations: vec![rumoca_core::Statement::Assignment {
                        comp: component_ref("out"),
                        value: rumoca_core::Expression::Binary {
                            op: OpBinary::Add,
                            lhs: Box::new(var("out")),
                            rhs: Box::new(var("x")),
                            span: rumoca_core::Span::DUMMY,
                        },
                        span: rumoca_core::Span::DUMMY,
                    }],
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: Default::default(),
        },
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(der(var("x")), real_expr(0.0)),
        Default::default(),
        "derivative row",
    ));
    let visible = vec![VisibleExpression {
        name: "bad_size_observation".to_string(),
        expr: call_expr("My.badSizeRange", vec![]),
    }];

    let prepared = lower_dae_to_solve_model_owned_with_visible_expressions(dae_model, visible)
        .expect("array-literal size range should lower");

    assert!(
        prepared
            .visible_names
            .contains(&"bad_size_observation".to_string())
    );
}

#[test]
fn lower_skips_visible_observation_with_unbound_function_input() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.needsInput"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("My.needsInput"),
            def_id: None,
            inputs: vec![rumoca_core::FunctionParam::new("u", "Real")],
            outputs: vec![rumoca_core::FunctionParam::new("out", "Real")],
            locals: vec![],
            body: vec![rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: var("u"),
                span: rumoca_core::Span::DUMMY,
            }],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: Default::default(),
        },
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(der(var("x")), real_expr(0.0)),
        Default::default(),
        "derivative row",
    ));
    let visible = vec![VisibleExpression {
        name: "missing_function_arg".to_string(),
        expr: call_expr("My.needsInput", vec![]),
    }];

    let prepared = lower_dae_to_solve_model_owned_with_visible_expressions(dae_model, visible)
        .expect("unsupported visible function observation should not block solve lowering");

    assert!(
        !prepared
            .visible_names
            .contains(&"missing_function_arg".to_string())
    );
}

#[test]
fn lower_propagates_initial_equation_through_runtime_alias() {
    let mut dae_model = dae::Dae::default();
    let mut active = scalar_var("active");
    active.start = Some(real_expr(0.0));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("active"), active);
    let mut local_active = scalar_var("localActive");
    local_active.start = Some(real_expr(0.0));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("localActive"), local_active);
    let mut new_active = scalar_var("newActive");
    new_active.start = Some(real_expr(0.0));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("newActive"), new_active);
    let mut pre_new_active = scalar_var("__pre__.newActive");
    pre_new_active.start = Some(real_expr(0.0));
    pre_new_active.fixed = Some(true);
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("__pre__.newActive"),
        pre_new_active,
    );
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            sub(var("active"), real_expr(1.0)),
            Default::default(),
            "initial equation active = true",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("active"),
            var("localActive"),
            Default::default(),
            "runtime alias active = localActive",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("localActive"),
            var("__pre__.newActive"),
            Default::default(),
            "runtime alias localActive = pre(newActive)",
        ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("initial runtime alias propagation should lower");

    let Some(solve::ScalarSlot::P {
        index: active_index,
        ..
    }) = prepared.problem.layout.binding("active")
    else {
        panic!("active should be a runtime parameter");
    };
    let Some(solve::ScalarSlot::P {
        index: local_index, ..
    }) = prepared.problem.layout.binding("localActive")
    else {
        panic!("localActive should be a runtime parameter");
    };
    let Some(solve::ScalarSlot::P {
        index: new_index, ..
    }) = prepared.problem.layout.binding("newActive")
    else {
        panic!("newActive should be a runtime parameter");
    };
    let Some(solve::ScalarSlot::P {
        index: pre_new_index,
        ..
    }) = prepared.problem.layout.binding("__pre__.newActive")
    else {
        panic!("__pre__.newActive should be a runtime parameter");
    };
    assert_eq!(prepared.parameters[active_index], 1.0);
    assert_eq!(prepared.parameters[local_index], 1.0);
    assert_eq!(prepared.parameters[new_index], 1.0);
    assert_eq!(prepared.parameters[pre_new_index], 1.0);
}

#[test]
fn lower_seeds_current_value_from_lowered_pre_initial_equation() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("__pre__.active"),
        scalar_var("__pre__.active"),
    );
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("active"), scalar_var("active"));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            sub(var("__pre__.active"), real_expr(1.0)),
            Default::default(),
            "lowered initial equation pre(active) = true",
        ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("lowered pre initial value should seed current value");

    let Some(solve::ScalarSlot::P {
        index: pre_index, ..
    }) = prepared.problem.layout.binding("__pre__.active")
    else {
        panic!("__pre__.active should be a runtime parameter");
    };
    let Some(solve::ScalarSlot::P {
        index: active_index,
        ..
    }) = prepared.problem.layout.binding("active")
    else {
        panic!("active should be a runtime parameter");
    };
    assert_eq!(prepared.parameters[pre_index], 1.0);
    assert_eq!(prepared.parameters[active_index], 1.0);
}

#[test]
fn lower_applies_subscripted_discrete_initial_equations() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .symbols
        .enum_literal_ordinals
        .insert("Logic.X".to_string(), 2);
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("state"),
        dae::Variable {
            name: rumoca_core::VarName::new("state"),
            dims: vec![2],
            start: Some(int_expr(0)),
            ..Default::default()
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("__pre__.state"),
        dae::Variable {
            name: rumoca_core::VarName::new("__pre__.state"),
            dims: vec![2],
            fixed: Some(true),
            ..Default::default()
        },
    );
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            sub(indexed_var("state", 2), var("Logic.X")),
            Default::default(),
            "state[2] = Logic.X",
        ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("subscripted discrete initial equation should seed solve parameters");

    let Some(solve::ScalarSlot::P { index, .. }) = prepared.problem.layout.binding("state[2]")
    else {
        panic!("state[2] should be a runtime parameter");
    };
    let Some(solve::ScalarSlot::P {
        index: pre_index, ..
    }) = prepared.problem.layout.binding("__pre__.state[2]")
    else {
        panic!("__pre__.state[2] should be a runtime parameter");
    };
    assert_eq!(prepared.parameters[index], 2.0);
    assert_eq!(prepared.parameters[pre_index], 2.0);
}

#[test]
fn lower_evaluates_enum_literal_runtime_starts() {
    let mut dae_model = dae::Dae::default();
    dae_model.symbols.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
        1,
    );
    let mut active = scalar_var("active");
    active.start = Some(var("Modelica.Electrical.Digital.Interfaces.Logic.'U'"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("active"), active);

    let prepared = lower_dae_to_solve_model(&dae_model).expect("enum literal start should lower");

    let Some(solve::ScalarSlot::P { index, .. }) = prepared.problem.layout.binding("active") else {
        panic!("active should be a runtime parameter");
    };
    assert_eq!(prepared.parameters[index], 1.0);
}

#[test]
fn lower_broadcasts_enum_literal_start_to_array_runtime_starts() {
    let mut dae_model = dae::Dae::default();
    dae_model.symbols.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
        1,
    );
    let mut active = scalar_var("active");
    active.dims = vec![2];
    active.start = Some(rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(
            "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
        ),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    });
    let scalar_names = scalar_names("active", &active);
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("active"), active);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("enum literal scalar start should apply to every array element");

    for name in scalar_names {
        let Some(solve::ScalarSlot::P { index, .. }) = prepared.problem.layout.binding(&name)
        else {
            panic!("{name} should be a runtime parameter");
        };
        assert_eq!(prepared.parameters[index], 1.0);
    }
}

#[test]
fn lower_propagates_enum_parameter_binding_chain_to_runtime_start() {
    let mut dae_model = dae::Dae::default();
    dae_model.symbols.enum_literal_ordinals.extend([
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
            1,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
            3,
        ),
    ]);
    let mut root = scalar_var("Counter.q0");
    root.start = Some(var("Modelica.Electrical.Digital.Interfaces.Logic.'0'"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("Counter.q0"), root);

    let mut child = scalar_var("Counter.FF[1].q0");
    child.start = Some(var("Counter.q0"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("Counter.FF[1].q0"), child);

    let mut q = scalar_var("Counter.q[1]");
    q.start = Some(var("Counter.FF[1].q0"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("Counter.q[1]"), q);

    let prepared =
        lower_dae_to_solve_model(&dae_model).expect("enum parameter binding chain should lower");

    let Some(solve::ScalarSlot::P { index, .. }) = prepared.problem.layout.binding("Counter.q[1]")
    else {
        panic!("Counter.q[1] should be a runtime parameter");
    };
    assert_eq!(prepared.parameters[index], 3.0);
}

fn dynamic_external_time_table_dae() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    let mut table = scalar_var("table");
    table.dims = vec![4];
    table.start = Some(array_expr(
        vec![
            real_expr(1.0),
            real_expr(3.0),
            real_expr(5.0),
            real_expr(7.0),
        ],
        false,
    ));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("table"), table);

    let mut n = scalar_var("n");
    n.start = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![var("table"), int_expr(1)],
        span: rumoca_core::Span::DUMMY,
    });
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("n"), n);

    let mut table_dyn = scalar_var("table_dyn");
    table_dyn.dims = vec![0, 2];
    table_dyn.start = Some(dynamic_external_time_table_matrix());
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("table_dyn"), table_dyn);

    let mut table_id = scalar_var("table_id");
    table_id.start = Some(call_expr(
        "ExternalCombiTimeTable",
        vec![
            real_expr(0.0),
            real_expr(0.0),
            var("table_dyn"),
            real_expr(0.0),
            array_expr(vec![int_expr(2)], false),
            int_expr(3),
            int_expr(1),
        ],
    ));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("table_id"), table_id);
    dae_model
}

fn dynamic_boolean_time_table_dae() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    let mut table = scalar_var("table1.table");
    table.dims = vec![2];
    table.start = Some(array_expr(vec![real_expr(0.05), real_expr(0.15)], false));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("table1.table"), table);

    let mut n = scalar_var("table1.n");
    n.start = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![var("table1.table"), int_expr(1)],
        span: rumoca_core::Span::DUMMY,
    });
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("table1.n"), n);

    let mut table_dyn = scalar_var("table1.combiTimeTable.table");
    table_dyn.dims = vec![0, 2];
    table_dyn.start = Some(dynamic_boolean_time_table_matrix());
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("table1.combiTimeTable.table"),
        table_dyn,
    );

    let mut table_id = scalar_var("table1.combiTimeTable.tableID");
    table_id.start = Some(call_expr(
        "ExternalCombiTimeTable",
        vec![
            real_expr(0.0),
            real_expr(0.0),
            var("table1.combiTimeTable.table"),
            real_expr(0.0),
            array_expr(vec![int_expr(2)], false),
            int_expr(3),
            int_expr(1),
        ],
    ));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("table1.combiTimeTable.tableID"),
        table_id,
    );
    dae_model
}

fn dynamic_boolean_time_table_matrix() -> rumoca_core::Expression {
    let first_row = array_expr(vec![var("table1.table[1]"), real_expr(0.0)], true);
    rumoca_core::Expression::If {
        branches: vec![(
            rumoca_core::Expression::Binary {
                op: OpBinary::Gt,
                lhs: Box::new(var("table1.n")),
                rhs: Box::new(real_expr(0.0)),
                span: rumoca_core::Span::DUMMY,
            },
            array_expr(
                vec![
                    first_row,
                    array_expr(
                        vec![var("table1.table"), dynamic_external_time_table_toggles()],
                        true,
                    ),
                ],
                true,
            ),
        )],
        else_branch: Box::new(array_expr(
            vec![array_expr(vec![real_expr(0.0), real_expr(0.0)], false)],
            true,
        )),
        span: rumoca_core::Span::DUMMY,
    }
}

fn dynamic_external_time_table_matrix() -> rumoca_core::Expression {
    let first_row = array_expr(
        vec![
            rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("table").into(),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    1,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            },
            real_expr(0.0),
        ],
        false,
    );
    rumoca_core::Expression::If {
        branches: vec![(
            rumoca_core::Expression::Binary {
                op: OpBinary::Gt,
                lhs: Box::new(var("n")),
                rhs: Box::new(real_expr(0.0)),
                span: rumoca_core::Span::DUMMY,
            },
            array_expr(
                vec![
                    first_row,
                    array_expr(
                        vec![var("table"), dynamic_external_time_table_toggles()],
                        false,
                    ),
                ],
                true,
            ),
        )],
        else_branch: Box::new(array_expr(
            vec![array_expr(vec![real_expr(0.0), real_expr(0.0)], false)],
            true,
        )),
        span: rumoca_core::Span::DUMMY,
    }
}

fn dynamic_external_time_table_toggles() -> rumoca_core::Expression {
    rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Mod,
            args: vec![var("i"), real_expr(2.0)],
            span: rumoca_core::Span::DUMMY,
        }),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int_expr(1)),
                step: None,
                end: Box::new(var("n")),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn default_parameters_build_external_time_table_from_dynamic_matrix_start() {
    let dae_model = dynamic_external_time_table_dae();
    let runtime = Arc::new(EvalRuntimeState::default());
    let params = default_parameter_values(&dae_model, None, runtime.clone())
        .expect("parameter starts lower");
    assert_eq!(
        params.len(),
        6,
        "dynamic matrix parameter must not consume a flattened slot"
    );
    let table_id = *params.last().expect("table_id parameter");
    let env = build_runtime_parameter_tail_env_with_runtime(&dae_model, &params, 0.0, runtime);
    let tables = external_table_data_for_parameter_values_in(&env, &params);
    let next = rumoca_eval_dae::eval::eval_time_table_next_event_value_in(table_id, 0.0, &tables);
    let high = rumoca_eval_dae::eval::eval_table_lookup_value_in(table_id, 1.0, 2.0, &tables);

    assert!(table_id > 0.0);
    assert!((next - 1.0).abs() <= 1.0e-12, "next={next}");
    assert!((high - 1.0).abs() <= 1.0e-12, "high={high}");
}

#[test]
fn default_parameters_build_boolean_time_table_with_duplicate_first_knot() {
    let dae_model = dynamic_boolean_time_table_dae();
    let runtime = Arc::new(EvalRuntimeState::default());
    let params = default_parameter_values(&dae_model, None, runtime.clone())
        .expect("parameter starts lower");
    let table_id = *params.last().expect("table_id parameter");
    let env = build_runtime_parameter_tail_env_with_runtime(&dae_model, &params, 0.0, runtime);
    let tables = external_table_data_for_parameter_values_in(&env, &params);
    let before = rumoca_eval_dae::eval::eval_table_lookup_value_in(table_id, 1.0, 0.049, &tables);
    let after = rumoca_eval_dae::eval::eval_table_lookup_value_in(table_id, 1.0, 0.050001, &tables);

    assert!(table_id > 0.0);
    assert!((before - 0.0).abs() <= 1.0e-12, "before={before}");
    assert!((after - 1.0).abs() <= 1.0e-12, "after={after}");
}

#[test]
fn default_parameters_build_external_time_table_from_constructor_marked_call_with_strings() {
    let mut dae_model = dynamic_boolean_time_table_dae();
    let table_id = dae_model
        .variables
        .parameters
        .get_mut(&rumoca_core::VarName::new("table1.combiTimeTable.tableID"))
        .expect("table ID parameter");
    table_id.start = Some(constructor_call_expr(
        "Modelica.Blocks.Types.ExternalCombiTimeTable",
        vec![
            string_expr("NoName"),
            string_expr("NoName"),
            var("table1.combiTimeTable.table"),
            real_expr(0.0),
            array_expr(vec![int_expr(2)], false),
            int_expr(3),
            int_expr(1),
        ],
    ));

    let runtime = Arc::new(EvalRuntimeState::default());
    let params = default_parameter_values(&dae_model, None, runtime.clone())
        .expect("parameter starts lower");
    let table_id = *params.last().expect("table_id parameter");
    let env = build_runtime_parameter_tail_env_with_runtime(&dae_model, &params, 0.0, runtime);
    let tables = external_table_data_for_parameter_values_in(&env, &params);
    let after = rumoca_eval_dae::eval::eval_table_lookup_value_in(table_id, 1.0, 0.050001, &tables);

    assert!(table_id > 0.0);
    assert!((after - 1.0).abs() <= 1.0e-12, "after={after}");
}

#[test]
fn lower_applies_initial_random_distribution_chain_to_runtime_parameters() {
    let dae_model = initial_random_distribution_chain_model();
    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("Solve IR lowering should apply direct initial equations");
    let layout = &prepared.problem.solve_layout;
    let r_raw = prepared.parameters[layout
        .discrete_real_parameter_index("r_raw")
        .expect("r_raw runtime parameter")];
    let r = prepared.parameters[layout
        .discrete_real_parameter_index("r")
        .expect("r runtime parameter")];
    let state_1 = prepared.parameters[layout
        .discrete_valued_parameter_index("state[1]")
        .expect("state[1] runtime parameter")];

    assert!(r_raw > 0.0 && r_raw < 1.0, "r_raw={r_raw}");
    assert!(r.is_finite(), "r={r}");
    assert!(state_1 > 0.0, "state_1={state_1}");
}

fn initial_random_distribution_chain_model() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    for (name, value) in [
        ("localSeed", 0.0),
        ("actualGlobalSeed", 0.0),
        ("mu", 0.0),
        ("sigma", 2.0),
    ] {
        let mut parameter = scalar_var(name);
        parameter.start = Some(real_expr(value));
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), parameter);
    }
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("state"),
        dae::Variable {
            name: rumoca_core::VarName::new("state"),
            dims: vec![4],
            start: Some(int_expr(0)),
            ..Default::default()
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("__pre__.state"),
        dae::Variable {
            name: rumoca_core::VarName::new("__pre__.state"),
            dims: vec![4],
            start: Some(int_expr(0)),
            ..Default::default()
        },
    );
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("r_raw"), scalar_var("r_raw"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("r"), scalar_var("r"));

    insert_initial_random_distribution_chain_equations(&mut dae_model);
    dae_model
}

fn insert_initial_random_distribution_chain_equations(dae_model: &mut dae::Dae) {
    dae_model
        .initialization
        .equations
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("localSeed"),
            call_expr(
                "Modelica.Math.Random.Utilities.automaticLocalSeed",
                vec![call_expr("getInstanceName", Vec::new())],
            ),
            Default::default(),
            "localSeed = automaticLocalSeed(getInstanceName())",
        ));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            sub(
                var("__pre__.state"),
                call_expr(
                    "Modelica.Math.Random.Generators.Xorshift128plus.initialState",
                    vec![var("localSeed"), var("actualGlobalSeed")],
                ),
            ),
            Default::default(),
            // MLS Appendix B.2.2: initial equations may initialize discrete
            // random state through pre(state), already lowered to the
            // `__pre__.state` parameter array at the Solve boundary.
            "__pre__.state = initialState(localSeed, actualGlobalSeed)",
        ));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("r_raw"),
            call_expr(
                "Modelica.Math.Random.Generators.Xorshift128plus.random",
                vec![
                    var("__pre__.state"),
                    rumoca_core::Expression::BuiltinCall {
                        function: rumoca_core::BuiltinFunction::Size,
                        args: vec![var("__pre__.state"), int_expr(1)],
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
            ),
            Default::default(),
            "r_raw = random(__pre__.state, size(__pre__.state, 1))",
        ));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("r"),
            call_expr(
                "Modelica.Math.Distributions.Normal.quantile",
                vec![var("r_raw"), var("mu"), var("sigma")],
            ),
            Default::default(),
            "r = Normal.quantile(r_raw, mu, sigma)",
        ));
}
