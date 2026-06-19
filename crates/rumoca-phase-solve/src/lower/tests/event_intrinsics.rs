use super::*;

#[test]
fn discrete_history_update_from_input_keeps_state_as_target() {
    // A sampled controller's end-of-step history update `prev_x := x`, where `x`
    // is an external input, must orient with the discrete state as the write
    // target. Reorienting it to `x := prev_x` would make the read-only input an
    // assignment target and clobber the caller's per-tick input every step,
    // leaving the controller blind to its live pose. Regression for the
    // discrete-alias orientation that ignored input causality.
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .inputs
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("prev_x"), scalar_var("prev_x"));
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("prev_x").into()),
        rhs: rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("x").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        span: lower_test_span(),
        origin: "prev_x := x history update".to_string(),
        scalar_count: 1,
    });

    let equations = crate::lower::normalized_discrete_update_equations(&dae_model)
        .expect("history update should lower");

    assert_eq!(equations.len(), 1, "expected a single oriented update");
    let eq = &equations[0];
    assert_eq!(
        eq.lhs.as_ref().map(|lhs| lhs.var_name().as_str()),
        Some("prev_x"),
        "discrete state (not the input) must be the update target"
    );
    let rumoca_core::Expression::VarRef { name, .. } = &eq.rhs else {
        panic!("history update RHS should read the input directly: {:?}", eq.rhs);
    };
    assert_eq!(
        name.var_name().as_str(),
        "x",
        "history update must read the live input on the RHS"
    );
}

#[test]
fn lower_discrete_rhs_keeps_edge_initial_as_runtime_event_flag() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("y"), source_scalar_var("y"));
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(source_ref("y")),
        rhs: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Edge,
            args: vec![rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Initial,
                args: vec![],
                span: lower_test_span(),
            }],
            span: lower_test_span(),
        },
        span: lower_test_span(),
        origin: "edge(initial()) event assignment".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("edge(initial()) should lower");
    let Some(ScalarSlot::P {
        index: initial_index,
        ..
    }) = layout.binding(crate::layout::INITIAL_EVENT_PARAMETER_NAME)
    else {
        panic!("initial event flag should be represented in the solve layout");
    };
    let mut p = vec![0.0; layout.p_scalars()];
    p[initial_index] = 1.0;
    let (_, startup) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    p[initial_index] = 0.0;
    let (_, ordinary_event) = eval_linear_ops(&rows[0], &[], &p, 0.0);

    assert_eq!(startup.expect("startup output"), 1.0);
    assert_eq!(ordinary_event.expect("ordinary event output"), 0.0);
}

#[test]
fn lower_discrete_rhs_uses_pre_relation_memory_for_edge_previous_value() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("u"), source_scalar_var("u"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c"), source_scalar_var("c"));
    insert_pre_parameter(&mut dae_model, "c", &[]);
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("hit"), source_scalar_var("hit"));
    let relation = binary(
        rumoca_core::OpBinary::Gt,
        var("u"),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span: lower_test_span(),
        },
    );
    dae_model.conditions.relations.push(relation.clone());
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(source_ref("hit")),
        rhs: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Edge,
            args: vec![relation.clone()],
            span: lower_test_span(),
        },
        span: lower_test_span(),
        origin: "edge relation memory update".to_string(),
        scalar_count: 1,
    });
    dae_model.conditions.equations.push(dae::Equation {
        lhs: Some(source_ref("c")),
        rhs: relation,
        span: lower_test_span(),
        origin: "relation memory update".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let Some(ScalarSlot::P {
        index: pre_c_index, ..
    }) = layout.binding("__pre__.c")
    else {
        panic!("relation memory should be stored in the parameter/runtime tail");
    };
    let Some(ScalarSlot::P {
        index: initial_index,
        ..
    }) = layout.binding(crate::layout::INITIAL_EVENT_PARAMETER_NAME)
    else {
        panic!("initial event flag should be represented in the solve layout");
    };
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("edge(relation) should lower");

    assert!(rows.iter().flatten().any(|op| matches!(
        op,
        LinearOp::LoadP { index, .. } if *index == pre_c_index
    )));
    assert!(rows.iter().flatten().any(|op| matches!(
        op,
        LinearOp::LoadP { index, .. } if *index == initial_index
    )));
}
