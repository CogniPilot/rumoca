use super::*;

#[test]
fn solve_problem_marks_sample_event_indicator_closure_for_observation_refresh() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("pulse"), scalar_var("pulse"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("derived"), scalar_var("derived"));
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("pulse").into()),
        rhs: sample_event_indicator_expr(0.0, 0.5),
        span: Default::default(),
        // MLS §16.5.1: sample(start, interval) is an event indicator,
        // not a held clocked sampled value.
        origin: "pulse = sample(0, 0.5)".to_string(),
        scalar_count: 1,
    });
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("derived").into()),
        rhs: rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs: Box::new(var("pulse")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        origin: "derived = not pulse".to_string(),
        scalar_count: 1,
    });

    let problem =
        lower_solve_problem(&dae_model).expect("periodic pulse discrete rows should lower");

    assert_eq!(problem.discrete.observation_refresh, vec![true, true]);
}

#[test]
fn solve_problem_marks_runtime_assignment_dependent_discretes_for_observation_refresh() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("table_y"), scalar_var("table_y"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("b"), scalar_var("b"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c"), scalar_var("c"));
    dae_model.continuous.equations.push(dae::Equation::explicit(
        rumoca_core::VarName::new("table_y"),
        var("u"),
        Default::default(),
        "table_y = u",
    ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("b"),
            binary(
                rumoca_core::OpBinary::Ge,
                var("u"),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.5),
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            Default::default(),
            "b = u >= 0.5",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("c"),
            var("b"),
            Default::default(),
            "c = b",
        ));

    let problem =
        lower_solve_problem(&dae_model).expect("runtime assignment dependent f_m should lower");

    // MLS Appendix B: a discrete-valued equation that reads a runtime-tail
    // assignment source belongs to the same settled observation system as
    // that source, with aliases refreshed by dependency closure.
    assert_eq!(problem.discrete.observation_refresh, vec![true, true]);
}

#[test]
fn solve_problem_marks_event_relation_aliases_for_observation_refresh() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("table_u"), scalar_var("table_u"));
    for name in [
        "to_boolean.y",
        "table.y",
        "sample.u",
        "sample.clock",
        "sample.y",
    ] {
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("to_boolean.y"),
            binary(
                rumoca_core::OpBinary::Ge,
                var("table_u"),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.5),
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            Default::default(),
            "to_boolean.y = table_u >= 0.5",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("table.y"),
            var("to_boolean.y"),
            Default::default(),
            "table.y = to_boolean.y",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("sample.u"),
            var("table.y"),
            Default::default(),
            "sample.u = table.y",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("sample.y"),
            internal_sample_call(vec![var("sample.u"), var("sample.clock")]),
            Default::default(),
            "sample.y = sample(sample.u, sample.clock)",
        ));

    let problem = lower_solve_problem(&dae_model).expect("event relation aliases should lower");
    let refresh_for = |name: &str| {
        let slot = problem
            .layout
            .binding(name)
            .unwrap_or_else(|| panic!("{name} should have a solve slot"));
        let row_idx = problem
            .discrete
            .update_targets
            .iter()
            .position(|target| *target == slot)
            .unwrap_or_else(|| panic!("{name} should have an update row"));
        problem.discrete.observation_refresh[row_idx]
    };

    assert!(refresh_for("to_boolean.y"));
    assert!(refresh_for("table.y"));
    assert!(refresh_for("sample.u"));
    assert!(!refresh_for("sample.y"));
}

#[test]
fn solve_problem_does_not_observation_refresh_no_event_relation_aliases() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("b"), scalar_var("b"));
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("b").into()),
        rhs: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::NoEvent,
            args: vec![binary(
                rumoca_core::OpBinary::Ge,
                var("u"),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
            )],
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        // MLS §3.7.4: noEvent takes Real elementary relations literally,
        // so this relation must not seed event observation refresh.
        origin: "b = noEvent(u >= 0)".to_string(),
        scalar_count: 1,
    });

    let problem = lower_solve_problem(&dae_model).expect("noEvent relation alias should lower");

    assert_eq!(problem.discrete.observation_refresh, vec![false]);
}

#[test]
fn solve_problem_marks_derived_clock_constructor_for_observation_refresh() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("clock_active"),
        scalar_var("clock_active"),
    );
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("clock_active").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("subSample").into(),
            args: vec![
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Clock").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(20),
                        span: rumoca_core::Span::DUMMY,
                    }],
                    is_constructor: false,

                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1000),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        // MLS §16.5.2: subSample(Clock(...), factor) constructs a clock.
        // The resulting event indicator is active only at ticks; it is not
        // a held sampled value.
        origin: "clock_active = subSample(Clock(20), 1000)".to_string(),
        scalar_count: 1,
    });

    let problem = lower_solve_problem(&dae_model).expect("derived clock constructor should lower");

    assert_eq!(problem.discrete.observation_refresh, vec![true]);
}

#[test]
fn solve_problem_marks_hold_dependencies_for_observation_refresh() {
    let mut dae_model = dae::Dae::default();
    for name in ["pulse", "hold.u", "hold.y"] {
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("pulse").into()),
        rhs: sample_event_indicator_expr(0.0, 0.5),
        span: Default::default(),
        origin: "pulse = sample(0, 0.5)".to_string(),
        scalar_count: 1,
    });
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("hold.u"),
            var("pulse"),
            Default::default(),
            "hold.u = pulse",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("hold.y"),
            function_call("hold", vec![var("hold.u")]),
            Default::default(),
            "hold.y = hold(hold.u)",
        ));

    let problem = lower_solve_problem(&dae_model).expect("hold observation refresh should lower");

    // MLS §16.5.1: hold() is a continuous-time zero-order hold. It is safe to
    // refresh for observations once its source clocked value has settled.
    assert_eq!(problem.discrete.observation_refresh, vec![true, true, true]);
}

#[test]
fn solve_problem_does_not_observation_refresh_clocked_previous_rows() {
    let mut dae_model = dae::Dae::default();
    for name in ["b_super", "u_super", "y"] {
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.clocks.intervals.insert("y".to_string(), 0.1);
    insert_pre_parameter(&mut dae_model, "b_super");
    insert_pre_parameter(&mut dae_model, "y");
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::If {
            branches: vec![(
                sample_event_indicator_expr(0.0, 0.1),
                rumoca_core::Expression::If {
                    branches: vec![(
                        binary(
                            rumoca_core::OpBinary::Neq,
                            var("b_super"),
                            pre_var("b_super"),
                        ),
                        var("u_super"),
                    )],
                    else_branch: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Boolean(false),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                },
            )],
            else_branch: Box::new(pre_var("y")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        // MLS section 16.4: lowered previous(..) on a clocked row needs the event-entry
        // clock history. Observation refresh does not own that history snapshot.
        origin: "y = if b_super <> previous(b_super) then u_super else false".to_string(),
        scalar_count: 1,
    });

    let problem = lower_solve_problem(&dae_model).expect("clocked previous row should lower");

    assert_eq!(problem.discrete.observation_refresh, vec![false]);
}

#[test]
fn solve_problem_does_not_observation_refresh_internal_sample_value_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("clock"), scalar_var("clock"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("sampled"), scalar_var("sampled"));
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("sampled").into()),
        rhs: function_call(
            rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME,
            vec![var("u"), var("clock")],
        ),
        span: Default::default(),
        origin: "sampled = sample(u, clock)".to_string(),
        scalar_count: 1,
    });

    let problem =
        lower_solve_problem(&dae_model).expect("internal sample value-form rows should lower");

    assert_eq!(problem.discrete.observation_refresh, vec![false]);
}

#[test]
fn solve_problem_recovers_discrete_target_from_conditional_residual_update_row() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("z"), scalar_var("z"));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::If {
                branches: vec![(
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Boolean(true),
                        span: rumoca_core::Span::DUMMY,
                    },
                    binary(
                        rumoca_core::OpBinary::Sub,
                        var("z"),
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(2.0),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ),
                )],
                else_branch: Box::new(binary(
                    rumoca_core::OpBinary::Sub,
                    var("z"),
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(3.0),
                        span: rumoca_core::Span::DUMMY,
                    },
                )),
                span: rumoca_core::Span::DUMMY,
            },
            Default::default(),
            // MLS §8.3.4: if-equations preserve branch equation semantics, so
            // residual branches assigning one target lower to one update row.
            "conditional residual discrete update",
        ));

    let problem =
        lower_solve_problem(&dae_model).expect("conditional residual discrete update should lower");

    assert_eq!(problem.discrete.rhs.programs.len(), 1);
    assert_eq!(problem.discrete.update_targets.len(), 1);
    assert!(matches!(
        problem.discrete.update_targets[0],
        solve::ScalarSlot::P { index: 0, .. }
    ));
}

#[test]
fn solve_problem_orients_discrete_aliases_away_from_updated_array_target() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            name: rumoca_core::VarName::new("y"),
            component_ref: Some(test_component_ref_from_name("y")),
            dims: vec![2],
            ..Default::default()
        },
    );
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("u1"), scalar_var("u1"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("u2"), scalar_var("u2"));
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y[1]").into()),
        rhs: var("u1"),
        span: Default::default(),
        // MLS §8.3: alias equations are equations, not ordered writes.
        // When another row defines y[1], orient this alias toward u1.
        origin: "alias y[1] = u1".to_string(),
        scalar_count: 1,
    });
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(2.0),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        origin: "guarded update to y".to_string(),
        scalar_count: 2,
    });
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y[2]").into()),
        rhs: var("u2"),
        span: Default::default(),
        origin: "alias y[2] = u2".to_string(),
        scalar_count: 1,
    });

    let problem = lower_solve_problem(&dae_model)
        .expect("discrete aliases should orient away from updated target");
    let indices = problem
        .discrete
        .update_targets
        .iter()
        .filter_map(|slot| match slot {
            solve::ScalarSlot::P { index, .. } => Some(*index),
            _ => None,
        })
        .collect::<Vec<_>>();
    let unique = indices
        .iter()
        .copied()
        .collect::<std::collections::HashSet<_>>();

    assert_eq!(indices.len(), 4);
    assert_eq!(unique.len(), 4);
}

#[test]
fn solve_problem_orients_residual_aliases_away_from_residual_update_target() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model.clocks.intervals.insert("y".to_string(), 0.1);
    insert_pre_parameter(&mut dae_model, "u");
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            binary(rumoca_core::OpBinary::Sub, var("y"), pre_var("u")),
            Default::default(),
            "residual update y = previous(u)",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            binary(rumoca_core::OpBinary::Sub, var("y"), var("u")),
            Default::default(),
            // MLS §8.3: alias residuals are equations. Since y is defined by
            // another update row, solve-lower orients this alias toward u.
            "residual alias y = u",
        ));

    let problem = lower_solve_problem(&dae_model)
        .expect("residual alias should orient away from residual update target");
    let indices = problem
        .discrete
        .update_targets
        .iter()
        .filter_map(|slot| match slot {
            solve::ScalarSlot::P { index, .. } => Some(*index),
            _ => None,
        })
        .collect::<Vec<_>>();
    let unique = indices
        .iter()
        .copied()
        .collect::<std::collections::HashSet<_>>();

    assert_eq!(indices.len(), 2);
    assert_eq!(unique.len(), 2);
    assert!(
        problem
            .discrete
            .pre_modes
            .iter()
            .any(|mode| matches!(mode, solve::DiscreteEventPreMode::EventEntry)),
        "MLS §16.4 previous(..) rows must keep tick left-limit pre values fixed"
    );
}

#[test]
fn solve_problem_orients_plain_aliases_to_keep_discrete_targets_unique() {
    let mut dae_model = dae::Dae::default();
    for name in ["a", "b", "c"] {
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("a").into()),
        rhs: var("b"),
        span: Default::default(),
        // MLS §8.3: alias equations define equality constraints. Solve-lower
        // may orient them as a single-writer update graph, but must not
        // produce duplicate update targets for the same variable.
        origin: "alias a = b".to_string(),
        scalar_count: 1,
    });
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("a").into()),
        rhs: var("c"),
        span: Default::default(),
        origin: "alias a = c".to_string(),
        scalar_count: 1,
    });

    let problem =
        lower_solve_problem(&dae_model).expect("plain aliases should lower without duplicates");
    let indices = problem
        .discrete
        .update_targets
        .iter()
        .filter_map(|slot| match slot {
            solve::ScalarSlot::P { index, .. } => Some(*index),
            _ => None,
        })
        .collect::<Vec<_>>();
    let unique = indices
        .iter()
        .copied()
        .collect::<std::collections::HashSet<_>>();

    assert_eq!(indices.len(), 2);
    assert_eq!(unique.len(), 2);
}

#[test]
fn build_var_layout_populates_shapes_for_all_array_variable_categories() {
    // Phase 2.7 audit: `build_var_layout` must record shapes for every
    // array variable category so that `VarLayout::y_slice` / `p_slice`
    // can construct valid `TensorSource` values without panicking.
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), array_var("x", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("y"), array_var("y", &[2, 2]));
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("w"), array_var("w", &[4]));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("A"), array_var("A", &[3, 3]));
    dae_model
        .variables
        .inputs
        .insert(rumoca_core::VarName::new("u"), array_var("u", &[2]));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("z"), array_var("z", &[3]));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");

    // Y-slot variables
    assert!(
        layout.y_slice("x").is_some(),
        "state array should have a recorded shape"
    );
    assert!(
        layout.y_slice("y").is_some(),
        "algebraic array should have a recorded shape"
    );
    assert!(
        layout.y_slice("w").is_some(),
        "output array should have a recorded shape"
    );

    // P-slot variables
    assert!(
        layout.p_slice("A").is_some(),
        "parameter array should have a recorded shape"
    );
    assert!(
        layout.p_slice("u").is_some(),
        "input array should have a recorded shape"
    );
    assert!(
        layout.p_slice("z").is_some(),
        "discrete_real array should have a recorded shape"
    );
}
