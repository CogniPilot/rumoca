use rumoca_core::Span;

use super::*;

mod clock_alias_tests;
mod dynamic_clock_tests;

fn populate_conditions(dae_model: &mut dae::Dae) {
    crate::condition_lowering::populate_canonical_conditions(dae_model);
}

fn time_gt(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("time").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    }
}

fn lit(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn clock_call(interval: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Clock").into(),
        args: vec![lit(interval)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn condition_memory_ref(name: &str, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            index,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    }
}

fn embedded_condition_memory_ref(name: &str, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(format!("{name}[{index}]")).into(),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn sub(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn dae_with_if_condition(cond: rumoca_core::Expression) -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable::new(rumoca_core::VarName::new("x")),
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::If {
            branches: vec![(cond, lit(1.0))],
            else_branch: Box::new(lit(0.0)),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "test_if_condition",
    ));
    dae_model
}

fn less_than_with_token(_token_number: u32) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(var("leg_h")),
        rhs: Box::new(var("ground_z")),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn test_runtime_precompute_collects_event_without_synthetic_root_for_time_if_condition() {
    let mut dae_model = dae::Dae::default();
    let cond = time_gt(5.0);
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::If {
            branches: vec![(
                cond.clone(),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span: rumoca_core::Span::DUMMY,
                },
            )],
            else_branch: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "test_if_condition",
    ));

    populate_conditions(&mut dae_model);
    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert!(
        dae_model
            .events
            .synthetic_root_conditions
            .iter()
            .all(|expr| format!("{expr:?}") != format!("{cond:?}")),
        "time-only branch conditions should be scheduled events, not synthetic roots"
    );
    assert!(
        dae_model
            .events
            .scheduled_time_events
            .iter()
            .any(|time| (*time - 5.0).abs() <= 1.0e-12),
        "expected precompute to capture scheduled event at t=5"
    );
}

#[test]
fn test_runtime_precompute_interns_and_orders_root_and_time_event_metadata() {
    let root_cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(0.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let mut dae_model = dae_with_if_condition(root_cond.clone());

    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Gt,
                    lhs: Box::new(var("x")),
                    rhs: Box::new(lit(0.0)),
                    span: rumoca_core::Span::DUMMY,
                },
                lit(2.0),
            )],
            else_branch: Box::new(lit(-1.0)),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "duplicate_root_condition",
    ));

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::If {
                branches: vec![(time_gt(1.5), lit(1.0))],
                else_branch: Box::new(lit(0.0)),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "time_event_late",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::If {
                branches: vec![(time_gt(0.5), lit(1.0))],
                else_branch: Box::new(lit(0.0)),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "time_event_early",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::If {
                branches: vec![(time_gt(1.5), lit(2.0))],
                else_branch: Box::new(lit(0.0)),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "time_event_late_duplicate",
        ));

    populate_conditions(&mut dae_model);
    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(
        dae_model.conditions.relations.len(),
        1,
        "canonical condition relations should be unique semantic surfaces"
    );
    assert!(
        dae_model.events.synthetic_root_conditions.is_empty(),
        "Appendix B relations must not be duplicated as synthetic roots"
    );
    assert_eq!(
        dae_model.events.scheduled_time_events,
        vec![0.5, 1.5],
        "scheduled time events should be canonicalized and sorted"
    );
}

#[test]
fn test_canonical_conditions_intern_equivalent_roots_with_different_source_tokens() {
    let mut dae_model = dae::Dae::default();
    for token_number in 1..=3 {
        dae_model.continuous.equations.push(dae::Equation::residual(
            rumoca_core::Expression::If {
                branches: vec![(less_than_with_token(token_number), lit(token_number as f64))],
                else_branch: Box::new(lit(0.0)),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            format!("duplicate_contact_condition_{token_number}"),
        ));
    }

    populate_conditions(&mut dae_model);
    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(
        dae_model.conditions.relations.len(),
        1,
        "MLS Appendix B relation roots should be unique semantic surfaces, not one entry per source token"
    );
    assert!(
        dae_model.events.synthetic_root_conditions.is_empty(),
        "relation roots are owned by f_c/relation, not synthetic_root_conditions"
    );
}

#[test]
fn test_runtime_precompute_uses_relation_root_instead_of_duplicate_synthetic_root() {
    let condition = less_than_with_token(1);
    let mut dae_model = dae_with_if_condition(condition.clone());
    dae_model.conditions.relations = vec![condition.clone()];
    dae_model.conditions.equations = vec![dae::Equation::explicit(
        rumoca_core::VarName::new("c[1]"),
        condition,
        Span::DUMMY,
        "condition equation from test",
    )];

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(
        dae_model.conditions.relations.len(),
        1,
        "Appendix B relations remain the canonical event roots"
    );
    assert!(
        dae_model.events.synthetic_root_conditions.is_empty(),
        "a branch relation already present in the condition partition must not be emitted as a second solver root"
    );
}

#[test]
fn test_runtime_precompute_skips_noevent_wrapped_conditions_for_events() {
    let mut dae_model = dae::Dae::default();
    let cond = time_gt(2.0);
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::NoEvent,
            args: vec![rumoca_core::Expression::If {
                branches: vec![(
                    cond,
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(1.0),
                        span: rumoca_core::Span::DUMMY,
                    },
                )],
                else_branch: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "test_noevent",
    ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert!(
        dae_model.events.scheduled_time_events.is_empty(),
        "noEvent-wrapped conditions should not produce scheduled event times"
    );
}

#[test]
fn test_runtime_precompute_skips_time_vs_parameter_synthetic_roots() {
    let cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(var("time")),
        rhs: Box::new(var("switch_time")),
        span: rumoca_core::Span::DUMMY,
    };
    let mut dae_model = dae_with_if_condition(cond.clone());
    dae_model.conditions.relations = vec![cond.clone()];
    dae_model.conditions.equations = vec![dae::Equation::explicit(
        rumoca_core::VarName::new("c[1]"),
        cond.clone(),
        Span::DUMMY,
        "condition equation from test".to_string(),
    )];
    let mut switch_time = dae::Variable::new(rumoca_core::VarName::new("switch_time"));
    switch_time.start = Some(lit(2.5));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("switch_time"), switch_time);

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert!(
        dae_model
            .events
            .synthetic_root_conditions
            .iter()
            .all(|expr| format!("{expr:?}") != format!("{cond:?}")),
        "time-vs-parameter branch conditions should be scheduled time events, not synthetic roots"
    );
    assert_eq!(dae_model.events.scheduled_time_events, vec![2.5]);
    assert!(
        dae_model.conditions.relations.is_empty(),
        "time-vs-parameter conditions should be represented as scheduled events, not solver roots"
    );
    assert!(
        dae_model.conditions.equations.is_empty(),
        "condition equations must stay aligned with pruned time-only relation roots"
    );
}

#[test]
fn test_runtime_precompute_renumbers_condition_partition_after_prune() {
    let time_only = time_gt(2.5);
    let root_cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(0.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let mut dae_model = dae_with_if_condition(root_cond.clone());
    dae_model.conditions.relations = vec![time_only.clone(), root_cond.clone()];
    dae_model.conditions.equations = vec![
        dae::Equation::explicit(
            rumoca_core::VarName::new("c[1]"),
            time_only,
            Span::DUMMY,
            "condition equation from test",
        ),
        dae::Equation::explicit(
            rumoca_core::VarName::new("c[2]"),
            root_cond.clone(),
            Span::DUMMY,
            "condition equation from test",
        ),
    ];

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(dae_model.conditions.relations, vec![root_cond.clone()]);
    assert_eq!(dae_model.conditions.equations.len(), 1);
    assert_eq!(
        dae_model.conditions.equations[0].lhs.as_ref(),
        Some(&rumoca_core::VarName::new("c[1]")),
        "surviving condition equations must be renumbered after pruning"
    );
    assert_eq!(dae_model.conditions.equations[0].rhs, root_cond);
}

#[test]
fn test_runtime_precompute_reindexes_existing_condition_memory_after_prune() {
    let time_only = time_gt(2.5);
    let root_cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(0.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let mut dae_model = dae_with_if_condition(root_cond.clone());
    dae_model.conditions.relations = vec![time_only.clone(), root_cond.clone()];
    dae_model.conditions.equations = vec![
        dae::Equation::explicit(
            rumoca_core::VarName::new("c[1]"),
            time_only,
            Span::DUMMY,
            "condition equation from test",
        ),
        dae::Equation::explicit(
            rumoca_core::VarName::new("c[2]"),
            root_cond.clone(),
            Span::DUMMY,
            "condition equation from test",
        ),
    ];
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("flag"),
            rumoca_core::Expression::If {
                branches: vec![(
                    embedded_condition_memory_ref("c", 2),
                    condition_memory_ref("__pre__.c", 2),
                )],
                else_branch: Box::new(var("flag")),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "pre-lowered discrete update",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert!(
        matches!(
            &dae_model.discrete.valued_updates[0].rhs,
            rumoca_core::Expression::If { branches, .. }
                if matches!(
                    &branches[0],
                    (
                        rumoca_core::Expression::VarRef {
                            name: condition_name,
                            subscripts: condition_subscripts,
                            ..
                        },
                        rumoca_core::Expression::VarRef {
                            name: pre_name,
                            subscripts: pre_subscripts,
                            ..
                        },
                    ) if condition_name.as_str() == "c"
                        && pre_name.as_str() == "__pre__.c"
                        && condition_subscripts
                            == &[rumoca_core::Subscript::generated_index(
                                1,
                                rumoca_core::Span::DUMMY
                            )]
                        && pre_subscripts
                            == &[rumoca_core::Subscript::generated_index(
                                1,
                                rumoca_core::Span::DUMMY
                            )]
                )
        ),
        "discrete updates that already reference condition memory must track pruned f_c indices"
    );
}

#[test]
fn test_runtime_precompute_reindexes_nested_condition_memory_in_fc_after_prune() {
    let time_only = time_gt(2.5);
    let root_cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(0.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let nested_cond = condition_memory_ref("c", 2);
    let mut dae_model = dae_with_if_condition(root_cond.clone());
    dae_model.conditions.relations = vec![time_only.clone(), root_cond, nested_cond];
    dae_model.conditions.equations = vec![
        dae::Equation::explicit(
            rumoca_core::VarName::new("c[1]"),
            time_only,
            Span::DUMMY,
            "condition equation from test",
        ),
        dae::Equation::explicit(
            rumoca_core::VarName::new("c[2]"),
            dae_model.conditions.relations[1].clone(),
            Span::DUMMY,
            "condition equation from test",
        ),
        dae::Equation::explicit(
            rumoca_core::VarName::new("c[3]"),
            dae_model.conditions.relations[2].clone(),
            Span::DUMMY,
            "condition equation from nested condition",
        ),
    ];

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert!(
        matches!(
            &dae_model.conditions.relations[1],
            rumoca_core::Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "c"
                    && subscripts == &[rumoca_core::Subscript::generated_index(
                        1,
                        rumoca_core::Span::DUMMY
                    )]
        ),
        "nested condition-memory references inside f_c/relation must be reindexed"
    );
    assert_eq!(
        dae_model.conditions.equations[1].rhs, dae_model.conditions.relations[1],
        "f_c and relation must stay aligned after condition-memory reindexing"
    );
}

#[test]
fn test_runtime_precompute_keeps_time_vs_state_synthetic_roots() {
    let cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(var("time")),
        rhs: Box::new(var("x")),
        span: rumoca_core::Span::DUMMY,
    };
    let mut dae_model = dae_with_if_condition(cond.clone());
    populate_conditions(&mut dae_model);
    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert!(
        dae_model
            .conditions
            .relations
            .iter()
            .any(|expr| rumoca_core::expressions_semantically_equal(expr, &cond)),
        "time-vs-state branch conditions should remain canonical relations"
    );
    assert!(
        dae_model.events.synthetic_root_conditions.is_empty(),
        "canonical relations should not be duplicated as synthetic roots"
    );
    assert!(
        dae_model.events.scheduled_time_events.is_empty(),
        "time-vs-state branch conditions should not be scheduled as static events"
    );
}

#[test]
fn test_runtime_precompute_extracts_affine_time_event() {
    let cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Le,
        lhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var("time")),
            rhs: Box::new(var("delay")),
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(var("switch_at")),
        span: rumoca_core::Span::DUMMY,
    };
    let mut dae_model = dae_with_if_condition(cond);
    let mut delay = dae::Variable::new(rumoca_core::VarName::new("delay"));
    delay.start = Some(lit(0.25));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("delay"), delay);
    let mut switch_at = dae::Variable::new(rumoca_core::VarName::new("switch_at"));
    switch_at.start = Some(lit(1.5));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("switch_at"), switch_at);

    populate_conditions(&mut dae_model);
    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert_eq!(dae_model.events.scheduled_time_events.len(), 1);
    assert!((dae_model.events.scheduled_time_events[0] - 1.25).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_extracts_discrete_partition_events() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("c"),
        dae::Variable::new(rumoca_core::VarName::new("c")),
    );
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            sub(
                var("c"),
                rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Gt,
                    lhs: Box::new(var("time")),
                    rhs: Box::new(lit(0.5)),
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            Span::DUMMY,
            "test_discrete_partition",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert_eq!(dae_model.events.scheduled_time_events, vec![0.5]);
}

#[test]
fn test_runtime_precompute_collects_clock_constructor_exprs() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("s"),
        dae::Variable::new(rumoca_core::VarName::new("s")),
    );
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("s").into(),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sample,
                    args: vec![
                        rumoca_core::Expression::VarRef {
                            name: rumoca_core::VarName::new("u").into(),
                            subscripts: vec![],
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("Clock").into(),
                            args: vec![rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Real(0.1),
                                span: rumoca_core::Span::DUMMY,
                            }],
                            is_constructor: false,
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "test_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(dae_model.clocks.constructor_exprs.len(), 1);
    assert_eq!(dae_model.clocks.schedules.len(), 1);
    assert!((dae_model.clocks.schedules[0].period_seconds - 0.1).abs() <= 1e-12);
    assert!(dae_model.clocks.schedules[0].phase_seconds.abs() <= 1e-12);
    assert!((dae_model.clocks.intervals["s"] - 0.1).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_collects_sample_start_interval_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("s"),
        dae::Variable::new(rumoca_core::VarName::new("s")),
    );
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            sub(
                var("s"),
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sample,
                    args: vec![
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(0.2),
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(0.1),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            Span::DUMMY,
            // MLS §16.5.1: sample(start, interval) defines a periodic event.
            "periodic_sample_start_interval",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(dae_model.clocks.schedules.len(), 1);
    assert!((dae_model.clocks.schedules[0].period_seconds - 0.1).abs() <= 1e-12);
    assert!((dae_model.clocks.schedules[0].phase_seconds - 0.2).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_assigns_implicit_sample_interval_from_unique_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("simTime"),
        dae::Variable::new(rumoca_core::VarName::new("simTime")),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("clockY"),
        dae::Variable::new(rumoca_core::VarName::new("clockY")),
    );

    // simTime = sample(time) (implicit clock sample form)
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            sub(
                var("simTime"),
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sample,
                    args: vec![var("time")],
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            Span::DUMMY,
            "implicit_clocked_sample",
        ));

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            sub(
                var("clockY"),
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Clock").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(0.1),
                        span: rumoca_core::Span::DUMMY,
                    }],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            Span::DUMMY,
            "periodic_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert_eq!(dae_model.clocks.schedules.len(), 1);
    assert!((dae_model.clocks.intervals["simTime"] - 0.1).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_assigns_clock_interval_to_algebraic_alias_chain() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.inputs.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable::new(rumoca_core::VarName::new("u")),
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("feedback.y"),
        dae::Variable::new(rumoca_core::VarName::new("feedback.y")),
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("PI.u"),
        dae::Variable::new(rumoca_core::VarName::new("PI.u")),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("sample2.y"),
        dae::Variable::new(rumoca_core::VarName::new("sample2.y")),
    );
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("sample2.clock"),
        dae::Variable::new(rumoca_core::VarName::new("sample2.clock")),
    );

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("sample2.clock"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Clock").into(),
                args: vec![rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.1),
                    span: rumoca_core::Span::DUMMY,
                }],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "explicit_clock_alias",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("sample2.y"),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args: vec![var("u"), var("sample2.clock")],
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "explicit_sample_value",
        ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("sample2.y")),
            rhs: Box::new(var("feedback.y")),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "sample_alias",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("feedback.y")),
            rhs: Box::new(var("PI.u")),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "controller_input_alias",
    ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert!((dae_model.clocks.intervals["sample2.clock"] - 0.1).abs() <= 1e-12);
    assert!((dae_model.clocks.intervals["sample2.y"] - 0.1).abs() <= 1e-12);
    assert!((dae_model.clocks.intervals["feedback.y"] - 0.1).abs() <= 1e-12);
    assert!((dae_model.clocks.intervals["PI.u"] - 0.1).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_does_not_assign_fallback_interval_for_non_sample_clock_ops() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("b"),
        dae::Variable::new(rumoca_core::VarName::new("b")),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("clockY"),
        dae::Variable::new(rumoca_core::VarName::new("clockY")),
    );

    // b = pre(b) is discrete/event logic, not an implicit sample(..) form.
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("b")),
                rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Pre,
                    args: vec![var("b")],
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "pre_based_discrete_update",
        ));

    // Add one static periodic schedule in the model.
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("clockY")),
                rhs: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Clock").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(0.1),
                        span: rumoca_core::Span::DUMMY,
                    }],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "periodic_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert_eq!(dae_model.clocks.schedules.len(), 1);
    assert!(
        !dae_model.clocks.intervals.contains_key("b"),
        "fallback interval must only apply to implicit sample(..) sources",
    );
}

#[test]
fn test_runtime_precompute_extracts_shifted_clock_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("b").into(),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("shiftSample").into(),
                    args: vec![
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("Clock").into(),
                            args: vec![rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Real(0.2),
                                span: rumoca_core::Span::DUMMY,
                            }],
                            is_constructor: false,
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "test_shifted_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert_eq!(dae_model.clocks.constructor_exprs.len(), 2);
    assert_eq!(dae_model.clocks.schedules.len(), 2);

    let has_base = dae_model.clocks.schedules.iter().any(|sched| {
        (sched.period_seconds - 0.2).abs() <= 1e-12 && sched.phase_seconds.abs() <= 1e-12
    });
    let has_shifted = dae_model.clocks.schedules.iter().any(|sched| {
        (sched.period_seconds - 0.2).abs() <= 1e-12 && (sched.phase_seconds - 0.2).abs() <= 1e-12
    });
    assert!(has_base);
    assert!(has_shifted);
}

#[test]
fn test_runtime_precompute_extracts_fractional_shift_sample_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("b")),
                rhs: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("shiftSample").into(),
                    args: vec![
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("Clock").into(),
                            args: vec![rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Real(0.2),
                                span: rumoca_core::Span::DUMMY,
                            }],
                            is_constructor: false,
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(5.0),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "test_fractional_shifted_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert!(
        dae_model.clocks.schedules.iter().any(|sched| {
            (sched.period_seconds - 0.2).abs() <= 1e-12
                && (sched.phase_seconds - 0.04).abs() <= 1e-12
        }),
        "expected shiftSample(Clock(0.2), 1, 5) to shift by 1/5 of the base period"
    );
}

#[test]
fn test_runtime_precompute_extracts_fractional_back_sample_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("b")),
                rhs: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("backSample").into(),
                    args: vec![
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("shiftSample").into(),
                            args: vec![
                                rumoca_core::Expression::FunctionCall {
                                    name: rumoca_core::VarName::new("Clock").into(),
                                    args: vec![rumoca_core::Expression::Literal {
                                        value: rumoca_core::Literal::Real(0.2),
                                        span: rumoca_core::Span::DUMMY,
                                    }],
                                    is_constructor: false,
                                    span: rumoca_core::Span::DUMMY,
                                },
                                rumoca_core::Expression::Literal {
                                    value: rumoca_core::Literal::Real(2.0),
                                    span: rumoca_core::Span::DUMMY,
                                },
                                rumoca_core::Expression::Literal {
                                    value: rumoca_core::Literal::Real(5.0),
                                    span: rumoca_core::Span::DUMMY,
                                },
                            ],
                            is_constructor: false,
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(5.0),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "test_fractional_back_sample_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert!(
        dae_model.clocks.schedules.iter().any(|sched| {
            (sched.period_seconds - 0.2).abs() <= 1e-12
                && (sched.phase_seconds - 0.04).abs() <= 1e-12
        }),
        "expected backSample(shiftSample(Clock(0.2), 2, 5), 1, 5) to land at phase 0.04"
    );
}

#[test]
fn test_runtime_precompute_records_per_variable_clock_phase() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("u"), {
            let mut source = dae::Variable::new(rumoca_core::VarName::new("u"));
            source.start = Some(var("u_start"));
            source
        });
    let mut start = dae::Variable::new(rumoca_core::VarName::new("u_start"));
    start.start = Some(lit(1.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("u_start"), start);
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable::new(rumoca_core::VarName::new("y")),
    );
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("u")),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("shiftSample").into(),
            args: vec![clock_call(0.02), lit(4.0), lit(3.0)],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "u = shiftSample(Clock(0.02), 4, 3)".to_string(),
        scalar_count: 1,
    });
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("backSample").into(),
            args: vec![var("u"), lit(4.0), lit(3.0)],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "y = backSample(u, 4, 3)".to_string(),
        scalar_count: 1,
    });

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    let u = dae_model
        .clocks
        .timings
        .get("u")
        .expect("shifted source timing should be recorded");
    assert!((u.period_seconds - 0.02).abs() <= 1e-12);
    assert!((u.phase_seconds - ((4.0 / 3.0) * 0.02)).abs() <= 1e-12);

    let y = dae_model
        .clocks
        .timings
        .get("y")
        .expect("back-sampled target timing should be recorded");
    assert!((y.period_seconds - 0.02).abs() <= 1e-12);
    assert!(y.phase_seconds.abs() <= 1e-12);
    assert!((dae_model.clocks.intervals["y"] - 0.02).abs() <= 1e-12);
}

#[test]
// SPEC_0021: Exception - single regression fixture for shiftSample alias-chain lowering.
#[allow(clippy::too_many_lines)]
fn test_runtime_precompute_resolves_shift_sample_via_sample_clock_alias_chain() {
    let mut dae_model = dae::Dae::default();

    for name in ["factor", "resolutionFactor"] {
        let mut p = dae::Variable::new(rumoca_core::VarName::new(name));
        p.start = Some(if name == "factor" {
            lit(20.0)
        } else {
            lit(1000.0)
        });
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), p);
    }

    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("periodicClock.y")),
            rhs: Box::new(var("periodicClock.c")),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "periodicClock_alias",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("sample1.clock")),
            rhs: Box::new(var("periodicClock.y")),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "clock_alias",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("sample1.y")),
            rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args: vec![var("sample1.u"), var("sample1.clock")],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "sample_rhs",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("shiftSample1.u")),
            rhs: Box::new(var("sample1.y")),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "shift_source_alias",
    ));

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("periodicClock.c"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("subSample").into(),
                args: vec![
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::VarName::new("Clock").into(),
                        args: vec![var("factor")],
                        is_constructor: false,
                        span: rumoca_core::Span::DUMMY,
                    },
                    var("resolutionFactor"),
                ],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "periodic_clock",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("shifted"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("shiftSample").into(),
                args: vec![var("shiftSample1.u"), lit(1.0), lit(1.0)],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "shifted_clock",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert!(
        dae_model.clocks.schedules.iter().any(|sched| {
            (sched.period_seconds - 0.02).abs() <= 1e-12
                && (sched.phase_seconds - 0.02).abs() <= 1e-12
        }),
        "expected shifted periodic schedule resolved through sample clock aliases"
    );
}

#[test]
fn test_runtime_precompute_resolves_shift_sample_with_reversed_clock_alias_equation() {
    let mut dae_model = dae::Dae::default();

    for (name, value) in [("factor", 20.0), ("resolutionFactor", 1000.0)] {
        let mut p = dae::Variable::new(rumoca_core::VarName::new(name));
        p.start = Some(lit(value));
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), p);
    }

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("periodicClock.c"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("subSample").into(),
                args: vec![
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::VarName::new("Clock").into(),
                        args: vec![var("factor")],
                        is_constructor: false,
                        span: rumoca_core::Span::DUMMY,
                    },
                    var("resolutionFactor"),
                ],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "periodic_clock",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("periodicClock.y"),
            var("periodicClock.c"),
            Span::DUMMY,
            "periodic_clock_output",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("periodicClock.y"),
            var("sample1.clock"),
            Span::DUMMY,
            "reversed_connection_alias",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("sample1.y"),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args: vec![var("sample1.u"), var("sample1.clock")],
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "sample_rhs",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("shiftSample1.u"),
            var("sample1.y"),
            Span::DUMMY,
            "shift_source_alias",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("shifted"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("shiftSample").into(),
                args: vec![var("shiftSample1.u"), lit(1.0), lit(1.0)],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "shifted_clock",
        ));

    populate_runtime_precompute(&mut dae_model).expect(
        "clock schedule should resolve even when connection alias equation orientation is reversed",
    );
    assert!(
        dae_model.clocks.schedules.iter().any(|sched| {
            (sched.period_seconds - 0.02).abs() <= 1e-12
                && (sched.phase_seconds - 0.02).abs() <= 1e-12
        }),
        "expected shifted periodic schedule resolved through reversed connection alias equation"
    );
}

#[test]
fn test_runtime_precompute_prunes_dead_clock_constructor_branch_from_const_relation() {
    let mut dae_model = dae::Dae::default();

    for (name, value) in [
        ("resolution", 1.0),
        ("threshold", 2.0),
        ("factor", 20.0),
        ("resolutionFactor", 1000.0),
    ] {
        let mut p = dae::Variable::new(rumoca_core::VarName::new(name));
        p.start = Some(lit(value));
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), p);
    }

    let cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(var("resolution")),
        rhs: Box::new(var("threshold")),
        span: rumoca_core::Span::DUMMY,
    };
    let live_branch = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("subSample").into(),
        args: vec![
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Clock").into(),
                args: vec![var("factor")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            var("resolutionFactor"),
        ],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let dead_branch = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Clock").into(),
        args: vec![
            var("periodicClock.factor"),
            var("periodicClock.resolutionFactor"),
        ],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("periodicClock.c"),
            rumoca_core::Expression::If {
                branches: vec![(cond, live_branch)],
                else_branch: Box::new(dead_branch),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "periodic_clock_conditional_constructor",
        ));

    populate_runtime_precompute(&mut dae_model)
        .expect("const-true branch should prune dead unresolved clock constructor");
    assert!(
        dae_model
            .clocks
            .schedules
            .iter()
            .any(|sched| (sched.period_seconds - 0.02).abs() <= 1e-12),
        "expected resolved subSample schedule from live branch only"
    );
}
