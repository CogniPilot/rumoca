use rumoca_core::Span;

use super::*;

mod clock_alias_resolution_tests;
mod clock_alias_tests;
mod dynamic_clock_tests;

fn populate_conditions(dae_model: &mut dae::Dae) {
    crate::condition_lowering::populate_canonical_conditions(dae_model)
        .expect("test condition fixtures should carry source provenance");
}

fn test_span(start: usize, end: usize) -> Span {
    Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), start, end)
}

fn time_gt(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("time").into(),
            subscripts: vec![],
            span: test_span(1, 5),
        }),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: test_span(8, 12),
        }),
        span: test_span(1, 12),
    }
}

fn lit(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: test_span(20, 24),
    }
}

fn clock_call(interval: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Clock").into(),
        args: vec![lit(interval)],
        is_constructor: false,
        span: test_span(30, 42),
    }
}

fn no_argument_clock_call(span: Span) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Clock").into(),
        args: vec![],
        is_constructor: false,
        span,
    }
}

fn var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![],
        span: test_span(50, 50 + name.len()),
    }
}

fn condition_memory_ref(name: &str, index: i64) -> rumoca_core::Expression {
    let span = test_span(60, 65);
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![rumoca_core::Subscript::generated_index(index, span)],
        span,
    }
}

fn condition_lhs(name: &str, index: i64) -> rumoca_core::Reference {
    let span = test_span(60, 65);
    rumoca_core::Reference::generated_component(
        name,
        vec![rumoca_core::Subscript::generated_index(index, span)],
        span,
    )
}

fn embedded_condition_memory_ref(name: &str, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(format!("{name}[{index}]")).into(),
        subscripts: Vec::new(),
        span: test_span(60, 65),
    }
}

fn sub(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(60, 65),
    }
}

fn and(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::And,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(60, 65),
    }
}

fn or(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Or,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(60, 65),
    }
}

fn if_expr(
    condition: rumoca_core::Expression,
    when_true: rumoca_core::Expression,
    when_false: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::If {
        branches: vec![(condition, when_true)],
        else_branch: Box::new(when_false),
        span: test_span(60, 65),
    }
}

fn initial_call() -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Initial,
        args: vec![],
        span: test_span(60, 65),
    }
}

fn if_then_else(
    condition: rumoca_core::Expression,
    value: rumoca_core::Expression,
    else_value: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::If {
        branches: vec![(condition, value)],
        else_branch: Box::new(else_value),
        span: test_span(60, 65),
    }
}

fn dae_with_if_condition(cond: rumoca_core::Expression) -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable::new(
            rumoca_core::VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::If {
            branches: vec![(cond, lit(1.0))],
            else_branch: Box::new(lit(0.0)),
            span: test_span(60, 65),
        },
        test_span(60, 65),
        "test_if_condition",
    ));
    dae_model
}

#[test]
fn test_runtime_precompute_suppresses_initial_only_synthetic_roots() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable::new(
            rumoca_core::VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    let initial_only_relation = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(0.25)),
        span: test_span(1, 2),
    };
    let runtime_relation = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(0.5)),
        span: test_span(1, 2),
    };
    dae_model.continuous.equations.push(dae::Equation::residual(
        if_expr(
            initial_call(),
            if_expr(initial_only_relation.clone(), lit(1.0), lit(2.0)),
            if_expr(runtime_relation.clone(), lit(3.0), lit(4.0)),
        ),
        test_span(1, 2),
        "initial_guarded_synthetic_root",
    ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    let initial_root = clock::relation_root_expression(&initial_only_relation)
        .expect("initial relation should have root form");
    let runtime_root =
        clock::relation_root_expression(&runtime_relation).expect("runtime relation has root form");
    assert!(
        !dae_model
            .events
            .synthetic_root_conditions
            .iter()
            .any(|root| rumoca_core::expressions_semantically_equal(root, &initial_root)),
        "relations reachable only while initial() is true must not become runtime synthetic roots"
    );
    assert!(
        dae_model
            .events
            .synthetic_root_conditions
            .iter()
            .any(|root| rumoca_core::expressions_semantically_equal(root, &runtime_root)),
        "runtime branch relations must remain synthetic roots"
    );
}

#[test]
fn test_runtime_precompute_suppresses_initial_only_condition_synthetic_roots() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable::new(
            rumoca_core::VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    let initial_only_relation = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(0.25)),
        span: test_span(1, 2),
    };
    dae_model.continuous.equations.push(dae::Equation::residual(
        if_expr(
            and(initial_call(), initial_only_relation.clone()),
            lit(1.0),
            lit(0.0),
        ),
        test_span(1, 2),
        "initial_condition_synthetic_root",
    ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    let initial_root = clock::relation_root_expression(&initial_only_relation)
        .expect("initial relation should have root form");
    assert!(
        !dae_model
            .events
            .synthetic_root_conditions
            .iter()
            .any(|root| rumoca_core::expressions_semantically_equal(root, &initial_root)),
        "relations inside runtime-false initial() conditions must not become synthetic roots"
    );
}

#[test]
fn test_runtime_precompute_suppresses_initial_only_time_events() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("y"),
            if_expr(
                initial_call(),
                if_expr(time_gt(0.25), lit(1.0), lit(2.0)),
                if_expr(time_gt(0.5), lit(3.0), lit(4.0)),
            ),
            test_span(1, 2),
            "initial_guarded_time_event",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(
        dae_model.events.scheduled_time_events,
        vec![0.5],
        "time events reachable only during initialization must not be scheduled for simulation"
    );
}

#[test]
fn test_runtime_precompute_suppresses_initial_only_condition_time_events() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("y"),
            if_expr(and(initial_call(), time_gt(0.25)), lit(1.0), lit(0.0)),
            test_span(1, 2),
            "initial_condition_time_event",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert!(
        dae_model.events.scheduled_time_events.is_empty(),
        "time events inside runtime-false initial() conditions must not be scheduled"
    );
}

#[test]
fn test_runtime_precompute_skips_compile_time_synthetic_roots() {
    let mut dae_model = dae::Dae::default();
    let mut parameter = dae::Variable::new(
        rumoca_core::VarName::new("p"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    parameter.start = Some(lit(0.2));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("p"), parameter);
    dae_model.continuous.equations.push(dae::Equation::residual(
        if_expr(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Gt,
                lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Abs,
                    args: vec![var("p")],
                    span: test_span(1, 2),
                }),
                rhs: Box::new(lit(0.1)),
                span: test_span(1, 2),
            },
            lit(1.0),
            lit(0.0),
        ),
        test_span(1, 2),
        "compile_time_synthetic_root",
    ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert!(
        dae_model.events.synthetic_root_conditions.is_empty(),
        "parameter-only synthetic roots cannot cross during simulation"
    );
}

fn less_than_with_token(_token_number: u32) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(var("leg_h")),
        rhs: Box::new(var("ground_z")),
        span: test_span(1, 2),
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
                    span: test_span(1, 2),
                },
            )],
            else_branch: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.0),
                span: test_span(1, 2),
            }),
            span: test_span(1, 2),
        },
        test_span(1, 2),
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
fn test_runtime_precompute_collects_event_action_condition_roots() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable::new(
            rumoca_core::VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    let condition = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(0.0)),
        span: test_span(1, 2),
    };
    dae_model.events.event_actions.push(dae::DaeEventAction {
        condition: condition.clone(),
        kind: dae::DaeEventActionKind::Assert {
            message: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("x crossed assertion boundary".to_string()),
                span: test_span(3, 4),
            },
        },
        span: test_span(1, 2),
        origin: "assert equation".to_string(),
    });

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    let root = clock::relation_root_expression(&condition).expect("condition has a root form");
    assert!(
        dae_model
            .events
            .synthetic_root_conditions
            .iter()
            .any(|candidate| rumoca_core::expressions_semantically_equal(candidate, &root)),
        "event action guards must contribute roots so assertions trigger across solvers"
    );
}

#[test]
fn test_runtime_precompute_interns_and_orders_root_and_time_event_metadata() {
    let root_cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(0.0)),
        span: test_span(1, 2),
    };
    let mut dae_model = dae_with_if_condition(root_cond.clone());

    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Gt,
                    lhs: Box::new(var("x")),
                    rhs: Box::new(lit(0.0)),
                    span: test_span(1, 2),
                },
                lit(2.0),
            )],
            else_branch: Box::new(lit(-1.0)),
            span: test_span(1, 2),
        },
        test_span(1, 2),
        "duplicate_root_condition",
    ));

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::If {
                branches: vec![(time_gt(1.5), lit(1.0))],
                else_branch: Box::new(lit(0.0)),
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "time_event_late",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::If {
                branches: vec![(time_gt(0.5), lit(1.0))],
                else_branch: Box::new(lit(0.0)),
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "time_event_early",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::If {
                branches: vec![(time_gt(1.5), lit(2.0))],
                else_branch: Box::new(lit(0.0)),
                span: test_span(1, 2),
            },
            test_span(1, 2),
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
                span: test_span(60, 65),
            },
            test_span(60, 65),
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
        condition_lhs("c", 1),
        condition,
        test_span(60, 65),
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
                        span: test_span(1, 2),
                    },
                )],
                else_branch: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: test_span(1, 2),
                }),
                span: test_span(1, 2),
            }],
            span: test_span(1, 2),
        },
        test_span(1, 2),
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
        span: test_span(1, 2),
    };
    let mut dae_model = dae_with_if_condition(cond.clone());
    dae_model.conditions.relations = vec![cond.clone()];
    dae_model.conditions.equations = vec![dae::Equation::explicit(
        condition_lhs("c", 1),
        cond.clone(),
        test_span(1, 2),
        "condition equation from test".to_string(),
    )];
    let mut switch_time = dae::Variable::new(
        rumoca_core::VarName::new("switch_time"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
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
        span: test_span(1, 2),
    };
    let mut dae_model = dae_with_if_condition(root_cond.clone());
    dae_model.conditions.relations = vec![time_only.clone(), root_cond.clone()];
    dae_model.conditions.equations = vec![
        dae::Equation::explicit(
            condition_lhs("c", 1),
            time_only,
            test_span(60, 65),
            "condition equation from test",
        ),
        dae::Equation::explicit(
            condition_lhs("c", 2),
            root_cond.clone(),
            test_span(60, 65),
            "condition equation from test",
        ),
    ];

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(dae_model.conditions.relations, vec![root_cond.clone()]);
    assert_eq!(dae_model.conditions.equations.len(), 1);
    assert_eq!(
        dae_model.conditions.equations[0]
            .lhs
            .as_ref()
            .map(rumoca_core::Reference::as_str),
        Some("c[1]"),
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
        span: test_span(1, 2),
    };
    let mut dae_model = dae_with_if_condition(root_cond.clone());
    dae_model.conditions.relations = vec![time_only.clone(), root_cond.clone()];
    dae_model.conditions.equations = vec![
        dae::Equation::explicit(
            condition_lhs("c", 1),
            time_only,
            test_span(60, 65),
            "condition equation from test",
        ),
        dae::Equation::explicit(
            condition_lhs("c", 2),
            root_cond.clone(),
            test_span(60, 65),
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
                span: test_span(60, 65),
            },
            test_span(60, 65),
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
                                test_span(60, 65)
                            )]
                        && pre_subscripts
                            == &[rumoca_core::Subscript::generated_index(
                                1,
                                test_span(60, 65)
                            )]
                )
        ),
        "discrete updates that already reference condition memory must track pruned f_c indices"
    );
}

#[test]
fn test_runtime_precompute_prunes_unreferenced_compound_condition_memory() {
    let root_cond = time_gt(2.5);
    let compound_cond = or(var("enabled"), root_cond.clone());
    let mut dae_model = dae_with_if_condition(condition_memory_ref("c", 2));
    dae_model.conditions.relations = vec![compound_cond, root_cond.clone()];
    dae_model.conditions.equations = vec![
        dae::Equation::explicit(
            condition_lhs("c", 1),
            dae_model.conditions.relations[0].clone(),
            test_span(60, 65),
            "condition equation from compound condition",
        ),
        dae::Equation::explicit(
            condition_lhs("c", 2),
            root_cond.clone(),
            test_span(60, 65),
            "condition equation from root condition",
        ),
    ];

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(dae_model.conditions.relations, vec![root_cond.clone()]);
    assert_eq!(dae_model.conditions.equations.len(), 1);
    assert_eq!(
        dae_model.conditions.equations[0]
            .lhs
            .as_ref()
            .map(rumoca_core::Reference::as_str),
        Some("c[1]"),
    );
    assert!(
        matches!(
            &dae_model.continuous.equations[0].rhs,
            rumoca_core::Expression::If { branches, .. }
                if matches!(
                    &branches[0].0,
                    rumoca_core::Expression::VarRef { name, subscripts, .. }
                        if name.as_str() == "c"
                            && subscripts == &[rumoca_core::Subscript::generated_index(
                                1,
                                test_span(60, 65)
                            )]
                )
        ),
        "condition memory references must be reindexed after pruning an unused compound guard"
    );
}

#[test]
fn test_runtime_precompute_reindexes_nested_condition_memory_in_fc_after_prune() {
    let time_only = time_gt(2.5);
    let root_cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(0.0)),
        span: test_span(1, 2),
    };
    let nested_cond = condition_memory_ref("c", 2);
    let mut dae_model = dae_with_if_condition(root_cond.clone());
    dae_model.conditions.relations = vec![time_only.clone(), root_cond, nested_cond];
    dae_model.conditions.equations = vec![
        dae::Equation::explicit(
            condition_lhs("c", 1),
            time_only,
            test_span(60, 65),
            "condition equation from test",
        ),
        dae::Equation::explicit(
            condition_lhs("c", 2),
            dae_model.conditions.relations[1].clone(),
            test_span(60, 65),
            "condition equation from test",
        ),
        dae::Equation::explicit(
            condition_lhs("c", 3),
            dae_model.conditions.relations[2].clone(),
            test_span(60, 65),
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
                        test_span(60, 65)
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
        span: test_span(1, 2),
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
            span: test_span(1, 2),
        }),
        rhs: Box::new(var("switch_at")),
        span: test_span(1, 2),
    };
    let mut dae_model = dae_with_if_condition(cond);
    let mut delay = dae::Variable::new(
        rumoca_core::VarName::new("delay"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    delay.start = Some(lit(0.25));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("delay"), delay);
    let mut switch_at = dae::Variable::new(
        rumoca_core::VarName::new("switch_at"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
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
        dae::Variable::new(
            rumoca_core::VarName::new("c"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
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
                    span: test_span(1, 2),
                },
            ),
            test_span(1, 2),
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
        dae::Variable::new(
            rumoca_core::VarName::new("s"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
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
                    span: test_span(1, 2),
                }),
                rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sample,
                    args: vec![
                        rumoca_core::Expression::VarRef {
                            name: rumoca_core::VarName::new("u").into(),
                            subscripts: vec![],
                            span: test_span(100, 101),
                        },
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("Clock").into(),
                            args: vec![rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Real(0.1),
                                span: test_span(108, 111),
                            }],
                            is_constructor: false,
                            span: test_span(102, 112),
                        },
                    ],
                    span: test_span(95, 113),
                }),
                span: test_span(1, 2),
            },
            test_span(1, 2),
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
fn test_runtime_precompute_rejects_static_clock_constructor_without_source_provenance() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("s"),
        dae::Variable::new(
            rumoca_core::VarName::new("s"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            sub(
                var("s"),
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
            "unspanned_static_clock_constructor",
        ));

    let err = populate_runtime_precompute(&mut dae_model)
        .expect_err("source-free static clock constructors must fail fast");
    assert!(matches!(
        err,
        crate::ToDaeError::RuntimeMetadataViolation { detail }
            if detail.contains("source provenance")
    ));
}

#[test]
fn test_runtime_precompute_collects_sample_start_interval_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("s"),
        dae::Variable::new(
            rumoca_core::VarName::new("s"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
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
                            span: test_span(120, 123),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(0.1),
                            span: test_span(125, 128),
                        },
                    ],
                    span: test_span(113, 129),
                },
            ),
            test_span(1, 2),
            // MLS §16.5.1: sample(start, interval) defines a periodic event.
            "periodic_sample_start_interval",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(dae_model.clocks.schedules.len(), 1);
    assert!((dae_model.clocks.schedules[0].period_seconds - 0.1).abs() <= 1e-12);
    assert!((dae_model.clocks.schedules[0].phase_seconds - 0.2).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_marks_schedule_backed_sample_root_condition() {
    let mut dae_model = dae::Dae::default();
    let relation = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME).into(),
        args: vec![lit(42.0), lit(0.05), lit(0.1)],
        is_constructor: false,
        span: test_span(120, 149),
    };
    dae_model.conditions.relations.push(relation.clone());
    dae_model.conditions.equations.push(dae::Equation::explicit(
        condition_lhs("c", 1),
        relation,
        test_span(1, 2),
        "scheduled sample condition memory",
    ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(dae_model.events.scheduled_root_conditions.len(), 1);
    let root = &dae_model.events.scheduled_root_conditions[0];
    assert_eq!(root.root_index, 0);
    assert!((root.period_seconds - 0.1).abs() <= 1e-12);
    assert!((root.phase_seconds - 0.05).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_collects_sample_schedule_from_initial_time_parameter() {
    let mut dae_model = dae::Dae::default();
    let mut frequency = dae::Variable::new(
        rumoca_core::VarName::new("mean.f"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    frequency.start = Some(lit(150.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("mean.f"), frequency);
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("mean.t0"),
        dae::Variable::new(
            rumoca_core::VarName::new("mean.t0"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("s"),
        dae::Variable::new(
            rumoca_core::VarName::new("s"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            sub(var("mean.t0"), var("time")),
            test_span(1, 2),
            "initial t0 = time",
        ));

    let interval = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Div,
        lhs: Box::new(lit(1.0)),
        rhs: Box::new(var("mean.f")),
        span: test_span(140, 148),
    };
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            sub(
                var("s"),
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new(rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME)
                        .into(),
                    args: vec![
                        rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Add,
                            lhs: Box::new(var("mean.t0")),
                            rhs: Box::new(interval.clone()),
                            span: test_span(130, 148),
                        },
                        interval,
                    ],
                    is_constructor: false,
                    span: test_span(120, 149),
                },
            ),
            test_span(1, 2),
            "periodic sample with initial-time origin",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(dae_model.clocks.schedules.len(), 1);
    assert!((dae_model.clocks.schedules[0].period_seconds - 1.0 / 150.0).abs() <= 1e-12);
    assert!((dae_model.clocks.schedules[0].phase_seconds - 1.0 / 150.0).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_assigns_implicit_sample_interval_from_unique_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("simTime"),
        dae::Variable::new(
            rumoca_core::VarName::new("simTime"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("clockY"),
        dae::Variable::new(
            rumoca_core::VarName::new("clockY"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
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
                    span: test_span(1, 2),
                },
            ),
            test_span(1, 2),
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
                        span: test_span(140, 143),
                    }],
                    is_constructor: false,
                    span: test_span(134, 144),
                },
            ),
            test_span(1, 2),
            "periodic_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert_eq!(dae_model.clocks.schedules.len(), 1);
    assert!((dae_model.clocks.intervals["simTime"] - 0.1).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_propagates_no_argument_clock_guard_timing() {
    let mut dae_model = dae::Dae::default();
    for name in ["u", "dummy", "b"] {
        dae_model.variables.discrete_valued.insert(
            rumoca_core::VarName::new(name),
            dae::Variable::new(
                rumoca_core::VarName::new(name),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
    }
    let clock_span = test_span(1_000, 1_005);
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("u").into()),
        rhs: clock_call(0.02),
        span: test_span(1, 2),
        origin: "u = Clock(0.02)".to_string(),
        scalar_count: 1,
    });
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("dummy").into()),
        rhs: if_then_else(
            no_argument_clock_call(clock_span),
            var("u"),
            var("__pre__.dummy"),
        ),
        span: test_span(1, 2),
        origin: "when Clock() then dummy = u".to_string(),
        scalar_count: 1,
    });
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("b").into()),
        rhs: if_then_else(
            no_argument_clock_call(clock_span),
            rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Not,
                rhs: Box::new(var("__pre__.__pre__.b")),
                span: test_span(1, 2),
            },
            var("__pre__.b"),
        ),
        span: test_span(1, 2),
        origin: "when Clock() then b = not previous(b)".to_string(),
        scalar_count: 1,
    });

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert!((dae_model.clocks.intervals["dummy"] - 0.02).abs() <= 1e-12);
    assert!((dae_model.clocks.intervals["b"] - 0.02).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_assigns_clock_interval_to_algebraic_alias_chain() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.inputs.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable::new(
            rumoca_core::VarName::new("u"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("feedback.y"),
        dae::Variable::new(
            rumoca_core::VarName::new("feedback.y"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("PI.u"),
        dae::Variable::new(
            rumoca_core::VarName::new("PI.u"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("sample2.y"),
        dae::Variable::new(
            rumoca_core::VarName::new("sample2.y"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("sample2.clock"),
        dae::Variable::new(
            rumoca_core::VarName::new("sample2.clock"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
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
                    span: test_span(150, 153),
                }],
                is_constructor: false,
                span: test_span(144, 154),
            },
            test_span(1, 2),
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
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "explicit_sample_value",
        ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("sample2.y")),
            rhs: Box::new(var("feedback.y")),
            span: test_span(1, 2),
        },
        test_span(1, 2),
        "sample_alias",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("feedback.y")),
            rhs: Box::new(var("PI.u")),
            span: test_span(1, 2),
        },
        test_span(1, 2),
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
        dae::Variable::new(
            rumoca_core::VarName::new("b"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("clockY"),
        dae::Variable::new(
            rumoca_core::VarName::new("clockY"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
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
                    span: test_span(1, 2),
                }),
                span: test_span(1, 2),
            },
            test_span(1, 2),
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
                        span: test_span(160, 163),
                    }],
                    is_constructor: false,
                    span: test_span(154, 164),
                }),
                span: test_span(1, 2),
            },
            test_span(1, 2),
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
                    span: test_span(1, 2),
                }),
                rhs: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("shiftSample").into(),
                    args: vec![
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("Clock").into(),
                            args: vec![rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Real(0.2),
                                span: test_span(170, 173),
                            }],
                            is_constructor: false,
                            span: test_span(164, 174),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: test_span(176, 179),
                        },
                    ],
                    is_constructor: false,
                    span: test_span(152, 180),
                }),
                span: test_span(1, 2),
            },
            test_span(1, 2),
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
                                span: test_span(190, 193),
                            }],
                            is_constructor: false,
                            span: test_span(184, 194),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: test_span(196, 199),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(5.0),
                            span: test_span(201, 204),
                        },
                    ],
                    is_constructor: false,
                    span: test_span(180, 205),
                }),
                span: test_span(1, 2),
            },
            test_span(1, 2),
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
                                        span: test_span(210, 213),
                                    }],
                                    is_constructor: false,
                                    span: test_span(204, 214),
                                },
                                rumoca_core::Expression::Literal {
                                    value: rumoca_core::Literal::Real(2.0),
                                    span: test_span(216, 219),
                                },
                                rumoca_core::Expression::Literal {
                                    value: rumoca_core::Literal::Real(5.0),
                                    span: test_span(221, 224),
                                },
                            ],
                            is_constructor: false,
                            span: test_span(192, 225),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: test_span(227, 230),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(5.0),
                            span: test_span(232, 235),
                        },
                    ],
                    is_constructor: false,
                    span: test_span(180, 236),
                }),
                span: test_span(1, 2),
            },
            test_span(1, 2),
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
            let mut source = dae::Variable::new(
                rumoca_core::VarName::new("u"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            );
            source.start = Some(var("u_start"));
            source
        });
    let mut start = dae::Variable::new(
        rumoca_core::VarName::new("u_start"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    start.start = Some(lit(1.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("u_start"), start);
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable::new(
            rumoca_core::VarName::new("y"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("u").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("shiftSample").into(),
            args: vec![clock_call(0.02), lit(4.0), lit(3.0)],
            is_constructor: false,
            span: test_span(1, 2),
        },
        span: test_span(1, 2),
        origin: "u = shiftSample(Clock(0.02), 4, 3)".to_string(),
        scalar_count: 1,
    });
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("backSample").into(),
            args: vec![var("u"), lit(4.0), lit(3.0)],
            is_constructor: false,
            span: test_span(1, 2),
        },
        span: test_span(1, 2),
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
