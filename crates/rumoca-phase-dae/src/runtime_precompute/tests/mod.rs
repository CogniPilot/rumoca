use rumoca_core::Span;

use super::*;

mod clock_alias_resolution_tests;
mod clock_alias_tests;
mod clock_schedule_tests;
mod condition_memory_resize;
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
fn test_runtime_precompute_skips_smooth_wrapped_conditions_for_synthetic_roots() {
    let mut dae_model = dae::Dae::default();
    let cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Ge,
        lhs: Box::new(var("w")),
        rhs: Box::new(lit(0.0)),
        span: test_span(10, 18),
    };
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Smooth,
            args: vec![
                lit(1.0),
                rumoca_core::Expression::If {
                    branches: vec![(
                        cond,
                        rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Mul,
                            lhs: Box::new(var("w")),
                            rhs: Box::new(var("w")),
                            span: test_span(20, 25),
                        },
                    )],
                    else_branch: Box::new(rumoca_core::Expression::Unary {
                        op: rumoca_core::OpUnary::Minus,
                        rhs: Box::new(rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Mul,
                            lhs: Box::new(var("w")),
                            rhs: Box::new(var("w")),
                            span: test_span(30, 35),
                        }),
                        span: test_span(29, 35),
                    }),
                    span: test_span(10, 35),
                },
            ],
            span: test_span(1, 36),
        },
        test_span(1, 36),
        "smooth directional loss",
    ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert!(
        dae_model.events.synthetic_root_conditions.is_empty(),
        "smooth-wrapped relations should not become synthetic zero-crossing roots"
    );
}

#[test]
fn test_runtime_precompute_suppresses_branch_roots_guarded_by_noevent_condition() {
    let mut dae_model = dae::Dae::default();
    let guard = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::NoEvent,
        args: vec![rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Abs,
                args: vec![var("w")],
                span: test_span(10, 16),
            }),
            rhs: Box::new(var("wLinear")),
            span: test_span(10, 26),
        }],
        span: test_span(2, 27),
    };
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::If {
            branches: vec![(
                guard,
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sign,
                    args: vec![var("w")],
                    span: test_span(30, 37),
                },
            )],
            else_branch: Box::new(var("w")),
            span: test_span(1, 40),
        },
        test_span(1, 40),
        "noevent guarded sign",
    ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert!(
        dae_model.events.synthetic_root_conditions.is_empty(),
        "roots inside branches guarded by noEvent conditions should not fire at unreachable surfaces"
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
