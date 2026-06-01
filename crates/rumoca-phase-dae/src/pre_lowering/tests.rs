use rumoca_ir_dae as dae;

use super::lower_pre_operator;
use crate::ToDaeError;

fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn pre_call(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Pre,
        args: vec![var_ref(name)],
        span: rumoca_core::Span::DUMMY,
    }
}

fn previous_call(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("previous").into(),
        args: vec![var_ref(name)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn indexed_field_access(base_name: &str, index: i64, field: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Index {
            base: Box::new(var_ref(base_name)),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                index,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        }),
        field: field.to_string(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn integer_literal(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn sample_with_clock_call(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sample,
        args: vec![
            var_ref(name),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Clock").into(),
                args: vec![],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

fn discrete_valued_var(name: &str) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        component_ref: None,
        source_span: rumoca_core::Span::DUMMY,
        dims: vec![],
        start: None,
        start_span: None,
        fixed: None,
        min: None,
        min_span: None,
        max: None,
        max_span: None,
        nominal: None,
        nominal_span: None,
        unit: None,
        state_select: rumoca_core::StateSelect::Default,
        description: None,
        is_tunable: false,
    }
}

#[test]
fn test_lower_pre_allocates_sampled_value_parameter_without_rewriting_call()
-> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("sampled.u"),
        discrete_valued_var("sampled.u"),
    );
    dae.discrete.valued_updates.push(dae::Equation::explicit(
        rumoca_core::VarName::new("sampled.y"),
        sample_with_clock_call("sampled.u"),
        rumoca_core::Span::default(),
        "clocked sample update".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.sampled.u")),
        "sample(u, clock) must sample the tick left limit of u"
    );
    assert!(
        matches!(
            &dae.discrete.valued_updates[0].rhs,
            rumoca_core::Expression::FunctionCall { name, .. }
                if name.as_str() == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME
        ),
        "sample(u, clock) must lower to the internal DAE sample-tick form"
    );
    Ok(())
}

#[test]
fn test_lower_pre_does_not_allocate_sampled_time_parameter() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.discrete.real_updates.push(dae::Equation::explicit(
        rumoca_core::VarName::new("source.simTime"),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sample,
            args: vec![rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("time").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::default(),
        "clocked time sample".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    assert!(
        !dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.time")),
        "time is a runtime scalar, not a pre-snapshot variable"
    );
    Ok(())
}

#[test]
fn test_lower_pre_does_not_rewrite_memoryless_time_edge_to_raw_relation() -> Result<(), ToDaeError>
{
    let mut dae = dae::Dae::new();
    let time_guard = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Ge,
        lhs: Box::new(var_ref("time")),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.5),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    dae.discrete.real_updates.push(dae::Equation::explicit(
        rumoca_core::VarName::new("v"),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Edge,
            args: vec![time_guard],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::default(),
        "time-event guard".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    assert!(
        matches!(
            dae.discrete.real_updates[0].rhs,
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::And,
                ..
            }
        ),
        "memoryless time-threshold edge guards must not lower to a level-sensitive raw relation"
    );
    Ok(())
}

#[test]
fn test_lower_pre_preserves_pre_time_threshold_condition_memory() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_reals.insert(
        rumoca_core::VarName::new("deadline"),
        discrete_valued_var("deadline"),
    );
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c"), discrete_valued_var("c"));
    let time_guard = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Ge,
        lhs: Box::new(var_ref("time")),
        rhs: Box::new(pre_call("deadline")),
        span: rumoca_core::Span::DUMMY,
    };
    dae.conditions.relations.push(time_guard.clone());
    dae.conditions.equations.push(dae::Equation::explicit(
        rumoca_core::VarName::new("c"),
        time_guard.clone(),
        rumoca_core::Span::default(),
        "condition variable".to_string(),
    ));
    dae.discrete.real_updates.push(dae::Equation::explicit(
        rumoca_core::VarName::new("v"),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Edge,
            args: vec![time_guard],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::default(),
        "pre time-event guard".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    assert!(
        matches!(
            &dae.discrete.real_updates[0].rhs,
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::And,
                lhs,
                rhs,
                ..
            } if matches!(
                lhs.as_ref(),
                rumoca_core::Expression::VarRef { name, subscripts, .. }
                    if name.as_str() == "c" && subscripts.is_empty()
            ) && matches!(
                rhs.as_ref(),
                rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Not,
                    rhs,
                    ..
                } if matches!(
                    rhs.as_ref(),
                    rumoca_core::Expression::VarRef { name, subscripts, .. }
                        if name.as_str() == "__pre__.c" && subscripts.is_empty()
                )
            )
        ),
        "pre-based time-threshold edge guards keep condition-memory edge detection"
    );
    Ok(())
}

#[test]
fn test_lower_pre_rewrites_relation_edge_to_condition_memory() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables
        .states
        .insert(rumoca_core::VarName::new("x"), discrete_valued_var("x"));
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c"), discrete_valued_var("c"));
    let relation = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(var_ref("x")),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    dae.conditions.relations.push(relation.clone());
    dae.conditions.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("c")),
        rhs: relation.clone(),
        span: rumoca_core::Span::DUMMY,
        origin: "condition variable".to_string(),
        scalar_count: 1,
    });
    dae.events.event_actions.push(dae::DaeEventAction {
        condition: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Edge,
            args: vec![relation],
            span: rumoca_core::Span::DUMMY,
        },
        kind: dae::DaeEventActionKind::Terminate {
            message: "edge fired".to_string(),
        },
        span: rumoca_core::Span::DUMMY,
        origin: "when x < 0".to_string(),
    });

    lower_pre_operator(&mut dae)?;

    assert!(
        matches!(
            &dae.events.event_actions[0].condition,
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::And,
                lhs,
                rhs,
                ..
            } if matches!(
                lhs.as_ref(),
                rumoca_core::Expression::VarRef { name, subscripts, .. }
                    if name.as_str() == "c" && subscripts.is_empty()
            ) && matches!(
                rhs.as_ref(),
                rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Not,
                    rhs,
                    ..
                } if matches!(
                    rhs.as_ref(),
                    rumoca_core::Expression::VarRef { name, subscripts, .. }
                        if name.as_str() == "__pre__.c" && subscripts.is_empty()
                )
            )
        ),
        "edge(relation) should lower directly to c && !pre(c)"
    );

    lower_pre_operator(&mut dae)?;

    assert!(
        matches!(
            &dae.events.event_actions[0].condition,
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::And,
                lhs,
                rhs,
                ..
            } if matches!(
                lhs.as_ref(),
                rumoca_core::Expression::VarRef { name, subscripts, .. }
                    if name.as_str() == "c" && subscripts.is_empty()
            ) && matches!(
                rhs.as_ref(),
                rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Not,
                    rhs,
                    ..
                } if matches!(
                    rhs.as_ref(),
                    rumoca_core::Expression::VarRef { name, subscripts, .. }
                        if name.as_str() == "__pre__.c" && subscripts.is_empty()
                )
            )
        ),
        "condition-memory edge should lower to c && !pre(c)"
    );
    Ok(())
}

#[test]
fn test_lower_pre_creates_parameter() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_reals.insert(
        rumoca_core::VarName::new("pulse_count"),
        dae::Variable {
            name: rumoca_core::VarName::new("pulse_count"),
            component_ref: None,
            source_span: rumoca_core::Span::DUMMY,
            dims: vec![],
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.0),
                span: rumoca_core::Span::DUMMY,
            }),
            start_span: None,
            fixed: None,
            min: None,
            min_span: None,
            max: None,
            max_span: None,
            nominal: None,
            nominal_span: None,
            unit: None,
            state_select: rumoca_core::StateSelect::Default,
            description: None,
            is_tunable: false,
        },
    );
    dae.continuous.equations.push(dae::Equation::residual(
        pre_call("pulse_count"),
        rumoca_core::Span::default(),
        "test".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    // Should have created a parameter __pre__.pulse_count
    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.pulse_count"))
    );
    let pre_param = &dae.variables.parameters[&rumoca_core::VarName::new("__pre__.pulse_count")];
    assert_eq!(
        pre_param.start,
        Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span: rumoca_core::Span::DUMMY,
        })
    );

    // The equation should now reference __pre__.pulse_count
    match &dae.continuous.equations[0].rhs {
        rumoca_core::Expression::VarRef { name, .. } => {
            assert_eq!(name.as_str(), "__pre__.pulse_count");
        }
        other => panic!("Expected VarRef, got {:?}", other),
    }
    Ok(())
}

#[test]
fn test_lower_pre_allocates_previous_parameter_without_rewriting_call() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("unitDelay.u"),
        discrete_valued_var("unitDelay.u"),
    );
    dae.discrete.valued_updates.push(dae::Equation::explicit(
        rumoca_core::VarName::new("unitDelay.y"),
        previous_call("unitDelay.u"),
        rumoca_core::Span::default(),
        "clocked previous update".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.unitDelay.u")),
        "previous(v) must allocate a pre parameter so solve lowering can read the tick left limit"
    );
    match &dae.discrete.valued_updates[0].rhs {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert_eq!(name.as_str(), "__pre__.unitDelay.u");
            assert!(subscripts.is_empty());
        }
        other => {
            panic!("previous(v) should lower to an explicit __pre__ parameter, got {other:?}")
        }
    }
    Ok(())
}

#[test]
fn test_lower_pre_no_pre_calls_is_noop() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.continuous.equations.push(dae::Equation::residual(
        var_ref("x"),
        rumoca_core::Span::default(),
        "test".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    assert!(dae.variables.parameters.is_empty());
    Ok(())
}

#[test]
fn test_lower_pre_does_not_allocate_time_for_change() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.conditions
        .relations
        .push(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Change,
            args: vec![var_ref("time")],
            span: rumoca_core::Span::DUMMY,
        });

    lower_pre_operator(&mut dae)?;

    assert!(
        !dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.time")),
        "time is a runtime scalar, not a pre-snapshot variable"
    );
    Ok(())
}

#[test]
fn test_lower_pre_normalizes_encoded_integer_subscript_target() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_reals.insert(
        rumoca_core::VarName::new("buf"),
        dae::Variable {
            name: rumoca_core::VarName::new("buf"),
            component_ref: None,
            source_span: rumoca_core::Span::DUMMY,
            dims: vec![2],
            start: None,
            start_span: None,
            fixed: None,
            min: None,
            min_span: None,
            max: None,
            max_span: None,
            nominal: None,
            nominal_span: None,
            unit: None,
            state_select: rumoca_core::StateSelect::Default,
            description: None,
            is_tunable: false,
        },
    );
    dae.continuous.equations.push(dae::Equation::residual(
        pre_call("buf[1]"),
        rumoca_core::Span::default(),
        "test".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.buf")),
        "encoded buf[1] target should use base array metadata"
    );
    match &dae.continuous.equations[0].rhs {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert_eq!(name.as_str(), "__pre__.buf");
            assert_eq!(
                subscripts,
                &[rumoca_core::Subscript::generated_index(
                    1,
                    rumoca_core::Span::DUMMY
                )]
            );
        }
        other => panic!("Expected VarRef, got {:?}", other),
    }
    Ok(())
}

#[test]
fn test_lower_pre_rejects_missing_target_variable() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.continuous.equations.push(dae::Equation::residual(
        pre_call("missing"),
        rumoca_core::Span::default(),
        "test".to_string(),
    ));

    let err = lower_pre_operator(&mut dae).expect_err("missing pre target must fail");

    assert!(
        format!("{err}").contains("pre() target `missing`"),
        "expected missing pre target diagnostic, got {err:?}"
    );
    Ok(())
}

#[test]
fn test_lower_pre_ignores_enum_literals_in_edge_relations() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("logic"),
        discrete_valued_var("logic"),
    );
    dae.symbols.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
        4,
    );

    let relation = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var_ref("logic")),
        rhs: Box::new(var_ref("Modelica.Electrical.Digital.Interfaces.Logic.'1'")),
        span: rumoca_core::Span::DUMMY,
    };
    dae.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Edge,
            args: vec![relation],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::default(),
        "test".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.logic")),
        "discrete variable should still get a pre parameter"
    );
    assert!(
        !dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new(
                "__pre__.Modelica.Electrical.Digital.Interfaces.Logic.'1'"
            )),
        "enum literal must not get a pre parameter"
    );
    let rhs = format!("{:?}", dae.continuous.equations[0].rhs);
    assert!(
        !rhs.contains("BuiltinFunction::Pre"),
        "pre operator should be fully lowered: {rhs}"
    );
    assert!(
        rhs.contains("__pre__.logic"),
        "missing pre logic ref: {rhs}"
    );
    assert!(
        rhs.contains("Modelica.Electrical.Digital.Interfaces.Logic.'1'"),
        "enum literal should remain as a symbol: {rhs}"
    );
    Ok(())
}

#[test]
fn test_lower_pre_rejects_continuous_state_target() {
    let mut dae = dae::Dae::new();
    dae.variables
        .states
        .insert(rumoca_core::VarName::new("x"), discrete_valued_var("x"));
    dae.continuous.equations.push(dae::Equation::residual(
        pre_call("x"),
        rumoca_core::Span::default(),
        "test".to_string(),
    ));

    let err = lower_pre_operator(&mut dae).expect_err("pre(state) must fail");

    assert!(
        format!("{err}").contains("not a discrete-time variable"),
        "expected pre(state) diagnostic, got {err:?}"
    );
}

#[test]
fn test_lower_pre_keeps_existing_pre_parameter_metadata() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_reals.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            name: rumoca_core::VarName::new("x"),
            component_ref: None,
            source_span: rumoca_core::Span::DUMMY,
            dims: vec![],
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            }),
            start_span: None,
            fixed: None,
            min: None,
            min_span: None,
            max: None,
            max_span: None,
            nominal: None,
            nominal_span: None,
            unit: None,
            state_select: rumoca_core::StateSelect::Default,
            description: None,
            is_tunable: false,
        },
    );
    dae.variables.parameters.insert(
        rumoca_core::VarName::new("__pre__.x"),
        dae::Variable {
            name: rumoca_core::VarName::new("__pre__.x"),
            component_ref: None,
            source_span: rumoca_core::Span::DUMMY,
            dims: vec![],
            start: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(5.0),
                span: rumoca_core::Span::DUMMY,
            }),
            start_span: None,
            fixed: Some(true),
            min: None,
            min_span: None,
            max: None,
            max_span: None,
            nominal: None,
            nominal_span: None,
            unit: None,
            state_select: rumoca_core::StateSelect::Default,
            description: Some("first pass metadata".to_string()),
            is_tunable: false,
        },
    );
    dae.continuous.equations.push(dae::Equation::residual(
        pre_call("x"),
        rumoca_core::Span::default(),
        "test".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    let pre_param = &dae.variables.parameters[&rumoca_core::VarName::new("__pre__.x")];
    assert_eq!(
        pre_param.start,
        Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(5.0),
            span: rumoca_core::Span::DUMMY,
        })
    );
    assert_eq!(
        pre_param.description.as_deref(),
        Some("first pass metadata")
    );
    Ok(())
}

#[test]
fn test_lower_pre_preserves_unhandled_pre_shape() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Pre,
            args: vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::default(),
        "test".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    assert!(matches!(
        &dae.continuous.equations[0].rhs,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            ..
        }
    ));
    Ok(())
}

#[test]
fn test_lower_pre_rewrites_expression_variables_to_pre_parameters() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    for name in ["left", "right"] {
        dae.variables.discrete_valued.insert(
            rumoca_core::VarName::new(name),
            dae::Variable {
                name: rumoca_core::VarName::new(name),
                component_ref: None,
                source_span: rumoca_core::Span::DUMMY,
                dims: vec![],
                start: None,
                start_span: None,
                fixed: None,
                min: None,
                min_span: None,
                max: None,
                max_span: None,
                nominal: None,
                nominal_span: None,
                unit: None,
                state_select: rumoca_core::StateSelect::Default,
                description: None,
                is_tunable: false,
            },
        );
    }
    dae.discrete.valued_updates.push(dae::Equation::residual(
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Pre,
            args: vec![rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Or,
                lhs: Box::new(var_ref("left")),
                rhs: Box::new(rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Not,
                    rhs: Box::new(var_ref("right")),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::default(),
        "test".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.left"))
    );
    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.right"))
    );
    let rhs = format!("{:?}", dae.discrete.valued_updates[0].rhs);
    assert!(
        rhs.contains("VarName(\"__pre__.left\")")
            && rhs.contains("VarName(\"__pre__.right\")")
            && !rhs.contains("BuiltinFunction::Pre"),
        "pre(expression) should lower structurally, got {rhs}"
    );
    Ok(())
}

#[test]
fn test_lower_pre_registers_change_operand_left_limit() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("clock"),
        discrete_valued_var("clock"),
    );
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("flag"),
        discrete_valued_var("flag"),
    );
    dae.discrete.valued_updates.push(dae::Equation::explicit(
        rumoca_core::VarName::new("flag"),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Change,
            args: vec![var_ref("clock")],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        "flag = change(clock)",
    ));

    lower_pre_operator(&mut dae)?;

    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.clock"))
    );
    assert!(
        matches!(
            &dae.discrete.valued_updates[0].rhs,
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Neq,
                lhs,
                rhs,
                ..
            } if matches!((lhs.as_ref(), rhs.as_ref()),
                (
                    rumoca_core::Expression::VarRef { name: lhs_name, .. },
                    rumoca_core::Expression::VarRef { name: rhs_name, .. },
                ) if lhs_name.as_str() == "clock" && rhs_name.as_str() == "__pre__.clock")
        ),
        "change(clock) must lower to clock <> __pre__.clock"
    );
    Ok(())
}

#[test]
fn test_lower_pre_rewrites_triggered_clock_conditions() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("clock_gate"),
        discrete_valued_var("clock_gate"),
    );
    dae.clocks.triggered_conditions.push(pre_call("clock_gate"));

    lower_pre_operator(&mut dae)?;

    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.clock_gate")),
        "triggered clock conditions must allocate pre parameters"
    );
    assert!(
        matches!(
            &dae.clocks.triggered_conditions[0],
            rumoca_core::Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "__pre__.clock_gate" && subscripts.is_empty()
        ),
        "pre() should not survive in triggered clock conditions"
    );
    Ok(())
}

#[test]
fn test_lower_pre_preserves_index_subscripts() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_reals.insert(
        rumoca_core::VarName::new("sampled"),
        dae::Variable {
            name: rumoca_core::VarName::new("sampled"),
            component_ref: None,
            source_span: rumoca_core::Span::DUMMY,
            dims: vec![3],
            start: None,
            start_span: None,
            fixed: None,
            min: None,
            min_span: None,
            max: None,
            max_span: None,
            nominal: None,
            nominal_span: None,
            unit: None,
            state_select: rumoca_core::StateSelect::Default,
            description: None,
            is_tunable: false,
        },
    );
    dae.discrete.real_updates.push(dae::Equation::residual(
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Pre,
            args: vec![rumoca_core::Expression::Index {
                base: Box::new(var_ref("sampled")),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    2,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::default(),
        "test".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    let pre_param = &dae.variables.parameters[&rumoca_core::VarName::new("__pre__.sampled")];
    assert_eq!(pre_param.dims, vec![3]);
    match &dae.discrete.real_updates[0].rhs {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert_eq!(name.as_str(), "__pre__.sampled");
            assert_eq!(
                subscripts,
                &[rumoca_core::Subscript::generated_index(
                    2,
                    rumoca_core::Span::DUMMY
                )]
            );
        }
        other => panic!("Expected VarRef, got {:?}", other),
    }
    Ok(())
}

#[test]
fn test_lower_pre_rewrites_var_ref_subscript_expressions() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("selector"),
        discrete_valued_var("selector"),
    );
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("target"),
        discrete_valued_var("target"),
    );
    dae.discrete.valued_updates.push(dae::Equation::explicit(
        rumoca_core::VarName::new("target"),
        rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("table").into(),
            subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(pre_call(
                "selector",
            )))],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        "target = table[pre(selector)]",
    ));

    lower_pre_operator(&mut dae)?;

    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.selector"))
    );
    let rhs = &dae.discrete.valued_updates[0].rhs;
    match rhs {
        rumoca_core::Expression::VarRef { subscripts, .. } => {
            assert!(matches!(
                subscripts.as_slice(),
                [rumoca_core::Subscript::Expr { expr, .. }]
                    if matches!(
                        expr.as_ref(),
                        rumoca_core::Expression::VarRef { name, .. }
                            if name.as_str() == "__pre__.selector"
                    )
            ));
        }
        other => panic!("Expected VarRef with rewritten subscript, got {:?}", other),
    }
    assert!(
        !format!("{rhs:?}").contains("BuiltinFunction::Pre"),
        "pre() should not survive in VarRef subscripts: {rhs:?}"
    );
    Ok(())
}

#[test]
fn test_lower_pre_rewrites_index_subscript_expressions() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("selector"),
        discrete_valued_var("selector"),
    );
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("target"),
        discrete_valued_var("target"),
    );
    dae.discrete.valued_updates.push(dae::Equation::explicit(
        rumoca_core::VarName::new("target"),
        rumoca_core::Expression::Index {
            base: Box::new(var_ref("table")),
            subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(pre_call(
                "selector",
            )))],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        "target = table[pre(selector)]",
    ));

    lower_pre_operator(&mut dae)?;

    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.selector"))
    );
    let rhs = &dae.discrete.valued_updates[0].rhs;
    match rhs {
        rumoca_core::Expression::Index { subscripts, .. } => {
            assert!(matches!(
                subscripts.as_slice(),
                [rumoca_core::Subscript::Expr { expr, .. }]
                    if matches!(
                        expr.as_ref(),
                        rumoca_core::Expression::VarRef { name, .. }
                            if name.as_str() == "__pre__.selector"
                    )
            ));
        }
        other => panic!("Expected Index with rewritten subscript, got {:?}", other),
    }
    Ok(())
}

#[test]
fn test_lower_pre_rewrites_record_field_target() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_reals.insert(
        rumoca_core::VarName::new("recorded.value"),
        dae::Variable {
            name: rumoca_core::VarName::new("recorded.value"),
            component_ref: None,
            source_span: rumoca_core::Span::DUMMY,
            dims: vec![],
            start: None,
            start_span: None,
            fixed: None,
            min: None,
            min_span: None,
            max: None,
            max_span: None,
            nominal: None,
            nominal_span: None,
            unit: None,
            state_select: rumoca_core::StateSelect::Default,
            description: None,
            is_tunable: false,
        },
    );
    dae.discrete.valued_updates.push(dae::Equation::residual(
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Pre,
            args: vec![rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("recorded")),
                field: "value".to_string(),
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::default(),
        "test".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.recorded.value"))
    );
    match &dae.discrete.valued_updates[0].rhs {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert_eq!(name.as_str(), "__pre__.recorded.value");
            assert!(subscripts.is_empty());
        }
        other => panic!("Expected VarRef, got {:?}", other),
    }
    Ok(())
}

#[test]
fn test_lower_pre_rewrites_indexed_record_field_target() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("step.outPort[1].reset"),
        discrete_valued_var("step.outPort[1].reset"),
    );
    dae.discrete.valued_updates.push(dae::Equation::residual(
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Pre,
            args: vec![indexed_field_access("step.outPort", 1, "reset")],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::default(),
        "test".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.step.outPort[1].reset"))
    );
    match &dae.discrete.valued_updates[0].rhs {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert_eq!(name.as_str(), "__pre__.step.outPort[1].reset");
            assert!(subscripts.is_empty());
        }
        other => panic!("Expected VarRef, got {:?}", other),
    }
    Ok(())
}

#[test]
fn test_lower_pre_prunes_static_if_branch_when_collecting_pre_values() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("step.outPort[1].available"),
        discrete_valued_var("step.outPort[1].available"),
    );
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("step.localActive"),
        discrete_valued_var("step.localActive"),
    );
    let condition = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(integer_literal(1)),
        rhs: Box::new(integer_literal(1)),
        span: rumoca_core::Span::DUMMY,
    };
    let guarded = rumoca_core::Expression::If {
        branches: vec![(condition, var_ref("step.localActive"))],
        else_branch: Box::new(indexed_field_access("step.outPort", 0, "available")),
        span: rumoca_core::Span::DUMMY,
    };
    dae.conditions
        .relations
        .push(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Change,
            args: vec![guarded],
            span: rumoca_core::Span::DUMMY,
        });

    lower_pre_operator(&mut dae)?;

    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.step.localActive"))
    );
    assert!(
        !dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new(
                "__pre__.step.outPort[0].available"
            ))
    );
    Ok(())
}

#[test]
fn test_lower_pre_resolves_singleton_scalarized_field_selection() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("step.suspend[1].reset"),
        discrete_valued_var("step.suspend[1].reset"),
    );
    dae.conditions
        .relations
        .push(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Change,
            args: vec![var_ref("step.suspend.reset")],
            span: rumoca_core::Span::DUMMY,
        });

    lower_pre_operator(&mut dae)?;

    assert!(
        dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.step.suspend[1].reset"))
    );
    assert!(
        !dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.step.suspend.reset"))
    );
    Ok(())
}

#[test]
fn test_lower_pre_resolves_encoded_matrix_element_to_declared_array() -> Result<(), ToDaeError> {
    let mut dae = dae::Dae::new();
    let mut memory = discrete_valued_var("dLATRAM.mem");
    memory.dims = vec![4, 2];
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("dLATRAM.mem"), memory);
    dae.discrete.valued_updates.push(dae::Equation::residual(
        pre_call("dLATRAM.mem[1,1]"),
        rumoca_core::Span::default(),
        "matrix memory read".to_string(),
    ));

    lower_pre_operator(&mut dae)?;

    let pre_memory_name = rumoca_core::VarName::new("__pre__.dLATRAM.mem");
    assert!(dae.variables.parameters.contains_key(&pre_memory_name));
    assert_eq!(dae.variables.parameters[&pre_memory_name].dims, vec![4, 2]);
    assert!(
        !dae.variables
            .parameters
            .contains_key(&rumoca_core::VarName::new("__pre__.dLATRAM.mem[1,1]"))
    );
    match &dae.discrete.valued_updates[0].rhs {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert_eq!(name.as_str(), "__pre__.dLATRAM.mem");
            assert_eq!(subscripts.len(), 2);
        }
        other => panic!("Expected indexed pre-memory VarRef, got {:?}", other),
    }
    Ok(())
}
