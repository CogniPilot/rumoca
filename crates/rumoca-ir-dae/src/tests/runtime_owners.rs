use super::*;

#[test]
fn delay_owner_rejects_invalid_evidence_and_coordinate_provenance_atomically() {
    let source = TestSource::new("delay(x, 0.0)");
    let owner = source.source("delay(x, 0.0)", 0);
    let literal_at = source.source("0.0", 0);
    let foreign_span = Span::from_offsets(SourceId::from_source_name("foreign.mo"), 0, 1);
    let foreign = DaeProvenance::source(foreign_span).expect("foreign span is not the dummy span");
    let dae = Dae::construct(source.map, |dae| {
        let (source, delay_time) = dae.expressions(|expressions| {
            Ok((
                expressions.at(owner).literal(DaeLiteral::Real(1.0))?,
                expressions.at(literal_at).literal(DaeLiteral::Real(0.0))?,
            ))
        })?;
        let positive = dae.temporal(|temporal| {
            let rejected = temporal.positive_parameter(delay_time, 0.0, literal_at);
            assert!(matches!(
                rejected,
                Err(DaeConstructionError::InvalidPositiveParameter { .. })
            ));
            temporal.positive_parameter(source, 1.0, owner)
        })?;
        dae.expressions(|expressions| {
            let rejected = expressions.at(foreign).delay(source, positive, owner);
            assert!(matches!(
                rejected,
                Err(DaeConstructionError::UnknownSource { span }) if span == foreign_span
            ));
            Ok(())
        })
    })
    .expect("a rejected capability cannot leave a partial delay owner");
    dae.inspect(|view| {
        assert_eq!(view.delay_count(), 0);
        assert_eq!(view.expression_count(), 2);
    });
}

#[test]
fn delay_operand_errors_use_exact_occurrence_spans_before_insertion() {
    let source = TestSource::new(r#"delay("bad", 1.0); delay(1.0, 7, 2.0)"#);
    let string_at = source.source(r#""bad""#, 0);
    let fixed_time_at = source.source("1.0", 0);
    let fixed_owner = source.source(r#"delay("bad", 1.0)"#, 0);
    let real_at = source.source("1.0", 1);
    let integer_time_at = source.source("7", 0);
    let maximum_at = source.source("2.0", 0);
    let bounded_owner = source.source("delay(1.0, 7, 2.0)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let (string, fixed_time, real, integer_time, maximum) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(string_at)
                    .literal(DaeLiteral::String("bad".to_owned()))?,
                expressions
                    .at(fixed_time_at)
                    .literal(DaeLiteral::Real(1.0))?,
                expressions.at(real_at).literal(DaeLiteral::Real(1.0))?,
                expressions
                    .at(integer_time_at)
                    .literal(DaeLiteral::Integer(7))?,
                expressions.at(maximum_at).literal(DaeLiteral::Real(2.0))?,
            ))
        })?;
        let fixed =
            dae.temporal(|temporal| temporal.positive_parameter(fixed_time, 1.0, fixed_time_at))?;
        let rejected = dae.expressions(|expressions| {
            expressions
                .at(fixed_owner)
                .delay(string, fixed, fixed_owner)
        });
        assert!(matches!(
            rejected,
            Err(DaeConstructionError::ExpectedNumeric { span, .. })
                if span == string_at.span()
        ));

        let maximum =
            dae.temporal(|temporal| temporal.positive_parameter(maximum, 2.0, maximum_at))?;
        let rejected = dae.expressions(|expressions| {
            expressions
                .at(bounded_owner)
                .bounded_delay(real, integer_time, maximum, bounded_owner)
        });
        assert!(matches!(
            rejected,
            Err(DaeConstructionError::TypeMismatch { span, .. })
                if span == integer_time_at.span()
        ));
        Ok(())
    })
    .expect("operand failures do not leave partial delay owners or coordinates");
    dae.inspect(|view| {
        assert_eq!(view.delay_count(), 0);
        assert_eq!(view.expression_count(), 5);
    });
}

#[test]
fn quotient_construction_rejects_dynamic_and_undefined_operands() {
    let source = TestSource::new("Real x; div(x, 2); mod(7, 0)");
    let declaration = source.source("Real x", 0);
    let x_use = source.source("x", 1);
    let two_use = source.source("2", 0);
    let dynamic_owner = source.source("div(x, 2)", 0);
    let seven_use = source.source("7", 0);
    let zero_use = source.source("0", 0);
    let undefined_owner = source.source("mod(7, 0)", 0);
    Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let x = dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (x_use, two, seven, zero) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(x_use)
                    .coordinate(CoordinateInput::Algebraic(x))?,
                expressions.at(two_use).literal(DaeLiteral::Integer(2))?,
                expressions.at(seven_use).literal(DaeLiteral::Integer(7))?,
                expressions.at(zero_use).literal(DaeLiteral::Integer(0))?,
            ))
        })?;
        let dynamic = dae.expressions(|expressions| {
            expressions
                .at(dynamic_owner)
                .builtin(PureBuiltin::Div, [x_use, two])
        });
        assert!(matches!(
            dynamic,
            Err(DaeConstructionError::NonStaticDiscontinuity { span, .. })
                if span == dynamic_owner.span()
        ));
        let undefined = dae.expressions(|expressions| {
            expressions
                .at(undefined_owner)
                .builtin(PureBuiltin::Mod, [seven, zero])
        });
        assert!(matches!(
            undefined,
            Err(DaeConstructionError::UndefinedBuiltinDomain { span, .. })
                if span == undefined_owner.span()
        ));
        Ok(())
    })
    .unwrap();
}

#[test]
fn runtime_mod_constructs_one_checked_synthetic_root() {
    let source = TestSource::new("Real x; mod(x, 2)");
    let declaration = source.source("Real x", 0);
    let x_at = source.source("x", 1);
    let two_at = source.source("2", 0);
    let mod_at = source.source("mod(x, 2)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let x = dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (x, two) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(x_at)
                    .coordinate(CoordinateInput::Algebraic(x))?,
                expressions.at(two_at).literal(DaeLiteral::Integer(2))?,
            ))
        })?;

        dae.runtime_quotient(PureBuiltin::Mod, [x, two], mod_at)?;
        Ok(())
    })
    .expect("runtime mod with a proven nonzero divisor owns its event surface");

    dae.inspect(|view| {
        assert_eq!(view.root_count(), 1);
        let root = view
            .root(view.root_id(0).expect("dense synthetic root identity"))
            .expect("checked synthetic root");
        assert_eq!(
            root.provenance().origin(),
            DaeProvenanceOrigin::Generated(DaeGeneration::RuntimeDiscontinuity)
        );
        assert!(matches!(
            view.condition(root.activation())
                .expect("checked root activation")
                .operation(),
            ConditionOperation::Always
        ));
        let relation = view
            .relation(root.relation())
            .expect("checked synthetic relation");
        assert!(matches!(
            view.expression(relation.expression())
                .expect("checked relation expression")
                .operation(),
            ExpressionOperation::Binary {
                operator: BinaryOperator::GreaterEqual,
                ..
            }
        ));
    });
}

#[test]
fn roots_accept_only_closed_primitive_relations() {
    let source = TestSource::new("Real x; when x > 0 then end when;");
    let declaration = source.source("Real x", 0);
    let x_use = source.source("x", 1);
    let zero_use = source.source("0", 0);
    let relation_owner = source.source("x > 0", 0);
    let when_owner = source.source("when x > 0 then end when", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let x = dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (relation_expression, boolean_literal) = dae.expressions(|expressions| {
            let x = expressions
                .at(x_use)
                .coordinate(CoordinateInput::Algebraic(x))?;
            let zero = expressions.at(zero_use).literal(DaeLiteral::Real(0.0))?;
            let relation =
                expressions
                    .at(relation_owner)
                    .binary(BinaryOperator::Greater, x, zero)?;
            let literal = expressions
                .at(when_owner)
                .literal(DaeLiteral::Boolean(true))?;
            Ok((relation, literal))
        })?;

        let rejected =
            dae.conditions(|conditions| conditions.relation(boolean_literal, when_owner));
        assert!(matches!(
            rejected,
            Err(DaeConstructionError::ExpectedPrimitiveRelation { .. })
        ));

        let (relation, activation) = dae.conditions(|conditions| {
            let relation = conditions.relation(relation_expression, relation_owner)?;
            let activation = conditions.reserve(when_owner)?;
            conditions.define(
                activation,
                ConditionInput::Relation(relation),
                relation_owner,
            )?;
            Ok((relation, activation))
        })?;
        dae.conditions(|conditions| conditions.root(relation, activation, when_owner))?;
        Ok(())
    })
    .expect("the root system enforces primitive and closed inputs");

    dae.inspect(|view| {
        assert_eq!(view.relation_count(), 1);
        assert_eq!(view.condition_count(), 1);
        assert_eq!(view.root_count(), 1);
    });

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.relation_count(), 1);
        assert_eq!(view.condition_count(), 1);
        assert_eq!(view.root_count(), 1);
    });
}

#[test]
fn event_actions_are_guarded_typed_and_keep_coincident_time_ids() {
    let source = TestSource::new(
        "Real x; when trigger then reinit(x, 1); assert(trigger, \"safe\"); end when;",
    );
    let declaration = source.source("Real x", 0);
    let guard_owner = source.source("trigger", 0);
    let action_owner = source.source("reinit(x, 1)", 0);
    let assert_owner = source.source("assert(trigger, \"safe\")", 0);
    let value_owner = source.source("1", 0);
    let message_owner = source.source("\"safe\"", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let state = dae.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (trigger, branch_guard) = dae.conditions(|conditions| {
            Ok((
                conditions.reserve(guard_owner)?,
                conditions.reserve(action_owner)?,
            ))
        })?;
        let (trigger_value, branch_value, value, message) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(guard_owner)
                    .literal(DaeLiteral::Boolean(true))?,
                expressions
                    .at(action_owner)
                    .literal(DaeLiteral::Boolean(false))?,
                expressions.at(value_owner).literal(DaeLiteral::Real(1.0))?,
                expressions
                    .at(message_owner)
                    .literal(DaeLiteral::String("safe".to_string()))?,
            ))
        })?;
        dae.conditions(|conditions| {
            conditions.define(
                trigger,
                ConditionInput::Discrete(trigger_value),
                guard_owner,
            )?;
            conditions.define(
                branch_guard,
                ConditionInput::Discrete(branch_value),
                action_owner,
            )
        })?;
        dae.events(|events| {
            let instant = ClockRational::new(1, 2).unwrap();
            let first = events.time_event(instant, action_owner)?;
            let second = events.time_event(instant, action_owner)?;
            assert_ne!(first.index(), second.index());
            events.reinitialize(trigger, branch_guard, state, value, action_owner)?;
            events.assert(trigger, branch_guard, message, assert_owner)?;
            Ok(())
        })
    })
    .expect("event actions are checked at their owner boundary");

    dae.inspect(|view| {
        assert_eq!(view.time_event_count(), 2);
        assert_eq!(view.event_action_count(), 2);
        assert_eq!(
            view.time_event(view.time_event_id(0).unwrap())
                .unwrap()
                .instant(),
            &ClockRational::new(1, 2).unwrap()
        );
        let reinitialize = view.event_action(view.event_action_id(0).unwrap()).unwrap();
        assert_ne!(reinitialize.trigger().index(), reinitialize.guard().index());
        assert!(matches!(
            reinitialize.operation(),
            EventActionOperation::Reinitialize { .. }
        ));
        let assertion = view.event_action(view.event_action_id(1).unwrap()).unwrap();
        assert!(matches!(
            assertion.operation(),
            EventActionOperation::Assert { .. }
        ));
    });
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.time_event_count(), 2);
        assert_eq!(view.event_action_count(), 2);
        let action = view.event_action(view.event_action_id(0).unwrap()).unwrap();
        assert_ne!(action.trigger().index(), action.guard().index());
    });

    assert_event_wire_requires_trigger(&encoded);
}

#[test]
fn initial_condition_is_typed_and_round_trips_through_wire_v12() {
    let source = TestSource::new("when initial() then end when;");
    let initial_at = source.source("initial()", 0);
    let dae = Dae::construct(source.map, |dae| {
        let condition = dae.conditions(|conditions| conditions.reserve(initial_at))?;
        dae.conditions(|conditions| {
            conditions.define(condition, ConditionInput::Initial, initial_at)
        })
    })
    .unwrap();

    let assert_initial = |model: &Dae| {
        model.inspect(|view| {
            let condition = view
                .condition(view.condition_id(0).unwrap())
                .expect("initial condition identity resolves");
            assert!(matches!(condition.operation(), ConditionOperation::Initial));
            assert_eq!(model.source_text(condition.provenance()), Some("initial()"));
        });
    };
    assert_initial(&dae);
    let wire = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&wire).unwrap();
    assert_initial(&decoded);
}

/// `Always` and `AnyRise` survive the wire together, at their own ordinals.
///
/// The two were added in the same change and land on opposite sides of the
/// ordinal-tagged encoding: `Always` was *inserted* at ordinal 1, shifting every
/// later condition variant, and `AnyRise` was appended. A round trip that
/// carries only one of them cannot catch a mis-shifted table, because a payload
/// written with the old ordinals still decodes — to the wrong variant. This
/// therefore builds a model holding both plus the neighbours the insertion moved
/// (`Relation`, `Discrete`, `Not`, `Or`) and asserts every node comes back as
/// itself, with its provenance.
///
/// `DAE_SCHEMA_VERSION` is bumped for the same reason; see its doc comment.
#[test]
fn always_and_any_rise_round_trip_through_wire_v14() {
    let source = TestSource::new("when {u, not u} then end when; algorithm x := 1;");
    let vector_at = source.source("{u, not u}", 0);
    let element_at = source.source("u", 0);
    let negated_at = source.source("not u", 0);
    let section_at = source.source("algorithm x := 1;", 0);
    let dae = Dae::construct(source.map, |dae| {
        // `u` and `not u`: a discrete leaf and its negation.
        let u_value = dae.expressions(|expressions| {
            expressions
                .at(element_at)
                .literal(DaeLiteral::Boolean(true))
        })?;
        let u = dae.conditions(|conditions| conditions.reserve(element_at))?;
        dae.conditions(|conditions| {
            conditions.define(u, ConditionInput::Discrete(u_value), element_at)
        })?;
        let not_u = dae.conditions(|conditions| conditions.reserve(negated_at))?;
        dae.conditions(|conditions| conditions.define(not_u, ConditionInput::Not(u), negated_at))?;
        // The vector activation over the two, and the disjunction it is *not*.
        let any_rise = dae.conditions(|conditions| conditions.reserve(vector_at))?;
        dae.conditions(|conditions| {
            conditions.define(any_rise, ConditionInput::AnyRise(u, not_u), vector_at)
        })?;
        let disjunction = dae.conditions(|conditions| conditions.reserve(vector_at))?;
        dae.conditions(|conditions| {
            conditions.define(disjunction, ConditionInput::Or(u, not_u), vector_at)
        })?;
        // The activation an unguarded algorithm section carries.
        let always = dae.conditions(|conditions| conditions.reserve(section_at))?;
        dae.conditions(|conditions| conditions.define(always, ConditionInput::Always, section_at))
    })
    .unwrap();

    let assert_nodes = |model: &Dae| {
        model.inspect(|view| {
            let node = |index: usize| {
                view.condition(view.condition_id(index).unwrap())
                    .expect("checked condition identity resolves")
            };
            assert!(matches!(
                node(0).operation(),
                ConditionOperation::Discrete(_)
            ));
            let ConditionOperation::Not(operand) = node(1).operation() else {
                panic!("condition 1 must stay a negation");
            };
            assert_eq!(operand.index(), 0);
            let ConditionOperation::AnyRise(lhs, rhs) = node(2).operation() else {
                panic!("condition 2 must stay a vector activation, not a disjunction");
            };
            assert_eq!((lhs.index(), rhs.index()), (0, 1));
            let ConditionOperation::Or(lhs, rhs) = node(3).operation() else {
                panic!("condition 3 must stay a disjunction, not a vector activation");
            };
            assert_eq!((lhs.index(), rhs.index()), (0, 1));
            assert!(matches!(node(4).operation(), ConditionOperation::Always));
            assert_eq!(model.source_text(node(2).provenance()), Some("{u, not u}"));
            assert_eq!(
                model.source_text(node(4).provenance()),
                Some("algorithm x := 1;")
            );
        });
    };
    assert_nodes(&dae);
    let wire = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&wire).unwrap();
    assert_eq!(decoded.schema_version(), DAE_SCHEMA_VERSION);
    assert_nodes(&decoded);
}

fn assert_event_wire_requires_trigger(encoded: &str) {
    let mut value: serde_json::Value = serde_json::from_str(encoded).unwrap();
    value["storage"]["event_actions"][0]
        .as_object_mut()
        .unwrap()
        .remove("trigger");
    let error = serde_json::from_value::<Dae>(value).unwrap_err();
    assert!(error.to_string().contains("missing field `trigger`"));
}
