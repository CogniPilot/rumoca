//! Event lowering: state-event roots and event action programs.
//!
//! A continuous-time relation registered as a root owns a signed zero-crossing
//! program, and an event action Solve cannot compute is rejected at the exact
//! span of the construct that made it uncomputable.

use super::*;

/// SOLVE-C12 maps the typed DAE terminal coordinate to one runtime P-slot and
/// marks the event partition so the driver activates that slot only at stop
/// time. The action program therefore remains a pure load from `(y, p, t)`.
#[test]
fn terminal_coordinate_lowers_to_the_driver_owned_final_event_slot() {
    let source = TestSource::new(
        "Real x; equation der(x) = 0; when terminal() then terminate(\"done\"); end when;",
    );
    let declaration = source.at(0, 6);
    let equation = source.at(17, 27);
    let terminal_at = source.at(34, 44);
    let action_at = source.at(50, 67);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(equation)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let zero = expressions
                .at(equation)
                .literal(dae::DaeLiteral::Real(0.0))?;
            expressions
                .at(equation)
                .binary(dae::BinaryOperator::Subtract, derivative, zero)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation, residual))?;

        let terminal = model.temporal(|temporal| temporal.terminal(terminal_at))?;
        let terminal_value = model.expressions(|expressions| {
            expressions
                .at(terminal_at)
                .coordinate(dae::CoordinateInput::Terminal(terminal))
        })?;
        let activation = model.conditions(|conditions| conditions.reserve(terminal_at))?;
        model.conditions(|conditions| {
            conditions.define(
                activation,
                dae::ConditionInput::Discrete(terminal_value),
                terminal_at,
            )
        })?;
        let message = model.expressions(|expressions| {
            expressions
                .at(action_at)
                .literal(dae::DaeLiteral::String("done".to_owned()))
        })?;
        model.events(|events| events.terminate(activation, activation, message, action_at))?;
        Ok(())
    })
    .expect("typed terminal DAE is valid by construction");

    let solve = lower_solve_problem(&model).expect("terminal DAE has checked Solve lowering");
    let terminal_index = solve
        .solve_layout
        .terminal_event_parameter_index
        .expect("terminal runtime slot is allocated");
    assert!(solve.events.has_terminal_event);
    assert!(terminal_index < solve.layout.p_scalars());
    assert_eq!(solve.events.actions.len(), 1);
    assert_eq!(solve.events.action_conditions.programs().len(), 1);
}

#[test]
fn explicit_string_format_fails_at_its_exact_solve_lowering_span() {
    const STRING_DECLARATION: rumoca_core::DefId = rumoca_core::DefId(41);
    let source = TestSource::new(
        "Real x; equation x = 0; when true then \
         assert(true, String(1, format = \"04d\")); end when;",
    );
    let declaration_at = source.at(0, 6);
    let equation_at = source.at(17, 22);
    let condition_at = source.at(29, 33);
    let action_at = source.at(39, 78);
    let conversion_at = source.at(52, 77);
    let format_at = source.at(71, 76);
    let model = dae::Dae::construct(source.map, |model| {
        model.register_predefined_string(STRING_DECLARATION)?;
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration_at,
            )
        })?;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                real,
                declaration_at,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let variable = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let zero = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(0.0))?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, variable, zero)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))?;
        let condition = model.conditions(|conditions| conditions.reserve(condition_at))?;
        let condition_value = model.expressions(|expressions| {
            expressions
                .at(condition_at)
                .literal(dae::DaeLiteral::Boolean(true))
        })?;
        model.conditions(|conditions| {
            conditions.define(
                condition,
                dae::ConditionInput::Discrete(condition_value),
                condition_at,
            )
        })?;
        let message = model.expressions(|expressions| {
            let value = expressions
                .at(conversion_at)
                .literal(dae::DaeLiteral::Integer(1))?;
            let format = expressions
                .at(format_at)
                .literal(dae::DaeLiteral::String("04d".to_owned()))?;
            expressions.at(conversion_at).string_conversion(
                STRING_DECLARATION,
                value,
                dae::StringConversionFormatInput::Format { value: format },
            )
        })?;
        model.events(|events| events.assert(condition, condition, message, action_at))?;
        Ok(())
    })
    .expect("explicit formatting is valid checked DAE semantics");

    let error =
        lower_solve_problem(&model).expect_err("Solve IR cannot claim an uncomputable formatter");

    assert!(
        matches!(
            error,
            LowerError::Unsupported { ref reason, span }
                if span == format_at.span() && reason.contains("explicit String format")
        ),
        "unexpected lowering error: {error:?}"
    );
}

#[test]
fn primitive_relation_root_lowers_to_signed_event_program() {
    let source = TestSource::new("Real x; der(x) = -1; when x > 0 then end when;");
    let declaration = source.at(0, 6);
    let equation_owner = source.at(8, 19);
    let relation_owner = source.at(26, 31);
    let when_owner = source.at(21, 45);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let (residual, relation_expression) = model.expressions(|expressions| {
            let derivative = expressions
                .at(equation_owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let negative_one = expressions
                .at(equation_owner)
                .literal(dae::DaeLiteral::Real(-1.0))?;
            let residual = expressions.at(equation_owner).binary(
                dae::BinaryOperator::Subtract,
                derivative,
                negative_one,
            )?;
            let state = expressions
                .at(relation_owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            let zero = expressions
                .at(relation_owner)
                .literal(dae::DaeLiteral::Real(0.0))?;
            let relation =
                expressions
                    .at(relation_owner)
                    .binary(dae::BinaryOperator::Greater, state, zero)?;
            Ok((residual, relation))
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_owner, residual))?;
        let (relation, activation) = model.conditions(|conditions| {
            let relation = conditions.relation(relation_expression, relation_owner)?;
            let activation = conditions.reserve(when_owner)?;
            conditions.define(
                activation,
                dae::ConditionInput::Relation(relation),
                relation_owner,
            )?;
            Ok((relation, activation))
        })?;
        model.conditions(|conditions| conditions.root(relation, activation, when_owner))?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(solve.events.root_conditions.programs().len(), 1);
    assert_eq!(
        solve.events.root_conditions.program_spans(),
        [when_owner.span()]
    );
    assert_eq!(
        solve.events.root_zero_domains,
        [rumoca_ir_solve::RootZeroDomain::Positive]
    );
    assert!(solve.events.root_relation_memory_targets[0].is_none());
}

#[test]
fn exact_unconditional_b1c_relation_owns_the_root_post_side() {
    let source = TestSource::new(
        "discrete Boolean active; equation active = time > 0.5; when time > 0.5 then end when;",
    );
    let declaration = source.at(0, 23);
    let relation_owner = source.at(42, 52);
    let assignment_owner = source.at(33, 52);
    let when_owner = source.at(54, 84);
    let model = dae::Dae::construct(source.map, |model| {
        let boolean = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Boolean),
                declaration,
            )
        })?;
        let active = model.variables(|variables| {
            variables.discrete_value(
                VarName::new("active"),
                boolean,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let relation_expression = model.expressions(|expressions| {
            let time = expressions
                .at(relation_owner)
                .coordinate(dae::CoordinateInput::Time)?;
            let threshold = expressions
                .at(relation_owner)
                .literal(dae::DaeLiteral::Real(0.5))?;
            expressions
                .at(relation_owner)
                .binary(dae::BinaryOperator::Greater, time, threshold)
        })?;
        let (relation, activation) = model.conditions(|conditions| {
            let relation = conditions.relation(relation_expression, relation_owner)?;
            let activation = conditions.reserve(when_owner)?;
            conditions.define(
                activation,
                dae::ConditionInput::Relation(relation),
                relation_owner,
            )?;
            Ok((relation, activation))
        })?;
        model.conditions(|conditions| conditions.root(relation, activation, when_owner))?;
        model.b1c([active], |topology| {
            topology.owner(assignment_owner, [active], |owner| {
                owner.always(assignment_owner, [(relation_expression, assignment_owner)])
            })?;
            Ok(())
        })
    })
    .expect("exact relation owner is valid checked DAE");

    let solve = lower_solve_problem(&model).expect("exact relation owner lowers");

    assert_eq!(
        solve.events.root_relation_memory_targets,
        [Some(rumoca_ir_solve::scalar_slot_p(0))]
    );
}

#[test]
fn multiply_owned_relation_fails_closed_without_an_arbitrary_root_target() {
    let source = TestSource::new(
        "discrete Boolean a, b; equation a = time > 0.5; b = time > 0.5; when time > 0.5 then end when;",
    );
    let declaration = source.at(0, 22);
    let assignment_owner = source.at(33, 69);
    let relation_owner = source.at(37, 47);
    let when_owner = source.at(70, 94);
    let model = dae::Dae::construct(source.map, |model| {
        let boolean = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Boolean),
                declaration,
            )
        })?;
        let (a, b) = model.variables(|variables| {
            Ok((
                variables.discrete_value(
                    VarName::new("a"),
                    boolean,
                    declaration,
                    dae::VariableAttributes::default(),
                )?,
                variables.discrete_value(
                    VarName::new("b"),
                    boolean,
                    declaration,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let relation_expression = model.expressions(|expressions| {
            let time = expressions
                .at(relation_owner)
                .coordinate(dae::CoordinateInput::Time)?;
            let threshold = expressions
                .at(relation_owner)
                .literal(dae::DaeLiteral::Real(0.5))?;
            expressions
                .at(relation_owner)
                .binary(dae::BinaryOperator::Greater, time, threshold)
        })?;
        let (relation, activation) = model.conditions(|conditions| {
            let relation = conditions.relation(relation_expression, relation_owner)?;
            let activation = conditions.reserve(when_owner)?;
            conditions.define(
                activation,
                dae::ConditionInput::Relation(relation),
                relation_owner,
            )?;
            Ok((relation, activation))
        })?;
        model.conditions(|conditions| conditions.root(relation, activation, when_owner))?;
        model.b1c([a, b], |topology| {
            topology.owner(assignment_owner, [a, b], |owner| {
                owner.always(
                    assignment_owner,
                    [
                        (relation_expression, assignment_owner),
                        (relation_expression, assignment_owner),
                    ],
                )
            })?;
            Ok(())
        })
    })
    .expect("duplicate exact relation owners are valid checked DAE");

    let solve = lower_solve_problem(&model).expect("duplicate exact relation owners lower");

    assert_eq!(solve.events.root_relation_memory_targets, [None]);
}
