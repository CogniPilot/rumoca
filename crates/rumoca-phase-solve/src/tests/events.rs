//! Event lowering: state-event roots and event action programs.
//!
//! A continuous-time relation registered as a root owns a signed zero-crossing
//! program, and an event action Solve cannot compute is rejected at the exact
//! span of the construct that made it uncomputable.

use super::*;

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
