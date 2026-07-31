//! Discrete `Real` partition rows: roles, causality, and computability.
//!
//! A discrete `Real` definition needs no clock to own a row, a coupled row is
//! oriented by the coordinate the rest of the partition leaves undefined, and a
//! partition that admits more than one causality is reported instead of guessed.

use super::*;

#[test]
fn unconditional_discrete_real_definition_does_not_require_a_clock() {
    let source = TestSource::new("discrete Real d; d=time;");
    let declaration = source.at(0, 15);
    let owner = source.at(17, 23);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let variable = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("d"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            let time = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Time)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, time)
        })?;
        model.discrete(|discrete| {
            discrete.real_equation(owner, |equation| equation.residual(residual))
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(
        solve.discrete.row_roles,
        [rumoca_ir_solve::DiscreteRowRole::Equation]
    );
    assert_eq!(solve.discrete.clock_owners, [None]);
}

/// Builds `a = <time>` plus `rows` further discrete `Real` residuals over the
/// pair `(a, b)`, so a test can state exactly which rows the partition owns.
fn discrete_real_pair_model(
    source: TestSource,
    couple_rows: usize,
    seed_first_row: bool,
) -> dae::Dae {
    let declaration = source.at(0, 6);
    let owner = source.at(8, 20);
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let first = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("a"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let second = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("b"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        if seed_first_row {
            let seeded = model.expressions(|expressions| {
                let target = expressions
                    .at(owner)
                    .coordinate(dae::CoordinateInput::DiscreteReal(first))?;
                let time = expressions
                    .at(owner)
                    .coordinate(dae::CoordinateInput::Time)?;
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Subtract, target, time)
            })?;
            model.discrete(|discrete| {
                discrete.real_equation(owner, |equation| equation.residual(seeded))
            })?;
        }
        for _ in 0..couple_rows {
            let coupled = model.expressions(|expressions| {
                let lhs = expressions
                    .at(owner)
                    .coordinate(dae::CoordinateInput::DiscreteReal(first))?;
                let rhs = expressions
                    .at(owner)
                    .coordinate(dae::CoordinateInput::DiscreteReal(second))?;
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Subtract, lhs, rhs)
            })?;
            model.discrete(|discrete| {
                discrete.real_equation(owner, |equation| equation.residual(coupled))
            })?;
        }
        Ok(())
    })
    .unwrap()
}

/// A connection row between two clocked coordinates states only that they are
/// equal, so its causality comes from the rest of the partition: `a` is already
/// defined by `a = time`, which leaves `b` as the row's only target.
#[test]
fn coupled_discrete_real_row_is_oriented_by_the_coordinate_left_undefined() {
    let source = TestSource::new("Real a; a=time; a=b;");
    let model = discrete_real_pair_model(source, 1, true);

    let solve = lower_solve_problem(&model).unwrap();
    let first = solve.layout.binding("a").expect("`a` owns a runtime slot");
    let second = solve.layout.binding("b").expect("`b` owns a runtime slot");
    assert_ne!(first, second);
    assert_eq!(solve.discrete.update_targets, [first, second]);
}

/// Two identical connection rows leave both coordinates undefined, so neither
/// row is ever forced. The partition admits more than one causality and is
/// reported instead of guessed.
#[test]
fn ambiguous_coupled_discrete_real_rows_are_reported_before_runtime() {
    let source = TestSource::new("Real a; Real b; a=b; a=b;");
    let model = discrete_real_pair_model(source, 2, false);

    let error = lower_solve_problem(&model).unwrap_err();
    assert!(
        matches!(
            &error,
            LowerError::NonComputable { reason, .. }
                if reason.contains("coupled discrete Real residual")
        ),
        "unforced discrete Real rows must be reported: {error}"
    );
}

#[test]
fn nonlinear_conditional_discrete_residual_fails_before_runtime() {
    let source = TestSource::new(
        "discrete Real z; Clock c=Clock(0.1); when c then z=3*pre(z)-z^2; end when;",
    );
    let declaration = source.at(0, 15);
    let clock_at = source.at(25, 35);
    let condition_at = source.at(44, 45);
    let owner = source.at(51, 65);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let variable = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("z"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock.into(), variable, owner))?;
        let condition = model.conditions(|conditions| conditions.reserve(condition_at))?;
        model.conditions(|conditions| {
            conditions.define(
                condition,
                dae::ConditionInput::Clock(clock.into()),
                condition_at,
            )
        })?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            let previous = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::PreDiscreteReal(variable))?;
            let three = expressions.at(owner).literal(dae::DaeLiteral::Real(3.0))?;
            let scaled_previous =
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Multiply, three, previous)?;
            let two = expressions.at(owner).literal(dae::DaeLiteral::Real(2.0))?;
            let squared = expressions
                .at(owner)
                .binary(dae::BinaryOperator::Power, target, two)?;
            let value = expressions.at(owner).binary(
                dae::BinaryOperator::Subtract,
                scaled_previous,
                squared,
            )?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, value)
        })?;
        model.discrete(|discrete| {
            discrete.when_real_equation(condition, condition, owner, |equation| {
                equation.residual(residual)
            })
        })?;
        Ok(())
    })
    .unwrap();

    let error = lower_solve_problem(&model).unwrap_err();
    assert!(matches!(
        error,
        LowerError::NonComputable { span, .. } if span == owner.span()
    ));
}
