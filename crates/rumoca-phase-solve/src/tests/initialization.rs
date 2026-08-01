//! The initialization system: its runtime flags, continuation parameter, and unknowns.
//!
//! `initial()` and `homotopy(..)` each own one dedicated runtime parameter, and a
//! `parameter` declared `fixed = false` enters the initialization projection as an
//! unknown rather than staying a checked residual.

use super::*;

#[test]
fn initial_condition_owns_a_dedicated_runtime_flag() {
    let source = TestSource::new(
        "discrete Real x; when initial() then x = 1; elsewhen false then x = 2; end when;",
    );
    let declaration = source.at(0, 15);
    let initial_at = source.at(22, 31);
    let assignment = source.at(37, 42);
    let false_at = source.at(53, 58);
    let second_assignment = source.at(64, 69);
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
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let condition = model.conditions(|conditions| conditions.reserve(initial_at))?;
        model.conditions(|conditions| {
            conditions.define(condition, dae::ConditionInput::Initial, initial_at)
        })?;
        let false_value = model.expressions(|expressions| {
            expressions
                .at(false_at)
                .literal(dae::DaeLiteral::Boolean(false))
        })?;
        let otherwise = model.conditions(|conditions| conditions.reserve(false_at))?;
        model.conditions(|conditions| {
            conditions.define(
                otherwise,
                dae::ConditionInput::Discrete(false_value),
                false_at,
            )
        })?;
        let residual = model.expressions(|expressions| {
            let value = expressions
                .at(assignment)
                .literal(dae::DaeLiteral::Real(1.0))?;
            let target = expressions
                .at(assignment)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            expressions
                .at(assignment)
                .binary(dae::BinaryOperator::Subtract, target, value)
        })?;
        let second_residual = model.expressions(|expressions| {
            let value = expressions
                .at(second_assignment)
                .literal(dae::DaeLiteral::Real(2.0))?;
            let target = expressions
                .at(second_assignment)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            expressions
                .at(second_assignment)
                .binary(dae::BinaryOperator::Subtract, target, value)
        })?;
        model.discrete(|discrete| {
            discrete.when_real_equation(condition, condition, assignment, |equation| {
                equation.residual(residual)
            })?;
            discrete.when_real_equation(otherwise, otherwise, second_assignment, |equation| {
                equation.residual(second_residual)
            })?;
            Ok(())
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_grouped_initial_b1b(&solve);
}

fn assert_grouped_initial_b1b(solve: &rumoca_ir_solve::SolveProblem) {
    let flag = solve
        .solve_layout
        .initial_event_parameter_index
        .expect("initial() owns one checked runtime flag");
    let guarded_rows = solve
        .discrete
        .row_roles
        .iter()
        .enumerate()
        .filter(|(_, role)| **role == rumoca_ir_solve::DiscreteRowRole::Equation)
        .collect::<Vec<_>>();
    let [(guarded_row, _)] = guarded_rows.as_slice() else {
        panic!("one conditional B.1b equation row expected");
    };
    assert_eq!(
        solve.discrete.rhs.programs()[*guarded_row]
            .iter()
            .filter(|operation| matches!(operation, LinearOp::Select { .. }))
            .count(),
        2
    );
    assert!(
        solve
            .discrete
            .rhs
            .programs()
            .iter()
            .flatten()
            .any(|operation| matches!(operation, LinearOp::LoadP { index, .. } if *index == flag))
    );
}

#[test]
fn homotopy_owns_a_dedicated_continuation_parameter() {
    let source = TestSource::new("Real x; der(x) = homotopy(x*x, x);");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 34);
    let homotopy_at = source.at(17, 33);
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
                .at(owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let x = expressions
                .at(homotopy_at)
                .coordinate(dae::CoordinateInput::State(state))?;
            let actual = expressions
                .at(homotopy_at)
                .binary(dae::BinaryOperator::Multiply, x, x)?;
            let homotopy = expressions
                .at(homotopy_at)
                .builtin(dae::PureBuiltin::Homotopy, [actual, x])?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, derivative, homotopy)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    model.inspect(|view| {
        let expression = (0..view.expression_count())
            .filter_map(|index| view.expression(view.expression_id(index)?))
            .find(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Builtin {
                        builtin: dae::PureBuiltin::Homotopy,
                        ..
                    }
                )
            })
            .expect("checked DAE retains the homotopy node");
        assert_eq!(
            view.source_text(expression.provenance()),
            Some("homotopy(x*x, x)")
        );
    });
    let solve = lower_solve_problem(&model).unwrap();
    let lambda = solve
        .solve_layout
        .initial_homotopy_parameter_index
        .expect("homotopy owns one checked continuation parameter");
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.derivative_rhs.nodes.as_slice()
    else {
        panic!("one scalar derivative block expected");
    };
    assert!(
        rows.programs()[0].iter().any(
            |operation| matches!(operation, LinearOp::LoadP { index, .. } if *index == lambda)
        )
    );
}

/// MLS §8.6: a `parameter` declared `fixed = false` is determined by the initialization
/// system, so it must appear there as an unknown with its own projection block. Without
/// one the residual can only be checked, which is what stalled
/// `Modelica.Electrical.Analog.Examples.ShowSaturatingInductor` on
/// `SaturatingInductor.Ipar`.
#[test]
fn fixed_false_parameter_becomes_an_initialization_projection_unknown() {
    let source = TestSource::new("parameter Real q(fixed=false); initial equation q*q=4;");
    let declaration = source.at(0, 29);
    let owner = source.at(48, 53);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let start = model.expressions(|expressions| {
            expressions
                .at(declaration)
                .literal(dae::DaeLiteral::Real(3.0))
        })?;
        let unsolved = model.variables(|variables| {
            variables.parameter(
                VarName::new("q"),
                real,
                declaration,
                dae::VariableAttributes {
                    start: Some(start),
                    fixed: Some(false),
                    ..dae::VariableAttributes::default()
                },
            )
        })?;
        let residual = model.expressions(|expressions| {
            let left = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(unsolved))?;
            let right = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(unsolved))?;
            let square =
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Multiply, left, right)?;
            let four = expressions.at(owner).literal(dae::DaeLiteral::Real(4.0))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, square, four)
        })?;
        model.initialization(|initialization| initialization.value_equation(owner, residual))?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    solve
        .validate()
        .expect("the initialization projection satisfies the Solve shape contract");
    let ScalarSlot::P { index, .. } = solve
        .layout
        .binding("q")
        .expect("the unsolved parameter keeps its parameter storage")
    else {
        panic!("a parameter occupies P storage");
    };
    let [block] = solve.initialization.projection_plan.blocks.as_slice() else {
        panic!(
            "one initialization projection block expected, got {:?}",
            solve.initialization.projection_plan.blocks
        );
    };
    assert_eq!(block.rows, [0]);
    assert_eq!(block.unknowns, [rumoca_ir_solve::scalar_slot_p(index)]);
    assert_eq!(
        solve.initialization.projection_unknowns,
        [rumoca_ir_solve::scalar_slot_p(index)]
    );
}
