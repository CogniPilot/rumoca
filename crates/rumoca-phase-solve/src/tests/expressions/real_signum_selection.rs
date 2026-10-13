//! A model conditional whose arms apply `sign` to a Real is an eager selection.
//!
//! `sign` of a Real is computed as the Real-valued signum on the scalar path,
//! so it cannot fault and the conditional needs no selected-arm program.

use super::*;

fn conditional_signum_model() -> dae::Dae {
    let source = TestSource::new("Real x; x = if time > 0.5 then sign(x) else 2;");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 40);
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let x = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let time = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Time)?;
            let half = expressions.at(owner).literal(dae::DaeLiteral::Real(0.5))?;
            let guard = expressions
                .at(owner)
                .binary(dae::BinaryOperator::Greater, time, half)?;
            let signum = expressions.at(owner).builtin(dae::PureBuiltin::Sign, [x])?;
            let two = expressions.at(owner).literal(dae::DaeLiteral::Real(2.0))?;
            let selected = expressions.at(owner).conditional([(guard, signum)], two)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, x, selected)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap()
}

#[test]
fn a_conditional_over_the_signum_of_a_real_lowers_as_an_eager_selection() {
    let model = conditional_signum_model();
    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    let program = &rows.programs()[0];
    assert!(
        program
            .iter()
            .any(|operation| matches!(operation, LinearOp::Select { .. }))
    );
    assert!(
        program
            .iter()
            .all(|operation| !matches!(operation, LinearOp::FunctionConditional { .. })),
        "a total conditional is not a selected-arm program"
    );
}
