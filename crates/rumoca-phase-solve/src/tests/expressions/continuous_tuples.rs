//! Direct tuple results retain one call across separate continuous owners.

use super::*;

fn continuous_tuple_model() -> dae::Dae {
    let source =
        TestSource::new("function split; Real a[2]; Real gap; Real b; equation (a,b)=split(time);");
    let at = source.at(0, 68);
    dae::Dae::construct(source.map, |model| {
        let (real, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2]), at)?,
            ))
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("split"), [real], [vector, real], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let input = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, at)
            })?;
            let array = model
                .functions(|functions| functions.output(&reservation, VarName::new("a"), 0, at))?;
            let scalar = model
                .functions(|functions| functions.output(&reservation, VarName::new("b"), 1, at))?;
            let (array_value, scalar_value) = model.expressions(|expressions| {
                let input = expressions.at(at).function_parameter(input)?;
                Ok((
                    expressions.at(at).array([input, input])?,
                    expressions
                        .at(at)
                        .unary(dae::UnaryOperator::Negate, input)?,
                ))
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, array, array_value, at))?;
            model.functions(|functions| functions.assign(&mut body, scalar, scalar_value, at))?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let (a, gap, b) = model.variables(|variables| {
            Ok((
                variables.algebraic(VarName::new("a"), vector, at, Default::default())?,
                variables.algebraic(VarName::new("gap"), real, at, Default::default())?,
                variables.algebraic(VarName::new("b"), real, at, Default::default())?,
            ))
        })?;
        let (first, middle, last) = model.expressions(|expressions| {
            let time = expressions.at(at).coordinate(dae::CoordinateInput::Time)?;
            let results = expressions.at(at).call_results(function, [0, 1], [time])?;
            let a = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(a))?;
            let gap = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(gap))?;
            let b = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(b))?;
            Ok((
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, a, results[0])?,
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, gap, time)?,
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, b, results[1])?,
            ))
        })?;
        model.continuous(|continuous| {
            continuous.value_equation(at, first)?;
            continuous.value_equation(at, middle)?;
            continuous.value_equation(at, last)
        })
    })
    .unwrap()
}

#[test]
fn direct_continuous_tuple_results_share_a_call_across_separated_rows() {
    let model = continuous_tuple_model();
    let package = lower_solve_package(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = package.problem.continuous.residual.nodes.as_slice()
    else {
        panic!("one residual block");
    };
    assert_eq!(
        rows.programs().len(),
        2,
        "one tuple program plus the intervening independent row"
    );
    assert_eq!(
        rows.programs()
            .iter()
            .flatten()
            .filter(|op| matches!(op, LinearOp::PureCall { .. }))
            .count(),
        1
    );
    assert_eq!(package.pure_calls.owners().len(), 1);
    let mut output = [0.0; 4];
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        rows,
        &[10.0, 20.0, 30.0, 40.0],
        &[],
        3.0,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(&package.pure_calls),
            ..Default::default()
        },
        &mut output,
    )
    .unwrap();
    assert_eq!(
        output,
        [7.0, 17.0, 27.0, 43.0],
        "canonical row and tuple result ordering"
    );
}
