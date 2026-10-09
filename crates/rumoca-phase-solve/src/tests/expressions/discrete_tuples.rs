//! Complete tuple projections share their issued call occurrence, including
//! direct scalar and tensor results. Equal operands do not identify a call.

use super::*;

fn tuple_values<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    parameter: dae::FunctionParameterId<'dae>,
    at: dae::DaeProvenance,
    width: u32,
    minimum: bool,
) -> Result<(dae::ExprId<'dae>, dae::ExprId<'dae>), dae::DaeConstructionError> {
    let mut input = expressions.at(at).function_parameter(parameter)?;
    if minimum {
        let one = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
        let minus_one = expressions.at(at).literal(dae::DaeLiteral::Real(-1.0))?;
        let selected = expressions.at(at).conditional([(input, one)], minus_one)?;
        let array = expressions.at(at).array([selected, one])?;
        let minimum = expressions.at(at).builtin(dae::PureBuiltin::Min, [array])?;
        let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
        input = expressions
            .at(at)
            .binary(dae::BinaryOperator::Greater, minimum, zero)?;
    }
    Ok((
        expressions
            .at(at)
            .array(std::iter::repeat_n(input, width as usize))?,
        expressions.at(at).unary(dae::UnaryOperator::Not, input)?,
    ))
}

fn tuple_model(width: u32, occurrences: usize, minimum: bool) -> dae::Dae {
    let source = TestSource::new("function split; equation (flags, tail) = split(u);");
    let at = source.at(0, 49);
    dae::Dae::construct(source.map, |model| {
        let (boolean, array) = model.types(|types| {
            let boolean = types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?;
            let array =
                types.derived(dae::ValueType::array(dae::ScalarType::Boolean, [width]), at)?;
            Ok((boolean, array))
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("split"), [boolean], [array, boolean], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let parameter = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, at)
            })?;
            let flags = model.functions(|functions| {
                functions.output(&reservation, VarName::new("flags"), 0, at)
            })?;
            let tail = model.functions(|functions| {
                functions.output(&reservation, VarName::new("tail"), 1, at)
            })?;
            let (flags_value, tail_value) = model.expressions(|expressions| {
                tuple_values(expressions, parameter, at, width, minimum)
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, flags, flags_value, at))?;
            model.functions(|functions| functions.assign(&mut body, tail, tail_value, at))?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let input = model.variables(|variables| {
            variables.discrete_value(
                VarName::new("u"),
                boolean,
                at,
                dae::VariableAttributes {
                    causality: dae::VariableCausality::Input,
                    declared_causality: dae::DeclaredCausality::Input,
                    ..Default::default()
                },
            )
        })?;
        let input = model.expressions(|expressions| {
            expressions
                .at(at)
                .coordinate(dae::CoordinateInput::DiscreteValue(input))
        })?;
        let mut targets = Vec::new();
        let mut values = Vec::new();
        for occurrence in 0..occurrences {
            targets.extend(model.variables(|variables| {
                Ok([
                    variables.discrete_value(
                        VarName::new(format!("flags{occurrence}")),
                        array,
                        at,
                        Default::default(),
                    )?,
                    variables.discrete_value(
                        VarName::new(format!("tail{occurrence}")),
                        boolean,
                        at,
                        Default::default(),
                    )?,
                ])
            })?);
            let results = model.expressions(|expressions| {
                expressions.at(at).call_results(function, [0, 1], [input])
            })?;
            values.extend(results.into_iter().map(|value| (value, at)));
        }
        model.b1c(targets.iter().copied(), |topology| {
            topology.owner(at, targets.iter().copied(), |owner| {
                owner.always(at, values)
            })?;
            Ok(())
        })
    })
    .unwrap()
}

fn assert_tuple_programs(width: u32, occurrences: usize, minimum: bool) {
    let model = tuple_model(width, occurrences, minimum);
    let package = lower_solve_package(&model).unwrap();
    let rows = &package.problem.discrete.rhs;
    assert_eq!(
        rows.programs().len(),
        occurrences,
        "one program per issued call occurrence"
    );
    assert_eq!(package.pure_calls.owners().len(), occurrences);
    if minimum {
        assert!(
            package
                .pure_calls
                .owners()
                .iter()
                .all(|owner| owner.directional().is_none()),
            "this fixture requires no fabricated directional relation"
        );
    }
    for program in rows.programs() {
        assert_eq!(
            program
                .iter()
                .filter(|op| matches!(op, LinearOp::PureCall { .. }))
                .count(),
            1
        );
    }
    assert_eq!(
        rows.output_indices().len(),
        occurrences * (width as usize + 1)
    );
    for input in [0.0, 1.0] {
        let mut parameters = vec![0.0; package.problem.layout.p_scalars()];
        let ScalarSlot::P { index, .. } = package.problem.layout.binding("u").unwrap() else {
            panic!("the input owns a P slot");
        };
        parameters[index] = input;
        let actual =
            eval_residual_rows_with_pure_calls(rows, &package.pure_calls, &[], &parameters);
        let expected: Vec<_> = (0..occurrences)
            .flat_map(|_| std::iter::repeat_n(input, width as usize).chain([1.0 - input]))
            .collect();
        assert_eq!(actual, expected);
    }
}

#[test]
fn direct_discrete_tuple_results_execute_one_call_at_every_width() {
    for width in [4, 128] {
        assert_tuple_programs(width, 1, false);
    }
}

#[test]
fn identical_tuple_operands_do_not_merge_distinct_call_occurrences() {
    assert_tuple_programs(4, 2, false);
}

#[test]
fn discrete_tuple_needs_only_the_compact_primal_call() {
    assert_tuple_programs(4, 1, true);
}
