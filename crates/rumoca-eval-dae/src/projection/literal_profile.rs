//! Immutable Integer literal facts available before the first summary walk.
use super::{IntegerBinding, dae};

/// A bounded set of literal cells; all remaining facts retain lazy discovery.
/// No parameter binding, runtime value or function execution proves a literal.
pub(super) fn arguments<'dae>(
    view: dae::DaeView<'dae>,
    arguments: &[dae::ExprId<'dae>],
) -> Vec<IntegerBinding> {
    let mut result = Vec::new();
    for (parameter, &argument) in arguments.iter().enumerate() {
        let node = view
            .expression(argument)
            .expect("checked call argument resolves");
        if node.value_type().scalar_type() != dae::ScalarType::Integer {
            continue;
        }
        let count = node
            .value_type()
            .scalar_count()
            .expect("checked Integer argument has fixed shape");
        if result.len().saturating_add(count) > 64 {
            continue;
        }
        for scalar in 0..count {
            if let Some(value) = integer(view, argument, scalar) {
                result.push(IntegerBinding {
                    parameter: u32::try_from(parameter).expect("checked argument ordinal fits u32"),
                    scalar,
                    value,
                });
            }
        }
    }
    result
}

fn integer<'dae>(
    view: dae::DaeView<'dae>,
    mut expression: dae::ExprId<'dae>,
    mut scalar: usize,
) -> Option<i64> {
    loop {
        match view.expression(expression)?.operation() {
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) => {
                return (scalar == 0).then_some(*value);
            }
            dae::ExpressionOperation::Array(elements) => {
                let first = elements.get(0)?;
                let width = view.expression(first)?.value_type().scalar_count()?;
                if width == 0 {
                    return None;
                }
                expression = elements.get(scalar / width)?;
                scalar %= width;
            }
            _ => return None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{SourceMap, Span, VarName};

    #[test]
    fn only_literal_cells_issue_profiles_and_large_or_tunable_arguments_stay_lazy() {
        let mut sources = SourceMap::new();
        let source = sources.add(
            "literal_profiles.mo",
            "Integer arrays and a tunable parameter",
        );
        let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 10)).unwrap();
        let mut arguments = Vec::new();
        let model = dae::Dae::construct(sources, |model| {
            let integer = model.types(|types| {
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)
            })?;
            let values = model.expressions(|expressions| {
                let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
                let three = expressions.at(at).literal(dae::DaeLiteral::Integer(3))?;
                let small = expressions.at(at).array([one, three])?;
                let large = expressions.at(at).array(vec![one; 65])?;
                let nine = expressions.at(at).literal(dae::DaeLiteral::Integer(9))?;
                Ok([small, large, nine])
            })?;
            let parameter = model.variables(|variables| {
                variables.parameter(
                    VarName::new("p"),
                    integer,
                    at,
                    dae::VariableAttributes {
                        binding: Some(values[2]),
                        is_tunable: true,
                        ..Default::default()
                    },
                )
            })?;
            let tunable = model.expressions(|expressions| {
                expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Parameter(parameter))
            })?;
            arguments.extend(values.into_iter().chain([tunable]).map(|id| id.index()));
            Ok(())
        })
        .unwrap();
        model.inspect(|view| {
            let arguments = arguments
                .iter()
                .map(|id| view.expression_id(*id as usize).unwrap())
                .collect::<Vec<_>>();
            assert_eq!(
                super::arguments(view, &arguments),
                vec![
                    IntegerBinding {
                        parameter: 0,
                        scalar: 0,
                        value: 1
                    },
                    IntegerBinding {
                        parameter: 0,
                        scalar: 1,
                        value: 3
                    },
                    IntegerBinding {
                        parameter: 2,
                        scalar: 0,
                        value: 9
                    },
                ]
            );
        });
    }
}
