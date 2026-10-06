//! Integer subscripts computed by a function call select through its result.
use super::*;

/// `next(i) = i + 1`, a scalar Integer function.
fn construct_next<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    integer: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature = dae::FunctionSignature::new(VarName::new("next"), [integer], [integer], at);
    model
        .function(signature, |model, reservation| {
            let (input, output) = model.functions(|functions| {
                Ok((
                    functions.parameter(&reservation, VarName::new("i"), 0, at)?,
                    functions.output(&reservation, VarName::new("j"), 0, at)?,
                ))
            })?;
            let result = model.expressions(|expressions| {
                let input = expressions.at(at).function_parameter(input)?;
                let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Add, input, one)
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| {
                functions.assign(&mut body, output, result, at)?;
                functions.define(body, at)
            })
        })
        .map(|(function, ())| function)
}

#[test]
fn a_call_subscript_selects_the_scalar_its_result_names() {
    let text = "Real x[4]; x[next(1)]; x[next(2)];";
    let mut sources = SourceMap::new();
    let source = sources.add("integer_call_subscript.mo", text);
    let at = provenance(source, 0, text.len());
    let model = dae::Dae::construct(sources, |model| {
        let (vector, integer) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [4]), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
            ))
        })?;
        let next = construct_next(model, integer, at)?;
        let x = model.variables(|variables| {
            variables.algebraic(VarName::new("x"), vector, at, Default::default())
        })?;
        model.expressions(|expressions| {
            for argument in [1, 2] {
                let argument = expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(argument))?;
                let subscript = expressions.at(at).call(next, 0, [argument])?;
                let x = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Algebraic(x))?;
                expressions.at(at).index(
                    x,
                    [dae::Subscript::Index {
                        expression: subscript,
                        provenance: at,
                    }],
                )?;
            }
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let indices = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter(|id| view.expression(*id).unwrap().kind() == dae::ExpressionKind::Index)
            .collect::<Vec<_>>();
        let selected = indices
            .into_iter()
            .map(|root| {
                let mut selected = Vec::new();
                for_each_scalar_coordinate(view, root, 0, None, |coordinate, scalar| {
                    assert!(matches!(coordinate, dae::CoordinateView::Algebraic(_)));
                    selected.push(scalar);
                })
                .unwrap();
                selected
            })
            .collect::<Vec<_>>();
        assert_eq!(selected, [[1], [2]]);
    });
}
