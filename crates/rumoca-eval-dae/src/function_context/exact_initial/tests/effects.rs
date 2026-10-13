use super::*;

#[test]
fn complete_invocation_preserves_warning_fatal_suffix_order_with_exact_guards() {
    let text = "function checked input Integer x; output Integer y; algorithm assert(false,\"warning\",AssertionLevel.warning); assert(x==9007199254740993,\"first\"); y:=9223372036854775807; assert(false,\"suffix\"); end checked;";
    let mut sources = SourceMap::new();
    let source = sources.add("exact-effects.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let located = |word: &str| {
        let start = text.find(word).unwrap();
        dae::DaeProvenance::source(Span::from_offsets(source, start, start + word.len())).unwrap()
    };
    let first = located("first");
    let suffix = located("suffix");
    let mut calls = Vec::new();
    let model = dae::Dae::construct(sources, |model| {
        let integer = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let signature =
            dae::FunctionSignature::new(VarName::new("checked"), [integer], [integer], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let (input, output) = model.functions(|functions| {
                Ok((
                    functions.parameter(&reservation, VarName::new("x"), 0, at)?,
                    functions.output(&reservation, VarName::new("y"), 0, at)?,
                ))
            })?;
            let (condition, false_value, maximum, warning_message, first_message, suffix_message) =
                model.expressions(|expressions| {
                    let input = expressions.at(at).function_parameter(input)?;
                    let high = expressions
                        .at(at)
                        .literal(dae::DaeLiteral::Integer(9_007_199_254_740_993))?;
                    let guard =
                        expressions
                            .at(first)
                            .binary(dae::BinaryOperator::Equal, input, high)?;
                    Ok((
                        guard,
                        expressions
                            .at(at)
                            .literal(dae::DaeLiteral::Boolean(false))?,
                        expressions
                            .at(at)
                            .literal(dae::DaeLiteral::Integer(i64::MAX))?,
                        expressions
                            .at(at)
                            .literal(dae::DaeLiteral::String("warning".into()))?,
                        expressions
                            .at(first)
                            .literal(dae::DaeLiteral::String("first".into()))?,
                        expressions
                            .at(suffix)
                            .literal(dae::DaeLiteral::String("suffix".into()))?,
                    ))
                })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| {
                functions.assertion_with_level(
                    &mut body,
                    false_value,
                    warning_message,
                    dae::AssertionLevel::Warning,
                    at,
                )?;
                functions.assertion(&mut body, condition, first_message, first)?;
                functions.assign(&mut body, output, maximum, at)?;
                functions.assertion(&mut body, false_value, suffix_message, suffix)?;
                functions.define(body, at)
            })
        })?;
        model.expressions(|expressions| {
            for value in [9_007_199_254_740_993, 9_007_199_254_740_992] {
                let argument = expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(value))?;
                calls.push(expressions.at(at).call(function, 0, [argument])?.index());
            }
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        for (call, message, provenance) in
            [(calls[0], "suffix", suffix), (calls[1], "first", first)]
        {
            let error = exact_initial_values(view, view.expression_id(call as usize).unwrap(), 1)
                .unwrap_err();
            assert_eq!(error.message, message);
            assert_eq!(error.span, provenance.span());
        }
    });
}
