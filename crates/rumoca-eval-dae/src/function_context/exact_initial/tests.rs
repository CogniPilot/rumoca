use super::*;
use rumoca_core::{SourceMap, VarName};
mod effects;

#[test]
fn source_integer_bits_broadcast_and_extent_are_exact() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "exact-start.mo",
        "Integer n=9007199254740993; Integer largest=9223372036854775807;",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 20)).unwrap();
    let mut roots = Vec::new();
    let model = dae::Dae::construct(sources, |model| {
        model.expressions(|expressions| {
            for value in [9_007_199_254_740_993, i64::MAX] {
                roots.push(
                    expressions
                        .at(at)
                        .literal(dae::DaeLiteral::Integer(value))?
                        .index(),
                );
            }
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
            for (root, expected) in roots.iter().zip([9_007_199_254_740_993, i64::MAX]) {
                let root = view.expression_id(*root as usize).unwrap();
                let ExactDeclarationValues::Integer(values) = exact_initial_values(view, root, 120_000).unwrap() else { panic!("Integer owner") };
                assert_eq!(values.len(), 120_000);
                assert!(!values.is_empty());
                assert!(matches!(values.runs().next(), Some(ExactInitialRun::Repeat { value, count: 120_000 }) if *value == expected));
                assert_eq!(values.runs().count(), 1);
            }
        });
}

#[test]
fn conditional_hidden_in_parameter_uses_ordered_source_evaluation() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "conditional-start.mo",
        "parameter Integer p=if true then 1 else 2;",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 40)).unwrap();
    let mut root = None;
    let model = dae::Dae::construct(sources, |model| {
        let integer = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let binding = model.expressions(|expressions| {
            let guard = expressions.at(at).literal(dae::DaeLiteral::Boolean(true))?;
            let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let two = expressions.at(at).literal(dae::DaeLiteral::Integer(2))?;
            expressions.at(at).conditional([(guard, one)], two)
        })?;
        let parameter = model.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                integer,
                at,
                dae::VariableAttributes {
                    binding: Some(binding),
                    ..Default::default()
                },
            )
        })?;
        root = Some(
            model
                .expressions(|expressions| {
                    expressions
                        .at(at)
                        .coordinate(dae::CoordinateInput::Parameter(parameter))
                })?
                .index(),
        );
        Ok(())
    })
    .unwrap();
    model.inspect(|view| {
        let root = view.expression_id(root.unwrap() as usize).unwrap();
        let ExactDeclarationValues::Integer(values) = exact_initial_values(view, root, 1).unwrap()
        else {
            panic!("Integer")
        };
        assert!(matches!(
            values.runs().next(),
            Some(ExactInitialRun::Repeat { value: 1, count: 1 })
        ));
    });
}

#[test]
fn boolean_comparison_uses_exact_integer_operands() {
    let mut sources = SourceMap::new();
    let source = sources.add("exact_boolean.mo", "9007199254740993 > 9007199254740992");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 33)).unwrap();
    let mut root = None;
    let model = dae::Dae::construct(sources, |model| {
        model.expressions(|expressions| {
            let lhs = expressions
                .at(at)
                .literal(dae::DaeLiteral::Integer(9_007_199_254_740_993))?;
            let rhs = expressions
                .at(at)
                .literal(dae::DaeLiteral::Integer(9_007_199_254_740_992))?;
            root = Some(
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Greater, lhs, rhs)?
                    .index(),
            );
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let root = view.expression_id(root.unwrap() as usize).unwrap();
        let ExactDeclarationValues::Boolean(values) =
            exact_initial_values(view, root, 120_000).unwrap()
        else {
            panic!("exact Boolean")
        };
        assert!(matches!(
            values.runs().next(),
            Some(ExactInitialRun::Repeat {
                value: true,
                count: 120_000
            })
        ));
    });
}

#[test]
fn literal_source_arrays_keep_literal_cost_and_reject_foreign_extents() {
    let mut sources = SourceMap::new();
    let source = sources.add("literal_start.mo", "{7,7}");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 5)).unwrap();
    let mut root = None;
    let model = dae::Dae::construct(sources, |model| {
        model.expressions(|expressions| {
            let value = expressions.at(at).literal(dae::DaeLiteral::Integer(7))?;
            root = Some(expressions.at(at).array([value, value])?.index());
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
            let root = view.expression_id(root.unwrap() as usize).unwrap();
            let ExactDeclarationValues::Integer(values) = exact_initial_values(view, root, 2).unwrap() else { panic!("exact Integer array") };
            assert!(matches!(values.runs().next(), Some(ExactInitialRun::Literal(values)) if values == &[7,7]));
            let error = exact_initial_values(view, root, 3).unwrap_err();
            assert_eq!(error.message, "source extent does not match declaration");
        });
}

#[test]
fn quotient_and_constructor_broadcast_reuse_checked_semantics() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "builtin_start.mo",
        "div(-7,3); fill(true,120000); zeros(120000)",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 9)).unwrap();
    let mut roots = Vec::new();
    let model = dae::Dae::construct(sources, |model| {
        model.expressions(|expressions| {
            let seven = expressions.at(at).literal(dae::DaeLiteral::Integer(-7))?;
            let three = expressions.at(at).literal(dae::DaeLiteral::Integer(3))?;
            let count = expressions
                .at(at)
                .literal(dae::DaeLiteral::Integer(120_000))?;
            let value = expressions.at(at).literal(dae::DaeLiteral::Boolean(true))?;
            roots.push(
                expressions
                    .at(at)
                    .builtin(dae::PureBuiltin::Div, [seven, three])?
                    .index(),
            );
            roots.push(
                expressions
                    .at(at)
                    .builtin(dae::PureBuiltin::Fill, [value, count])?
                    .index(),
            );
            roots.push(
                expressions
                    .at(at)
                    .builtin(dae::PureBuiltin::Zeros, [count])?
                    .index(),
            );
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let mut values = roots.iter().map(|root| {
            exact_initial_values(view, view.expression_id(*root as usize).unwrap(), 120_000)
                .unwrap()
        });
        let ExactDeclarationValues::Integer(quotient) = values.next().unwrap() else {
            panic!("Integer quotient")
        };
        assert!(matches!(
            quotient.runs().next(),
            Some(ExactInitialRun::Repeat {
                value: -2,
                count: 120_000
            })
        ));
        let ExactDeclarationValues::Boolean(fill) = values.next().unwrap() else {
            panic!("Boolean fill")
        };
        assert!(matches!(
            fill.runs().next(),
            Some(ExactInitialRun::Repeat {
                value: true,
                count: 120_000
            })
        ));
        let ExactDeclarationValues::Integer(zeros) = values.next().unwrap() else {
            panic!("Integer zeros")
        };
        assert!(matches!(
            zeros.runs().next(),
            Some(ExactInitialRun::Repeat {
                value: 0,
                count: 120_000
            })
        ));
    });
}

#[test]
fn parameter_call_executes_original_source_invocation() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "call_start.mo",
        "function f output Integer y; algorithm y:=7; end f; parameter Integer p=f();",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 49)).unwrap();
    let mut root = None;
    let model = dae::Dae::construct(sources, |model| {
        let integer = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let signature = dae::FunctionSignature::new(VarName::new("f"), [], [integer], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let value = model.expressions(|expressions| {
                expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(i64::MAX))
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| {
                functions.assign(&mut body, output, value, at)?;
                functions.define(body, at)
            })
        })?;
        let binding = model.expressions(|expressions| expressions.at(at).call(function, 0, []))?;
        let parameter = model.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                integer,
                at,
                dae::VariableAttributes {
                    binding: Some(binding),
                    ..Default::default()
                },
            )
        })?;
        root = Some(
            model
                .expressions(|expressions| {
                    expressions
                        .at(at)
                        .coordinate(dae::CoordinateInput::Parameter(parameter))
                })?
                .index(),
        );
        Ok(())
    })
    .unwrap();
    model.inspect(|view| {
        let ExactDeclarationValues::Integer(values) =
            exact_initial_values(view, view.expression_id(root.unwrap() as usize).unwrap(), 1)
                .unwrap()
        else {
            panic!("Integer")
        };
        assert!(matches!(
            values.runs().next(),
            Some(ExactInitialRun::Repeat {
                value: i64::MAX,
                count: 1
            })
        ));
    });
}
