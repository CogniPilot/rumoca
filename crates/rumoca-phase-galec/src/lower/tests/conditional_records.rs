//! Conditional record-call lowering tests.
//!
//! A record-returning call inside an `if` branch must be materialized only on
//! the branch that selects it, so these cases assert both the fixture shape and
//! the laziness of the emitted calls.

use super::*;

fn conditional_record_fixture() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "record Pair Real left; Real right; end Pair; function makePair input Real u; output Pair p; algorithm p := Pair(u, u); end makePair; (if true then makePair(2.0) else Pair(0.0, 0.0)).right";
    let source = sources.add("conditional-record-call.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let pair = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("left"), real), (VarName::new("right"), real)],
                provenance,
            )
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("makePair"), [real], [pair], provenance);
        let (function, ()) = dae.function(signature, |dae, reservation| {
            let parameter = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, provenance)
            })?;
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("p"), 0, provenance)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            let fields = dae.expressions(|expressions| {
                Ok([
                    expressions.at(provenance).function_parameter(parameter)?,
                    expressions.at(provenance).function_parameter(parameter)?,
                ])
            })?;
            let value =
                dae.expressions(|expressions| expressions.at(provenance).record(pair, fields))?;
            dae.functions(|functions| functions.assign(&mut body, output, value, provenance))?;
            dae.functions(|functions| functions.define(body, provenance))
        })?;
        let (condition, argument, zero) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Boolean(true))?,
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(2.0))?,
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(0.0))?,
            ))
        })?;
        let call = dae
            .expressions(|expressions| expressions.at(provenance).call(function, 0, [argument]))?;
        let fallback =
            dae.expressions(|expressions| expressions.at(provenance).record(pair, [zero, zero]))?;
        let selection = dae.expressions(|expressions| {
            expressions
                .at(provenance)
                .conditional([(condition, call)], fallback)
        })?;
        dae.expressions(|expressions| expressions.at(provenance).field(selection, 1))?;
        let shared_call_selection = dae.expressions(|expressions| {
            expressions
                .at(provenance)
                .conditional([(condition, call)], call)
        })?;
        dae.expressions(|expressions| expressions.at(provenance).field(shared_call_selection, 0))?;
        let shared_call_field =
            dae.expressions(|expressions| expressions.at(provenance).field(call, 0))?;
        dae.expressions(|expressions| {
            expressions.at(provenance).array([
                shared_call_field,
                shared_call_field,
                shared_call_field,
            ])
        })?;
        Ok(())
    })
    .unwrap()
}

fn assert_dynamic_array_record_calls_are_lazy(view: dae::DaeView<'_>) {
    let array = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .find(|id| {
            matches!(
                view.expression(*id).unwrap().operation(),
                dae::ExpressionOperation::Array(_)
            )
        })
        .unwrap();
    let index_name = gast::Name::ident("i");
    let dynamic_index = gast::Expression::Ref(gast::Reference::local(index_name.clone()));
    let variables = HashMap::new();
    let previous = HashMap::new();
    let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
    let mut lowerer = ExpressionLowerer::with_do_step_effects(
        view,
        &definitions,
        &variables,
        &previous,
        positive_zero_arithmetic(),
    );
    lowerer.loop_index_bounds.push(LoopIndexBound {
        name: index_name,
        minimum: 1,
        maximum: 3,
    });
    lowerer.lower_at(array, &[dynamic_index]).unwrap();
    let prefix = lowerer.take_prefix_statements();
    let gast::Statement::If(first) = &prefix[0].node else {
        panic!("the array selection must dominate its first call")
    };
    assert!(matches!(
        first.branches[0].body[0].node,
        gast::Statement::MultiAssignment { .. }
    ));
    let first_else = first.else_body.as_ref().unwrap();
    let gast::Statement::If(second) = &first_else[0].node else {
        panic!("the array selection must dominate its second call")
    };
    assert!(matches!(
        second.branches[0].body[0].node,
        gast::Statement::MultiAssignment { .. }
    ));
    assert!(matches!(
        second.else_body.as_ref().unwrap()[0].node,
        gast::Statement::MultiAssignment { .. }
    ));
}

#[test]
fn conditional_record_call_is_materialized_only_in_its_selected_branch() {
    let model = conditional_record_fixture();
    model.inspect(|view| {
        let field = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Field { field: 1, .. }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        let selected = lowerer.lower(field).unwrap();
        let prefix = lowerer.take_prefix_statements();

        assert_eq!(selected.scalar_type, gast::ScalarType::Real);
        assert_eq!(prefix.len(), 1);
        let gast::Statement::If(selection) = &prefix[0].node else {
            panic!("the conditional must dominate its record call")
        };
        assert!(matches!(
            selection.branches[0].body[0].node,
            gast::Statement::MultiAssignment { .. }
        ));
        assert!(
            prefix.iter().all(|statement| !matches!(
                statement.node,
                gast::Statement::MultiAssignment { .. }
            ))
        );

        let shared_call_field = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Field { field: 0, .. }
                )
            })
            .unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        lowerer.lower(shared_call_field).unwrap();
        let prefix = lowerer.take_prefix_statements();
        let gast::Statement::If(selection) = &prefix[0].node else {
            panic!("the conditional must dominate both uses of its shared call")
        };
        assert!(matches!(
            selection.branches[0].body[0].node,
            gast::Statement::MultiAssignment { .. }
        ));
        assert!(matches!(
            selection.else_body.as_ref().unwrap()[0].node,
            gast::Statement::MultiAssignment { .. }
        ));

        assert_dynamic_array_record_calls_are_lazy(view);
    });
}

struct ConditionalRecordActualRefusalFixture {
    model: dae::Dae,
    call_span: Span,
}

fn conditional_record_actual_refusal_fixture() -> ConditionalRecordActualRefusalFixture {
    let mut sources = SourceMap::new();
    let text = "make(if true then maker(2.0) else Pair(0.0, 0.0))";
    let source = sources.add("conditional-record-actual.mo", text);
    let whole_span = Span::from_offsets(source, 0, text.len());
    let call_start = text.find("maker(2.0)").unwrap();
    let call_span = Span::from_offsets(source, call_start, call_start + "maker(2.0)".len());
    let whole = dae::DaeProvenance::source(whole_span).unwrap();
    let call_at = dae::DaeProvenance::source(call_span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let (real, pair, nested) = dae.types(|types| {
            let real = types.derived(dae::ValueType::scalar(dae::ScalarType::Real), whole)?;
            let pair = types.record(
                VarName::new("Pair"),
                [(VarName::new("left"), real), (VarName::new("right"), real)],
                whole,
            )?;
            let nested = types.record(
                VarName::new("Nested"),
                [(VarName::new("pair"), pair)],
                whole,
            )?;
            Ok((real, pair, nested))
        })?;
        let (maker, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("maker"), [real], [pair], whole),
            |dae, reservation| {
                let (input, output) = dae.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("u"), 0, whole)?,
                        functions.output(&reservation, VarName::new("result"), 0, whole)?,
                    ))
                })?;
                let input =
                    dae.expressions(|expressions| expressions.at(whole).function_parameter(input))?;
                let value = dae.expressions(|expressions| {
                    expressions.at(whole).record(pair, [input, input])
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, whole))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, value, whole)?;
                    functions.define(body, whole)
                })
            },
        )?;
        let (consumer, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("make"), [pair], [real], whole),
            |dae, reservation| {
                let (input, output) = dae.functions(|functions| {
                    let input =
                        functions.parameter(&reservation, VarName::new("value"), 0, whole)?;
                    let output =
                        functions.output(&reservation, VarName::new("result"), 0, whole)?;
                    functions.local(&reservation, VarName::new("forceEntered"), nested, whole)?;
                    Ok((input, output))
                })?;
                let input =
                    dae.expressions(|expressions| expressions.at(whole).function_parameter(input))?;
                let field = dae.expressions(|expressions| expressions.at(whole).field(input, 0))?;
                let mut body = dae.functions(|functions| functions.begin(reservation, whole))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, field, whole)?;
                    functions.define(body, whole)
                })
            },
        )?;
        let (condition, argument, zero) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(whole)
                    .literal(dae::DaeLiteral::Boolean(true))?,
                expressions.at(whole).literal(dae::DaeLiteral::Real(2.0))?,
                expressions.at(whole).literal(dae::DaeLiteral::Real(0.0))?,
            ))
        })?;
        let maker_call =
            dae.expressions(|expressions| expressions.at(call_at).call(maker, 0, [argument]))?;
        let fallback =
            dae.expressions(|expressions| expressions.at(whole).record(pair, [zero, zero]))?;
        let conditional = dae.expressions(|expressions| {
            expressions
                .at(whole)
                .conditional([(condition, maker_call)], fallback)
        })?;
        dae.expressions(|expressions| {
            expressions.at(whole).call(consumer, 0, [conditional])?;
            Ok(())
        })
    })
    .unwrap();

    ConditionalRecordActualRefusalFixture { model, call_span }
}

#[test]
fn conditional_record_actual_refusal_reports_the_nested_call_span() {
    let ConditionalRecordActualRefusalFixture { model, call_span } =
        conditional_record_actual_refusal_fixture();
    model.inspect(|view| {
        let consumer = view.function_id(1).unwrap();
        assert!(
            !user_functions::is_directly_lowerable(view, consumer),
            "the nested record local must exercise entered-body argument preparation"
        );
        let root = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .rfind(|expression| {
                matches!(
                    view.expression(*expression).unwrap().operation(),
                    dae::ExpressionOperation::Call { function, .. } if function == consumer
                )
            })
            .unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        assert!(matches!(
            lowerer.lower(root),
            Err(GalecTargetError::UnsupportedFeature {
                feature,
                span: Some(span),
                ..
            }) if feature == "record-argument-call-projection" && span == call_span
        ));
        assert!(
            lowerer.take_prefix_statements().is_empty(),
            "a refused record transaction must not expose partial statements"
        );
    });
}
