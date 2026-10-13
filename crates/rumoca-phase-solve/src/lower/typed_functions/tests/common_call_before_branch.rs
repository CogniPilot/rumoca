use super::*;

fn checked_pair<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(VarName::new("checked_pair"), [real], [real, real], at),
            |model, reservation| {
                let input =
                    model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
                let first =
                    model.functions(|f| f.output(&reservation, VarName::new("a"), 0, at))?;
                let second =
                    model.functions(|f| f.output(&reservation, VarName::new("b"), 1, at))?;
                let (input, positive, message, bounded, bound_message) =
                    model.expressions(|e| {
                        let input = e.at(at).function_parameter(input)?;
                        let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                        let ten = e.at(at).literal(dae::DaeLiteral::Real(10.0))?;
                        Ok((
                            input,
                            e.at(at).binary(dae::BinaryOperator::Greater, input, zero)?,
                            e.at(at)
                                .literal(dae::DaeLiteral::String("pair input positive".into()))?,
                            e.at(at).binary(dae::BinaryOperator::Less, input, ten)?,
                            e.at(at)
                                .literal(dae::DaeLiteral::String("pair input below ten".into()))?,
                        ))
                    })?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| {
                    f.assertion(&mut body, positive, message, at)?;
                    f.assertion(&mut body, bounded, bound_message, at)?;
                    f.assign_all(&mut body, &[(first, input), (second, input)], at)?;
                    f.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

fn overwritten_first_result<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    child: dae::FunctionId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(VarName::new("overwrite_first"), [real], [real], at),
            |model, reservation| {
                let input =
                    model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
                let output =
                    model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
                let saved =
                    model.functions(|f| f.local(&reservation, VarName::new("saved"), real, at))?;
                let (tuple, condition) = model.expressions(|e| {
                    let input = e.at(at).function_parameter(input)?;
                    let two = e.at(at).literal(dae::DaeLiteral::Real(2.0))?;
                    Ok((
                        e.at(at).call_results(child, [0, 1], [input])?,
                        e.at(at).binary(dae::BinaryOperator::Greater, input, two)?,
                    ))
                })?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| {
                    f.assign_all(&mut body, &[(output, tuple[0]), (saved, tuple[1])], at)
                })?;
                let saved = model.functions(|f| f.read(&body, saved, at))?;
                let (left, right) = model.expressions(|e| {
                    let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
                    Ok((
                        e.at(at).binary(dae::BinaryOperator::Add, saved, one)?,
                        e.at(at).binary(dae::BinaryOperator::Subtract, saved, one)?,
                    ))
                })?;
                model.functions(|f| {
                    let body = crate::tests::scoped_assignment_body(
                        f,
                        body,
                        &[output],
                        &[condition],
                        &[vec![left]],
                        &[right],
                        at,
                    )?;
                    f.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

#[test]
fn prebranch_asserting_tuple_survives_overwritten_first_result() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "prebranch_tuple.mo",
        "function checked_pair overwrite_first",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 21)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let real = model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let child = checked_pair(model, real, at)?;
        let parent = overwritten_first_result(model, real, child, at)?;
        model.expressions(|e| {
            let input = e.at(at).literal(dae::DaeLiteral::Real(3.0))?;
            e.at(at).call(parent, 0, [input])
        })?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);
    let parent = &table.owners()[1];
    assert!(
        parent.assertion_flow().is_some(),
        "one authored prebranch tuple must retain its checked assertion flow"
    );
    assert_eq!(
        parent
            .body()
            .operations()
            .iter()
            .filter(|op| matches!(op.operation(), solve::SolveOperation::Call { .. }))
            .count(),
        1,
        "the shared tuple belongs in the authored parent scope"
    );
    for (input, expected) in [(3.0_f64, 4.0_f64), (1.0, 0.0)] {
        let argument = rumoca_eval_solve::TypedValue::construct(
            solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic_profile())),
            vec![solve::SolveValueKind::Real64(input.to_bits())],
        )
        .unwrap();
        let result = rumoca_eval_solve::eval_pure_call(&table, parent.id(), &[argument]).unwrap();
        assert_eq!(
            result[0].elements(),
            [solve::SolveValueKind::Real64(expected.to_bits())]
        );
    }
}

fn nested_regularization<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    child: dae::FunctionId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(VarName::new("regularization"), [real], [real], at),
            |model, reservation| {
                let input =
                    model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
                let output =
                    model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
                let (outer, inner, direct) = model.expressions(|e| {
                    let input = e.at(at).function_parameter(input)?;
                    let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
                    let two = e.at(at).literal(dae::DaeLiteral::Real(2.0))?;
                    let ten = e.at(at).literal(dae::DaeLiteral::Real(10.0))?;
                    let tuple = e.at(at).call_results(child, [0, 1], [input])?;
                    let condition = e.at(at).binary(dae::BinaryOperator::Greater, input, two)?;
                    let left = e.at(at).binary(dae::BinaryOperator::Add, tuple[1], one)?;
                    let right = e
                        .at(at)
                        .binary(dae::BinaryOperator::Subtract, tuple[1], one)?;
                    let inner = e.at(at).conditional([(condition, left)], right)?;
                    Ok((
                        e.at(at).binary(dae::BinaryOperator::Greater, input, ten)?,
                        inner,
                        input,
                    ))
                })?;
                let body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| {
                    let body = crate::tests::scoped_assignment_body(
                        f,
                        body,
                        &[output],
                        &[outer],
                        &[vec![direct]],
                        &[inner],
                        at,
                    )?;
                    f.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

#[test]
fn outer_else_shared_tuple_dag_declines_observation_without_authored_prefix() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "nested_prebranch_tuple.mo",
        "function checked_pair regularization",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 21)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let real = model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let child = checked_pair(model, real, at)?;
        let parent = nested_regularization(model, real, child, at)?;
        model.expressions(|e| {
            let input = e.at(at).literal(dae::DaeLiteral::Real(3.0))?;
            e.at(at).call(parent, 0, [input])
        })?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);
    let parent = &table.owners()[1];
    assert!(parent.assertion_flow().is_none());
    let site = parent.call_site();
    assert!(
        table.matches_site(&site),
        "the source call belongs to this exact table"
    );
    let observation = solve::SolveAssertionObservationSite::new(site)
        .expect("the original assertion interface remains present");
    assert!(
        !observation.matches_table(&table),
        "a value DAG cannot invent the missing lexical tuple prefix or joined publication owner"
    );
}

fn real_argument(input: f64) -> rumoca_eval_solve::TypedValue {
    rumoca_eval_solve::TypedValue::construct(
        solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic_profile())),
        vec![solve::SolveValueKind::Real64(input.to_bits())],
    )
    .unwrap()
}

fn ordered_nested_regularization<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    child: dae::FunctionId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(VarName::new("ordered_regularization"), [real], [real], at),
            |model, reservation| {
                let (input, output, saved) = model.functions(|f| {
                    Ok((
                        f.parameter(&reservation, VarName::new("u"), 0, at)?,
                        f.output(&reservation, VarName::new("y"), 0, at)?,
                        f.local(&reservation, VarName::new("saved"), real, at)?,
                    ))
                })?;
                let (input, outer, inner) = model.expressions(|e| {
                    let input = e.at(at).function_parameter(input)?;
                    let ten = e.at(at).literal(dae::DaeLiteral::Real(10.0))?;
                    let two = e.at(at).literal(dae::DaeLiteral::Real(2.0))?;
                    Ok((
                        input,
                        e.at(at).binary(dae::BinaryOperator::Greater, input, ten)?,
                        e.at(at).binary(dae::BinaryOperator::Greater, input, two)?,
                    ))
                })?;
                let body = model.functions(|f| f.begin(reservation, at))?;
                let (mut direct, finish) = model.functions(|f| f.begin_branch(body, at))?;
                model.functions(|f| f.assign(&mut direct, output, input, at))?;
                let (body, direct) = model.functions(|f| f.finish_branch(finish, direct))?;
                let (mut fallback, outer_finish) = model.functions(|f| f.begin_branch(body, at))?;
                let tuple = model.expressions(|e| e.at(at).call_results(child, [0, 1], [input]))?;
                model.functions(|f| {
                    f.assign_all(&mut fallback, &[(output, tuple[0]), (saved, tuple[1])], at)
                })?;
                let saved = model.functions(|f| f.read(&fallback, saved, at))?;
                let (left, right) = model.expressions(|e| {
                    let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
                    Ok((
                        e.at(at).binary(dae::BinaryOperator::Add, saved, one)?,
                        e.at(at).binary(dae::BinaryOperator::Subtract, saved, one)?,
                    ))
                })?;
                let (mut branch, finish) = model.functions(|f| f.begin_branch(fallback, at))?;
                model.functions(|f| f.assign(&mut branch, output, left, at))?;
                let (fallback, branch) = model.functions(|f| f.finish_branch(finish, branch))?;
                let (mut other, finish) = model.functions(|f| f.begin_branch(fallback, at))?;
                model.functions(|f| f.assign(&mut other, output, right, at))?;
                let (mut fallback, other) = model.functions(|f| f.finish_branch(finish, other))?;
                model.functions(|f| {
                    f.finish_conditional(&mut fallback, vec![(inner, branch)], other, at)
                })?;
                let (mut body, fallback) =
                    model.functions(|f| f.finish_branch(outer_finish, fallback))?;
                model.functions(|f| {
                    f.finish_conditional(&mut body, vec![(outer, direct)], fallback, at)?;
                    f.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

#[test]
fn ordered_outer_else_prefix_tuple_keeps_checked_flow_and_inactive_faults() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "ordered_prebranch_tuple.mo",
        "function checked_pair regularization",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 21)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let real = model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let child = checked_pair(model, real, at)?;
        let parent = ordered_nested_regularization(model, real, child, at)?;
        model.expressions(|e| {
            let input = e.at(at).literal(dae::DaeLiteral::Real(3.0))?;
            e.at(at).call(parent, 0, [input])
        })?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);
    let parent = &table.owners()[1];
    assert!(parent.assertion_flow().is_some());
    for (input, expected) in [(12.0_f64, 12.0_f64), (3.0, 4.0), (1.0, 0.0)] {
        let result =
            rumoca_eval_solve::eval_pure_call(&table, parent.id(), &[real_argument(input)])
                .unwrap();
        assert_eq!(
            result[0].elements(),
            [solve::SolveValueKind::Real64(expected.to_bits())]
        );
    }
    let Err(rumoca_eval_solve::TypedProgramEvalError::AssertionFailed { failure }) =
        rumoca_eval_solve::eval_pure_call_checked(&table, parent.id(), &[real_argument(-1.0)])
    else {
        panic!("selected source prefix must preserve its original assertion");
    };
    assert_eq!(failure.source_span(), at.span());
}
