use super::*;

fn update<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionLoop<'dae>,
    carried: [dae::FunctionValueId<'dae>; 3],
    extra: dae::ExprId<'dae>,
    at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let [a, b, c] = model.functions(|functions| {
        Ok([
            functions.read(body.body(), carried[0], at)?,
            functions.read(body.body(), carried[1], at)?,
            functions.read(body.body(), carried[2], at)?,
        ])
    })?;
    let [a, b] = model.expressions(|expressions| {
        Ok([
            expressions.at(at).binary(dae::BinaryOperator::Add, a, b)?,
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Add, b, extra)?,
        ])
    })?;
    model.functions(|functions| {
        for (target, value) in carried.into_iter().zip([a, b, c]) {
            functions.assign_loop(body, target, value, at)?;
        }
        Ok(())
    })
}

fn function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(VarName::new("queued"), [vector], [real; 5], at),
            |model, reservation| {
                let (input, outputs) = model.functions(|functions| {
                    let input = functions.parameter(&reservation, VarName::new("u"), 0, at)?;
                    let outputs = (0..5)
                        .map(|i| {
                            functions.output(&reservation, VarName::new(format!("y{i}")), i, at)
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok((input, outputs))
                })?;
                let input = model
                    .expressions(|expressions| expressions.at(at).function_parameter(input))?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                let mut values = Vec::new();
                for index in 1..=4 {
                    values.push(model.expressions(|expressions| {
                        let index = expressions
                            .at(at)
                            .literal(dae::DaeLiteral::Integer(index))?;
                        expressions.at(at).index(
                            input,
                            [dae::Subscript::Index {
                                expression: index,
                                provenance: at,
                            }],
                        )
                    })?);
                }
                model.functions(|functions| {
                    for (&target, &value) in outputs.iter().zip(&values).take(3) {
                        functions.assign(&mut body, target, value, at)?;
                    }
                    Ok(())
                })?;
                let domain = model.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                provenance: rumoca_core::IndexBinderProvenance::Generated,
                                id: 0,
                                display_name: "k".into(),
                                lower: 1,
                                upper: 2,
                                step: 1,
                            }],
                        },
                        at,
                    )
                })?;
                let mut loop_body = model.functions(|functions| {
                    functions.begin_loop(body, domain, outputs[..3].iter().copied(), at)
                })?;
                update(
                    model,
                    &mut loop_body,
                    [outputs[0], outputs[1], outputs[2]],
                    values[3],
                    at,
                )?;
                body = model.functions(|functions| functions.finish_loop(loop_body, at))?;
                let (a, c) = model.functions(|functions| {
                    Ok((
                        functions.read(&body, outputs[0], at)?,
                        functions.read(&body, outputs[2], at)?,
                    ))
                })?;
                let result = model.expressions(|expressions| {
                    expressions.at(at).binary(dae::BinaryOperator::Add, a, c)
                })?;
                model.functions(|functions| {
                    functions.assign(&mut body, outputs[3], result, at)?;
                    functions.assign(&mut body, outputs[4], result, at)?;
                    functions.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

fn fixture() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "queued_fold_order.mo",
        "A depends on B; unrelated C is a sibling root",
    );
    let at = provenance(source, 0, 10);
    dae::Dae::construct(sources, |model| {
        let (vector, real) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [4]), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
            ))
        })?;
        let function = function(model, vector, real, at)?;
        let variable = model.variables(|variables| {
            variables.algebraic(VarName::new("x"), vector, at, Default::default())
        })?;
        model.expressions(|expressions| {
            let input = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(variable))?;
            expressions.at(at).call(function, 3, [input])?;
            expressions.at(at).call(function, 4, [input])?;
            Ok(())
        })
    })
    .unwrap()
}

fn project<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
) -> Vec<usize> {
    let mut values = Vec::new();
    for_each_scalar_coordinate_cached(view, root, 0, None, cache, |_, scalar| values.push(scalar))
        .unwrap();
    values
}

#[test]
fn replay_keeps_sibling_roots_before_transitive_dependencies_in_original_queue_order() {
    fixture().inspect(|view| {
        let calls = (0..view.expression_count())
            .filter_map(|i| view.expression_id(i))
            .filter(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Call { .. }
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(calls.len(), 2);
        let mut shared = ScalarCoordinateProjectionCache::default();
        let first = project(view, calls[0], &mut shared);
        let second = project(view, calls[1], &mut shared);
        let cold = project(
            view,
            calls[1],
            &mut ScalarCoordinateProjectionCache::default(),
        );
        assert_eq!(first, [0, 2, 1, 3]);
        assert_eq!(
            second, cold,
            "a flattened closure would move transitive B ahead of sibling C"
        );
        assert_eq!(second, first);
        assert!(shared.fold_reuses > 0, "exercise actual recipe replay");
    });
}
