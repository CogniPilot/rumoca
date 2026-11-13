use super::*;

fn selected_outputs<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionBody<'dae>,
    carried: dae::FunctionValueId<'dae>,
    outputs: [dae::FunctionValueId<'dae>; 2],
    index: dae::ExprId<'dae>,
    at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let value = model.functions(|functions| functions.read(body, carried, at))?;
    let (selected, other) = model.expressions(|expressions| {
        let selected = expressions.at(at).index(
            value,
            [dae::Subscript::Index {
                expression: index,
                provenance: at,
            }],
        )?;
        let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
        let other = expressions
            .at(at)
            .binary(dae::BinaryOperator::Add, selected, zero)?;
        Ok((selected, other))
    })?;
    model.functions(|functions| {
        functions.assign(body, outputs[0], selected, at)?;
        functions.assign(body, outputs[1], other, at)
    })
}

fn function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    types: [dae::ValueTypeId<'dae>; 3],
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let [vector, integer, real] = types;
    model
        .function(
            dae::FunctionSignature::new(
                VarName::new("profiled"),
                [vector, integer],
                [vector, real, real],
                at,
            ),
            |model, reservation| {
                let (input, index, carried, first, second) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("u"), 0, at)?,
                        functions.parameter(&reservation, VarName::new("i"), 1, at)?,
                        functions.output(&reservation, VarName::new("carry"), 0, at)?,
                        functions.output(&reservation, VarName::new("a"), 1, at)?,
                        functions.output(&reservation, VarName::new("b"), 2, at)?,
                    ))
                })?;
                let (input, index) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(at).function_parameter(input)?,
                        expressions.at(at).function_parameter(index)?,
                    ))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, carried, input, at))?;
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
                let mut loop_body = model
                    .functions(|functions| functions.begin_loop(body, domain, [carried], at))?;
                let previous =
                    model.functions(|functions| functions.read(loop_body.body(), carried, at))?;
                let updated = model.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, previous, input)
                })?;
                model.functions(|functions| {
                    functions.assign_loop(&mut loop_body, carried, updated, at)
                })?;
                body = model.functions(|functions| functions.finish_loop(loop_body, at))?;
                selected_outputs(model, &mut body, carried, [first, second], index, at)?;
                model.functions(|functions| functions.define(body, at))
            },
        )
        .map(|(function, ())| function)
}

fn fixture() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("profiled_folds.mo", "array fold then selected results");
    let at = provenance(source, 0, 10);
    dae::Dae::construct(sources, |model| {
        let types = model.types(|types| {
            Ok([
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [4]), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
            ])
        })?;
        let function = function(model, types, at)?;
        let actual = model.variables(|variables| {
            variables.algebraic(VarName::new("x"), types[0], at, Default::default())
        })?;
        model.expressions(|expressions| {
            let actual = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(actual))?;
            for (index, output) in [(2, 1), (2, 2), (3, 1), (0, 1)] {
                let index = expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(index))?;
                expressions.at(at).call(function, output, [actual, index])?;
            }
            Ok(())
        })
    })
    .unwrap()
}

fn project<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
) -> Result<Vec<usize>, ProjectionError> {
    let mut result = Vec::new();
    for_each_scalar_coordinate_cached(view, root, 0, None, cache, |_, scalar| result.push(scalar))?;
    Ok(result)
}

#[test]
fn completed_folds_reuse_only_the_exact_integer_profile_across_result_summaries() {
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
        assert_eq!(calls.len(), 4);
        let mut cache = ScalarCoordinateProjectionCache::default();
        for (call, expected) in calls[..3].iter().zip([1, 1, 2]) {
            let actual = project(view, *call, &mut cache).unwrap();
            let fresh =
                project(view, *call, &mut ScalarCoordinateProjectionCache::default()).unwrap();
            assert_eq!(actual, [expected]);
            assert_eq!(actual, fresh);
        }
        assert!(cache.fold_reuses > 0);
        assert!(cache.completed_folds.keys().any(|node| {
            node.integer_profile
                .iter()
                .any(|binding| binding.value == 2)
        }));
        assert!(cache.completed_folds.keys().any(|node| {
            node.integer_profile
                .iter()
                .any(|binding| binding.value == 3)
        }));
        let retained = cache.completed_folds.clone();
        let error = project(view, calls[3], &mut cache).unwrap_err();
        let fresh = project(
            view,
            calls[3],
            &mut ScalarCoordinateProjectionCache::default(),
        )
        .unwrap_err();
        assert_eq!(error, fresh);
        assert!(matches!(
            error,
            ProjectionError::IndexOutOfBounds {
                index: 0,
                extent: 4,
                ..
            }
        ));
        assert_eq!(
            cache.completed_folds, retained,
            "failed profiles publish no closures"
        );
    });
}
