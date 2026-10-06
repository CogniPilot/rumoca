use super::*;

fn calls(view: dae::DaeView<'_>) -> Vec<dae::ExprId<'_>> {
    (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter(|root| {
            matches!(
                view.expression(*root).unwrap().operation(),
                dae::ExpressionOperation::Call { .. }
            )
        })
        .collect()
}

fn project<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
    filtered: bool,
) -> Result<Vec<(dae::CoordinateView<'dae>, usize)>, String> {
    let mut result = Vec::new();
    let checked = if filtered {
        for_each_scalar_coordinate_filtered_cached(
            view,
            root,
            0,
            None,
            cache,
            |_| false,
            |coordinate, scalar| result.push((coordinate, scalar)),
        )
    } else {
        for_each_scalar_coordinate_cached(view, root, 0, None, cache, |coordinate, scalar| {
            result.push((coordinate, scalar))
        })
    };
    checked
        .map(|()| result)
        .map_err(|error| format!("{error:?}"))
}

#[test]
fn query_filtered_and_full_consumers_keep_separate_complete_formal_inventories() {
    parameter_fragment_reuse::model().inspect(|view| {
        let roots = calls(view);
        let mut mixed = ScalarCoordinateProjectionCache::default();
        let mut reference = ScalarCoordinateProjectionCache::default();
        for root in roots.into_iter().cycle().take(8) {
            assert!(project(view, root, &mut mixed, true).unwrap().is_empty());
            assert_eq!(
                project(view, root, &mut mixed, false),
                project(view, root, &mut reference, false)
            );
            assert!(project(view, root, &mut mixed, true).unwrap().is_empty());
            assert_eq!(mixed.function_results, reference.function_results);
        }
    });
}

#[derive(Clone, Copy)]
enum Nested {
    None,
    Selector,
    FixedIndex,
}

fn scalar_child<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    integer: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(VarName::new("child"), [real, integer], [real], at),
            |model, reservation| {
                let (u, y) = model.functions(|functions| {
                    let u = functions.parameter(&reservation, VarName::new("u"), 0, at)?;
                    functions.parameter(&reservation, VarName::new("unused"), 1, at)?;
                    Ok((u, functions.output(&reservation, VarName::new("y"), 0, at)?))
                })?;
                let value =
                    model.expressions(|expressions| expressions.at(at).function_parameter(u))?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| {
                    functions.assign(&mut body, y, value, at)?;
                    functions.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

fn outer<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    types: [dae::ValueTypeId<'dae>; 2],
    child: dae::FunctionId<'dae>,
    kind: Nested,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let [vector, real] = types;
    model
        .function(
            dae::FunctionSignature::new(VarName::new("outer"), [vector], [real], at),
            |model, reservation| {
                let (u, y) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("u"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                    ))
                })?;
                let value = model.expressions(|expressions| {
                    let u = expressions.at(at).function_parameter(u)?;
                    let bad = expressions.at(at).literal(dae::DaeLiteral::Integer(4))?;
                    let argument = if matches!(kind, Nested::FixedIndex) {
                        expressions.at(at).index(
                            u,
                            [dae::Subscript::Index {
                                expression: bad,
                                provenance: at,
                            }],
                        )?
                    } else {
                        u
                    };
                    expressions.at(at).call(child, 0, [argument, bad])
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| {
                    functions.assign(&mut body, y, value, at)?;
                    functions.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

fn selector_argument<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    kind: Nested,
    array: dae::ExprId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if matches!(kind, Nested::FixedIndex) {
        expressions.at(at).literal(dae::DaeLiteral::Real(1.0))
    } else {
        Ok(array)
    }
}

fn selector_model(kind: Nested) -> dae::Dae {
    let text = "parameter Real a[3]; select(a,1); select(a,2); select(a,4); outer(a);";
    let mut sources = SourceMap::new();
    let source = sources.add("query_selector.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let integer = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let child = if matches!(kind, Nested::FixedIndex) {
            scalar_child(model, real, integer, at)?
        } else {
            construct_select_function(model, vector, integer, real, at)?
        };
        let outer = if matches!(kind, Nested::None) {
            None
        } else {
            Some(outer(model, [vector, real], child, kind, at)?)
        };
        let a = model.variables(|variables| {
            variables.parameter(VarName::new("a"), vector, at, Default::default())
        })?;
        model.expressions(|expressions| {
            let a = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(a))?;
            for index in [1, 2, 4] {
                let literal = expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(index))?;
                let argument = selector_argument(expressions, kind, a, at)?;
                expressions.at(at).call(child, 0, [argument, literal])?;
            }
            if let Some(outer) = outer {
                expressions.at(at).call(outer, 0, [a])?;
            }
            Ok(())
        })
    })
    .unwrap()
}

fn check_selector(kind: Nested) {
    selector_model(kind).inspect(|view| {
        let roots = calls(view);
        let mut mixed = ScalarCoordinateProjectionCache::default();
        let mut reference = ScalarCoordinateProjectionCache::default();
        // Nested function-body calls are not model roots; begin at the last
        // three direct controls, then the outer call. Warm each child as a
        // validation root before it is needed as a full nested callee.
        let count = if matches!(kind, Nested::None) { 3 } else { 4 };
        let roots = &roots[roots.len() - count..];
        for root in roots {
            let expected = project(view, *root, &mut reference, false);
            let filtered = project(view, *root, &mut mixed, true);
            // A query excludes every coordinate: a call over plain actuals
            // contributes nothing, and a selector actual is still walked.
            match (&expected, filtered) {
                (Err(error), Err(filtered)) => assert_eq!(filtered, *error),
                (_, filtered) => assert!(filtered.unwrap().is_empty()),
            }
            assert_eq!(project(view, *root, &mut mixed, false), expected);
        }
        let final_error = project(view, *roots.last().unwrap(), &mut mixed, false).unwrap_err();
        assert!(final_error.contains("IndexOutOfBounds") && final_error.contains("index: 4"));
    });
}

#[test]
fn query_free_parameter_selectors_preserve_actual_one_two_and_oob_diagnostics() {
    check_selector(Nested::None);
}

#[test]
fn nested_selector_arguments_keep_bounds_after_child_root_cache_warmup() {
    check_selector(Nested::Selector);
}

#[test]
fn nested_fixed_bad_index_selected_by_scalar_child_still_refuses() {
    check_selector(Nested::FixedIndex);
}

#[test]
fn query_free_calls_contribute_no_queried_dependency_over_an_external_body() {
    fold_external::external_fold_model().inspect(|view| {
        let root = *calls(view).last().unwrap();
        let mut full = ScalarCoordinateProjectionCache::default();
        let mut filtered = ScalarCoordinateProjectionCache::default();
        let original = project(view, root, &mut full, false).unwrap_err();
        assert!(original.contains("ExternalFunction"));
        // A pure call reads only its actuals; none is queried, so the call
        // contributes no queried dependency and its body is not walked.
        assert_eq!(project(view, root, &mut filtered, true).unwrap(), []);
        assert!(filtered.function_results.is_empty());
    });
}

fn complex_arguments() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "select(p, if y>0 then 1 else 2); second(if y>0 then p else q);";
    let source = sources.add("query_complex.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let integer = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let select = construct_select_function(model, vector, integer, real, at)?;
        let second = construct_second_function(model, vector, real, at)?;
        let (p, q, y) = model.variables(|variables| {
            Ok((
                variables.parameter(VarName::new("p"), vector, at, Default::default())?,
                variables.parameter(VarName::new("q"), vector, at, Default::default())?,
                variables.algebraic(VarName::new("y"), real, at, Default::default())?,
            ))
        })?;
        model.expressions(|expressions| {
            let p = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(p))?;
            let q = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(q))?;
            let y = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(y))?;
            let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let condition = expressions
                .at(at)
                .binary(dae::BinaryOperator::Greater, y, zero)?;
            let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let two = expressions.at(at).literal(dae::DaeLiteral::Integer(2))?;
            let index = expressions.at(at).conditional([(condition, one)], two)?;
            expressions.at(at).call(select, 0, [p, index])?;
            let values = expressions.at(at).conditional([(condition, p)], q)?;
            expressions.at(at).call(second, 0, [values])?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn query_reads_inside_actual_selector_and_condition_remain_incident() {
    complex_arguments().inspect(|view| {
        let mut filtered = ScalarCoordinateProjectionCache::default();
        for root in calls(view) {
            let mut full = ScalarCoordinateProjectionCache::default();
            let expected = project(view, root, &mut full, false)
                .unwrap()
                .into_iter()
                .filter(|(coordinate, _)| matches!(coordinate, dae::CoordinateView::Algebraic(_)))
                .collect::<Vec<_>>();
            let mut actual = Vec::new();
            for_each_scalar_coordinate_filtered_cached(
                view,
                root,
                0,
                None,
                &mut filtered,
                |coordinate| matches!(coordinate, dae::CoordinateView::Algebraic(_)),
                |coordinate, scalar| actual.push((coordinate, scalar)),
            )
            .unwrap();
            assert_eq!(actual, expected);
            assert!(!actual.is_empty());
            assert!(actual.iter().all(|(_, scalar)| *scalar == 0));
        }
    });
}

fn record_argument() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "record Pair Real x; end Pair; noEvent(Pair(u)).x; first(p);";
    let source = sources.add("query_record.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let record = model
            .types(|types| types.record(VarName::new("Pair"), [(VarName::new("x"), real)], at))?;
        let (first, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("first"), [real], [real], at),
            |model, reservation| {
                let (u, y) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("u"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                    ))
                })?;
                let field = model.expressions(|expressions| {
                    let u = expressions.at(at).function_parameter(u)?;
                    let pair = expressions.at(at).record(record, [u])?;
                    let opaque = expressions
                        .at(at)
                        .builtin(dae::PureBuiltin::NoEvent, [pair])?;
                    expressions.at(at).field(opaque, 0)
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| {
                    functions.assign(&mut body, y, field, at)?;
                    functions.define(body, at)
                })
            },
        )?;
        let p = model.variables(|variables| {
            variables.parameter(VarName::new("p"), real, at, Default::default())
        })?;
        model.expressions(|expressions| {
            let p = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(p))?;
            expressions.at(at).call(first, 0, [p])?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn query_free_calls_contribute_no_queried_dependency_over_a_record_body() {
    record_argument().inspect(|view| {
        let root = *calls(view).last().unwrap();
        let mut mixed = ScalarCoordinateProjectionCache::default();
        let error = project(view, root, &mut mixed, false).unwrap_err();
        assert!(error.contains("UnsupportedRecordOperation"));
        assert_eq!(project(view, root, &mut mixed, true).unwrap(), []);
    });
}

