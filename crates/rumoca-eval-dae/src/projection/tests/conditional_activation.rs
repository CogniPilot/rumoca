use super::*;

pub(super) fn project<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
    filtered: bool,
) -> Result<Vec<(dae::CoordinateView<'dae>, usize)>, ProjectionError> {
    let mut values = Vec::new();
    if filtered {
        for_each_scalar_coordinate_filtered_cached(
            view,
            root,
            0,
            None,
            cache,
            |_| true,
            |v, s| values.push((v, s)),
        )?;
    } else {
        for_each_scalar_coordinate_cached(view, root, 0, None, cache, |v, s| values.push((v, s)))?;
    }
    Ok(values)
}

pub(super) fn reference<'dae>() -> ScalarCoordinateProjectionCache<'dae> {
    ScalarCoordinateProjectionCache {
        uncached_fold_reference: true,
        uncached_parameter_fragments: true,
        uncached_literal_update_sweeps: true,
        ..Default::default()
    }
}

fn refusal(error: ProjectionError, extent: u32) {
    assert!(
        matches!(error, ProjectionError::IndexOutOfBounds { index: 2, extent: actual, .. } if actual == extent)
    );
}

pub(super) fn model_inputs<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<[dae::ExprId<'dae>; 3], dae::DaeConstructionError> {
    let boolean =
        model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at))?;
    let (a, b, guard) = model.variables(|v| {
        Ok((
            v.algebraic(VarName::new("a"), vector, at, Default::default())?,
            v.algebraic(VarName::new("b"), real, at, Default::default())?,
            v.input(
                VarName::new("guard"),
                boolean,
                dae::InputVariability::Discrete,
                at,
                Default::default(),
            )?,
        ))
    })?;
    model.expressions(|e| {
        Ok([
            e.at(at).coordinate(dae::CoordinateInput::Algebraic(a))?,
            e.at(at).coordinate(dae::CoordinateInput::Algebraic(b))?,
            e.at(at).coordinate(dae::CoordinateInput::Input(guard))?,
        ])
    })
}

fn address_model() -> (dae::Dae, Vec<usize>) {
    let text = "if guard then a[2]+b else b; a[2]; elseif a[2]>0;";
    let mut sources = SourceMap::new();
    let source = sources.add("conditional_projection.mo", text);
    let at = provenance(source, 0, text.len());
    let mut roots = Vec::new();
    let model = dae::Dae::construct(sources, |m| {
        let vector =
            m.types(|t| t.derived(dae::ValueType::array(dae::ScalarType::Real, [1]), at))?;
        let real = m.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let [a, b, guard] = model_inputs(m, vector, real, at)?;
        m.expressions(|e| {
            let two = e.at(at).literal(dae::DaeLiteral::Integer(2))?;
            let bad = e.at(at).index(
                a,
                [dae::Subscript::Index {
                    expression: two,
                    provenance: at,
                }],
            )?;
            let arm = e.at(at).binary(dae::BinaryOperator::Add, bad, b)?;
            let lazy = e.at(at).conditional([(guard, arm)], b)?;
            let first = e.at(at).binary(dae::BinaryOperator::Add, lazy, bad)?;
            let reverse = e.at(at).binary(dae::BinaryOperator::Add, bad, lazy)?;
            let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let invalid_guard = e.at(at).binary(dae::BinaryOperator::Greater, bad, zero)?;
            let later = e.at(at).conditional([(guard, b), (invalid_guard, b)], b)?;
            let first_guard = e.at(at).conditional([(invalid_guard, b)], b)?;
            let yes = e.at(at).literal(dae::DaeLiteral::Boolean(true))?;
            let no = e.at(at).literal(dae::DaeLiteral::Boolean(false))?;
            let selected = e.at(at).conditional([(yes, bad)], b)?;
            let impossible = e.at(at).conditional([(no, bad)], b)?;
            let nested = e.at(at).conditional([(guard, selected)], b)?;
            roots.extend(
                [
                    lazy,
                    bad,
                    first,
                    reverse,
                    later,
                    first_guard,
                    selected,
                    impossible,
                    nested,
                ]
                .map(|r| r.index() as usize),
            );
            Ok(())
        })
    })
    .unwrap();
    (model, roots)
}

#[test]
fn same_gather_keeps_conditional_and_guaranteed_visits_distinct_in_both_orders() {
    let (model, roots) = address_model();
    model.inspect(|view| {
        let mut cache = ScalarCoordinateProjectionCache::default();
        let lazy = view.expression_id(roots[0]).unwrap();
        let values = project(view, lazy, &mut cache, false).unwrap();
        assert!(
            values
                .iter()
                .any(|(v, _)| matches!(v, dae::CoordinateView::Algebraic(v) if v.index() == 0))
        );
        assert!(
            values
                .iter()
                .any(|(v, _)| matches!(v, dae::CoordinateView::Algebraic(v) if v.index() == 1)),
            "a deferred address must still visit later siblings"
        );
        assert_eq!(
            values,
            project(view, lazy, &mut reference(), false).unwrap()
        );
        for ordinal in [2, 3] {
            let root = view.expression_id(roots[ordinal]).unwrap();
            refusal(project(view, root, &mut cache, false).unwrap_err(), 1);
            refusal(project(view, root, &mut reference(), false).unwrap_err(), 1);
        }
        refusal(
            project(
                view,
                view.expression_id(roots[1]).unwrap(),
                &mut cache,
                false,
            )
            .unwrap_err(),
            1,
        );
    });
}

#[test]
fn later_guards_and_literal_reachability_preserve_exact_source_faults() {
    let (model, roots) = address_model();
    model.inspect(|view| {
        for filtered in [false, true] {
            let mut cache = ScalarCoordinateProjectionCache::default();
            for ordinal in [4, 7, 8] {
                let root = view.expression_id(roots[ordinal]).unwrap();
                assert_eq!(
                    project(view, root, &mut cache, filtered).unwrap(),
                    project(view, root, &mut reference(), filtered).unwrap()
                );
            }
            let impossible = project(
                view,
                view.expression_id(roots[7]).unwrap(),
                &mut cache,
                filtered,
            )
            .unwrap();
            assert!(
                impossible.iter().all(
                    |(v, _)| !matches!(v, dae::CoordinateView::Algebraic(v) if v.index() == 0)
                )
            );
            for ordinal in [5, 6] {
                let root = view.expression_id(roots[ordinal]).unwrap();
                let error = project(view, root, &mut cache, filtered).unwrap_err();
                assert_eq!(
                    fault_span(&error),
                    view.expression(view.expression_id(roots[1]).unwrap())
                        .unwrap()
                        .provenance()
                        .span()
                );
                refusal(error, 1);
            }
        }
    });
}

fn fault_span(error: &ProjectionError) -> Span {
    let ProjectionError::IndexOutOfBounds { span, .. } = error else {
        panic!("expected address fault")
    };
    *span
}

fn call_model() -> (dae::Dae, [usize; 2]) {
    let text = "if guard then select(a,2) else b; select(a,2);";
    let mut sources = SourceMap::new();
    let source = sources.add("conditional_calls.mo", text);
    let at = provenance(source, 0, text.len());
    let mut roots = [0; 2];
    let model = dae::Dae::construct(sources, |m| {
        let vector =
            m.types(|t| t.derived(dae::ValueType::array(dae::ScalarType::Real, [1]), at))?;
        let real = m.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let integer =
            m.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let select = construct_select_function(m, vector, integer, real, at)?;
        let (function, ()) = m.function(
            dae::FunctionSignature::new(
                VarName::new("forwardSelect"),
                [vector, integer],
                [real],
                at,
            ),
            |m, reservation| {
                let (values, index, output) = m.functions(|f| {
                    Ok((
                        f.parameter(&reservation, VarName::new("values"), 0, at)?,
                        f.parameter(&reservation, VarName::new("index"), 1, at)?,
                        f.output(&reservation, VarName::new("output"), 0, at)?,
                    ))
                })?;
                let result = m.expressions(|e| {
                    let values = e.at(at).function_parameter(values)?;
                    let index = e.at(at).function_parameter(index)?;
                    e.at(at).call(select, 0, [values, index])
                })?;
                let mut body = m.functions(|f| f.begin(reservation, at))?;
                m.functions(|f| f.assign(&mut body, output, result, at))?;
                m.functions(|f| f.define(body, at))
            },
        )?;
        let [a, b, guard] = model_inputs(m, vector, real, at)?;
        m.expressions(|e| {
            let two = e.at(at).literal(dae::DaeLiteral::Integer(2))?;
            let call = e.at(at).call(function, 0, [a, two])?;
            let lazy = e.at(at).conditional([(guard, call)], b)?;
            roots = [lazy.index() as usize, call.index() as usize];
            Ok(())
        })
    })
    .unwrap();
    (model, roots)
}

#[test]
fn conditional_call_summaries_and_query_child_caches_cannot_validate_strict_calls() {
    let (model, roots) = call_model();
    model.inspect(|view| {
        let lazy = view.expression_id(roots[0]).unwrap();
        let direct = view.expression_id(roots[1]).unwrap();
        for reverse in [false, true] {
            for filtered in [false, true] {
                check_conditional_call_order(view, lazy, direct, reverse, filtered);
            }
        }
    });
}

fn check_conditional_call_order<'dae>(
    view: dae::DaeView<'dae>,
    lazy: dae::ExprId<'dae>,
    direct: dae::ExprId<'dae>,
    reverse: bool,
    filtered: bool,
) {
    let mut cache = ScalarCoordinateProjectionCache::default();
    if reverse {
        refusal(project(view, direct, &mut cache, filtered).unwrap_err(), 1);
    }
    let expected = project(view, lazy, &mut reference(), filtered).unwrap();
    assert_eq!(project(view, lazy, &mut cache, filtered).unwrap(), expected);
    assert_eq!(project(view, lazy, &mut cache, filtered).unwrap(), expected);
    refusal(project(view, direct, &mut cache, filtered).unwrap_err(), 1);
    // A query excluding every argument reads no queried coordinate through
    // either call: dependency projection contributes nothing for them.
    for root in [lazy, direct] {
        for_each_scalar_coordinate_filtered_cached(
            view,
            root,
            0,
            None,
            &mut cache,
            |_| false,
            |_, _| panic!("excluded coordinate"),
        )
        .unwrap();
    }
    assert!(
        cache
            .function_results
            .keys()
            .filter(|k| k.activation == Activation::Guaranteed && !k.integers.is_empty())
            .all(|k| k.integers[0].value != 2),
        "failed specialized profile cannot publish"
    );
}

fn fold_model() -> (dae::Dae, [usize; 2]) {
    let text = "if guard then nested(a) else b; nested(a);";
    let mut sources = SourceMap::new();
    let source = sources.add("conditional_nested_fold.mo", text);
    let at = provenance(source, 0, text.len());
    let mut roots = [0; 2];
    let model = dae::Dae::construct(sources, |m| {
        let vector =
            m.types(|t| t.derived(dae::ValueType::array(dae::ScalarType::Real, [7]), at))?;
        let real = m.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let function = super::fold_context::nested_function(m, vector, real, 4, false, at)?;
        let [a, b, guard] = model_inputs(m, vector, real, at)?;
        m.expressions(|e| {
            let call = e.at(at).call(function, 0, [a])?;
            let lazy = e.at(at).conditional([(guard, call)], b)?;
            roots = [lazy.index() as usize, call.index() as usize];
            Ok(())
        })
    })
    .unwrap();
    (model, roots)
}

#[test]
fn deferred_nested_folds_restore_activation_and_completed_replay_cannot_hide_refusals() {
    let (model, roots) = fold_model();
    model.inspect(|view| {
        let lazy = view.expression_id(roots[0]).unwrap();
        let direct = view.expression_id(roots[1]).unwrap();
        for reverse in [false, true] {
            let mut cache = ScalarCoordinateProjectionCache::default();
            if reverse {
                assert!(matches!(
                    project(view, direct, &mut cache, false),
                    Err(ProjectionError::IndexOutOfBounds {
                        index: 8,
                        extent: 7,
                        ..
                    })
                ));
            }
            let expected = project(view, lazy, &mut reference(), false).unwrap();
            assert_eq!(project(view, lazy, &mut cache, false).unwrap(), expected);
            assert!(
                !cache.completed_folds.is_empty(),
                "exercise deferred graph publication"
            );
            assert!(
                cache
                    .completed_folds
                    .keys()
                    .all(|node| node.activation == Activation::Conditional)
            );
            cache.function_results.clear();
            assert_eq!(project(view, lazy, &mut cache, false).unwrap(), expected);
            assert!(cache.fold_reuses > 0, "exercise completed fold replay");
            for _ in 0..2 {
                assert!(matches!(
                    project(view, direct, &mut cache, false),
                    Err(ProjectionError::IndexOutOfBounds {
                        index: 8,
                        extent: 7,
                        ..
                    })
                ));
                assert!(
                    cache
                        .completed_folds
                        .keys()
                        .all(|node| node.activation == Activation::Conditional),
                    "failed guaranteed folds cannot publish"
                );
            }
        }
    });
}

#[test]
fn conditional_record_gather_keeps_field_dependencies_and_strict_address_errors() {
    let text = "if guard then pairs[2] else pairs[1];";
    let mut sources = SourceMap::new();
    let source = sources.add("conditional_record_projection.mo", text);
    let at = provenance(source, 0, text.len());
    let mut roots = [0; 2];
    let model = dae::Dae::construct(sources, |m| {
        let real = m.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let vector =
            m.types(|t| t.derived(dae::ValueType::array(dae::ScalarType::Real, [1]), at))?;
        let pair = m.types(|t| {
            t.record(
                VarName::new("Pair"),
                [
                    (VarName::new("first"), real),
                    (VarName::new("second"), real),
                ],
                at,
            )
        })?;
        let [a, b, guard] = model_inputs(m, vector, real, at)?;
        m.expressions(|e| {
            let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let two = e.at(at).literal(dae::DaeLiteral::Integer(2))?;
            let scalar = e.at(at).index(
                a,
                [dae::Subscript::Index {
                    expression: one,
                    provenance: at,
                }],
            )?;
            let record = e.at(at).record(pair, [scalar, b])?;
            let records = e.at(at).array([record])?;
            let bad = e.at(at).index(
                records,
                [dae::Subscript::Index {
                    expression: two,
                    provenance: at,
                }],
            )?;
            let conditional = e.at(at).conditional([(guard, bad)], record)?;
            let lazy = e.at(at).field(conditional, 0)?;
            let direct = e.at(at).field(bad, 0)?;
            roots = [lazy.index() as usize, direct.index() as usize];
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let mut cache = ScalarCoordinateProjectionCache::default();
        let root = view.expression_id(roots[0]).unwrap();
        let values = project(view, root, &mut cache, false).unwrap();
        assert_eq!(
            values,
            project(view, root, &mut reference(), false).unwrap()
        );
        assert!(
            values
                .iter()
                .any(|(v, _)| matches!(v, dae::CoordinateView::Algebraic(v) if v.index() == 0))
        );
        assert!(
            !values
                .iter()
                .any(|(v, _)| matches!(v, dae::CoordinateView::Algebraic(v) if v.index() == 1)),
            "project only the selected record field"
        );
        refusal(
            project(
                view,
                view.expression_id(roots[1]).unwrap(),
                &mut cache,
                false,
            )
            .unwrap_err(),
            1,
        );
    });
}
