use super::*;

fn range(name: &str) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: name.to_string(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    }
}

fn nested_index<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    outer: dae::DomainId<'dae>,
    inner: dae::DomainId<'dae>,
    offset: i64,
    only_outer: bool,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let p = model.domains(|domains| domains.binder(outer, 0, at))?;
    let q = model.domains(|domains| domains.binder(inner, 0, at))?;
    model.expressions(|expressions| {
        let p = expressions.at(at).binder(p)?;
        let q = expressions.at(at).binder(q)?;
        if only_outer {
            return Ok(p);
        }
        let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
        let two = expressions.at(at).literal(dae::DaeLiteral::Integer(2))?;
        let last = expressions
            .at(at)
            .literal(dae::DaeLiteral::Integer(offset))?;
        let offset = expressions
            .at(at)
            .binary(dae::BinaryOperator::Subtract, p, one)?;
        let offset = expressions
            .at(at)
            .binary(dae::BinaryOperator::Multiply, offset, two)?;
        let index = expressions
            .at(at)
            .binary(dae::BinaryOperator::Add, offset, q)?;
        expressions
            .at(at)
            .binary(dae::BinaryOperator::Add, index, last)
    })
}

fn update_nested<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    inner: &mut dae::FunctionLoop<'dae>,
    targets: [dae::FunctionValueId<'dae>; 2],
    selected: dae::ExprId<'dae>,
    at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let [y, z] = targets;
    let (old_y, old_z) = model.functions(|functions| {
        Ok((
            functions.read(inner.body(), y, at)?,
            functions.read(inner.body(), z, at)?,
        ))
    })?;
    let (next_y, next_z) = model.expressions(|expressions| {
        Ok((
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Add, old_y, old_z)?,
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Add, old_z, selected)?,
        ))
    })?;
    model.functions(|functions| {
        functions.assign_loop(inner, y, next_y, at)?;
        functions.assign_loop(inner, z, next_z, at)
    })
}

pub(super) fn nested_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    scalar: dae::ValueTypeId<'dae>,
    offset: i64,
    only_outer: bool,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(
                VarName::new("nested"),
                [vector],
                [scalar, scalar, scalar],
                at,
            ),
            |model, reservation| {
                let (u, y, z, unrelated) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("u"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                        functions.output(&reservation, VarName::new("z"), 1, at)?,
                        functions.output(&reservation, VarName::new("unrelated"), 2, at)?,
                    ))
                })?;
                let u =
                    model.expressions(|expressions| expressions.at(at).function_parameter(u))?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                for (target, index) in [(y, 1), (z, 2), (unrelated, 7)] {
                    let selected = model.expressions(|expressions| {
                        let index = expressions
                            .at(at)
                            .literal(dae::DaeLiteral::Integer(index))?;
                        expressions.at(at).index(
                            u,
                            [dae::Subscript::Index {
                                expression: index,
                                provenance: at,
                            }],
                        )
                    })?;
                    model
                        .functions(|functions| functions.assign(&mut body, target, selected, at))?;
                }
                let outer_domain = model.domains(|domains| domains.structured(range("i"), at))?;
                let inner_domain =
                    model.domains(|domains| domains.nested(outer_domain, range("i"), at))?;
                let outer = model.functions(|functions| {
                    functions.begin_loop(body, outer_domain, [y, z, unrelated], at)
                })?;
                let mut inner = model.functions(|functions| {
                    functions.begin_nested_loop(outer, inner_domain, [y, z], at)
                })?;
                let index =
                    nested_index(model, outer_domain, inner_domain, offset, only_outer, at)?;
                let selected = model.expressions(|expressions| {
                    expressions.at(at).index(
                        u,
                        [dae::Subscript::Index {
                            expression: index,
                            provenance: at,
                        }],
                    )
                })?;
                update_nested(model, &mut inner, [y, z], selected, at)?;
                let outer = model.functions(|functions| functions.finish_nested_loop(inner, at))?;
                body = model.functions(|functions| functions.finish_loop(outer, at))?;
                let (last_y, last_z) = model.functions(|functions| {
                    Ok((functions.read(&body, y, at)?, functions.read(&body, z, at)?))
                })?;
                let result = model.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, last_y, last_z)
                })?;
                model.functions(|functions| {
                    functions.assign(&mut body, y, result, at)?;
                    functions.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

pub(in crate::projection) fn fold_model(offset: i64) -> dae::Dae {
    fold_model_used_axes(offset, false)
}

pub(in crate::projection) fn fold_model_used_axes(offset: i64, only_outer: bool) -> dae::Dae {
    let text = "nested accumulation with shadowed i, sequential y/z reads and an unrelated carry";
    let mut sources = SourceMap::new();
    let source = sources.add("nested_projection.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [7]), at))?;
        let scalar = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let function = nested_function(model, vector, scalar, offset, only_outer, at)?;
        for name in ["a", "b"] {
            let variable = model.variables(|variables| {
                variables.algebraic(VarName::new(name), vector, at, Default::default())
            })?;
            model.expressions(|expressions| {
                let argument = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Algebraic(variable))?;
                expressions.at(at).call(function, 0, [argument])?;
                Ok(())
            })?;
        }
        Ok(())
    })
    .unwrap()
}

fn dependencies<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
) -> Vec<(u32, usize)> {
    let mut dependencies = Vec::new();
    for_each_scalar_coordinate_cached(view, root, 0, None, cache, |coordinate, scalar| {
        if let dae::CoordinateView::Algebraic(variable) = coordinate {
            dependencies.push((variable.index(), scalar));
        } else {
            panic!("expected only actual model array coordinates");
        }
    })
    .unwrap();
    dependencies.sort_unstable();
    dependencies.dedup();
    dependencies
}

fn check_offset_incidence(view: dae::DaeView<'_>, offset: usize) {
    let calls = (0..view.expression_count())
        .filter_map(|i| view.expression_id(i))
        .filter(|expression| {
            matches!(
                view.expression(*expression).unwrap().operation(),
                dae::ExpressionOperation::Call { .. }
            )
        })
        .collect::<Vec<_>>();
    let mut original = ScalarCoordinateProjectionCache {
        uncached_fold_reference: true,
        ..Default::default()
    };
    let mut graph = ScalarCoordinateProjectionCache::default();
    let expected_scalars = [0, 1]
        .into_iter()
        .chain(offset..offset + 4)
        .collect::<std::collections::BTreeSet<_>>();
    for (variable, root) in calls.into_iter().enumerate() {
        let expected = expected_scalars
            .iter()
            .map(|scalar| (variable as u32, *scalar))
            .collect::<Vec<_>>();
        assert_eq!(dependencies(view, root, &mut original), expected);
        assert_eq!(dependencies(view, root, &mut graph), expected);
    }
}

#[test]
fn nested_address_offsets_match_original_exact_incidence_including_sparse_controls() {
    for offset in 0..4 {
        fold_model(offset).inspect(|view| check_offset_incidence(view, offset as usize));
    }
}

fn ordered_dependencies<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
) -> Vec<(u32, usize)> {
    let mut values = Vec::new();
    for_each_scalar_coordinate_cached(view, root, 0, None, cache, |coordinate, scalar| {
        let dae::CoordinateView::Algebraic(variable) = coordinate else {
            panic!("checked test calls use algebraic arguments");
        };
        values.push((variable.index(), scalar));
    })
    .unwrap();
    values
}

fn check_fragment_order(view: dae::DaeView<'_>) -> u64 {
    let mut cached = ScalarCoordinateProjectionCache::default();
    let mut reference = ScalarCoordinateProjectionCache {
        uncached_parameter_fragments: true,
        ..Default::default()
    };
    for ordinal in 0..view.expression_count() {
        let root = view.expression_id(ordinal).unwrap();
        if !matches!(
            view.expression(root).unwrap().operation(),
            dae::ExpressionOperation::Call { .. }
        ) {
            continue;
        }
        assert_eq!(
            ordered_dependencies(view, root, &mut cached),
            ordered_dependencies(view, root, &mut reference),
            "the public visitor keeps first-occurrence order"
        );
        assert_eq!(
            cached.function_results, reference.function_results,
            "function formal ownership and summary order remain exact"
        );
        assert_eq!(
            cached.completed_folds, reference.completed_folds,
            "each scalar and lexical parent keeps its complete closure"
        );
    }
    assert_eq!(reference.fragment_hits, 0);
    cached.fragment_hits
}

#[test]
fn parameter_fragments_match_uncached_ordered_summaries_at_distinct_parent_contexts() {
    let replayed: u64 = (0..4)
        .map(|offset| fold_model(offset).inspect(check_fragment_order))
        .sum();
    assert!(
        replayed > 0,
        "overlapping addresses exercise completed fragment replay; disjoint addresses retain independent captures"
    );
}

#[test]
fn lower_bound_address_error_matches_original_and_cannot_publish_a_graph() {
    fold_model(-1).inspect(|view| {
        let root = view.expression_id(view.expression_count() - 1).unwrap();
        for reference in [true, false] {
            let mut cache = ScalarCoordinateProjectionCache {
                uncached_fold_reference: reference,
                ..Default::default()
            };
            let error =
                for_each_scalar_coordinate_cached(view, root, 0, None, &mut cache, |_, _| {})
                    .unwrap_err();
            assert!(matches!(
                error,
                ProjectionError::IndexOutOfBounds {
                    index: 0,
                    extent: 7,
                    ..
                }
            ));
            assert!(cache.function_results.is_empty());
            assert!(cache.completed_folds.is_empty());
        }
    });
}

#[test]
fn nested_fold_graph_matches_original_walk_and_completes_cyclic_closures() {
    fold_model(2).inspect(|view| {
        let calls = (0..view.expression_count())
            .filter_map(|i| view.expression_id(i))
            .filter(|expression| {
                matches!(
                    view.expression(*expression).unwrap().operation(),
                    dae::ExpressionOperation::Call { .. }
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(calls.len(), 2);
        let mut optimized = ScalarCoordinateProjectionCache::default();
        let mut original = ScalarCoordinateProjectionCache {
            uncached_fold_reference: true,
            ..Default::default()
        };
        for (variable, root) in calls.into_iter().enumerate() {
            let expected = (0..6)
                .map(|scalar| (variable as u32, scalar))
                .collect::<Vec<_>>();
            assert_eq!(dependencies(view, root, &mut original), expected);
            assert_eq!(dependencies(view, root, &mut optimized), expected);
        }
        assert!(
            original.fold_suppressions > 0,
            "the original reference exercises carried-value recursion"
        );
        assert!(
            optimized.fold_graph_repeated_edges > 0,
            "cyclic and already pending graph edges remain represented"
        );
        assert!(!optimized.completed_folds.is_empty());
        let mut parents = optimized
            .completed_folds
            .keys()
            .filter(|node| node.fold.ordinal() == 1 && node.carried == 1 && node.scalar == 0)
            .map(|node| node.parent.as_ref().clone())
            .collect::<Vec<_>>();
        parents.sort();
        assert_eq!(
            parents,
            [vec![(0, vec![1])], vec![(0, vec![2])]],
            "equal final incidence cannot collapse distinct checked parent addresses"
        );
        assert!(optimized.fold_walks < original.fold_walks);
    });
}

#[test]
fn completed_fold_reuse_matches_original_with_an_exact_empty_parent_context() {
    positive_model().inspect(|view| {
        let root = view.expression_id(view.expression_count() - 1).unwrap();
        let mut optimized = ScalarCoordinateProjectionCache::default();
        let mut original = ScalarCoordinateProjectionCache {
            uncached_fold_reference: true,
            ..Default::default()
        };
        let expected = (0..3).map(|scalar| (0, scalar)).collect::<Vec<_>>();
        assert_eq!(dependencies(view, root, &mut original), expected);
        assert_eq!(dependencies(view, root, &mut optimized), expected);
        assert!(optimized.fold_graph_repeated_edges > 0);
        assert!(optimized.fold_walks < original.fold_walks);
        optimized.function_results.clear();
        assert_eq!(dependencies(view, root, &mut optimized), expected);
        assert!(
            optimized.fold_reuses > 0,
            "completed reuse must be exercised"
        );
        assert!(optimized.fold_walks < original.fold_walks);
        eprintln!(
            "exact completed folds: original walks {}, optimized walks {}, reuses {}",
            original.fold_walks, optimized.fold_walks, optimized.fold_reuses
        );
    });
}

#[test]
fn failed_fold_projection_is_never_cached_and_retains_the_exact_address_error() {
    fold_model(4).inspect(|view| {
        let root = view.expression_id(view.expression_count() - 1).unwrap();
        for original in [true, false] {
            let mut cache = ScalarCoordinateProjectionCache {
                uncached_fold_reference: original,
                ..Default::default()
            };
            for _ in 0..2 {
                check_failed_projection(view, root, &mut cache);
                assert!(
                    cache.completed_folds.is_empty(),
                    "failed graph nodes stay uncached"
                );
            }
        }
    });
}

fn check_failed_projection<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
) {
    let error =
        for_each_scalar_coordinate_cached(view, root, 0, None, cache, |_, _| {}).unwrap_err();
    assert!(matches!(
        error,
        ProjectionError::IndexOutOfBounds {
            index: 8,
            extent: 7,
            ..
        }
    ));
    assert!(
        cache.function_results.is_empty(),
        "a failed summary cannot become reusable"
    );
}

fn specialized_model() -> dae::Dae {
    let text = "select(a,1); select(a,2); select(a,4);";
    let mut sources = SourceMap::new();
    let source = sources.add("selector_address.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let integer = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let function = construct_select_function(model, vector, integer, real, at)?;
        let a = model.variables(|variables| {
            variables.algebraic(VarName::new("a"), vector, at, Default::default())
        })?;
        model.expressions(|expressions| {
            let a = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(a))?;
            for index in [1, 2, 4] {
                let index = expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(index))?;
                expressions.at(at).call(function, 0, [a, index])?;
            }
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn specialized_actual_arguments_remain_distinct_and_out_of_bounds_stays_a_refusal() {
    specialized_model().inspect(|view| {
        let calls = (0..view.expression_count())
            .filter_map(|i| view.expression_id(i))
            .filter(|expression| {
                matches!(
                    view.expression(*expression).unwrap().operation(),
                    dae::ExpressionOperation::Call { .. }
                )
            })
            .collect::<Vec<_>>();
        let mut cache = ScalarCoordinateProjectionCache::default();
        assert_eq!(dependencies(view, calls[0], &mut cache), [(0, 0)]);
        assert_eq!(dependencies(view, calls[1], &mut cache), [(0, 1)]);
        let error =
            for_each_scalar_coordinate_cached(view, calls[2], 0, None, &mut cache, |_, _| {})
                .unwrap_err();
        assert!(matches!(
            error,
            ProjectionError::IndexOutOfBounds {
                index: 4,
                extent: 3,
                ..
            }
        ));
        assert_eq!(cache.function_results.len(), 3);
        let mut profiles = Vec::new();
        for (key, entry) in &cache.function_results {
            match entry {
                FunctionSummaryEntry::NeedsIntegers(needed) => {
                    assert!(key.integers.is_empty());
                    assert_eq!(needed, &[(1, 0)]);
                }
                FunctionSummaryEntry::Complete(dependencies) => {
                    assert_eq!(key.integers.len(), 1);
                    let binding = key.integers[0];
                    assert_eq!((binding.parameter, binding.scalar), (1, 0));
                    assert_eq!(
                        dependencies,
                        &[FunctionParameterDependency::Scalar {
                            activation: crate::projection::Activation::Guaranteed,
                            parameter: 0,
                            scalar: usize::try_from(binding.value - 1).unwrap(),
                        }]
                    );
                    profiles.push(binding.value);
                }
                FunctionSummaryEntry::Direct => panic!("direct fallback cannot be cached"),
            }
        }
        profiles.sort_unstable();
        assert_eq!(profiles, [1, 2], "failed profile 4 cannot be published");
        assert_eq!(cache.fold_reuses, 0);
        assert!(cache.completed_folds.is_empty());
    });
}

fn simple_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    scalar: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(VarName::new("simple"), [vector], [scalar, scalar], at),
            |model, reservation| {
                let (u, y, z) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("u"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                        functions.output(&reservation, VarName::new("z"), 1, at)?,
                    ))
                })?;
                let u =
                    model.expressions(|expressions| expressions.at(at).function_parameter(u))?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                for (target, index) in [(y, 1), (z, 2)] {
                    let value = model.expressions(|expressions| {
                        let index = expressions
                            .at(at)
                            .literal(dae::DaeLiteral::Integer(index))?;
                        expressions.at(at).index(
                            u,
                            [dae::Subscript::Index {
                                expression: index,
                                provenance: at,
                            }],
                        )
                    })?;
                    model.functions(|functions| functions.assign(&mut body, target, value, at))?;
                }
                let domain = model.domains(|domains| domains.structured(range("i"), at))?;
                let mut loop_body =
                    model.functions(|functions| functions.begin_loop(body, domain, [y, z], at))?;
                let binder = model.domains(|domains| domains.binder(domain, 0, at))?;
                let selected = model.expressions(|expressions| {
                    let index = expressions.at(at).binder(binder)?;
                    let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
                    let index = expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, index, one)?;
                    expressions.at(at).index(
                        u,
                        [dae::Subscript::Index {
                            expression: index,
                            provenance: at,
                        }],
                    )
                })?;
                update_nested(model, &mut loop_body, [y, z], selected, at)?;
                let mut body = model.functions(|functions| functions.finish_loop(loop_body, at))?;
                let (last_y, last_z) = model.functions(|functions| {
                    Ok((functions.read(&body, y, at)?, functions.read(&body, z, at)?))
                })?;
                let result = model.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, last_y, last_z)
                })?;
                model.functions(|functions| {
                    functions.assign(&mut body, y, result, at)?;
                    functions.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

fn positive_model() -> dae::Dae {
    let text = "for i in 1:2 loop y:=y+z; z:=z+u[i+1]; end for; y:=y+z;";
    let mut sources = SourceMap::new();
    let source = sources.add("completed_fold.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let scalar = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let function = simple_function(model, vector, scalar, at)?;
        let variable = model.variables(|variables| {
            variables.algebraic(VarName::new("a"), vector, at, Default::default())
        })?;
        model.expressions(|expressions| {
            let argument = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(variable))?;
            expressions.at(at).call(function, 0, [argument])?;
            Ok(())
        })
    })
    .unwrap()
}

fn checked_graph_nodes<'dae>(
    view: dae::DaeView<'dae>,
) -> [crate::projection::fold_graph::FoldNode<'dae>; 2] {
    let root = view.expression_id(view.expression_count() - 1).unwrap();
    let dae::ExpressionOperation::Call { function, .. } =
        view.expression(root).unwrap().operation()
    else {
        panic!("the fixture root is a checked call");
    };
    let fold = view.function(function).unwrap().fold_id(0).unwrap();
    let transition = view.function_fold(fold).unwrap();
    std::array::from_fn(|carried| crate::projection::fold_graph::FoldNode {
        activation: crate::projection::Activation::Guaranteed,
        fold,
        carried: carried as u32,
        field: None,
        scalar: 0,
        initial: transition.initial_values().rhs(carried).unwrap(),
        update: transition.update_values().rhs(carried).unwrap(),
        parent: Default::default(),
    })
}

// Independent exhaustive graph reachability: no worklist, cache or closure code.
fn reference_graph_parameters(edges: u32, direct: u32, root: usize) -> Vec<usize> {
    let mut reached = [false; 2];
    let mut stack = vec![root];
    while let Some(node) = stack.pop() {
        if reached[node] {
            continue;
        }
        reached[node] = true;
        for next in 0..2 {
            if edges & (1 << (2 * node + next)) != 0 {
                stack.push(next);
            }
        }
    }
    (0..2)
        .filter(|parameter| {
            (0..2).any(|node| reached[node] && direct & (1 << (2 * node + parameter)) != 0)
        })
        .collect()
}

fn check_graph_case(
    nodes: &[crate::projection::fold_graph::FoldNode<'_>; 2],
    edges: u32,
    direct: u32,
) {
    let mut graph = crate::projection::fold_graph::FoldGraph::default();
    for node in nodes {
        graph.enqueue(node.clone());
    }
    for node in 0..2 {
        assert_eq!(graph.begin_next().as_ref(), Some(&nodes[node]));
        for (target, key) in nodes.iter().enumerate() {
            if edges & (1 << (2 * node + target)) != 0 {
                graph.enqueue(key.clone());
            }
        }
        for scalar in 0..2 {
            if direct & (1 << (2 * node + scalar)) != 0 {
                graph.capture(&crate::projection::FunctionParameterDependency::Scalar {
                    activation: crate::projection::Activation::Guaranteed,
                    parameter: 0,
                    scalar,
                });
            }
        }
        graph.finish_node();
    }
    assert!(graph.begin_next().is_none());
    for (node, (_, closure)) in graph.completed().into_iter().enumerate() {
        let mut actual = closure
            .iter()
            .map(|dependency| {
                let crate::projection::FunctionParameterDependency::Scalar { scalar, .. } =
                    dependency
                else {
                    panic!("these checked carries read scalar parameter elements");
                };
                *scalar
            })
            .collect::<Vec<_>>();
        actual.sort_unstable();
        assert_eq!(actual, reference_graph_parameters(edges, direct, node));
    }
}

#[test]
fn graph_fixed_point_matches_every_two_node_graph_and_parameter_assignment() {
    positive_model().inspect(|view| {
        let nodes = checked_graph_nodes(view);
        for edges in 0..16 {
            for direct in 0..16 {
                check_graph_case(&nodes, edges, direct);
            }
        }
    });
}
