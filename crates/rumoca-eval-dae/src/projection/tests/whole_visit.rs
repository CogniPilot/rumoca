use super::*;

fn finish_expression<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    finished: dae::ExprId<'dae>,
    fault: bool,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let sum = expressions
        .at(at)
        .builtin(dae::PureBuiltin::Sum, [finished])?;
    let product = expressions
        .at(at)
        .builtin(dae::PureBuiltin::Product, [finished])?;
    let result = expressions
        .at(at)
        .binary(dae::BinaryOperator::Add, sum, product)?;
    if !fault {
        return Ok(result);
    }
    let zero = expressions.at(at).literal(dae::DaeLiteral::Integer(0))?;
    let bad = expressions.at(at).index(
        finished,
        [dae::Subscript::Index {
            expression: zero,
            provenance: at,
        }],
    )?;
    expressions
        .at(at)
        .binary(dae::BinaryOperator::Add, result, bad)
}

fn algebraic(coordinate: dae::CoordinateView<'_>, scalar: usize) -> (u32, usize) {
    let dae::CoordinateView::Algebraic(id) = coordinate else {
        panic!("algebraic actual");
    };
    (id.index(), scalar)
}

fn fixture(fault: bool) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "whole_visit.mo",
        "Real u[4]; sum(fold(u)) + product(fold(u));",
    );
    let at = provenance(source, 0, 10);
    dae::Dae::construct(sources, |model| {
        let (vector, scalar) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [4]), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
            ))
        })?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("whole"), [vector], [vector, scalar], at),
            |model, reservation| {
                let (input, carried, output) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("u"), 0, at)?,
                        functions.output(&reservation, VarName::new("carried"), 0, at)?,
                        functions.output(&reservation, VarName::new("result"), 1, at)?,
                    ))
                })?;
                let input = model
                    .expressions(|expressions| expressions.at(at).function_parameter(input))?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, carried, input, at))?;
                let domain = model.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                provenance: rumoca_core::IndexBinderProvenance::Generated,
                                id: 0,
                                display_name: "i".into(),
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
                let finished = model.functions(|functions| functions.read(&body, carried, at))?;
                let result = model.expressions(|expressions| {
                    finish_expression(expressions, finished, fault, at)
                })?;
                model.functions(|functions| {
                    functions.assign(&mut body, output, result, at)?;
                    functions.define(body, at)
                })
            },
        )?;
        let variable = model.variables(|variables| {
            variables.algebraic(VarName::new("x"), vector, at, Default::default())
        })?;
        model.expressions(|expressions| {
            let argument = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(variable))?;
            expressions.at(at).call(function, 1, [argument])?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn completed_whole_walk_preserves_ordered_dependencies_and_fold_edges() {
    fixture(false).inspect(|view| {
        let root = view.expression_id(view.expression_count() - 1).unwrap();
        let mut results = Vec::new();
        let mut caches = Vec::new();
        for reference in [false, true] {
            let mut cache = ScalarCoordinateProjectionCache {
                uncached_whole_visits: reference,
                ..Default::default()
            };
            let mut result = Vec::new();
            for_each_scalar_coordinate_cached(
                view,
                root,
                0,
                None,
                &mut cache,
                |coordinate, scalar| {
                    result.push(algebraic(coordinate, scalar));
                },
            )
            .unwrap();
            results.push(result);
            caches.push(cache);
        }
        assert_eq!(results[0], results[1]);
        assert_eq!(results[0], [(0, 0), (0, 1), (0, 2), (0, 3)]);
        assert_eq!(caches[0].function_results, caches[1].function_results);
        assert_eq!(caches[0].completed_folds, caches[1].completed_folds);
        assert_eq!(caches[0].fold_edges, caches[1].fold_edges);
        assert!(caches[0].whole_visit_hits > 0);
        assert_eq!(caches[1].whole_visit_hits, 0);
        assert!(caches[0].all_scalar_entries < caches[1].all_scalar_entries);
    });
}

#[test]
fn a_later_address_fault_keeps_the_original_error_and_no_function_summary() {
    fixture(true).inspect(|view| {
        let root = view.expression_id(view.expression_count() - 1).unwrap();
        let mut errors = Vec::new();
        for reference in [false, true] {
            let mut cache = ScalarCoordinateProjectionCache {
                uncached_whole_visits: reference,
                ..Default::default()
            };
            errors.push(
                for_each_scalar_coordinate_cached(view, root, 0, None, &mut cache, |_, _| {})
                    .unwrap_err(),
            );
            assert!(cache.function_results.is_empty());
        }
        assert_eq!(errors[0], errors[1]);
        assert!(matches!(
            errors[0],
            ProjectionError::IndexOutOfBounds {
                index: 0,
                extent: 4,
                ..
            }
        ));
    });
}
