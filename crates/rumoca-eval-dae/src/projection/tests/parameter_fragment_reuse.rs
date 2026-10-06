use super::*;

fn shared_results<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature = dae::FunctionSignature::new(VarName::new("shared"), [vector], [real, real], at);
    model
        .function(signature, |model, reservation| {
            let input = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, at)
            })?;
            let first = model
                .functions(|functions| functions.output(&reservation, VarName::new("a"), 0, at))?;
            let second = model
                .functions(|functions| functions.output(&reservation, VarName::new("b"), 1, at))?;
            let sum = model.expressions(|expressions| {
                let input = expressions.at(at).function_parameter(input)?;
                expressions.at(at).builtin(dae::PureBuiltin::Sum, [input])
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| {
                functions.assign(&mut body, first, sum, at)?;
                functions.assign(&mut body, second, sum, at)?;
                functions.define(body, at)
            })
        })
        .map(|(function, ())| function)
}

pub(super) fn model() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "function shared input Real u[3]; output Real a; output Real b; algorithm a:=sum(u); b:=sum(u); end shared;";
    let source = sources.add("parameter_fragment_reuse.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let function = shared_results(model, vector, real, at)?;
        for ordinal in 0..2 {
            let variable = model.variables(|variables| {
                variables.algebraic(
                    VarName::new(format!("x{ordinal}")),
                    vector,
                    at,
                    Default::default(),
                )
            })?;
            model.expressions(|expressions| {
                let input = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Algebraic(variable))?;
                expressions.at(at).call(function, ordinal, [input])?;
                Ok(())
            })?;
        }
        Ok(())
    })
    .unwrap()
}

#[test]
fn imported_complete_fragments_record_a_new_summary_and_substitute_its_actual_arguments() {
    model().inspect(|view| {
        let roots = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter(|root| {
                matches!(
                    view.expression(*root).unwrap().operation(),
                    dae::ExpressionOperation::Call { .. }
                )
            })
            .collect::<Vec<_>>();
        let mut cached = ScalarCoordinateProjectionCache::default();
        let mut reference = ScalarCoordinateProjectionCache {
            uncached_parameter_fragments: true,
            ..Default::default()
        };
        for (ordinal, root) in roots.into_iter().enumerate() {
            let actual = coordinates(view, root, &mut cached);
            assert_eq!(actual, coordinates(view, root, &mut reference));
            assert_eq!(
                actual,
                (0..3)
                    .map(|scalar| (ordinal as u32, scalar))
                    .collect::<Vec<_>>()
            );
            assert_eq!(cached.function_results, reference.function_results);
        }
        assert!(
            cached.imported_fragment_hits > 0,
            "distinct returned scalars reuse a completed parameter-relative fragment"
        );
        assert_eq!(reference.imported_fragment_hits, 0);
    });
}

fn coordinates<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
) -> Vec<(u32, usize)> {
    let mut result = Vec::new();
    for_each_scalar_coordinate_cached(view, root, 0, None, cache, |coordinate, scalar| {
        let dae::CoordinateView::Algebraic(variable) = coordinate else {
            panic!("only model inputs expected")
        };
        result.push((variable.index(), scalar));
    })
    .unwrap();
    result
}
