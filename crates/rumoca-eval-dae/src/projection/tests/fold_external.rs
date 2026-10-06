use super::*;

fn external_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    scalar: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(VarName::new("opaque"), [], [scalar], at),
            |model, reservation| {
                let y = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let body = dae::ExternalFunctionBody::new(
                    dae::FunctionPurity::Pure,
                    dae::ExternalLanguage::C,
                    VarName::new("opaqueC"),
                    [],
                    Some(y),
                    dae::ExternalLinkage::new([], None, None, None),
                );
                model.functions(|functions| functions.define_external(reservation, body, at))
            },
        )
        .map(|(function, ())| function)
}

pub(super) fn external_fold_model() -> dae::Dae {
    let text = "pure function opaque output Real y; external \"C\" y=opaqueC(); end opaque; function f output Real y; algorithm y:=0; for i in 1:2 loop y:=opaque(); end for; end f;";
    let mut sources = SourceMap::new();
    let source = sources.add("external_fold.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        let scalar = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let external = external_function(model, scalar, at)?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("f"), [], [scalar], at),
            |model, reservation| {
                let y = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let zero = model.expressions(|expressions| {
                    expressions.at(at).literal(dae::DaeLiteral::Real(0.0))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, y, zero, at))?;
                let domain = model.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
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
                let mut loop_body =
                    model.functions(|functions| functions.begin_loop(body, domain, [y], at))?;
                let rhs =
                    model.expressions(|expressions| expressions.at(at).call(external, 0, []))?;
                model.functions(|functions| functions.assign_loop(&mut loop_body, y, rhs, at))?;
                let body = model.functions(|functions| functions.finish_loop(loop_body, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, []))?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn external_refusal_inside_a_fold_keeps_its_exact_interface_and_no_partial_cache() {
    external_fold_model().inspect(|view| {
        let root = view.expression_id(view.expression_count() - 1).unwrap();
        for reference in [true, false] {
            let mut cache = ScalarCoordinateProjectionCache { uncached_fold_reference: reference, ..Default::default() };
            let error = for_each_scalar_coordinate_cached(view, root, 0, None, &mut cache, |_, _| {}).unwrap_err();
            assert!(matches!(error, ProjectionError::ExternalFunction { name, language: "C", symbol, .. } if name == "opaque" && symbol == "opaqueC"));
            assert!(cache.function_results.is_empty());
            assert!(cache.completed_folds.is_empty());
        }
    });
}
