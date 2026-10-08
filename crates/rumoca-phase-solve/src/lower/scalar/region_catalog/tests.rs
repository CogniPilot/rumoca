//! A region nested in a region reads the enclosing folds through the outer
//! region's catalog and loads a scalar only when it reads it.
use super::*;

fn model() -> dae::Dae {
    let text = "function f output Real y; algorithm y:=0; for i in 1:2 loop y:=1; end for; end f;";
    let mut sources = rumoca_core::SourceMap::new();
    let source = sources.add("nested_region.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |model| {
        let scalar = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(rumoca_core::VarName::new("f"), [], [scalar], at),
            |model, reservation| {
                let y = model.functions(|functions| {
                    functions.output(&reservation, rumoca_core::VarName::new("y"), 0, at)
                })?;
                let zero = model.expressions(|expressions| {
                    expressions.at(at).literal(dae::DaeLiteral::Real(0.0))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, y, zero, at))?;
                let domain = model.domains(|domains| {
                    domains.structured(
                        rumoca_core::StructuredIndexDomain {
                            binders: vec![rumoca_core::StructuredIndexBinder {
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
                let one = model.expressions(|expressions| {
                    expressions.at(at).literal(dae::DaeLiteral::Real(1.0))
                })?;
                model.functions(|functions| functions.assign_loop(&mut loop_body, y, one, at))?;
                let body = model.functions(|functions| functions.finish_loop(loop_body, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, []))?;
        Ok(())
    })
    .unwrap()
}

/// The outer region's catalog: the one carried scalar of the fold, held in
/// register 7 of the compiler that owns the outer region.
fn outer_compiler<'layout, 'dae>(
    view: dae::DaeView<'dae>,
    layout: &'layout LoweredLayout<'dae>,
    fold: dae::FunctionFoldId<'dae>,
) -> ScalarCompiler<'layout, 'dae> {
    let mut outer = ScalarCompiler::new(view, layout, None);
    outer.next_register = 8;
    outer.deferred_function_conditional_captures = Some(DeferredFunctionConditionalCaptures {
        owner_function: fold.function(),
        owner_context: 0,
        sources: Vec::new(),
        locals: HashMap::new(),
        slots: HashMap::new(),
        width: 0,
        visible: RegionVisiblePoints {
            symbolic: Vec::new(),
            folds: vec![(
                fold,
                vec![vec![FunctionConditionalCaptureSource::ParentRegister {
                    source: 7,
                }]],
            )],
        },
    });
    outer
}

#[test]
fn an_inner_region_loads_an_outer_catalog_scalar_only_when_it_reads_it() {
    let model = model();
    model.inspect(|view| {
        let layout = crate::layout::lower_layout(view).unwrap();
        let function = view.function_id(0).unwrap();
        let fold = view.function(function).unwrap().fold_id(0).unwrap();
        let span = view.function_fold(fold).unwrap().provenance().span();

        // An inner region that reads nothing leaves the outer catalog unloaded.
        let outer = outer_compiler(view, &layout, fold);
        let idle = outer.fork_for_function_conditional_region(fold.function(), 0, &[]);
        let idle_catalog = &idle
            .deferred_function_conditional_captures
            .as_ref()
            .unwrap()
            .visible;
        assert!(matches!(
            idle_catalog.folds.as_slice(),
            [(candidate, tuple)] if *candidate == fold
                && matches!(
                    tuple.as_slice(),
                    [inner] if matches!(inner.as_slice(), [FunctionConditionalCaptureSource::ParentCatalog { ordinal: 0 }])
                )
        ));
        assert!(idle.ops.is_empty());
        assert!(outer.ops.is_empty());
        assert!(
            outer
                .deferred_function_conditional_captures
                .as_ref()
                .unwrap()
                .sources
                .is_empty()
        );

        // An inner region that reads the carried scalar captures the catalog
        // ordinal; the outer region resolves it to its own source, loading
        // that one scalar.
        let mut outer = outer_compiler(view, &layout, fold);
        let mut inner = outer.fork_for_function_conditional_region(fold.function(), 0, &[]);
        inner.function_fold_parameter(fold, 0, 0, span).unwrap();
        let captures = inner.deferred_function_conditional_captures.as_ref().unwrap();
        assert!(matches!(
            captures.sources.as_slice(),
            [FunctionConditionalCaptureSource::ParentCatalog { ordinal: 0 }]
        ));
        let range = outer
            .resolve_function_conditional_capture(captures.sources[0], span)
            .unwrap();
        assert_eq!(range.count, 1);
        let outer_captures = outer.deferred_function_conditional_captures.as_ref().unwrap();
        assert!(matches!(
            outer_captures.sources.as_slice(),
            [FunctionConditionalCaptureSource::ParentRegister { source: 7 }]
        ));
        assert_eq!(outer.ops.len(), 1);
    });
}
