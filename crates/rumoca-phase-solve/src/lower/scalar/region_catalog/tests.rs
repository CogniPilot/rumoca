//! A region nested in a region reads the enclosing folds through the outer
//! region's catalog and loads a scalar only when it reads it.
use super::super::functions::FunctionConditionalRegisterRange;
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

#[test]
fn fold_tensor_capture_aliases_share_the_parent_range_but_overlaps_do_not() {
    let model = model();
    model.inspect(|view| {
        let layout = crate::layout::lower_layout(view).unwrap();
        let function = view.function_id(0).unwrap();
        let fold = view.function(function).unwrap().fold_id(0).unwrap();
        let owner = view.function_fold(fold).unwrap();
        let initial = owner.initial_values().rhs_iter().next().unwrap();
        let update = owner.update_values().rhs_iter().next().unwrap();
        assert_ne!(initial, update);
        let span = owner.provenance().span();
        let mut compiler = ScalarCompiler::new(view, &layout, None);
        compiler.deferred_fold_captures = Some(DeferredFoldCaptures {
            fold_values: Vec::new(),
            symbolic_domain_points: Vec::new(),
            packed_expressions: HashMap::from([(initial, (22, 16)), (update, (22, 16))]),
            packed_capture_ranges: HashMap::new(),
            sources: Vec::new(),
            locals: HashMap::new(),
        });
        assert_eq!(
            compiler.fold_capture_tensor_base(initial, span).unwrap(),
            Some(0)
        );
        assert_eq!(
            compiler.fold_capture_tensor_base(update, span).unwrap(),
            Some(0)
        );
        assert_eq!(
            compiler.deferred_fold_captures.as_ref().unwrap().sources,
            (22..38).collect::<Vec<_>>()
        );
        compiler
            .deferred_fold_captures
            .as_mut()
            .unwrap()
            .packed_expressions
            .insert(update, (23, 16));
        assert_eq!(
            compiler.fold_capture_tensor_base(update, span).unwrap(),
            Some(16)
        );
        compiler
            .deferred_fold_captures
            .as_mut()
            .unwrap()
            .packed_expressions
            .insert(update, (22, 8));
        assert_eq!(
            compiler.fold_capture_tensor_base(update, span).unwrap(),
            Some(32)
        );
        let captures = compiler.deferred_fold_captures.as_ref().unwrap();
        assert_eq!(
            captures.sources,
            (22..38).chain(23..39).chain(22..30).collect::<Vec<_>>()
        );
        assert_eq!(captures.packed_capture_ranges.len(), 3);
        compiler
            .deferred_fold_captures
            .as_mut()
            .unwrap()
            .packed_expressions
            .insert(update, (u32::MAX, 2));
        assert!(compiler.fold_capture_tensor_base(update, span).is_err());
        let captures = compiler.deferred_fold_captures.as_ref().unwrap();
        assert_eq!(captures.sources.len(), 40);
        assert_eq!(captures.packed_capture_ranges.len(), 3);
        assert!(compiler.ops.is_empty());
    });
}

#[test]
fn register_packing_preserves_order_aliases_and_overlaps_in_primal_and_ad() {
    model().inspect(|view| {
        let layout = crate::layout::lower_layout(view).unwrap();
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let span = view
            .function_fold(function.fold_id(0).unwrap())
            .unwrap()
            .provenance()
            .span();
        let mut compiler = ScalarCompiler::new(view, &layout, None);
        for index in 0..32 {
            let dst = compiler.register(span).unwrap();
            compiler
                .emit(solve::LinearOp::LoadY { dst, index })
                .unwrap();
        }
        let sources = (0..8)
            .chain(12..20)
            .chain(4..12)
            .chain(4..12)
            .chain([31, 0])
            .collect::<Vec<_>>();
        let start = compiler.pack_fold_registers(&sources, span).unwrap();
        assert_eq!(start, 32);
        assert_eq!(compiler.ops.len(), 33);
        let solve::LinearOp::TensorConcatenate {
            sources: runs,
            dimensions,
            ..
        } = compiler.ops.last().unwrap()
        else {
            panic!("expected one compact pack")
        };
        assert_eq!(dimensions.as_ref(), [34]);
        assert_eq!(
            runs.iter()
                .map(|run| (run.start, run.dimensions[0]))
                .collect::<Vec<_>>(),
            [(0, 8), (12, 8), (4, 8), (4, 8), (31, 1), (0, 1)]
        );
        assert_eq!(compiler.pack_fold_registers(&sources, span).unwrap(), start);
        assert_eq!(compiler.ops.len(), 33);
        compiler
            .emit(solve::LinearOp::StoreOutputRange {
                start,
                count: sources.len(),
                stride: 1,
            })
            .unwrap();
        let rows = vec![compiler.finish_operations().unwrap()];
        let provenance = span.require_provenance("register packing control").unwrap();
        let block = solve::ScalarProgramBlock::with_source_span(rows.clone(), provenance).unwrap();
        let input = (0..32)
            .map(|index| index as f64 * 1.25 - 7.0)
            .collect::<Vec<_>>();
        let seed = (0..32)
            .map(|index| index as f64 * -0.5 + 3.0)
            .collect::<Vec<_>>();
        let mut output = vec![0.0; sources.len()];
        rumoca_eval_solve::eval_scalar_program_block(&block, &input, &[], 0.0, None, &mut output)
            .unwrap();
        assert_eq!(
            output,
            sources
                .iter()
                .map(|&source| input[source as usize])
                .collect::<Vec<_>>()
        );
        let derived = crate::lower_scalar_program_block_ad(&rows).unwrap();
        assert_eq!(
            derived[0]
                .iter()
                .filter(|op| matches!(op, solve::LinearOp::TensorConcatenate { lanes: 2, .. }))
                .count(),
            1
        );
        let block = solve::ScalarProgramBlock::with_source_span(derived, provenance).unwrap();
        rumoca_eval_solve::eval_scalar_program_block(
            &block,
            &input,
            &[],
            0.0,
            Some(&seed),
            &mut output,
        )
        .unwrap();
        assert_eq!(
            output,
            sources
                .iter()
                .map(|&source| seed[source as usize])
                .collect::<Vec<_>>()
        );
    });
}

#[test]
fn register_packing_checks_sources_before_mutation_and_reuses_contiguous_inputs() {
    model().inspect(|view| {
        let layout = crate::layout::lower_layout(view).unwrap();
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let span = view
            .function_fold(function.fold_id(0).unwrap())
            .unwrap()
            .provenance()
            .span();
        let mut compiler = ScalarCompiler::new(view, &layout, None);
        for value in 0..4 {
            compiler.constant(f64::from(value), span).unwrap();
        }
        assert_eq!(compiler.pack_fold_registers(&[], span).unwrap(), 4);
        assert_eq!(compiler.pack_fold_registers(&[1, 2, 3], span).unwrap(), 1);
        assert_eq!(compiler.ops.len(), 4);
        let cache_len = compiler.fold_register_pack_cache.len();
        assert!(compiler.pack_fold_registers(&[0, 4], span).is_err());
        assert_eq!(compiler.fold_register_pack_cache.len(), cache_len);
        for ranges in [
            vec![FunctionConditionalRegisterRange {
                start: u32::MAX,
                count: 2,
            }],
            vec![FunctionConditionalRegisterRange {
                start: 0,
                count: usize::MAX,
            }],
            vec![
                FunctionConditionalRegisterRange {
                    start: 0,
                    count: usize::MAX,
                },
                FunctionConditionalRegisterRange { start: 0, count: 1 },
            ],
            vec![FunctionConditionalRegisterRange { start: 3, count: 2 }],
        ] {
            assert!(compiler.pack_register_ranges(&ranges, span).is_err());
            assert_eq!(compiler.next_register, 4);
            assert_eq!(compiler.ops.len(), 4);
        }
        assert_eq!(
            compiler
                .pack_register_ranges(
                    &[FunctionConditionalRegisterRange {
                        start: u32::MAX,
                        count: 0
                    },],
                    span
                )
                .unwrap(),
            4
        );
        assert_eq!(compiler.next_register, 4);
        assert_eq!(compiler.ops.len(), 4);
        let start = compiler
            .pack_register_ranges(
                &[
                    FunctionConditionalRegisterRange {
                        start: u32::MAX,
                        count: 0,
                    },
                    FunctionConditionalRegisterRange { start: 1, count: 2 },
                ],
                span,
            )
            .unwrap();
        compiler
            .emit(solve::LinearOp::StoreOutputRange {
                start,
                count: 2,
                stride: 1,
            })
            .unwrap();
        let rows = vec![compiler.finish_operations().unwrap()];
        let block = solve::ScalarProgramBlock::with_source_span(
            rows,
            span.require_provenance("empty register pack segment control")
                .unwrap(),
        )
        .unwrap();
        let mut output = [0.0; 2];
        rumoca_eval_solve::eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut output)
            .unwrap();
        assert_eq!(output, [1.0, 2.0]);
    });
}
