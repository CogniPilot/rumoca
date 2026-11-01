//! A refused program of a multi-program block is reported with its own span.
use super::*;
use std::num::NonZeroU64;

fn span(start: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("refusal_provenance.mo"),
        start,
        start + 10,
    )
}

#[test]
fn the_refused_entry_of_distinct_programs_keeps_its_own_span_through_construction() {
    let mut compiler = rumoca_compile::Session::new(Default::default());
    compiler.update_document(
        "refusal_provenance.mo",
        "model Provenance Real x(start=1, fixed=true); Real y(start=2, fixed=true); equation der(x)=-x; der(y)=-y; end Provenance;",
    );
    let compiled = compiler.compile_model("Provenance").unwrap();
    let lowered = crate::lower_correlated_for_simulation_with_overrides(
        &compiled.dae,
        &rumoca_solver::SimOptions::default(),
    )
    .unwrap();
    let mut model = lowered.model().clone();
    let (first, refused) = (span(10), span(30));
    model.problem.continuous.derivative_rhs =
        rumoca_ir_solve::ComputeBlock::from_scalar_program_block(
            ScalarProgramBlock::with_program_spans(
                vec![
                    vec![
                        LinearOp::LoadY { dst: 0, index: 0 },
                        LinearOp::StoreOutput { src: 0 },
                    ],
                    // A primal program may not read a seed: refused at request time.
                    vec![
                        LinearOp::LoadSeed { dst: 0, index: 0 },
                        LinearOp::StoreOutput { src: 0 },
                    ],
                ],
                vec![first, refused],
            )
            .unwrap(),
        );
    let backend = Rc::new(WasmExecutionBackend::new(
        model.problem.layout.clone(),
        Rc::new(model.pure_calls.clone()),
    ));
    let error = rumoca_solver::SolveRuntime::new_with_execution_backend(&model, Some(backend))
        .err()
        .expect("the primal program's seed load is refused during construction");
    match error {
        rumoca_eval_solve::EvalSolveError::CompiledExecution { message, span } => {
            assert!(message.contains("seed"), "unexpected refusal: {message}");
            assert_eq!(span, Some(refused));
        }
        other => panic!("unexpected construction error: {other:?}"),
    }
}

/// A call owner refused only at linkage: a two-member recursive group.
fn recursive_call() -> (
    rumoca_ir_solve::SolvePureCallTable,
    rumoca_ir_solve::SolvePureCallSite,
) {
    use rumoca_ir_solve as solve;
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
    let mut builder = solve::SolvePureCallTable::builder(profile);
    let members = (0..2)
        .map(|i| {
            solve::SolveRecursiveMember::new(
                solve::SolvePureCallIdentity::issued(NonZeroU64::new(10 + i).unwrap()),
                vec![real.clone()],
                vec![solve::SolvePureCallOutput::result(real.clone())],
                span(100 + i as usize),
            )
        })
        .collect();
    let group = builder
        .add_recursive_group(members, |ordinal, ids, body, inputs, outputs| {
            let value = body.load(inputs[0], span(200))?;
            let result = body.call(ids[(ordinal + 1) % ids.len()], &[value], span(210))?;
            body.store(outputs[0], result[0], span(220))
        })
        .unwrap();
    let site = builder.call_site(group[0]).unwrap();
    (builder.finish(), site)
}

#[test]
fn a_cached_linkage_refusal_carries_the_span_of_the_request_that_meets_it() {
    let (table, site) = recursive_call();
    let layout = VarLayout::from_parts(Default::default(), 1, 0);
    let batch = super::super::super::batch::ModuleBatch::new(layout.clone(), Rc::new(table));
    let calling = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::PureCall {
            dst_start: 1,
            input_starts: vec![0].into(),
            site,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let ordinary = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    for (calling_span, ordinary_span) in [(span(40), span(50)), (span(60), span(70))] {
        let block = ScalarProgramBlock::with_program_spans(
            vec![ordinary.clone(), calling.clone()],
            vec![ordinary_span, calling_span],
        )
        .unwrap();
        let Err(refusal) = WasmExpression::compile_deferred(&block, &layout, &batch, None) else {
            panic!("a recursive call owner is refused at request time");
        };
        assert_eq!(refusal.span(), Some(calling_span));
    }
}

#[test]
fn a_directional_preparation_refusal_keeps_its_program_span() {
    let (first, refused) = (span(300), span(330));
    let block = ScalarProgramBlock::with_program_spans(
        vec![
            vec![
                LinearOp::LoadSeed { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadSeed {
                    dst: 0,
                    index: usize::MAX,
                },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        vec![first, refused],
    )
    .unwrap();
    let preparation_error = rumoca_eval_solve::PreparedScalarProgramBlock::new(block.clone())
        .err()
        .expect("the required seed extent overflows");
    assert_eq!(preparation_error.source_span(), Some(refused));
    let backend = WasmExecutionBackend::new(
        VarLayout::from_parts(Default::default(), 1, 0),
        Rc::new(rumoca_ir_solve::SolvePureCallTable::default()),
    );
    let refusal =
        rumoca_solver::SolveExecutionBackend::compile_jacobian_expression(&backend, &block)
            .err()
            .expect("overflow is refused during construction");
    assert_eq!(
        refusal.span(),
        Some(refused),
        "structured preparation error was flattened: {refusal}"
    );
}
