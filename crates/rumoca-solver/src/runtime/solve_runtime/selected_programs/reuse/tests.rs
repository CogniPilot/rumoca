//! Regression controls for complete chart authority and atomic publication.
use super::*;
use std::cell::Cell;

struct Probe {
    source: solve::ScalarProgramBlock,
    fail_replacement: Rc<Cell<bool>>,
    calls: Rc<Cell<usize>>,
}

impl Probe {
    fn run(&self, out: &mut [f64]) -> Result<(), RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        out.fill(0.0);
        let mut ordinal = 0;
        for ops in self.source.programs() {
            let value = match ops.first() {
                Some(solve::LinearOp::Const { value, .. }) => *value,
                _ => panic!("fixture requires an actual constant source"),
            };
            let mut registers = [0.0; 4];
            for op in ops {
                self.write_operation(op, &mut registers, &mut ordinal, out);
            }
            // Deliberately fail after touching private output storage.
            if value == 99.0 && self.fail_replacement.get() {
                return Err(RuntimeSolveError::solve_ir("injected replacement fault"));
            }
        }
        Ok(())
    }

    fn write_operation(
        &self,
        op: &solve::LinearOp,
        registers: &mut [f64],
        ordinal: &mut usize,
        out: &mut [f64],
    ) {
        match op {
            solve::LinearOp::Const { dst, value } => registers[*dst as usize] = *value,
            solve::LinearOp::StoreOutput { src } => {
                out[self.source.output_index_at(*ordinal).unwrap()] = registers[*src as usize];
                *ordinal += 1;
            }
            _ => panic!("fixture supports only constants and actual stores"),
        }
    }

    fn bound(&self, source: &solve::ScalarProgramBlock, index: usize) -> Rc<Probe> {
        assert!(self.source.shares_program_owner(source));
        let source = solve::ScalarProgramBlock::with_program_spans(
            vec![source.program(index).unwrap().to_vec()],
            vec![source.program_span(index).unwrap()],
        )
        .unwrap();
        Rc::new(Self {
            source,
            fail_replacement: self.fail_replacement.clone(),
            calls: self.calls.clone(),
        })
    }
}

impl CompiledSolveExpression for Probe {
    fn bind_program(
        &self,
        source: &solve::ScalarProgramBlock,
        index: usize,
    ) -> Result<Option<Rc<dyn CompiledSolveExpression>>, String> {
        Ok(Some(self.bound(source, index)))
    }

    fn call(
        &self,
        _: &[f64],
        _: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.run(out)
    }
}

impl CompiledSolveJacobianExpression for Probe {
    fn bind_program(
        &self,
        source: &solve::ScalarProgramBlock,
        index: usize,
    ) -> Result<Option<Rc<dyn CompiledSolveJacobianExpression>>, String> {
        Ok(Some(self.bound(source, index)))
    }

    fn call(
        &self,
        _: &[f64],
        _: &[f64],
        _: f64,
        _: &[f64],
        _: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.run(out)
    }
}

struct Backend {
    fail_replacement: Rc<Cell<bool>>,
    calls: Rc<Cell<usize>>,
    compiled: Cell<usize>,
}

impl Backend {
    fn code(&self, source: &solve::ScalarProgramBlock) -> Rc<Probe> {
        self.compiled.set(self.compiled.get() + 1);
        Rc::new(Probe {
            source: source.clone(),
            fail_replacement: self.fail_replacement.clone(),
            calls: self.calls.clone(),
        })
    }
}

impl SolveExecutionBackend for Backend {
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        Ok(self.code(source))
    }
    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        Ok(self.code(source))
    }
}

fn sparse_source(second: f64) -> solve::ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("complete_reuse.mo"),
        0,
        1,
    );
    let rows = [10.0, second].map(|value| {
        vec![
            solve::LinearOp::Const { dst: 0, value },
            solve::LinearOp::StoreOutput { src: 0 },
        ]
    });
    solve::ScalarProgramBlock::with_output_indices(rows.into(), vec![span; 2], vec![0, 3]).unwrap()
}

#[test]
fn replacement_fault_after_primary_success_keeps_whole_and_bulk_outputs_atomic() {
    let backend = Backend {
        fail_replacement: Rc::new(Cell::new(true)),
        calls: Rc::new(Cell::new(0)),
        compiled: Cell::new(0),
    };
    let source = sparse_source(20.0);
    let alternate = sparse_source(99.0);
    let primary = expression(&backend, "primary", &source).unwrap();
    let reuse = BlockReuse::of_programs(Some(&source), &alternate);
    let composite = reuse
        .expression(Some(&primary), &alternate, &mut |source| {
            expression(&backend, "replacement", source)
        })
        .unwrap();
    assert_eq!(
        backend.compiled.get(),
        2,
        "only the replacement source is compiled again"
    );

    let mut whole = [81.0, 82.0, 83.0, 84.0];
    let error = composite.call(&[], &[], 0.0, &[], &mut whole).unwrap_err();
    assert!(error.to_string().contains("injected replacement fault"));
    assert_eq!(
        backend.calls.get(),
        2,
        "primary succeeds before replacement faults"
    );
    assert_eq!(whole, [81.0, 82.0, 83.0, 84.0]);

    let mut bulk = [91.0, 92.0];
    assert!(
        composite
            .call_program_outputs_at(&[(0, 0), (1, 0)], (&[], &[], 0.0), &[], &mut bulk)
            .is_err()
    );
    assert_eq!(bulk, [91.0, 92.0]);
    backend.fail_replacement.set(false);
    composite.call(&[], &[], 0.0, &[], &mut whole).unwrap();
    assert_eq!(whole, [10.0, 0.0, 0.0, 99.0]);
    composite
        .call_program_outputs_at(&[(1, 0), (0, 0)], (&[], &[], 0.0), &[], &mut bulk)
        .unwrap();
    assert_eq!(bulk, [99.0, 10.0]);
}

#[test]
fn replacement_directional_fault_keeps_whole_outputs_atomic_and_recovers() {
    let backend = Backend {
        fail_replacement: Rc::new(Cell::new(true)),
        calls: Rc::new(Cell::new(0)),
        compiled: Cell::new(0),
    };
    let source = sparse_source(20.0);
    let alternate = sparse_source(99.0);
    let primary = jacobian(&backend, "primary", &source).unwrap();
    let composite = BlockReuse::of_programs(Some(&source), &alternate)
        .jacobian(Some(&primary), &alternate, &mut |source| {
            jacobian(&backend, "replacement", source)
        })
        .unwrap();
    let mut whole = [81.0, 82.0, 83.0, 84.0];
    assert!(composite.call(&[], &[], 0.0, &[], &[], &mut whole).is_err());
    assert_eq!(backend.calls.get(), 2);
    assert_eq!(whole, [81.0, 82.0, 83.0, 84.0]);
    assert_eq!(backend.compiled.get(), 2);
    backend.fail_replacement.set(false);
    composite.call(&[], &[], 0.0, &[], &[], &mut whole).unwrap();
    assert_eq!(whole, [10.0, 0.0, 0.0, 99.0]);
}

#[test]
fn reuse_rechecks_spans_sparse_maps_and_exact_replaced_programs() {
    let source = sparse_source(20.0);
    let changed_span = solve::ScalarProgramBlock::with_output_indices(
        source.programs().to_vec(),
        vec![
            rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name("complete_reuse.mo"),
                0,
                2,
            );
            2
        ],
        source.output_indices().to_vec(),
    )
    .unwrap();
    let changed_map = solve::ScalarProgramBlock::with_output_indices(
        source.programs().to_vec(),
        source.program_spans().to_vec(),
        vec![3, 0],
    )
    .unwrap();
    assert!(matches!(
        BlockReuse::of_programs(Some(&source), &source.clone()),
        BlockReuse::Same
    ));
    assert!(matches!(
        BlockReuse::of_programs(Some(&source), &changed_span),
        BlockReuse::Fresh
    ));
    assert!(matches!(
        BlockReuse::of_programs(Some(&source), &changed_map),
        BlockReuse::Fresh
    ));
    let backend = Backend {
        fail_replacement: Rc::new(Cell::new(false)),
        calls: Rc::new(Cell::new(0)),
        compiled: Cell::new(0),
    };
    let primary = expression(&backend, "primary", &source).unwrap();
    assert!(
        BlockReuse::Same
            .expression(Some(&primary), &changed_span, &mut |_| panic!(
                "foreign reuse must fail before compilation"
            ))
            .is_err()
    );
    assert!(
        BlockReuse::Replaced(vec![0])
            .expression(Some(&primary), &sparse_source(99.0), &mut |_| panic!(
                "wrong delta must fail before compilation"
            ))
            .is_err()
    );
    assert!(
        BlockReuse::Replaced(vec![1])
            .expression(Some(&primary), &sparse_source(99.0), &mut |_| Ok(
                primary.clone()
            ))
            .is_err(),
        "a replacement compiler cannot return a complete product for another owner"
    );
    assert_eq!(backend.compiled.get(), 1);
    assert_eq!(backend.calls.get(), 0);
}

#[test]
fn foreign_external_table_context_is_fresh_even_for_equal_scalar_ops() {
    let make = || {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("complete_table_reuse.mo"),
            0,
            1,
        );
        solve::ScalarProgramBlock::with_program_spans(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::Const { dst: 1, value: 1.0 },
                solve::LinearOp::Const { dst: 2, value: 0.5 },
                solve::LinearOp::TableLookup {
                    dst: 3,
                    table_id: 0,
                    column: 1,
                    input: 2,
                },
                solve::LinearOp::StoreOutput { src: 3 },
            ]],
            vec![span],
        )
        .unwrap()
    };
    let source = make();
    let independent = make();
    assert_eq!(source.programs(), independent.programs());
    assert!(!source.shares_program_owner(&independent));
    assert!(matches!(
        BlockReuse::of_programs(Some(&source), &independent),
        BlockReuse::Fresh
    ));
    assert!(matches!(
        BlockReuse::of_programs(Some(&source), &source.clone()),
        BlockReuse::Same
    ));
}

#[test]
fn replacement_preserves_noncontiguous_multioutput_program_offsets() {
    let make = |second| {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("complete_multioutput_reuse.mo"),
            0,
            1,
        );
        let row = |value| {
            vec![
                solve::LinearOp::Const { dst: 0, value },
                solve::LinearOp::StoreOutput { src: 0 },
                solve::LinearOp::Const {
                    dst: 1,
                    value: value + 1.0,
                },
                solve::LinearOp::StoreOutput { src: 1 },
            ]
        };
        solve::ScalarProgramBlock::with_output_indices(
            vec![row(10.0), row(second)],
            vec![span; 2],
            vec![4, 1, 3, 0],
        )
        .unwrap()
    };
    let backend = Backend {
        fail_replacement: Rc::new(Cell::new(false)),
        calls: Rc::new(Cell::new(0)),
        compiled: Cell::new(0),
    };
    let source = make(20.0);
    let alternate = make(99.0);
    let primary = expression(&backend, "primary", &source).unwrap();
    let composite = BlockReuse::of_programs(Some(&source), &alternate)
        .expression(Some(&primary), &alternate, &mut |source| {
            expression(&backend, "replacement", source)
        })
        .unwrap();
    let mut whole = [f64::NAN; 5];
    composite.call(&[], &[], 0.0, &[], &mut whole).unwrap();
    assert_eq!(whole, [100.0, 11.0, 0.0, 99.0, 10.0]);
    let mut selected = [0.0; 4];
    composite
        .call_program_outputs_at(
            &[(1, 1), (0, 0), (1, 0), (0, 1)],
            (&[], &[], 0.0),
            &[],
            &mut selected,
        )
        .unwrap();
    assert_eq!(selected, [100.0, 10.0, 99.0, 11.0]);
    assert_eq!(
        composite
            .call_program_output((1, 1), &[], &[], 0.0, &[])
            .unwrap(),
        100.0
    );
    assert_eq!(backend.compiled.get(), 2);
}

#[test]
fn same_primitive_source_retains_current_owner_and_only_exact_owner_shares_rc_identity() {
    let backend = Backend {
        fail_replacement: Rc::new(Cell::new(false)),
        calls: Rc::new(Cell::new(0)),
        compiled: Cell::new(0),
    };
    let original = sparse_source(20.0);
    let independent = sparse_source(20.0);
    let primary = expression(&backend, "primary", &original).unwrap();
    let exact = BlockReuse::Same
        .expression(Some(&primary), &original.clone(), &mut |_| {
            panic!("exact source must share native preparation")
        })
        .unwrap();
    assert!(Rc::ptr_eq(&exact, &primary));
    let equivalent = BlockReuse::of_programs(Some(&original), &independent)
        .expression(Some(&primary), &independent, &mut |_| {
            panic!("proved equivalent source needs no new kernel")
        })
        .unwrap();
    assert!(!Rc::ptr_eq(&equivalent, &primary));
    assert!(equivalent.shares_source(&independent));
    assert!(!equivalent.shares_source(&original));
    let mut output = [0.0; 4];
    equivalent.call(&[], &[], 0.0, &[], &mut output).unwrap();
    assert_eq!(output, [10.0, 0.0, 0.0, 20.0]);
    assert_eq!(backend.compiled.get(), 1);
    assert_eq!(
        backend.calls.get(),
        1,
        "same source alias executes original native instance once"
    );

    let primary = jacobian(&backend, "primary_jvp", &original).unwrap();
    let equivalent = BlockReuse::of_programs(Some(&original), &independent)
        .jacobian(Some(&primary), &independent, &mut |_| {
            panic!("equivalent directional source needs no new kernel")
        })
        .unwrap();
    assert!(equivalent.shares_source(&independent));
    equivalent
        .call(&[], &[], 0.0, &[], &[], &mut output)
        .unwrap();
    assert_eq!(output, [10.0, 0.0, 0.0, 20.0]);
    assert_eq!(backend.compiled.get(), 2);
    assert_eq!(backend.calls.get(), 2);
}

#[test]
fn shared_layout_replacement_preserves_changed_constant_zero_bits() {
    let backend = Backend {
        fail_replacement: Rc::new(Cell::new(false)),
        calls: Rc::new(Cell::new(0)),
        compiled: Cell::new(0),
    };
    let original = sparse_source(-0.0);
    let alternate = sparse_source(0.0);
    let reuse = BlockReuse::of_programs(Some(&original), &alternate);
    match &reuse {
        BlockReuse::Replaced(rows) => assert_eq!(rows.as_slice(), &[1]),
        _ => panic!("changed constant bits require a replacement"),
    }
    let primary = expression(&backend, "primary", &original).unwrap();
    let replacement = reuse
        .expression(Some(&primary), &alternate, &mut |source| {
            expression(&backend, "replacement", source)
        })
        .unwrap();
    let mut output = [f64::NAN; 4];
    replacement.call(&[], &[], 0.0, &[], &mut output).unwrap();
    assert_eq!(output[3].to_bits(), 0.0f64.to_bits());
    assert_eq!(
        replacement
            .call_program_output((1, 0), &[], &[], 0.0, &[])
            .unwrap()
            .to_bits(),
        0.0f64.to_bits()
    );
    assert_eq!(backend.compiled.get(), 2);
}
