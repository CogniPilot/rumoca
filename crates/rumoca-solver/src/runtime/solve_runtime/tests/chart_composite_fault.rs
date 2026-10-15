//! A failed alternate transaction preserves caller outputs after primary success.
use super::*;
use std::cell::Cell;

struct PartialFault {
    source: solve::ScalarProgramBlock,
    calls: Rc<Cell<usize>>,
}

impl CompiledSolveExpression for PartialFault {
    fn call(
        &self,
        _: &[f64],
        _: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        for (ordinal, ops) in self.source.programs().iter().enumerate() {
            let Some(solve::LinearOp::Const { value, .. }) = ops.first() else {
                panic!("constant fixture")
            };
            out[self.source.output_indices()[ordinal]] = *value;
            if *value == 99.0 {
                return Err(RuntimeSolveError::solve_ir("injected replacement fault"));
            }
        }
        Ok(())
    }
}

struct Backend {
    calls: Rc<Cell<usize>>,
}
impl SolveExecutionBackend for Backend {
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        Ok(Rc::new(PartialFault {
            source: source.clone(),
            calls: self.calls.clone(),
        }))
    }
}

#[test]
fn chart_composite_replacement_fault_must_not_publish_primary_success() {
    let source = |second| {
        solve::ScalarProgramBlock::with_output_indices(
            [10.0, second]
                .map(|value| {
                    vec![
                        solve::LinearOp::Const { dst: 0, value },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ]
                })
                .into(),
            vec![test_span("chart_composite_fault.mo"); 2],
            vec![0, 3],
        )
        .unwrap()
    };
    let primary_source = source(20.0);
    let alternate_source = source(99.0);
    let calls = Rc::new(Cell::new(0));
    let backend = Backend {
        calls: calls.clone(),
    };
    let primary =
        super::super::selected_programs::expression(&backend, "primary", &primary_source).unwrap();
    let composite = BlockReuse::of_programs(Some(&primary_source), &alternate_source)
        .expression(Some(&primary), &alternate_source, &mut |source| {
            super::super::selected_programs::expression(&backend, "replacement", source)
        })
        .unwrap();
    let mut output = [81.0, 82.0, 83.0, 84.0];
    assert!(composite.call(&[], &[], 0.0, &[], &mut output).is_err());
    assert_eq!(calls.get(), 2, "primary succeeds before replacement faults");
    assert_eq!(
        output,
        [81.0, 82.0, 83.0, 84.0],
        "the failed alternate transaction must not publish the primary output"
    );
}
