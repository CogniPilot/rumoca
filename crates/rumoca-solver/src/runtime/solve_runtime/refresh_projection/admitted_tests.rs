use super::super::tests::native_projection_jvp::selected_projection;
use super::super::tests::warm_start_test_model;
use super::*;
use std::cell::Cell;

struct Schedule {
    calls: Rc<Cell<usize>>,
    fail: bool,
    value: f64,
}
impl CompiledSolveAssignmentSchedule for Schedule {
    fn call(
        &self,
        y: &mut [f64],
        _: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        y[1] = self.value;
        if self.fail {
            Err("first torn schedule fault".into())
        } else {
            Ok(())
        }
    }
}
struct Residual {
    calls: Rc<Cell<usize>>,
    fail: bool,
}
impl CompiledSolveExpression for Residual {
    fn call(
        &self,
        _: &[f64],
        _: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        out[0] = 777.0;
        if self.fail {
            Err("first torn residual fault".into())
        } else {
            Ok(())
        }
    }
}

#[test]
fn admitted_torn_schedule_and_residual_faults_restore_only_issued_targets() {
    for schedule_failure in [true, false] {
        let runtime = SolveRuntime::new_fixture(&warm_start_test_model()).unwrap();
        let schedule_calls = Rc::new(Cell::new(0));
        let residual_calls = Rc::new(Cell::new(0));
        let span = runtime.implicit_scalar_rhs.block().program_span(1).unwrap();
        let compiled = CompiledTornSweep {
            schedule: Rc::new(Schedule {
                calls: schedule_calls.clone(),
                fail: schedule_failure,
                value: 321.0,
            }),
            residual_block: Rc::new(Residual {
                calls: residual_calls.clone(),
                fail: true,
            }),
            residual_outputs: vec![Some(0)].into_boxed_slice(),
            residual_len: 1,
            source_spans: vec![span].into_boxed_slice(),
            affected_y: vec![1].into_boxed_slice(),
            rollback: RefCell::new(Vec::new()),
        };
        let tearing = solve::BlockTearing {
            causal_steps: vec![solve::CausalStep {
                row: 1,
                y_index: 1,
                coefficient: solve::CausalCoefficient::Unit,
            }],
            ..Default::default()
        };
        let mut y = [f64::from_bits(0x7ff8_0000_0000_0999), -0.0_f64];
        let before = y.map(f64::to_bits);
        let mut raw = vec![Some(f64::from_bits(0x7ff8_0000_0000_0888))];
        let raw_before = raw[0].unwrap().to_bits();
        let error = selected_projection(&runtime, false)
            .eval_compiled_torn_sweep(&compiled, &tearing, &mut y, &[], 0.0, &mut raw)
            .unwrap_err();
        assert!(matches!(
            &error,
            RuntimeSolveError::CompiledExecution { .. }
        ));
        assert_eq!(error.source_span(), Some(span));
        assert_eq!(schedule_calls.get(), 1);
        assert_eq!(residual_calls.get(), usize::from(!schedule_failure));
        assert_eq!(y.map(f64::to_bits), before);
        assert_eq!(raw[0].unwrap().to_bits(), raw_before);
    }
}

#[test]
fn admitted_torn_singular_target_declines_without_residual_execution() {
    let runtime = SolveRuntime::new_fixture(&warm_start_test_model()).unwrap();
    let calls = Rc::new(Cell::new(0));
    let compiled = CompiledTornSweep {
        schedule: Rc::new(Schedule {
            calls: Rc::new(Cell::new(0)),
            fail: false,
            value: f64::NAN,
        }),
        residual_block: Rc::new(Residual {
            calls: calls.clone(),
            fail: false,
        }),
        residual_outputs: vec![Some(0)].into_boxed_slice(),
        residual_len: 1,
        source_spans: Box::new([]),
        affected_y: vec![1].into_boxed_slice(),
        rollback: RefCell::new(Vec::new()),
    };
    let tearing = solve::BlockTearing {
        causal_steps: vec![solve::CausalStep {
            row: 1,
            y_index: 1,
            coefficient: solve::CausalCoefficient::Unit,
        }],
        ..Default::default()
    };
    let status = selected_projection(&runtime, false)
        .eval_compiled_torn_sweep(
            &compiled,
            &tearing,
            &mut [1.0, 2.0],
            &[],
            0.0,
            &mut Vec::new(),
        )
        .unwrap();
    assert_eq!(status, TornSweepStatus::Declined);
    assert_eq!(calls.get(), 0);
}
