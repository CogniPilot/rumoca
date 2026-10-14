//! Algebraic projection preserves source output coordinates through native dispatch.

use super::native_projection_jvp::selected_projection;
use super::*;
use std::cell::Cell;

struct SelectedResidual {
    calls: Cell<usize>,
    fail: bool,
    decline: bool,
    coordinate: (usize, usize),
}

impl CompiledSolveExpression for SelectedResidual {
    fn call(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _external_tables: &[rumoca_core::ExternalTableData],
        _out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError> {
        panic!("selected residuals must not execute unrelated programs");
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        _p: &[f64],
        _t: f64,
        _external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, crate::RuntimeSolveError> {
        assert_eq!(coordinate, self.coordinate);
        self.calls.set(self.calls.get() + 1);
        if self.fail {
            return Err("selected native residual failed".into());
        }
        Ok((!self.decline).then_some(y[1]))
    }
}

#[test]
fn selected_projection_residual_requires_complete_preparation() {
    let mut runtime = SolveRuntime::new_fixture(&warm_start_test_model()).unwrap();
    let native = Rc::new(SelectedResidual {
        calls: Cell::new(0),
        fail: false,
        decline: false,
        coordinate: (1, 0),
    });
    install_mock_primal(&mut runtime, native.clone());
    for value in [2.0, -7.0] {
        assert_eq!(
            selected_projection(&runtime, false)
                .eval_implicit_residual_row(1, &[1.0, value], &[], 0.0)
                .unwrap(),
            Some(value)
        );
    }
    assert_eq!(native.calls.get(), 2);
    let source =
        to_scalar_program_block(&warm_start_test_model().problem.continuous.implicit_rhs).unwrap();
    let missing = Rc::new(SelectedResidual {
        calls: Cell::new(0),
        fail: false,
        decline: true,
        coordinate: (1, 0),
    });
    let error = prepare_mock_primal(source.clone(), missing.clone(), true)
        .err()
        .expect("missing required selected kernel must fail during preparation");
    assert!(
        matches!(error, EvalSolveError::CompiledExecution { span: Some(span), .. } if Some(span) == source.program_span(0))
    );
    assert_eq!(
        missing.calls.get(),
        0,
        "a refused owner cannot execute numeric code"
    );
}

#[test]
fn selected_projection_residual_propagates_native_errors() {
    let mut runtime = SolveRuntime::new_fixture(&warm_start_test_model()).unwrap();
    let native = Rc::new(SelectedResidual {
        calls: Cell::new(0),
        fail: true,
        decline: false,
        coordinate: (1, 0),
    });
    install_mock_primal(&mut runtime, native.clone());
    let error = selected_projection(&runtime, false)
        .eval_implicit_residual_row(1, &[1.0, 2.0], &[], 0.0)
        .expect_err("native failure must not be retried through the interpreter");
    assert!(
        error
            .to_string()
            .contains("selected native residual failed")
    );
    assert_eq!(native.calls.get(), 1);
}

#[test]
fn selected_projection_residual_preserves_aggregate_output_mapping() {
    let mut runtime = SolveRuntime::new_fixture(&warm_start_test_model()).unwrap();
    let block = solve::ScalarProgramBlock::with_output_indices(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::LoadY { dst: 1, index: 1 },
            solve::LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ]],
        vec![test_span("aggregate_residual.mo")],
        vec![4, 1],
    )
    .unwrap();
    let native = Rc::new(SelectedResidual {
        calls: Cell::new(0),
        fail: false,
        decline: false,
        coordinate: (0, 1),
    });
    runtime.scalar_fixture_mut().implicit_scalar_rhs =
        prepare_mock_primal(block, native.clone(), false).unwrap();
    assert_eq!(
        selected_projection(&runtime, false)
            .eval_implicit_residual_row(1, &[1.0, 7.0], &[], 0.0)
            .unwrap(),
        Some(7.0)
    );
    assert_eq!(native.calls.get(), 1);
}
