use super::super::tests::warm_start_test_model;
use super::*;
use std::cell::Cell;

struct Fault(Rc<Cell<usize>>);
impl CompiledSolveExpression for Fault {
    fn call(
        &self,
        _: &[f64],
        _: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.0.set(self.0.get() + 1);
        out[0] = 777.0;
        Err("first initial hook fault".into())
    }
}
impl CompiledSolveJacobianExpression for Fault {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        _: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        CompiledSolveExpression::call(self, y, p, t, tables, out)
    }
}

#[test]
fn admitted_initial_full_and_partial_faults_preserve_output_bits() {
    for owner in 0..4 {
        let mut runtime = SolveRuntime::new_fixture(&warm_start_test_model()).unwrap();
        let calls = Rc::new(Cell::new(0));
        let compiled = Rc::new(Fault(calls.clone()));
        match owner {
            0 => runtime.compiled_implicit_rhs = Some(compiled),
            1 => runtime.compiled_implicit_projection_jacobian_v = Some(compiled),
            2 => runtime.compiled_initial_residual = Some(compiled),
            _ => runtime.compiled_initial_residual_jacobian_v = Some(compiled),
        }
        let projection = InitialProjectionModel {
            runtime: &runtime,
            tol: 1e-10,
            max_iters: 16,
            refreshes_algebraic_reads: false,
        };
        let mut out = [-0.0_f64, f64::from_bits(0x7ff8_0000_0000_0123)];
        let before = out.map(f64::to_bits);
        let error = match owner {
            0 => projection.eval_residual(&[1.0, 2.0], &[], 0.0, &mut out),
            1 => projection.eval_jacobian_v(&[1.0, 2.0], &[], 0.0, &[1.0, 1.0], &mut out),
            2 => projection.eval_initial_residual(&[1.0, 2.0], &[], 0.0, None, &mut out),
            _ => projection.eval_partial_initial_jacobian_v(
                &[1.0, 2.0],
                &[],
                0.0,
                &[1.0, 1.0],
                &mut out,
            ),
        }
        .expect_err("an admitted initial hook fault must propagate");
        assert!(matches!(
            &error,
            RuntimeSolveError::CompiledExecution { .. }
        ));
        assert!(error.to_string().contains("first initial hook fault"));
        assert_eq!(calls.get(), 1);
        assert_eq!(out.map(f64::to_bits), before);
    }
}
