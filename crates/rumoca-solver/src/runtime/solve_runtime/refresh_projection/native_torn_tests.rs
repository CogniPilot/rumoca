use super::*;
use std::cell::Cell;

struct Schedule {
    status: Result<Option<TornSweepStatus>, &'static str>,
}
impl CompiledSolveAssignmentSchedule for Schedule {
    fn call(
        &self,
        _: &mut [f64],
        _: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
    ) -> Result<(), String> {
        panic!("unguarded protocol must not execute")
    }
    fn call_torn(
        &self,
        _: &mut [f64],
        _: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<TornSweepStatus>, String> {
        self.status.map_err(str::to_owned)
    }
}
struct Residual {
    calls: Cell<usize>,
}
impl CompiledSolveExpression for Residual {
    fn call(
        &self,
        _: &[f64],
        _: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.calls.set(self.calls.get() + 1);
        out[0] = 999.0;
        Err("native residual witness".into())
    }
}

#[test]
fn native_torn_decline_skips_residual_and_execution_errors_never_replay() {
    let model = super::super::tests::warm_start_test_model();
    let runtime = SolveRuntime::new_fixture(&model).unwrap();
    let projection = RefreshProjectionModel {
        runtime: &runtime,
        seed_linearizations: None,
        plan: &runtime.algebraic_refresh.simultaneous_plan,
        block_indices: &runtime.algebraic_refresh.simultaneous_block_indices,
        plan_validated: true,
        jacobian_v: ProjectionJacobian::SolverY {
            block: &runtime.implicit_projection_jacobian_v,
            scalar: &runtime.implicit_projection_scalar_jacobian_v,
        },
    };
    for status in [
        Ok(None),
        Ok(Some(TornSweepStatus::Declined)),
        Err("native assignment witness"),
        Ok(Some(TornSweepStatus::Completed)),
    ] {
        let residual = Rc::new(Residual {
            calls: Cell::new(0),
        });
        let compiled = CompiledTornSweep {
            schedule: Rc::new(Schedule { status }),
            residual_block: residual.clone(),
            residual_outputs: vec![Some(0)].into_boxed_slice(),
            residual_len: 1,
        };
        let mut raw = vec![Some(7.0)];
        let result =
            projection.eval_compiled_torn_sweep(&compiled, &mut [1.0, 2.0], &[], 0.0, &mut raw);
        match status {
            Ok(Some(TornSweepStatus::Completed)) => {
                assert!(
                    result
                        .unwrap_err()
                        .to_string()
                        .contains("native residual witness")
                );
                assert_eq!(residual.calls.get(), 1);
            }
            Err(message) => {
                assert!(result.unwrap_err().to_string().contains(message));
                assert_eq!(residual.calls.get(), 0);
            }
            Ok(expected) => {
                assert_eq!(result.unwrap(), expected);
                assert_eq!(residual.calls.get(), 0);
            }
        }
        assert_eq!(
            raw,
            vec![Some(7.0)],
            "failed residual does not publish output"
        );
    }
}
