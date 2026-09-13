//! Selected projection JVPs use the checked native program without a full sweep.

use super::*;
use std::cell::Cell;

struct SelectedJacobian {
    calls: Cell<usize>,
    fail: bool,
    coordinate: (usize, usize),
}

impl CompiledSolveJacobianExpression for SelectedJacobian {
    fn call(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _seed: &[f64],
        _external_tables: &[rumoca_core::ExternalTableData],
        _out: &mut [f64],
    ) -> Result<(), String> {
        panic!("a selected projection must not evaluate unrelated JVP programs");
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        seed: &[f64],
        _external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, String> {
        assert_eq!(coordinate, self.coordinate);
        self.calls.set(self.calls.get() + 1);
        if self.fail {
            return Err("selected native JVP failed".into());
        }
        Ok(Some(seed[1]))
    }
}

pub(super) fn selected_projection(
    runtime: &SolveRuntime,
    full_seed: bool,
) -> RefreshProjectionModel<'_> {
    RefreshProjectionModel {
        runtime,
        plan: &runtime.algebraic_refresh.simultaneous_plan,
        block_indices: &runtime.algebraic_refresh.simultaneous_block_indices,
        plan_validated: true,
        jacobian_v: if full_seed {
            ProjectionJacobian::SolverYAndParameters(&runtime.implicit_jacobian_v)
        } else {
            ProjectionJacobian::SolverY {
                block: &runtime.implicit_projection_jacobian_v,
                scalar: &runtime.implicit_projection_scalar_jacobian_v,
            }
        },
    }
}

#[test]
fn selected_projection_jvp_uses_the_compiled_program_in_both_seed_spaces() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let native = Rc::new(SelectedJacobian {
        calls: Cell::new(0),
        fail: false,
        coordinate: (1, 0),
    });
    for full_seed in [false, true] {
        runtime.compiled_implicit_projection_jacobian_v = None;
        runtime.compiled_implicit_full_jacobian_v = None;
        if full_seed {
            runtime.compiled_implicit_full_jacobian_v = Some(native.clone());
        } else {
            runtime.compiled_implicit_projection_jacobian_v = Some(native.clone());
        }
        let projection = selected_projection(&runtime, full_seed);
        for seed in [[1.0, 2.0], [-3.0, 5.0]] {
            assert_eq!(
                projection
                    .eval_implicit_jacobian_v_row(1, &[1.0, 2.0], &[], 0.0, &seed)
                    .unwrap(),
                Some(seed[1])
            );
        }
    }
    assert_eq!(
        native.calls.get(),
        4,
        "selected JVPs bypassed native execution"
    );
}

#[test]
fn selected_native_jvp_failure_is_not_retried_in_the_interpreter() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let native = Rc::new(SelectedJacobian {
        calls: Cell::new(0),
        fail: true,
        coordinate: (1, 0),
    });
    runtime.compiled_implicit_projection_jacobian_v = Some(native.clone());
    let error = selected_projection(&runtime, false)
        .eval_implicit_jacobian_v_row(1, &[1.0, 2.0], &[], 0.0, &[1.0, 2.0])
        .expect_err("native execution failure must propagate");
    assert!(error.to_string().contains("selected native JVP failed"));
    assert_eq!(native.calls.get(), 1);
}

#[test]
fn selected_projection_uses_the_jvp_program_mapping_and_aggregate_output_offset() {
    let seed = |index| {
        vec![
            solve::LinearOp::LoadSeed { dst: 0, index },
            solve::LinearOp::StoreOutput { src: 0 },
        ]
    };
    let permuted = solve::ScalarProgramBlock::with_output_indices(
        vec![seed(1), seed(0)],
        vec![test_span("permuted_jvp.mo"); 2],
        vec![1, 0],
    )
    .unwrap();
    let aggregate = spanned_block(
        vec![vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::LoadSeed { dst: 1, index: 1 },
            solve::LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ]],
        "aggregate_jvp.mo",
    );
    for (block, coordinate) in [(permuted, (0, 0)), (aggregate, (0, 1))] {
        let mut model = warm_start_test_model();
        model.artifacts.continuous.implicit_jacobian_v_scalar = block;
        mirror_scalar_implicit_jvp(&mut model);
        let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
        let native = Rc::new(SelectedJacobian {
            calls: Cell::new(0),
            fail: false,
            coordinate,
        });
        runtime.compiled_implicit_projection_jacobian_v = Some(native.clone());
        let actual = selected_projection(&runtime, false)
            .eval_implicit_jacobian_v_row(1, &[1.0, 2.0], &[], 0.0, &[1.0, 7.0])
            .unwrap();
        assert_eq!(actual, Some(7.0));
        assert_eq!(native.calls.get(), 1);
    }
}
