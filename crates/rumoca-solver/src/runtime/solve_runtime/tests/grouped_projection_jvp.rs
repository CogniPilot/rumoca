//! Shared tensor outputs execute once per prepared projection color.

use super::*;
use std::cell::Cell;

#[derive(Default)]
struct GroupedJacobian {
    single_calls: Cell<usize>,
    grouped_calls: Cell<usize>,
    fail: bool,
}

impl CompiledSolveJacobianExpression for GroupedJacobian {
    fn call(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _seed: &[f64],
        _tables: &[rumoca_core::ExternalTableData],
        _out: &mut [f64],
    ) -> Result<(), String> {
        panic!("a selected projection must not execute unrelated programs")
    }

    fn call_program_output(
        &self,
        (program, offset): (usize, usize),
        _y: &[f64],
        p: &[f64],
        _t: f64,
        seed: &[f64],
        _tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, String> {
        assert_eq!(program, 0);
        self.single_calls.set(self.single_calls.get() + 1);
        Ok(Some(
            [seed[0] + p[0] * seed[1], p[0] * seed[0] - seed[1]][offset],
        ))
    }

    fn call_program_outputs(
        &self,
        program: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, String> {
        assert_eq!(program, 0);
        self.grouped_calls.set(self.grouped_calls.get() + 1);
        if self.fail {
            return Err("grouped native JVP failed".into());
        }
        let solve_eval::JacobianEvalInputs { p, seed, .. } = inputs;
        out.clear();
        out.extend([seed[0] + p[0] * seed[1], p[0] * seed[0] - seed[1]]);
        Ok(true)
    }
}

fn coupled_program(derivative: bool) -> solve::ScalarProgramBlock {
    use solve::{BinaryOp as B, LinearOp as L};
    let load = |dst, index| {
        if derivative {
            L::LoadSeed { dst, index }
        } else {
            L::LoadY { dst, index }
        }
    };
    let ops = vec![
        load(0, 0),
        load(1, 1),
        L::LoadP { dst: 2, index: 0 },
        L::Binary {
            dst: 3,
            op: B::Mul,
            lhs: 2,
            rhs: 1,
        },
        L::Binary {
            dst: 4,
            op: B::Add,
            lhs: 0,
            rhs: 3,
        },
        L::Binary {
            dst: 5,
            op: B::Mul,
            lhs: 2,
            rhs: 0,
        },
        L::Binary {
            dst: 6,
            op: B::Sub,
            lhs: 5,
            rhs: 1,
        },
        L::Const {
            dst: 7,
            value: if derivative { 0.0 } else { 2.0 },
        },
        L::Const {
            dst: 8,
            value: if derivative { 0.0 } else { 1.0 },
        },
        L::Binary {
            dst: 9,
            op: B::Sub,
            lhs: 4,
            rhs: 7,
        },
        L::Binary {
            dst: 10,
            op: B::Sub,
            lhs: 6,
            rhs: 8,
        },
        L::StoreOutputRange {
            start: 9,
            count: 2,
            stride: 1,
        },
    ];
    solve::ScalarProgramBlock::with_output_indices(
        vec![ops],
        vec![test_span("grouped_projection.mo")],
        vec![1, 0],
    )
    .unwrap()
}

pub(super) fn coupled_model() -> solve::SolveModel {
    let mut model = mode_dependent_repivot_model();
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(coupled_program(false));
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(1)), Some(solve::scalar_slot_y(0))];
    model.problem.continuous.algebraic_projection_plan.blocks[0].rows = vec![1, 0];
    model.artifacts.continuous.implicit_jacobian_v_scalar = coupled_program(true);
    mirror_scalar_implicit_jvp(&mut model);
    derive_test_structural_artifacts(&mut model);
    model
}

#[derive(Default)]
struct GroupedResidual {
    batch: bool,
    batch_calls: Cell<usize>,
    single_calls: Cell<usize>,
    grouped_calls: Cell<usize>,
    fail: bool,
}

impl CompiledSolveExpression for GroupedResidual {
    fn call_projection_outputs(
        &self,
        _selection: &solve::ProjectionOutputSelection,
        y: &[f64],
        p: &[f64],
        _t: f64,
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<bool, String> {
        if !self.batch {
            return Ok(false);
        }
        self.batch_calls.set(self.batch_calls.get() + 1);
        if self.fail {
            return Err("batch native residual failed".into());
        }
        out.copy_from_slice(&[y[0] + p[0] * y[1] - 2.0, p[0] * y[0] - y[1] - 1.0]);
        Ok(true)
    }

    fn call(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _tables: &[rumoca_core::ExternalTableData],
        _out: &mut [f64],
    ) -> Result<(), String> {
        panic!("selected residuals must not execute unrelated programs")
    }

    fn call_program_output(
        &self,
        (program, offset): (usize, usize),
        y: &[f64],
        p: &[f64],
        _t: f64,
        _tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, String> {
        assert_eq!(program, 0);
        self.single_calls.set(self.single_calls.get() + 1);
        Ok(Some(
            [y[0] + p[0] * y[1] - 2.0, p[0] * y[0] - y[1] - 1.0][offset],
        ))
    }

    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        _t: f64,
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, String> {
        assert_eq!(program, 0);
        self.grouped_calls.set(self.grouped_calls.get() + 1);
        if self.fail {
            return Err("grouped native residual failed".into());
        }
        out.clear();
        out.extend([y[0] + p[0] * y[1] - 2.0, p[0] * y[0] - y[1] - 1.0]);
        Ok(true)
    }
}

#[test]
fn grouped_projection_residual_evaluates_shared_outputs_once_at_each_point() {
    let mut runtime = SolveRuntime::new_fixture(&coupled_model()).unwrap();
    let native = Rc::new(GroupedResidual::default());
    runtime.compiled_implicit_rhs = Some(native.clone());
    let projection = super::native_projection_jvp::selected_projection(&runtime, false);
    let selection = runtime.continuous_structural.algebraic_projection()[0]
        .residual_output_evaluation()
        .unwrap();
    let mut selected = [0.0; 2];
    assert!(
        projection
            .eval_implicit_residual_outputs(selection, &[8.0, -5.0], &[2.0], 1.0, &mut selected)
            .unwrap()
    );
    assert_eq!(selected, [-4.0, 20.0]);
    for k in [2.0, -3.0, 0.0, 2.0] {
        let before = native.grouped_calls.get();
        let mut y = vec![8.0, -5.0];
        runtime
            .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[k], 1e-10, 4)
            .unwrap();
        assert!((y[0] - (2.0 + k) / (1.0 + k * k)).abs() < 1e-9);
        assert!((y[1] - (2.0 * k - 1.0) / (1.0 + k * k)).abs() < 1e-9);
        assert!(
            native.grouped_calls.get() > before,
            "changed coordinates need fresh residuals"
        );
        assert_eq!(
            native.single_calls.get(),
            0,
            "tensor residual outputs must share one invocation"
        );
    }
}

#[test]
fn grouped_projection_residual_failure_propagates_without_replay_or_partial_commit() {
    let mut runtime = SolveRuntime::new_fixture(&coupled_model()).unwrap();
    let native = Rc::new(GroupedResidual {
        fail: true,
        ..Default::default()
    });
    runtime.compiled_implicit_rhs = Some(native.clone());
    let mut y = vec![8.0, -5.0];
    let error = runtime
        .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[2.0], 1e-10, 4)
        .unwrap_err();
    assert!(error.to_string().contains("grouped native residual failed"));
    assert_eq!(native.grouped_calls.get(), 1);
    assert_eq!(native.single_calls.get(), 0);
    assert_eq!(y, [8.0, -5.0]);
}

#[test]
fn grouped_projection_jvp_executes_once_per_color_at_changing_coefficients() {
    let model = coupled_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let native = Rc::new(GroupedJacobian::default());
    runtime.compiled_implicit_projection_jacobian_v = Some(native.clone());
    for (iteration, k) in [2.0, -3.0, 0.0, 2.0].into_iter().enumerate() {
        let mut y = vec![8.0, -5.0];
        runtime
            .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[k], 1e-10, 4)
            .unwrap();
        assert!((y[0] - (2.0 + k) / (1.0 + k * k)).abs() < 1e-9);
        assert!((y[1] - (2.0 * k - 1.0) / (1.0 + k * k)).abs() < 1e-9);
        assert_eq!(native.grouped_calls.get(), 2 * (iteration + 1));
        assert_eq!(
            native.single_calls.get(),
            0,
            "shared outputs must not replay the program"
        );
    }
}

#[test]
fn grouped_projection_jvp_error_propagates_without_single_row_retry() {
    let model = coupled_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let native = Rc::new(GroupedJacobian {
        fail: true,
        ..Default::default()
    });
    runtime.compiled_implicit_projection_jacobian_v = Some(native.clone());
    let mut y = vec![8.0, -5.0];
    let error = runtime
        .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[2.0], 1e-10, 4)
        .unwrap_err();
    assert!(error.to_string().contains("grouped native JVP failed"));
    assert_eq!(native.grouped_calls.get(), 1);
    assert_eq!(native.single_calls.get(), 0);
    assert_eq!(y, [8.0, -5.0]);
}

#[test]
fn grouped_projection_jvp_interpreter_uses_the_same_output_projection() {
    let runtime = SolveRuntime::new_fixture(&coupled_model()).unwrap();
    let mut y = vec![8.0, -5.0];
    runtime
        .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[2.0], 1e-10, 4)
        .unwrap();
    assert!((y[0] - 0.8).abs() < 1e-12);
    assert!((y[1] - 0.6).abs() < 1e-12);
}

#[test]
fn grouped_projection_jvp_preserves_reverse_completed_rows_and_full_seed_space() {
    let mut runtime = SolveRuntime::new_fixture(&coupled_model()).unwrap();
    let native = Rc::new(GroupedJacobian::default());
    runtime.compiled_implicit_full_jacobian_v = Some(native.clone());
    let projection = super::native_projection_jvp::selected_projection(&runtime, true);
    let selection = runtime.continuous_structural.algebraic_projection()[0]
        .output_evaluation(0)
        .unwrap();
    let mut out = [-99.0, -99.0];
    let inputs = solve_eval::JacobianEvalInputs {
        y: &[8.0, -5.0],
        p: &[2.0],
        t: 1.0,
        seed: &[1.0, 3.0, 0.0],
    };
    assert!(
        projection
            .eval_implicit_jacobian_v_outputs(selection, inputs, &[true, false], &mut out)
            .unwrap()
    );
    assert_eq!(out, [7.0, -99.0]);
    assert_eq!(native.grouped_calls.get(), 1);
    assert!(
        projection
            .eval_implicit_jacobian_v_outputs(selection, inputs, &[false, false], &mut out)
            .unwrap()
    );
    assert_eq!(
        native.grouped_calls.get(),
        1,
        "settled rows need no invocation"
    );
}

#[test]
fn batched_projection_residual_failure_is_not_replayed_or_committed() {
    let mut runtime = SolveRuntime::new_fixture(&coupled_model()).unwrap();
    let native = Rc::new(GroupedResidual {
        batch: true,
        fail: true,
        ..Default::default()
    });
    runtime.compiled_implicit_rhs = Some(native.clone());
    let mut y = [8.0, -5.0];
    let error = runtime
        .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[2.0], 1e-10, 4)
        .unwrap_err();
    assert_eq!(y, [8.0, -5.0]);
    assert!(error.to_string().contains("batch native residual failed"));
    assert_eq!(native.batch_calls.get(), 1);
    assert_eq!(native.grouped_calls.get(), 0);
    assert_eq!(native.single_calls.get(), 0);
}

#[test]
fn batched_projection_residual_preserves_fresh_parameter_certificates() {
    let mut runtime = SolveRuntime::new_fixture(&coupled_model()).unwrap();
    let native = Rc::new(GroupedResidual {
        batch: true,
        ..Default::default()
    });
    runtime.compiled_implicit_rhs = Some(native.clone());
    for k in [2.0, -3.0, 0.0] {
        let mut y = [8.0, -5.0];
        let before = native.batch_calls.get();
        runtime
            .refresh_algebraic_and_output_slots_certified(1.0, &mut y, &[k], 1e-10, 4)
            .unwrap();
        assert!((y[0] + k * y[1] - 2.0).abs() < 1e-10);
        assert!((k * y[0] - y[1] - 1.0).abs() < 1e-10);
        assert!(native.batch_calls.get() > before);
    }
    assert_eq!(native.grouped_calls.get(), 0);
    assert_eq!(native.single_calls.get(), 0);
}
