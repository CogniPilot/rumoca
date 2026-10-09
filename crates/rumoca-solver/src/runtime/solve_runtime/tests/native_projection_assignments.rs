//! Native singleton projection consumes the same checked exact assignment.

use super::*;
use crate::runtime::projection::ImplicitProjectionModel;
use std::cell::Cell;

#[derive(Default)]
struct AssignmentBackend {
    compilations: Cell<usize>,
    calls: Rc<Cell<usize>>,
    fail: bool,
    decline: bool,
    schedule_failure: bool,
    target_values: bool,
    target_compilations: Cell<usize>,
}

struct FailingAssignment(Rc<Cell<usize>>);

impl CompiledSolveAssignmentSchedule for FailingAssignment {
    fn call(
        &self,
        y: &mut [f64],
        _p: &[f64],
        _t: f64,
        _tables: &[rumoca_core::ExternalTableData],
    ) -> Result<(), crate::RuntimeSolveError> {
        self.0.set(self.0.get() + 1);
        y[1] = 123.0;
        Err("native assignment failed after a partial write".into())
    }
}

struct AssignmentExpression {
    block: PreparedScalarProgramBlock,
    calls: Rc<Cell<usize>>,
    fail: bool,
}

impl CompiledSolveExpression for AssignmentExpression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        _external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        if self.fail {
            return Err("native projection assignment failed".into());
        }
        self.block
            .eval_with_context(y, p, t, RowEvalContext::default(), out)
            .map_err(Into::into)
    }
}

struct PrivateTargetValues {
    block: PreparedScalarProgramBlock,
    count: usize,
    calls: Rc<Cell<usize>>,
    fail: bool,
}
impl CompiledSolveTargetValues for PrivateTargetValues {
    fn call(
        &self,
        selected: usize,
        y: &[f64],
        p: &[f64],
        time: f64,
        context: RowEvalContext<'_>,
    ) -> Result<f64, crate::RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        if self.fail {
            return Err("admitted private target-value fault".into());
        }
        let mut output = vec![0.; self.count];
        self.block
            .eval_with_context(y, p, time, context, &mut output)
            .map_err(|error| error.to_string())?;
        output
            .get(selected)
            .copied()
            .ok_or_else(|| "private target index out of bounds".into())
    }
}

impl SolveExecutionBackend for AssignmentBackend {
    fn compile_target_values(
        &self,
        plan: &rumoca_eval_solve::PreparedTargetValuePlan,
        _context: RowEvalContext<'_>,
    ) -> Result<Rc<dyn CompiledSolveTargetValues>, String> {
        if !self.target_values {
            return Err("test backend target values are disabled".into());
        }
        self.target_compilations
            .set(self.target_compilations.get() + 1);
        if self.decline {
            return Err("test backend target values declined".into());
        }
        Ok(Rc::new(PrivateTargetValues {
            block: PreparedScalarProgramBlock::new(plan.program().clone())
                .map_err(|error| error.to_string())?,
            count: plan.program().stored_output_count(),
            calls: self.calls.clone(),
            fail: self.fail,
        }))
    }

    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        self.compilations.set(self.compilations.get() + 1);
        if self.decline {
            return Err("test backend cannot compile this program".into());
        }
        Ok(Rc::new(AssignmentExpression {
            block: PreparedScalarProgramBlock::new(block.clone())
                .map_err(|error| error.to_string())?,
            calls: self.calls.clone(),
            fail: self.fail,
        }))
    }

    fn compile_jacobian_expression(
        &self,
        _block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        Err("unused by assignment test".into())
    }

    fn compile_assignment_schedule(
        &self,
        _source: &solve::ComputeBlock,
        _owners: &solve::ContinuousRefreshOwners,
        _schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        if self.schedule_failure {
            return Ok(Rc::new(FailingAssignment(self.calls.clone())));
        }
        Err("unused by assignment test".into())
    }

    fn compile_event_transaction(
        &self,
        _program: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        Err("unused by assignment test".into())
    }
}

#[test]
fn failed_native_refresh_restores_values_without_disabling_native_execution() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let backend = Rc::new(AssignmentBackend {
        schedule_failure: true,
        ..Default::default()
    });
    runtime.execution_backend = Some(backend.clone());
    for _ in 0..2 {
        let mut y = model.initial_y.to_vec();
        let error = runtime
            .refresh_algebraic_and_output_slots(0.0, &mut y, &[], 1e-10, 4)
            .expect_err("admitted native assignment errors must reach the caller");
        assert!(
            error
                .to_string()
                .contains("native assignment failed after a partial write")
        );
        assert_eq!(
            y,
            model.initial_y.to_vec(),
            "a failed refresh must restore the complete snapshot"
        );
    }
    assert_eq!(
        backend.calls.get(),
        2,
        "failure must not cache an interpreter fallback"
    );
}

fn projection(runtime: &SolveRuntime) -> RefreshProjectionModel<'_> {
    RefreshProjectionModel {
        runtime,
        seed_linearizations: None,
        plan: &runtime.algebraic_refresh.simultaneous_plan,
        block_indices: &runtime.algebraic_refresh.simultaneous_block_indices,
        plan_validated: true,
        jacobian_v: ProjectionJacobian::SolverY {
            block: &runtime.implicit_projection_jacobian_v,
            scalar: &runtime.implicit_projection_scalar_jacobian_v,
        },
    }
}

#[test]
fn singleton_assignment_uses_one_compiled_exact_program() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let backend = Rc::new(AssignmentBackend::default());
    runtime.execution_backend = Some(backend.clone());
    for target in [2.0, -3.0, 1e20] {
        assert_eq!(
            projection(&runtime)
                .eval_implicit_target_value(1, 1, &[1.0, target], &[], 0.0)
                .unwrap(),
            Some(0.0)
        );
    }
    assert_eq!(backend.compilations.get(), 1);
    assert_eq!(backend.calls.get(), 3);
}

#[test]
fn singleton_native_failure_is_not_retried_through_the_interpreter() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let backend = Rc::new(AssignmentBackend {
        fail: true,
        ..Default::default()
    });
    runtime.execution_backend = Some(backend.clone());
    let error = projection(&runtime)
        .eval_implicit_target_value(1, 1, &[1.0, 2.0], &[], 0.0)
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("native projection assignment failed")
    );
    assert_eq!(backend.calls.get(), 1);
}

#[test]
fn unsupported_native_assignment_keeps_the_interpreter_and_caches_the_decline() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let backend = Rc::new(AssignmentBackend {
        decline: true,
        ..Default::default()
    });
    runtime.execution_backend = Some(backend.clone());
    for target in [3.0, -8.0] {
        assert_eq!(
            projection(&runtime)
                .eval_implicit_target_value(1, 1, &[1.0, target], &[], 0.0)
                .unwrap(),
            Some(0.0)
        );
    }
    assert_eq!(backend.compilations.get(), 1);
    assert_eq!(backend.calls.get(), 0);
}

#[test]
fn aggregate_outputs_do_not_generate_separate_native_assignment_programs() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let backend = Rc::new(AssignmentBackend::default());
    runtime.execution_backend = Some(backend.clone());
    let mut row = shifted_variable_residual_row(1, 3.0);
    row.extend([
        solve::LinearOp::Const { dst: 3, value: 9.0 },
        solve::LinearOp::StoreOutput { src: 3 },
    ]);
    runtime.implicit_scalar_rhs =
        PreparedScalarProgramBlock::new(spanned_block(vec![row], "shared_assignment.mo")).unwrap();
    assert_eq!(
        projection(&runtime)
            .eval_implicit_target_value(0, 1, &[1.0, 100.0], &[], 0.0)
            .unwrap(),
        Some(3.0)
    );
    assert_eq!(backend.compilations.get(), 0);
}

#[test]
fn native_assignment_preserves_a_tiny_root_beside_a_large_incoming_value() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let backend = Rc::new(AssignmentBackend::default());
    runtime.execution_backend = Some(backend.clone());
    runtime.implicit_scalar_rhs = PreparedScalarProgramBlock::new(spanned_block(
        vec![shifted_variable_residual_row(1, 1e-17)],
        "tiny_assignment.mo",
    ))
    .unwrap();
    assert_eq!(
        projection(&runtime)
            .eval_implicit_target_value(0, 1, &[1.0, 1e20], &[], 0.0)
            .unwrap(),
        Some(1e-17)
    );
    assert_eq!(backend.calls.get(), 1);
}

#[test]
fn singular_coefficients_preserve_the_projection_decline() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let backend = Rc::new(AssignmentBackend::default());
    runtime.implicit_scalar_rhs = PreparedScalarProgramBlock::new(spanned_block(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::LoadY { dst: 1, index: 1 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Const { dst: 3, value: 1.0 },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 2,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ]],
        "singular_assignment.mo",
    ))
    .unwrap();
    for coefficient in [2.0, 0.0, f64::INFINITY, f64::NAN] {
        runtime.execution_backend = None;
        let reference = projection(&runtime)
            .isolation_value(0, 1, &[1.0, 3.0], &[coefficient], 0.0)
            .unwrap();
        runtime.execution_backend = Some(backend.clone());
        assert_eq!(
            projection(&runtime)
                .isolation_value(0, 1, &[1.0, 3.0], &[coefficient], 0.0)
                .unwrap(),
            reference,
            "coefficient={coefficient}"
        );
    }
    assert_eq!(backend.compilations.get(), 1);
    assert_eq!(backend.calls.get(), 4);
}

#[test]
fn compiled_private_target_returns_checked_value_without_y_commit() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let backend = Rc::new(AssignmentBackend {
        target_values: true,
        ..Default::default()
    });
    runtime.execution_backend = Some(backend.clone());
    for previous in [2., -3., 1e20] {
        let y = [1., previous];
        let bits = y.map(f64::to_bits);
        assert_eq!(
            projection(&runtime)
                .eval_implicit_target_value(1, 1, &y, &[], 0.)
                .unwrap(),
            Some(0.)
        );
        assert_eq!(y.map(f64::to_bits), bits);
    }
    assert_eq!(backend.target_compilations.get(), 1);
    assert_eq!(backend.compilations.get(), 0);
    assert_eq!(backend.calls.get(), 3);
}

#[test]
fn admitted_private_target_fault_is_authoritative_and_does_not_demote() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let backend = Rc::new(AssignmentBackend {
        target_values: true,
        fail: true,
        ..Default::default()
    });
    runtime.execution_backend = Some(backend.clone());
    let y = [1., 2.];
    for _ in 0..2 {
        let error = projection(&runtime)
            .eval_implicit_target_value(1, 1, &y, &[], 0.)
            .unwrap_err();
        assert!(
            error
                .to_string()
                .contains("admitted private target-value fault")
        );
        assert_eq!(y, [1., 2.]);
    }
    assert_eq!(backend.target_compilations.get(), 1);
    assert_eq!(backend.compilations.get(), 0);
    assert_eq!(backend.calls.get(), 2);
}

#[test]
fn private_target_pre_admission_decline_is_cached_and_keeps_canonical_path() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let backend = Rc::new(AssignmentBackend {
        target_values: true,
        decline: true,
        ..Default::default()
    });
    runtime.execution_backend = Some(backend.clone());
    for previous in [3., -8.] {
        assert_eq!(
            projection(&runtime)
                .eval_implicit_target_value(1, 1, &[1., previous], &[], 0.)
                .unwrap(),
            Some(0.)
        );
    }
    assert_eq!(backend.target_compilations.get(), 1);
    assert_eq!(backend.calls.get(), 0);
}
