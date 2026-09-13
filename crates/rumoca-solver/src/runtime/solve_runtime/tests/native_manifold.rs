use super::*;
use std::cell::Cell;

#[derive(Default)]
struct Backend {
    residual_calls: Rc<Cell<usize>>,
    directional_calls: Rc<Cell<usize>>,
    fail_residual: Rc<Cell<bool>>,
    fail_directional: Rc<Cell<bool>>,
    decline: bool,
}

struct Expression {
    prepared: PreparedScalarProgramBlock,
    calls: Rc<Cell<usize>>,
    fail: Rc<Cell<bool>>,
}

impl CompiledSolveExpression for Expression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.calls.set(self.calls.get() + 1);
        if self.fail.get() {
            return Err("native manifold residual failed".into());
        }
        self.prepared
            .eval_with_context(y, p, t, RowEvalContext::default(), out)
            .map_err(|error| error.to_string())
    }
}

impl CompiledSolveJacobianExpression for Expression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.calls.set(self.calls.get() + 1);
        if self.fail.get() {
            return Err("native manifold directional failed".into());
        }
        self.prepared
            .eval_with_context(
                y,
                p,
                t,
                RowEvalContext {
                    seed: Some(seed),
                    ..Default::default()
                },
                out,
            )
            .map_err(|error| error.to_string())
    }
}

impl SolveExecutionBackend for Backend {
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        if self.decline {
            return Err("test backend declines this program".into());
        }
        Ok(Rc::new(Expression {
            prepared: PreparedScalarProgramBlock::new(block.clone()).unwrap(),
            calls: self.residual_calls.clone(),
            fail: self.fail_residual.clone(),
        }))
    }

    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        if self.decline {
            return Err("test backend declines this program".into());
        }
        Ok(Rc::new(Expression {
            prepared: PreparedScalarProgramBlock::new(block.clone()).unwrap(),
            calls: self.directional_calls.clone(),
            fail: self.fail_directional.clone(),
        }))
    }

    fn compile_assignment_schedule(
        &self,
        _: &solve::ComputeBlock,
        _: &solve::ContinuousRefreshOwners,
        _: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        Err("unused by manifold test".into())
    }

    fn compile_event_transaction(
        &self,
        _: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        Err("unused by manifold test".into())
    }
}

fn runtime(backend: Rc<Backend>) -> SolveRuntime {
    use solve::LinearOp::{Binary, Const, LoadSeed, LoadY, StoreOutput};
    let mut model = warm_start_test_model();
    model.problem.continuous.manifold_residual =
        solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![vec![
                LoadY { dst: 0, index: 0 },
                Binary {
                    dst: 1,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 0,
                },
                Const { dst: 2, value: 4.0 },
                Binary {
                    dst: 3,
                    op: solve::BinaryOp::Sub,
                    lhs: 1,
                    rhs: 2,
                },
                StoreOutput { src: 3 },
            ]],
            "native_manifold.mo",
        ));
    model.artifacts.continuous.manifold_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![vec![
                LoadY { dst: 0, index: 0 },
                LoadSeed { dst: 1, index: 0 },
                Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                Binary {
                    dst: 3,
                    op: solve::BinaryOp::Add,
                    lhs: 2,
                    rhs: 2,
                },
                StoreOutput { src: 3 },
            ]],
            "native_manifold_directional.mo",
        ));
    model.problem.continuous.manifold_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![0],
            y_indices: vec![0],
            tearing: None,
        }],
    };
    model.problem.continuous.refresh_owners =
        solve_eval::refresh_plan::build_continuous_refresh_owners(&mut model.problem).unwrap();
    SolveRuntime::new_with_execution_backend(&model, Some(backend)).unwrap()
}

#[test]
fn manifold_callbacks_use_the_prepared_native_backend() {
    for decline in [false, true] {
        let backend = Rc::new(Backend {
            decline,
            ..Default::default()
        });
        let runtime = runtime(backend.clone());
        let projection = RuntimeManifoldProjection { runtime: &runtime };
        let mut residual = [0.0];
        let mut directional = [0.0];
        // Only the state prefix is supplied; an unrelated algebraic slot is absent.
        for x in [2.0, 2.5, -3.0] {
            projection
                .eval_manifold_residual(&[x], &[], 0.0, &mut residual)
                .unwrap();
            projection
                .eval_manifold_jacobian_v(&[x], &[], 0.0, &[3.0], &mut directional)
                .unwrap();
            assert_eq!(residual, [x * x - 4.0]);
            assert_eq!(directional, [6.0 * x]);
        }
        let expected = if decline { 0 } else { 3 };
        assert_eq!(
            backend.residual_calls.get(),
            expected,
            "manifold residual bypassed native execution"
        );
        assert_eq!(
            backend.directional_calls.get(),
            expected,
            "manifold JVP bypassed native execution"
        );
    }
}

#[test]
fn native_manifold_failure_propagates_and_projection_rolls_back() {
    for fail_directional in [false, true] {
        let backend = Rc::new(Backend::default());
        let runtime = runtime(backend.clone());
        if fail_directional {
            backend.fail_directional.set(true);
        } else {
            backend.fail_residual.set(true);
        }
        let mut y = [3.0];
        let error = runtime
            .project_state_manifold(&mut y, &[], 0.0, 1e-8)
            .expect_err("a native error must not be retried in the interpreter");
        assert!(error.to_string().contains("native manifold"));
        assert_eq!(y, [3.0]);
        assert_eq!(backend.residual_calls.get(), 1);
        assert_eq!(
            backend.directional_calls.get(),
            usize::from(fail_directional)
        );
    }
}
