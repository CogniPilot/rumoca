use super::*;
use std::cell::Cell;

#[derive(Default)]
struct Backend {
    attempts: Cell<usize>,
    calls: Rc<Cell<usize>>,
    composite: bool,
}

struct Schedule {
    rows: PreparedScalarProgramBlock,
    targets: Vec<usize>,
    calls: Rc<Cell<usize>>,
}

impl CompiledSolveAssignmentSchedule for Schedule {
    fn call(
        &self,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        _: &[rumoca_core::ExternalTableData],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        for (program, target) in self.targets.iter().enumerate() {
            let value =
                self.rows
                    .eval_row_with_context(program, y, p, t, RowEvalContext::default())?;
            y[*target] = value;
        }
        Ok(())
    }
}

struct Expression {
    code: Rc<dyn CompiledSolveExpression>,
    calls: Rc<Cell<usize>>,
}

impl CompiledSolveExpression for Expression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        self.code.call(y, p, t, tables, out)
    }
}

impl SolveExecutionBackend for Backend {
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        Ok(Rc::new(Expression {
            code: super::super::test_kernels::expression(source)?,
            calls: Rc::clone(&self.calls),
        }))
    }

    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        super::super::test_kernels::jacobian(source)
    }

    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        super::super::test_kernels::assignment(source, owners, schedule)
    }

    fn compile_torn_assignment_rows(
        &self,
        rows: &[Vec<solve::LinearOp>],
        targets: &[usize],
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        self.attempts.set(self.attempts.get() + 1);
        if !self.composite {
            return Err("optional composite unavailable".into());
        }
        Ok(Rc::new(Schedule {
            rows: PreparedScalarProgramBlock::new(spanned_block(rows.to_vec(), "torn_schedule.mo"))
                .map_err(|error| error.to_string())?,
            targets: targets.to_vec(),
            calls: Rc::clone(&self.calls),
        }))
    }
}

fn model() -> solve::SolveModel {
    let mut model = warm_start_test_model();
    model.problem.continuous.algebraic_projection_plan.blocks =
        vec![solve::AlgebraicProjectionBlock {
            rows: vec![1],
            y_indices: vec![1],
            tearing: Some(solve::BlockTearing {
                causal_steps: vec![solve::CausalStep {
                    row: 1,
                    y_index: 1,
                    coefficient: solve::CausalCoefficient::Unit,
                }],
                ..Default::default()
            }),
            alternate_charts: Vec::new(),
        }]
        .into();
    derive_test_structural_artifacts(&mut model);
    model
}

#[test]
fn optional_torn_composite_is_prepared_before_execution_and_not_retried_by_clones() {
    check_preparation(false);
}

#[test]
fn admitted_torn_composite_is_prepared_before_execution_and_survives_clones() {
    check_preparation(true);
}

fn check_preparation(composite: bool) {
    let model = model();
    let backend = Rc::new(Backend {
        composite,
        ..Default::default()
    });
    let runtime = SolveRuntime::new_fixture_with_backend(&model, backend.clone()).unwrap();
    let prepared = backend.attempts.get();
    assert!(
        prepared > 0,
        "cold composite attempts must happen during construction"
    );
    assert_eq!(
        backend.calls.get(),
        0,
        "preparation must not execute source code"
    );
    let tearing = model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .scalar_blocks()[0]
        .tearing
        .as_ref()
        .unwrap();
    for runtime in [&runtime, &runtime.clone()] {
        let unissued = solve::BlockTearing {
            residual_rows: vec![usize::MAX],
            ..Default::default()
        };
        assert!(
            runtime
                .scalar_execution()
                .unwrap()
                .prepared_torn_sweep(&unissued)
                .is_none()
        );
        let projection = super::native_projection_jvp::selected_projection(runtime, false);
        let mut y = [1.0, 2.0];
        assert!(
            projection
                .torn_block_sweep(tearing, &mut y, &[], 0.0, &mut Vec::new())
                .unwrap()
        );
        assert_eq!(y, [1.0, 0.0]);
    }
    assert_eq!(
        backend.attempts.get(),
        prepared,
        "execution must not compile again"
    );
    assert!(
        backend.calls.get() >= 2,
        "the prepared native base must execute"
    );
}
