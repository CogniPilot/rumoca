//! Reference implementations for backend test doubles, never production code.

use super::*;
pub(crate) mod recording;

#[derive(Default)]
pub(crate) struct ReferenceBackend;

impl SolveExecutionBackend for ReferenceBackend {
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        expression(source)
    }
    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        jacobian(source)
    }
    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        assignment(source, owners, schedule)
    }
}

struct Expression(PreparedScalarProgramBlock);

pub(crate) fn expression(
    block: &solve::ScalarProgramBlock,
) -> Result<Rc<dyn CompiledSolveExpression>, String> {
    Ok(Rc::new(Expression(
        PreparedScalarProgramBlock::new(block.clone()).map_err(|error| error.to_string())?,
    )))
}

pub(crate) fn jacobian(
    block: &solve::ScalarProgramBlock,
) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
    Ok(Rc::new(Expression(
        PreparedScalarProgramBlock::new(block.clone()).map_err(|error| error.to_string())?,
    )))
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
        self.0
            .eval_with_context(
                y,
                p,
                t,
                RowEvalContext {
                    external_tables: Some(tables),
                    ..Default::default()
                },
                out,
            )
            .map_err(Into::into)
    }
}

impl CompiledSolveJacobianExpression for Expression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.0
            .eval_with_context(
                y,
                p,
                t,
                RowEvalContext {
                    seed: Some(seed),
                    external_tables: Some(tables),
                    ..Default::default()
                },
                out,
            )
            .map_err(Into::into)
    }
}

struct Assignment(interpreted_schedules::InterpretedSchedule);

pub(crate) fn assignment(
    source: &solve::ComputeBlock,
    owners: &solve::ContinuousRefreshOwners,
    schedule: &solve::ExactRefreshAssignmentSchedule,
) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
    Ok(Rc::new(Assignment(
        interpreted_schedules::prepare_schedule(schedule, source, owners)
            .map_err(|error| error.to_string())?,
    )))
}

impl CompiledSolveAssignmentSchedule for Assignment {
    fn call(
        &self,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<(), RuntimeSolveError> {
        let mut values = Vec::new();
        let mut ordinal = 0;
        for program in 0..self.0.block.block().row_count() {
            self.0.block.eval_row_outputs_unchecked_with_context(
                program,
                y,
                p,
                t,
                RowEvalContext {
                    external_tables: Some(tables),
                    ..Default::default()
                },
                &mut values,
            )?;
            for value in &values {
                y[self.0.targets[ordinal]] = *value;
                ordinal += 1;
            }
        }
        Ok(())
    }
}
