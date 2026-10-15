//! Complete tangent execution issued during runtime construction.

#[cfg(test)]
mod tests;

use std::rc::Rc;

use rumoca_eval_solve::{
    EvalSolveError, JacobianEvalInputs, PreparedScalarProgramBlock, RowEvalContext,
};
use rumoca_ir_solve as solve;

use super::super::super::selected_programs::CompleteJacobian;
use super::super::super::{SolveExecutionBackend, admitted_compiled_error, compiled_jacobian};
use crate::RuntimeSolveError;

#[derive(Clone)]
enum Execution {
    Reference,
    Compiled(Rc<CompleteJacobian>),
}

#[derive(Clone)]
pub(super) struct TangentProgram {
    source: PreparedScalarProgramBlock,
    execution: Execution,
}

impl TangentProgram {
    pub(super) fn prepare(
        label: &str,
        source: &solve::ScalarProgramBlock,
        backend: Option<&Rc<dyn SolveExecutionBackend>>,
    ) -> Result<Self, EvalSolveError> {
        let execution = match compiled_jacobian(backend, label, source)? {
            Some(code) => Execution::Compiled(code),
            None => Execution::Reference,
        };
        Ok(Self {
            source: PreparedScalarProgramBlock::new(source.clone())?,
            execution,
        })
    }

    pub(super) fn row_output_position(&self, output: usize) -> Option<(usize, usize)> {
        self.source.row_output_position(output)
    }

    fn inputs<'a>(
        y: &'a [f64],
        p: &'a [f64],
        t: f64,
        context: RowEvalContext<'a>,
    ) -> Result<JacobianEvalInputs<'a>, RuntimeSolveError> {
        let seed = context
            .seed
            .ok_or_else(|| RuntimeSolveError::solve_ir("coupled event tangent has no direction"))?;
        Ok(JacobianEvalInputs { y, p, t, seed })
    }

    fn error(&self, error: RuntimeSolveError) -> RuntimeSolveError {
        admitted_compiled_error(
            "coupled event tangent",
            self.source.block().program_spans(),
            error,
        )
    }

    pub(super) fn eval_with_context(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        match &self.execution {
            Execution::Reference => self
                .source
                .eval_with_context(y, p, t, context, out)
                .map_err(Into::into),
            Execution::Compiled(code) => {
                let inputs = Self::inputs(y, p, t, context)?;
                code.call(
                    y,
                    p,
                    t,
                    inputs.seed,
                    context.external_tables.unwrap_or_default(),
                    out,
                )
                .map_err(|error| self.error(error))
            }
        }
    }

    pub(super) fn eval_row_output_unchecked_with_context(
        &self,
        program: usize,
        output: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
    ) -> Result<f64, RuntimeSolveError> {
        match &self.execution {
            Execution::Reference => self
                .source
                .eval_row_output_unchecked_with_context(program, output, y, p, t, context)
                .map_err(Into::into),
            Execution::Compiled(code) => {
                let inputs = Self::inputs(y, p, t, context)?;
                code.call_program_output(
                    (program, output),
                    y,
                    p,
                    t,
                    inputs.seed,
                    context.external_tables.unwrap_or_default(),
                )
                .map_err(|error| self.error(error))
            }
        }
    }

    pub(super) fn eval_row_outputs_unchecked_with_context(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        match &self.execution {
            Execution::Reference => self
                .source
                .eval_row_outputs_unchecked_with_context(program, y, p, t, context, out)
                .map_err(Into::into),
            Execution::Compiled(code) => {
                let inputs = Self::inputs(y, p, t, context)?;
                code.call_program_outputs(
                    program,
                    inputs,
                    context.external_tables.unwrap_or_default(),
                    out,
                )
                .map_err(|error| self.error(error))
            }
        }
    }

    pub(super) fn eval_row_unchecked_with_context(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
    ) -> Result<f64, RuntimeSolveError> {
        match &self.execution {
            Execution::Reference => self
                .source
                .eval_row_unchecked_with_context(program, y, p, t, context)
                .map_err(Into::into),
            Execution::Compiled(_) => {
                if self.source.row_output_count(program) != Some(1) {
                    return Err(RuntimeSolveError::solve_ir(
                        "a scalar coupled event tangent requires exactly one source output",
                    ));
                }
                self.eval_row_output_unchecked_with_context(program, 0, y, p, t, context)
            }
        }
    }
}
