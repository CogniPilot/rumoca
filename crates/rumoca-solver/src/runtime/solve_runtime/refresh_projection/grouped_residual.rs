//! Execute complete residual programs selected by the construction owner.

use super::*;

impl RefreshProjectionModel<'_> {
    pub(super) fn eval_grouped_residual_outputs(
        &self,
        selection: &solve::ProjectionOutputSelection,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if out.len() != selection.output_len() {
            return Err(RuntimeSolveError::solve_ir(
                "grouped residual output extent mismatch",
            ));
        }
        let mut values = self.runtime.compiled_output_scratch.borrow_mut();
        for program in selection.programs() {
            self.eval_residual_program_outputs(program.program(), y, p, t, &mut values)?;
            if values.len() != program.output_count() {
                return Err(RuntimeSolveError::solve_ir(
                    "grouped residual program output count mismatch",
                ));
            }
            self.scatter_residual_program_outputs(program, y, t, &values, out)?;
        }
        Ok(())
    }

    fn scatter_residual_program_outputs(
        &self,
        program: &solve::ProjectionProgramOutputs,
        y: &[f64],
        t: f64,
        values: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        for &(offset, row) in program.placements() {
            let value = values[offset];
            if !value.is_finite() {
                let source = self
                    .runtime
                    .implicit_scalar_rhs
                    .row_output_index(program.program(), offset)
                    .ok_or(RuntimeSolveError::solve_ir(
                        "grouped residual source output is absent",
                    ))?;
                self.runtime
                    .report_nonfinite_implicit_residual_row_inputs(t, y, source, value);
            }
            out[row] = value;
        }
        Ok(())
    }

    fn eval_residual_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        if let Some(result) = self
            .runtime
            .eval_split_residual_program(program, (y, p, t), out)
        {
            return result;
        }
        if let Some(compiled) = &self.runtime.compiled_implicit_rhs
            && compiled
                .call_program_outputs(
                    program,
                    y,
                    p,
                    t,
                    self.runtime.model.external_tables.as_slice(),
                    out,
                )
                .map_err(RuntimeSolveError::solve_ir)?
        {
            return Ok(());
        }
        self.runtime
            .implicit_scalar_rhs
            .eval_row_outputs_unchecked_with_context(
                program,
                y,
                p,
                t,
                self.runtime.row_eval_context(),
                out,
            )
            .map_err(Into::into)
    }
}
