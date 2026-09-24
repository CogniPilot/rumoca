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
        if let Some(compiled) = &self.runtime.compiled_implicit_rhs
            && compiled
                .call_projection_outputs(
                    selection,
                    y,
                    p,
                    t,
                    self.runtime.model.external_tables.as_slice(),
                    out,
                )
                .map_err(RuntimeSolveError::solve_ir)?
        {
            self.report_nonfinite_projection_outputs(selection, y, t, out)?;
            return Ok(());
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

    fn report_nonfinite_projection_outputs(
        &self,
        selection: &solve::ProjectionOutputSelection,
        y: &[f64],
        t: f64,
        out: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if out.iter().all(|value| value.is_finite()) {
            return Ok(());
        }
        for program in selection.programs() {
            for &(offset, row) in program.placements() {
                self.report_nonfinite_residual_output(program.program(), offset, y, t, out[row])?;
            }
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
            self.report_nonfinite_residual_output(program.program(), offset, y, t, value)?;
            out[row] = value;
        }
        Ok(())
    }

    fn report_nonfinite_residual_output(
        &self,
        program: usize,
        offset: usize,
        y: &[f64],
        t: f64,
        value: f64,
    ) -> Result<(), RuntimeSolveError> {
        if !value.is_finite() {
            let source = self
                .runtime
                .implicit_scalar_rhs
                .row_output_index(program, offset)
                .ok_or_else(|| {
                    RuntimeSolveError::solve_ir("grouped residual source output is absent")
                })?;
            self.runtime
                .report_nonfinite_implicit_residual_row_inputs(t, y, source, value);
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
