//! Execute constructor-issued JVP output groups without recovering a schedule.

use super::*;

impl RefreshProjectionModel<'_> {
    pub(super) fn eval_grouped_jacobian_outputs(
        &self,
        selection: &solve::ProjectionOutputSelection,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        enabled: &[bool],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if enabled.len() != selection.output_len() || out.len() != selection.output_len() {
            return Err(RuntimeSolveError::solve_ir(
                "grouped Jacobian output extent mismatch",
            ));
        }
        let mut values = self.runtime.compiled_output_scratch.borrow_mut();
        for program in selection.programs() {
            if !program.placements().iter().any(|&(_, row)| enabled[row]) {
                continue;
            }
            self.eval_jacobian_program_outputs(program.program(), inputs, &mut values)?;
            if values.len() != program.output_count() {
                return Err(RuntimeSolveError::solve_ir(
                    "grouped Jacobian program output count mismatch",
                ));
            }
            for (offset, row) in program
                .placements()
                .iter()
                .copied()
                .filter(|&(_, row)| enabled[row])
            {
                out[row] = values[offset];
            }
        }
        Ok(())
    }

    fn eval_jacobian_program_outputs(
        &self,
        program: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        if let Some(compiled) = self.jacobian_v.compiled(self.runtime)
            && compiled
                .call_program_outputs(
                    program,
                    inputs,
                    self.runtime.model.external_tables.as_slice(),
                    out,
                )
                .map_err(RuntimeSolveError::solve_ir)?
        {
            return Ok(());
        }
        self.jacobian_v
            .scalar()
            .eval_row_outputs_unchecked_with_context(
                program,
                inputs.y,
                inputs.p,
                inputs.t,
                RowEvalContext {
                    seed: Some(inputs.seed),
                    ..self.runtime.row_eval_context()
                },
                out,
            )
            .map_err(Into::into)
    }
}
