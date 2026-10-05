//! Evaluate all outputs of one retained JVP program in one invocation.

use super::*;

impl CompiledJacobianRows {
    pub(crate) fn call_program_outputs(
        &self,
        program: usize,
        inputs: rumoca_eval_solve::JacobianEvalInputs<'_>,
        external_tables: &[ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<(), CompileError> {
        let row = self.rows.get(program).ok_or_else(|| {
            CompileError::Input(format!(
                "Jacobian program {program} is outside compiled rows"
            ))
        })?;
        validate_input_requirements(
            self.input_requirements,
            inputs.y,
            inputs.p,
            Some(inputs.seed),
        )?;
        out.resize(row.plan.output_count(), 0.0);
        with_active_external_tables(external_tables, || {
            self.call_jacobian_row(
                row,
                &mut self.regs_scratch.borrow_mut(),
                &JacobianCallContext {
                    y: inputs.y,
                    p: inputs.p,
                    t: inputs.t,
                    v: inputs.seed,
                    external_tables,
                },
                out,
            )
        })
    }
}

impl CompiledJacobianRows {
    /// Output `offset` of each program at `coordinates`, in order, into
    /// `out`: the values [`Self::call_program_output`] returns one call at a
    /// time, with the inputs checked and the external tables installed once
    /// for the whole set.
    pub(crate) fn call_program_outputs_at(
        &self,
        coordinates: &[(usize, usize)],
        inputs: rumoca_eval_solve::JacobianEvalInputs<'_>,
        external_tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        validate_output_len(out, coordinates.len())?;
        for &(program, offset) in coordinates {
            let row = self.rows.get(program).ok_or_else(|| {
                CompileError::Input(format!(
                    "Jacobian program {program} is outside compiled rows"
                ))
            })?;
            let count = row.plan.output_count();
            if offset >= count {
                return Err(CompileError::Input(format!(
                    "Jacobian program {program} output {offset} is outside {count} outputs"
                )));
            }
        }
        validate_input_requirements(
            self.input_requirements,
            inputs.y,
            inputs.p,
            Some(inputs.seed),
        )?;
        let context = JacobianCallContext {
            y: inputs.y,
            p: inputs.p,
            t: inputs.t,
            v: inputs.seed,
            external_tables,
        };
        with_active_external_tables(external_tables, || {
            let mut output = self.output_scratch.borrow_mut();
            let mut registers = self.regs_scratch.borrow_mut();
            for (&(program, offset), value) in coordinates.iter().zip(out) {
                let row = &self.rows[program];
                output.resize(row.plan.output_count(), 0.0);
                self.call_jacobian_row(row, &mut registers, &context, &mut output)?;
                *value = output[offset];
            }
            Ok(())
        })
    }
}
