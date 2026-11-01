//! Evaluate all outputs of one retained JVP program in one invocation.

use super::*;

impl CompiledJacobianRows {
    pub(crate) fn independent_program_output_count(&self, program: usize) -> Option<usize> {
        self.rows.get(program).map(|row| row.plan.output_count())
    }

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
        reserve_native_outputs(out, row.plan.output_count())?;
        self.with_tables(external_tables, || {
            let mut staged = self.output_scratch.borrow_mut();
            resize_native_outputs(&mut staged, row.plan.output_count())?;
            self.call_jacobian_row(
                row,
                &mut self.regs_scratch.borrow_mut(),
                &JacobianCallContext {
                    y: inputs.y,
                    p: inputs.p,
                    t: inputs.t,
                    v: inputs.seed,
                    #[cfg(test)]
                    external_tables,
                },
                &mut staged,
            )?;
            out.clear();
            out.extend_from_slice(&staged);
            Ok(())
        })
    }
}

impl CompiledJacobianRows {
    /// Execute only the listed programs, in list order. Each successful
    /// program commits before the next reads its seed; failures leave earlier
    /// commits standing and stop the sequence. Validate every index before
    /// execution, then check inputs and borrow scratch once for the sweep.
    pub(crate) fn call_selected_programs_committing<E: From<CompileError>>(
        &self,
        programs: &[usize],
        inputs: (&[f64], &[f64], f64),
        seed: &mut [f64],
        external_tables: &[ExternalTableData],
        commit: &mut crate::ProgramCommit<'_, E>,
    ) -> Result<(), E> {
        if let Some(program) = programs.iter().find(|&&program| program >= self.rows.len()) {
            return Err(CompileError::Input(format!(
                "Jacobian program {program} is outside compiled rows"
            ))
            .into());
        }
        let rows = programs
            .iter()
            .filter_map(|&program| self.rows.get(program).map(|row| (program, row)));
        self.call_rows_committing(rows, inputs, seed, external_tables, commit)
    }

    fn call_rows_committing<'r, E: From<CompileError>>(
        &self,
        rows: impl Iterator<Item = (usize, &'r CompiledJacobianRow)>,
        inputs: (&[f64], &[f64], f64),
        seed: &mut [f64],
        external_tables: &[ExternalTableData],
        commit: &mut crate::ProgramCommit<'_, E>,
    ) -> Result<(), E> {
        let (y, p, t) = inputs;
        validate_input_requirements(self.input_requirements, y, p, Some(seed))?;
        self.with_tables(external_tables, || {
            let mut staged = self.output_scratch.borrow_mut();
            let mut registers = self.regs_scratch.borrow_mut();
            for (program, row) in rows {
                resize_native_outputs(&mut staged, row.plan.output_count())?;
                self.call_jacobian_row(
                    row,
                    &mut registers,
                    &JacobianCallContext {
                        y,
                        p,
                        t,
                        v: seed,
                        #[cfg(test)]
                        external_tables,
                    },
                    &mut staged,
                )?;
                commit(program, &staged, seed)?;
            }
            Ok(())
        })
    }

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
            #[cfg(test)]
            external_tables,
        };
        self.with_tables(external_tables, || {
            let mut output = self.output_scratch.borrow_mut();
            let mut registers = self.regs_scratch.borrow_mut();
            let mut staged = self.transaction_scratch.borrow_mut();
            resize_native_outputs(&mut staged, coordinates.len())?;
            for (&(program, offset), value) in coordinates.iter().zip(staged.iter_mut()) {
                let row = &self.rows[program];
                output.resize(row.plan.output_count(), 0.0);
                self.call_jacobian_row(row, &mut registers, &context, &mut output)?;
                *value = output[offset];
            }
            out[..coordinates.len()].copy_from_slice(&staged);
            Ok(())
        })
    }
}
