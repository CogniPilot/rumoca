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
    /// Run `programs` in order at one point, passing each program's outputs to
    /// `commit` (which may write `seed`, an input of later programs) before the
    /// next program runs: what [`Self::call_program_outputs`] and a commit give
    /// one program at a time, with the inputs checked, the external tables
    /// installed and the scratch borrowed once for the whole sequence. Each
    /// program's outputs are staged and reach `commit` only after its native
    /// call succeeds, so a failing program exposes nothing and stops the
    /// sequence with every earlier commit standing.
    pub(crate) fn call_programs_committing<E: From<CompileError>>(
        &self,
        programs: std::ops::Range<usize>,
        inputs: (&[f64], &[f64], f64),
        seed: &mut [f64],
        external_tables: &[ExternalTableData],
        commit: &mut crate::ProgramCommit<'_, E>,
    ) -> Result<(), E> {
        let (y, p, t) = inputs;
        let rows = self.rows.get(programs.clone()).ok_or_else(|| {
            CompileError::Input(format!(
                "Jacobian programs {programs:?} are outside compiled rows"
            ))
        })?;
        validate_input_requirements(self.input_requirements, y, p, Some(seed))?;
        self.with_tables(external_tables, || {
            let mut staged = self.output_scratch.borrow_mut();
            let mut registers = self.regs_scratch.borrow_mut();
            for (program, row) in programs.zip(rows) {
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
