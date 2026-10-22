//! Native entry points preserve one complete canonical residual program.

use super::*;

pub(super) fn has_shared_conditional_owner(rows: &[Vec<LinearOp>]) -> bool {
    rows.iter().flatten().any(|operation| {
        matches!(operation, LinearOp::FunctionConditional { program, .. } if program.owner.is_some())
    })
}

impl CompiledResidualRows {
    pub(crate) fn independent_program_output_count(&self, program: usize) -> Option<usize> {
        self.selectable.then_some(())?;
        self.rows.get(program).map(|row| row.plan.output_count())
    }

    pub(crate) fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, CompileError> {
        let row = self.rows.get(program).ok_or_else(|| {
            CompileError::Input(format!(
                "residual program {program} is outside compiled rows"
            ))
        })?;
        if !self.selectable {
            return Ok(false);
        }
        validate_input_requirements(row_input_requirements(&row.plan), y, p, None)?;
        reserve_native_outputs(out, row.plan.output_count())?;
        let inputs = RowInputs {
            y,
            p,
            t,
            #[cfg(test)]
            seed: None,
            #[cfg(test)]
            external_tables,
        };
        self.with_tables(external_tables, || {
            let mut staged = self.output_scratch.borrow_mut();
            resize_native_outputs(&mut staged, row.plan.output_count())?;
            self.call_selected_active(program, row, inputs, &mut staged)?;
            out.clear();
            out.extend_from_slice(&staged);
            Ok::<_, CompileError>(())
        })?;
        Ok(true)
    }

    pub(crate) fn call_program_output(
        &self,
        (program, offset): (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
    ) -> Result<Option<f64>, CompileError> {
        let row = self.rows.get(program).ok_or_else(|| {
            CompileError::Input(format!(
                "residual program {program} is outside compiled rows"
            ))
        })?;
        let count = row.plan.output_count();
        if offset >= count {
            return Err(CompileError::Input(format!(
                "residual program {program} output {offset} is outside {count} outputs"
            )));
        }
        if !self.selectable {
            return Ok(None);
        }
        validate_input_requirements(row_input_requirements(&row.plan), y, p, None)?;
        let inputs = RowInputs {
            y,
            p,
            t,
            #[cfg(test)]
            seed: None,
            #[cfg(test)]
            external_tables,
        };
        self.with_tables(external_tables, || {
            let mut output = self.output_scratch.borrow_mut();
            output.resize(count, 0.0);
            self.call_selected_active(program, row, inputs, &mut output)?;
            Ok(Some(output[offset]))
        })
    }

    fn call_selected_active(
        &self,
        program: usize,
        _row: &CompiledResidualRow,
        inputs: RowInputs<'_>,
        output: &mut [f64],
    ) -> Result<(), CompileError> {
        let mut regs = self.regs_scratch.borrow_mut();
        let compiled = &self.jits[program];
        // SAFETY: selectable compilation emits one entry per source program;
        // the input bounds and aggregate output extent were checked above.
        let status =
            unsafe { call_residual_jit(compiled, &mut regs, inputs.y, inputs.p, inputs.t, output) };
        self.record_jit_call();
        status::check(status)?;
        #[cfg(test)]
        if _row.interpreter_supported {
            let mut expected = vec![0.0; output.len()];
            execute_row(&_row.plan, &mut regs, inputs, &mut expected)?;
            validate_interpreted_outputs(output, &expected)?;
        }
        Ok(())
    }
}

impl CompiledResidualRows {
    /// Output `offset` of each program at `coordinates`, in order, into
    /// `out`: the values [`Self::call_program_output`] returns one call at a
    /// time, with the external tables installed once for the whole set.
    /// `false` declines, before any execution, exactly when those calls
    /// would decline.
    pub(crate) fn call_program_outputs_at(
        &self,
        coordinates: &[(usize, usize)],
        inputs: (&[f64], &[f64], f64),
        external_tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<bool, CompileError> {
        let (y, p, t) = inputs;
        validate_output_len(out, coordinates.len())?;
        for &(program, offset) in coordinates {
            let row = self.rows.get(program).ok_or_else(|| {
                CompileError::Input(format!(
                    "residual program {program} is outside compiled rows"
                ))
            })?;
            let count = row.plan.output_count();
            if offset >= count {
                return Err(CompileError::Input(format!(
                    "residual program {program} output {offset} is outside {count} outputs"
                )));
            }
        }
        if !self.selectable {
            return Ok(false);
        }
        for &(program, _) in coordinates {
            let plan = &self.rows[program].plan;
            validate_input_requirements(row_input_requirements(plan), y, p, None)?;
        }
        let inputs = RowInputs {
            y,
            p,
            t,
            #[cfg(test)]
            seed: None,
            #[cfg(test)]
            external_tables,
        };
        self.with_tables(external_tables, || {
            let mut output = self.output_scratch.borrow_mut();
            let mut staged = self.transaction_scratch.borrow_mut();
            resize_native_outputs(&mut staged, coordinates.len())?;
            for (&(program, offset), value) in coordinates.iter().zip(staged.iter_mut()) {
                let row = &self.rows[program];
                output.resize(row.plan.output_count(), 0.0);
                self.call_selected_active(program, row, inputs, &mut output)?;
                *value = output[offset];
            }
            out[..coordinates.len()].copy_from_slice(&staged);
            Ok(true)
        })
    }
}
