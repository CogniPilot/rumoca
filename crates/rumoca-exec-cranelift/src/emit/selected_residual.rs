//! Native entry points preserve one complete canonical residual program.

use super::*;

pub(super) fn has_shared_conditional_owner(rows: &[Vec<LinearOp>]) -> bool {
    rows.iter().flatten().any(|operation| {
        matches!(operation, LinearOp::FunctionConditional { program, .. } if program.owner.is_some())
    })
}

impl CompiledResidualRows {
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
            seed: None,
            external_tables,
        };
        with_active_external_tables(external_tables, || {
            let mut output = self.output_scratch.borrow_mut();
            output.resize(count, 0.0);
            self.call_selected_active(program, row, inputs, &mut output)?;
            Ok(Some(output[offset]))
        })
    }

    fn call_selected_active(
        &self,
        program: usize,
        row: &CompiledResidualRow,
        inputs: RowInputs<'_>,
        output: &mut [f64],
    ) -> Result<(), CompileError> {
        let mut regs = self.regs_scratch.borrow_mut();
        let expected = if row.interpreter_supported
            && should_validate_jit_row(row.validate_with_interpreter)
        {
            let mut expected = vec![0.0; output.len()];
            execute_row(&row.plan, &mut regs, inputs, &mut expected)?;
            Some(expected)
        } else {
            None
        };
        let compiled = &self.jits[program];
        // SAFETY: selectable compilation emits one entry per source program;
        // the input bounds and aggregate output extent were checked above.
        let status = unsafe {
            call_residual_jit(
                &compiled.jit,
                compiled.register_count,
                &mut regs,
                inputs.y,
                inputs.p,
                inputs.t,
                output,
            )
        };
        self.record_jit_call();
        status::check(status)?;
        if let Some(expected) = expected {
            validate_interpreted_outputs(output, &expected)?;
        }
        Ok(())
    }
}
