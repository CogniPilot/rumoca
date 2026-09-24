//! Native entry points preserve one complete canonical residual program.

use super::*;

pub(super) fn has_shared_conditional_owner(rows: &[Vec<LinearOp>]) -> bool {
    rows.iter().flatten().any(|operation| {
        matches!(operation, LinearOp::FunctionConditional { program, .. } if program.owner.is_some())
    })
}

impl CompiledResidualRows {
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
        out.resize(row.plan.output_count(), 0.0);
        let inputs = RowInputs {
            y,
            p,
            t,
            seed: None,
            external_tables,
        };
        with_active_external_tables(external_tables, || {
            self.call_selected_active(program, row, inputs, out)
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
        self.call_selected_with_registers(program, row, inputs, output, &mut regs)
    }

    fn call_selected_with_registers(
        &self,
        program: usize,
        row: &CompiledResidualRow,
        inputs: RowInputs<'_>,
        output: &mut [f64],
        regs: &mut Vec<f64>,
    ) -> Result<(), CompileError> {
        let expected = if row.interpreter_supported
            && should_validate_jit_row(row.validate_with_interpreter)
        {
            let mut expected = vec![0.0; output.len()];
            execute_row(&row.plan, regs, inputs, &mut expected)?;
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
                regs,
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

impl CompiledResidualRows {
    pub(crate) fn call_projection_outputs(
        &self,
        selection: &rumoca_ir_solve::ProjectionOutputSelection,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<bool, CompileError> {
        if out.len() != selection.output_len() {
            return Err(CompileError::Input(
                "projection output extent mismatch".into(),
            ));
        }
        if !self.selectable {
            return Ok(false);
        }
        let mut values = self.output_scratch.borrow_mut();
        let mut projected = self.projection_scratch.borrow_mut();
        let mut registers = self.regs_scratch.borrow_mut();
        projected.resize(out.len(), 0.0);
        projected.copy_from_slice(out);
        let inputs = RowInputs {
            y,
            p,
            t,
            seed: None,
            external_tables,
        };
        with_active_external_tables(external_tables, || {
            for program in selection.programs() {
                self.call_projection_program(program, inputs, &mut registers, &mut values)?;
                scatter_projection_outputs(program, &values, &mut projected)?;
            }
            Ok::<_, CompileError>(())
        })?;
        out.copy_from_slice(&projected);
        Ok(true)
    }

    fn call_projection_program(
        &self,
        program: &rumoca_ir_solve::ProjectionProgramOutputs,
        inputs: RowInputs<'_>,
        registers: &mut Vec<f64>,
        values: &mut Vec<f64>,
    ) -> Result<(), CompileError> {
        let index = program.program();
        let row = self.rows.get(index).ok_or_else(|| {
            CompileError::Input(format!("residual program {index} is outside compiled rows"))
        })?;
        validate_input_requirements(row_input_requirements(&row.plan), inputs.y, inputs.p, None)?;
        if row.plan.output_count() != program.output_count() {
            return Err(CompileError::Input(
                "projection program output count mismatch".into(),
            ));
        }
        values.resize(program.output_count(), 0.0);
        self.call_selected_with_registers(index, row, inputs, values, registers)
    }
}

fn scatter_projection_outputs(
    program: &rumoca_ir_solve::ProjectionProgramOutputs,
    values: &[f64],
    projected: &mut [f64],
) -> Result<(), CompileError> {
    for &(offset, target) in program.placements() {
        let value = values
            .get(offset)
            .ok_or_else(|| CompileError::Input("projection program output is absent".into()))?;
        let target = projected.get_mut(target).ok_or_else(|| {
            CompileError::Input("projection output placement is outside buffer".into())
        })?;
        *target = *value;
    }
    Ok(())
}
