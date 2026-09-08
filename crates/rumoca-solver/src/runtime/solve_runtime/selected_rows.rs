//! Session-sealed interpreter execution for dynamically guarded scalar rows.
//!
//! These owners require a concrete guard trace before a native specialization
//! can be constructed. Runtime discovery is forbidden: until preparation can
//! issue that trace, their one execution arm is the interpreter for the whole
//! session.

use std::collections::BTreeMap;

use super::*;

#[derive(Clone, Copy)]
pub(super) struct SelectedRows<'a> {
    pub(super) block: &'a PreparedScalarProgramBlock,
}

#[derive(Clone, Copy)]
pub(super) struct RowEvalPoint<'a> {
    pub(super) y: &'a [f64],
    pub(super) p: &'a [f64],
    pub(super) t: f64,
}

impl SolveRuntime {
    pub(super) fn eval_single_output_rows<P: InterpreterPermit>(
        &self,
        selected_arm: &P,
        rows: SelectedRows<'_>,
        row_indices: &[usize],
        point: RowEvalPoint<'_>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        for &row in row_indices {
            let value = rows.block.eval_row_unchecked_with_context(
                row,
                point.y,
                point.p,
                point.t,
                selected_arm.row_eval_context(self),
            )?;
            let out_len = out.len();
            let slot = out.get_mut(row).ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "selected expression row {row} is outside output length {out_len}"
                ))
            })?;
            *slot = value;
        }
        Ok(())
    }

    pub(super) fn eval_selected_outputs<P: InterpreterPermit>(
        &self,
        selected_arm: &P,
        rows: SelectedRows<'_>,
        output_indices: &[usize],
        point: RowEvalPoint<'_>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut programs = BTreeMap::<usize, Vec<(usize, usize)>>::new();
        for &output in output_indices {
            let (program, offset) = rows.block.row_output_position(output).ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "selected expression output {output} has no owning program"
                ))
            })?;
            programs.entry(program).or_default().push((output, offset));
        }
        for (program, selected) in programs {
            let mut values = Vec::new();
            rows.block.eval_row_outputs_unchecked_with_context(
                program,
                point.y,
                point.p,
                point.t,
                selected_arm.row_eval_context(self),
                &mut values,
            )?;
            copy_interpreted_program_outputs(&values, program, &selected, out)?;
        }
        Ok(())
    }
}

fn copy_interpreted_program_outputs(
    values: &[f64],
    program: usize,
    selected: &[(usize, usize)],
    out: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    for &(output, offset) in selected {
        let value = values.get(offset).copied().ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "selected expression output {output} offset {offset} is outside program {program}"
            ))
        })?;
        let out_len = out.len();
        let slot = out.get_mut(output).ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "selected expression output {output} is outside output length {out_len}"
            ))
        })?;
        *slot = value;
    }
    Ok(())
}
