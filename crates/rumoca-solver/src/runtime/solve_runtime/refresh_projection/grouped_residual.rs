//! Execute complete residual programs selected by the construction owner.

use super::*;

impl RefreshProjectionModel<'_> {
    /// The residual of block `block_index` over `selection`, its issued
    /// residual selection. A fused residual entry is tried first, silently:
    /// when it answers, the selection's other programs follow in order; when
    /// it declines, every program runs in order exactly as without it, so the
    /// first error and every non-finite report are those of that order.
    pub(super) fn eval_grouped_residual_outputs(
        &self,
        block_index: usize,
        selection: &solve::ProjectionOutputSelection,
        point: (&[f64], &[f64], f64),
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if out.len() != selection.output_len() {
            return Err(RuntimeSolveError::solve_ir(
                "grouped residual output extent mismatch",
            ));
        }
        let tables = self.runtime.model.external_tables.as_slice();
        if let Some((entry, fused)) = self.fused_residual(block_index, selection)
            && entry.eval(fused, point, tables, out)
        {
            self.eval_unfused_residual_programs(selection, fused, point, out)?;
            #[cfg(debug_assertions)]
            self.debug_assert_fused_residual_matches(selection, point, out)?;
            return Ok(());
        }
        for program in selection.programs() {
            self.eval_residual_entry(program, point, out)?;
        }
        Ok(())
    }

    /// After a fused residual entry answered `fused`'s programs, evaluate the
    /// rest of `selection` in its order.
    fn eval_unfused_residual_programs(
        &self,
        selection: &solve::ProjectionOutputSelection,
        fused: &solve::FusedResidualSelection,
        point: (&[f64], &[f64], f64),
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut fused_programs = fused
            .selection()
            .programs()
            .iter()
            .map(solve::ProjectionProgramOutputs::program)
            .peekable();
        for program in selection.programs() {
            if fused_programs.next_if_eq(&program.program()).is_none() {
                self.eval_residual_entry(program, point, out)?;
            }
        }
        Ok(())
    }

    /// Evaluate one program of a residual selection and place its outputs.
    fn eval_residual_entry(
        &self,
        program: &solve::ProjectionProgramOutputs,
        (y, p, t): (&[f64], &[f64], f64),
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut values = self.runtime.scalar.compiled_output_scratch.borrow_mut();
        self.eval_residual_program_outputs(program.program(), y, p, t, &mut values)?;
        if values.len() != program.output_count() {
            return Err(RuntimeSolveError::solve_ir(
                "grouped residual program output count mismatch",
            ));
        }
        self.scatter_residual_program_outputs(program, y, t, &values, out)
    }

    /// Debug-only refinement guard: a pass answered by the fused residual
    /// entry places exactly the bits the program-by-program pass places.
    #[cfg(debug_assertions)]
    fn debug_assert_fused_residual_matches(
        &self,
        selection: &solve::ProjectionOutputSelection,
        point: (&[f64], &[f64], f64),
        out: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut reference = out.to_vec();
        for program in selection.programs() {
            self.eval_residual_entry(program, point, &mut reference)?;
        }
        let bits = |values: &[f64]| values.iter().map(|v| v.to_bits()).collect::<Vec<_>>();
        assert_eq!(
            bits(out),
            bits(&reference),
            "a fused residual pass places its programs' residuals bit for bit"
        );
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
                    .scalar
                    .implicit_scalar_rhs
                    .source()
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
        if self
            .runtime
            .eval_split_residual_program(program, (y, p, t), out)?
            .is_some()
        {
            return Ok(());
        }
        self.runtime
            .scalar
            .implicit_scalar_rhs
            .eval_program_outputs(
                program,
                RowEvalPoint { y, p, t },
                self.runtime.row_eval_context(),
                out,
            )
    }
}
