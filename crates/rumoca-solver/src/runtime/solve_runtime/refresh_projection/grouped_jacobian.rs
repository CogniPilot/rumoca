//! Execute constructor-issued JVP output groups without recovering a schedule.

use super::*;

impl RefreshProjectionModel<'_> {
    /// Every program of `selection` with an enabled row runs once, in
    /// selection order, as one committing sweep of the directional owner. An
    /// owner that declines the sweep runs the same programs one at a time.
    /// Neither path writes `seed`.
    pub(super) fn eval_grouped_jacobian_outputs(
        &self,
        selection: &solve::ProjectionOutputSelection,
        point: (&[f64], &[f64], f64),
        seed: &mut [f64],
        enabled: &[bool],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if enabled.len() != selection.output_len() || out.len() != selection.output_len() {
            return Err(RuntimeSolveError::solve_ir(
                "grouped Jacobian output extent mismatch",
            ));
        }
        let is_enabled = |program: &&solve::ProjectionProgramOutputs| {
            program.placements().iter().any(|&(_, row)| enabled[row])
        };
        // Taken, not borrowed, so an evaluation nested inside the sweep works
        // on its own (unreserved) list; the reserved list is restored after.
        let mut programs = self.runtime.scalar.grouped_program_scratch.take();
        programs.clear();
        programs.extend(
            selection
                .programs()
                .iter()
                .filter(is_enabled)
                .map(solve::ProjectionProgramOutputs::program),
        );
        let swept = if programs.is_empty() {
            Ok(true)
        } else {
            self.sweep_enabled_programs(&programs, selection, point, seed, enabled, out)
        };
        self.runtime
            .scalar
            .grouped_program_scratch
            .replace(programs);
        if swept? {
            return Ok(());
        }
        let (y, p, t) = point;
        let inputs = solve_eval::JacobianEvalInputs {
            y,
            p,
            t,
            seed: &*seed,
        };
        let mut values = self.runtime.scalar.compiled_output_scratch.borrow_mut();
        for program in selection.programs().iter().filter(is_enabled) {
            self.eval_jacobian_program_outputs(program.program(), inputs, &mut values)?;
            place_enabled_outputs(program, &values, enabled, out)?;
        }
        Ok(())
    }

    /// Run `programs`, the enabled programs of `selection` in its order, as
    /// one committing sweep; `Ok(false)` when the owner declines before any
    /// program executes.
    fn sweep_enabled_programs(
        &self,
        programs: &[usize],
        selection: &solve::ProjectionOutputSelection,
        point: (&[f64], &[f64], f64),
        seed: &mut [f64],
        enabled: &[bool],
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let mut entries = selection
            .programs()
            .iter()
            .filter(|program| program.placements().iter().any(|&(_, row)| enabled[row]));
        self.jacobian_v.owner().eval_selected_programs_committing(
            programs,
            point,
            seed,
            self.runtime.row_eval_context(),
            &mut |program, values, _| {
                let entry = next_in_selection_order(&mut entries, program)?;
                place_enabled_outputs(entry, values, enabled, out)
            },
        )
    }

    fn eval_jacobian_program_outputs(
        &self,
        program: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        self.jacobian_v.owner().eval_program_outputs(
            program,
            inputs,
            self.runtime.row_eval_context(),
            out,
        )
    }
}

/// The selection entry a sweep commits next, which must be `program`.
fn next_in_selection_order<'s>(
    entries: &mut impl Iterator<Item = &'s solve::ProjectionProgramOutputs>,
    program: usize,
) -> Result<&'s solve::ProjectionProgramOutputs, RuntimeSolveError> {
    entries
        .next()
        .filter(|entry| entry.program() == program)
        .ok_or_else(|| {
            RuntimeSolveError::solve_ir(
                "grouped Jacobian sweep committed outside its selection order",
            )
        })
}

/// Write the enabled placements of one program's outputs.
fn place_enabled_outputs(
    program: &solve::ProjectionProgramOutputs,
    values: &[f64],
    enabled: &[bool],
    out: &mut [f64],
) -> Result<(), RuntimeSolveError> {
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
    Ok(())
}
