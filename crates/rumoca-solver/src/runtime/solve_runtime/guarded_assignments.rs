use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;

use crate::RuntimeSolveError;

use super::event_update::DiscretePreSnapshot;
use super::{InterpreterPermit, SolveRuntime};

impl SolveRuntime {
    pub(super) fn guarded_assignment_active_at(
        &self,
        program_index: usize,
        t: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let owner = self.guarded_assignment_owner(program_index)?;
        let Some(clock) = owner.clock_owner() else {
            return Ok(true);
        };
        self.periodic_clock_active(clock, t, "guarded assignment")
    }

    pub(super) fn guarded_assignment_accepts_snapshot(
        &self,
        program_index: usize,
        snapshot: &DiscretePreSnapshot<'_>,
        t: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let owner = self.guarded_assignment_owner(program_index)?;
        let clock_owned = owner.clock_owner().is_some();
        if clock_owned && snapshot.event_iteration != 0 {
            return Ok(false);
        }
        Ok(snapshot
            .row_filter
            .accepts(crate::EventPreMode::from(owner.pre_mode()), clock_owned)
            && self.guarded_assignment_active_at(program_index, t)?)
    }

    pub(super) fn eval_guarded_assignment_outputs(
        &self,
        program_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        let prepared = self
            .guarded_assignment_programs
            .get(program_index)
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "prepared guarded assignment program {program_index} is out of bounds"
                ))
            })?;
        prepared
            .eval_outputs_with_context(
                y,
                p,
                t,
                self.execution_plan
                    .interpreter
                    .guarded_assignments
                    .row_eval_context(self),
                out,
            )
            .map_err(Into::into)
    }

    pub(super) fn apply_guarded_assignment_outputs(
        &self,
        program_index: usize,
        values: &[f64],
        root_relation_overrides: &[(usize, f64)],
        y: &mut [f64],
        p: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let owner = self.guarded_assignment_owner(program_index)?;
        if values.len() != owner.output_count() {
            return Err(RuntimeSolveError::solve_ir(format!(
                "guarded assignment program {program_index} produced {} values for {} checked targets",
                values.len(),
                owner.output_count()
            )));
        }
        let mut changed = false;
        let mut output = 0usize;
        for range in owner.target_ranges() {
            for offset in 0..range.count() {
                let target = guarded_target_at(range.base(), offset)?;
                let value = guarded_relation_override(
                    &self.model.problem().events().root_relation_memory_targets,
                    root_relation_overrides,
                    target,
                )
                .unwrap_or(values[output]);
                tracing::trace!(
                    target: "rumoca_solver::guarded_assignments",
                    program_index,
                    target = ?target,
                    value,
                    "apply guarded assignment output"
                );
                changed |= solve_eval::apply_scalar_slot_value_exact(target, value, y, p)?;
                output += 1;
            }
        }
        Ok(changed)
    }

    fn guarded_assignment_owner(
        &self,
        program_index: usize,
    ) -> Result<&solve::GuardedAssignmentProgram, RuntimeSolveError> {
        self.model
            .problem()
            .discrete()
            .guarded_assignments
            .get(program_index)
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "guarded assignment program {program_index} is out of bounds"
                ))
            })
    }
}

pub(super) fn guarded_target_at(
    base: solve::ScalarSlot,
    offset: usize,
) -> Result<solve::ScalarSlot, RuntimeSolveError> {
    match base {
        solve::ScalarSlot::Y { index, .. } => index
            .checked_add(offset)
            .map(solve::scalar_slot_y)
            .ok_or_else(|| RuntimeSolveError::solve_ir("guarded Y target range overflowed")),
        solve::ScalarSlot::P { index, .. } => index
            .checked_add(offset)
            .map(solve::scalar_slot_p)
            .ok_or_else(|| RuntimeSolveError::solve_ir("guarded P target range overflowed")),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => Err(
            RuntimeSolveError::solve_ir("guarded target range is not mutable storage"),
        ),
    }
}

fn guarded_relation_override(
    relation_targets: &[Option<solve::ScalarSlot>],
    overrides: &[(usize, f64)],
    target: solve::ScalarSlot,
) -> Option<f64> {
    overrides.iter().find_map(|&(root, value)| {
        (relation_targets.get(root).copied().flatten() == Some(target)).then_some(value)
    })
}
