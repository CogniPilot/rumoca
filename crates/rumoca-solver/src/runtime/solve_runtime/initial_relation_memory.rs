//! Relation truth at the current simultaneous initialization solution.

use super::*;

impl ScalarExecutionView<'_> {
    pub(super) fn refresh_initial_relation_memory(
        &self,
        t: f64,
        y: &mut [f64],
        p: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        if self
            .model
            .problem
            .events
            .root_relation_memory_targets
            .iter()
            .all(Option::is_none)
        {
            return Ok(false);
        }
        // The no-continuation projection can leave algebraics unvisited. Settle
        // the issued root cone on this same geometry; reconstructing from state
        // starts would discard a root that homotopy has already selected.
        let before = support::copy_runtime_values(y, "initial relation geometry")?;
        self.refresh_slots_with_plan(
            &self.scalar.root_refresh,
            RefreshSlotArgs {
                t,
                solver_y: y,
                params: p,
                tol,
                max_iters,
                certify_coordinates: true,
            },
        )?;
        let roots = self.eval_root_conditions_from_solver_y(t, y, p)?;
        let memory_changed = self.update_root_relation_memory_from_values(&roots, p, &[])?;
        Ok(memory_changed || crate::runtime_values_changed(&before, y, tol))
    }
}
