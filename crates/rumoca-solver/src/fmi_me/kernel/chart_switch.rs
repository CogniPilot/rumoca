//! Dynamic state selection of the ME component: accepted-step chart decisions
//! and the Event-Mode basis change (SPEC_0053 section 2a; SPEC_0040 STRUCT-T07
//! constraint-fold chart rows).

use super::*;

impl SolveMeKernel {
    /// Whether this component switches among reduced charts. Such a component
    /// refuses a trial point its active chart cannot certify as a recoverable
    /// discard, and leaves the switch to the next accepted step.
    pub(crate) fn switches_reduced_charts(&self) -> bool {
        self.reduced_charts.is_some()
    }

    /// Snapshot the mutable numerical state of every reduced-chart runtime, in
    /// chart index order. A model with no folding first-integral group has one
    /// runtime, so the vector is a single primary-basis snapshot.
    pub(super) fn chart_runtime_snapshots(
        &self,
    ) -> Vec<crate::runtime::solve_runtime::SolveRuntimeSnapshot> {
        match &self.reduced_charts {
            None => vec![self.runtime.snapshot()],
            Some(charts) => charts
                .runtimes
                .iter()
                .map(|runtime| runtime.snapshot())
                .collect(),
        }
    }

    /// Restore every reduced-chart runtime from a saved snapshot and rebind the
    /// active basis pointer to the saved chart.
    pub(super) fn restore_chart_runtimes(
        &mut self,
        active_chart: usize,
        snapshots: &[crate::runtime::solve_runtime::SolveRuntimeSnapshot],
    ) -> Result<(), MeError> {
        match &self.reduced_charts {
            None => {
                let snapshot = snapshots
                    .first()
                    .ok_or_else(|| contract("saved state carries no runtime snapshot"))?;
                self.runtime.restore(snapshot);
                self.active_chart = 0;
            }
            Some(charts) => {
                if snapshots.len() != charts.runtimes.len() {
                    return Err(contract(
                        "saved state carries a different number of chart runtimes",
                    ));
                }
                let active = charts.runtimes.get(active_chart).ok_or_else(|| {
                    contract("saved state names a chart index the component does not carry")
                })?;
                let active = Rc::clone(active);
                for (runtime, snapshot) in charts.runtimes.iter().zip(snapshots) {
                    runtime.restore(snapshot);
                }
                self.active_chart = active_chart;
                self.runtime = active;
            }
        }
        Ok(())
    }

    /// At an accepted step, estimate the conditioning of every reduced chart
    /// and, when the active one is approaching its fold while a strictly better
    /// conditioned regular alternate exists, latch a basis-change request whose
    /// coordinate is this step's settled full physical vector. Returns whether a
    /// request was latched. An active chart that settled below its regular region
    /// crossed its fold before a change could be requested: that is a typed
    /// failure, and no chart is adopted after the fact. A model with no chart set
    /// never enters this path, so its completed step is unchanged.
    pub(super) fn detect_basis_change_request(&mut self) -> Result<bool, MeError> {
        let Some(charts) = self.reduced_charts.as_ref() else {
            return Ok(false);
        };
        let t = self.continuous_eval_time();
        let settle = self.numerics_settle();
        // Warm-start the reconstruction from the last settled continuous vector
        // so the dependent first-integral coordinate stays on the physical
        // branch rather than the mirror root a cold declaration guess selects.
        let mut solver_y = self.solver_y_guess.borrow().clone();
        self.runtime
            .full_solver_y_with_guess(
                t,
                &self.states,
                &self.params,
                &mut solver_y,
                settle.tol,
                settle.max_iters,
            )
            .map_err(|error| MeError::from(error).at_stage(MeStage::Integration))?;
        let (decision, conditioning) =
            dynamic_chart::decide_at(charts, self.active_chart, t, &solver_y, &self.params)
                .map_err(|error| MeError::from(error).at_stage(MeStage::Integration))?;
        match decision {
            dynamic_chart::ChartDecision::Switch(target) => {
                // A switch is needless when the active chart is still far from
                // its fold: conditioning above a tenth of its construction-time
                // value. The event lets a sweep count switches per run.
                let active = conditioning[self.active_chart].rcond;
                let constructed = charts.charts[self.active_chart].trial_rcond;
                tracing::info!(
                    target: "rumoca_solver::chart_switch",
                    t,
                    from = self.active_chart,
                    to = target,
                    sigma_active = active,
                    sigma_target = conditioning[target].rcond,
                    sigma_constructed = constructed,
                    needless = active > 0.1 * constructed,
                    "reduced chart switch requested"
                );
                self.pending_basis_change = Some(dynamic_chart::PendingBasisChange {
                    target,
                    physical_solver_y: solver_y,
                });
                Ok(true)
            }
            dynamic_chart::ChartDecision::Keep => {
                self.pending_basis_change = None;
                Ok(false)
            }
            dynamic_chart::ChartDecision::Folded { sigma, regular } => Err(MeError::Evaluation {
                message: format!(
                    "reduced chart {} crossed its fold before a basis change could be requested at t={t}: its conditioning {sigma:.3e} is below its regular bound {regular:.3e}",
                    self.active_chart
                ),
            }
            .at_stage(MeStage::Integration)),
        }
    }

    /// Apply a latched basis change as one atomic Event-Mode transaction: swap
    /// the active basis, re-seed the generated state coordinates to the target
    /// chart from the pre-margin physical coordinate, re-establish the target
    /// chart's reconstruction with branch-limited certified projection, and
    /// rebind the coordinate map, integrator state, numerical caches, and
    /// rollback context to the one active basis. The target's transfer is
    /// computed completely before anything is rebound, so a failed transfer
    /// leaves the active chart, states, and caches exactly as they were and
    /// reports a typed error; it never becomes an accepted step or a partial
    /// switch.
    pub(super) fn run_basis_change_boundary(&mut self) -> Result<MeDiscreteStates, MeError> {
        let before = self.states.clone();
        let change = self
            .pending_basis_change
            .take()
            .ok_or_else(|| contract("basis-change boundary requires a latched basis change"))?;
        let target = change.target;
        let (target_runtime, binding_rows) = {
            let charts = self
                .reduced_charts
                .as_ref()
                .ok_or_else(|| contract("basis change requested without a reduced chart set"))?;
            let runtime = charts
                .runtimes
                .get(target)
                .ok_or_else(|| contract("basis change names an unknown chart index"))?;
            let chart = &charts.charts[target];
            (Rc::clone(runtime), chart.binding_rows.clone())
        };
        let t = self.continuous_eval_time();
        let solver_y = self.basis_transfer(&target_runtime, &binding_rows, &change, t)?;

        self.active_chart = target;
        self.runtime = target_runtime;
        self.copy_states_from_solver_y(&solver_y);
        *self.solver_y_guess.borrow_mut() = solver_y;
        self.clear_runtime_caches();
        self.invalidate_continuous_linearization();
        self.discrete_states_after_update(continuous_state_values_changed(&before, &self.states))
    }

    /// The full solver coordinate of a basis change, computed on the target
    /// chart's runtime without touching the component: the target's generated
    /// state values recovered from the latched physical coordinate, and every
    /// original constraint re-established by branch-limited certified projection
    /// at unchanged tolerances.
    fn basis_transfer(
        &self,
        target_runtime: &SolveRuntime,
        binding_rows: &[usize],
        change: &dynamic_chart::PendingBasisChange,
        t: f64,
    ) -> Result<Vec<f64>, MeError> {
        let physical = &change.physical_solver_y;
        // Recover the target chart's integrated source value for each generated
        // state coordinate from the identity residual `state - source`.
        let residuals = target_runtime
            .evaluate_implicit_residual_rows(t, physical, &self.params, binding_rows)
            .map_err(|error| MeError::from(error).at_stage(MeStage::EventIteration))?;
        let mut new_states = self.states.clone();
        for (state, residual) in new_states.iter_mut().zip(&residuals) {
            *state -= residual;
        }

        // Seed the transferred solve from the pre-margin physical coordinate so
        // the branch-limited certified projection stays on the physical branch
        // rather than the mirror root the fold shares.
        let mut solver_y = physical.clone();
        let prefix = solver_y
            .get_mut(..self.state_count)
            .ok_or_else(|| contract("physical coordinate is shorter than the state prefix"))?;
        prefix.copy_from_slice(&new_states);
        let settle = self.numerics_settle();
        target_runtime
            .refresh_algebraic_and_output_slots_certified(
                t,
                &mut solver_y,
                &self.params,
                settle.tol,
                settle.max_iters,
            )
            .map_err(|error| MeError::from(error).at_stage(MeStage::EventIteration))?;
        Ok(solver_y)
    }
}
