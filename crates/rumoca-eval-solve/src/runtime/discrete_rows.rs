use rumoca_solver::{RuntimeSolveError, discrete_row_pre_mode, row_reads_solver_or_time};

use crate::{self as solve_eval};

use super::SolveRuntime;
use super::event_update::{
    DiscretePreSnapshot, DiscreteRowsSettleInput, EventEvalParamCache, EventUpdateRowFilter,
};
use super::support::{copy_runtime_values, reserve_runtime_vec_capacity};
use crate::runtime_events::event_eval_params_with_relation_overrides;

#[derive(Clone, Copy)]
struct DiscreteRowEvalScope {
    skip_solver_or_time_rows: bool,
    observation_only: bool,
}

impl SolveRuntime {
    pub(super) fn settle_discrete_rows_for_pre_snapshot<P>(
        &self,
        snapshot: &DiscretePreSnapshot<'_>,
        input: &mut DiscreteRowsSettleInput<'_>,
        project_algebraics: &mut P,
    ) -> Result<bool, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        let mut changed_any = false;
        for _ in 0..input.max_iters {
            let mut pass_changed = self.apply_discrete_rows_for_pre_snapshot(
                snapshot,
                input.y,
                input.p,
                input.t,
                input.tol,
                DiscreteRowEvalScope {
                    skip_solver_or_time_rows: false,
                    observation_only: false,
                },
            )?;
            pass_changed |= self.apply_runtime_assignments_until_stable(
                input.y,
                input.p,
                input.t,
                input.tol,
                input.max_iters,
            )?;
            pass_changed |= project_algebraics(input.y, input.p)?;
            pass_changed |= self.apply_runtime_assignments_until_stable(
                input.y,
                input.p,
                input.t,
                input.tol,
                input.max_iters,
            )?;
            if !pass_changed {
                return Ok(changed_any);
            }
            changed_any = true;
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "discrete event equations did not converge at t={}",
            input.t
        )))
    }

    pub(super) fn apply_constant_discrete_rows_for_pre_snapshot(
        &self,
        snapshot: &DiscretePreSnapshot<'_>,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        self.apply_discrete_rows_for_pre_snapshot(
            snapshot,
            y,
            p,
            t,
            tol,
            DiscreteRowEvalScope {
                skip_solver_or_time_rows: true,
                observation_only: false,
            },
        )
    }

    fn apply_discrete_rows_for_pre_snapshot(
        &self,
        snapshot: &DiscretePreSnapshot<'_>,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        scope: DiscreteRowEvalScope,
    ) -> Result<bool, RuntimeSolveError> {
        self.validate_discrete_row_eval_scope(scope)?;
        let eval_y = copy_runtime_values(y, "discrete row eval y snapshot")?;
        let eval_p = copy_runtime_values(p, "discrete row eval p snapshot")?;
        let sources = snapshot.event_pre_sources();
        let mut eval_p_cache = EventEvalParamCache::default();
        let mut row_values = Vec::new();
        reserve_runtime_vec_capacity(
            &mut row_values,
            self.model.problem.discrete.rhs.programs.len(),
            "discrete row values",
        )?;
        for (row_idx, row) in self.model.problem.discrete.rhs.programs.iter().enumerate() {
            if scope.observation_only && !self.observation_refresh_row(row_idx)? {
                continue;
            }
            if scope.skip_solver_or_time_rows && row_reads_solver_or_time(row) {
                continue;
            }
            let row_pre_mode = discrete_row_pre_mode(&self.model, row_idx);
            if !snapshot.row_filter.accepts(row_pre_mode) {
                continue;
            }
            let row_p = eval_p_cache.params(&self.model, &eval_p, row_pre_mode, &sources, tol);
            let row_p_with_root_overrides;
            let row_p = if snapshot.root_relation_overrides.is_empty() {
                row_p
            } else {
                row_p_with_root_overrides = event_eval_params_with_relation_overrides(
                    &self.model.problem.events.root_relation_memory_targets,
                    snapshot.root_relation_overrides,
                    row_p,
                )?;
                &row_p_with_root_overrides
            };
            let value = self.discrete_rhs.eval_row_unchecked_with_context(
                row_idx,
                &eval_y,
                row_p,
                t,
                self.row_eval_context(),
            )?;
            row_values.push((self.model.problem.discrete.update_targets[row_idx], value));
        }
        self.override_relation_memory_row_values(snapshot.root_relation_overrides, &mut row_values);
        let mut changed = false;
        for (target, value) in row_values {
            changed |= solve_eval::apply_scalar_slot_value(target, value, y, p, tol)?;
        }
        Ok(changed)
    }

    pub fn refresh_observation_discrete_rows(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        if self.model.problem.discrete.observation_refresh.is_empty() {
            return Ok(false);
        }
        self.validate_observation_refresh_rows()?;
        let event_pre_y = copy_runtime_values(y, "observation event-pre y snapshot")?;
        let event_pre_p = copy_runtime_values(p, "observation event-pre p snapshot")?;
        let mut changed_any = false;
        for _ in 0..max_iters {
            let iter_pre_y = copy_runtime_values(y, "observation iteration y snapshot")?;
            let iter_pre_p = copy_runtime_values(p, "observation iteration p snapshot")?;
            let snapshot = DiscretePreSnapshot {
                event_pre_y: event_pre_y.as_slice(),
                event_pre_p: event_pre_p.as_slice(),
                iter_pre_y: iter_pre_y.as_slice(),
                iter_pre_p: iter_pre_p.as_slice(),
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: &[],
                // Preserve the existing observation-refresh policy: fixed
                // rows keep the observation-entry pre snapshot for the whole
                // refresh loop.
                event_iteration: 0,
            };
            let changed = self.apply_discrete_rows_for_pre_snapshot(
                &snapshot,
                y,
                p,
                t,
                tol,
                DiscreteRowEvalScope {
                    skip_solver_or_time_rows: false,
                    observation_only: true,
                },
            )?;
            if !changed {
                return Ok(changed_any);
            }
            changed_any = true;
        }
        Err(RuntimeSolveError::solve_ir(
            "observation-time discrete refresh did not converge",
        ))
    }

    fn validate_discrete_row_eval_scope(
        &self,
        scope: DiscreteRowEvalScope,
    ) -> Result<(), RuntimeSolveError> {
        if scope.observation_only {
            self.validate_observation_refresh_rows()?;
        }
        Ok(())
    }

    fn validate_observation_refresh_rows(&self) -> Result<(), RuntimeSolveError> {
        let observation_rows = self.model.problem.discrete.observation_refresh.len();
        let rhs_rows = self.model.problem.discrete.rhs.len();
        if observation_rows == rhs_rows {
            return Ok(());
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "discrete observation-refresh row count {observation_rows} does not match discrete RHS row count {rhs_rows}"
        )))
    }

    fn observation_refresh_row(&self, row_idx: usize) -> Result<bool, RuntimeSolveError> {
        self.model
            .problem
            .discrete
            .observation_refresh
            .get(row_idx)
            .copied()
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "discrete observation-refresh row index {row_idx} is out of bounds"
                ))
            })
    }
}
