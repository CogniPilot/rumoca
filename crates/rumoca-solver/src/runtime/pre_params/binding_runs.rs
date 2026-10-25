//! The `pre()` bindings of one model as contiguous copy runs, built once per
//! runtime, so the per-event seed and commit copy ranges instead of walking
//! every binding (Canyon Run has tens of thousands of agent `pre` lanes).

#[cfg(test)]
mod tests;

use super::*;

/// Ordinary `pre()` bindings grouped into runs of consecutive bindings with
/// contiguous destination and contiguous same-kind source. A scheduled
/// `previous()` binding is a run of its own, checked against its clock.
#[derive(Clone, Debug, Default)]
pub(crate) struct PreBindingPlan {
    runs: Vec<PreBindingRun>,
    /// Some binding reads, from `P`, another binding's destination. The commit
    /// then reads its post-event snapshot of `P` exactly as before.
    aliased: bool,
}

#[derive(Clone, Debug)]
struct PreBindingRun {
    /// The first binding of the run, in model order.
    first: usize,
    len: usize,
    dest: usize,
    source: solve::PreParamSource,
    scheduled: bool,
}

impl PreBindingRun {
    const fn source_start(&self) -> usize {
        match self.source {
            solve::PreParamSource::Y { index } | solve::PreParamSource::P { index } => index,
        }
    }

    /// Whether `binding` continues this run.
    fn extends_with(&self, binding: &solve::PreParamBinding) -> bool {
        if self.scheduled || binding.clock_schedule.is_some() {
            return false;
        }
        let same_kind = matches!(
            (&self.source, &binding.source),
            (
                solve::PreParamSource::Y { .. },
                solve::PreParamSource::Y { .. }
            ) | (
                solve::PreParamSource::P { .. },
                solve::PreParamSource::P { .. }
            )
        );
        let next_source = match binding.source {
            solve::PreParamSource::Y { index } | solve::PreParamSource::P { index } => index,
        };
        same_kind
            && self.dest.checked_add(self.len) == Some(binding.dest_p_index)
            && self.source_start().checked_add(self.len) == Some(next_source)
    }
}

impl PreBindingPlan {
    pub(crate) fn new(bindings: &[solve::PreParamBinding]) -> Self {
        let mut runs: Vec<PreBindingRun> = Vec::new();
        for (index, binding) in bindings.iter().enumerate() {
            if let Some(last) = runs.last_mut()
                && last.extends_with(binding)
            {
                last.len += 1;
                continue;
            }
            runs.push(PreBindingRun {
                first: index,
                len: 1,
                dest: binding.dest_p_index,
                source: binding.source.clone(),
                scheduled: binding.clock_schedule.is_some(),
            });
        }
        let destinations = bindings
            .iter()
            .map(|binding| binding.dest_p_index)
            .collect::<std::collections::HashSet<_>>();
        let aliased = bindings.iter().any(|binding| {
            matches!(binding.source, solve::PreParamSource::P { index } if destinations.contains(&index))
        });
        Self { runs, aliased }
    }

    /// `seed_event_entry_pre_params`, one copy per run.
    ///
    /// Bounds are checked for every ordinary binding before anything is
    /// written, and the first failing binding is reported with the same message.
    pub(crate) fn seed_event_entry(
        &self,
        event_pre_y: &[f64],
        event_pre_p: &[f64],
        params: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let ordinary = || self.runs.iter().filter(|run| !run.scheduled);
        for run in ordinary() {
            let source = match run.source {
                solve::PreParamSource::Y { .. } => event_pre_y,
                solve::PreParamSource::P { .. } => event_pre_p,
            };
            let source_ok = source.len().saturating_sub(run.source_start()).min(run.len);
            let dest_ok = params.len().saturating_sub(run.dest).min(run.len);
            if source_ok < run.len && source_ok <= dest_ok {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "event-entry pre binding {} source is out of bounds",
                    run.first + source_ok
                )));
            }
            if dest_ok < run.len {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "event-entry pre binding {} destination {} is out of bounds",
                    run.first + dest_ok,
                    run.dest + dest_ok
                )));
            }
        }
        for run in ordinary() {
            let source = match run.source {
                solve::PreParamSource::Y { .. } => event_pre_y,
                solve::PreParamSource::P { .. } => event_pre_p,
            };
            let start = run.source_start();
            params[run.dest..run.dest + run.len].copy_from_slice(&source[start..start + run.len]);
        }
        Ok(())
    }

    /// `commit_pre_params_after_event_at`, one loop per run and no copy of
    /// `P` unless a binding reads another binding's destination.
    pub(crate) fn commit_after_event_at(
        &self,
        model: &solve::SolveModel,
        y: &[f64],
        params: &mut [f64],
        event_time: Option<f64>,
        tol: f64,
    ) -> bool {
        if self.aliased {
            return commit_pre_params_after_event_at(model, y, params, event_time, tol);
        }
        let bindings = &model.problem.solve_layout.pre_param_bindings;
        let mut changed = false;
        for run in &self.runs {
            if run.scheduled
                && let (Some(schedule), Some(event_time)) =
                    (&bindings[run.first].clock_schedule, event_time)
                && !crate::timeline::periodic_schedule_matches_time(schedule, event_time)
            {
                continue;
            }
            let start = run.source_start();
            let dest_ok = params.len().saturating_sub(run.dest).min(run.len);
            match run.source {
                solve::PreParamSource::Y { .. } => {
                    let count = y.len().saturating_sub(start).min(dest_ok);
                    changed |= commit_lanes(params, run.dest, y, start, count, tol);
                }
                solve::PreParamSource::P { .. } => {
                    // Alias-free: no source lane is any binding's destination,
                    // so reading P in place sees the post-event values.
                    let count = params.len().saturating_sub(start).min(dest_ok);
                    changed |= commit_lanes_in_place(params, run.dest, start, count, tol);
                }
            }
        }
        changed
    }
}

/// One run's commit from `source`: lane `offset` of `params[dest..]` takes
/// `source[start + offset]`, in lane order, with the binding walk's change test.
fn commit_lanes(
    params: &mut [f64],
    dest: usize,
    source: &[f64],
    start: usize,
    count: usize,
    tol: f64,
) -> bool {
    let mut changed = false;
    for offset in 0..count {
        changed |= update_slot(&mut params[dest + offset], source[start + offset], tol);
    }
    changed
}

/// [`commit_lanes`] with `P` itself as the source. Only alias-free runs come
/// here, so no lane this run writes is one it reads.
fn commit_lanes_in_place(
    params: &mut [f64],
    dest: usize,
    start: usize,
    count: usize,
    tol: f64,
) -> bool {
    let mut changed = false;
    for offset in 0..count {
        let value = params[start + offset];
        changed |= update_slot(&mut params[dest + offset], value, tol);
    }
    changed
}
