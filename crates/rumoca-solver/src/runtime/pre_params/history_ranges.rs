//! Borrow the contiguous history ranges proved by event-iteration construction.

#[cfg(test)]
mod tests;

use super::*;
use crate::runtime::event_history::{EventIterationRange, advance_event_iteration_ranges};

struct HistoryRange<'a> {
    current: &'a [f64],
    destination: usize,
    value_kind: solve::EventIterationValueKind,
    fixed_point: bool,
}

pub(super) fn advance(
    model: &solve::SolveModel,
    source_y: &[f64],
    source_p: &[f64],
    params: &mut [f64],
) -> Result<bool, RuntimeSolveError> {
    let mut owners = Vec::with_capacity(model.problem.discrete.event_iteration_plan.runs.len());
    for (index, run) in model
        .problem
        .discrete
        .event_iteration_plan
        .runs
        .iter()
        .enumerate()
    {
        if let Some(range) = history_range(model, run, index, source_y, source_p, params)? {
            owners.push(range);
        }
    }
    owners.sort_by_key(|range| range.destination);
    let mut ranges = Vec::with_capacity(owners.len());
    let mut remaining = params;
    let mut cursor = 0;
    for owner in owners {
        let offset = owner
            .destination
            .checked_sub(cursor)
            .ok_or_else(|| RuntimeSolveError::solve_ir("event-iteration history ranges overlap"))?;
        let (_, tail) = remaining.split_at_mut(offset);
        let (pre, tail) = tail.split_at_mut(owner.current.len());
        remaining = tail;
        cursor = owner.destination + owner.current.len();
        ranges.push(EventIterationRange {
            current: owner.current,
            pre,
            value_kind: owner.value_kind,
            fixed_point: owner.fixed_point,
        });
    }
    advance_event_iteration_ranges(&mut ranges).map_err(|()| {
        RuntimeSolveError::solve_ir("event-iteration plan contains an invalid typed value")
    })
}

fn history_range<'a>(
    model: &solve::SolveModel,
    run: &solve::EventIterationRun,
    index: usize,
    source_y: &'a [f64],
    source_p: &'a [f64],
    params: &[f64],
) -> Result<Option<HistoryRange<'a>>, RuntimeSolveError> {
    let storage = &model.problem.solve_layout.variable_storage_runs[run.variable];
    let fixed_point = event_iteration_run_clock(model, run)?.is_none();
    if storage.scalar_count == 0 {
        return Ok(None);
    }
    let binding = model
        .problem
        .solve_layout
        .pre_param_bindings
        .get(run.pre_binding_start)
        .ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "event-iteration run {index} binding range is out of bounds"
            ))
        })?;
    let (source, base) = match binding.source {
        solve::PreParamSource::Y { index } => (source_y, index),
        solve::PreParamSource::P { index } => (source_p, index),
    };
    let current = checked_range(source, base, storage.scalar_count, index, "current")?;
    let pre = checked_range(
        params,
        binding.dest_p_index,
        storage.scalar_count,
        index,
        "pre",
    )?;
    let value_kind = storage
        .event_iteration_kind()
        .expect("validated event plan references a typed discrete variable");
    for (&current, &pre) in current.iter().zip(pre) {
        validate_event_iteration_value(value_kind, current, index, "current")?;
        validate_event_iteration_value(value_kind, pre, index, "pre")?;
    }
    Ok(Some(HistoryRange {
        current,
        destination: binding.dest_p_index,
        value_kind,
        fixed_point,
    }))
}

fn checked_range<'a>(
    values: &'a [f64],
    start: usize,
    count: usize,
    run: usize,
    kind: &str,
) -> Result<&'a [f64], RuntimeSolveError> {
    start
        .checked_add(count)
        .and_then(|end| values.get(start..end))
        .ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "event-iteration run {run} {kind} range is out of bounds"
            ))
        })
}
