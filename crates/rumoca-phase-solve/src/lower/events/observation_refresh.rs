//! Construction-issued observation refresh for event-pulse aliases.
//!
//! MLS event indicators such as `sample(start, interval)` are true while the
//! event is being processed and false on its observable right limit. Solve
//! represents their periodic leaves with hidden activation parameters. This
//! module derives the exact unclocked, history-free scalar rows that must be
//! recomputed when those leaves are projected to a public observation time.
//! An observed B.1c owner (SPEC_0022 EXPR-012), an unread discrete-valued
//! variable defined by a continuous-time expression, is a seed as well: its
//! rows are recomputed at every public observation.
//! Runtime receives only the resulting row-aligned proof; it never inspects
//! bytecode, names, or model provenance to rediscover this ownership.

use std::collections::BTreeSet;
use std::sync::Arc;

use rumoca_ir_solve as solve;

use super::HistoryDependencySlot;
use crate::LowerError;

#[derive(Debug)]
struct ObservationRow {
    row: usize,
    span: rumoca_core::Span,
    target: HistoryDependencySlot,
    reads_y: Arc<solve::IndexIntervals>,
    reads_p: Arc<solve::IndexIntervals>,
    safe: bool,
    seed: bool,
}

pub(super) fn derive_observation_refresh(
    discrete: &mut solve::DiscreteSolveSystem,
    clock_activation_parameters: &[usize],
    observed_rows: &[usize],
) -> Result<(), LowerError> {
    let activation_parameters =
        solve::IndexIntervals::of(clock_activation_parameters.iter().copied());
    let mut rows = observation_rows(discrete, &activation_parameters)?;
    let observed = observed_rows.iter().copied().collect::<BTreeSet<_>>();
    let mut seeded = 0usize;
    for row in rows.iter_mut().filter(|row| observed.contains(&row.row)) {
        if !row.safe {
            return Err(LowerError::contract(
                "an observed discrete row must be unclocked and follow its current value",
                row.span,
            ));
        }
        row.seed = true;
        seeded += 1;
    }
    if seeded != observed.len() {
        // The observed row that owns no scalar program has no span of its own.
        return Err(LowerError::unspanned_non_computable(
            "an observed discrete row has no scalar observation row",
        ));
    }
    let selected = select_refresh_closure(&rows);
    discrete.observation_refresh_reads_y = rows
        .iter()
        .zip(&selected)
        .any(|(row, selected)| *selected && !row.reads_y.is_empty());
    for (row, selected) in rows.iter().zip(selected) {
        discrete.observation_refresh[row.row] = selected;
    }
    Ok(())
}

fn observation_rows(
    discrete: &solve::DiscreteSolveSystem,
    activation_parameters: &solve::IndexIntervals,
) -> Result<Vec<ObservationRow>, LowerError> {
    let mut rows = Vec::with_capacity(discrete.update_targets.len());
    let mut stored_output = 0usize;
    for (program_index, program) in discrete.rhs.programs().iter().enumerate() {
        let span = discrete
            .rhs
            .program_span(program_index)
            .expect("checked scalar program has provenance");
        let y_dependencies =
            solve::StructuralPattern::derive_output_y_dependency_ranges(program, Some(span))
                .map_err(|error| {
                    LowerError::contract(
                        format!("cannot prove observation-refresh Y dependencies: {error}"),
                        span,
                    )
                })?;
        let p_dependencies =
            solve::StructuralPattern::derive_output_p_dependency_ranges(program, Some(span))
                .map_err(|error| {
                    LowerError::contract(
                        format!("cannot prove observation-refresh P dependencies: {error}"),
                        span,
                    )
                })?;
        if y_dependencies.len() != p_dependencies.len() {
            return Err(LowerError::contract(
                "observation-refresh dependency projections disagree on output count",
                span,
            ));
        }
        for (local_output, (y_dependencies, p_dependencies)) in
            y_dependencies.into_iter().zip(p_dependencies).enumerate()
        {
            let output_ordinal = stored_output.checked_add(local_output).ok_or_else(|| {
                LowerError::contract("observation-refresh output ordinal overflow", span)
            })?;
            let row = discrete
                .rhs
                .output_indices()
                .get(output_ordinal)
                .copied()
                .ok_or_else(|| {
                    LowerError::contract(
                        "observation-refresh output has no checked row identity",
                        span,
                    )
                })?;
            let target = discrete
                .update_targets
                .get(row)
                .copied()
                .and_then(dependency_slot)
                .ok_or_else(|| {
                    LowerError::contract(
                        "observation-refresh target is not runtime Y/P storage",
                        span,
                    )
                })?;
            let safe = discrete.clock_owners.get(row) == Some(&None)
                && discrete.pre_modes.get(row) == Some(&solve::DiscreteEventPreMode::FollowCurrent);
            let seed = !p_dependencies
                .intersection(activation_parameters)
                .is_empty();
            rows.push(ObservationRow {
                row,
                span,
                target,
                reads_y: y_dependencies,
                reads_p: p_dependencies,
                safe,
                seed,
            });
        }
        stored_output = stored_output
            .checked_add(solve::ScalarProgramBlock::program_output_count(program))
            .ok_or_else(|| {
                LowerError::contract("observation-refresh stored-output count overflow", span)
            })?;
    }
    Ok(rows)
}

fn dependency_slot(slot: solve::ScalarSlot) -> Option<HistoryDependencySlot> {
    match slot {
        solve::ScalarSlot::Y { index, .. } => Some(HistoryDependencySlot::Y(index)),
        solve::ScalarSlot::P { index, .. } => Some(HistoryDependencySlot::P(index)),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => None,
    }
}

fn select_refresh_closure(rows: &[ObservationRow]) -> Vec<bool> {
    let mut selected = rows
        .iter()
        .map(|row| row.safe && row.seed)
        .collect::<Vec<_>>();
    let mut active_y = solve::IndexIntervals::default();
    let mut active_p = solve::IndexIntervals::default();
    for (row, selected) in rows.iter().zip(&selected) {
        if *selected {
            insert_target(row.target, &mut active_y, &mut active_p);
        }
    }
    loop {
        let mut changed = false;
        for (index, row) in rows.iter().enumerate() {
            if selected[index] || !row.safe {
                continue;
            }
            let connected = !row.reads_y.intersection(&active_y).is_empty()
                || !row.reads_p.intersection(&active_p).is_empty();
            if connected {
                selected[index] = true;
                insert_target(row.target, &mut active_y, &mut active_p);
                changed = true;
            }
        }
        if !changed {
            return selected;
        }
    }
}

fn insert_target(
    target: HistoryDependencySlot,
    active_y: &mut solve::IndexIntervals,
    active_p: &mut solve::IndexIntervals,
) {
    match target {
        HistoryDependencySlot::Y(index) => active_y.insert(index),
        HistoryDependencySlot::P(index) => active_p.insert(index),
    }
}

#[cfg(test)]
mod tests;
