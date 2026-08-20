//! Construction-issued observation refresh for event-pulse aliases.
//!
//! MLS event indicators such as `sample(start, interval)` are true while the
//! event is being processed and false on its observable right limit. Solve
//! represents their periodic leaves with hidden activation parameters. This
//! module derives the exact unclocked, history-free scalar rows that must be
//! recomputed when those leaves are projected to a public observation time.
//! Runtime receives only the resulting row-aligned proof; it never inspects
//! bytecode, names, or model provenance to rediscover this ownership.

use std::collections::BTreeSet;

use rumoca_ir_solve as solve;

use super::HistoryDependencySlot;
use crate::LowerError;

#[derive(Debug)]
struct ObservationRow {
    row: usize,
    target: HistoryDependencySlot,
    reads: BTreeSet<HistoryDependencySlot>,
    safe: bool,
    seed: bool,
}

pub(super) fn derive_observation_refresh(
    discrete: &mut solve::DiscreteSolveSystem,
    clock_activation_parameters: &[usize],
) -> Result<(), LowerError> {
    let activation_parameters = clock_activation_parameters
        .iter()
        .copied()
        .map(HistoryDependencySlot::P)
        .collect::<BTreeSet<_>>();
    let rows = observation_rows(discrete, &activation_parameters)?;
    let selected = select_refresh_closure(&rows);
    discrete.observation_refresh_reads_y = rows.iter().zip(&selected).any(|(row, selected)| {
        *selected
            && row
                .reads
                .iter()
                .any(|dependency| matches!(dependency, HistoryDependencySlot::Y(_)))
    });
    for (row, selected) in rows.iter().zip(selected) {
        discrete.observation_refresh[row.row] = selected;
    }
    Ok(())
}

fn observation_rows(
    discrete: &solve::DiscreteSolveSystem,
    activation_parameters: &BTreeSet<HistoryDependencySlot>,
) -> Result<Vec<ObservationRow>, LowerError> {
    let mut rows = Vec::with_capacity(discrete.update_targets.len());
    let mut stored_output = 0usize;
    for (program_index, program) in discrete.rhs.programs().iter().enumerate() {
        let span = discrete
            .rhs
            .program_span(program_index)
            .expect("checked scalar program has provenance");
        let y_dependencies =
            solve::StructuralPattern::derive_output_y_dependencies(program, Some(span)).map_err(
                |error| {
                    LowerError::contract(
                        format!("cannot prove observation-refresh Y dependencies: {error}"),
                        span,
                    )
                },
            )?;
        let p_dependencies =
            solve::StructuralPattern::derive_output_p_dependencies(program, Some(span)).map_err(
                |error| {
                    LowerError::contract(
                        format!("cannot prove observation-refresh P dependencies: {error}"),
                        span,
                    )
                },
            )?;
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
            let reads = y_dependencies
                .into_iter()
                .map(HistoryDependencySlot::Y)
                .chain(p_dependencies.iter().copied().map(HistoryDependencySlot::P))
                .collect::<BTreeSet<_>>();
            let safe = discrete.clock_owners.get(row) == Some(&None)
                && discrete.pre_modes.get(row) == Some(&solve::DiscreteEventPreMode::FollowCurrent);
            let seed = !p_dependencies
                .into_iter()
                .map(HistoryDependencySlot::P)
                .collect::<BTreeSet<_>>()
                .is_disjoint(activation_parameters);
            rows.push(ObservationRow {
                row,
                target,
                reads,
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
    loop {
        let mut changed = false;
        for (index, row) in rows.iter().enumerate() {
            if selected[index] || !row.safe {
                continue;
            }
            let connected = rows
                .iter()
                .zip(&selected)
                .any(|(active, selected)| *selected && row.reads.contains(&active.target));
            if connected {
                selected[index] = true;
                changed = true;
            }
        }
        if !changed {
            return selected;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn closure_selects_safe_consumers_but_not_upstream_or_history_rows() {
        let rows = vec![
            ObservationRow {
                row: 0,
                target: HistoryDependencySlot::P(9),
                reads: BTreeSet::from([HistoryDependencySlot::P(200)]),
                safe: true,
                seed: false,
            },
            ObservationRow {
                row: 1,
                target: HistoryDependencySlot::P(10),
                reads: BTreeSet::from([HistoryDependencySlot::P(9), HistoryDependencySlot::P(100)]),
                safe: true,
                seed: true,
            },
            ObservationRow {
                row: 2,
                target: HistoryDependencySlot::P(11),
                reads: BTreeSet::from([HistoryDependencySlot::P(10)]),
                safe: true,
                seed: false,
            },
            ObservationRow {
                row: 3,
                target: HistoryDependencySlot::P(12),
                reads: BTreeSet::from([HistoryDependencySlot::P(11)]),
                safe: false,
                seed: false,
            },
        ];

        assert_eq!(select_refresh_closure(&rows), [false, true, true, false]);
    }
}
