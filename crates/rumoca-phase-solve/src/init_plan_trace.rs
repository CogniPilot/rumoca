//! Debug trace of the lowered initialization projection plan.
//!
//! The runtime's initialization projection must satisfy the COMPLETE initial
//! residual system, but the plan itself only names the rows the projection
//! matching covered. When a model fails with "initial variable projection did
//! not satisfy the complete residual system", the first question is always
//! which rows the plan left uncovered and which unknown each covered row was
//! matched to. Recovering that from `--emit solve-json` is not possible for the
//! simulation pipeline, because `--emit` lowers the DAE before the structural
//! preparation `sim` runs, so the row numbering differs. This trace reports the
//! plan the simulation actually uses.
//!
//! Enable with `--trace=rumoca_phase_solve::init_plan`.

use rumoca_ir_solve as solve;

const TARGET: &str = "rumoca_phase_solve::init_plan";

/// Log per-row plan coverage and the target/unknown pairing for each row.
pub(crate) fn trace_initialization_plan(
    layout: &solve::VarLayout,
    row_targets: &[Option<solve::ScalarSlot>],
    plan: &solve::InitializationProjectionPlan,
    residual_len: usize,
) {
    if !tracing::enabled!(target: TARGET, tracing::Level::DEBUG) {
        return;
    }
    let plan_rows = plan
        .blocks
        .iter()
        .map(|block| block.rows.len())
        .sum::<usize>();
    let plan_unknowns = plan
        .blocks
        .iter()
        .map(|block| block.unknowns.len())
        .sum::<usize>();
    tracing::debug!(
        target: TARGET,
        residual_len,
        row_targets = row_targets.len(),
        plan_rows,
        plan_unknowns,
        blocks = plan.blocks.len(),
        "initialization projection plan"
    );
    for (block_idx, block) in plan.blocks.iter().enumerate() {
        tracing::debug!(
            target: TARGET,
            block = block_idx,
            rows = ?block.rows,
            unknowns = ?block
                .unknowns
                .iter()
                .map(|slot| slot_label(layout, *slot))
                .collect::<Vec<_>>(),
            "initialization projection block"
        );
    }
    for row in 0..residual_len {
        let covered = plan.blocks.iter().any(|block| block.rows.contains(&row));
        let target = row_targets
            .get(row)
            .copied()
            .flatten()
            .map_or_else(|| "<none>".to_string(), |slot| slot_label(layout, slot));
        tracing::debug!(
            target: TARGET,
            row,
            covered,
            target,
            "initialization residual row"
        );
    }
}

fn slot_label(layout: &solve::VarLayout, slot: solve::ScalarSlot) -> String {
    let name = layout
        .bindings()
        .iter()
        .find_map(|(name, candidate)| (*candidate == slot).then(|| name.clone()));
    match name {
        Some(name) => format!("{slot:?} {name}"),
        None => format!("{slot:?}"),
    }
}
