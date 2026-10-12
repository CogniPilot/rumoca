//! Compact numeric event-memory producers (MLS §8.5).

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{DiscreteRows, EventCompilationContext, LowerError};

pub(super) fn lower_quotient_memory<'dae>(
    view: dae::DaeView<'dae>,
    layout: &EventCompilationContext<'_, 'dae>,
    rows: &mut DiscreteRows<'dae>,
) -> Result<(), LowerError> {
    for memory in layout.buffered_quotients.entries() {
        if memory.domain.is_some() {
            lower_structured_quotient_memory(view, layout, rows, *memory)?;
            continue;
        }
        let span = view
            .expression(memory.quotient)
            .expect("checked quotient expression")
            .provenance()
            .span();
        rows.push(
            layout
                .scalar_compiler(view, None)
                .quotient_memory_program(memory.quotient)?,
            span,
            solve::scalar_slot_p(memory.parameter),
            solve::DiscreteRowRole::QuotientMemory {
                step: memory.step(),
                root_index: memory.root_index,
            },
            solve::DiscreteEventPreMode::FollowCurrent,
            None,
        );
    }
    Ok(())
}

fn lower_structured_quotient_memory<'dae>(
    view: dae::DaeView<'dae>,
    layout: &EventCompilationContext<'_, 'dae>,
    rows: &mut DiscreteRows<'dae>,
    memory: crate::layout::QuotientMemory<'dae>,
) -> Result<(), LowerError> {
    if memory.count == 0 {
        return Ok(());
    }
    let node = layout
        .scalar_compiler(view, None)
        .quotient_memory_map(memory, rows.structured_output_cursor)?;
    let solve::ComputeNode::Map { domain, span, .. } = &node else {
        return Err(LowerError::contract(
            "quotient memory producer is not a Map",
            view.expression(memory.quotient)
                .expect("checked quotient expression")
                .provenance()
                .span(),
        ));
    };
    let target = solve::StructuredDiscreteTargetMap {
        base: solve::scalar_slot_p(memory.parameter),
        map: solve::TensorOutputMap::dense_contiguous(0, domain)
            .map_err(|error| LowerError::contract(format!("{error:?}"), *span))?,
    };
    rows.structured_updates
        .push(solve::StructuredDiscreteUpdate {
            node_index: rows.structured_rhs.nodes.len(),
            target,
            role: solve::DiscreteRowRole::QuotientMemory {
                step: memory.step(),
                root_index: memory.root_index,
            },
            pre_mode: solve::DiscreteEventPreMode::FollowCurrent,
            observation_refresh: false,
            integrator_history_effect: solve::IntegratorHistoryEffect::Restart,
            clock_owner: None,
        });
    let span = *span;
    rows.structured_rhs.nodes.push(node);
    rows.structured_output_cursor = rows
        .structured_output_cursor
        .checked_add(memory.count)
        .ok_or_else(|| LowerError::contract("quotient memory output overflow", span))?;
    Ok(())
}
