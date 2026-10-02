//! MLS §8.6 discrete initial values lowered to initialization update rows.
//!
//! The DAE owner is a definition, not a residual: nothing solves for it, so it
//! becomes an assignment the runtime applies at the initialization instant
//! through `apply_initialization_updates`. The DAE constructor proved the value
//! calls no impure function and reads only `time`, parameters, constants, and
//! continuous coordinates. A continuous read is settled by the initialization
//! projection, so `settle_initialization_system` applies the update after it;
//! `initial_projection::prove_initial_definition_reads` proves here that the
//! projection settles those reads independently of every discrete value, so the
//! application after the projection is the fixed point the loop looks for.

use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::initial_projection::{self, InitializationUnknownSpace};
use super::{ScalarRows, variable_scalar_slot};
use crate::LowerError;
use crate::layout::LoweredLayout;
use crate::lower::scalar::ScalarCompiler;

pub(super) struct InitialDiscreteUpdates {
    pub(super) rows: ScalarRows,
    pub(super) targets: Vec<solve::ScalarSlot>,
}

/// Lower every MLS §8.6 discrete initial value into an initialization update row.
///
/// Each definition writes two slots. The coordinate's own slot is the value
/// itself. Its `pre` slot takes the same value because MLS §8.6 holds
/// `pre(v) = v` at the initialization instant: the `pre` slot is otherwise
/// carried from the declared `start` value, which would make a `when` whose
/// trigger reads `pre(v)` schedule against a value the initial algorithm has
/// already replaced.
pub(super) fn lower_initial_discrete_values<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    space: &InitializationUnknownSpace<'_, 'dae>,
) -> Result<InitialDiscreteUpdates, LowerError> {
    let mut rows = ScalarRows::default();
    let mut targets = Vec::new();
    for definition in view.initial_discrete_values() {
        let span = definition.provenance().span();
        let variable = definition.target().index();
        let scalar_count = layout
            .variables
            .get(variable as usize)
            .map(|entry| entry.count)
            .ok_or_else(|| LowerError::contract("variable has no Solve layout entry", span))?;
        let pre_base = initial_pre_base(layout, variable, span)?;
        // An array coordinate's definition is its whole aggregate; each scalar
        // writes its own lane and the matching lane of its `pre` storage.
        for scalar in 0..scalar_count {
            initial_projection::prove_initial_definition_reads(
                space,
                definition.value(),
                scalar,
                span,
            )?;
            let program =
                ScalarCompiler::new(view, layout, None).program(definition.value(), scalar)?;
            let current = variable_scalar_slot(layout, variable, scalar, span)?;
            let solve::ScalarSlot::P { .. } = current else {
                return Err(LowerError::contract(
                    "a discrete coordinate with an initial-algorithm value does not occupy \
                     parameter storage",
                    span,
                ));
            };
            let pre = pre_base
                .checked_add(scalar)
                .map(solve::scalar_slot_p)
                .ok_or_else(|| LowerError::contract("pre-value layout overflow", span))?;
            for target in [current, pre] {
                let output = rows.len();
                rows.push(program.clone(), span, output);
                targets.push(target);
            }
        }
    }
    Ok(InitialDiscreteUpdates { rows, targets })
}

/// The first parameter index of one discrete coordinate's `pre` storage.
///
/// Every discrete coordinate is given one by `append_pre_variables`, so a
/// missing slot is a layout contract failure rather than a coordinate without
/// history — skipping it would silently leave `pre(v)` at the declared `start`.
fn initial_pre_base(
    layout: &LoweredLayout<'_>,
    variable: u32,
    span: Span,
) -> Result<usize, LowerError> {
    layout
        .pre_variables
        .get(variable as usize)
        .copied()
        .flatten()
        .ok_or_else(|| {
            LowerError::contract(
                "a discrete coordinate with an initial-algorithm value has no lowered `pre` slot",
                span,
            )
        })
}
