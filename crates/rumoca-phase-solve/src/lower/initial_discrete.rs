//! MLS §8.6 discrete initial values lowered to initialization update rows.
//!
//! The DAE owner is a definition, not a residual: nothing solves for it, so it
//! becomes an assignment the runtime applies at the initialization instant
//! through `apply_initialization_updates`. The DAE constructor already proved
//! the value reads only `time`, parameters, and constants and calls no impure
//! function — none of which any update row writes — so the first application is
//! already the fixed point `settle_initialization_system` looks for.

use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

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
) -> Result<InitialDiscreteUpdates, LowerError> {
    let mut rows = ScalarRows::default();
    let mut targets = Vec::new();
    for definition in view.initial_discrete_values() {
        let span = definition.provenance().span();
        let variable = definition.target().index();
        let program = ScalarCompiler::new(view, layout, None).program(definition.value(), 0)?;
        let current = variable_scalar_slot(layout, variable, 0, span)?;
        let solve::ScalarSlot::P { .. } = current else {
            return Err(LowerError::contract(
                "a discrete coordinate with an initial-algorithm value does not occupy \
                 parameter storage",
                span,
            ));
        };
        for target in [current, initial_pre_slot(layout, variable, span)?] {
            let output = rows.len();
            rows.push(program.clone(), span, output);
            targets.push(target);
        }
    }
    Ok(InitialDiscreteUpdates { rows, targets })
}

/// The `pre` slot of one discrete coordinate.
///
/// Every discrete coordinate is given one by `append_pre_variables`, so a
/// missing slot is a layout contract failure rather than a coordinate without
/// history — skipping it would silently leave `pre(v)` at the declared `start`.
fn initial_pre_slot(
    layout: &LoweredLayout<'_>,
    variable: u32,
    span: Span,
) -> Result<solve::ScalarSlot, LowerError> {
    layout
        .pre_variables
        .get(variable as usize)
        .copied()
        .flatten()
        .map(solve::scalar_slot_p)
        .ok_or_else(|| {
            LowerError::contract(
                "a discrete coordinate with an initial-algorithm value has no lowered `pre` slot",
                span,
            )
        })
}
