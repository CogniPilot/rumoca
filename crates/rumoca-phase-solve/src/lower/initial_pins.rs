//! Stated initial values the structural proof carried onto another coordinate.
//!
//! MLS 3.6 §8.6 turns every `fixed = true` start into an initialization
//! equation. A runtime that seeds one value per state and then projects the
//! algebraic unknowns answers that equation for a pinned *state* and drops it
//! for a pinned coordinate the states determine — an aliased or displaced
//! position, say. The structural phase proves which state such a pin fixes and
//! hands the value over as a signed sum of time-invariant terms
//! (`rumoca_phase_structural::InitialValuePin`).
//!
//! What that proof establishes decides the row it becomes. A value proved to
//! *define* the state it names lowers to an initialization update row: the
//! solving is already done, so the runtime assigns it. A value the proof could
//! only place beside another stated one — the two agreeing exactly when some
//! parameter does — lowers to an initialization *residual* instead, so the
//! initialization instant answers with numbers what this phase cannot answer
//! with expressions. Both rows name the same state, which is what makes the
//! residual answerable: the state holds a value from the moment it is seeded.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_phase_structural::{InitialValuePin, InitialValueRole};

use super::initial_parameters::{InitialRowIncidence, InitializationParameterOwnership};
use super::{ScalarRows, variable_scalar_slot};
use crate::LowerError;
use crate::layout::LoweredLayout;
use crate::lower::scalar::ScalarCompiler;

/// The initialization rows one set of transferred pins lowers to.
#[derive(Default)]
pub(super) struct TransferredInitialValues<'dae> {
    /// Assignments the runtime applies at the initialization instant.
    pub(super) updates: ScalarRows,
    /// The slot each update row writes.
    pub(super) update_targets: Vec<solve::ScalarSlot>,
    /// Residuals the initialization instant has to satisfy.
    pub(super) checks: ScalarRows,
    /// What each check row reads, positionally paired with `checks`, so the
    /// parameter projection can plan a row that determines an unknown.
    pub(super) check_incidence: Vec<InitialRowIncidence<'dae>>,
}

/// Lower every transferred initial value into the row its proof allows.
pub(super) fn lower_transferred_initial_values<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    ownership: &InitializationParameterOwnership<'dae>,
    pins: &[InitialValuePin],
) -> Result<TransferredInitialValues<'dae>, LowerError> {
    let mut lowered = TransferredInitialValues::default();
    for pin in pins {
        let span = pin.provenance.span();
        let terms = pin
            .value
            .iter()
            .map(|term| {
                view.expression_id(term.expression as usize)
                    .map(|expression| (expression, term.negated))
                    .ok_or_else(|| {
                        LowerError::contract(
                            "a transferred initial value names an expression the prepared \
                             system does not have",
                            span,
                        )
                    })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let slot = variable_scalar_slot(layout, pin.coordinate, 0, span)?;
        let solve::ScalarSlot::Y { .. } = slot else {
            return Err(LowerError::contract(
                "a state carrying a transferred initial value does not occupy solver storage",
                span,
            ));
        };
        // A term may read a parameter the initialization system re-derives, so
        // the row recomputes that binding instead of loading the seed the
        // parameter set stored before anything was solved.
        let compiler = ScalarCompiler::new(view, layout, None)
            .with_parameter_substitutions(ownership.substitutions());
        match pin.role {
            InitialValueRole::Definition => {
                let program = compiler.signed_sum_program(&terms, span)?;
                let output = lowered.updates.len();
                lowered.updates.push(program, span, output);
                lowered.update_targets.push(slot);
            }
            InitialValueRole::Check => {
                let program = compiler.slot_residual_program(slot, &terms, span)?;
                let output = lowered.checks.len();
                lowered.checks.push(program, span, output);
                lowered
                    .check_incidence
                    .push(InitialRowIncidence::CarriedValue(
                        terms.iter().map(|(expression, _)| *expression).collect(),
                    ));
            }
        }
    }
    Ok(lowered)
}
