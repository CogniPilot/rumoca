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
//! Every carried state value stays an equation in the joint initialization
//! solve. A definition can still depend on an unknown parameter, so treating
//! it as a separately replayed assignment would destroy simultaneous coupling.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_phase_structural::{InitialValuePin, InitialValueRole};

use super::initial_parameters::InitializationParameterOwnership;
use super::initial_projection::InitialRowIncidence;
use super::{ScalarRows, variable_scalar_slot};
use crate::LowerError;
use crate::layout::LoweredLayout;
use crate::lower::scalar::ScalarCompiler;

/// The initialization rows one set of transferred pins lowers to.
#[derive(Default)]
pub(super) struct TransferredInitialValues<'dae> {
    /// Source-fixed starts proved independent of initialization unknowns.
    pub(super) given_state_indices: Vec<usize>,
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
                    .map(|expression| (expression, term.scalar as usize, term.negated))
                    .ok_or_else(|| {
                        LowerError::contract(
                            "a transferred initial value names an expression the prepared \
                             system does not have",
                            span,
                        )
                    })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let coordinate = view
            .variable_id(pin.coordinate as usize)
            .and_then(|id| view.variable(id))
            .ok_or_else(|| {
                LowerError::contract(
                    "a retained initial value names a variable the prepared system does not have",
                    span,
                )
            })?;
        // A term may read a parameter the initialization system re-derives, so
        // the row recomputes that binding instead of loading the seed the
        // parameter set stored before anything was solved.
        let compiler = ScalarCompiler::new(view, layout, None)
            .with_parameter_substitutions(ownership.substitutions());
        if pin.role == InitialValueRole::Definition && coordinate.role() != dae::VariableRole::State
        {
            return Err(LowerError::contract(
                "a transferred initial-value definition does not target a state",
                span,
            ));
        }
        let slot = variable_scalar_slot(layout, pin.coordinate, pin.scalar as usize, span)?;
        let solve::ScalarSlot::Y { index, .. } = slot else {
            return Err(LowerError::contract(
                "a retained continuous initial value does not occupy solver storage",
                span,
            ));
        };
        let incidence = match coordinate.identity() {
            dae::VariableIdentity::State(_) => InitialRowIncidence::StateValue {
                index,
                terms: terms
                    .iter()
                    .map(|(expression, scalar, _)| (*expression, *scalar))
                    .collect(),
            },
            dae::VariableIdentity::Algebraic(variable) => InitialRowIncidence::AlgebraicValue {
                variable,
                scalar: pin.scalar as usize,
                terms: terms
                    .iter()
                    .map(|(expression, scalar, _)| (*expression, *scalar))
                    .collect(),
            },
            _ => {
                return Err(LowerError::contract(
                    "a retained continuous initial value targets a non-continuous coordinate",
                    span,
                ));
            }
        };
        let program = compiler.slot_residual_program(slot, &terms, span)?;
        let output = lowered.checks.len();
        lowered.checks.push(program, span, output);
        lowered.check_incidence.push(incidence);
    }
    lower_unrepresented_fixed_continuous_reals(view, layout, ownership, pins, &mut lowered)?;
    Ok(lowered)
}

/// Lower continuous Real `fixed = true` equations that do not participate in
/// structural equality-class transfer.
///
/// MLS 3.6 section 8.6 contributes one equation per scalar coordinate. Real's
/// default `start` is exactly zero, so an absent attribute is not missing
/// information. A scalar start broadcasts over an aggregate; an aggregate
/// start retains its checked-DAE scalar order.
fn lower_unrepresented_fixed_continuous_reals<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    ownership: &InitializationParameterOwnership<'dae>,
    pins: &[InitialValuePin],
    lowered: &mut TransferredInitialValues<'dae>,
) -> Result<(), LowerError> {
    let mut projection_cache = rumoca_eval_dae::ScalarCoordinateProjectionCache::default();
    for (id, variable) in view.variables() {
        if variable.fixed() != Some(true)
            || variable.value_type().scalar_type() != dae::ScalarType::Real
            || !matches!(
                variable.role(),
                dae::VariableRole::State | dae::VariableRole::Algebraic | dae::VariableRole::Output
            )
        {
            continue;
        }
        let span = variable.declaration().span();
        let start = variable.start();
        let start_count = match start {
            Some(expression) => Some(
                view.expression(expression)
                    .ok_or_else(|| {
                        LowerError::contract(
                            "a fixed continuous Real names a missing start expression",
                            span,
                        )
                    })?
                    .value_type()
                    .scalar_count()
                    .unwrap_or(0),
            ),
            None => None,
        };
        if matches!(start_count, Some(0)) {
            return Err(LowerError::contract(
                "a fixed continuous Real has a start expression with no scalar values",
                span,
            ));
        }
        for scalar in 0..variable.scalar_count() {
            if pins
                .iter()
                .any(|pin| pin.source == id.index() && pin.source_scalar as usize == scalar)
            {
                continue;
            }
            let slot = variable_scalar_slot(layout, id.index(), scalar, span)?;
            let solve::ScalarSlot::Y { index, .. } = slot else {
                return Err(LowerError::contract(
                    "a fixed algebraic/output does not occupy solver storage",
                    span,
                ));
            };
            let start = start.map(|expression| {
                let start_scalar = broadcast_start_scalar(start_count, scalar);
                (expression, start_scalar)
            });
            if variable.role() == dae::VariableRole::State
                && super::initial_given_states::is_known(
                    view,
                    ownership,
                    start,
                    &mut projection_cache,
                )
            {
                lowered.given_state_indices.push(index);
                continue;
            }
            let compiler = ScalarCompiler::new(view, layout, None)
                .with_parameter_substitutions(ownership.substitutions());
            let program = compiler.slot_start_residual_program(slot, start, span)?;
            let output = lowered.checks.len();
            lowered.checks.push(program, span, output);
            let incidence = match variable.identity() {
                dae::VariableIdentity::State(_) => InitialRowIncidence::StateValue {
                    index,
                    terms: start.into_iter().collect(),
                },
                dae::VariableIdentity::Algebraic(variable) => InitialRowIncidence::AlgebraicValue {
                    variable,
                    scalar,
                    terms: start.into_iter().collect(),
                },
                _ => {
                    return Err(LowerError::contract(
                        "fixed continuous value has no continuous identity",
                        span,
                    ));
                }
            };
            lowered.check_incidence.push(incidence);
        }
    }
    Ok(())
}

fn broadcast_start_scalar(start_count: Option<usize>, scalar: usize) -> usize {
    if start_count == Some(1) { 0 } else { scalar }
}
