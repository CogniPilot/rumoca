//! State demotion for derivatives no continuous equation reads (SPEC_0040
//! STRUCT-T04).
//!
//! The SPEC_0032 zero-coefficient proof omits from structural incidence a
//! factor multiplied by a coefficient proven exactly zero, and Solve lowering
//! omits the same product term. A state each of whose derivative scalars is
//! read only through such omitted terms has a derivative no equation
//! determines, so the system stays structurally singular for as long as the
//! variable stays a state, while the remaining equations determine its value
//! exactly as they determine an algebraic declaration's. One checked
//! reconstruction declares it algebraic and rebuilds each derivative
//! occurrence as the zero of its shape, the value its omitted product term
//! already contributed.
//!
//! A state is kept when no zero-coefficient proof covers some read of its
//! derivative (an initialization, event, assertion, or delay-source
//! expression reads it),
//! when it carries a stated initial value, which an algebraic declaration has
//! no equation for, or when it is a `StateSelect.always` request (MLS 3.7
//! §4.9.7.1).

use std::collections::BTreeSet;

use rumoca_core::StateSelect;
use rumoca_ir_dae as dae;

use crate::StructuralError;
use crate::types::UnknownId;

/// Declare algebraic every state of `model` whose derivative no continuous
/// equation reads.
///
/// Returns `None` when there is no such state, leaving the source untouched.
pub fn demote_inert_states(model: &dae::Dae) -> Result<Option<dae::Dae>, StructuralError> {
    let inert = model.inspect(inert_states)?;
    if inert.is_empty() {
        return Ok(None);
    }
    super::reconstruction::rebuild_inert_states(model, &inert).map(Some)
}

/// Declaration ordinals of the states whose every derivative scalar has an
/// empty incidence column and is read only by continuous owners.
fn inert_states(view: dae::DaeView<'_>) -> Result<Vec<u32>, StructuralError> {
    let incidence = crate::incidence::build_incidence(view)?;
    let mut read = vec![false; incidence.n_var];
    for row in &incidence.eq_unknowns {
        for &column in row {
            read[column] = true;
        }
    }
    let incident = incidence
        .unknowns
        .iter()
        .zip(&read)
        .filter_map(|(unknown, &read)| match unknown {
            UnknownId::Derivative { state, .. } if read => Some(state.index()),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    let stated = super::initial_pins::stated_initial_variables(view);
    let foreign = derivatives_read_outside_continuous_owners(view);
    Ok(view
        .variables()
        .filter(|(id, variable)| {
            variable.role() == dae::VariableRole::State
                && variable.scalar_count() > 0
                && variable.state_select() != StateSelect::Always
                && !incident.contains(&id.index())
                && !stated.contains(&id.index())
                && !foreign.contains(&id.index())
        })
        .map(|(id, _)| id.index())
        .collect())
}

/// States whose derivative an expression the continuous incidence does not
/// cover reads, or that an event reinitializes: an initialization equation, a
/// relation (and so every condition, zero crossing, and assertion guard built
/// on it), a discrete condition, a discrete equation or assignment, an
/// initialization-instant discrete value, a model-event definition, a
/// time-event deadline, an event action, or the source or delay time of a
/// delay of either kind. Incidence reads a delay as its own coordinate, so a
/// derivative inside a delay source has no incidence column even when a
/// continuous equation reads the delay.
fn derivatives_read_outside_continuous_owners(view: dae::DaeView<'_>) -> BTreeSet<u32> {
    let mut states = BTreeSet::new();
    let mut roots = Vec::new();
    for owner in view.initialization_owners() {
        match owner {
            dae::InitializationOwnerView::Residual { equation, .. } => {
                roots.push(equation.residual());
            }
            dae::InitializationOwnerView::Structured { family, .. } => {
                roots.extend(family.bodies().iter());
            }
        }
    }
    roots.extend(
        (0..view.relation_count())
            .filter_map(|index| view.relation(view.relation_id(index)?))
            .map(|relation| relation.expression()),
    );
    roots.extend(
        (0..view.structured_root_count())
            .filter_map(|index| view.structured_root(view.structured_root_id(index)?))
            .map(|root| root.expression()),
    );
    roots.extend(
        (0..view.discrete_real_equation_count())
            .filter_map(|index| view.discrete_real_equation(index))
            .map(|equation| equation.residual()),
    );
    for owner in (0..view.discrete_value_owner_count())
        .filter_map(|index| view.discrete_value_owner(view.discrete_value_owner_id(index)?))
    {
        for branch in owner.branches().iter() {
            roots.extend(branch.values().iter().map(|(value, _)| value));
        }
    }
    roots.extend(view.initial_discrete_values().map(|value| value.value()));
    for (_, transaction) in view.model_event_transactions() {
        for step in transaction.steps() {
            roots.extend(step.definitions().map(|definition| definition.value()));
        }
    }
    roots.extend(
        view.conditions()
            .filter_map(|(_, condition)| match condition.operation() {
                dae::ConditionOperation::Discrete(expression) => Some(expression),
                _ => None,
            }),
    );
    for index in 0..view.delay_count() {
        let id = view.delay_id(index).expect("dense delay identity resolves");
        let delay = view.delay(id).expect("checked delay identity resolves");
        roots.push(delay.source());
        if let dae::DelayOperation::BoundedDelay { delay_time, .. } = delay.operation() {
            roots.push(delay_time);
        }
    }
    for event in
        (0..view.time_event_count()).filter_map(|index| view.time_event(view.time_event_id(index)?))
    {
        if let dae::TimeEventOperation::Dynamic(deadline) = event.operation() {
            roots.push(deadline);
        }
    }
    for action in (0..view.event_action_count())
        .filter_map(|index| view.event_action(view.event_action_id(index)?))
    {
        match action.operation() {
            dae::EventActionOperation::Assert { message }
            | dae::EventActionOperation::Terminate { message }
            | dae::EventActionOperation::Print { message } => roots.push(message),
            dae::EventActionOperation::Warning { message, condition } => {
                roots.extend([message, condition]);
            }
            dae::EventActionOperation::Reinitialize { state, value } => {
                states.insert(state.index());
                roots.push(value);
            }
        }
    }
    dae::ExpressionTraversal::new().visit_pruned(view, roots, |_, node| {
        if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) =
            node.operation()
        {
            states.insert(state.index());
        }
        true
    });
    states
}
