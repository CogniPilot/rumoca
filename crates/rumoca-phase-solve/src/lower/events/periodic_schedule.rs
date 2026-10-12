//! A Boolean sample schedule belongs to a whole ordinary event target.

use super::*;

pub(super) fn is_ordinary_target<'dae>(
    clocks: &LoweredClocks<'dae>,
    variable: dae::VariableId<'dae>,
) -> bool {
    clocks.variable_owner(variable).is_none() && clocks.variable_trigger(variable).is_none()
}

pub(super) fn common_periodic_clock<'dae>(
    view: dae::DaeView<'dae>,
    candidates: impl IntoIterator<Item = Option<dae::ClockId<'dae>>>,
) -> Option<dae::ClockId<'dae>> {
    let mut candidates = candidates.into_iter();
    let clock = candidates.next().flatten()?;
    if !matches!(
        view.clock(clock)?.operation(),
        dae::ClockOperation::Periodic(_)
    ) {
        return None;
    }
    candidates
        .all(|candidate| candidate == Some(clock))
        .then_some(clock)
}

pub(super) fn ordinary_update_clocks<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    updates: &[EventUpdate<'dae>],
) -> BTreeMap<u32, Option<dae::ClockId<'dae>>> {
    let mut schedules = BTreeMap::new();
    for update in updates {
        if !is_ordinary_target(clocks, update.variable) {
            continue;
        }
        schedules
            .entry(update.variable.index())
            .and_modify(|prior| *prior = common_periodic_clock(view, [*prior, update.clock]))
            .or_insert_with(|| common_periodic_clock(view, [update.clock]));
    }
    schedules
}

pub(super) fn checked_discrete_real_activation_clock<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    variable: dae::VariableId<'dae>,
    guard: dae::ConditionId<'dae>,
    span: Span,
) -> Result<Option<dae::ClockId<'dae>>, LowerError> {
    let guard_view = view.condition(guard).ok_or_else(|| {
        LowerError::contract("discrete Real activation has no checked condition", span)
    })?;
    let activation = condition_memory::checked_condition_clock_owner(view, guard_view);
    let owner = clocks.variable_owner(variable).map(|(clock, _)| clock);
    match (activation, owner) {
        (None, None) => Ok(None),
        (Some(activation), Some(owner)) if activation == owner => Ok(Some(owner)),
        (Some(activation), None) if guard_view.synchronous_clock_owner().is_none() => {
            // A checked Boolean periodic task is an MLS §8.5 schedule;
            // it does not mint MLS §16 synchronous variable ownership.
            Ok(Some(activation))
        }
        (Some(_), None) => Err(LowerError::non_computable(
            "clock-activated discrete Real definition has no matching target clock owner",
            span,
        )),
        (None, Some(_)) => Err(LowerError::non_computable(
            "clock-owned discrete Real target has a non-clock event activation",
            span,
        )),
        (Some(_), Some(_)) => Err(LowerError::non_computable(
            "discrete Real definition activation does not match its target clock owner",
            span,
        )),
    }
}
