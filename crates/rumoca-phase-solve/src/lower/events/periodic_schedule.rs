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
