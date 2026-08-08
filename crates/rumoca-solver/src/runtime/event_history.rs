use rumoca_ir_solve as solve;

#[derive(Clone, Copy, Debug)]
pub(crate) struct EventIterationLane {
    pub(crate) dest_p_index: usize,
    pub(crate) current: f64,
    pub(crate) pre: f64,
    pub(crate) value_kind: solve::EventIterationValueKind,
    pub(crate) fixed_point: bool,
}

/// Atomically advance one already-cataloged set of typed event-history lanes.
///
/// Validation completes before the first write. Fixed-point lanes advance to
/// their current value; clock-owned first-pass and excluded continuous lanes
/// remain unchanged.
pub(crate) fn advance_event_iteration_lanes(lanes: &mut [EventIterationLane]) -> Result<bool, ()> {
    if lanes.iter().any(|lane| {
        !event_iteration_value_is_valid(lane.value_kind, lane.current)
            || !event_iteration_value_is_valid(lane.value_kind, lane.pre)
    }) {
        return Err(());
    }
    let mut changed = false;
    for lane in lanes.iter_mut().filter(|lane| lane.fixed_point) {
        changed |= lane.current != lane.pre;
        lane.pre = lane.current;
    }
    Ok(changed)
}

pub(crate) fn event_iteration_value_is_valid(
    kind: solve::EventIterationValueKind,
    value: f64,
) -> bool {
    match kind {
        solve::EventIterationValueKind::Real => value.is_finite(),
        solve::EventIterationValueKind::Integer => value.is_finite() && value.fract() == 0.0,
        solve::EventIterationValueKind::Boolean => value == 0.0 || value == 1.0,
        solve::EventIterationValueKind::Enumeration => {
            value.is_finite() && value >= 1.0 && value.fract() == 0.0
        }
    }
}
