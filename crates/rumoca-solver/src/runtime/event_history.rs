#[cfg(test)]
mod tests;

use rumoca_ir_solve as solve;

#[derive(Debug)]
pub(crate) struct EventIterationRange<'a> {
    pub(crate) current: &'a [f64],
    pub(crate) pre: &'a mut [f64],
    pub(crate) value_kind: solve::EventIterationValueKind,
    pub(crate) fixed_point: bool,
}

/// Atomically advance one already-cataloged set of typed event-history lanes.
///
/// Validation completes before the first write. Fixed-point lanes advance to
/// their current value; clock-owned first-pass and excluded continuous lanes
/// remain unchanged. The runtime validates once at cataloguing and calls
/// [`copy_validated_event_iteration_ranges`]; this checked whole is the
/// reference the tests and the Kani proof compare it with.
#[cfg(any(test, kani))]
pub(crate) fn advance_event_iteration_ranges(
    ranges: &mut [EventIterationRange<'_>],
) -> Result<bool, ()> {
    if ranges.iter().any(|range| {
        range.current.len() != range.pre.len()
            || range
                .current
                .iter()
                .zip(range.pre.iter())
                .any(|(&current, &pre)| {
                    !event_iteration_value_is_valid(range.value_kind, current)
                        || !event_iteration_value_is_valid(range.value_kind, pre)
                })
    }) {
        return Err(());
    }
    Ok(copy_validated_event_iteration_ranges(ranges))
}

/// The write half of [`advance_event_iteration_ranges`], for ranges whose
/// extents and every value the caller has already validated before the first
/// write, as event-history cataloguing does. Fixed-point lanes advance to their
/// current value; the others remain unchanged.
pub(crate) fn copy_validated_event_iteration_ranges(
    ranges: &mut [EventIterationRange<'_>],
) -> bool {
    debug_assert!(ranges.iter().all(|range| {
        range.current.len() == range.pre.len()
            && range
                .current
                .iter()
                .chain(range.pre.iter())
                .all(|&value| event_iteration_value_is_valid(range.value_kind, value))
    }));
    let mut changed = false;
    for range in ranges.iter_mut().filter(|range| range.fixed_point) {
        for (current, pre) in range.current.iter().zip(range.pre.iter_mut()) {
            changed |= current != pre;
            *pre = *current;
        }
    }
    changed
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
