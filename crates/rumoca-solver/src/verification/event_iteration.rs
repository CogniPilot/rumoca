//! Bounded proof of the atomic ordinary-`pre` advance between whole event
//! equation passes.

use rumoca_ir_solve::EventIterationValueKind;

use crate::runtime::event_history::{EventIterationRange, advance_event_iteration_ranges};

const LANE_COUNT: usize = 3;

fn kind_from_index(index: u8) -> EventIterationValueKind {
    match index % 4 {
        0 => EventIterationValueKind::Real,
        1 => EventIterationValueKind::Integer,
        2 => EventIterationValueKind::Boolean,
        _ => EventIterationValueKind::Enumeration,
    }
}

fn typed_value(kind: EventIterationValueKind, numeric: i16, flag: bool) -> f64 {
    match kind {
        EventIterationValueKind::Real => f64::from(numeric) / 4.0,
        EventIterationValueKind::Integer => f64::from(numeric),
        EventIterationValueKind::Boolean => f64::from(flag),
        EventIterationValueKind::Enumeration => f64::from(numeric.unsigned_abs() % 8 + 1),
    }
}

/// # Property
///
/// A successful atomic event-history advance makes every ordinary fixed-point
/// lane exactly equal to its typed current value while leaving clocked or
/// excluded lanes unchanged. Invalid typed input rejects without changing any
/// lane.
///
/// MLS Appendix-B event iteration, registry row SIM-010.
fn property_atomic_fixed_point_pre_advance(
    kinds: [u8; LANE_COUNT],
    numerics: [i16; LANE_COUNT * 2],
    flags: [bool; LANE_COUNT * 3],
    inject_invalid: bool,
) {
    let mut current: [f64; LANE_COUNT] = std::array::from_fn(|index| {
        typed_value(kind_from_index(kinds[index]), numerics[index], flags[index])
    });
    let mut pre: [f64; LANE_COUNT] = std::array::from_fn(|index| {
        typed_value(
            kind_from_index(kinds[index]),
            numerics[LANE_COUNT + index],
            flags[LANE_COUNT + index],
        )
    });
    if inject_invalid {
        current[LANE_COUNT - 1] = f64::NAN;
    }
    let before = pre;
    let mut ranges = pre
        .iter_mut()
        .enumerate()
        .map(|(index, pre)| EventIterationRange {
            current: &current[index..index + 1],
            pre: std::slice::from_mut(pre),
            value_kind: kind_from_index(kinds[index]),
            fixed_point: flags[LANE_COUNT * 2 + index],
        })
        .collect::<Vec<_>>();
    let result = advance_event_iteration_ranges(&mut ranges);
    drop(ranges);
    #[cfg(kani)]
    kani::cover!(inject_invalid, "typed input rejection is reachable");
    if inject_invalid {
        assert!(result.is_err(), "an invalid typed range must reject");
        for (actual, expected) in pre.iter().zip(before) {
            assert_eq!(actual.to_bits(), expected.to_bits());
        }
        return;
    }
    let expected_changed = (0..LANE_COUNT)
        .any(|index| flags[LANE_COUNT * 2 + index] && current[index] != before[index]);
    #[cfg(kani)]
    kani::cover!(
        expected_changed,
        "a successful changing advance is reachable"
    );
    #[cfg(kani)]
    kani::cover!(
        !expected_changed,
        "a successful already-settled advance is reachable"
    );
    assert_eq!(result, Ok(expected_changed));
    for index in 0..LANE_COUNT {
        let expected = if flags[LANE_COUNT * 2 + index] {
            current[index]
        } else {
            before[index]
        };
        assert_eq!(pre[index].to_bits(), expected.to_bits());
    }
}

#[cfg(kani)]
mod proof {
    #[kani::proof]
    #[kani::unwind(16)]
    fn atomic_fixed_point_pre_advance_is_typed_and_transactional() {
        super::property_atomic_fixed_point_pre_advance(
            kani::any(),
            kani::any(),
            kani::any(),
            kani::any(),
        );
    }
}

#[cfg(all(test, not(kani)))]
mod fallback {
    use proptest::prelude::*;

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(64))]

        #[test]
        fn atomic_fixed_point_pre_advance_is_typed_and_transactional(
            kinds in any::<[u8; super::LANE_COUNT]>(),
            numerics in any::<[i16; super::LANE_COUNT * 2]>(),
            flags in any::<[bool; super::LANE_COUNT * 3]>(),
            inject_invalid in any::<bool>(),
        ) {
            super::property_atomic_fixed_point_pre_advance(
                kinds,
                numerics,
                flags,
                inject_invalid,
            );
        }
    }
}
