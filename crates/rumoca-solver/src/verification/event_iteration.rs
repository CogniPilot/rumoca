//! Bounded proof of the atomic ordinary-`pre` advance between whole event
//! equation passes.

use rumoca_ir_solve::EventIterationValueKind;

use crate::runtime::pre_params::{EventIterationLane, advance_event_iteration_lanes};

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
    let mut lanes: [EventIterationLane; LANE_COUNT] = std::array::from_fn(|index| {
        let kind = kind_from_index(kinds[index]);
        EventIterationLane {
            dest_p_index: index,
            current: typed_value(kind, numerics[index], flags[index]),
            pre: typed_value(
                kind,
                numerics[LANE_COUNT + index],
                flags[LANE_COUNT + index],
            ),
            value_kind: kind,
            fixed_point: flags[LANE_COUNT * 2 + index],
        }
    });
    if inject_invalid {
        lanes[LANE_COUNT - 1].current = f64::NAN;
    }
    let before = lanes;
    let result = advance_event_iteration_lanes(&mut lanes);
    if inject_invalid {
        assert!(result.is_err(), "an invalid typed lane must reject");
        for (actual, expected) in lanes.iter().zip(before) {
            assert_eq!(actual.current.to_bits(), expected.current.to_bits());
            assert_eq!(actual.pre.to_bits(), expected.pre.to_bits());
            assert_eq!(actual.dest_p_index, expected.dest_p_index);
            assert_eq!(actual.value_kind, expected.value_kind);
            assert_eq!(actual.fixed_point, expected.fixed_point);
        }
        return;
    }

    let expected_changed = before
        .iter()
        .any(|lane| lane.fixed_point && lane.current != lane.pre);
    assert_eq!(result, Ok(expected_changed));
    for (actual, expected) in lanes.iter().zip(before) {
        assert_eq!(actual.current.to_bits(), expected.current.to_bits());
        assert_eq!(actual.dest_p_index, expected.dest_p_index);
        assert_eq!(actual.value_kind, expected.value_kind);
        assert_eq!(actual.fixed_point, expected.fixed_point);
        if actual.fixed_point {
            assert_eq!(actual.current, actual.pre);
        } else {
            assert_eq!(actual.pre.to_bits(), expected.pre.to_bits());
        }
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
