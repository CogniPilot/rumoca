use super::*;

#[test]
fn invalid_final_element_preserves_every_range() {
    let mut first = [1.0, 2.0];
    let mut last = [3.0, 4.0];
    let mut ranges = [
        EventIterationRange {
            current: &[5.0, 6.0],
            pre: &mut first,
            value_kind: solve::EventIterationValueKind::Real,
            fixed_point: true,
        },
        EventIterationRange {
            current: &[7.0, f64::NAN],
            pre: &mut last,
            value_kind: solve::EventIterationValueKind::Real,
            fixed_point: false,
        },
    ];
    assert!(advance_event_iteration_ranges(&mut ranges).is_err());
    assert_eq!(first, [1.0, 2.0]);
    assert_eq!(last, [3.0, 4.0]);
}

#[test]
fn range_extent_mismatch_rejects_before_other_ranges_advance() {
    let mut first = [1.0];
    let mut last = [2.0];
    let mut ranges = [
        EventIterationRange {
            current: &[3.0],
            pre: &mut first,
            value_kind: solve::EventIterationValueKind::Real,
            fixed_point: true,
        },
        EventIterationRange {
            current: &[],
            pre: &mut last,
            value_kind: solve::EventIterationValueKind::Real,
            fixed_point: true,
        },
    ];
    assert!(advance_event_iteration_ranges(&mut ranges).is_err());
    assert_eq!(first, [1.0]);
    assert_eq!(last, [2.0]);
}

#[test]
fn numeric_equality_preserves_signed_zero_transfer_and_clock_holds() {
    let mut ordinary = [0.0, 1.0];
    let mut clocked = [2.0, -0.0];
    let mut ranges = [
        EventIterationRange {
            current: &[-0.0, 1.0],
            pre: &mut ordinary,
            value_kind: solve::EventIterationValueKind::Real,
            fixed_point: true,
        },
        EventIterationRange {
            current: &[3.0, 0.0],
            pre: &mut clocked,
            value_kind: solve::EventIterationValueKind::Real,
            fixed_point: false,
        },
    ];
    assert_eq!(advance_event_iteration_ranges(&mut ranges), Ok(false));
    assert_eq!(
        ordinary.map(f64::to_bits),
        [(-0.0_f64).to_bits(), 1.0_f64.to_bits()]
    );
    assert_eq!(
        clocked.map(f64::to_bits),
        [2.0_f64.to_bits(), (-0.0_f64).to_bits()]
    );
}

#[test]
fn the_validated_copy_writes_exactly_what_the_checked_advance_writes() {
    let currents: [&[f64]; 2] = [&[-0.0, 4.0, 5.0], &[1.0, 0.0]];
    let (start_pre, start_clocked) = ([0.0, 4.0, 2.0], [7.0, 1.0]);
    let lanes = |pre: &[[f64; 3]; 2], clocked: &[f64; 2]| -> (Vec<u64>, Vec<u64>) {
        (
            pre[0].map(f64::to_bits).to_vec(),
            clocked.map(f64::to_bits).to_vec(),
        )
    };
    let mut checked = ([start_pre, [0.0; 3]], start_clocked);
    let mut copied = checked;
    let checked_changed = {
        let mut ranges = [
            EventIterationRange {
                current: currents[0],
                pre: &mut checked.0[0],
                value_kind: solve::EventIterationValueKind::Real,
                fixed_point: true,
            },
            EventIterationRange {
                current: currents[1],
                pre: &mut checked.1,
                value_kind: solve::EventIterationValueKind::Real,
                fixed_point: false,
            },
        ];
        advance_event_iteration_ranges(&mut ranges)
    };
    let copied_changed = {
        let mut ranges = [
            EventIterationRange {
                current: currents[0],
                pre: &mut copied.0[0],
                value_kind: solve::EventIterationValueKind::Real,
                fixed_point: true,
            },
            EventIterationRange {
                current: currents[1],
                pre: &mut copied.1,
                value_kind: solve::EventIterationValueKind::Real,
                fixed_point: false,
            },
        ];
        copy_validated_event_iteration_ranges(&mut ranges)
    };
    assert_eq!(checked_changed, Ok(copied_changed));
    assert!(copied_changed, "the third lane changes");
    assert_eq!(lanes(&checked.0, &checked.1), lanes(&copied.0, &copied.1));
}
