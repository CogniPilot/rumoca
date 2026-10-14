use super::SlidingModes;
use crate::fmi::RootLocationPlan;

const RESOLUTION: f64 = 0.125;

fn record(modes: &mut SlidingModes, time: f64, changed: &[usize]) {
    let plan = RootLocationPlan::STANDARD;
    modes.begin_interval();
    modes.record_event(
        time,
        changed,
        plan.sliding_gap(RESOLUTION),
        plan.sliding_streak(),
    );
}

/// Re-crossings 1e-8 apart, as a sliding anti-windup limit produces them.
fn slide(modes: &mut SlidingModes, start: f64, events: u32, indicator: usize) {
    for event in 0..events {
        record(modes, start + f64::from(event) * 1.0e-8, &[indicator]);
    }
}

#[test]
fn a_streak_of_immediate_re_crossings_slides() {
    let streak = RootLocationPlan::STANDARD.sliding_streak();
    let mut modes = SlidingModes::new(2);
    slide(&mut modes, 1.0, streak - 1, 1);
    assert!(!modes.is_sliding(1));
    slide(&mut modes, 2.0e-7 + 1.0, 1, 1);
    assert!(modes.is_sliding(1));
    assert!(!modes.is_sliding(0));
    assert!(modes.only_sliding(&[1]));
    assert!(!modes.only_sliding(&[0, 1]));
    assert!(!modes.only_sliding(&[]));
}

/// A 1 kHz switch: far faster than the scan resolution, far slower than a
/// sliding mode.
#[test]
fn fast_periodic_switching_never_slides() {
    let mut modes = SlidingModes::new(1);
    for event in 0..64 {
        record(&mut modes, f64::from(event) * 1.0e-3, &[0]);
        assert!(!modes.is_sliding(0));
    }
}

/// Applied at the scan coordinate, sliding events are an interval apart, far
/// beyond the gap; each one in the interval after the last keeps the mode.
#[test]
fn a_crossing_in_the_next_interval_keeps_the_sliding_mode() {
    let mut modes = SlidingModes::new(2);
    slide(&mut modes, 1.0, 8, 1);
    record(&mut modes, 1.001, &[1]);
    record(&mut modes, 1.002, &[1]);
    assert!(modes.is_sliding(1));
}

#[test]
fn an_interval_without_a_crossing_ends_the_sliding_mode() {
    let mut modes = SlidingModes::new(2);
    slide(&mut modes, 1.0, 8, 1);
    modes.begin_interval();
    assert!(modes.is_sliding(1));
    modes.begin_interval();
    assert!(!modes.is_sliding(1));
    record(&mut modes, 2.0, &[1]);
    assert!(!modes.is_sliding(1));
}

#[test]
fn an_event_of_another_indicator_ends_the_sliding_mode() {
    let mut modes = SlidingModes::new(2);
    slide(&mut modes, 1.0, 8, 1);
    record(&mut modes, 1.001, &[0]);
    assert!(!modes.is_sliding(1));
    record(&mut modes, 1.001 + 1.0e-8, &[1]);
    assert!(!modes.is_sliding(1));
}

#[test]
fn a_reset_forgets_every_event() {
    let mut modes = SlidingModes::new(2);
    slide(&mut modes, 1.0, 8, 1);
    modes.reset();
    assert_eq!(modes, SlidingModes::new(2));
}
