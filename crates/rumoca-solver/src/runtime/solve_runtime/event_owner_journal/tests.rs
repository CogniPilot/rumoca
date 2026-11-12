use super::*;

/// Program 0 reads `y[3]`, program 1 reads `y[5]` and `p[2]`; the root
/// conditions read `p[2]`.
fn lanes() -> EventOwnerLanes {
    let owner = |y: &[u32], p: &[u32]| OwnerLanes {
        y: y.into(),
        p: p.into(),
    };
    EventOwnerLanes {
        y: [3, 5].into(),
        p: [2].into(),
        roots: owner(&[], &[0]),
        programs: [owner(&[0], &[]), owner(&[1], &[0])].into(),
    }
}

const T: f64 = 0.5;

/// A journal opened over `y`, `p` with both programs evaluated at the first
/// observation.
fn evaluated(lanes: &EventOwnerLanes, y: &[f64], p: &[f64]) -> EventOwnerJournal {
    let mut journal = EventOwnerJournal::default();
    journal.open(Some(lanes), y, p);
    journal.observe(lanes, y, p);
    for program in 0..2 {
        assert_eq!(journal.clean_program(lanes, program, T, &[]), None);
        journal.record_program(program, T, &[], &[program as f64]);
    }
    journal
}

#[test]
fn a_lane_changed_before_an_evaluation_is_read_by_it() {
    let lanes = lanes();
    let (mut y, p) = (vec![0.0; 6], vec![0.0; 3]);
    let mut journal = evaluated(&lanes, &y, &p);
    // Within the next pass the lane changes, and only then is the program
    // evaluated: it reads the new value, so it is dirty at that evaluation
    // and clean after it.
    y[3] = 1.0;
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 0, T, &[]), None);
    journal.record_program(0, T, &[], &[10.0]);
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 0, T, &[]), Some(&[10.0][..]));
    // The other program reads neither changed lane.
    assert_eq!(journal.clean_program(&lanes, 1, T, &[]), Some(&[1.0][..]));
}

#[test]
fn a_lane_changed_after_an_evaluation_reruns_it_next_pass() {
    let lanes = lanes();
    let (y, mut p) = (vec![0.0; 6], vec![0.0; 3]);
    let mut journal = evaluated(&lanes, &y, &p);
    // The write lands after both evaluations of this pass.
    p[2] = 4.0;
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 1, T, &[]), None);
    assert_eq!(journal.clean_program(&lanes, 0, T, &[]), Some(&[0.0][..]));
    assert_eq!(journal.changed_in_event(), [0, 1]);
}

#[test]
fn a_write_of_the_same_bits_is_no_change() {
    let lanes = lanes();
    let (mut y, p) = (vec![2.0; 6], vec![0.0; 3]);
    let mut journal = evaluated(&lanes, &y, &p);
    y[5] = 2.0;
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 1, T, &[]), Some(&[1.0][..]));
    assert_eq!(journal.changed_in_event(), [0, 0]);
}

#[test]
fn a_signed_zero_flip_is_a_change() {
    let lanes = lanes();
    let (mut y, p) = (vec![0.0; 6], vec![0.0; 3]);
    let mut journal = evaluated(&lanes, &y, &p);
    y[3] = -0.0;
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 0, T, &[]), None);
    assert_eq!(journal.changed_in_event(), [1, 0]);
}

#[test]
fn another_time_or_other_overrides_or_a_new_event_rerun_every_program() {
    let lanes = lanes();
    let (y, p) = (vec![0.0; 6], vec![0.0; 3]);
    let mut journal = evaluated(&lanes, &y, &p);
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 0, T + 1.0, &[]), None);
    assert_eq!(journal.clean_program(&lanes, 0, T, &[(0, 1.0)]), None);
    assert!(journal.clean_program(&lanes, 0, T, &[]).is_some());
    assert_eq!(journal.clean_roots(&lanes, T), None);
    journal.record_roots(T, &[7.0]);
    assert_eq!(journal.clean_roots(&lanes, T), Some(&[7.0][..]));
    journal.open(Some(&lanes), &y, &p);
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 0, T, &[]), None);
    assert_eq!(journal.clean_roots(&lanes, T), None);
    // A journal opened without lanes takes nothing from it.
    journal.open(None, &y, &p);
    journal.record_roots(T, &[7.0]);
    assert_eq!(journal.clean_roots(&lanes, T), None);
}
