use super::*;

fn bounded(indices: &[usize]) -> solve::LeafDependencies {
    solve::LeafDependencies::Bounded(solve::IndexIntervals::of(indices.iter().copied()))
}

fn selected(cones: &StateCones, state_count: usize, seed: &[f64]) -> Vec<usize> {
    let mut out = vec![99];
    cones.select(
        &active_states(state_count, seed).expect("state-only seed"),
        &mut out,
    );
    out
}

/// Three states (y 0..3). Program 0 reads state 0; program 1 depends on states
/// 0 and 1 through earlier assignments; program 2 depends only on a non-state
/// leaf (y 7); program 3 has no certificate; program 4 is unbounded.
#[test]
fn programs_are_selected_by_their_issued_state_leaves() {
    let (a, b, c) = (bounded(&[0]), bounded(&[0, 1, 5]), bounded(&[7]));
    let unbounded = solve::LeafDependencies::Unbounded;
    let programs = vec![
        vec![Some(&a)],
        vec![Some(&b)],
        vec![Some(&c)],
        vec![None],
        vec![Some(&unbounded)],
    ];
    let cones = StateCones::from_leaves(3, &programs).expect("states exist");
    assert_eq!(selected(&cones, 3, &[1.0, 0.0, 0.0]), vec![0, 1, 3, 4]);
    assert_eq!(selected(&cones, 3, &[0.0, -2.0, 0.0]), vec![1, 3, 4]);
    assert_eq!(selected(&cones, 3, &[0.0, 0.0, 4.0]), vec![3, 4]);
}

#[test]
fn a_nonzero_seed_outside_the_states_keeps_the_full_sweep() {
    assert!(active_states(2, &[1.0, 0.0, 0.0, 0.5]).is_none());
    assert_eq!(active_states(2, &[0.0, 3.0, 0.0]), Some(vec![0b10]));
    assert!(active_states(4, &[1.0]).is_none());
}

#[test]
fn a_model_without_states_has_no_cones() {
    let programs: Vec<Vec<Option<&solve::LeafDependencies>>> = vec![vec![None]];
    assert!(StateCones::from_leaves(0, &programs).is_none());
}
