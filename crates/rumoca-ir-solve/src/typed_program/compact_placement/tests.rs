use super::*;

#[test]
fn placement_refuses_indices_outside_the_state_domain() {
    // Row k and column k + 1 over 0..3 stay inside four states.
    let placement = CompactJacobianPlacement::new(vec![3], (0, vec![1]), vec![(1, vec![1])], 4)
        .expect("in range");
    assert_eq!(placement.value_dimensions(), Some(vec![3, 1]));
    // The same column overruns three states at the last point.
    assert!(CompactJacobianPlacement::new(vec![3], (0, vec![1]), vec![(1, vec![1])], 3).is_none());
    // A negative stride below zero, an empty box and a rank mismatch all refuse.
    assert!(CompactJacobianPlacement::new(vec![3], (1, vec![-1]), vec![(0, vec![0])], 4).is_none());
    assert!(CompactJacobianPlacement::new(vec![0], (0, vec![1]), vec![(0, vec![0])], 4).is_none());
    // A node that reads no state places nothing; its map is scalar per point.
    let empty = CompactJacobianPlacement::new(vec![3], (0, vec![1]), vec![], 4).unwrap();
    assert_eq!(empty.value_dimensions(), Some(vec![3]));
    assert!(
        CompactJacobianPlacement::new(vec![3], (0, vec![1, 0]), vec![(0, vec![0])], 4).is_none()
    );
}

#[test]
fn placement_points_run_row_major() {
    let placement =
        CompactJacobianPlacement::new(vec![2, 3], (0, vec![3, 1]), vec![(0, vec![0, 0])], 6)
            .unwrap();
    let mut rows = Vec::new();
    placement
        .for_each_point(|ordinals| {
            rows.push(placement.row.at(ordinals).unwrap());
            Ok(())
        })
        .unwrap();
    assert_eq!(rows, vec![0, 1, 2, 3, 4, 5]);
}
