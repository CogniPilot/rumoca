use super::*;

fn host() -> NativeCarriedHost {
    NativeCarriedHost::new((10, 20), std::iter::once(0..8), [2..6, 8..16])
}

#[test]
fn every_pair_stays_one_run_in_y_order() {
    let state =
        NativeCarriedState::construct_for_host(&host(), &[(2..4, 4..6), (0..2, 2..4)]).unwrap();
    let [first, second] = state.runs() else {
        panic!("two pairs are two runs");
    };
    assert_eq!((first.y_range(), first.p_range()), (0..2, 2..4));
    assert_eq!((second.y_range(), second.p_range()), (2..4, 4..6));
    assert_eq!(state.run_of_y(3), Some(second));
    assert_eq!(state.run_of_y(4), None);
    assert!(NativeCarriedState::default().is_empty());
}

#[test]
fn publication_writes_each_run_into_p_and_leaves_other_cells() {
    let state =
        NativeCarriedState::construct_for_host(&host(), &[(1..3, 8..10), (5..6, 4..5)]).unwrap();
    let work: Vec<f64> = (0..10).map(f64::from).collect();
    let mut p = vec![-1.0; 20];
    state.publish(&work, &mut p);
    let written: Vec<usize> = (0..20).filter(|&i| p[i] >= 0.0).collect();
    assert_eq!(written, [4, 8, 9]);
    assert_eq!((p[4], p[8], p[9]), (5.0, 1.0, 2.0));
}

#[test]
fn a_pair_is_refused_unless_both_ranges_are_host_cells_of_the_right_kind() {
    let check = |y: Range<usize>, p: Range<usize>| {
        NativeCarriedState::construct_for_host(&host(), &[(y, p)])
    };
    assert!(matches!(
        check(0..2, 2..5),
        Err(NativeCarriedRefusal::MismatchedRanges { .. })
    ));
    assert!(matches!(
        check(0..0, 2..2),
        Err(NativeCarriedRefusal::MismatchedRanges { .. })
    ));
    assert!(matches!(
        check(8..11, 2..5),
        Err(NativeCarriedRefusal::YOutOfRange { .. })
    ));
    assert!(matches!(
        check(0..2, 19..21),
        Err(NativeCarriedRefusal::POutOfRange { .. })
    ));
    assert!(matches!(
        check(7..9, 2..4),
        Err(NativeCarriedRefusal::YNotOutput { .. })
    ));
    assert!(matches!(
        check(0..2, 5..7),
        Err(NativeCarriedRefusal::PNotRealInput { .. })
    ));
    assert!(matches!(
        check(0..2, 7..9),
        Err(NativeCarriedRefusal::PNotRealInput { .. })
    ));
}

#[test]
fn two_pairs_naming_one_cell_are_refused() {
    let y = NativeCarriedState::construct_for_host(&host(), &[(0..3, 2..5), (2..4, 8..10)]);
    assert_eq!(y, Err(NativeCarriedRefusal::Overlap));
    let p = NativeCarriedState::construct_for_host(&host(), &[(0..2, 2..4), (4..6, 3..5)]);
    assert_eq!(p, Err(NativeCarriedRefusal::Overlap));
}

#[test]
fn typed_input_cells_are_removed_from_the_real_inputs() {
    assert_eq!(
        subtract(std::iter::once(0..10).collect(), vec![2..4, 6..7]),
        [0..2, 4..6, 7..10]
    );
    assert_eq!(
        subtract(
            std::iter::once(3..4).collect(),
            std::iter::once(0..10).collect()
        ),
        Vec::<Range<usize>>::new()
    );
    assert_eq!(merged(vec![5..7, 0..2, 1..3, 3..3]), [0..3, 5..7]);
}
