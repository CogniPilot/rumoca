use super::*;

#[test]
fn guards_follow_selected_coordinates_through_composition() {
    let source = Coordinates::identity(1).restricted(AffineForm::unit_binder(0, 1), 2, 2);
    for selected in [0, 2] {
        let access = Coordinates::access(0, &[], vec![AffineForm::constant(selected, 0)]);
        let mapped = source.compose(&access).unwrap();
        assert_eq!(
            mapped.input_elements(&[], 0, &[3]),
            Some(if selected == 2 { vec![2] } else { vec![] })
        );
        assert_eq!(mapped.is_empty(), selected != 2);
    }
}

#[test]
fn guarded_multidimensional_reshape_does_not_scan_the_whole_box() {
    let relation = Coordinates::access(
        1,
        &[1_048_576, 4],
        vec![AffineForm::unit_binder(1, 3), AffineForm::unit_binder(2, 3)],
    )
    .restricted(
        AffineForm {
            constant: 0,
            coeffs: vec![1, -4, -1],
        },
        0,
        0,
    );
    assert_eq!(
        relation.complete_domain(&[4_194_304], &[1_048_576, 4]),
        Some(())
    );
    for selected in [0, 2_097_153, 4_194_302] {
        assert_eq!(
            relation.input_elements(&[4_194_304], selected, &[1_048_576, 4]),
            Some(vec![selected])
        );
    }
}

#[test]
fn negative_guard_coefficients_keep_both_interval_boundaries() {
    let relation = Coordinates::identity(1).restricted(
        AffineForm {
            constant: 0,
            coeffs: vec![-1],
        },
        -3,
        -1,
    );
    for selected in 0..4 {
        assert_eq!(
            relation.input_elements(&[4], selected, &[4]),
            Some(if selected == 0 {
                vec![]
            } else {
                vec![selected]
            })
        );
    }
}

#[test]
fn unused_input_axes_remain_live_when_the_guard_reads_them() {
    let relation = Coordinates::access(1, &[4], Vec::new()).restricted(
        AffineForm {
            constant: 0,
            coeffs: vec![0, 1],
        },
        2,
        2,
    );
    let selected = relation
        .compose(&Coordinates::access(
            0,
            &[],
            vec![AffineForm::constant(0, 0)],
        ))
        .unwrap();
    assert_eq!(selected.complete_domain(&[], &[]), Some(()));
    assert_eq!(selected.input_elements(&[], 0, &[]), Some(vec![0]));
}

#[test]
fn malformed_and_overflowing_guards_fail_closed() {
    let malformed = Coordinates::identity(1).restricted(
        AffineForm {
            constant: 0,
            coeffs: vec![1, 0],
        },
        0,
        1,
    );
    assert!(malformed.compose(&Coordinates::identity(1)).is_none());
    assert!(malformed.complete_domain(&[2], &[2]).is_none());
    let malformed_constant = Coordinates::identity(1).restricted(
        AffineForm {
            constant: 0,
            coeffs: vec![0, 0],
        },
        0,
        1,
    );
    assert!(
        malformed_constant
            .compose(&Coordinates::identity(1))
            .is_none()
    );
    let overflowing = Coordinates::identity(1).restricted(
        AffineForm {
            constant: i64::MAX,
            coeffs: vec![1],
        },
        0,
        i64::MAX,
    );
    assert!(overflowing.complete_domain(&[2], &[2]).is_none());
    assert!(
        overflowing
            .compose(&Coordinates::access(
                0,
                &[],
                vec![AffineForm::constant(1, 0)]
            ))
            .is_none()
    );
}
