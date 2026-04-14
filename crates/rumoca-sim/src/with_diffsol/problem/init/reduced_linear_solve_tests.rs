use super::*;

#[test]
fn reduced_newton_solve_drops_ignored_rows_and_fixed_columns_before_solving() {
    let jac =
        nalgebra::DMatrix::from_row_slice(3, 3, &[2.0, 1.0, 0.0, 5.0, 6.0, 7.0, 0.0, 1.0, 3.0]);
    let rhs = nalgebra::DVector::from_vec(vec![3.0, 0.0, 7.0]);
    let ignored_rows = [false, true, false];
    let fixed_cols = [false, false, true];

    let (delta, method) =
        solve_newton_linear_system_reduced(&jac, &rhs, &ignored_rows, &fixed_cols)
            .expect("reduced solve");

    assert_eq!(method, NewtonLinearSolveMethod::Lu);
    assert!((delta[0] + 2.0).abs() <= 1.0e-12);
    assert!((delta[1] - 7.0).abs() <= 1.0e-12);
    assert_eq!(delta[2], 0.0);
}

#[test]
fn reduced_newton_solve_returns_zero_when_all_unknowns_are_fixed() {
    let jac = nalgebra::DMatrix::identity(2, 2);
    let rhs = nalgebra::DVector::from_vec(vec![0.0, 0.0]);
    let ignored_rows = [true, true];
    let fixed_cols = [true, true];

    let (delta, method) =
        solve_newton_linear_system_reduced(&jac, &rhs, &ignored_rows, &fixed_cols)
            .expect("zero reduced solve");

    assert_eq!(method, NewtonLinearSolveMethod::LeastSquaresPseudoInverse);
    assert_eq!(delta, nalgebra::DVector::zeros(2));
}
