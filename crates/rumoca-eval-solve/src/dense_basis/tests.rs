use super::*;

#[test]
fn singular_trial_correction_is_minimum_norm_but_cannot_certify_a_basis() {
    let matrix = DenseStageMatrix::new(2, 3, &[1., 1., 0., 2., 2., 0.]).unwrap();
    let step = matrix.correction(&[2., 4.]).unwrap();
    assert!((step[0] + 1.).abs() < 1e-13);
    assert!((step[1] + 1.).abs() < 1e-13);
    assert_eq!(step[2], 0.);
    assert_eq!(
        matrix.independent_columns(&[ColumnChoice::Eligible(0); 3]),
        Err(DenseBasisError::Rank)
    );
}

#[test]
fn required_states_and_preference_groups_precede_numerical_pivoting() {
    let matrix = DenseStageMatrix::new(2, 4, &[1., 100., 1., 0., 0., 0., 0., 1.]).unwrap();
    let choices = [
        ColumnChoice::Eligible(0),
        ColumnChoice::Independent,
        ColumnChoice::Eligible(1),
        ColumnChoice::Dependent,
    ];
    assert_eq!(matrix.independent_columns(&choices).unwrap(), [1, 2]);
    assert_eq!(
        matrix.independent_columns(&[ColumnChoice::Independent; 4]),
        Err(DenseBasisError::Rank)
    );
}

#[test]
fn dependent_columns_must_be_independent_and_all_inputs_finite() {
    let matrix = DenseStageMatrix::new(2, 3, &[1., 2., 0., 0., 0., 1.]).unwrap();
    assert_eq!(
        matrix.independent_columns(&[
            ColumnChoice::Dependent,
            ColumnChoice::Dependent,
            ColumnChoice::Eligible(0)
        ]),
        Err(DenseBasisError::Rank)
    );
    assert!(matches!(
        DenseStageMatrix::new(1, 1, &[f64::NAN]),
        Err(DenseBasisError::NonFinite)
    ));
    assert_eq!(
        matrix.correction(&[0., f64::INFINITY]),
        Err(DenseBasisError::NonFinite)
    );
    assert!(matches!(
        DenseStageMatrix::new(usize::MAX, 2, &[]),
        Err(DenseBasisError::Shape)
    ));
}
