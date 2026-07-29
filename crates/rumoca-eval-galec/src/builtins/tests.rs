use rumoca_ir_galec::ast::{Block, Name};
use rumoca_ir_galec::package::CheckedAlgorithmBlock;

use super::{
    lu_factorize_builtin, lu_solve_builtin, pivot_value, scalar_builtin, select_pivot,
    solve_linear_equations,
};
use crate::{EvaluationError, Evaluator, IntegerDomain, Value};

fn scalar_array(values: &[f64]) -> Value {
    Value::Array(values.iter().copied().map(Value::Real).collect())
}

fn matrix(rows: &[&[f64]]) -> Value {
    Value::Array(rows.iter().map(|row| scalar_array(row)).collect())
}

fn all_nan(value: &Value) -> bool {
    match value {
        Value::Real(value) => value.is_nan(),
        Value::Array(values) => !values.is_empty() && values.iter().all(all_nan),
        _ => false,
    }
}

fn checked_block() -> CheckedAlgorithmBlock {
    CheckedAlgorithmBlock::construct(Block::new(Name::ident("BuiltinSignals")))
        .expect("empty builtin fixture must be checked")
}

#[test]
fn pivot_selection_rejects_an_empty_candidate_range() {
    assert_eq!(
        select_pivot(&[vec![1.0]], 1),
        Err(EvaluationError::InvalidBuiltinArgument {
            name: "luFactorize",
            detail: "pivot column is outside the matrix",
        })
    );
}

#[test]
fn pivot_conversion_rejects_host_and_target_overflow() {
    let domain = IntegerDomain::signed_32();
    assert_eq!(
        pivot_value(usize::MAX, domain),
        Err(EvaluationError::IntegerOverflow)
    );
    assert_eq!(
        pivot_value(i32::MAX as usize, domain),
        Err(EvaluationError::IntegerOverflow)
    );
    assert_eq!(pivot_value(0, domain), Ok(Value::Integer(1)));
}

#[test]
fn integer_conversion_returns_the_beta_1_defined_zero_and_signals() {
    let block = checked_block();
    let mut evaluator =
        Evaluator::new(&block, IntegerDomain::signed_32()).expect("create evaluator");

    assert_eq!(
        scalar_builtin(&mut evaluator, "integer", vec![Value::Real(f64::NAN)]),
        Ok(Value::Integer(0))
    );
    assert!(evaluator.active_signals().contains("NAN"));

    assert_eq!(
        scalar_builtin(&mut evaluator, "integer", vec![Value::Real(f64::INFINITY)]),
        Ok(Value::Integer(0))
    );
    assert!(evaluator.active_signals().contains("OVERFLOW"));
}

#[test]
fn integer_conversion_checks_the_exact_signed_64_boundary() {
    let block = checked_block();
    let domain = IntegerDomain::new(i64::MIN, i64::MAX).expect("signed 64-bit domain");
    let mut evaluator = Evaluator::new(&block, domain).expect("create evaluator");
    let upper_exclusive = i64::MAX as f64;
    let largest_representable = f64::from_bits(upper_exclusive.to_bits() - 1);

    assert_eq!(
        scalar_builtin(
            &mut evaluator,
            "integer",
            vec![Value::Real(largest_representable)]
        ),
        Ok(Value::Integer(largest_representable as i64))
    );
    assert_eq!(
        scalar_builtin(
            &mut evaluator,
            "integer",
            vec![Value::Real(upper_exclusive)]
        ),
        Ok(Value::Integer(0))
    );
    assert!(evaluator.active_signals().contains("OVERFLOW"));
}

#[test]
fn failed_direct_solve_signals_and_returns_only_nan() {
    let block = checked_block();
    let mut evaluator =
        Evaluator::new(&block, IntegerDomain::signed_32()).expect("create evaluator");
    let result = solve_linear_equations(
        &mut evaluator,
        vec![
            matrix(&[&[1.0, 2.0], &[2.0, 4.0]]),
            scalar_array(&[3.0, 6.0]),
        ],
    )
    .expect("singularity is a GALEC signal, not an evaluator failure");

    assert!(all_nan(&result));
    assert!(
        evaluator
            .active_signals()
            .contains("SOLVE_LINEAR_EQUATIONS_FAILED")
    );
}

#[test]
fn regular_direct_solve_preserves_the_unique_solution() {
    let block = checked_block();
    let mut evaluator =
        Evaluator::new(&block, IntegerDomain::signed_32()).expect("create evaluator");
    let result = solve_linear_equations(
        &mut evaluator,
        vec![
            matrix(&[&[2.0, 0.0], &[0.0, 4.0]]),
            scalar_array(&[6.0, 8.0]),
        ],
    );

    assert_eq!(result, Ok(scalar_array(&[3.0, 2.0])));
    assert!(evaluator.active_signals().is_empty());
}

#[test]
fn failed_lu_factorization_signals_and_returns_only_nan_matrix() {
    let block = checked_block();
    let mut evaluator =
        Evaluator::new(&block, IntegerDomain::signed_32()).expect("create evaluator");
    let values = lu_factorize_builtin(
        &mut evaluator,
        vec![matrix(&[&[f64::NAN, 0.0], &[0.0, 1.0]])],
    )
    .expect("NaN input is a GALEC signal, not an evaluator failure");

    assert!(all_nan(&values[0]));
    assert!(
        evaluator
            .active_signals()
            .contains("SOLVE_LINEAR_EQUATIONS_FAILED")
    );
}

#[test]
fn failed_lu_solve_signals_and_returns_only_nan() {
    let block = checked_block();
    let mut evaluator =
        Evaluator::new(&block, IntegerDomain::signed_32()).expect("create evaluator");
    let result = lu_solve_builtin(
        &mut evaluator,
        vec![
            matrix(&[&[1.0, 0.0], &[0.0, 0.0]]),
            Value::Array(vec![Value::Integer(1), Value::Integer(2)]),
            scalar_array(&[2.0, 1.0]),
        ],
    )
    .expect("singularity is a GALEC signal, not an evaluator failure");

    assert!(all_nan(&result));
    assert!(
        evaluator
            .active_signals()
            .contains("SOLVE_LINEAR_EQUATIONS_FAILED")
    );
}
