use rumoca_ir_galec::ast::{Block, Name};
use rumoca_ir_galec::package::{
    AlgorithmCodeIntegerFormat, AlgorithmCodePackage, AlgorithmCodeRealFormat,
};

use super::{
    lift_builtin, lu_factorize_builtin, lu_solve_builtin, pivot_value, scalar_builtin,
    select_pivot, solve_linear_equations,
};
use crate::test_support::{binary64_i32, package};
use crate::{EvaluationError, Evaluator, Value};

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

fn checked_block() -> AlgorithmCodePackage {
    binary64_i32(Block::new(Name::ident("BuiltinSignals")))
}

fn checked_i64_block() -> AlgorithmCodePackage {
    package(
        Block::new(Name::ident("BuiltinSignalsI64")),
        AlgorithmCodeRealFormat::Binary64,
        AlgorithmCodeIntegerFormat::I64,
    )
}

fn real(value: Value) -> f64 {
    value.real().expect("expected Real result")
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
    let block = checked_block();
    let evaluator = Evaluator::new(&block).expect("create evaluator");
    let format = evaluator.integer_format();
    assert_eq!(
        pivot_value(usize::MAX, format),
        Err(EvaluationError::IntegerOverflow)
    );
    assert_eq!(
        pivot_value(i32::MAX as usize, format),
        Err(EvaluationError::IntegerOverflow)
    );
    assert_eq!(pivot_value(0, format), Ok(Value::Integer(1)));
}

#[test]
fn integer_conversion_returns_the_beta_1_defined_zero_and_signals() {
    let block = checked_block();
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");

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
fn sign_matches_the_emitted_c_helper_at_zero_and_nan() {
    let block = checked_block();
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");

    for (input, expected) in [
        (2.5, 1.0),
        (-2.5, -1.0),
        (0.0, 0.0),
        (-0.0, 0.0),
        (f64::NAN, 0.0),
    ] {
        assert_eq!(
            scalar_builtin(&mut evaluator, "sign", vec![Value::Real(input)]),
            Ok(Value::Real(expected)),
            "sign({input})"
        );
    }
}

#[test]
fn integer_conversion_checks_the_exact_signed_64_boundary() {
    let block = checked_i64_block();
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");
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
fn integer_conversion_uses_the_package_source_integer_domain() {
    let block = package(
        Block::new(Name::ident("BuiltinSignalsI8")),
        AlgorithmCodeRealFormat::Binary64,
        AlgorithmCodeIntegerFormat::I8,
    );
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");

    assert_eq!(
        scalar_builtin(&mut evaluator, "integer", vec![Value::Real(127.0)]),
        Ok(Value::Integer(127))
    );
    assert_eq!(
        scalar_builtin(&mut evaluator, "integer", vec![Value::Real(128.0)]),
        Ok(Value::Integer(0))
    );
    assert!(evaluator.active_signals().contains("OVERFLOW"));
}

#[test]
fn real_min_max_use_galec_relational_selection() {
    let block = checked_block();
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");

    assert_eq!(
        scalar_builtin(
            &mut evaluator,
            "min",
            vec![Value::Real(3.0), Value::Real(2.0)]
        ),
        Ok(Value::Real(2.0))
    );
    assert_eq!(
        scalar_builtin(
            &mut evaluator,
            "max",
            vec![Value::Real(3.0), Value::Real(2.0)]
        ),
        Ok(Value::Real(3.0))
    );
    assert_eq!(
        scalar_builtin(
            &mut evaluator,
            "min",
            vec![Value::Real(2.0), Value::Real(2.0)]
        ),
        Ok(Value::Real(2.0))
    );
    assert_eq!(
        scalar_builtin(
            &mut evaluator,
            "max",
            vec![Value::Real(2.0), Value::Real(2.0)]
        ),
        Ok(Value::Real(2.0))
    );
}

#[test]
fn real_min_max_preserve_galec_nan_operand_order() {
    let block = checked_block();
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");

    for name in ["min", "max"] {
        let lhs_nan = scalar_builtin(
            &mut evaluator,
            name,
            vec![Value::Real(f64::NAN), Value::Real(2.0)],
        )
        .expect("builtin evaluation");
        assert_eq!(lhs_nan, Value::Real(2.0));

        let rhs_nan = scalar_builtin(
            &mut evaluator,
            name,
            vec![Value::Real(2.0), Value::Real(f64::NAN)],
        )
        .expect("builtin evaluation");
        assert!(real(rhs_nan).is_nan());

        let both_nan = scalar_builtin(
            &mut evaluator,
            name,
            vec![Value::Real(f64::NAN), Value::Real(f64::NAN)],
        )
        .expect("builtin evaluation");
        assert!(real(both_nan).is_nan());
    }
}

#[test]
fn lifted_real_min_max_share_scalar_nan_selection() {
    let block = checked_block();
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");

    for name in ["min", "max"] {
        let result = lift_builtin(
            &mut evaluator,
            name,
            vec![
                scalar_array(&[f64::NAN, 2.0]),
                scalar_array(&[3.0, f64::NAN]),
            ],
        )
        .expect("lifted builtin evaluation");
        let Value::Array(values) = result else {
            panic!("expected lifted Real array");
        };
        assert_eq!(values[0], Value::Real(3.0));
        assert!(real(values[1].clone()).is_nan());
    }
}

#[test]
fn failed_direct_solve_signals_and_returns_only_nan() {
    let block = checked_block();
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");
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
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");
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
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");
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
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");
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
