use super::{pivot_value, select_pivot};
use crate::{EvaluationError, IntegerDomain, Value};

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
