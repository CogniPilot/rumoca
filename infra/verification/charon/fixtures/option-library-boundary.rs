//! Pure standard-library operations reached by DAE profile admission.

use std::marker::PhantomData;

pub fn result_then_option(first: Result<u32, u32>, second: Option<u32>) -> Option<(u32, u32)> {
    let first = first.ok()?;
    let second = second?;
    Some((first, second))
}

pub fn same_marker<T: ?Sized>(left: PhantomData<T>, right: PhantomData<T>) -> bool {
    left == right
}

#[cfg(test)]
mod tests {
    use super::{result_then_option, same_marker};
    use std::marker::PhantomData;

    #[test]
    fn option_result_cases_preserve_values_and_refusals() {
        for first in [0, 1, u32::MAX] {
            for second in [0, 7, u32::MAX] {
                assert_eq!(
                    result_then_option(Ok(first), Some(second)),
                    Some((first, second))
                );
                assert_eq!(result_then_option(Err(first), Some(second)), None);
            }
            assert_eq!(result_then_option(Ok(first), None), None);
            assert_eq!(result_then_option(Err(first), None), None);
        }
    }

    #[test]
    fn marker_equality_does_not_require_value_equality() {
        struct NoEquality;
        assert!(same_marker::<NoEquality>(PhantomData, PhantomData));
        assert!(same_marker::<[NoEquality]>(PhantomData, PhantomData));
        assert!(same_marker::<str>(PhantomData, PhantomData));
    }
}
