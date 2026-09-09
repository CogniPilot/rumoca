use std::num::NonZeroU32;

pub fn equal(left: NonZeroU32, right: NonZeroU32) -> bool {
    left == right
}

pub fn cloned<T: Clone>(value: &T) -> T {
    value.clone()
}

pub fn clone_optional(value: &Option<NonZeroU32>) -> Option<NonZeroU32> {
    cloned(value)
}

#[cfg(test)]
mod tests {
    use super::{clone_optional, equal};
    use std::num::NonZeroU32;

    #[test]
    fn equality_distinguishes_identity() {
        let first = NonZeroU32::new(1).expect("nonzero test input");
        let second = NonZeroU32::new(17).expect("nonzero test input");
        let maximum = NonZeroU32::new(u32::MAX).expect("nonzero test input");
        assert!(equal(first, first));
        assert!(equal(maximum, maximum));
        assert!(!equal(first, second));
        assert!(!equal(second, first));
        assert!(!equal(second, maximum));
    }

    #[test]
    fn cloning_preserves_payload_and_absence() {
        let value = Some(NonZeroU32::new(17).expect("nonzero test input"));
        assert_eq!(clone_optional(&value), value);
        assert_eq!(clone_optional(&None), None);
    }
}
