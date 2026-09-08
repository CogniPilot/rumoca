//! Small source controls for library calls reached by DAE profile admission.
//! These are translation fixtures, not substitutes for the production checker.

pub fn conditional_reference(enabled: bool, values: &[u32]) -> Option<&u32> {
    enabled.then(|| &values[0])
}

pub fn conditional_effect(enabled: bool, calls: &mut u32) -> Option<u32> {
    enabled.then(|| {
        *calls += 1;
        *calls
    })
}

pub fn mapped_count(values: &[u32]) -> usize {
    values.iter().map(|value| (value, value)).len()
}

pub fn mapped_sum(values: &[u32]) -> usize {
    values.iter().map(|value| *value as usize).sum()
}

pub fn generic_count<I: ExactSizeIterator>(values: I) -> usize {
    values.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn conditional_reference_preserves_identity() {
        let values = [17];
        assert!(conditional_reference(false, &[]).is_none());
        let selected = conditional_reference(true, &values).expect("enabled");
        assert!(std::ptr::eq(selected, &values[0]));
    }

    #[test]
    fn then_does_not_evaluate_the_disabled_closure() {
        let mut calls = 4;
        assert_eq!(conditional_effect(false, &mut calls), None);
        assert_eq!(calls, 4);
        assert_eq!(conditional_effect(true, &mut calls), Some(5));
        assert_eq!(calls, 5);
    }

    #[test]
    fn mapped_counts_and_sums_cover_empty_and_multiple_elements() {
        assert_eq!(mapped_count(&[]), 0);
        assert_eq!(mapped_count(&[9]), 1);
        assert_eq!(mapped_count(&[9, 4, 2]), 3);
        assert_eq!(mapped_sum(&[]), 0);
        assert_eq!(mapped_sum(&[9]), 9);
        assert_eq!(mapped_sum(&[9, 4, 2]), 15);
        assert_eq!(generic_count([9, 4, 2].iter()), 3);
    }
}
