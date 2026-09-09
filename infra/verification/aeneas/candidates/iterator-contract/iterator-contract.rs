//! Calls through the actual standard-library trait interfaces reached by DAE admission.

pub fn iterator_len<I: ExactSizeIterator>(values: &I) -> usize {
    values.len()
}

pub fn fold_from<I: Iterator<Item = usize>>(values: I, initial: usize) -> usize {
    values.fold(initial, |sum, entry| sum + entry)
}

#[cfg(test)]
mod tests {
    use super::{fold_from, iterator_len};

    #[test]
    fn exact_size_and_fold_preserve_empty_and_nonempty_inputs() {
        for (values, expected_len, expected_sum) in
            [(&[][..], 0, 10), (&[3][..], 1, 13), (&[3, 5][..], 2, 18)]
        {
            assert_eq!(iterator_len(&values.iter()), expected_len);
            assert_eq!(fold_from(values.iter().copied(), 10), expected_sum);
        }
    }
}
