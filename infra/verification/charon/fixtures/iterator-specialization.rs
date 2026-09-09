//! Source witness for iterator callbacks whose mutable state must survive a call.

pub fn mapped_next(values: &[u32], initial_calls: u32) -> (Option<u32>, u32) {
    let mut calls = initial_calls;
    let value = values
        .iter()
        .map(|value| {
            calls += 1;
            *value
        })
        .next();
    (value, calls)
}

pub type CountedItem = (u32, u32);

pub fn mapped_pair(
    values: &[u32],
    initial_calls: u32,
) -> (Option<CountedItem>, Option<CountedItem>) {
    let mut calls = initial_calls;
    let mut iter = values.iter().map(move |value| {
        calls += 1;
        (*value, calls)
    });
    (iter.next(), iter.next())
}

#[cfg(test)]
mod tests {
    use super::{mapped_next, mapped_pair};

    #[test]
    fn empty_iterator_does_not_call_the_closure() {
        assert_eq!(mapped_next(&[], 0), (None, 0));
        assert_eq!(mapped_next(&[], u32::MAX), (None, u32::MAX));
    }

    #[test]
    fn next_calls_the_closure_once_and_retains_its_state() {
        assert_eq!(mapped_next(&[7], 4), (Some(7), 5));
        assert_eq!(mapped_next(&[7, 11, 13], 4), (Some(7), 5));
        assert_eq!(mapped_next(&[0], u32::MAX - 1), (Some(0), u32::MAX));
    }

    #[test]
    #[should_panic(expected = "attempt to add with overflow")]
    fn overflowing_callback_is_not_silently_discarded() {
        let _ = mapped_next(&[7], u32::MAX);
    }

    #[test]
    fn owned_callback_state_survives_between_calls() {
        assert_eq!(mapped_pair(&[], u32::MAX), (None, None));
        assert_eq!(mapped_pair(&[7], 4), (Some((7, 5)), None));
        assert_eq!(mapped_pair(&[7, 11, 13], 4), (Some((7, 5)), Some((11, 6))));
        assert_eq!(mapped_pair(&[0], u32::MAX - 1), (Some((0, u32::MAX)), None));
    }

    #[test]
    #[should_panic(expected = "attempt to add with overflow")]
    fn second_callback_overflow_is_not_silently_discarded() {
        let _ = mapped_pair(&[7, 11], u32::MAX - 1);
    }
}
