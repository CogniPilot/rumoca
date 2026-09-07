pub struct Entry<'a> {
    pub value: &'a u32,
}

pub fn direct(value: &u32) -> Entry<'_> {
    Entry { value }
}

pub fn adapter<'a>() -> impl FnMut(&'a u32) -> Entry<'a> {
    |value| Entry { value }
}

pub fn closure_call(value: &u32) -> Entry<'_> {
    adapter()(value)
}

pub fn mapped(values: &[u32]) -> impl ExactSizeIterator<Item = Entry<'_>> {
    values.iter().map(|value| Entry { value })
}

pub fn first_via_map(values: &[u32]) -> Option<Entry<'_>> {
    mapped(values).next()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn direct_and_closure_keep_the_supplied_referent() {
        let values = [13, 42, 99];
        for value in &values {
            assert!(std::ptr::eq(direct(value).value, value));
            assert!(std::ptr::eq(closure_call(value).value, value));
        }
    }

    #[test]
    fn one_adapter_can_return_distinct_referents() {
        let values = [13, 42];
        let mut project = adapter();
        let first = project(&values[0]);
        let second = project(&values[1]);
        assert!(std::ptr::eq(first.value, &values[0]));
        assert!(std::ptr::eq(second.value, &values[1]));
    }

    #[test]
    fn map_preserves_order_and_referents() {
        let values = [13, 42, 99];
        let mut entries = mapped(&values);
        assert_eq!(entries.len(), values.len());
        for value in &values {
            assert!(std::ptr::eq(entries.next().unwrap().value, value));
        }
        assert!(entries.next().is_none());
    }

    #[test]
    fn empty_and_nonzero_slice_start_are_preserved() {
        assert!(first_via_map(&[]).is_none());
        let values = [13, 42, 99];
        assert!(std::ptr::eq(
            first_via_map(&values[1..]).unwrap().value,
            &values[1]
        ));
    }
}
