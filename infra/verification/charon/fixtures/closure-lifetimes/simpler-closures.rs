pub fn named<'a>() -> impl FnMut(&'a u32) -> &'a u32 {
    |value| value
}

pub fn higher_ranked() -> impl for<'a> FnMut(&'a u32) -> &'a u32 {
    |value| value
}

pub fn named_call(value: &u32) -> &u32 {
    named()(value)
}

pub fn higher_ranked_call(value: &u32) -> &u32 {
    higher_ranked()(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn both_forms_preserve_two_distinct_referents() {
        let values = [13, 42];
        for value in &values {
            assert!(std::ptr::eq(named_call(value), value));
            assert!(std::ptr::eq(higher_ranked_call(value), value));
        }
    }

    #[test]
    fn returned_references_do_not_borrow_the_closure_receiver() {
        let values = [13, 42];
        let mut project = named();
        let first = project(&values[0]);
        let second = project(&values[1]);
        assert!(std::ptr::eq(first, &values[0]));
        assert!(std::ptr::eq(second, &values[1]));
    }
}
