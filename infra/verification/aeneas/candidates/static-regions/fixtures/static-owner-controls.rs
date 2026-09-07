#[derive(Clone, Copy)]
pub struct Pair {
    pub first: &'static u32,
    pub second: &'static u32,
}

const VALUES: Pair = Pair { first: &11, second: &29 };

pub fn global() -> Pair {
    VALUES
}

pub fn project(value: Pair) -> &'static u32 {
    value.second
}

pub fn through_call() -> &'static u32 {
    project(global())
}

pub fn copies() -> (Pair, Pair) {
    let value = global();
    (value, value)
}

pub fn local_scope() -> &'static u32 {
    let selected;
    {
        let pair = global();
        selected = pair.second;
    }
    selected
}

pub fn nested(value: &'static &'static u32) -> &'static u32 {
    value
}

pub fn ordinary(value: &u32) -> &u32 {
    value
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn static_field_identity_survives_copy_call_and_scope() {
        let pair = global();
        for selected in [project(pair), through_call(), local_scope()] {
            assert!(std::ptr::eq(selected, pair.second));
            assert_eq!(*selected, 29);
        }
        let (first, second) = copies();
        assert!(std::ptr::eq(first.first, second.first));
        assert!(std::ptr::eq(first.second, second.second));
        assert_eq!((*first.first, *first.second), (11, 29));
    }

    #[test]
    fn nested_and_ordinary_keep_the_input_referent() {
        static VALUE: u32 = 43;
        static REFERENCE: &u32 = &VALUE;
        assert!(std::ptr::eq(nested(&REFERENCE), &VALUE));
        let local = 71;
        assert!(std::ptr::eq(ordinary(&local), &local));
    }
}
