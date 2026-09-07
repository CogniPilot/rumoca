pub fn distinct<'a, 'b>() -> impl FnMut(&'a u32, &'b u32) -> &'b u32 {
    |_first, second| second
}

pub fn first<'a, 'b>() -> impl FnMut(&'a u32, &'b u32) -> &'a u32 {
    |first, _second| first
}

pub fn captured<'a, 'b>(saved: &'a u32) -> impl FnMut(&'b u32) -> &'a u32 {
    move |_other| saved
}

pub fn captured_argument<'a, 'b>(saved: &'a u32) -> impl FnMut(&'b u32) -> &'b u32 + use<'a, 'b> {
    move |other| {
        std::hint::black_box(saved);
        other
    }
}

pub fn paired<'a, 'b>() -> impl FnMut(&'a u32, &'b u32) -> (&'a u32, &'b u32) {
    |first, second| (first, second)
}

pub fn swapped<'a, 'b>() -> impl FnMut(&'a u32, &'b u32) -> (&'b u32, &'a u32) {
    |first, second| (second, first)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn independent_arguments_preserve_the_selected_referent() {
        let a = 13;
        let b = 42;
        assert!(std::ptr::eq(distinct()(&a, &b), &b));
        assert!(std::ptr::eq(first()(&a, &b), &a));
        let (left, right) = paired()(&a, &b);
        assert!(std::ptr::eq(left, &a));
        assert!(std::ptr::eq(right, &b));
        let (left, right) = swapped()(&a, &b);
        assert!(std::ptr::eq(left, &b));
        assert!(std::ptr::eq(right, &a));
    }

    #[test]
    fn capture_and_argument_remain_distinct() {
        let saved = 13;
        let other = 42;
        assert!(std::ptr::eq(captured(&saved)(&other), &saved));
        assert!(std::ptr::eq(captured_argument(&saved)(&other), &other));
    }
}
