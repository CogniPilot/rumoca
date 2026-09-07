// A declared bound can hold in the solved region values without being a path
// required by the closure body's region-constraint graph. Keep these distinct.
pub fn declared_bound<'long: 'short, 'short>(
    first: &'long u8,
    second: &'short u8,
) -> impl Fn() -> u8 + 'short {
    move || first.wrapping_add(*second)
}

// A second genuine closure lets the common owner-refusal test retain its
// wrong-receiver witness on this source, without fabricating a DefId.
pub fn identity(value: u8) -> impl Fn() -> u8 {
    move || value
}

#[cfg(test)]
mod tests {
    #[test]
    fn declared_bound_keeps_both_inputs() {
        let first = 3;
        let second = 7;
        assert_eq!(super::declared_bound(&first, &second)(), 10);
        assert_eq!(super::identity(13)(), 13);
    }
}
