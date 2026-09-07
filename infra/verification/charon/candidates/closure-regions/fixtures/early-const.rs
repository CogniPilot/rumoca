const fn select() -> fn(u32) -> u32 {
    |value| value + 1
}

const SELECTED: fn(u32) -> u32 = select();

pub fn through_const(value: u32) -> u32 {
    SELECTED(value)
}

pub fn nested<'a>(value: &'a u32) -> impl FnOnce() -> &'a u32 {
    move || {
        let inner = || value;
        inner()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn const_and_nested_closures() {
        assert_eq!(super::through_const(5), 6);
        let value = 42;
        assert!(std::ptr::eq(super::nested(&value)(), &value));
    }
}
