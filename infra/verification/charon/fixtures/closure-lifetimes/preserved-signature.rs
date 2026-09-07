pub fn unrelated_outer<'env>() -> impl for<'a> FnMut(&'a u32) -> &'a u32 + use<'env> {
    |value| value
}

pub fn no_outer() -> impl for<'a> FnMut(&'a u32) -> &'a u32 {
    |value| value
}

pub fn call(value: &u32) -> &u32 {
    unrelated_outer()(value)
}

pub fn captured_call(value: &u32) -> &u32 {
    let captured = || value;
    captured()
}

pub fn captured_move(value: &u32) -> &u32 {
    let captured = move || value;
    captured()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn both_closures_return_the_original_argument() {
        let first = 13;
        let second = 42;
        let mut adapter = unrelated_outer();
        assert!(std::ptr::eq(adapter(&first), &first));
        assert!(std::ptr::eq(adapter(&second), &second));
        assert!(std::ptr::eq(no_outer()(&first), &first));
        assert!(std::ptr::eq(call(&second), &second));
        assert!(std::ptr::eq(captured_call(&first), &first));
        assert!(std::ptr::eq(captured_move(&second), &second));
    }
}
