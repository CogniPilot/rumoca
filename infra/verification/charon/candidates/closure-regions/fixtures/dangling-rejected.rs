pub fn dangling() -> impl FnOnce() -> &'static u32 {
    let value = 42;
    || &value
}
