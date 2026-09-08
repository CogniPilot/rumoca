pub fn helper(value: &u8) -> u8 {
    *value
}

pub fn make(value: &u8) -> impl Fn() -> u8 + '_ {
    move || *value
}
