pub fn bound() -> &'static u32 {
    &7
}

pub fn within(value: u32) -> bool {
    value < *bound()
}
