pub fn even(value: u32) -> bool {
    if value == 0 { true } else { odd(value - 1) }
}

pub fn odd(value: u32) -> bool {
    if value == 0 { false } else { even(value - 1) }
}
