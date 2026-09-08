pub fn named<'a>() -> impl FnMut(&'a u32) -> &'a u32 {
    |value| value
}

pub fn higher_ranked() -> impl for<'a> FnMut(&'a u32) -> &'a u32 {
    |value| value
}

pub fn captured(value: &u32) -> impl Fn() -> u32 + '_ {
    move || *value
}
