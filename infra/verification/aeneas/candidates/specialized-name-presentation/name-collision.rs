#![feature(register_tool)]
#![register_tool(charon)]

#[charon::rename("collision")]
pub fn left(value: u32) -> u32 {
    value
}

#[charon::rename("collision")]
pub fn right(value: u32) -> u32 {
    value
}
