#![feature(register_tool)]
#![register_tool(verify)]

#[verify::opaque]
pub struct Shared<'a> {
    pub value: &'a u32,
}

#[verify::opaque]
pub struct Mutable<'a> {
    pub value: &'a mut u32,
}

pub fn opaque_shared(value: Shared<'static>) -> Shared<'static> {
    value
}

pub fn opaque_ordinary_mut<'a>(value: Mutable<'a>) -> Mutable<'a> {
    value
}

pub fn opaque_static_mut(value: Mutable<'static>) -> Mutable<'static> {
    value
}
