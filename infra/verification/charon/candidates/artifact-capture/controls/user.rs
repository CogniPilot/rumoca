//! Dependent crate reaching the library's closure.
pub fn use_it(x: &u32) -> u32 { dep::apply(dep::make(), x) }
