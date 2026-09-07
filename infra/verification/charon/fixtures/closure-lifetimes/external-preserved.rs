extern crate preserved_signature;

pub fn identity(value: &u32) -> &u32 {
    preserved_signature::unrelated_outer()(value)
}

pub fn capture(value: &u32) -> &u32 {
    preserved_signature::captured_move(value)
}
