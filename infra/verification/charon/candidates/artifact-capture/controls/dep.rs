//! Library that defines a closure a dependent will reach.
pub fn make() -> impl Fn(&u32) -> u32 { |x| *x + 1 }
pub fn apply<F: Fn(&u32) -> u32>(f: F, x: &u32) -> u32 { f(x) }
