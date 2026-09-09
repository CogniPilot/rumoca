fn bump(x: &u32) -> u32 { *x + 1 }
fn main() { let f = |x: &u32| bump(x) * 2; std::process::exit(f(&1) as i32); }
