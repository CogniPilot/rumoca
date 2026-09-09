pub fn make() -> impl Fn(&u32) -> u32 { |x| *x + undefined_name }
