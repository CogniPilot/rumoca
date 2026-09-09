//! Loop constructors must remain distinct from source functions with the same names.

pub fn cont(value: bool) -> bool {
    value
}

pub fn done(value: u32) -> u32 {
    value
}

pub fn once(mut again: bool, value: u32) -> u32 {
    loop {
        if !again {
            return done(value);
        }
        again = cont(false);
    }
}

#[cfg(test)]
mod tests {
    use super::once;

    #[test]
    fn both_loop_paths_preserve_the_payload() {
        for again in [false, true] {
            for value in [0, 1, u32::MAX] {
                assert_eq!(once(again, value), value);
            }
        }
    }
}
