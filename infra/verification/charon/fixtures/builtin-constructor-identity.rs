//! Source names must not capture constructors emitted by the Lean backend.

pub mod shadow {
    pub fn ok(value: u32) -> u32 {
        value
    }

    pub fn fail() -> u32 {
        panic!("explicit failure");
    }

    pub fn panic() -> u32 {
        panic!("explicit failure");
    }
}

pub mod capture {
    pub fn ok(_value: u32) -> u32 {
        panic!("poisoned constructor name");
    }

    pub fn identity(value: u32) -> u32 {
        value
    }
}

#[cfg(test)]
mod tests {
    use super::{capture, shadow};

    #[test]
    fn source_functions_keep_their_values() {
        for value in [0, 1, u32::MAX] {
            assert_eq!(shadow::ok(value), value);
            assert_eq!(capture::identity(value), value);
        }
    }

    #[test]
    #[should_panic(expected = "explicit failure")]
    fn function_named_fail_keeps_its_panic() {
        let _ = shadow::fail();
    }

    #[test]
    #[should_panic(expected = "explicit failure")]
    fn function_named_panic_keeps_its_panic() {
        let _ = shadow::panic();
    }
}
