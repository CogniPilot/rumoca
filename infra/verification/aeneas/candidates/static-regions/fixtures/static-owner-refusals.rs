const MUTABLE_EMPTY: [&mut u32; 0] = [];

pub struct MutableField {
    pub value: Option<&'static mut u32>,
}

const MUTABLE_WRAPPED: MutableField = MutableField { value: None };

pub struct NonstaticField<'a> {
    pub value: Option<&'a u32>,
}

impl<'a> NonstaticField<'a> {
    pub const EMPTY: Self = Self { value: None };
}

pub fn mutable_empty() -> [&'static mut u32; 0] {
    MUTABLE_EMPTY
}

pub fn mutable_wrapped() -> MutableField {
    MUTABLE_WRAPPED
}

pub fn nonstatic<'a>() -> NonstaticField<'a> {
    NonstaticField::EMPTY
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_rust_but_outside_the_static_shared_profile() {
        assert!(mutable_empty().is_empty());
        assert!(mutable_wrapped().value.is_none());
        assert!(nonstatic().value.is_none());
    }
}
