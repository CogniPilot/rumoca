//! Control: no floating-point type anywhere. The translator must emit no
//! external-types seam for this crate, and its manifest must list no float
//! dependency, so the registration fires only on a typed float occurrence.

pub enum Literal {
    Integer(i64),
    Flag(bool),
}

pub struct Entry {
    pub expression: u32,
    pub value: i64,
}

pub fn integer(value: i64) -> Literal {
    Literal::Integer(value)
}

pub fn entry_value(entry: &Entry) -> i64 {
    entry.value
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn values_pass_through_untouched() {
        assert!(matches!(integer(-7), Literal::Integer(-7)));
        assert_eq!(
            entry_value(&Entry {
                expression: 3,
                value: 9
            }),
            9
        );
    }
}
