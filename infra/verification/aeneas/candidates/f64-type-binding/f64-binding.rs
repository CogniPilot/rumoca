//! Reduced shape of the DAE root's `f64` fields at the Lean boundary.
//!
//! `Literal::Real` and `Entry::value` carry an `f64` exactly as
//! `rumoca_core::ir_primitives::Literal::Real`, `DaeLiteral::Real` and
//! `PositiveParameterEntry::value` do. The functions only move the value; no
//! floating-point arithmetic is performed, so a storage-only model of `f64`
//! is the only obligation the translation creates.

pub enum Literal {
    Real(f64),
    Integer(i64),
}

pub struct Entry {
    pub expression: u32,
    pub value: f64,
}

pub fn real(value: f64) -> Literal {
    Literal::Real(value)
}

pub fn entry_value(entry: &Entry) -> f64 {
    entry.value
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn values_pass_through_untouched() {
        let bits = 0x4009_21FB_5444_2D18_u64;
        let x = f64::from_bits(bits);
        assert!(matches!(real(x), Literal::Real(v) if v.to_bits() == bits));
        assert_eq!(
            entry_value(&Entry {
                expression: 3,
                value: x
            })
            .to_bits(),
            bits
        );
    }
}
