//! Control: a floating-point width with no reviewed model. The translator must
//! name the dependency explicitly (`F32` in the template and the manifest) so
//! that its absence is a decision the consumer can see and refuse, not a bare
//! identifier that fails elaboration.

pub struct Sample {
    pub index: u32,
    pub value: f32,
}

pub fn sample_value(sample: &Sample) -> f32 {
    sample.value
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_passes_through_untouched() {
        let bits = 0x4049_0FDB_u32;
        let x = f32::from_bits(bits);
        assert_eq!(sample_value(&Sample { index: 1, value: x }).to_bits(), bits);
    }
}
