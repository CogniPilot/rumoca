//! One private codec for native lane bytes, independent of execution authority.

use rumoca_ir_solve::{NativeOutputLane, SolveValueKind};

pub(super) fn bits(lanes: &[u64], offset: usize, lane: NativeOutputLane) -> u64 {
    let word = lanes[offset / 8];
    match lane {
        NativeOutputLane::Real | NativeOutputLane::Integer => word,
        NativeOutputLane::Boolean => (word >> ((offset % 8) * 8)) & 0xff,
    }
}

pub(super) fn decode(bits: u64, lane: NativeOutputLane) -> Option<SolveValueKind> {
    match lane {
        NativeOutputLane::Real => Some(SolveValueKind::Real64(bits)),
        NativeOutputLane::Integer => Some(SolveValueKind::Integer(i64::from_le_bytes(
            bits.to_le_bytes(),
        ))),
        NativeOutputLane::Boolean if bits <= 1 => Some(SolveValueKind::Boolean(bits == 1)),
        NativeOutputLane::Boolean => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exact_integer_bits_and_boolean_bytes_share_one_codec() {
        for value in [i64::MIN, -(1_i64 << 53) - 1, (1_i64 << 53) + 1, i64::MAX] {
            let lanes = [u64::from_le_bytes(value.to_le_bytes())];
            assert_eq!(
                decode(
                    bits(&lanes, 0, NativeOutputLane::Integer),
                    NativeOutputLane::Integer
                ),
                Some(SolveValueKind::Integer(value))
            );
        }
        let lanes = [0x0002_0100];
        assert_eq!(
            decode(
                bits(&lanes, 0, NativeOutputLane::Boolean),
                NativeOutputLane::Boolean
            ),
            Some(SolveValueKind::Boolean(false))
        );
        assert_eq!(
            decode(
                bits(&lanes, 1, NativeOutputLane::Boolean),
                NativeOutputLane::Boolean
            ),
            Some(SolveValueKind::Boolean(true))
        );
        assert_eq!(
            decode(
                bits(&lanes, 2, NativeOutputLane::Boolean),
                NativeOutputLane::Boolean
            ),
            None
        );
    }
}
