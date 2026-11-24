//! The literal Integer exponent of an elementwise Real power.
//!
//! [`IntegerPower`] is the plan of `rumoca_core::modelica_integer_power` for
//! one source exponent: the base is replaced by its reciprocal `1 / x` when
//! the exponent is negative, then the magnitude is expanded into the
//! square-and-multiply products of the canonical rule, in its order. Every
//! backend of [`crate::LinearOp::TensorIntegerPower`] walks these steps, so an
//! element is the value of the scalar plan bit for bit. The native
//! evaluators, Cranelift, wasm and the generated C keep two live values per
//! element whatever the exponent; the scalar expansion unrolls one register
//! per product, and inline WGSL rows one expression per product.

use serde::{Deserialize, Serialize};

/// One step of an [`IntegerPower`] plan over a running `result` and a
/// running `base`, both starting at the (possibly inverted) element.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerPowerStep {
    /// `result = base`, the first set bit of the magnitude: no arithmetic.
    Seed,
    /// `result = result * base`, the left operand being `result`.
    Multiply,
    /// `base = base * base`.
    Square,
}

/// A literal Integer exponent of magnitude at least 2: the exponents whose
/// plan has a product. Exponent 0 is the constant 1, exponent 1 the base
/// itself, and exponent -1 the single quotient `1 / x`, which an elementwise
/// `TensorBinary` division already is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "i64", into = "i64")]
pub struct IntegerPower {
    exponent: i64,
}

impl IntegerPower {
    /// The plan of `x ^ exponent`, or `None` for an exponent of magnitude
    /// below 2, whose plan has no product.
    #[must_use]
    pub fn new(exponent: i64) -> Option<Self> {
        (exponent.unsigned_abs() >= 2).then_some(Self { exponent })
    }

    #[must_use]
    pub fn exponent(self) -> i64 {
        self.exponent
    }

    /// Whether the element is first replaced by `1 / x`.
    #[must_use]
    pub fn reciprocal(self) -> bool {
        self.exponent < 0
    }

    /// The unsigned magnitude, so the minimum signed exponent is representable.
    #[must_use]
    pub fn magnitude(self) -> u64 {
        self.exponent.unsigned_abs()
    }

    /// The steps of the canonical rule, in order, without allocating. The
    /// result is `result` after the last step.
    #[must_use]
    pub fn steps(self) -> IntegerPowerSteps {
        let magnitude = self.magnitude();
        IntegerPowerSteps {
            magnitude,
            bits: u64::BITS - magnitude.leading_zeros(),
            seed: magnitude.trailing_zeros(),
            bit: 0,
            at_square: false,
        }
    }

    /// Most products one plan computes: `i64::MAX` has 62 squares and 62
    /// multiplications.
    pub const MAX_PRODUCTS: usize = 124;

    /// The products of the plan over value indices, without allocating:
    /// value 0 is the (inverted) element and product `k` defines value
    /// `k + 1` as `values[lhs] * values[rhs]`. The last value is the result.
    pub fn product_pairs(self) -> impl Iterator<Item = (usize, usize)> {
        let (mut base, mut result, mut defined) = (0, 0, 0);
        self.steps().filter_map(move |step| {
            let pair = match step {
                IntegerPowerStep::Seed => {
                    result = base;
                    return None;
                }
                IntegerPowerStep::Multiply => (result, base),
                IntegerPowerStep::Square => (base, base),
            };
            defined += 1;
            match step {
                IntegerPowerStep::Square => base = defined,
                _ => result = defined,
            }
            Some(pair)
        })
    }

    /// [`Self::product_pairs`], collected.
    #[must_use]
    pub fn products(self) -> Vec<(usize, usize)> {
        self.product_pairs().collect()
    }

    /// Number of products the plan computes per element, the reciprocal
    /// excluded: `O(log |n|)`, between 1 and 124 (`i64::MAX`, with 62
    /// squares and 62 multiplications).
    #[must_use]
    pub fn product_count(self) -> usize {
        self.steps()
            .filter(|step| *step != IntegerPowerStep::Seed)
            .count()
    }
}

/// The steps of one [`IntegerPower`] plan: for each bit of the magnitude
/// from the lowest, `Seed` or `Multiply` when the bit is set, then `Square`
/// unless it is the highest bit.
#[derive(Debug, Clone)]
pub struct IntegerPowerSteps {
    magnitude: u64,
    bits: u32,
    seed: u32,
    bit: u32,
    at_square: bool,
}

impl IntegerPowerSteps {
    fn odd(&self, bit: u32) -> Option<IntegerPowerStep> {
        ((self.magnitude >> bit) & 1 == 1).then_some(if bit == self.seed {
            IntegerPowerStep::Seed
        } else {
            IntegerPowerStep::Multiply
        })
    }

    fn square(&self, bit: u32) -> Option<IntegerPowerStep> {
        (bit + 1 < self.bits).then_some(IntegerPowerStep::Square)
    }
}

impl Iterator for IntegerPowerSteps {
    type Item = IntegerPowerStep;

    fn next(&mut self) -> Option<IntegerPowerStep> {
        while self.bit < self.bits {
            let bit = self.bit;
            let step = if self.at_square {
                self.square(bit)
            } else {
                self.odd(bit)
            };
            self.bit += u32::from(self.at_square);
            self.at_square = !self.at_square;
            if step.is_some() {
                return step;
            }
        }
        None
    }
}

impl TryFrom<i64> for IntegerPower {
    type Error = String;

    fn try_from(exponent: i64) -> Result<Self, Self::Error> {
        Self::new(exponent).ok_or_else(|| {
            format!("integer power exponent {exponent} has no product (its magnitude is below 2)")
        })
    }
}

impl From<IntegerPower> for i64 {
    fn from(power: IntegerPower) -> Self {
        power.exponent
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The value-index form of the plan, as `LiteralPower::steps` lowered it
    /// into scalar products: value 0 is the base, step `k` defines value
    /// `k + 1` as the product of two earlier values.
    fn indexed_steps(magnitude: u64) -> Vec<(usize, usize)> {
        let mut steps = Vec::new();
        let mut base = 0;
        let mut result: Option<usize> = None;
        let mut remaining = magnitude;
        while remaining != 0 {
            if !remaining.is_multiple_of(2) {
                result = Some(multiply_into(&mut steps, result, base));
            }
            remaining /= 2;
            if remaining != 0 {
                steps.push((base, base));
                base = steps.len();
            }
        }
        steps
    }

    fn multiply_into(steps: &mut Vec<(usize, usize)>, result: Option<usize>, base: usize) -> usize {
        let Some(product) = result else {
            return base;
        };
        steps.push((product, base));
        steps.len()
    }

    /// The steps walked in Real arithmetic.
    fn walk(power: IntegerPower, element: f64) -> f64 {
        let mut base = if power.reciprocal() {
            1.0 / element
        } else {
            element
        };
        let mut result = base;
        for step in power.steps() {
            match step {
                IntegerPowerStep::Seed => result = base,
                IntegerPowerStep::Multiply => result *= base,
                IntegerPowerStep::Square => base *= base,
            }
        }
        result
    }

    fn exponents() -> Vec<i64> {
        let mut exponents = vec![i64::MIN, i64::MIN + 1, i64::MAX, i64::MAX - 1];
        exponents.extend((-70..=70).filter(|n: &i64| n.unsigned_abs() >= 2));
        exponents.extend((2..63).flat_map(|k| [(1i64 << k) - 1, 1i64 << k, -(1i64 << k)]));
        exponents
    }

    #[test]
    fn steps_are_the_products_of_the_scalar_plan_in_order() {
        let widest = IntegerPower::new(i64::MAX).expect("i64::MAX");
        assert_eq!(widest.product_count(), IntegerPower::MAX_PRODUCTS);
        for exponent in exponents() {
            let power = IntegerPower::new(exponent).expect("exponent has a product");
            assert_eq!(
                power.products(),
                indexed_steps(power.magnitude()),
                "exponent {exponent}"
            );
            assert_eq!(
                power.product_count(),
                indexed_steps(power.magnitude()).len()
            );
        }
    }

    #[test]
    fn the_steps_walk_the_canonical_value_rule_bit_for_bit() {
        let bases = [
            0.0,
            -0.0,
            1.0,
            -1.0,
            0.5,
            -1.75,
            1.000_000_1,
            3.0e-5,
            -7.25e3,
            f64::MIN_POSITIVE,
            f64::INFINITY,
            f64::NEG_INFINITY,
        ];
        for exponent in exponents() {
            let power = IntegerPower::new(exponent).expect("exponent has a product");
            for base in bases {
                let expected = rumoca_core::modelica_integer_power(base, exponent);
                let actual = walk(power, base);
                assert!(
                    expected.to_bits() == actual.to_bits()
                        || (expected.is_nan() && actual.is_nan()),
                    "{base} ^ {exponent}: {actual} != {expected}"
                );
            }
        }
    }

    #[test]
    fn exponents_without_a_product_are_refused_and_do_not_deserialize() {
        for exponent in [-1, 0, 1] {
            assert_eq!(IntegerPower::new(exponent), None);
            assert!(serde_json::from_str::<IntegerPower>(&exponent.to_string()).is_err());
        }
        let power = IntegerPower::new(-3).expect("negative exponent");
        let text = serde_json::to_string(&power).expect("serialize");
        assert_eq!(
            serde_json::from_str::<IntegerPower>(&text).ok(),
            Some(power)
        );
    }

    /// Every backend writes an element as soon as it is computed, so a
    /// destination that overlaps the source is refused at construction.
    #[test]
    fn a_destination_overlapping_its_source_is_refused() {
        use crate::{LinearOp, ScalarProgramRegisterFlow, TensorInputKind};
        let power = IntegerPower::new(2).expect("square");
        let program = |dst_start| {
            vec![
                LinearOp::TensorLoad {
                    dst_start: 0,
                    input: TensorInputKind::Y,
                    input_start: 0,
                    count: 2,
                    seed_start: None,
                    lanes: 1,
                },
                LinearOp::TensorIntegerPower {
                    dst_start,
                    src_start: 0,
                    count: 2,
                    power,
                    lanes: 1,
                },
                LinearOp::StoreOutputRange {
                    start: dst_start,
                    count: 2,
                    stride: 1,
                },
            ]
        };
        assert!(ScalarProgramRegisterFlow::derive(&program(2)).is_ok());
        for dst_start in [0, 1] {
            let error = ScalarProgramRegisterFlow::derive(&program(dst_start))
                .expect_err("an overlapping destination refuses");
            assert!(
                error.to_string().contains("overlaps its source"),
                "{dst_start}: {error}"
            );
        }
    }

    /// An extent whose range leaves the register space is a validation
    /// error, never an arithmetic overflow, wherever the destination lies.
    #[test]
    fn an_extent_past_the_register_space_is_refused_without_overflow() {
        use crate::{LinearOp, ScalarProgramRegisterFlow};
        let power = IntegerPower::new(3).expect("cube");
        for (dst_start, src_start) in [(0, 1), (2, 1), (u32::MAX, 1), (1, u32::MAX)] {
            let program = [
                LinearOp::Const { dst: 0, value: 2.0 },
                LinearOp::TensorIntegerPower {
                    dst_start,
                    src_start,
                    count: usize::MAX,
                    power,
                    lanes: 1,
                },
            ];
            assert!(ScalarProgramRegisterFlow::derive(&program).is_err());
        }
    }
}
