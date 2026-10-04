//! The one arithmetic form of an affine, additive, or reciprocal isolated value, read by
//! the materialized isolator and by the evaluator's per-row isolation alike.
//!
//! The isolated value is `-offset / coefficient`, with `offset` the sum of
//! the scaled offset terms. The form keeps every operation whose rounding
//! matters and drops the exact identities: a unit scale (`1 * r = r`,
//! `-1 * r = -r`), a unit coefficient (`-x / 1 = -x`, `-x / -1 = x`), and a
//! power-of-two coefficient, whose reciprocal is exact so `-x / c` equals
//! `x * (-1 / c)` bit for bit. The sum starts from its first term rather than
//! from `+0`, so an offset that is `-0` stays `-0`; that sign of zero is the
//! only difference from the plain `-(0 + ...) / c` form. A register
//! coefficient `scale * r` is `r` itself when the scale is exactly 1.

use crate::{Reg, TargetAssignmentShape};

/// One term of the offset sum.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IsolatedTerm {
    /// `r`
    Register(Reg),
    /// `-r`
    Negated(Reg),
    /// `scale * r`
    Scaled(Reg, f64),
}

/// How the offset sum `x` becomes the isolated value.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IsolatedDivisor {
    /// `-x` (coefficient exactly 1).
    Negate,
    /// `x` (coefficient exactly -1).
    Keep,
    /// `x * factor` with `factor = -1 / c` exact (a power-of-two coefficient).
    Multiply(f64),
    /// `-x / c` for any other constant coefficient, singular ones included.
    Divide(f64),
    /// `-x / k` with `k = scale * r`, or `r` when the scale is exactly 1; the
    /// materialized program poisons the value when `k` is not finite.
    DivideRegister { register: Reg, scale: f64 },
}

/// The isolated value of an affine or additive shape.
#[derive(Clone, Debug, PartialEq)]
pub struct IsolatedValue {
    pub terms: Vec<IsolatedTerm>,
    pub divisor: IsolatedDivisor,
}

/// The offset terms of an isolated value in sum order, read from the shape
/// without allocating.
#[derive(Clone, Debug)]
pub enum IsolatedTerms<'a> {
    /// The single offset term of an affine or reciprocal shape.
    One(Option<IsolatedTerm>),
    /// The `(register, scale)` offset terms of an additive shape.
    Scaled(std::slice::Iter<'a, (Reg, f64)>),
}

impl Iterator for IsolatedTerms<'_> {
    type Item = IsolatedTerm;

    fn next(&mut self) -> Option<IsolatedTerm> {
        match self {
            Self::One(term) => term.take(),
            Self::Scaled(terms) => terms.next().map(|&(register, scale)| term(register, scale)),
        }
    }
}

impl IsolatedValue {
    /// The form of an `Affine`, `Additive`, or `Reciprocal` shape; `None` for
    /// the others.
    #[must_use]
    pub fn of(shape: &TargetAssignmentShape) -> Option<Self> {
        let (terms, divisor) = isolated_parts(shape)?;
        Some(fold_single_term(terms.collect(), divisor))
    }
}

/// The offset terms and divisor of an `Affine`, `Additive`, or `Reciprocal`
/// shape before the single-term fold of [`IsolatedValue::of`]; `None` for the
/// other shapes. Both read as the same value bit for bit: the fold is an exact
/// identity.
#[must_use]
pub fn isolated_parts(
    shape: &TargetAssignmentShape,
) -> Option<(IsolatedTerms<'_>, IsolatedDivisor)> {
    Some(match shape {
        TargetAssignmentShape::Affine {
            offset_reg,
            coefficient_reg,
            offset_scale,
            coefficient_scale,
            ..
        } => (
            IsolatedTerms::One(Some(term(*offset_reg, *offset_scale))),
            match coefficient_reg {
                Some(register) => IsolatedDivisor::DivideRegister {
                    register: *register,
                    scale: *coefficient_scale,
                },
                None => constant_divisor(*coefficient_scale),
            },
        ),
        TargetAssignmentShape::Additive {
            offset_terms,
            coefficient,
            ..
        } => (
            IsolatedTerms::Scaled(offset_terms.iter()),
            constant_divisor(*coefficient),
        ),
        TargetAssignmentShape::Reciprocal {
            numerator_reg,
            numerator_scale,
            divisor_reg,
            divisor_scale,
            ..
        } => (
            IsolatedTerms::One(Some(term(*numerator_reg, *numerator_scale))),
            IsolatedDivisor::DivideRegister {
                register: *divisor_reg,
                scale: *divisor_scale,
            },
        ),
        _ => return None,
    })
}

fn term(register: Reg, scale: f64) -> IsolatedTerm {
    if scale == 1.0 {
        IsolatedTerm::Register(register)
    } else if scale == -1.0 {
        IsolatedTerm::Negated(register)
    } else {
        IsolatedTerm::Scaled(register, scale)
    }
}

fn constant_divisor(coefficient: f64) -> IsolatedDivisor {
    if coefficient == 1.0 {
        IsolatedDivisor::Negate
    } else if coefficient == -1.0 {
        IsolatedDivisor::Keep
    } else if exact_power_of_two(coefficient) {
        IsolatedDivisor::Multiply(-1.0 / coefficient)
    } else {
        IsolatedDivisor::Divide(coefficient)
    }
}

/// A normal power of two, whose reciprocal is exact and normal or subnormal
/// without rounding, so multiplying by it rounds exactly as dividing does.
fn exact_power_of_two(value: f64) -> bool {
    const MANTISSA: u64 = (1 << 52) - 1;
    value.is_normal() && value.to_bits() & MANTISSA == 0
}

/// Fold the sign of a negating or keeping divisor into a lone term: the
/// negation of a rounded product is the product with the negated scale.
fn fold_single_term(terms: Vec<IsolatedTerm>, divisor: IsolatedDivisor) -> IsolatedValue {
    let negate = match (terms.as_slice(), divisor) {
        ([_], IsolatedDivisor::Negate) => true,
        ([_], IsolatedDivisor::Keep) => false,
        _ => return IsolatedValue { terms, divisor },
    };
    let folded = match (terms[0], negate) {
        (term, false) => term,
        (IsolatedTerm::Register(register), true) => IsolatedTerm::Negated(register),
        (IsolatedTerm::Negated(register), true) => IsolatedTerm::Register(register),
        (IsolatedTerm::Scaled(register, scale), true) => IsolatedTerm::Scaled(register, -scale),
    };
    IsolatedValue {
        terms: vec![folded],
        divisor: IsolatedDivisor::Keep,
    }
}

#[cfg(test)]
mod tests;
