//! Evaluation of the isolated value form of an affine, additive, or
//! reciprocal target assignment ([`rumoca_ir_solve::IsolatedValue`]), in the
//! operation order its materialized isolator emits, so both agree bit for bit.

use rumoca_ir_solve::{IsolatedDivisor, IsolatedTerm, Reg, TargetAssignmentShape, isolated_parts};

/// The isolated value of an `Affine`, `Additive`, or `Reciprocal` shape over
/// register values, without building its form; `None` for the other shapes.
pub(super) fn eval_isolated_value<E>(
    shape: &TargetAssignmentShape,
    read: impl FnMut(Reg) -> Result<f64, E>,
) -> Option<Result<f64, E>> {
    let (terms, divisor) = isolated_parts(shape)?;
    Some(eval_isolated_parts(terms, divisor, read))
}

/// The value of offset `terms` summed from the first term, then divided as
/// `divisor` says.
pub(super) fn eval_isolated_parts<E>(
    terms: impl Iterator<Item = IsolatedTerm>,
    divisor: IsolatedDivisor,
    mut read: impl FnMut(Reg) -> Result<f64, E>,
) -> Result<f64, E> {
    // The sum starts from its first term, so a `-0` offset keeps its sign; an
    // empty offset sum is `+0`, the constant the materialized isolator emits.
    let mut sum = 0.0;
    for (index, term) in terms.enumerate() {
        let value = match term {
            IsolatedTerm::Register(register) => read(register)?,
            IsolatedTerm::Negated(register) => -read(register)?,
            IsolatedTerm::Scaled(register, scale) => scale * read(register)?,
        };
        sum = if index == 0 { value } else { sum + value };
    }
    Ok(match divisor {
        IsolatedDivisor::Negate => -sum,
        IsolatedDivisor::Keep => sum,
        IsolatedDivisor::Multiply(factor) => sum * factor,
        IsolatedDivisor::Divide(coefficient) => -sum / coefficient,
        IsolatedDivisor::DivideRegister { register, scale } => {
            -sum / register_coefficient(read(register)?, scale)
        }
    })
}

/// The value of a register coefficient under its scale.
#[must_use]
pub(super) fn register_coefficient(value: f64, scale: f64) -> f64 {
    if scale == 1.0 { value } else { scale * value }
}
