//! Differentiability certified by `smooth(p, expr)`.
//!
//! MLS §3.7.5 states that `smooth(p, expr)` returns `expr` and that `expr` is
//! `p` times continuously differentiable in every real variable it reads. A
//! conditional inside it whose guard switches without an event therefore has
//! branch derivatives up to order `p` that agree wherever the guard changes,
//! so on every interval `d^k/dt^k (if c then a else b)` is
//! `if c then d^k a else d^k b` with the guard evaluated live, for `k <= p`.
//! The derivative of the whole operand keeps the remaining certificate:
//! `d^k/dt^k smooth(p, e) = smooth(p - k, d^k e)`.
//!
//! Past order `p` the certificate says nothing: the derivative may jump where
//! a guard changes. A conditional there differentiates only when its guards
//! are fixed between events (`parameter_conditionals`). This module is the
//! one owner of the certified bound: the constraint proof walk and the
//! expression differentiator both ask it.

use rumoca_ir_dae as dae;

/// The certified order `p` and the operand of a `smooth(p, expr)` call whose
/// order is an Integer literal. The DAE constructor proves the arity.
pub(super) fn smooth_operands<'dae>(
    view: dae::DaeView<'dae>,
    arguments: dae::ExpressionOperands<'dae>,
) -> Option<(u8, dae::ExprId<'dae>)> {
    let order = arguments.get(0)?;
    let value = arguments.get(1)?;
    let dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(order)) =
        view.expression(order)?.operation()
    else {
        return None;
    };
    let order = u8::try_from(*order).unwrap_or(if *order < 0 { 0 } else { u8::MAX });
    Some((order, value))
}

/// The order a derivative of order `order` of `smooth(p, ..)` still certifies.
pub(super) fn remaining_order(smoothness: u8, order: u8) -> u8 {
    smoothness.saturating_sub(order)
}

/// The certificate an operand of `smooth(smoothness, ..)` is walked under: the
/// larger of its own and the enclosing one. `None` is outside every `smooth`.
pub(super) fn certificate_within(enclosing: Option<u8>, smoothness: u8) -> Option<u8> {
    Some(enclosing.map_or(smoothness, |enclosing| enclosing.max(smoothness)))
}

/// Whether a conditional under the enclosing `smooth` certificate
/// differentiates branch-wise at `order` with its guard evaluated live: the
/// certificate covers the order, so the branch derivatives agree wherever the
/// guard changes.
pub(super) fn admits_live_guards(certificate: Option<u8>, order: u8) -> bool {
    certificate.is_some_and(|smoothness| order <= smoothness)
}

#[cfg(test)]
mod tests {
    use super::{admits_live_guards, certificate_within};

    #[test]
    fn a_nested_smooth_keeps_the_larger_certificate() {
        assert_eq!(certificate_within(None, 0), Some(0));
        assert_eq!(certificate_within(Some(2), 1), Some(2));
        assert_eq!(certificate_within(Some(1), 2), Some(2));
    }

    #[test]
    fn a_certificate_admits_live_guards_through_its_own_order() {
        assert!(!admits_live_guards(None, 1));
        assert!(!admits_live_guards(Some(0), 1));
        assert!(admits_live_guards(Some(1), 1));
        assert!(!admits_live_guards(Some(1), 2));
        assert!(admits_live_guards(Some(u8::MAX), 2));
    }
}
