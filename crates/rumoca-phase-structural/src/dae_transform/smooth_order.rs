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
