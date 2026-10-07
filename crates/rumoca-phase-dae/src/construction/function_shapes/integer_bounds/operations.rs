//! Checked interval operations, never corner sampling of arbitrary syntax.

use super::*;
use rumoca_core::BuiltinFunction;

pub(super) fn builtin(
    shapes: &ShapeEnvironment,
    function: BuiltinFunction,
    args: &[Expression],
) -> IntegerInterval {
    let [lhs, rhs] = args else {
        return IntegerInterval::UNBOUNDED;
    };
    let lhs = shapes.proven_integer_interval(lhs);
    let rhs = shapes.proven_integer_interval(rhs);
    let positive_constant = rhs
        .bounds()
        .filter(|(lower, upper)| lower == upper && *lower > 0)
        .map(|(divisor, _)| divisor);
    match (function, positive_constant) {
        (BuiltinFunction::Min, _) => lhs.minimum(rhs),
        (BuiltinFunction::Max, _) => lhs.maximum(rhs),
        (BuiltinFunction::Div, Some(divisor)) => lhs.divided_by(divisor),
        (BuiltinFunction::Mod, Some(divisor)) => IntegerInterval::finite(0, divisor - 1),
        _ => IntegerInterval::UNBOUNDED,
    }
}
