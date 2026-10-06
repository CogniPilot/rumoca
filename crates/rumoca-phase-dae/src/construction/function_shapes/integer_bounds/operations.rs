//! Checked interval operations, never corner sampling of arbitrary syntax.

use super::*;
use rumoca_core::BuiltinFunction;

pub(super) fn multiply(lhs: (i64, i64), rhs: (i64, i64)) -> Option<(i64, i64)> {
    let products = [
        lhs.0.checked_mul(rhs.0)?,
        lhs.0.checked_mul(rhs.1)?,
        lhs.1.checked_mul(rhs.0)?,
        lhs.1.checked_mul(rhs.1)?,
    ];
    Some((*products.iter().min()?, *products.iter().max()?))
}

pub(super) fn builtin(
    shapes: &ShapeEnvironment,
    function: BuiltinFunction,
    args: &[Expression],
) -> Option<(i64, i64)> {
    let [lhs, rhs] = args else {
        return None;
    };
    let lhs = shapes.proven_integer_bounds(lhs)?;
    let rhs = shapes.proven_integer_bounds(rhs)?;
    match function {
        BuiltinFunction::Min => Some((lhs.0.min(rhs.0), lhs.1.min(rhs.1))),
        BuiltinFunction::Max => Some((lhs.0.max(rhs.0), lhs.1.max(rhs.1))),
        BuiltinFunction::Div if rhs.0 == rhs.1 && rhs.0 > 0 => {
            Some((lhs.0.checked_div(rhs.0)?, lhs.1.checked_div(rhs.0)?))
        }
        BuiltinFunction::Mod if rhs.0 == rhs.1 && rhs.0 > 0 => Some((0, rhs.0.checked_sub(1)?)),
        _ => None,
    }
}
