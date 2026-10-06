//! Preliminary declaration-owned scalar signature; DAE owns actual call shape.

use super::*;

pub(super) fn fixed_scalar(context: &CaptureContext<'_>, expression: &ast::Expression) -> bool {
    if let ast::Expression::Parenthesized { inner, .. } = expression {
        return fixed_scalar(context, inner);
    }
    let ast::Expression::FunctionCall { comp, .. } = expression else {
        return false;
    };
    context
        .ctx
        .function_result_shapes
        .scalar_input_dimensions(comp.target_def_id())
        .is_some()
}
