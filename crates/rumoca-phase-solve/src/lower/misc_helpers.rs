use super::*;

pub(super) fn static_singleton_subscript_index(
    expr: &rumoca_core::Expression,
) -> Result<Option<usize>, LowerError> {
    lower_static_index_expr(expr)
}

pub(super) fn direct_assignment_component(
    values: &[Reg],
    flat_index: usize,
    repeat_period: Option<usize>,
) -> Option<Reg> {
    values.get(flat_index).copied().or_else(|| {
        let period = repeat_period?;
        (period > 0 && values.len() == period).then(|| values[flat_index % period])
    })
}

pub(super) fn complex_operator_call_op(name: &str) -> Option<BinaryOp> {
    match name {
        "Complex.'+'" => Some(BinaryOp::Add),
        "Complex.'-'" => Some(BinaryOp::Sub),
        "Complex.'*'" => Some(BinaryOp::Mul),
        "Complex.'/'" => Some(BinaryOp::Div),
        _ => None,
    }
}

pub(super) fn is_time_var_ref(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            ..
        } if name.as_str() == "time" && subscripts.is_empty()
    )
}

pub(super) fn size_binding_key(name: &str, dim: usize) -> String {
    format!("{SIZE_BINDING_PREFIX}{name}.{dim}")
}
