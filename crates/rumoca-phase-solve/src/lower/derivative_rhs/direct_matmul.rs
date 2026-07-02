use super::{
    DerivativeRhsAnalysis, DerivativeRhsLoweringContext, LowerError, Scope,
    active_assignment_stack, derivative_rhs_expr_or_owner_span, inline_direct_assignment_expr,
    row_builder, shared_vector_rhs_base,
};
use rumoca_core::{Literal, OpBinary};
use rumoca_ir_solve::ComputeNode;

pub(super) fn lower_direct_row_group_matmul(
    analysis: &DerivativeRhsAnalysis,
    start: usize,
    group_len: usize,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<Option<ComputeNode>, LowerError> {
    if !direct_row_group_has_unit_coefficients(analysis, start, group_len) {
        return Ok(None);
    }
    let Some(base) = direct_row_group_inlined_base(analysis, start, ctx)? else {
        return Ok(None);
    };
    let rumoca_core::Expression::Binary {
        op: OpBinary::Mul,
        lhs,
        rhs,
        span,
    } = &base
    else {
        return Ok(None);
    };

    let mut builder = row_builder(
        ctx.dae_model,
        ctx.layout,
        ctx.direct_assignments,
        ctx.structural_bindings,
        ctx.indexed_bindings,
    );
    let scope = Scope::new();
    let lhs_dims = match builder.infer_expr_dims(lhs, &scope) {
        Ok(dims) => dims,
        Err(err) if direct_group_tensor_probe_can_fallback(&err) => return Ok(None),
        Err(err) => return Err(err),
    };
    let rhs_dims = match builder.infer_expr_dims(rhs, &scope) {
        Ok(dims) => dims,
        Err(err) if direct_group_tensor_probe_can_fallback(&err) => return Ok(None),
        Err(err) => return Err(err),
    };
    let Some(shape) =
        super::super::array_values::matmul_shape_from_dims(&lhs_dims, &rhs_dims, group_len)
    else {
        return Ok(None);
    };

    let head = &analysis.states[start];
    let head_eq = &analysis.equations[analysis.direct_equations[&head.name]];
    let matmul_span = if span.is_dummy() { head_eq.span } else { *span };
    if matmul_span.is_dummy() {
        return Err(LowerError::UnspannedContractViolation {
            reason: "direct derivative MatMul lowering requires a source span".to_string(),
        });
    }
    builder
        .with_optional_source_context(Some(matmul_span), |builder| {
            builder.build_matmul_node(lhs, rhs, matmul_span, &scope, 0, shape)
        })
        .map(Some)
}

fn direct_row_group_has_unit_coefficients(
    analysis: &DerivativeRhsAnalysis,
    start: usize,
    group_len: usize,
) -> bool {
    (start..start + group_len).all(|idx| {
        let state = &analysis.states[idx];
        let equation = &analysis.equations[analysis.direct_equations[&state.name]];
        equation
            .coefficients
            .get(&state.name)
            .is_some_and(is_one_literal)
    })
}

fn is_one_literal(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::Literal {
            value: Literal::Real(value),
            ..
        } if *value == 1.0
    ) || matches!(
        expr,
        rumoca_core::Expression::Literal {
            value: Literal::Integer(value),
            ..
        } if *value == 1
    )
}

fn direct_row_group_inlined_base(
    analysis: &DerivativeRhsAnalysis,
    start: usize,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let head = &analysis.states[start];
    let head_eq = &analysis.equations[analysis.direct_equations[&head.name]];
    let head_base = shared_vector_rhs_base(&head_eq.rhs);
    let span = derivative_rhs_expr_or_owner_span(head_base, head_eq.span)?;
    let mut active_assignments = active_assignment_stack(span)?;
    match inline_direct_assignment_expr(head_base, ctx, &mut active_assignments) {
        Ok(expr) => Ok(Some(expr)),
        Err(err) if direct_group_tensor_probe_can_fallback(&err) => Ok(None),
        Err(err) => Err(err),
    }
}

fn direct_group_tensor_probe_can_fallback(err: &LowerError) -> bool {
    matches!(
        err,
        LowerError::Unsupported { .. }
            | LowerError::UnsupportedAt { .. }
            | LowerError::MissingBinding { .. }
            | LowerError::DynamicSubscript
    )
}
