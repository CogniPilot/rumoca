use indexmap::IndexMap;
use rumoca_core::OpBinary;
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{BinaryOp, Reg};

use crate::lower::{
    LowerBuilder, LowerError, Scope, derivative_rhs, helpers::format_usize_dims, unsupported_at,
};

pub(super) fn scalarized_tuple_residual_operands(
    target: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    scalar_count: usize,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    builder: &mut LowerBuilder<'_>,
    span: rumoca_core::Span,
) -> Result<Option<Vec<Reg>>, LowerError> {
    let scope = Scope::new();
    let lhs_values =
        builder.lower_array_like_values_with_source_context(target, span, &scope, 0)?;
    let Some(rhs_values) = derivative_rhs::function_call_projected_scalars_with_owner(
        rhs,
        dae_model,
        structural_bindings,
        span,
    )?
    .or(derivative_rhs::project_array_like_scalars_with_owner(
        rhs,
        dae_model,
        structural_bindings,
        span,
    )?) else {
        return Ok(None);
    };
    if lhs_values.len() != scalar_count || rhs_values.len() != scalar_count {
        return Ok(None);
    }
    let mut values = super::expression_vec_with_capacity(
        scalar_count,
        "scalarized tuple residual value count",
        span,
    )?;
    for (lhs, rhs) in lhs_values.into_iter().zip(rhs_values) {
        let rhs = builder.lower_expr_with_source_context(&rhs, span, &scope, 0)?;
        values.push(builder.emit_binary_at(BinaryOp::Sub, lhs, rhs, span)?);
    }
    Ok(Some(values))
}

pub(super) fn reject_array_denominator_division(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<(), LowerError> {
    match expr {
        rumoca_core::Expression::Binary {
            op: OpBinary::Div,
            lhs,
            rhs,
            span,
        } => {
            let lhs_dims =
                derivative_rhs::expression_result_dims(lhs, dae_model, structural_bindings, *span)?;
            let rhs_dims =
                derivative_rhs::expression_result_dims(rhs, dae_model, structural_bindings, *span)?;
            if !rhs_dims.is_empty() {
                return Err(unsupported_at(
                    format!(
                        "array division requires a scalar denominator \
                         (lhs_shape={}, lhs_values={}, rhs_shape={}, rhs_values={})",
                        format_usize_dims(&lhs_dims),
                        dims_value_count(&lhs_dims, *span)?,
                        format_usize_dims(&rhs_dims),
                        dims_value_count(&rhs_dims, *span)?,
                    ),
                    *span,
                ));
            }
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            reject_array_denominator_division(lhs, dae_model, structural_bindings)?;
            reject_array_denominator_division(rhs, dae_model, structural_bindings)?;
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            reject_array_denominator_division(rhs, dae_model, structural_bindings)?;
        }
        rumoca_core::Expression::FieldAccess { base, .. }
        | rumoca_core::Expression::Index { base, .. } => {
            reject_array_denominator_division(base, dae_model, structural_bindings)?;
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            for element in elements {
                reject_array_denominator_division(element, dae_model, structural_bindings)?;
            }
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            for arg in args {
                reject_array_denominator_division(arg, dae_model, structural_bindings)?;
            }
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (_, branch_expr) in branches {
                reject_array_denominator_division(branch_expr, dae_model, structural_bindings)?;
            }
            reject_array_denominator_division(else_branch, dae_model, structural_bindings)?;
        }
        _ => {}
    }
    Ok(())
}

fn dims_value_count(dims: &[usize], span: rumoca_core::Span) -> Result<usize, LowerError> {
    dims.iter().try_fold(1usize, |count, dim| {
        count.checked_mul(*dim).ok_or_else(|| {
            LowerError::contract_violation("array division shape overflows host index range", span)
        })
    })
}
