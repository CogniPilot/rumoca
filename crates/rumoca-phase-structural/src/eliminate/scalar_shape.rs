use rumoca_core::{Expression, Reference, Subscript, VarName};
use rumoca_ir_dae as dae;

use crate::StructuralError;
use crate::variable_scope::{DaeVariableScope, scalar_count_from_dims};

use super::exact_reference_expr_name_in_dae;

pub(super) fn expression_is_scalar_after_subscripts(
    expr: &Expression,
    dae: &dae::Dae,
) -> Result<bool, StructuralError> {
    if let Some(exact_name) = exact_reference_expr_name_in_dae(dae, expr)
        && let Some(var) = DaeVariableScope::new(dae).exact(&exact_name)
    {
        return Ok(scalar_count_from_dims(&exact_name, &var.dims)? == 1);
    }
    match expr {
        Expression::Literal { .. } | Expression::Empty { .. } => Ok(true),
        Expression::VarRef {
            name,
            subscripts,
            span,
        } => var_ref_is_scalar_after_subscripts(name, subscripts, *span, dae),
        Expression::Unary { rhs, .. } => expression_is_scalar_after_subscripts(rhs, dae),
        Expression::Binary { lhs, rhs, .. } => Ok(expression_is_scalar_after_subscripts(lhs, dae)?
            && expression_is_scalar_after_subscripts(rhs, dae)?),
        Expression::BuiltinCall { args, .. } => args
            .iter()
            .map(|arg| expression_is_scalar_after_subscripts(arg, dae))
            .try_fold(true, |all_scalar, arg_scalar| Ok(all_scalar && arg_scalar?)),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let branches_scalar = branches
                .iter()
                .map(|(_, value)| expression_is_scalar_after_subscripts(value, dae))
                .try_fold(true, |all_scalar, value_scalar| {
                    Ok(all_scalar && value_scalar?)
                })?;
            Ok(branches_scalar && expression_is_scalar_after_subscripts(else_branch, dae)?)
        }
        Expression::FunctionCall { .. }
        | Expression::Array { .. }
        | Expression::Tuple { .. }
        | Expression::Range { .. }
        | Expression::ArrayComprehension { .. }
        | Expression::Index { .. }
        | Expression::FieldAccess { .. } => Ok(false),
    }
}

fn var_ref_is_scalar_after_subscripts(
    name: &Reference,
    subscripts: &[Subscript],
    reference_span: rumoca_core::Span,
    dae: &dae::Dae,
) -> Result<bool, StructuralError> {
    let scope = DaeVariableScope::new(dae);
    let dims = match scope.dims_for_reference(name) {
        Ok(Some(dims)) => dims,
        Ok(None) => return Ok(false),
        Err(StructuralError::ContractViolation { reason, .. })
        | Err(StructuralError::UnspannedContractViolation { reason })
            if reason.contains("missing DAE variable metadata") =>
        {
            return Ok(false);
        }
        Err(err) => return Err(err),
    };
    let remaining_dims = dims_after_subscripts(name.var_name(), &dims, subscripts, reference_span)?;
    Ok(scalar_count_from_dims(name.var_name(), &remaining_dims)? == 1)
}

fn dims_after_subscripts(
    name: &VarName,
    dims: &[i64],
    subscripts: &[Subscript],
    reference_span: rumoca_core::Span,
) -> Result<Vec<i64>, StructuralError> {
    if subscripts.len() > dims.len() {
        return Err(StructuralError::ContractViolation {
            reason: format!(
                "indexed DAE reference `{}` has {} subscripts for dimensions {:?}",
                name.as_str(),
                subscripts.len(),
                dims
            ),
            span: subscripts
                .first()
                .map(Subscript::span)
                .filter(|span| !span.is_dummy())
                .unwrap_or(reference_span),
        });
    }
    let mut remaining = Vec::new();
    for (index, dim) in subscripts.iter().zip(dims) {
        match index {
            Subscript::Index { value, span } => {
                if *value < 1 || *value > *dim {
                    return Err(StructuralError::ContractViolation {
                        reason: format!(
                            "indexed DAE reference `{}` subscript {} is outside dimension {}",
                            name.as_str(),
                            value,
                            dim
                        ),
                        span: *span,
                    });
                }
            }
            Subscript::Colon { .. } => remaining.push(*dim),
            Subscript::Expr { .. } => return Ok(dims.to_vec()),
        }
    }
    remaining.extend_from_slice(&dims[subscripts.len()..]);
    Ok(remaining)
}
