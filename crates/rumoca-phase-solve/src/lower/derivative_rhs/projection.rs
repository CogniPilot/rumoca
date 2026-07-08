// SPEC_0021 file-size exception: derivative projection lowering still combines
// array indexing, shape validation, and row projection. split plan: move index
// proof and row assembly into focused projection submodules.

use crate::lower::{
    function_calls::{external_table_intrinsic_kind, split_named_and_positional_call_args},
    helpers::{
        format_i64_dims, format_usize_dims, is_stream_passthrough_intrinsic, positive_i64_index,
    },
    unsupported_at,
};
use crate::projection_suffix::parse_output_projection_suffix;

use super::*;

mod binding_expressions;
mod slices;
pub(in crate::lower) use binding_expressions::*;
use slices::{collect_slice_keys, literal_array_elements, scalar_keys_for_dims, slice_selections};
#[cfg(test)]
use slices::{
    compile_time_integer, compile_time_positive_range, next_range_value, range_would_overshoot_i64,
    slice_subscript_indices,
};
pub(in crate::lower) use slices::{
    compile_time_subscript_indices_with_owner, literal_array_elements_flat,
};

pub(in crate::lower) fn checked_usize_to_i64(
    value: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<i64, LowerError> {
    i64::try_from(value).map_err(|_| {
        LowerError::contract_violation(format!("{context} {value} exceeds i64 range"), span)
    })
}

fn checked_usize_dims_to_i64(
    dims: &[usize],
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    let mut converted = derivative_vec_with_capacity(dims.len(), context, span)?;
    for dim in dims {
        converted.push(checked_usize_to_i64(*dim, context, span)?);
    }
    Ok(converted)
}

fn checked_usize_scalar_count(
    dims: &[usize],
    context: &str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    dims.iter().try_fold(1usize, |count, dim| {
        count.checked_mul(*dim).ok_or_else(|| {
            LowerError::contract_violation(
                format!("{context} scalar count overflows host index range"),
                span,
            )
        })
    })
}

fn checked_projection_offset(
    base: usize,
    stride: usize,
    offset: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    let scaled = base.checked_mul(stride).ok_or_else(|| {
        LowerError::contract_violation(
            format!("{context} multiplication overflows host index range"),
            span,
        )
    })?;
    scaled.checked_add(offset).ok_or_else(|| {
        LowerError::contract_violation(
            format!("{context} addition overflows host index range"),
            span,
        )
    })
}

fn checked_modelica_index_from_zero_based(
    index: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<i64, LowerError> {
    let one_based = index.checked_add(1).ok_or_else(|| {
        LowerError::contract_violation(
            format!("{context} zero-based index {index} cannot be converted to one-based"),
            span,
        )
    })?;
    checked_usize_to_i64(one_based, context, span)
}

fn checked_generated_derivative_subscript(
    index: i64,
    span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Subscript, LowerError> {
    Ok(rumoca_core::Subscript::try_generated_index(
        index, span, context,
    )?)
}

fn checked_integer_f64_to_i64(
    value: f64,
    context: &str,
    span: rumoca_core::Span,
) -> Result<i64, LowerError> {
    let rounded = value.round();
    if !rounded.is_finite() || (rounded - value).abs() >= f64::EPSILON {
        return Err(unsupported_at(
            format!("{context} must be an integer"),
            span,
        ));
    }
    if rounded < i64::MIN as f64 || rounded >= i64::MAX as f64 {
        return Err(LowerError::contract_violation(
            format!("{context} {rounded} exceeds i64 range"),
            span,
        ));
    }
    // Bounds and integrality are checked above; Rust has no TryFrom<f64>.
    Ok(rounded as i64)
}

fn single_expression_vec(
    expr: rumoca_core::Expression,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    let mut values = derivative_vec_with_capacity(1, context, span)?;
    values.push(expr);
    Ok(values)
}

fn repeated_expression_vec(
    expr: &rumoca_core::Expression,
    count: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    let mut values = derivative_vec_with_capacity(count, context, span)?;
    for _ in 0..count {
        values.push(expr.clone());
    }
    Ok(values)
}

fn projection_expr_pair_or_owner_span(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Span, LowerError> {
    if let Some(span) = lhs
        .span()
        .filter(|span| !span.is_dummy())
        .or_else(|| rhs.span().filter(|span| !span.is_dummy()))
    {
        return Ok(span);
    }
    if !owner_span.is_dummy() {
        return Ok(owner_span);
    }
    Err(LowerError::UnspannedContractViolation {
        reason: "derivative projection expression requires source span metadata".to_string(),
    })
}

fn projection_expr_or_owner_span(
    expr: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Span, LowerError> {
    if let Some(span) = expr.span().filter(|span| !span.is_dummy()) {
        return Ok(span);
    }
    if !owner_span.is_dummy() {
        return Ok(owner_span);
    }
    Err(LowerError::UnspannedContractViolation {
        reason: "derivative projection expression requires source span metadata".to_string(),
    })
}

fn inherited_projection_span(
    span: rumoca_core::Span,
    owner_span: rumoca_core::Span,
) -> rumoca_core::Span {
    if span.is_dummy() { owner_span } else { span }
}

fn projection_first_expr_or_owner_span(
    values: &[rumoca_core::Expression],
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Span, LowerError> {
    if let Some(span) = values
        .first()
        .and_then(rumoca_core::Expression::span)
        .filter(|span| !span.is_dummy())
    {
        return Ok(span);
    }
    if !owner_span.is_dummy() {
        return Ok(owner_span);
    }
    Err(LowerError::UnspannedContractViolation {
        reason: "derivative projection expression requires source span metadata".to_string(),
    })
}

fn subscript_list_span_or_owner(
    subscripts: &[rumoca_core::Subscript],
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Span, LowerError> {
    if let Some(span) = subscripts
        .iter()
        .map(subscript_span)
        .find(|span| !span.is_dummy())
    {
        return Ok(span);
    }
    if !owner_span.is_dummy() {
        return Ok(owner_span);
    }
    Err(LowerError::UnspannedContractViolation {
        reason: "derivative projection subscript list requires source span metadata".to_string(),
    })
}

fn subscript_span(subscript: &rumoca_core::Subscript) -> rumoca_core::Span {
    match subscript {
        rumoca_core::Subscript::Index { span, .. }
        | rumoca_core::Subscript::Expr { span, .. }
        | rumoca_core::Subscript::Colon { span } => *span,
    }
}

fn one_based_index_range(
    dim: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut values = derivative_vec_with_capacity(dim, context, span)?;
    for index in 1..=dim {
        values.push(index);
    }
    Ok(values)
}

fn slice_selection_count(
    selections: &[Vec<usize>],
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    selections.iter().try_fold(1usize, |count, selection| {
        count.checked_mul(selection.len()).ok_or_else(|| {
            LowerError::contract_violation(
                "derivative slice selection count overflows host index range",
                span,
            )
        })
    })
}

pub(in crate::lower) fn derivative_arg_binding_keys(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<String>, LowerError> {
    let span = projection_expr_or_owner_span(expr, owner_span)?;
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => binding_keys_for_subscripted_name(
            name.as_str(),
            subscripts,
            dae_model,
            structural_bindings,
            span,
        ),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let base = binding_base_name(base, span)?;
            binding_keys_for_subscripted_name(
                &base,
                subscripts,
                dae_model,
                structural_bindings,
                span,
            )
        }
        _ => Err(unsupported_at(
            "unsupported der() argument in derivative RHS lowering",
            span,
        )),
    }
}

pub(in crate::lower) fn scalarized_rhs_expressions_with_owner(
    expr: &rumoca_core::Expression,
    target: &rumoca_core::Expression,
    expected: usize,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    let span = projection_expr_pair_or_owner_span(target, expr, owner_span)?;
    if let Some(values) =
        function_call_projected_scalars_with_owner(expr, dae_model, structural_bindings, span)?
        && values.len() == expected
    {
        return Ok(values);
    }
    if let Some(expressions) =
        expression_binding_expressions(expr, dae_model, structural_bindings, span)?
        && expressions.len() == expected
    {
        return Ok(expressions);
    }
    if let Some(elements) = literal_array_elements(expr)?
        && elements.len() == expected
    {
        return Ok(elements);
    }
    if preserve_indexed_tensor_product_rhs(
        expr,
        target,
        expected,
        dae_model,
        structural_bindings,
        span,
    )? {
        return indexed_rhs_expressions(
            expr,
            target,
            expected,
            dae_model,
            structural_bindings,
            span,
        );
    }
    if expected == 1 {
        return single_expression_vec(expr.clone(), "scalarized RHS expression count", span);
    }
    let target_dims = derivative_target_result_dims(target, dae_model, structural_bindings, span)?;
    if checked_usize_scalar_count(&target_dims, "array derivative target shape", span)? == expected
        && let Some(values) =
            project_expression_scalars(expr, &target_dims, dae_model, structural_bindings, span)?
        && values.len() == expected
    {
        return Ok(values);
    }
    indexed_rhs_expressions(expr, target, expected, dae_model, structural_bindings, span)
}

fn preserve_indexed_tensor_product_rhs(
    expr: &rumoca_core::Expression,
    target: &rumoca_core::Expression,
    expected: usize,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    let rumoca_core::Expression::Binary { op, .. } = expr else {
        return Ok(false);
    };
    if !is_mul(op) || expected <= 1 {
        return Ok(false);
    }
    let expr_dims = expression_result_dims(expr, dae_model, structural_bindings, owner_span)?;
    if expr_dims.is_empty()
        || checked_usize_scalar_count(&expr_dims, "tensor product RHS shape", owner_span)?
            != expected
    {
        return Ok(false);
    }
    let target_dims =
        derivative_target_result_dims(target, dae_model, structural_bindings, owner_span)?;
    Ok(target_dims == expr_dims)
}

pub(in crate::lower) fn scalarized_coefficient_expressions(
    expr: &rumoca_core::Expression,
    target: &rumoca_core::Expression,
    expected: usize,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    let span = projection_expr_pair_or_owner_span(target, expr, owner_span)?;
    if expected == 1
        || expression_result_dims(expr, dae_model, structural_bindings, span)?.is_empty()
    {
        return repeated_expression_vec(
            expr,
            expected,
            "scalarized coefficient expression count",
            span,
        );
    }
    scalarized_rhs_expressions_with_owner(
        expr,
        target,
        expected,
        dae_model,
        structural_bindings,
        span,
    )
}

pub(in crate::lower) fn project_expression_scalars(
    expr: &rumoca_core::Expression,
    dims: &[usize],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
    let span = projection_expr_or_owner_span(expr, owner_span)?;
    let count = checked_usize_scalar_count(dims, "projected expression dimensions", span)?;
    let mut values =
        derivative_vec_with_capacity(count, "projected expression scalar count", span)?;
    for flat_index in 0..count {
        let Some(value) = project_expression_scalar_with_owner(
            expr,
            dims,
            flat_index,
            dae_model,
            structural_bindings,
            span,
        )?
        else {
            return Ok(None);
        };
        values.push(value);
    }
    Ok(Some(values))
}

pub(in crate::lower) fn project_expression_scalar(
    expr: &rumoca_core::Expression,
    dims: &[usize],
    flat_index: usize,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    project_expression_scalar_with_owner(
        expr,
        dims,
        flat_index,
        dae_model,
        structural_bindings,
        projection_expr_or_owner_span(expr, owner_span)?,
    )
}

fn project_expression_scalar_with_owner(
    expr: &rumoca_core::Expression,
    dims: &[usize],
    flat_index: usize,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let ctx = ProjectionContext {
        dims,
        flat_index,
        dae_model,
        structural_bindings,
        owner_span,
    };
    project_expression_scalar_ctx(expr, &ctx)
}

struct ProjectionContext<'a> {
    dims: &'a [usize],
    flat_index: usize,
    dae_model: &'a dae::Dae,
    structural_bindings: &'a IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
}

// SPEC_0021: Exception - projection keeps expression shape cases together so
// unsupported scalarization paths retain their source span.
#[allow(clippy::too_many_lines)]
fn project_expression_scalar_ctx(
    expr: &rumoca_core::Expression,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef { .. } => {
            if let Some(expressions) = expression_binding_expressions(
                expr,
                ctx.dae_model,
                ctx.structural_bindings,
                ctx.owner_span,
            )? && let Some(expr) = expressions.get(ctx.flat_index)
            {
                return Ok(Some(expr.clone()));
            }
            Ok(None)
        }
        rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } => project_index_expression_scalar(
            base,
            subscripts,
            inherited_projection_span(*span, ctx.owner_span),
            ctx,
        ),
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            project_literal_array_scalar(elements, ctx.flat_index, ctx.owner_span)
        }
        rumoca_core::Expression::Unary { op, rhs, span } => {
            let span = inherited_projection_span(*span, ctx.owner_span);
            let Some(rhs) = project_operand_scalar_ctx(rhs, ctx, span)? else {
                return Ok(None);
            };
            Ok(Some(rumoca_core::Expression::Unary {
                op: op.clone(),
                rhs: Box::new(rhs),
                span,
            }))
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args,
            span,
        } => {
            let span = inherited_projection_span(*span, ctx.owner_span);
            let [arg] = args.as_slice() else {
                return Err(LowerError::contract_violation(
                    format!(
                        "der() in derivative projection requires exactly one argument, found {}",
                        args.len()
                    ),
                    span,
                ));
            };
            let Some(arg) = project_operand_scalar_ctx(arg, ctx, span)? else {
                return Ok(None);
            };
            Ok(Some(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Der,
                args: vec![arg],
                span,
            }))
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Zeros,
            args,
            span,
        } => {
            let span = inherited_projection_span(*span, ctx.owner_span);
            check_array_builtin_has_dimensions("zeros", args, span)?;
            Ok(Some(real_literal_expr(0.0, span)))
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Ones,
            args,
            span,
        } => {
            let span = inherited_projection_span(*span, ctx.owner_span);
            check_array_builtin_has_dimensions("ones", args, span)?;
            Ok(Some(real_literal_expr(1.0, span)))
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args,
            span,
        } => {
            let span = inherited_projection_span(*span, ctx.owner_span);
            let value = fill_value_arg(args, span)?;
            project_operand_scalar_ctx(value, ctx, span)
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Transpose,
            args,
            span,
        } => project_transpose_scalar(args, inherited_projection_span(*span, ctx.owner_span), ctx),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Cross,
            args,
            span,
        } => project_cross_scalar(args, inherited_projection_span(*span, ctx.owner_span), ctx),
        rumoca_core::Expression::FunctionCall { .. } => project_function_call_scalar(expr, ctx),
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_mul(op) => {
            let span = inherited_projection_span(*span, ctx.owner_span);
            if let Some(product) = project_tensor_product_scalar(lhs, rhs, span, ctx)? {
                return Ok(Some(product));
            }
            if tensor_product_matches_result_shape(lhs, rhs, span, ctx)? {
                return Ok(None);
            }
            project_binary_elementwise_scalar(op.clone(), lhs, rhs, span, ctx)
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_add(op) || is_sub(op) => {
            project_binary_elementwise_scalar(
                op.clone(),
                lhs,
                rhs,
                inherited_projection_span(*span, ctx.owner_span),
                ctx,
            )
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_div(op) => {
            project_binary_elementwise_scalar(
                op.clone(),
                lhs,
                rhs,
                inherited_projection_span(*span, ctx.owner_span),
                ctx,
            )
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            span,
        } => project_if_scalar_expr(
            branches,
            else_branch,
            inherited_projection_span(*span, ctx.owner_span),
            ctx,
        ),
        rumoca_core::Expression::FieldAccess { base, field, span } => {
            project_record_array_field_scalar(
                base,
                field,
                inherited_projection_span(*span, ctx.owner_span),
                ctx,
            )
        }
        _ => Ok(None),
    }
}

fn project_index_expression_scalar(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let base_dims = expression_result_dims(base, ctx.dae_model, ctx.structural_bindings, span)?;
    let selections = slice_selections(subscripts, &base_dims, ctx.structural_bindings, span)?;
    let mut result_dims =
        derivative_vec_with_capacity(selections.len(), "indexed expression result rank", span)?;
    for selection in &selections {
        result_dims.push(selection.len());
    }
    if checked_usize_scalar_count(&result_dims, "indexed expression result dimensions", span)?
        != checked_usize_scalar_count(ctx.dims, "projection context dimensions", span)?
    {
        return Ok(None);
    }
    let result_dims_i64 =
        checked_usize_dims_to_i64(&result_dims, "indexed expression result dimension", span)?;
    let Some(result_indices) = dae::flat_index_to_subscripts(&result_dims_i64, ctx.flat_index)
    else {
        return Ok(None);
    };
    let mut base_indices = derivative_vec_with_capacity(
        selections.len(),
        "indexed expression base index count",
        span,
    )?;
    for (selection, result_index) in selections.iter().zip(result_indices) {
        let result_offset = result_index.checked_sub(1).ok_or_else(|| {
            unsupported_at(
                "indexed expression projection uses non-positive slice index",
                span,
            )
        })?;
        let selected = selection.get(result_offset).copied().ok_or_else(|| {
            unsupported_at(
                "indexed expression projection is outside slice bounds",
                span,
            )
        })?;
        base_indices.push(selected);
    }
    let base_flat_index = flat_index_from_one_based_indices(&base_dims, &base_indices, span)?;
    project_expression_scalar_with_owner(
        base,
        &base_dims,
        base_flat_index,
        ctx.dae_model,
        ctx.structural_bindings,
        span,
    )
}

fn flat_index_from_one_based_indices(
    dims: &[usize],
    indices: &[usize],
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    if dims.len() != indices.len() {
        return Err(unsupported_at(
            "indexed expression projection rank does not match base rank",
            span,
        ));
    }
    let mut flat_index = 0usize;
    for (dim, index) in dims.iter().copied().zip(indices.iter().copied()) {
        if index == 0 || index > dim {
            return Err(unsupported_at(
                "indexed expression projection is outside base bounds",
                span,
            ));
        }
        let offset = index.checked_sub(1).ok_or_else(|| {
            unsupported_at(
                "indexed expression projection uses non-positive base index",
                span,
            )
        })?;
        flat_index = flat_index
            .checked_mul(dim)
            .and_then(|value| value.checked_add(offset))
            .ok_or_else(|| {
                LowerError::contract_violation(
                    "indexed expression flat index overflows host index range",
                    span,
                )
            })?;
    }
    Ok(flat_index)
}

/// Projects one element of a record-array member slice such as
/// `ac.pin[:].v`: element `k` resolves to the scalarized component variable
/// `ac.pin[k].v`. Declines when the selection is not a full one-dimensional
/// colon slice over a structured base or the element variable does not
/// exist.
fn project_record_array_field_scalar(
    base: &rumoca_core::Expression,
    field: &str,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let rumoca_core::Expression::Index {
        base: inner,
        subscripts,
        ..
    } = base
    else {
        return Ok(None);
    };
    let rumoca_core::Expression::VarRef {
        name,
        subscripts: ref_subscripts,
        ..
    } = inner.as_ref()
    else {
        return Ok(None);
    };
    if !ref_subscripts.is_empty()
        || ctx.dims.len() != 1
        || subscripts.len() != 1
        || !matches!(subscripts[0], rumoca_core::Subscript::Colon { .. })
    {
        return Ok(None);
    }
    let Some(component_ref) = name.component_ref() else {
        return Ok(None);
    };
    let mut element_ref = component_ref.clone();
    let Some(last) = element_ref.parts.last_mut() else {
        return Ok(None);
    };
    let index = checked_modelica_index_from_zero_based(
        ctx.flat_index,
        "projected record field index",
        span,
    )?;
    last.subs = vec![checked_generated_derivative_subscript(
        index,
        span,
        "projected record field index",
    )?];
    element_ref.parts.push(rumoca_core::ComponentRefPart {
        ident: field.to_string(),
        span,
        subs: Vec::new(),
    });
    let reference = rumoca_core::Reference::from_component_reference(element_ref);
    if variable_by_name(ctx.dae_model, reference.as_str()).is_none() {
        return Ok(None);
    }
    Ok(Some(rumoca_core::Expression::VarRef {
        name: reference,
        subscripts: vec![],
        span,
    }))
}

fn project_literal_array_scalar(
    elements: &[rumoca_core::Expression],
    flat_index: usize,
    owner_span: rumoca_core::Span,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    Ok(literal_array_elements_flat(elements, owner_span)?
        .get(flat_index)
        .cloned())
}

fn real_literal_expr(value: f64, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(value),
        span,
    }
}

fn project_function_call_scalar(
    expr: &rumoca_core::Expression,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let span = projection_expr_or_owner_span(expr, ctx.owner_span)?;
    if let rumoca_core::Expression::FunctionCall { name, args, .. } = expr
        && is_stream_passthrough_intrinsic(name.as_str())
    {
        let Some(arg) = args.first() else {
            return Ok(None);
        };
        return project_operand_scalar_ctx(arg, ctx, span);
    }
    let values = function_call_projected_scalars_with_owner(
        expr,
        ctx.dae_model,
        ctx.structural_bindings,
        span,
    )?;
    if let Some(value) = values.and_then(|values| values.get(ctx.flat_index).cloned()) {
        return Ok(Some(value));
    }
    project_vectorized_scalar_function_call(expr, ctx, span)
}

fn project_vectorized_scalar_function_call(
    expr: &rumoca_core::Expression,
    ctx: &ProjectionContext<'_>,
    span: rumoca_core::Span,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: false,
        ..
    } = expr
    else {
        return Ok(None);
    };
    let Some(function) = ctx.dae_model.symbols.functions.get(name.var_name()) else {
        return Ok(None);
    };
    let [output] = function.outputs.as_slice() else {
        return Ok(None);
    };
    if !output.dims.is_empty() {
        return Ok(None);
    }
    let (named_args, positional_args) = split_named_and_positional_call_args(name.as_str(), args)?;
    let lane_dims =
        checked_usize_dims_to_i64(ctx.dims, "vectorized function lane dimension", span)?;
    let lane_indices =
        dae::flat_index_to_subscripts(&lane_dims, ctx.flat_index).ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                    "vectorized scalar function flat index {} is outside lane dimensions {}",
                    ctx.flat_index,
                    format_usize_dims(ctx.dims)
                ),
                span,
            )
        })?;
    let mut projected_args = derivative_vec_with_capacity(
        function.inputs.len(),
        "vectorized scalar function projected argument count",
        span,
    )?;
    let mut positional_idx = 0usize;
    let mut projected_any = false;
    for input in &function.inputs {
        let actual = if let Some(actual) = named_args.get(input.name.as_str()) {
            *actual
        } else if let Some(actual) = positional_args.get(positional_idx).copied() {
            positional_idx += 1;
            actual
        } else if let Some(default) = input.default.as_ref() {
            default
        } else {
            return Ok(None);
        };
        let Some((projected, did_project)) =
            project_vectorized_function_actual(actual, input.dims.len(), &lane_indices, ctx, span)?
        else {
            return Ok(None);
        };
        projected_any |= did_project;
        projected_args.push(projected);
    }
    if !projected_any {
        return Ok(None);
    }
    Ok(Some(rumoca_core::Expression::FunctionCall {
        name: name.clone(),
        args: projected_args,
        is_constructor: false,
        span,
    }))
}

fn project_vectorized_function_actual(
    actual: &rumoca_core::Expression,
    formal_rank: usize,
    lane_indices: &[usize],
    ctx: &ProjectionContext<'_>,
    span: rumoca_core::Span,
) -> Result<Option<(rumoca_core::Expression, bool)>, LowerError> {
    let actual_dims = expression_result_dims(actual, ctx.dae_model, ctx.structural_bindings, span)?;
    if actual_dims.is_empty() || actual_dims.len() == formal_rank {
        return Ok(Some((actual.clone(), false)));
    }
    if formal_rank == 0 && actual_dims == ctx.dims {
        let Some(projected) = project_expression_scalar_with_owner(
            actual,
            ctx.dims,
            ctx.flat_index,
            ctx.dae_model,
            ctx.structural_bindings,
            span,
        )?
        else {
            return Ok(None);
        };
        return Ok(Some((projected, true)));
    }
    if actual_dims.len() != ctx.dims.len() + formal_rank
        || actual_dims[..ctx.dims.len()] != *ctx.dims
    {
        return Ok(None);
    }
    let mut subscripts = derivative_vec_with_capacity(
        actual_dims.len(),
        "vectorized function argument subscript count",
        span,
    )?;
    for index in lane_indices {
        let index = checked_usize_to_i64(*index, "vectorized function lane index", span)?;
        subscripts.push(rumoca_core::Subscript::try_generated_index(
            index,
            span,
            "vectorized function lane index",
        )?);
    }
    for _ in 0..formal_rank {
        subscripts.push(rumoca_core::Subscript::try_generated_colon(
            span,
            "vectorized function formal slice",
        )?);
    }
    Ok(Some((
        rumoca_core::Expression::Index {
            base: Box::new(actual.clone()),
            subscripts,
            span,
        },
        true,
    )))
}

fn project_cross_scalar(
    args: &[rumoca_core::Expression],
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    if ctx.dims != [3] || ctx.flat_index >= 3 {
        return Ok(None);
    }
    let [lhs, rhs] = args else {
        return Err(unsupported_at(
            "cross() derivative projection requires two vector arguments",
            span,
        ));
    };
    let lhs_dims = expression_result_dims(lhs, ctx.dae_model, ctx.structural_bindings, span)?;
    let rhs_dims = expression_result_dims(rhs, ctx.dae_model, ctx.structural_bindings, span)?;
    if lhs_dims != [3] || rhs_dims != [3] {
        return Ok(None);
    }
    let (lhs_a, rhs_a, lhs_b, rhs_b) = match ctx.flat_index {
        0 => (1, 2, 2, 1),
        1 => (2, 0, 0, 2),
        2 => (0, 1, 1, 0),
        _ => return Ok(None),
    };
    let Some(lhs_a) = project_expression_scalar_with_owner(
        lhs,
        &[3],
        lhs_a,
        ctx.dae_model,
        ctx.structural_bindings,
        span,
    )?
    else {
        return Ok(None);
    };
    let Some(rhs_a) = project_expression_scalar_with_owner(
        rhs,
        &[3],
        rhs_a,
        ctx.dae_model,
        ctx.structural_bindings,
        span,
    )?
    else {
        return Ok(None);
    };
    let Some(lhs_b) = project_expression_scalar_with_owner(
        lhs,
        &[3],
        lhs_b,
        ctx.dae_model,
        ctx.structural_bindings,
        span,
    )?
    else {
        return Ok(None);
    };
    let Some(rhs_b) = project_expression_scalar_with_owner(
        rhs,
        &[3],
        rhs_b,
        ctx.dae_model,
        ctx.structural_bindings,
        span,
    )?
    else {
        return Ok(None);
    };
    Ok(Some(rumoca_core::Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(mul_with_span(lhs_a, rhs_a, span)),
        rhs: Box::new(mul_with_span(lhs_b, rhs_b, span)),
        span,
    }))
}

fn project_if_scalar_expr(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let mut projected_branches =
        derivative_vec_with_capacity(branches.len(), "projected if branch count", span)?;
    for (condition, branch) in branches {
        let branch = project_branch_scalar_ctx(branch, ctx, span)?;
        projected_branches.push((condition.clone(), branch));
    }
    let else_branch = project_branch_scalar_ctx(else_branch, ctx, span)?;
    Ok(Some(rumoca_core::Expression::If {
        branches: projected_branches,
        else_branch: Box::new(else_branch),
        span,
    }))
}

fn project_branch_scalar_ctx(
    expr: &rumoca_core::Expression,
    ctx: &ProjectionContext<'_>,
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, LowerError> {
    if let Some(projected) = project_operand_scalar_ctx(expr, ctx, owner_span)? {
        return Ok(projected);
    }
    if expression_result_dims(expr, ctx.dae_model, ctx.structural_bindings, owner_span)?.is_empty()
    {
        return Ok(expr.clone());
    }
    Err(unsupported_at(
        "array derivative if branch could not be projected to a scalar",
        projection_expr_or_owner_span(expr, owner_span)?,
    ))
}

fn project_operand_scalar_ctx(
    expr: &rumoca_core::Expression,
    ctx: &ProjectionContext<'_>,
    owner_span: rumoca_core::Span,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let expr_dims =
        expression_result_dims(expr, ctx.dae_model, ctx.structural_bindings, owner_span)?;
    if expr_dims.is_empty() {
        return Ok(Some(expr.clone()));
    }
    if expr_dims != ctx.dims {
        return Ok(None);
    }
    project_expression_scalar_ctx(expr, ctx)
}

fn check_array_builtin_has_dimensions(
    name: &str,
    args: &[rumoca_core::Expression],
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    if args.is_empty() {
        return Err(LowerError::contract_violation(
            format!("{name}() in derivative projection requires at least one dimension argument"),
            span,
        ));
    }
    Ok(())
}

fn fill_value_arg(
    args: &[rumoca_core::Expression],
    span: rumoca_core::Span,
) -> Result<&rumoca_core::Expression, LowerError> {
    let Some(value) = args.first() else {
        return Err(LowerError::contract_violation(
            "fill() in derivative projection requires a value argument",
            span,
        ));
    };
    if args.len() == 1 {
        return Err(LowerError::contract_violation(
            "fill() in derivative projection requires at least one dimension argument",
            span,
        ));
    }
    Ok(value)
}

fn project_transpose_scalar(
    args: &[rumoca_core::Expression],
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let [arg] = args else {
        return Err(LowerError::contract_violation(
            format!(
                "transpose() in derivative projection requires one argument, found {}",
                args.len()
            ),
            span,
        ));
    };
    let arg_dims = expression_result_dims(arg, ctx.dae_model, ctx.structural_bindings, span)?;
    let [rows, cols] = arg_dims.as_slice() else {
        return Err(unsupported_at(
            format!(
                "transpose() requires a matrix input, got shape {}",
                format_usize_dims(&arg_dims)
            ),
            span,
        ));
    };
    let result_dims = [*cols, *rows];
    if ctx.dims != result_dims {
        return Ok(None);
    }
    if *rows == 0 {
        return Ok(None);
    }
    let out_row = ctx.flat_index / rows;
    let out_col = ctx.flat_index % rows;
    if out_row >= *cols {
        return Ok(None);
    }
    let arg_flat_index = checked_projection_offset(
        out_col,
        *cols,
        out_row,
        "transpose derivative projection source flat index",
        span,
    )?;
    project_expression_scalar_with_owner(
        arg,
        &arg_dims,
        arg_flat_index,
        ctx.dae_model,
        ctx.structural_bindings,
        span,
    )
}

fn project_binary_elementwise_scalar(
    op: OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let Some(lhs) = project_operand_scalar_ctx(lhs, ctx, span)? else {
        return Ok(None);
    };
    let Some(rhs) = project_operand_scalar_ctx(rhs, ctx, span)? else {
        return Ok(None);
    };
    Ok(Some(rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }))
}

fn project_tensor_product_scalar(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let lhs_dims = expression_result_dims(lhs, ctx.dae_model, ctx.structural_bindings, span)?;
    let rhs_dims = expression_result_dims(rhs, ctx.dae_model, ctx.structural_bindings, span)?;
    match (lhs_dims.as_slice(), rhs_dims.as_slice(), ctx.dims) {
        ([rows, cols], [n], [_]) if cols == n => {
            project_matrix_vector_product_scalar(lhs, rhs, span, ctx, *rows, *cols)
        }
        ([n], [rows, cols], [_]) if n == rows => {
            project_vector_matrix_product_scalar(lhs, rhs, span, ctx, *rows, *cols)
        }
        ([rows, inner_lhs], [inner_rhs, cols], [out_rows, out_cols])
            if inner_lhs == inner_rhs && rows == out_rows && cols == out_cols =>
        {
            project_matrix_matrix_product_scalar(lhs, rhs, span, ctx, *inner_lhs, *cols)
        }
        _ => Ok(None),
    }
}

fn tensor_product_matches_result_shape(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
) -> Result<bool, LowerError> {
    let lhs_dims = expression_result_dims(lhs, ctx.dae_model, ctx.structural_bindings, span)?;
    let rhs_dims = expression_result_dims(rhs, ctx.dae_model, ctx.structural_bindings, span)?;
    Ok(matches!(
        (lhs_dims.as_slice(), rhs_dims.as_slice(), ctx.dims),
        ([_, cols], [n], [_])
            if cols == n
    ) || matches!(
        (lhs_dims.as_slice(), rhs_dims.as_slice(), ctx.dims),
        ([n], [rows, _], [_])
            if n == rows
    ) || matches!(
        (lhs_dims.as_slice(), rhs_dims.as_slice(), ctx.dims),
        ([rows, inner_lhs], [inner_rhs, cols], [out_rows, out_cols])
            if inner_lhs == inner_rhs && rows == out_rows && cols == out_cols
    ))
}

fn project_matrix_vector_product_scalar(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
    rows: usize,
    cols: usize,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    if ctx.flat_index >= rows {
        return Ok(None);
    }
    let lhs_dims = [rows, cols];
    let rhs_dims = [cols];

    let mut terms =
        derivative_vec_with_capacity(cols, "matrix-vector derivative projection term count", span)?;
    for col in 0..cols {
        let matrix_flat_index = checked_projection_offset(
            ctx.flat_index,
            cols,
            col,
            "matrix-vector derivative projection flat index",
            span,
        )?;
        let Some(lhs_term) = project_expression_scalar_with_owner(
            lhs,
            &lhs_dims,
            matrix_flat_index,
            ctx.dae_model,
            ctx.structural_bindings,
            span,
        )?
        else {
            return Ok(None);
        };
        let Some(rhs_term) = project_expression_scalar_with_owner(
            rhs,
            &rhs_dims,
            col,
            ctx.dae_model,
            ctx.structural_bindings,
            span,
        )?
        else {
            return Ok(None);
        };
        terms.push(mul_with_span(lhs_term, rhs_term, span));
    }
    Ok(Some(sum_expressions(terms, span)))
}

fn project_vector_matrix_product_scalar(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
    rows: usize,
    cols: usize,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    if ctx.flat_index >= cols {
        return Ok(None);
    }
    let lhs_dims = [rows];
    let rhs_dims = [rows, cols];
    let col = ctx.flat_index;

    let mut terms =
        derivative_vec_with_capacity(rows, "vector-matrix derivative projection term count", span)?;
    for row in 0..rows {
        let Some(lhs_term) = project_expression_scalar_with_owner(
            lhs,
            &lhs_dims,
            row,
            ctx.dae_model,
            ctx.structural_bindings,
            span,
        )?
        else {
            return Ok(None);
        };
        let rhs_flat_index = checked_projection_offset(
            row,
            cols,
            col,
            "vector-matrix derivative projection flat index",
            span,
        )?;
        let Some(rhs_term) = project_expression_scalar_with_owner(
            rhs,
            &rhs_dims,
            rhs_flat_index,
            ctx.dae_model,
            ctx.structural_bindings,
            span,
        )?
        else {
            return Ok(None);
        };
        terms.push(mul_with_span(lhs_term, rhs_term, span));
    }
    Ok(Some(sum_expressions(terms, span)))
}

fn project_matrix_matrix_product_scalar(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
    inner: usize,
    cols: usize,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let lhs_dims = expression_result_dims(lhs, ctx.dae_model, ctx.structural_bindings, span)?;
    let rhs_dims = expression_result_dims(rhs, ctx.dae_model, ctx.structural_bindings, span)?;
    if cols == 0 {
        return Ok(None);
    }
    let row = ctx.flat_index / cols;
    let col = ctx.flat_index % cols;

    let mut terms = derivative_vec_with_capacity(
        inner,
        "matrix-matrix derivative projection term count",
        span,
    )?;
    for inner_idx in 0..inner {
        let lhs_flat_index = checked_projection_offset(
            row,
            inner,
            inner_idx,
            "matrix-matrix derivative projection lhs flat index",
            span,
        )?;
        let rhs_flat_index = checked_projection_offset(
            inner_idx,
            cols,
            col,
            "matrix-matrix derivative projection rhs flat index",
            span,
        )?;
        let Some(lhs_term) = project_expression_scalar_with_owner(
            lhs,
            &lhs_dims,
            lhs_flat_index,
            ctx.dae_model,
            ctx.structural_bindings,
            span,
        )?
        else {
            return Ok(None);
        };
        let Some(rhs_term) = project_expression_scalar_with_owner(
            rhs,
            &rhs_dims,
            rhs_flat_index,
            ctx.dae_model,
            ctx.structural_bindings,
            span,
        )?
        else {
            return Ok(None);
        };
        terms.push(mul_with_span(lhs_term, rhs_term, span));
    }
    Ok(Some(sum_expressions(terms, span)))
}

pub(in crate::lower) fn expression_result_dims(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let expr_span = projection_expr_or_owner_span(expr, owner_span)?;
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => expression_dims_for_subscripted_binding(
            name.as_str(),
            subscripts,
            dae_model,
            structural_bindings,
            expr_span,
        ),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let base_dims =
                expression_result_dims(base, dae_model, structural_bindings, expr_span)?;
            result_dims_for_subscripts(&base_dims, subscripts, structural_bindings, expr_span)
        }
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            ..
        } => array_expression_dims(elements, *is_matrix),
        rumoca_core::Expression::Tuple { elements, .. } => Ok(vec![elements.len()]),
        rumoca_core::Expression::BuiltinCall {
            function,
            args,
            span,
        } => builtin_call_result_dims(
            function,
            args,
            inherited_projection_span(*span, expr_span),
            dae_model,
            structural_bindings,
        ),
        rumoca_core::Expression::Unary { rhs, .. } => {
            expression_result_dims(rhs, dae_model, structural_bindings, expr_span)
        }
        rumoca_core::Expression::FunctionCall {
            name,
            is_constructor: false,
            span,
            ..
        } => required_declared_function_output_dims(
            dae_model,
            name,
            inherited_projection_span(*span, expr_span),
        ),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } if is_mul(op) => {
            binary_mul_result_dims(lhs, rhs, dae_model, structural_bindings, expr_span)
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } if is_add(op) || is_sub(op) => {
            binary_elementwise_result_dims(lhs, rhs, dae_model, structural_bindings, expr_span)
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } if is_div(op) => {
            binary_elementwise_result_dims(lhs, rhs, dae_model, structural_bindings, expr_span)
        }
        rumoca_core::Expression::If { else_branch, .. } => {
            expression_result_dims(else_branch, dae_model, structural_bindings, expr_span)
        }
        _ => Ok(Vec::new()),
    }
}

fn transpose_result_dims(
    args: &[rumoca_core::Expression],
    span: rumoca_core::Span,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<usize>, LowerError> {
    let [arg] = args else {
        return Err(LowerError::contract_violation(
            format!(
                "transpose() in derivative projection requires one argument, found {}",
                args.len()
            ),
            span,
        ));
    };
    let dims = expression_result_dims(arg, dae_model, structural_bindings, span)?;
    match dims.as_slice() {
        [rows, cols] => copy_result_dims(&[*cols, *rows], "transpose result dimension count", span),
        _ => Err(unsupported_at(
            format!(
                "transpose() requires a matrix input, got shape {}",
                format_usize_dims(&dims)
            ),
            span,
        )),
    }
}

fn builtin_call_result_dims(
    function: &rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    span: rumoca_core::Span,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<usize>, LowerError> {
    match function {
        rumoca_core::BuiltinFunction::Der => {
            let arg = args.first().ok_or_else(|| {
                LowerError::contract_violation(
                    "der() in derivative projection requires one argument",
                    span,
                )
            })?;
            expression_result_dims(arg, dae_model, structural_bindings, span)
        }
        rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Ones => {
            builtin_size_args_dims(args, structural_bindings, span)
        }
        rumoca_core::BuiltinFunction::Fill => {
            let size_args = args
                .get(1..)
                .filter(|args| !args.is_empty())
                .ok_or_else(|| {
                    LowerError::contract_violation(
                        "fill() in derivative projection requires at least one dimension argument",
                        span,
                    )
                })?;
            builtin_size_args_dims(size_args, structural_bindings, span)
        }
        rumoca_core::BuiltinFunction::Transpose => {
            transpose_result_dims(args, span, dae_model, structural_bindings)
        }
        rumoca_core::BuiltinFunction::Cross => Ok(vec![3]),
        _ => Ok(Vec::new()),
    }
}

pub(in crate::lower) fn builtin_size_args_dims(
    args: &[rumoca_core::Expression],
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let span = projection_first_expr_or_owner_span(args, owner_span)?;
    let mut dims = derivative_vec_with_capacity(args.len(), "size() dimension count", span)?;
    for arg in args {
        dims.push(
            super::super::compile_time_non_negative_dimension_expr_with_owner(
                arg,
                structural_bindings,
                span,
                "builtin array dimension",
            )?,
        );
    }
    Ok(dims)
}

pub(in crate::lower) fn expression_dims_for_subscripted_binding(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    fallback_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    if let Some(var) = variable_by_name(dae_model, base) {
        if var.dims.is_empty() {
            if subscripts.is_empty() {
                return Ok(Vec::new());
            }
            return Err(unsupported_at(
                format!("scalar binding `{base}` was indexed"),
                subscript_list_span_or_owner(subscripts, fallback_span)?,
            ));
        }
        return result_dims_for_subscripted_binding(
            base,
            subscripts,
            dae_model,
            structural_bindings,
            subscript_list_span_or_owner(subscripts, fallback_span)?,
        );
    }

    if subscripts.is_empty() {
        if scalarized_variable_name_is_declared(dae_model, base)? {
            return Ok(Vec::new());
        }
        if let Some(dims) = scalarized_child_dims(dae_model, base, fallback_span)? {
            return Ok(dims);
        }
        if scalarized_record_root_exists(dae_model, base) {
            return Ok(Vec::new());
        }
        return Err(LowerError::MissingBinding {
            name: base.to_string(),
        });
    }
    if let Ok(scalarized_key) =
        scalarized_binding_key(base, subscripts, structural_bindings, fallback_span)
        && variable_by_name(dae_model, &scalarized_key).is_some()
    {
        return Ok(Vec::new());
    }
    let span = subscript_list_span_or_owner(subscripts, fallback_span)?;
    let dims = scalarized_child_dims(dae_model, base, span)?.ok_or_else(|| {
        LowerError::MissingBinding {
            name: base.to_string(),
        }
    })?;
    let selections = slice_selections(subscripts, &dims, structural_bindings, span)?;
    let mut result_dims = derivative_vec_with_capacity(
        selections.len(),
        "scalarized child result dimension count",
        span,
    )?;
    for selection in selections {
        result_dims.push(selection.len());
    }
    Ok(result_dims)
}

fn required_declared_function_output_dims(
    dae_model: &dae::Dae,
    name: &rumoca_core::Reference,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let requested = name.as_str();
    if let Some(function) = dae_model.symbols.functions.get(name.var_name()) {
        if function.is_constructor {
            return Ok(Vec::new());
        }
        return declared_function_output_dims(function, requested, span);
    }
    if let Some(dims) = projected_declared_function_output_dims(dae_model, requested, span)? {
        return Ok(dims);
    }
    if external_table_intrinsic_kind(requested).is_some() {
        return Ok(Vec::new());
    }
    if is_stream_passthrough_intrinsic(requested) {
        return Ok(Vec::new());
    }
    Err(LowerError::MissingFunction {
        name: name.to_string(),
    }
    .with_fallback_span(span))
}

fn declared_function_output_dims(
    function: &rumoca_core::Function,
    name: &str,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let [output] = function.outputs.as_slice() else {
        return Err(LowerError::InvalidFunction {
            name: name.to_owned(),
            reason: format!(
                "derivative projection requires exactly one output, found {}",
                function.outputs.len()
            ),
        }
        .with_fallback_span(span));
    };
    let mut dims = derivative_vec_with_capacity(
        output.dims.len(),
        "declared function output dimension count",
        span,
    )?;
    for dim in &output.dims {
        let dim = usize::try_from(*dim)
            .map_err(|_| LowerError::InvalidFunction {
                name: name.to_owned(),
                reason: format!("output `{}` has invalid dimension {dim}", output.name),
            })
            .map_err(|err| err.with_fallback_span(span))?;
        dims.push(dim);
    }
    Ok(dims)
}

fn projected_declared_function_output_dims(
    dae_model: &dae::Dae,
    requested: &str,
    span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    rumoca_core::find_map_top_level_splits_rev(requested, |base_name, suffix| {
        match projected_declared_function_output_dims_split(dae_model, base_name, suffix, span) {
            Ok(Some(dims)) => Some(Ok(dims)),
            Ok(None) => None,
            Err(err) => Some(Err(err)),
        }
    })
    .transpose()
}

fn projected_declared_function_output_dims_split(
    dae_model: &dae::Dae,
    base_name: &str,
    suffix: &str,
    span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    let Some(function) = dae_model
        .symbols
        .functions
        .get(&rumoca_core::VarName::new(base_name))
    else {
        return Ok(None);
    };
    let Some(projection_suffix) = parse_output_projection_suffix(suffix) else {
        return Ok(None);
    };
    let (output, output_name) = match function
        .outputs
        .iter()
        .find(|output| output.name == projection_suffix.output_name)
    {
        Some(output) => (output, projection_suffix.output_name.as_str()),
        None if projection_suffix.output_field.is_none()
            && matches!(projection_suffix.output_name.as_str(), "re" | "im")
            && function.outputs.len() == 1
            && function_output_is_complex_record(&function.outputs[0]) =>
        {
            (&function.outputs[0], function.outputs[0].name.as_str())
        }
        None => return Ok(None),
    };
    if let Some(field) = projection_suffix.output_field.as_deref()
        && (!function_output_is_complex_record(output) || !matches!(field, "re" | "im"))
    {
        return Ok(None);
    }
    let output_span = if output.span.is_dummy() {
        span
    } else {
        output.span
    };
    let dims = declared_output_concrete_dims(output, output_name, output_span)?;
    projected_output_remaining_dims(&dims, &projection_suffix.indices, output_name, span).map(Some)
}

fn function_output_is_complex_record(output: &rumoca_core::FunctionParam) -> bool {
    rumoca_core::qualified_type_name_matches(&output.type_name, "Complex")
}

fn declared_output_concrete_dims(
    output: &rumoca_core::FunctionParam,
    output_name: &str,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut dims = derivative_vec_with_capacity(
        output.dims.len(),
        "projected function output dimension count",
        span,
    )?;
    for dim in &output.dims {
        dims.push(
            usize::try_from(*dim)
                .map_err(|_| LowerError::InvalidFunction {
                    name: output_name.to_owned(),
                    reason: format!("output `{}` has invalid dimension {dim}", output.name),
                })
                .map_err(|err| err.with_fallback_span(span))?,
        );
    }
    Ok(dims)
}

fn projected_output_remaining_dims(
    dims: &[usize],
    indices: &[usize],
    output_name: &str,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    if indices.is_empty() {
        let mut copied =
            derivative_vec_with_capacity(dims.len(), "projected function output rank", span)?;
        copied.extend(dims.iter().copied());
        return Ok(copied);
    }
    if indices.len() > dims.len() {
        return Err(LowerError::contract_violation(
            format!(
                "projected function output `{output_name}` has {} indices for {} dimensions",
                indices.len(),
                dims.len()
            ),
            span,
        ));
    }
    for (index, dim) in indices.iter().zip(dims.iter()) {
        if *index == 0 || *index > *dim {
            return Err(LowerError::contract_violation(
                format!(
                    "projected function output `{output_name}` index {index} is outside dimension {dim}"
                ),
                span,
            ));
        }
    }
    let mut remaining = derivative_vec_with_capacity(
        dims.len() - indices.len(),
        "projected function output remaining rank",
        span,
    )?;
    remaining.extend(dims[indices.len()..].iter().copied());
    Ok(remaining)
}

pub(in crate::lower) fn binary_elementwise_result_dims(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let span = projection_expr_pair_or_owner_span(lhs, rhs, owner_span)?;
    let lhs_dims = expression_result_dims(lhs, dae_model, structural_bindings, span)?;
    let rhs_dims = expression_result_dims(rhs, dae_model, structural_bindings, span)?;
    Ok(match (lhs_dims.is_empty(), rhs_dims.is_empty()) {
        (true, true) => Vec::new(),
        (true, false) => rhs_dims,
        (false, true) => lhs_dims,
        (false, false) if lhs_dims == rhs_dims => lhs_dims,
        _ => Vec::new(),
    })
}

pub(in crate::lower) fn binary_mul_result_dims(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let span = projection_expr_pair_or_owner_span(lhs, rhs, owner_span)?;
    let lhs_dims = expression_result_dims(lhs, dae_model, structural_bindings, span)?;
    let rhs_dims = expression_result_dims(rhs, dae_model, structural_bindings, span)?;
    Ok(match (lhs_dims.as_slice(), rhs_dims.as_slice()) {
        ([], dims) if !dims.is_empty() => {
            copy_result_dims(dims, "scalar lhs product dimension count", span)?
        }
        (dims, []) if !dims.is_empty() => {
            copy_result_dims(dims, "scalar rhs product dimension count", span)?
        }
        ([lhs_rows, lhs_cols], [rhs_rows, rhs_cols]) if lhs_cols == rhs_rows => copy_result_dims(
            &[*lhs_rows, *rhs_cols],
            "matrix product dimension count",
            span,
        )?,
        ([rows, cols], [n]) if cols == n => {
            copy_result_dims(&[*rows], "matrix-vector product dimension count", span)?
        }
        ([n], [rows, cols]) if n == rows => {
            copy_result_dims(&[*cols], "vector-matrix product dimension count", span)?
        }
        ([n], [m]) if n == m => Vec::new(),
        _ if lhs_dims == rhs_dims => lhs_dims,
        _ => Vec::new(),
    })
}

fn copy_result_dims(
    dims: &[usize],
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    if dims.is_empty() {
        return Ok(Vec::new());
    }
    let mut copied = derivative_vec_with_capacity(dims.len(), context, span)?;
    copied.extend_from_slice(dims);
    Ok(copied)
}

#[cfg(test)]
mod tests;
