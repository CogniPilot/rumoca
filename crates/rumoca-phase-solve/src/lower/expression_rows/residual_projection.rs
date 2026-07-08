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
    if let rumoca_core::Expression::Tuple { elements, .. } = target
        && let Some(output_groups) =
            derivative_rhs::function_call_projected_output_groups_with_owner(
                rhs,
                dae_model,
                structural_bindings,
                span,
            )?
    {
        if elements.len() != output_groups.len() {
            return Ok(None);
        }
        let mut values = super::expression_vec_with_capacity(
            scalar_count,
            "scalarized tuple residual value count",
            span,
        )?;
        for (target_element, rhs_values) in elements.iter().zip(output_groups) {
            if rhs_values.is_empty() || !tuple_target_element_has_binding(target_element) {
                continue;
            }
            let lhs_values = lower_tuple_target_element_values(
                builder,
                target_element,
                rhs_values.len(),
                span,
                &scope,
            )?;
            if lhs_values.len() != rhs_values.len() {
                return Ok(None);
            }
            for (lhs, rhs) in lhs_values.into_iter().zip(rhs_values) {
                let rhs = builder.lower_expr_with_source_context(&rhs, span, &scope, 0)?;
                values.push(builder.emit_binary_at(BinaryOp::Sub, lhs, rhs, span)?);
            }
        }
        if values.is_empty() {
            return Ok(None);
        }
        return Ok(Some(values));
    }
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

pub(super) fn scalarized_tuple_residual_binding_count(
    target: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    let rumoca_core::Expression::Tuple { elements, .. } = target else {
        return Ok(None);
    };
    if let Some(output_lengths) =
        function_call_declared_output_group_lengths(rhs, dae_model, structural_bindings, span)?
        && let Some(count) = tuple_bound_output_count(elements, &output_lengths, span)?
    {
        return Ok(Some(count));
    }
    let Some(output_groups) = derivative_rhs::function_call_projected_output_groups_with_owner(
        rhs,
        dae_model,
        structural_bindings,
        span,
    )?
    else {
        return Ok(None);
    };
    if elements.len() != output_groups.len() {
        return Ok(None);
    }
    let output_lengths: Vec<_> = output_groups.into_iter().map(|group| group.len()).collect();
    tuple_bound_output_count(elements, &output_lengths, span)
}

fn function_call_declared_output_group_lengths(
    rhs: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: false,
        ..
    } = rhs
    else {
        return Ok(None);
    };
    let Some(function) = dae_model.symbols.functions.get(name.var_name()) else {
        return Ok(None);
    };
    let Some(shape_env) = function_call_shape_env(
        name.as_str(),
        &function.inputs,
        args,
        dae_model,
        structural_bindings,
        span,
    )?
    else {
        return Ok(None);
    };
    let mut lengths = super::expression_vec_with_capacity(
        function.outputs.len(),
        "declared function output group count",
        span,
    )?;
    for output in &function.outputs {
        let Some(count) = function_param_scalar_count(output, &shape_env, span)? else {
            return Ok(None);
        };
        lengths.push(count);
    }
    Ok(Some(lengths))
}

struct FunctionCallShapeEnv {
    input_dims: IndexMap<String, Vec<usize>>,
    input_scalars: IndexMap<String, f64>,
}

fn function_call_shape_env(
    function_name: &str,
    inputs: &[rumoca_core::FunctionParam],
    args: &[rumoca_core::Expression],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    span: rumoca_core::Span,
) -> Result<Option<FunctionCallShapeEnv>, LowerError> {
    let (named_args, positional_args) =
        crate::lower::function_calls::split_named_and_positional_call_args(function_name, args)?;
    let mut positional_idx = 0usize;
    let mut dims_by_input = IndexMap::new();
    let mut scalars_by_input = IndexMap::new();
    for input in inputs {
        let actual = if let Some(actual) = named_args.get(input.name.as_str()).copied() {
            actual
        } else if let Some(actual) = positional_args.get(positional_idx).copied() {
            positional_idx += 1;
            actual
        } else if let Some(default) = input.default.as_ref() {
            default
        } else if input.dims.iter().all(|dim| *dim >= 0) {
            let dims = input
                .dims
                .iter()
                .map(|dim| usize::try_from(*dim))
                .collect::<Result<Vec<_>, _>>()
                .map_err(|_| {
                    LowerError::contract_violation(
                        format!(
                            "function `{function_name}` input `{}` has invalid declared dimension",
                            input.name
                        ),
                        span,
                    )
                })?;
            dims_by_input.insert(input.name.clone(), dims);
            continue;
        } else {
            return Ok(None);
        };
        let mut dims =
            derivative_rhs::expression_result_dims(actual, dae_model, structural_bindings, span)?;
        if let Some(value) = static_shape_scalar(actual, &scalars_by_input, structural_bindings)? {
            scalars_by_input.insert(input.name.clone(), value);
        }
        if input.dims.as_slice() == [0]
            && dims.is_empty()
            && is_zero_length_component_placeholder(actual)
        {
            dims.push(0);
        }
        dims_by_input.insert(input.name.clone(), dims);
    }
    Ok(Some(FunctionCallShapeEnv {
        input_dims: dims_by_input,
        input_scalars: scalars_by_input,
    }))
}

fn function_param_scalar_count(
    param: &rumoca_core::FunctionParam,
    shape_env: &FunctionCallShapeEnv,
    span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    if !param.shape_expr.is_empty() {
        let mut count = 1usize;
        for shape in &param.shape_expr {
            let Some(dim) = shape_expr_scalar_count_dim(shape, shape_env, span)? else {
                return Ok(None);
            };
            count = count.checked_mul(dim).ok_or_else(|| {
                LowerError::contract_violation(
                    format!(
                        "function output `{}` scalar count overflows host index range",
                        param.name
                    ),
                    span,
                )
            })?;
        }
        return Ok(Some(count));
    }
    if param.dims.is_empty() {
        return Ok(Some(1));
    }
    let mut count = 1usize;
    for dim in &param.dims {
        let dim = usize::try_from(*dim).map_err(|_| {
            LowerError::contract_violation(
                format!(
                    "function output `{}` has invalid declared dimension {dim}",
                    param.name
                ),
                span,
            )
        })?;
        count = count.checked_mul(dim).ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                    "function output `{}` scalar count overflows host index range",
                    param.name
                ),
                span,
            )
        })?;
    }
    Ok(Some(count))
}

fn shape_expr_scalar_count_dim(
    shape: &rumoca_core::Subscript,
    shape_env: &FunctionCallShapeEnv,
    span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    match shape {
        rumoca_core::Subscript::Index { value, .. } => usize::try_from(*value)
            .map(Some)
            .map_err(|_| LowerError::contract_violation("negative function output shape", span)),
        rumoca_core::Subscript::Expr { expr, .. } => {
            match shape_expr_builtin_size_dim(expr, shape_env, span)? {
                Some(dim) => Ok(Some(dim)),
                None => static_shape_scalar(expr, &shape_env.input_scalars, &IndexMap::new())?
                    .map(|value| usize_shape_dim(value, expr.span().unwrap_or(span)))
                    .transpose(),
            }
        }
        rumoca_core::Subscript::Colon { .. } => Ok(None),
    }
}

fn shape_expr_builtin_size_dim(
    expr: &rumoca_core::Expression,
    shape_env: &FunctionCallShapeEnv,
    span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    let rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args,
        ..
    } = expr
    else {
        return Ok(None);
    };
    let [base, dim] = args.as_slice() else {
        return Ok(None);
    };
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = base
    else {
        return Ok(None);
    };
    if !subscripts.is_empty() {
        return Ok(None);
    }
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(dim),
        ..
    } = dim
    else {
        return Ok(None);
    };
    let dim = usize::try_from(*dim)
        .ok()
        .and_then(|dim| dim.checked_sub(1))
        .ok_or_else(|| LowerError::contract_violation("size dimension must be positive", span))?;
    Ok(shape_env
        .input_dims
        .get(name.as_str())
        .and_then(|dims| dims.get(dim))
        .copied())
}

fn static_shape_scalar(
    expr: &rumoca_core::Expression,
    local_scalars: &IndexMap<String, f64>,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Option<f64>, LowerError> {
    match expr {
        rumoca_core::Expression::Literal { value, .. } => Ok(literal_scalar(value)),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Ok(local_scalars
            .get(name.as_str())
            .copied()
            .or_else(|| structural_bindings.get(name.as_str()).copied())),
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            let Some(value) = static_shape_scalar(rhs, local_scalars, structural_bindings)? else {
                return Ok(None);
            };
            match op {
                rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => Ok(Some(-value)),
                rumoca_core::OpUnary::Plus
                | rumoca_core::OpUnary::DotPlus
                | rumoca_core::OpUnary::Empty => Ok(Some(value)),
                rumoca_core::OpUnary::Not => Ok(None),
            }
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let Some(lhs) = static_shape_scalar(lhs, local_scalars, structural_bindings)? else {
                return Ok(None);
            };
            let Some(rhs) = static_shape_scalar(rhs, local_scalars, structural_bindings)? else {
                return Ok(None);
            };
            match op {
                rumoca_core::OpBinary::Add => Ok(Some(lhs + rhs)),
                rumoca_core::OpBinary::Sub => Ok(Some(lhs - rhs)),
                rumoca_core::OpBinary::Mul => Ok(Some(lhs * rhs)),
                rumoca_core::OpBinary::Div => Ok(Some(lhs / rhs)),
                _ => Ok(None),
            }
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            static_shape_builtin_scalar(function, args, local_scalars, structural_bindings)
        }
        _ => Ok(None),
    }
}

fn static_shape_builtin_scalar(
    function: &rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    local_scalars: &IndexMap<String, f64>,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Option<f64>, LowerError> {
    match (function, args) {
        (rumoca_core::BuiltinFunction::Integer, [arg]) => {
            Ok(static_shape_scalar(arg, local_scalars, structural_bindings)?.map(f64::trunc))
        }
        (rumoca_core::BuiltinFunction::Mod, [lhs, rhs]) => {
            let Some(lhs) = static_shape_scalar(lhs, local_scalars, structural_bindings)? else {
                return Ok(None);
            };
            let Some(rhs) = static_shape_scalar(rhs, local_scalars, structural_bindings)? else {
                return Ok(None);
            };
            Ok(Some(lhs - rhs * (lhs / rhs).floor()))
        }
        _ => Ok(None),
    }
}

fn literal_scalar(value: &rumoca_core::Literal) -> Option<f64> {
    match value {
        rumoca_core::Literal::Integer(value) => Some(*value as f64),
        rumoca_core::Literal::Real(value) => Some(*value),
        rumoca_core::Literal::Boolean(value) => Some(if *value { 1.0 } else { 0.0 }),
        rumoca_core::Literal::String(_) => None,
    }
}

fn usize_shape_dim(value: f64, span: rumoca_core::Span) -> Result<usize, LowerError> {
    if value.is_finite() && value >= 0.0 && value.fract() == 0.0 && value <= usize::MAX as f64 {
        Ok(value as usize)
    } else {
        Err(LowerError::contract_violation(
            format!("function output shape evaluated to invalid dimension {value}"),
            span,
        ))
    }
}

fn is_zero_length_component_placeholder(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::FunctionCall {
            name,
            is_constructor: true,
            ..
        } if name.as_str() == "Real"
    )
}

fn tuple_bound_output_count(
    elements: &[rumoca_core::Expression],
    output_lengths: &[usize],
    span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    if elements.len() != output_lengths.len() {
        return Ok(None);
    }
    let mut count = 0usize;
    for (target_element, rhs_len) in elements.iter().zip(output_lengths.iter().copied()) {
        let target_has_binding = tuple_target_element_has_binding(target_element);
        if rhs_len == 0 && target_has_binding {
            return Ok(None);
        }
        if rhs_len == 0 || !target_has_binding {
            continue;
        }
        count = count.checked_add(rhs_len).ok_or_else(|| {
            LowerError::contract_violation(
                "scalarized tuple residual binding count overflows host index range",
                span,
            )
        })?;
    }
    Ok(Some(count))
}

fn tuple_target_element_has_binding(target: &rumoca_core::Expression) -> bool {
    match target {
        rumoca_core::Expression::VarRef { .. } => true,
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            elements.iter().any(tuple_target_element_has_binding)
        }
        rumoca_core::Expression::FieldAccess { base, .. }
        | rumoca_core::Expression::Index { base, .. } => tuple_target_element_has_binding(base),
        _ => false,
    }
}

fn lower_tuple_target_element_values(
    builder: &mut LowerBuilder<'_>,
    target: &rumoca_core::Expression,
    expected_count: usize,
    span: rumoca_core::Span,
    scope: &Scope,
) -> Result<Vec<Reg>, LowerError> {
    if expected_count == 1
        && builder
            .infer_expr_dims(target, scope)
            .map(|dims| dims.is_empty())
            .unwrap_or(false)
    {
        let mut values =
            super::expression_vec_with_capacity(1, "scalarized tuple target scalar count", span)?;
        values.push(builder.lower_expr_with_source_context(target, span, scope, 0)?);
        return Ok(values);
    }
    builder.lower_array_like_values_with_source_context(target, span, scope, 0)
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
