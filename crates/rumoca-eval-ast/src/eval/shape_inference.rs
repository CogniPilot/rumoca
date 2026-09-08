use super::*;

/// Infer dimensions from an array literal expression.
pub(super) fn infer_array_dims(
    elements: &[Expression],
    is_matrix: bool,
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<Vec<usize>> {
    if elements.is_empty() {
        return Some(vec![0]);
    }
    if is_matrix {
        return infer_matrix_constructor_dims(elements, ctx, scope);
    }
    let inner = infer_dimensions_from_binding_with_scope(elements.first()?, ctx, scope)?;
    for element in &elements[1..] {
        if infer_dimensions_from_binding_with_scope(element, ctx, scope)? != inner {
            return None;
        }
    }
    let mut dims = vec![elements.len()];
    dims.extend(inner);
    Some(dims)
}

fn infer_matrix_constructor_dims(
    elements: &[Expression],
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<Vec<usize>> {
    let has_nested_rows = matches!(elements.first(), Some(Expression::Array { .. }));
    if !has_nested_rows {
        return infer_matrix_row_dims(elements, ctx, scope).map(|(_, cols)| vec![1, cols]);
    }

    let mut rows = 0usize;
    let mut expected_cols = None;
    for row in elements {
        let Expression::Array {
            elements: row_elements,
            ..
        } = row
        else {
            return None;
        };
        let (row_count, col_count) = infer_matrix_row_dims(row_elements, ctx, scope)?;
        match expected_cols {
            Some(expected) if expected != col_count => return None,
            None => expected_cols = Some(col_count),
            _ => {}
        }
        rows = rows.checked_add(row_count)?;
    }

    Some(vec![rows, expected_cols.unwrap_or(0)])
}

fn infer_matrix_row_dims(
    elements: &[Expression],
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<(usize, usize)> {
    let single_entry = elements.len() == 1;
    let mut expected_rows = None;
    let mut cols = 0usize;
    for element in elements {
        let dims = infer_dimensions_from_binding_with_scope(element, ctx, scope)?;
        let (entry_rows, entry_cols) = matrix_entry_dims(&dims, single_entry)?;
        match expected_rows {
            Some(expected) if expected != entry_rows => return None,
            None => expected_rows = Some(entry_rows),
            _ => {}
        }
        cols = cols.checked_add(entry_cols)?;
    }
    Some((expected_rows?, cols))
}

fn matrix_entry_dims(dims: &[usize], single_entry: bool) -> Option<(usize, usize)> {
    match dims {
        [] => Some((1, 1)),
        [len] if single_entry => Some((*len, 1)),
        [len] => Some((1, *len)),
        [rows, cols] => Some((*rows, *cols)),
        _ => None,
    }
}

/// Infer dimensions for `cat(dim, A, B, ...)` concatenation.
fn infer_cat_dims_with_scope(
    args: &[Expression],
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<Vec<usize>> {
    let cat_dim = usize::try_from(ctx.eval_integer(&args[0], scope)?).ok()?;
    if cat_dim < 1 {
        return None;
    }
    let cat_idx = cat_dim - 1;
    let mut result_dims: Option<Vec<usize>> = None;
    for arg in &args[1..] {
        let arg_dims = infer_dimensions_from_binding_with_scope(arg, ctx, scope)?;
        match &mut result_dims {
            None => {
                if cat_idx >= arg_dims.len() {
                    return None;
                }
                result_dims = Some(arg_dims);
            }
            Some(dims) => {
                if arg_dims.len() != dims.len() || cat_idx >= dims.len() {
                    return None;
                }
                if dims
                    .iter()
                    .zip(&arg_dims)
                    .enumerate()
                    .any(|(index, (lhs, rhs))| index != cat_idx && lhs != rhs)
                {
                    return None;
                }
                dims[cat_idx] = dims[cat_idx].checked_add(arg_dims[cat_idx])?;
            }
        }
    }
    result_dims
}

/// Scope-aware dimension inference from array-constructing function calls.
pub(super) fn infer_dims_from_func_with_scope(
    function: &rumoca_ir_ast::ComponentReference,
    args: &[Expression],
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<Vec<usize>> {
    let func_name = function
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    if !ctx.is_predefined_function(function, &func_name) {
        return ctx.infer_user_function_dimensions_for_call(function, args, scope);
    }
    match func_name.as_str() {
        "zeros" | "ones" => args
            .iter()
            .map(|a| {
                ctx.eval_integer(a, scope)
                    .and_then(|i| usize::try_from(i).ok())
            })
            .collect(),
        "fill" if args.len() >= 2 => args[1..]
            .iter()
            .map(|a| {
                ctx.eval_integer(a, scope)
                    .and_then(|i| usize::try_from(i).ok())
            })
            .collect(),
        "identity" if args.len() == 1 => usize::try_from(ctx.eval_integer(&args[0], scope)?)
            .ok()
            .map(|n| vec![n, n]),
        "cat" if args.len() >= 2 => infer_cat_dims_with_scope(args, ctx, scope),
        // transpose(A) → swap dimensions
        "transpose" if args.len() == 1 => {
            let dims = infer_dimensions_from_binding_with_scope(&args[0], ctx, scope)?;
            if dims.len() == 2 {
                Some(vec![dims[1], dims[0]])
            } else {
                None
            }
        }
        // diagonal(v) → [n,n] from [n]
        "diagonal" if args.len() == 1 => {
            let dims = infer_dimensions_from_binding_with_scope(&args[0], ctx, scope)?;
            if dims.len() == 1 {
                Some(vec![dims[0], dims[0]])
            } else {
                None
            }
        }
        // symmetric(A) → same dims as A
        "symmetric" if args.len() == 1 => {
            infer_dimensions_from_binding_with_scope(&args[0], ctx, scope)
        }
        // linspace(a, b, n) → [n]
        "linspace" if args.len() == 3 => usize::try_from(ctx.eval_integer(&args[2], scope)?)
            .ok()
            .map(|n| vec![n]),
        // scalar(A) → [] (scalar)
        "scalar" if args.len() == 1 => Some(vec![]),
        // vector(A) → [product(dims)]
        "vector" if args.len() == 1 => {
            let dims = infer_dimensions_from_binding_with_scope(&args[0], ctx, scope)?;
            let total = dims
                .iter()
                .try_fold(1usize, |total, extent| total.checked_mul(*extent))?;
            Some(vec![total])
        }
        // matrix(A) → [n,m] reshape to 2D
        "matrix" if args.len() == 1 => {
            let dims = infer_dimensions_from_binding_with_scope(&args[0], ctx, scope)?;
            match dims.len() {
                0 => Some(vec![1, 1]),
                1 => Some(vec![dims[0], 1]),
                2 => Some(dims),
                _ => None,
            }
        }
        // cross(a, b) → [3] (cross product is always 3D)
        "cross" if args.len() == 2 => Some(vec![3]),
        // skew(v) → [3,3] from [3]
        "skew" if args.len() == 1 => Some(vec![3, 3]),
        // array(args...) → [len(args)] if all scalars, or [len(args), inner...] if arrays
        "array" if !args.is_empty() => infer_array_dims(args, false, ctx, scope),
        // Fallback: infer dimensions from user-defined function output type (MLS §12.4)
        _ => ctx.infer_user_function_dimensions_for_call(function, args, scope),
    }
}

/// Infer output array dimensions from a user-defined function call (MLS §12.4).
///
/// Looks up the function definition, finds the output variable's dimension
/// expressions, substitutes actual argument values, and evaluates them.
pub(super) fn infer_dims_from_user_func(
    func_name: &str,
    args: &[Expression],
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<Vec<usize>> {
    if ctx.func_eval_depth >= MAX_FUNC_EVAL_DEPTH {
        return None;
    }
    let func_def = lookup_function(func_name, ctx)?;
    if func_def.class_type != ClassType::Function {
        return None;
    }
    let local_ctx = build_func_eval_context(func_def, args, ctx, scope)?;
    let (_, output) = func_def
        .components
        .iter()
        .find(|(_, comp)| matches!(comp.causality, Causality::Output(_)))?;
    // Scalar output (no dimension expressions)
    if output.shape_expr.is_empty() {
        // MLS §12.4.6: scalar functions applied element-wise to arrays.
        // If any actual argument has array dims, the result inherits those dims.
        return Some(find_broadcast_dims(args, ctx, scope));
    }
    // Evaluate each dimension expression in the local context
    output
        .shape_expr
        .iter()
        .map(|sub| match sub {
            Subscript::Expression(expr) => {
                usize::try_from(eval_integer_with_scope(expr, &local_ctx, "")?).ok()
            }
            _ => None,
        })
        .collect()
}

/// Find the largest array dimensions among actual arguments (MLS §12.4.6).
///
/// When a scalar function is called with array arguments, the result has
/// the shape of the largest argument (element-wise broadcast).
fn find_broadcast_dims(args: &[Expression], ctx: &TypeCheckEvalContext, scope: &str) -> Vec<usize> {
    let mut best: Vec<usize> = vec![];
    for arg in args {
        // Skip named arguments, use the value inside
        let expr = if let Expression::NamedArgument { value, .. } = arg {
            value.as_ref()
        } else {
            arg
        };
        if let Some(dims) = infer_dimensions_from_binding_with_scope(expr, ctx, scope)
            && dims.len() > best.len()
        {
            best = dims;
        }
    }
    best
}

/// Compute range length from start, step, end.
fn compute_range_len(start: i64, step: i64, end: i64) -> Option<usize> {
    if step == 0 {
        return None;
    }
    let directed = if step > 0 {
        if end < start {
            return Some(0);
        }
        i128::from(end) - i128::from(start)
    } else {
        if start < end {
            return Some(0);
        }
        i128::from(start) - i128::from(end)
    };
    let magnitude = i128::from(step).abs();
    usize::try_from(directed / magnitude + 1).ok()
}

/// Compute range length for real-valued ranges.
///
/// MLS range expressions (`start:step:end`) enumerate values while stepping
/// toward the end value; the number of elements is therefore determined by the
/// reachable step count, not by integer-only arithmetic.
fn compute_range_len_real(start: f64, step: f64, end: f64) -> Option<usize> {
    if !start.is_finite() || !step.is_finite() || !end.is_finite() || step == 0.0 {
        return None;
    }

    if (step > 0.0 && start > end) || (step < 0.0 && start < end) {
        return Some(0);
    }

    let quotient = (end - start) / step;
    if !quotient.is_finite() {
        return None;
    }
    let nearest = quotient.round();
    let next = f64::from_bits(quotient.to_bits().checked_add(1)?);
    let quotient_for_floor = if (quotient - nearest).abs() <= next - quotient {
        nearest
    } else {
        quotient
    };
    let last_index = quotient_for_floor.floor();
    if last_index < 0.0 || last_index >= usize::MAX as f64 {
        return None;
    }
    (last_index as usize).checked_add(1)
}

pub(super) fn infer_range_len_numeric(
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<usize> {
    let int_start = ctx.eval_integer(start, scope);
    let int_end = ctx.eval_integer(end, scope);
    let int_step = step.map(|x| ctx.eval_integer(x, scope)).unwrap_or(Some(1));
    if let (Some(s), Some(e), Some(st)) = (int_start, int_end, int_step)
        && st != 0
    {
        return compute_range_len(s, st, e);
    }

    let s = ctx.eval_real(start, scope)?;
    let e = ctx.eval_real(end, scope)?;
    let st = step.map(|x| ctx.eval_real(x, scope)).unwrap_or(Some(1.0))?;
    compute_range_len_real(s, st, e)
}

#[cfg(test)]
mod tests;
