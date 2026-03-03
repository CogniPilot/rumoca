use crate::path_utils::last_top_level_subscript_span;
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
use std::collections::HashSet;

/// Compute the scalar size of a variable from its dimensions.
///
/// Per MLS §8.4, array variables represent multiple scalar unknowns.
/// The size is the product of all dimensions, or 1 for scalars.
pub(crate) fn compute_var_size(dims: &[i64]) -> usize {
    if dims.is_empty() {
        1
    } else {
        dims.iter().map(|&d| d.max(0) as usize).product()
    }
}

/// Compute scalar count for a subscripted variable access.
///
/// Each `Index` or `Expr` subscript reduces one dimension (selects a single element),
/// while `Colon` preserves the dimension. The result is the product of remaining dimensions.
/// For example, `M[1]` where M is `[2,3]` gives `3` (one row of a 2×3 matrix).
pub(crate) fn compute_subscripted_size(dims: &[i64], subscripts: &[flat::Subscript]) -> usize {
    let mut remaining_dims = Vec::new();
    let mut dim_idx = 0;
    for sub in subscripts {
        if dim_idx >= dims.len() {
            break;
        }
        match sub {
            flat::Subscript::Index(_) | flat::Subscript::Expr(_) => {
                // Indexing into a zero-sized dimension yields no scalar equations.
                if dims[dim_idx] <= 0 {
                    return 0;
                }
                // This subscript selects a single element, consuming one dimension
                dim_idx += 1;
            }
            flat::Subscript::Colon => {
                // Colon preserves the dimension
                remaining_dims.push(dims[dim_idx]);
                dim_idx += 1;
            }
        }
    }
    // Add any unsubscripted trailing dimensions
    remaining_dims.extend_from_slice(&dims[dim_idx..]);
    compute_var_size(&remaining_dims)
}

/// Check if a bracket-enclosed subscript content is evaluable integer arithmetic.
///
/// Returns true if the content consists only of digits and arithmetic operators
/// (and is not a simple integer), e.g. `(2 * 1) - 1`. Returns false for simple
/// integers (`1`, `2,3`), variable names (`n`, `i`), or mixed expressions.
pub(crate) fn is_evaluable_arithmetic(content: &str) -> bool {
    // Simple integer subscript: all digits/commas -> not arithmetic
    if content.bytes().all(|b| b.is_ascii_digit() || b == b',') {
        return false;
    }
    // Evaluable arithmetic: only digits and operators, must contain a digit
    let has_digit = content.bytes().any(|b| b.is_ascii_digit());
    let all_arith = content
        .bytes()
        .all(|b| b.is_ascii_digit() || matches!(b, b'+' | b'-' | b'*' | b'/' | b'(' | b')' | b' '));
    has_digit && all_arith
}

/// Check if a VarRef name contains evaluable integer arithmetic in subscripts.
///
/// Returns true ONLY when the subscript contains integer arithmetic expressions
/// (digits, `+`, `-`, `*`, `/`, parentheses, spaces) - e.g. `pc[((2 * 1) - 1)].i`.
/// Returns false for simple subscripts (`pc[1].i`), unresolved variable names
/// (`x[n]`, `suspend[i]`), or names without subscripts.
///
/// This distinguishes genuinely-evaluable arithmetic (from for-loop expansion
/// where arithmetic wasn't simplified) from truly absent variables (zero-sized
/// arrays or unresolved loop variables).
pub(crate) fn has_evaluable_arithmetic_subscript(name: &str) -> bool {
    let bytes = name.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] != b'[' {
            i += 1;
            continue;
        }
        let start = i + 1;
        let mut depth = 1;
        i += 1;
        while i < bytes.len() && depth > 0 {
            match bytes[i] {
                b'[' => depth += 1,
                b']' => depth -= 1,
                _ => {}
            }
            i += 1;
        }
        if i > start + 1 && is_evaluable_arithmetic(&name[start..i - 1]) {
            return true;
        }
    }
    false
}

/// Strip embedded subscripts from a variable name.
///
/// Returns the base name if subscripts are present, e.g. "RotationMatrix[1]" -> "RotationMatrix".
pub(crate) fn strip_embedded_subscripts(name: &str) -> Option<&str> {
    let (idx, _) = last_top_level_subscript_span(name)?;
    Some(&name[..idx])
}

/// Count the number of embedded subscript indices in a variable name.
///
/// Each comma-separated value inside brackets counts as one index.
/// E.g. "R[1]" -> 1, "M[1][2]" -> 2, "T[1,2]" -> 2, "A[1,2][3]" -> 3, "x" -> 0.
pub(crate) fn count_embedded_subscripts(name: &str) -> usize {
    let mut count = 0;
    let mut depth = 0i32;
    for c in name.chars() {
        match c {
            '[' => {
                if depth == 0 {
                    count += 1; // first index in this top-level bracket group
                }
                depth += 1;
            }
            ',' if depth == 1 => count += 1,
            ']' if depth > 0 => depth -= 1,
            _ => {}
        }
    }
    count
}

pub(crate) fn residual_lhs_var_ref(
    residual: &flat::Expression,
) -> Option<(&flat::VarName, &[flat::Subscript])> {
    let flat::Expression::Binary { op, lhs, .. } = residual else {
        return None;
    };
    if !matches!(op, rumoca_ir_ast::OpBinary::Sub(_)) {
        return None;
    }
    if let flat::Expression::VarRef { name, subscripts } = lhs.as_ref() {
        Some((name, subscripts))
    } else {
        None
    }
}

pub(crate) fn parse_embedded_subscript_indices(name: &str, flat: &flat::Model) -> Option<Vec<i64>> {
    let mut indices = Vec::new();
    let mut depth = 0i32;
    let mut sub_start = 0usize;
    for (i, c) in name.char_indices() {
        match c {
            '[' => {
                depth += 1;
                if depth == 1 {
                    sub_start = i + 1;
                }
            }
            ',' if depth == 1 => {
                let token = name[sub_start..i].trim();
                indices.push(eval_embedded_scalar_subscript(token, flat)?);
                sub_start = i + 1;
            }
            ']' => {
                if depth == 1 {
                    let token = name[sub_start..i].trim();
                    indices.push(eval_embedded_scalar_subscript(token, flat)?);
                }
                if depth > 0 {
                    depth -= 1;
                }
            }
            _ => {}
        }
    }
    if indices.is_empty() {
        None
    } else {
        Some(indices)
    }
}

pub(crate) fn eval_embedded_scalar_subscript(token: &str, flat: &flat::Model) -> Option<i64> {
    if token.contains(':') {
        return None;
    }
    try_eval_subscript_term(token, flat)
}

pub(crate) fn collect_linearized_embedded_lhs_bases(flat: &flat::Model) -> HashSet<flat::VarName> {
    let mut bases = HashSet::default();
    for eq in &flat.equations {
        let Some((lhs_name, lhs_subscripts)) = residual_lhs_var_ref(&eq.residual) else {
            continue;
        };
        if !lhs_subscripts.is_empty() {
            continue;
        }
        if !lhs_name.as_str().contains('[') || has_embedded_range_subscript(lhs_name.as_str()) {
            continue;
        }
        let Some(base) = strip_embedded_subscripts(lhs_name.as_str()) else {
            continue;
        };
        let base_name = flat::VarName::new(base);
        let Some(var) = flat.variables.get(&base_name) else {
            continue;
        };
        let Some(indices) = parse_embedded_subscript_indices(lhs_name.as_str(), flat) else {
            continue;
        };
        if indices.len() >= var.dims.len() {
            continue;
        }
        let out_of_bounds = indices
            .iter()
            .zip(var.dims.iter())
            .any(|(idx, dim)| *dim > 0 && (*idx < 1 || *idx > *dim));
        if out_of_bounds {
            bases.insert(base_name);
        }
    }
    bases
}

/// Check whether a variable name contains an embedded range subscript.
///
/// A range subscript has `:` inside brackets, e.g., `x[2:n]` or `x[1:(n-1)]`.
/// Simple scalar subscripts like `x[1]` or `x[1,2]` do not contain `:`.
pub(crate) fn has_embedded_range_subscript(name: &str) -> bool {
    let mut depth = 0i32;
    for c in name.chars() {
        match c {
            '[' => depth += 1,
            ']' => depth -= 1,
            ':' if depth == 1 => return true,
            _ => {}
        }
    }
    false
}

/// Compute the scalar size for a variable with embedded range subscripts.
///
/// For each bracket group, processes each comma-separated subscript:
/// - Range subscripts (containing `:`) are evaluated to compute the range size
/// - Scalar subscripts select a single element (size 1 per dimension)
///
/// Falls back to the full dimension size when a range cannot be evaluated.
pub(crate) fn compute_embedded_range_size(name: &str, dims: &[i64], flat: &flat::Model) -> usize {
    let mut result = 1usize;
    let mut dim_idx = 0;
    let mut depth = 0i32;
    let mut sub_start = 0usize;
    for (i, c) in name.char_indices() {
        match c {
            '[' => {
                depth += 1;
                if depth == 1 {
                    sub_start = i + 1;
                }
            }
            ',' if depth == 1 => {
                if dim_idx < dims.len() {
                    let sub = &name[sub_start..i];
                    result *= eval_single_subscript_size(sub.trim(), dims[dim_idx], flat);
                    dim_idx += 1;
                }
                sub_start = i + 1;
            }
            ']' => {
                if depth == 1 && dim_idx < dims.len() {
                    let sub = &name[sub_start..i];
                    result *= eval_single_subscript_size(sub.trim(), dims[dim_idx], flat);
                    dim_idx += 1;
                }
                depth -= 1;
            }
            _ => {}
        }
    }
    // Multiply by remaining unsubscripted dimensions
    for d in &dims[dim_idx..] {
        result *= *d as usize;
    }
    result
}

/// Evaluate the scalar size produced by a single subscript expression.
///
/// - Range subscripts like `2:n` return the range length (end - start + 1)
/// - Scalar subscripts return 1 (single element selected)
pub(crate) fn eval_single_subscript_size(sub: &str, dim: i64, flat: &flat::Model) -> usize {
    if !sub.contains(':') {
        return 1;
    }
    if dim <= 0 {
        return 0;
    }
    let parts: Vec<&str> = sub.splitn(3, ':').collect();
    if parts.len() < 2 {
        return dim as usize;
    }

    let start = try_eval_subscript_term(parts[0].trim(), flat);
    let end = try_eval_subscript_term(parts.last().unwrap().trim(), flat);
    range_size_from_optional_bounds(start, end, dim)
}

pub(crate) fn range_size_from_optional_bounds(
    start: Option<i64>,
    end: Option<i64>,
    dim: i64,
) -> usize {
    let clamp_to_dim = |v: i64| v.clamp(1, dim);

    match (start, end) {
        (Some(s), Some(e)) => {
            let lo = clamp_to_dim(s);
            let hi = clamp_to_dim(e);
            if hi >= lo { (hi - lo + 1) as usize } else { 0 }
        }
        (Some(s), None) => {
            let lo = clamp_to_dim(s);
            if dim >= lo {
                (dim - lo + 1) as usize
            } else {
                0
            }
        }
        (None, Some(e)) => {
            let hi = clamp_to_dim(e);
            if hi >= 1 { hi as usize } else { 0 }
        }
        (None, None) => dim as usize,
    }
}

/// Try to evaluate a subscript term to an integer value.
///
/// Handles integer literals (`2`), parameter references (`padeDelay1.n`),
/// parenthesized terms (`(expr)`), and simple binary expressions (`nx - 1`).
pub(crate) fn try_eval_subscript_term(term: &str, flat: &flat::Model) -> Option<i64> {
    let trimmed = term.trim();
    if let Ok(n) = trimmed.parse::<i64>() {
        return Some(n);
    }
    if trimmed.starts_with('(') && trimmed.ends_with(')') {
        return try_eval_subscript_term(&trimmed[1..trimmed.len() - 1], flat);
    }
    // Try as simple binary expression: "a - b", "a + b"
    if let Some(val) = try_eval_string_binary(trimmed, flat) {
        return Some(val);
    }
    let var = flat.variables.get(&flat::VarName::new(trimmed))?;
    // Dimension/range terms are structural. If a parameter/constant lacks an
    // explicit binding, fall back to its compile-time start value.
    let structural_value = var.binding.as_ref().or_else(|| {
        if var.evaluate
            || matches!(
                var.variability,
                ast::Variability::Parameter(_) | ast::Variability::Constant(_)
            )
        {
            return var.start.as_ref();
        }
        None
    })?;
    try_eval_flat_expr_i64(structural_value, flat, 0)
}

/// Try to evaluate a string-form binary expression like `"nx - 1"`.
///
/// Finds the last `+` or `-` operator at parenthesis depth 0
/// and evaluates each side recursively.
pub(crate) fn try_eval_string_binary(expr: &str, flat: &flat::Model) -> Option<i64> {
    let mut depth = 0i32;
    let mut last_op_pos = None;
    let mut last_op_char = ' ';
    let bytes = expr.as_bytes();
    for (i, &b) in bytes.iter().enumerate() {
        match b {
            b'(' => depth += 1,
            b')' => depth -= 1,
            b'+' | b'-' if depth == 0 && i > 0 => {
                last_op_pos = Some(i);
                last_op_char = b as char;
            }
            _ => {}
        }
    }
    let pos = last_op_pos?;
    let left = try_eval_subscript_term(&expr[..pos], flat)?;
    let right = try_eval_subscript_term(&expr[pos + 1..], flat)?;
    match last_op_char {
        '+' => Some(left + right),
        '-' => Some(left - right),
        _ => None,
    }
}

/// Recursively evaluate a flat expression to an integer value.
///
/// Handles literals, parameter references (via binding), binary arithmetic
/// (`+`, `-`, `*`), and `size(array, dim)` calls.
pub(crate) fn try_eval_flat_expr_i64(
    expr: &flat::Expression,
    flat: &flat::Model,
    depth: u8,
) -> Option<i64> {
    if depth > 8 {
        return None;
    }
    match expr {
        flat::Expression::Literal(flat::Literal::Integer(n)) => Some(*n),
        flat::Expression::Literal(flat::Literal::Real(f)) => {
            let n = *f as i64;
            if (n as f64 - *f).abs() < 0.001 {
                Some(n)
            } else {
                None
            }
        }
        flat::Expression::VarRef { name, .. } => {
            let var = flat.variables.get(name)?;
            let binding = var.binding.as_ref()?;
            try_eval_flat_expr_i64(binding, flat, depth + 1)
        }
        flat::Expression::Binary { op, lhs, rhs } => {
            let l = try_eval_flat_expr_i64(lhs, flat, depth + 1)?;
            let r = try_eval_flat_expr_i64(rhs, flat, depth + 1)?;
            eval_binary_op_i64(op, l, r)
        }
        flat::Expression::BuiltinCall { function, args }
            if matches!(function, rumoca_ir_flat::BuiltinFunction::Size) && args.len() == 2 =>
        {
            eval_size_call_i64(&args[0], &args[1], flat, depth)
        }
        _ => None,
    }
}

/// Evaluate a binary arithmetic operation on two integer values.
pub(crate) fn eval_binary_op_i64(op: &rumoca_ir_ast::OpBinary, l: i64, r: i64) -> Option<i64> {
    match op {
        rumoca_ir_ast::OpBinary::Add(_) | rumoca_ir_ast::OpBinary::AddElem(_) => Some(l + r),
        rumoca_ir_ast::OpBinary::Sub(_) | rumoca_ir_ast::OpBinary::SubElem(_) => Some(l - r),
        rumoca_ir_ast::OpBinary::Mul(_) | rumoca_ir_ast::OpBinary::MulElem(_) => Some(l * r),
        rumoca_ir_ast::OpBinary::Div(_) | rumoca_ir_ast::OpBinary::DivElem(_) => {
            if r != 0 {
                Some(l / r)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Evaluate `size(array, dim)` by looking up the variable's dimensions.
pub(crate) fn eval_size_call_i64(
    array_arg: &flat::Expression,
    dim_arg: &flat::Expression,
    flat: &flat::Model,
    depth: u8,
) -> Option<i64> {
    let dim = try_eval_flat_expr_i64(dim_arg, flat, depth + 1)? as usize;
    if let flat::Expression::VarRef { name, .. } = array_arg {
        let var = flat.variables.get(name)?;
        let idx = dim.checked_sub(1)?;
        var.dims.get(idx).copied()
    } else {
        None
    }
}

/// Resolve the scalar size of a VarRef with embedded subscripts.
///
/// When flatten expands multi-dim array equations (e.g. `RotationMatrix = {{...}}`),
/// it creates partially-expanded equations like `RotationMatrix[1] = ...` where
/// the subscript is embedded in the flat::VarName string. This function strips the
/// embedded subscripts, looks up the base variable, and computes the remaining
/// scalar count from the unindexed dimensions.
pub(crate) fn resolve_embedded_subscript_size(
    name: &str,
    flat: &flat::Model,
    linearized_embedded_lhs_bases: &HashSet<flat::VarName>,
) -> Option<usize> {
    let base = strip_embedded_subscripts(name)?;
    let base_name = flat::VarName::new(base);
    let var = flat.variables.get(&base_name)?;
    let n = count_embedded_subscripts(name);
    if n > 0 && n < var.dims.len() {
        if linearized_embedded_lhs_bases.contains(&base_name) {
            return Some(1);
        }
        Some(compute_var_size(&var.dims[n..]))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_subscripted_size_mixed_index_and_colon() {
        let size = compute_subscripted_size(
            &[2, 3, 4],
            &[flat::Subscript::Index(1), flat::Subscript::Colon],
        );
        assert_eq!(size, 12);
    }

    #[test]
    fn test_range_size_from_optional_bounds_clamps_to_dimension() {
        let size = range_size_from_optional_bounds(Some(-2), Some(10), 5);
        assert_eq!(size, 5);
    }
}
