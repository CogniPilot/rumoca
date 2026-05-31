use super::*;

/// Propagate array dims from unexpanded record array parents to scalar field variables.
///
/// When `Complex[3] aw` isn't array-expanded (dims not evaluable at instantiation),
/// fields like `aw.re` are scalar (dims=[]). This propagates parent dims=[3] so
/// that `aw.re` correctly counts as 3 scalars in todae balance checking.
///
/// Only propagates when the parent's per-element variables DON'T exist (i.e.,
/// `aw[1].re` is absent), meaning the expansion didn't happen.
type ParentDims = Vec<(String, Vec<i64>)>;
pub(crate) type DimMap = std::collections::HashMap<String, Vec<i64>>;

pub(crate) fn dims_have_prefix(dims: &[i64], prefix: &[i64]) -> bool {
    dims.len() >= prefix.len() && dims.iter().zip(prefix.iter()).all(|(a, b)| a == b)
}

pub(crate) fn join_parent_child_dims(parent: &[i64], child: &[i64]) -> Vec<i64> {
    let mut dims = Vec::with_capacity(parent.len() + child.len());
    dims.extend(parent.iter().copied());
    dims.extend(child.iter().copied());
    dims
}

pub(crate) fn normalize_inferred_dims_for_parent(inferred: &[i64], parent: &[i64]) -> Vec<i64> {
    if inferred.is_empty() {
        return parent.to_vec();
    }
    if dims_have_prefix(inferred, parent) {
        return inferred.to_vec();
    }
    join_parent_child_dims(parent, inferred)
}

pub(crate) fn matching_parent<'a>(
    name: &str,
    parent_dims: &'a ParentDims,
) -> Option<(&'a str, &'a [i64])> {
    for (prefix, dims) in parent_dims {
        let has_prefix = name.starts_with(prefix.as_str());
        let is_field = name.as_bytes().get(prefix.len()) == Some(&b'.');
        if has_prefix && is_field {
            return Some((prefix.as_str(), dims.as_slice()));
        }
    }
    None
}

pub(crate) fn infer_function_call_dims(
    name: &str,
    function_output_dims: &DimMap,
) -> Option<Vec<i64>> {
    function_output_dims.get(name).cloned()
}

pub(crate) fn infer_array_dims_from_expression(
    elements: &[Expression],
    is_matrix: bool,
    var_dims: &DimMap,
    function_output_dims: &DimMap,
) -> Option<Vec<i64>> {
    if elements.is_empty() {
        return Some(vec![0]);
    }
    if is_matrix {
        return match elements.first() {
            Some(Expression::Array { elements: row, .. }) => {
                Some(vec![elements.len() as i64, row.len() as i64])
            }
            _ => Some(vec![1, elements.len() as i64]),
        };
    }

    let mut dims = vec![elements.len() as i64];
    let inner_dims = elements
        .iter()
        .find_map(|element| infer_expr_dims(element, var_dims, function_output_dims));
    if let Some(inner) = inner_dims {
        dims.extend(inner);
    }
    Some(dims)
}

pub(crate) fn infer_array_comprehension_dims(
    expr: &Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&Expression>,
    var_dims: &DimMap,
    function_output_dims: &DimMap,
) -> Option<Vec<i64>> {
    if filter.is_some() {
        return None;
    }

    let mut dims = Vec::with_capacity(indices.len().saturating_add(1));
    for index in indices {
        let range_dims = infer_expr_dims(&index.range, var_dims, function_output_dims)
            .or_else(|| infer_array_dimensions(&index.range))?;
        if range_dims.is_empty() {
            return None;
        }
        let range_size = range_dims
            .iter()
            .copied()
            .fold(1i64, |acc, dim| acc.saturating_mul(dim.max(0)));
        dims.push(range_size);
    }

    if let Some(mut body_dims) = infer_expr_dims(expr, var_dims, function_output_dims)
        .or_else(|| infer_array_dimensions(expr))
    {
        dims.append(&mut body_dims);
    }
    Some(dims)
}

pub(crate) fn infer_expr_dims(
    expr: &Expression,
    var_dims: &DimMap,
    function_output_dims: &DimMap,
) -> Option<Vec<i64>> {
    match expr {
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => infer_array_dims_from_expression(elements, *is_matrix, var_dims, function_output_dims),
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => var_dims.get(name.as_str()).cloned(),
        Expression::VarRef { .. } | Expression::BuiltinCall { .. } => None,
        Expression::FunctionCall { name, .. } => {
            infer_function_call_dims(name.as_str(), function_output_dims)
        }
        Expression::Binary { lhs, rhs, .. } => {
            let lhs_dims = infer_expr_dims(lhs, var_dims, function_output_dims);
            let rhs_dims = infer_expr_dims(rhs, var_dims, function_output_dims);
            match (lhs_dims, rhs_dims) {
                (Some(l), Some(r)) if l == r => Some(l),
                (Some(l), Some(r)) if l.is_empty() && !r.is_empty() => Some(r),
                (Some(l), Some(r)) if r.is_empty() && !l.is_empty() => Some(l),
                (Some(l), None) if !l.is_empty() => Some(l),
                (None, Some(r)) if !r.is_empty() => Some(r),
                _ => None,
            }
        }
        Expression::Unary { rhs, .. } => infer_expr_dims(rhs, var_dims, function_output_dims),
        Expression::Literal { value: _, .. } => Some(Vec::new()),
        Expression::If {
            branches,
            else_branch,
            ..
        } => branches
            .iter()
            .find_map(|(_cond, branch_expr)| {
                infer_expr_dims(branch_expr, var_dims, function_output_dims)
            })
            .or_else(|| infer_expr_dims(else_branch, var_dims, function_output_dims)),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => infer_array_comprehension_dims(
            expr,
            indices,
            filter.as_deref(),
            var_dims,
            function_output_dims,
        ),
        Expression::Tuple { .. }
        | Expression::Range { .. }
        | Expression::Index { .. }
        | Expression::FieldAccess { .. }
        | Expression::Empty { .. } => None,
    }
}

pub(crate) fn collect_parent_dims(flat: &Model, overlay: &InstanceOverlay) -> ParentDims {
    overlay
        .components
        .values()
        .filter(|inst| !inst.is_primitive && !inst.dims.is_empty())
        .filter_map(|inst| {
            let path = inst.qualified_name.to_flat_string();
            let first_elem = format!("{path}[1].");
            let expanded = flat
                .variables
                .keys()
                .any(|k| k.as_str().starts_with(&first_elem));
            if expanded {
                None
            } else {
                Some((path, inst.dims.clone()))
            }
        })
        .collect()
}

pub(crate) fn collect_function_output_dims(flat: &Model) -> DimMap {
    flat.functions
        .iter()
        .filter_map(|(name, function)| {
            function
                .outputs
                .first()
                .map(|output| (name.as_str().to_string(), output.dims.clone()))
        })
        .collect()
}

pub(crate) fn infer_and_normalize_dims(
    expr: &Expression,
    parent: &[i64],
    var_dims: &DimMap,
    function_output_dims: &DimMap,
) -> Option<Vec<i64>> {
    infer_expr_dims(expr, var_dims, function_output_dims)
        .or_else(|| infer_array_dimensions(expr))
        .map(|dims| normalize_inferred_dims_for_parent(&dims, parent))
}

pub(crate) fn choose_more_specific_dims(first: Vec<i64>, second: Vec<i64>) -> Vec<i64> {
    if second.len() > first.len() {
        return second;
    }
    if first.len() > second.len() {
        return first;
    }

    let first_size = first.iter().product::<i64>();
    let second_size = second.iter().product::<i64>();
    if second_size > first_size {
        second
    } else {
        first
    }
}

pub(crate) fn infer_best_dims_for_var(
    var: &rumoca_ir_flat::Variable,
    parent: &[i64],
    var_dims: &DimMap,
    function_output_dims: &DimMap,
) -> Option<Vec<i64>> {
    let inferred_from_binding = var.binding.as_ref().and_then(|binding| {
        infer_and_normalize_dims(binding, parent, var_dims, function_output_dims)
    });
    let inferred_from_start = var
        .start
        .as_ref()
        .and_then(|start| infer_and_normalize_dims(start, parent, var_dims, function_output_dims));

    match (inferred_from_binding, inferred_from_start) {
        (Some(binding), Some(start)) => Some(choose_more_specific_dims(binding, start)),
        (Some(binding), None) => Some(binding),
        (None, Some(start)) => Some(start),
        (None, None) => {
            if var.binding.is_none() {
                Some(parent.to_vec())
            } else {
                None
            }
        }
    }
}

pub(crate) fn recover_nested_dims_from_bindings(
    flat: &mut Model,
    parent_dims: &ParentDims,
    function_output_dims: &DimMap,
) {
    for _ in 0..2 {
        let var_dims_lookup: DimMap = flat
            .variables
            .iter()
            .map(|(name, var)| (name.as_str().to_string(), var.dims.clone()))
            .collect();

        let mut changed = false;
        for var in flat.variables.values_mut() {
            let Some((_, parent)) = matching_parent(var.name.as_str(), parent_dims) else {
                continue;
            };
            let Some(inferred) =
                infer_best_dims_for_var(var, parent, &var_dims_lookup, function_output_dims)
            else {
                continue;
            };
            if inferred.len() > var.dims.len() {
                var.dims = inferred;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
}

pub(crate) fn prepend_missing_parent_dims(flat: &mut Model, parent_dims: &ParentDims) {
    for var in flat.variables.values_mut() {
        let Some((_, parent)) = matching_parent(var.name.as_str(), parent_dims) else {
            continue;
        };
        if dims_have_prefix(&var.dims, parent) {
            continue;
        }
        var.dims = join_parent_child_dims(parent, &var.dims);
    }
}

pub(crate) fn build_child_dim_hints(flat: &Model, parent_dims: &ParentDims) -> DimMap {
    let mut child_dim_hints = DimMap::new();
    for var in flat.variables.values() {
        let Some((prefix, parent)) = matching_parent(var.name.as_str(), parent_dims) else {
            continue;
        };
        if !dims_have_prefix(&var.dims, parent) || var.dims.len() <= parent.len() {
            continue;
        }
        let Some(suffix) = var
            .name
            .as_str()
            .strip_prefix(prefix)
            .and_then(|rest| rest.strip_prefix('.'))
        else {
            continue;
        };
        let child_dims = var.dims[parent.len()..].to_vec();
        let replace = child_dim_hints
            .get(suffix)
            .is_none_or(|existing| child_dims.len() > existing.len());
        if replace {
            child_dim_hints.insert(suffix.to_string(), child_dims);
        }
    }
    child_dim_hints
}

pub(crate) fn complete_child_dims_from_hints(
    flat: &mut Model,
    parent_dims: &ParentDims,
    hints: &DimMap,
) {
    for var in flat.variables.values_mut() {
        let Some((prefix, parent)) = matching_parent(var.name.as_str(), parent_dims) else {
            continue;
        };
        if !dims_have_prefix(&var.dims, parent) {
            continue;
        }
        let Some(suffix) = var
            .name
            .as_str()
            .strip_prefix(prefix)
            .and_then(|rest| rest.strip_prefix('.'))
        else {
            continue;
        };
        let Some(hinted_child_dims) = hints.get(suffix) else {
            continue;
        };
        let current_child_len = var.dims.len().saturating_sub(parent.len());
        if current_child_len < hinted_child_dims.len() {
            var.dims = join_parent_child_dims(parent, hinted_child_dims);
        }
    }
}

pub(crate) fn propagate_unexpanded_record_array_dims(flat: &mut Model, overlay: &InstanceOverlay) {
    let mut parent_dims = collect_parent_dims(flat, overlay);
    if parent_dims.is_empty() {
        return;
    }
    // Longest prefix wins when parents are nested.
    parent_dims.sort_by_key(|entry| std::cmp::Reverse(entry.0.len()));

    let function_output_dims = collect_function_output_dims(flat);
    recover_nested_dims_from_bindings(flat, &parent_dims, &function_output_dims);
    prepend_missing_parent_dims(flat, &parent_dims);
    let child_dim_hints = build_child_dim_hints(flat, &parent_dims);
    complete_child_dims_from_hints(flat, &parent_dims, &child_dim_hints);
}
