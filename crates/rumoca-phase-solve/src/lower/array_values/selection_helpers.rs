use super::*;

pub(in crate::lower) fn matmul_shape_from_dims(
    lhs_dims: &[usize],
    rhs_dims: &[usize],
    scalar_count: usize,
) -> Option<MatMulShape> {
    match (lhs_dims, rhs_dims) {
        ([m, k], [rhs_inner, n]) if k == rhs_inner && m * n == scalar_count => Some(MatMulShape {
            m: *m,
            k: *k,
            n: *n,
        }),
        ([m, k], [rhs_inner]) if k == rhs_inner && *m == scalar_count => {
            Some(MatMulShape { m: *m, k: *k, n: 1 })
        }
        ([lhs_inner], [rhs_inner, n]) if lhs_inner == rhs_inner && *n == scalar_count => {
            Some(MatMulShape {
                m: 1,
                k: *lhs_inner,
                n: *n,
            })
        }
        ([lhs_inner], [rhs_inner]) if lhs_inner == rhs_inner && scalar_count == 1 => {
            Some(MatMulShape {
                m: 1,
                k: *lhs_inner,
                n: 1,
            })
        }
        _ => None,
    }
}

pub(in crate::lower) fn projected_record_field_expression(
    value: &rumoca_core::Expression,
    field: &str,
) -> Result<rumoca_core::Expression, LowerError> {
    match value {
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let span = value
                .require_span("projected record field if expression")?
                .span();
            let mut projected_branches = crate::lower_vec_with_capacity(
                branches.len(),
                "projected record field branch count",
                span,
            )?;
            for (cond, branch) in branches {
                projected_branches.push((
                    cond.clone(),
                    projected_record_field_expression(branch, field)?,
                ));
            }
            Ok(rumoca_core::Expression::If {
                branches: projected_branches,
                else_branch: Box::new(projected_record_field_expression(else_branch, field)?),
                span,
            })
        }
        _ => Ok(rumoca_core::Expression::FieldAccess {
            base: Box::new(value.clone()),
            field: field.to_string(),
            span: value
                .require_span("projected record field expression")?
                .span(),
        }),
    }
}

pub(in crate::lower) fn slice_indices_match(
    parts: &[ArraySelectionPart],
    indices: &[usize],
    output_tuple: &[usize],
) -> bool {
    let mut output_pos = 0usize;
    for (part, index) in parts.iter().zip(indices.iter().copied()) {
        if part.slice_indices.is_some() {
            let Some(expected) = output_tuple.get(output_pos).copied() else {
                return false;
            };
            if index != expected {
                return false;
            }
            output_pos += 1;
        }
    }
    output_pos == output_tuple.len()
}

pub(in crate::lower) fn indexed_record_field_key_indices(
    key: &str,
    base_key: &str,
    field: &str,
) -> Option<Vec<usize>> {
    let requested_key = format!("{base_key}.{field}");
    let mut candidate = key;
    let mut fields = Vec::new();
    while let Some((prefix, candidate_field)) = rumoca_core::split_last_top_level(candidate) {
        fields.push(candidate_field);
        if let Some((candidate_base, indices)) = parse_indexed_binding_key(prefix) {
            fields.reverse();
            let candidate_key = format!("{}.{}", candidate_base, fields.join("."));
            return (candidate_key == requested_key).then_some(indices);
        }
        candidate = prefix;
    }
    None
}

/// Cartesian product of per-dimension index selections into one-based
/// subscript tuples (the same ordering `collect_slice_binding_keys` uses).
pub(in crate::lower) fn collect_slice_index_combos(
    selections: &[Vec<usize>],
    depth: usize,
    current: &mut Vec<usize>,
    combos: &mut Vec<Vec<usize>>,
) {
    if depth >= selections.len() {
        combos.push(current.clone());
        return;
    }
    for &index in &selections[depth] {
        current.push(index);
        collect_slice_index_combos(selections, depth + 1, current, combos);
        current.pop();
    }
}

pub(in crate::lower) fn infer_dims_from_index_sets(
    index_sets: impl IntoIterator<Item = Vec<usize>>,
) -> Vec<usize> {
    let mut dims = Vec::new();
    for indices in index_sets {
        if indices.len() > dims.len() {
            dims.resize(indices.len(), 0);
        }
        for (idx, value) in indices.into_iter().enumerate() {
            dims[idx] = dims[idx].max(value);
        }
    }
    dims
}

/// One-pass parent-to-direct-children index over layout binding names,
/// mirroring `scope_key_direct_child_suffix` for generated keys: a binding
/// `<parent>.<seg>` is a direct child of `<parent>` when `<seg>` contains
/// neither `.` nor `[`.
pub(in crate::lower) fn build_scalarized_children_index(
    layout: &VarLayout,
) -> IndexMap<String, Vec<(ComponentReferenceKey, String)>> {
    let mut index: IndexMap<String, Vec<(ComponentReferenceKey, String)>> = IndexMap::new();
    for name in layout.bindings().keys() {
        let Some(dot) = name.rfind('.') else {
            continue;
        };
        let segment = &name[dot + 1..];
        if segment.is_empty() || segment.contains('[') {
            continue;
        }
        index
            .entry(name[..dot].to_string())
            .or_default()
            .push((generated_scope_key(name.clone()), name.clone()));
    }
    index
}
