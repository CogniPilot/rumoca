use super::{IndexedBinding, LowerBuilder, LowerError, Scope, unsupported_at};
use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_solve::ComponentReferenceKey;
use rumoca_ir_solve::Reg;
use rumoca_ir_solve::VarLayout;

fn subscripts_span_with_owner(
    subscripts: &[rumoca_core::Subscript],
    owner_span: rumoca_core::Span,
) -> rumoca_core::Span {
    subscripts
        .iter()
        .map(rumoca_core::Subscript::span)
        .find(|span| !span.is_dummy())
        .unwrap_or(owner_span)
}

fn required_subscripts_span_with_owner(
    subscripts: &[rumoca_core::Subscript],
    owner_span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    let span = subscripts_span_with_owner(subscripts, owner_span);
    if span.is_dummy() {
        return Err(LowerError::UnspannedContractViolation {
            reason: format!("missing source provenance for {context}"),
        });
    }
    Ok(span)
}

pub(in crate::lower) fn span_or_owner(
    span: rumoca_core::Span,
    owner_span: rumoca_core::Span,
) -> rumoca_core::Span {
    if span.is_dummy() { owner_span } else { span }
}

fn subscript_span_with_owner(
    subscript: &rumoca_core::Subscript,
    owner_span: rumoca_core::Span,
) -> rumoca_core::Span {
    span_or_owner(subscript.span(), owner_span)
}

pub(super) fn build_indexed_binding_map(
    layout: &VarLayout,
) -> IndexMap<ComponentReferenceKey, Vec<IndexedBinding>> {
    layout
        .indexed_bindings()
        .iter()
        .map(|(base, entries)| {
            (
                base.clone(),
                entries
                    .iter()
                    .map(|entry| IndexedBinding {
                        slot: entry.slot,
                        indices: entry.indices.clone(),
                    })
                    .collect(),
            )
        })
        .collect()
}

pub(super) fn variable_size(var: &dae::Variable) -> Result<usize, LowerError> {
    var.try_size()
        .map_err(|err| LowerError::contract_violation(err.to_string(), err.span()))
}

pub(super) fn indexed_entries_for_key(
    grouped: &IndexMap<ComponentReferenceKey, Vec<IndexedBinding>>,
    key: &str,
) -> Vec<IndexedBinding> {
    match grouped.get(&ComponentReferenceKey::generated(key)) {
        Some(entries) => entries.clone(),
        None => Vec::new(),
    }
}

/// Cached per-key view of an indexed binding group: inferred dims plus an
/// indices-to-position map over the rank-length entries. Deriving these per
/// reference (clone + scan + sort of the whole group) was quadratic in
/// model size for large arrays.
pub(super) struct IndexedMeta {
    pub(super) dims: Vec<usize>,
    pub(super) by_indices: IndexMap<Vec<usize>, usize>,
}

impl IndexedMeta {
    pub(super) fn build(entries: &[IndexedBinding]) -> Self {
        let dims = infer_indexed_dims(entries);
        let rank = entries
            .iter()
            .map(|entry| entry.indices.len())
            .max()
            .unwrap_or(0);
        let mut by_indices = IndexMap::with_capacity(entries.len());
        for (position, entry) in entries.iter().enumerate() {
            if entry.indices.len() == rank {
                by_indices.entry(entry.indices.clone()).or_insert(position);
            }
        }
        Self { dims, by_indices }
    }
}

pub(super) fn indexed_entries_for_reference(
    grouped: &IndexMap<ComponentReferenceKey, Vec<IndexedBinding>>,
    reference: &rumoca_core::Reference,
    span: rumoca_core::Span,
) -> Result<Vec<IndexedBinding>, LowerError> {
    let Some(key) = indexed_key_for_reference(grouped, reference, span)? else {
        return Ok(Vec::new());
    };
    let Some(entries) = grouped.get(&key) else {
        return Ok(Vec::new());
    };
    Ok(entries.clone())
}

/// Resolve the indexed-binding group key for a reference without cloning
/// the group. Returns `Ok(None)` when no group exists for the reference.
pub(super) fn indexed_key_for_reference(
    grouped: &IndexMap<ComponentReferenceKey, Vec<IndexedBinding>>,
    reference: &rumoca_core::Reference,
    span: rumoca_core::Span,
) -> Result<Option<ComponentReferenceKey>, LowerError> {
    let contained = |key: ComponentReferenceKey| grouped.contains_key(&key).then_some(key);
    if reference.is_generated() {
        return Ok(contained(ComponentReferenceKey::generated(
            reference.as_str(),
        )));
    }
    let Some(component_ref) = reference.component_ref() else {
        let generated_key = ComponentReferenceKey::generated(reference.as_str());
        #[cfg(test)]
        if grouped.contains_key(&generated_key) || span.is_dummy() {
            return Ok(contained(generated_key));
        }
        if !grouped.contains_key(&generated_key) {
            return Ok(None);
        }
        return Err(LowerError::contract_violation(
            format!(
                "indexed solve-layout lookup for `{}` lost structured component-reference metadata",
                reference.as_str()
            ),
            span,
        ));
    };
    #[cfg(test)]
    if let Some(key) =
        crate::test_support::fixture_key_for_component_ref(component_ref, reference.as_str())
    {
        return Ok(contained(key));
    }
    let key = ComponentReferenceKey::from_component_reference(component_ref).map_err(|err| {
        LowerError::contract_violation(
            format!(
                "indexed solve-layout lookup for `{}` has non-static component reference: {err}",
                reference.as_str(),
            ),
            err.span,
        )
    })?;
    Ok(contained(key))
}

pub(super) fn parse_indexed_binding_key(key: &str) -> Option<(String, Vec<usize>)> {
    let scalar = rumoca_core::parse_scalar_name(key)?;
    let indices = scalar
        .indices
        .into_iter()
        .map(|index| usize::try_from(index).ok().filter(|index| *index > 0))
        .collect::<Option<Vec<_>>>()?;
    (!indices.is_empty()).then_some((scalar.base.to_string(), indices))
}

pub(super) fn is_record_constructor_signature(
    name: &str,
    function: &rumoca_core::Function,
) -> bool {
    // MLS §12.6: record constructors are ordinary function calls. Compiled
    // lowering must therefore recognize constructor-shaped functions even when
    // the parser/front-end did not preserve an explicit constructor marker.
    if !function.locals.is_empty() || !function.body.is_empty() || function.external.is_some() {
        return false;
    }

    let function_leaf = crate::path_utils::leaf_segment(name);
    if function.inputs.is_empty() {
        return false;
    }

    if function.outputs.is_empty() {
        return function_leaf
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_uppercase());
    }

    if function.outputs.len() != 1 {
        return false;
    }

    let output = &function.outputs[0];
    if !output.dims.is_empty() {
        return false;
    }

    let output_leaf = crate::path_utils::leaf_segment(&output.type_name);
    !output_leaf.is_empty() && output_leaf == function_leaf && output.name == "res"
}

pub(super) fn sorted_flat_entries(entries: &[IndexedBinding]) -> Vec<&IndexedBinding> {
    let rank = entries
        .iter()
        .map(|entry| entry.indices.len())
        .max()
        .unwrap_or(0);
    let mut flat = entries
        .iter()
        .filter(|entry| entry.indices.len() == rank)
        .collect::<Vec<_>>();
    flat.sort_by(|lhs, rhs| lhs.indices.cmp(&rhs.indices));
    flat
}

/// Given a sorted `(indices, parameter-slot)` grid, return `(base, count,
/// row_major_strides)` when the elements form a dense, 1-based, rectangular
/// array whose parameter slots are contiguous in row-major order. Returns
/// `None` otherwise, so the caller falls back to the select-chain lowering.
pub(super) fn contiguous_param_grid_layout(
    grid: &[(Vec<usize>, usize)],
) -> Option<(usize, usize, Vec<usize>)> {
    let ndim = grid.first()?.0.len();
    if ndim == 0 {
        return None;
    }
    // Per-dimension extents; every index must be 1-based.
    let mut extents = vec![0usize; ndim];
    for (indices, _) in grid {
        if indices.len() != ndim {
            return None;
        }
        for (d, &i) in indices.iter().enumerate() {
            if i == 0 {
                return None;
            }
            extents[d] = extents[d].max(i);
        }
    }
    // Dense rectangular coverage: the element count must equal the box volume.
    let total: usize = extents.iter().product();
    if total != grid.len() {
        return None;
    }
    // Row-major strides (last dimension contiguous).
    let mut strides = vec![1usize; ndim];
    for d in (0..ndim - 1).rev() {
        strides[d] = strides[d + 1] * extents[d + 1];
    }
    // Slots must be contiguous in row-major order from a common base.
    let base = grid.iter().map(|(_, slot)| *slot).min()?;
    for (indices, slot) in grid {
        let flat: usize = indices
            .iter()
            .enumerate()
            .map(|(d, &i)| (i - 1) * strides[d])
            .sum();
        if *slot != base + flat {
            return None;
        }
    }
    Some((base, grid.len(), strides))
}

pub(super) fn infer_indexed_dims(entries: &[IndexedBinding]) -> Vec<usize> {
    let has_multi_dim = entries.iter().any(|entry| entry.indices.len() > 1);
    if has_multi_dim {
        let mut dims = Vec::<usize>::new();
        for entry in entries.iter().filter(|entry| entry.indices.len() > 1) {
            if entry.indices.len() > dims.len() {
                dims.resize(entry.indices.len(), 0);
            }
            for (idx, value) in entry.indices.iter().enumerate() {
                dims[idx] = dims[idx].max(*value);
            }
        }
        return dims;
    }

    let flat_count = entries
        .iter()
        .filter(|entry| entry.indices.len() == 1)
        .count();
    if flat_count > 0 {
        return vec![flat_count];
    }
    Vec::new()
}

pub(super) fn dims_scalar_count(
    dims: &[i64],
    context: impl Into<String>,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    let context = context.into();
    dims.iter().try_fold(1usize, |acc, dim| {
        let dim = usize::try_from(*dim).map_err(|_| {
            LowerError::contract_violation(
                format!("{context} has negative dimension `{dim}`"),
                span,
            )
        })?;
        acc.checked_mul(dim).ok_or_else(|| {
            LowerError::contract_violation(
                format!("{context} dimension product overflows usize"),
                span,
            )
        })
    })
}

pub(super) fn format_i64_dims(dims: &[i64]) -> String {
    let dims = dims
        .iter()
        .map(i64::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{dims}]")
}

pub(super) fn format_usize_dims(dims: &[usize]) -> String {
    let dims = dims
        .iter()
        .map(usize::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{dims}]")
}

pub(super) fn resolve_array_dims_for_value_count(
    dims: &[i64],
    value_count: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    let unknown_count = dims.iter().filter(|dim| **dim <= 0).count();
    if dims.is_empty() || unknown_count != 1 || value_count == 0 {
        return copy_i64_dims(dims, context, span);
    }
    let Some(known_product) = dims.iter().try_fold(1usize, |acc, dim| {
        if *dim > 0 {
            usize::try_from(*dim)
                .ok()
                .and_then(|dim| acc.checked_mul(dim))
        } else {
            Some(acc)
        }
    }) else {
        return Err(LowerError::contract_violation(
            format!("{context} known dimension product overflows usize"),
            span,
        ));
    };
    if known_product == 0 || !value_count.is_multiple_of(known_product) {
        return Err(LowerError::contract_violation(
            format!("{context} cannot infer one dynamic dimension from {value_count} value(s)"),
            span,
        ));
    }
    let inferred = i64::try_from(value_count / known_product).map_err(|_| {
        LowerError::contract_violation(format!("{context} inferred dimension exceeds i64"), span)
    })?;
    let mut resolved = crate::lower_vec_with_capacity(dims.len(), context, span)?;
    resolved.extend(
        dims.iter()
            .map(|dim| if *dim > 0 { *dim } else { inferred }),
    );
    Ok(resolved)
}

fn copy_i64_dims(
    dims: &[i64],
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    if dims.is_empty() {
        return Ok(Vec::new());
    }
    let mut copied = crate::lower_vec_with_capacity(dims.len(), context, span)?;
    copied.extend_from_slice(dims);
    Ok(copied)
}

pub(super) fn static_subscript_indices_with_owner(
    subscripts: &[rumoca_core::Subscript],
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    if subscripts.is_empty() {
        return Ok(Some(Vec::new()));
    }
    let mut indices = crate::lower_vec_with_capacity(
        subscripts.len(),
        "static subscript index count",
        subscripts_span_with_owner(subscripts, owner_span),
    )?;
    for sub in subscripts {
        match sub {
            rumoca_core::Subscript::Index { value, span } if *value > 0 => {
                indices.push(positive_i64_index(
                    *value,
                    span_or_owner(*span, owner_span),
                )?);
            }
            rumoca_core::Subscript::Expr { expr, span } => {
                match lower_static_index_expr_with_owner(expr, span_or_owner(*span, owner_span))? {
                    Some(value) => indices.push(value),
                    None => return Ok(None),
                }
            }
            rumoca_core::Subscript::Colon { span } => {
                return Err(unsupported_at(
                    "slice subscript `:` is unsupported",
                    span_or_owner(*span, owner_span),
                ));
            }
            _ => {
                return Err(unsupported_at(
                    "non-positive subscript is unsupported",
                    subscript_span_with_owner(sub, owner_span),
                ));
            }
        }
    }
    Ok(Some(indices))
}

pub(super) fn is_static_singleton_scalar_projection(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
) -> Result<bool, LowerError> {
    let owner_span = base.require_span("singleton scalar projection")?.span();
    let Some(indices) = static_subscript_indices_with_owner(subscripts, owner_span)? else {
        return Ok(false);
    };
    if indices.is_empty() || !indices.iter().all(|index| *index == 1) {
        return Ok(false);
    }
    Ok(matches!(
        base,
        rumoca_core::Expression::Binary { .. }
            | rumoca_core::Expression::Unary { .. }
            | rumoca_core::Expression::If { .. }
            | rumoca_core::Expression::FunctionCall { .. }
            | rumoca_core::Expression::Literal { .. }
    ))
}

pub(super) fn dynamic_binding_base_key(
    expr: &rumoca_core::Expression,
) -> Result<String, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
            ..
        } => {
            if subscripts.is_empty() {
                return Ok(name.as_str().to_string());
            }
            append_subscripts_to_key_with_owner(name.as_str().to_string(), subscripts, *span)
        }
        rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } => {
            let base_key = dynamic_binding_base_key(base)?;
            append_subscripts_to_key_with_owner(base_key, subscripts, *span)
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let base_key = dynamic_binding_base_key(base)?;
            Ok(format!("{base_key}.{field}"))
        }
        _ => Err(LowerError::DynamicBindingBase {
            tag: expr_tag(expr).to_string(),
        }),
    }
}

#[cfg(test)]
pub(super) fn lower_subscript_index(
    subscript: &rumoca_core::Subscript,
) -> Result<usize, LowerError> {
    lower_subscript_index_with_owner(subscript, subscript.span())
}

pub(super) fn lower_subscript_index_with_owner(
    subscript: &rumoca_core::Subscript,
    owner_span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    match subscript {
        rumoca_core::Subscript::Index { value, span } if *value > 0 => {
            positive_i64_index(*value, span_or_owner(*span, owner_span))
        }
        rumoca_core::Subscript::Expr { expr, span } => {
            lower_index_expr_with_owner(expr, span_or_owner(*span, owner_span))
        }
        rumoca_core::Subscript::Colon { span } => Err(unsupported_at(
            "slice subscript `:` is unsupported",
            span_or_owner(*span, owner_span),
        )),
        _ => Err(unsupported_at(
            "non-positive subscript is unsupported",
            subscript_span_with_owner(subscript, owner_span),
        )),
    }
}

pub(super) fn indexed_binding_key(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
) -> Result<String, LowerError> {
    let base_key = binding_base_key(base)?;
    let Some(owner_span) = base.span().filter(|span| !span.is_dummy()) else {
        return Err(LowerError::UnspannedContractViolation {
            reason: "missing source provenance for indexed binding key".to_string(),
        });
    };
    append_subscripts_to_key_with_owner(base_key, subscripts, owner_span)
}

pub(super) fn field_access_binding_key(
    base: &rumoca_core::Expression,
    field: &str,
) -> Result<String, LowerError> {
    let base_key = binding_base_key(base)?;
    Ok(format!("{base_key}.{field}"))
}

pub(super) fn binding_base_key(expr: &rumoca_core::Expression) -> Result<String, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
            ..
        } => {
            if subscripts.is_empty() {
                Ok(name.as_str().to_string())
            } else {
                append_subscripts_to_key_with_owner(name.as_str().to_string(), subscripts, *span)
            }
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => indexed_binding_key(base, subscripts),
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            field_access_binding_key(base, field)
        }
        _ => Err(unsupported_at(
            format!(
                "unsupported base expression for binding path: {}",
                expr_tag(expr)
            ),
            required_index_expr_span(expr, "binding path expression")?,
        )),
    }
}

pub(super) fn append_subscripts_to_key_with_owner(
    base: String,
    subscripts: &[rumoca_core::Subscript],
    owner_span: rumoca_core::Span,
) -> Result<String, LowerError> {
    if subscripts.is_empty() {
        return Ok(base);
    }

    let mut indices = crate::lower_vec_with_capacity(
        subscripts.len(),
        "binding key subscript count",
        required_subscripts_span_with_owner(subscripts, owner_span, "binding key subscripts")?,
    )?;
    for sub in subscripts {
        indices.push(lower_subscript_index_with_owner(sub, owner_span)?);
    }

    Ok(dae::format_subscript_key(&base, &indices))
}

pub(super) fn constructor_positional_field_index(field: &str) -> Option<usize> {
    match field {
        "re" => Some(0),
        "im" => Some(1),
        _ => None,
    }
}

fn lower_index_expr_with_owner(
    expr: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    match lower_static_index_expr_with_owner(expr, owner_span)? {
        Some(index) => Ok(index),
        None => Err(LowerError::DynamicSubscript),
    }
}

#[cfg(test)]
pub(super) fn lower_static_index_expr(
    expr: &rumoca_core::Expression,
) -> Result<Option<usize>, LowerError> {
    let owner_span = required_index_expr_span(expr, "static subscript expression")?;
    lower_static_index_expr_with_owner(expr, owner_span)
}

pub(super) fn lower_static_index_expr_with_owner(
    expr: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    let Some(raw) = lower_static_index_numeric(expr)? else {
        return Ok(None);
    };

    let rounded = raw.round();
    if rounded.is_finite() && rounded > 0.0 && (rounded - raw).abs() < f64::EPSILON {
        return Ok(Some(positive_f64_index(
            rounded,
            index_expr_owner_span(expr, owner_span, "static subscript expression")?,
        )?));
    }

    Err(unsupported_at(
        "subscript expression did not evaluate to a positive integer",
        index_expr_owner_span(expr, owner_span, "static subscript expression")?,
    ))
}

fn index_expr_owner_span(
    expr: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    expr.span()
        .or_else(|| (!owner_span.is_dummy()).then_some(owner_span))
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: format!("missing source provenance for {context}"),
        })
}

fn required_index_expr_span(
    expr: &rumoca_core::Expression,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    expr.span()
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: format!("missing source provenance for {context}"),
        })
}

pub(super) fn lower_static_index_numeric(
    expr: &rumoca_core::Expression,
) -> Result<Option<f64>, LowerError> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(v),
            ..
        } => Ok(Some(*v as f64)),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(v),
            ..
        } => Ok(Some(*v)),
        rumoca_core::Expression::Unary {
            op:
                rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus | rumoca_core::OpUnary::Empty,
            rhs,
            ..
        } => lower_static_index_numeric(rhs),
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus,
            rhs,
            ..
        } => Ok(lower_static_index_numeric(rhs)?.map(|value| -value)),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let Some(l) = lower_static_index_numeric(lhs)? else {
                return Ok(None);
            };
            let Some(r) = lower_static_index_numeric(rhs)? else {
                return Ok(None);
            };
            let value = match op {
                rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => l + r,
                rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => l - r,
                rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => l * r,
                rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => l / r,
                rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => l.powf(r),
                _ => return Ok(None),
            };
            Ok(Some(value))
        }
        _ => Ok(None),
    }
}

pub(super) fn lower_static_condition_truth(
    expr: &rumoca_core::Expression,
) -> Result<Option<bool>, LowerError> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(value),
            ..
        } => Ok(Some(*value)),
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs,
            ..
        } => Ok(lower_static_condition_truth(rhs)?.map(|value| !value)),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            lower_static_binary_condition(op, lhs, rhs)
        }
        _ => Ok(None),
    }
}

fn lower_static_binary_condition(
    op: &rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
) -> Result<Option<bool>, LowerError> {
    match op {
        rumoca_core::OpBinary::And => Ok(
            match (
                lower_static_condition_truth(lhs)?,
                lower_static_condition_truth(rhs)?,
            ) {
                (Some(false), _) | (_, Some(false)) => Some(false),
                (Some(true), Some(true)) => Some(true),
                _ => None,
            },
        ),
        rumoca_core::OpBinary::Or => Ok(
            match (
                lower_static_condition_truth(lhs)?,
                lower_static_condition_truth(rhs)?,
            ) {
                (Some(true), _) | (_, Some(true)) => Some(true),
                (Some(false), Some(false)) => Some(false),
                _ => None,
            },
        ),
        rumoca_core::OpBinary::Eq
        | rumoca_core::OpBinary::Neq
        | rumoca_core::OpBinary::Lt
        | rumoca_core::OpBinary::Le
        | rumoca_core::OpBinary::Gt
        | rumoca_core::OpBinary::Ge => {
            let Some(l) = lower_static_index_numeric(lhs)? else {
                return Ok(None);
            };
            let Some(r) = lower_static_index_numeric(rhs)? else {
                return Ok(None);
            };
            let comparison = match op {
                rumoca_core::OpBinary::Eq => l == r,
                rumoca_core::OpBinary::Neq => l != r,
                rumoca_core::OpBinary::Lt => l < r,
                rumoca_core::OpBinary::Le => l <= r,
                rumoca_core::OpBinary::Gt => l > r,
                rumoca_core::OpBinary::Ge => l >= r,
                _ => return Ok(None),
            };
            Ok(Some(comparison))
        }
        _ => Ok(None),
    }
}

pub(super) fn compile_time_var_key(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    const_scope: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<String, LowerError> {
    let name = name.as_str();
    if subscripts.is_empty() {
        return Ok(name.to_string());
    }
    let mut indices = crate::lower_vec_with_capacity(
        subscripts.len(),
        "compile-time subscript index count",
        required_subscripts_span_with_owner(
            subscripts,
            owner_span,
            "compile-time subscript index count",
        )?,
    )?;
    for sub in subscripts {
        let index = compile_time_subscript_index_with_owner(sub, const_scope, owner_span)?;
        indices.push(index);
    }
    Ok(dae::format_subscript_key(name, &indices))
}

#[cfg(test)]
pub(super) fn compile_time_subscript_index(
    subscript: &rumoca_core::Subscript,
    const_scope: &IndexMap<String, f64>,
) -> Result<usize, LowerError> {
    compile_time_subscript_index_with_owner(subscript, const_scope, subscript.span())
}

pub(super) fn compile_time_subscript_index_with_owner(
    subscript: &rumoca_core::Subscript,
    const_scope: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    match subscript {
        rumoca_core::Subscript::Index { value, span } if *value > 0 => {
            positive_i64_index(*value, span_or_owner(*span, owner_span))
        }
        rumoca_core::Subscript::Expr { expr, span } => {
            compile_time_index_expr_with_owner(expr, const_scope, span_or_owner(*span, owner_span))
        }
        rumoca_core::Subscript::Colon { span } => Err(unsupported_at(
            "slice subscript `:` is unsupported in compile-time context",
            span_or_owner(*span, owner_span),
        )),
        _ => Err(unsupported_at(
            "non-positive subscript is unsupported in compile-time context",
            subscript_span_with_owner(subscript, owner_span),
        )),
    }
}

pub(super) fn compile_time_index_expr_with_owner(
    expr: &rumoca_core::Expression,
    const_scope: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    let raw = compile_time_index_raw_with_owner(expr, const_scope, owner_span)?;

    let rounded = raw.round();
    if rounded.is_finite() && rounded > 0.0 && (rounded - raw).abs() < f64::EPSILON {
        return positive_f64_index(
            rounded,
            index_expr_owner_span(expr, owner_span, "compile-time subscript expression")?,
        );
    }

    Err(unsupported_at(
        "subscript expression did not evaluate to a positive integer",
        index_expr_owner_span(expr, owner_span, "compile-time subscript expression")?,
    ))
}

pub(in crate::lower) fn positive_i64_index(
    value: i64,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    usize::try_from(value).map_err(|_| {
        LowerError::contract_violation(
            format!("subscript index {value} exceeds host index range"),
            span,
        )
    })
}

pub(in crate::lower) fn positive_f64_index(
    value: f64,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    if !value.is_finite() || value <= 0.0 || value.fract().abs() > f64::EPSILON {
        return Err(unsupported_at(
            "subscript expression did not evaluate to a positive integer",
            span,
        ));
    }
    if value < usize::MAX as f64 {
        // Bounds and integrality are checked above; Rust has no TryFrom<f64>.
        return Ok(value as usize);
    }
    Err(LowerError::contract_violation(
        format!("subscript index {value} exceeds host index range"),
        span,
    ))
}

#[cfg(test)]
pub(super) fn compile_time_index_raw(
    expr: &rumoca_core::Expression,
    const_scope: &IndexMap<String, f64>,
) -> Result<f64, LowerError> {
    let owner_span = required_index_expr_span(expr, "compile-time index expression")?;
    compile_time_index_raw_with_owner(expr, const_scope, owner_span)
}

pub(super) fn compile_time_index_raw_with_owner(
    expr: &rumoca_core::Expression,
    const_scope: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<f64, LowerError> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(v),
            ..
        } => Ok(*v as f64),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(v),
            ..
        } => Ok(*v),
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
            ..
        } if subscripts.is_empty() => Ok(*const_scope.get(name.as_str()).ok_or_else(|| {
            unsupported_at(
                format!(
                    "subscript variable `{}` is not compile-time bound",
                    name.as_str()
                ),
                *span,
            )
        })?),
        rumoca_core::Expression::Unary {
            op:
                rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus | rumoca_core::OpUnary::Empty,
            rhs,
            ..
        } => compile_time_index_raw_with_owner(rhs, const_scope, owner_span),
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus,
            rhs,
            ..
        } => Ok(-compile_time_index_raw_with_owner(
            rhs,
            const_scope,
            owner_span,
        )?),
        rumoca_core::Expression::Binary { op, lhs, rhs, span } => {
            let nested_owner_span = if span.is_dummy() { owner_span } else { *span };
            let l = compile_time_index_raw_with_owner(lhs, const_scope, nested_owner_span)?;
            let r = compile_time_index_raw_with_owner(rhs, const_scope, nested_owner_span)?;
            match op {
                rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => Ok(l + r),
                rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => Ok(l - r),
                rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => Ok(l * r),
                rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => Ok(l / r),
                rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => Ok(l.powf(r)),
                _ => Err(unsupported_at(
                    "unsupported operator in compile-time subscript expression",
                    nested_owner_span,
                )),
            }
        }
        rumoca_core::Expression::BuiltinCall {
            function,
            args,
            span,
        } => compile_time_index_builtin(
            *function,
            args,
            const_scope,
            if span.is_dummy() { owner_span } else { *span },
        ),
        _ => Err(unsupported_at(
            "dynamic subscript expressions are unsupported in compile-time context",
            index_expr_owner_span(expr, owner_span, "compile-time subscript expression")?,
        )),
    }
}

fn compile_time_index_builtin(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    const_scope: &IndexMap<String, f64>,
    span: rumoca_core::Span,
) -> Result<f64, LowerError> {
    let Some(arg) = args.first() else {
        return Err(unsupported_at(
            "compile-time subscript builtin requires an argument",
            span,
        ));
    };
    let value = compile_time_index_raw_with_owner(arg, const_scope, span)?;
    match function {
        rumoca_core::BuiltinFunction::Floor | rumoca_core::BuiltinFunction::Integer => {
            Ok(value.floor())
        }
        rumoca_core::BuiltinFunction::Ceil => Ok(value.ceil()),
        _ => Err(unsupported_at(
            format!(
                "builtin `{}` is unsupported in compile-time subscript expression",
                function.name()
            ),
            span,
        )),
    }
}

pub(super) struct AssignmentTarget {
    pub base: String,
    pub indices: Option<Vec<usize>>,
}

pub(super) fn assignment_target(
    comp: &rumoca_core::ComponentReference,
    const_scope: &IndexMap<String, f64>,
) -> Result<AssignmentTarget, LowerError> {
    if comp.parts.is_empty() {
        return Err(LowerError::InvalidFunction {
            name: "<anonymous>".to_string(),
            reason: "assignment target has no path parts".to_string(),
        });
    }

    let mut indices = None;
    for (idx, part) in comp.parts.iter().enumerate() {
        if part.subs.is_empty() {
            continue;
        }
        if idx + 1 != comp.parts.len() || indices.is_some() {
            return Err(unsupported_at(
                format!(
                    "assignment target `{}` has unsupported nested subscripts",
                    comp.to_var_name().as_str()
                ),
                comp.span,
            ));
        }
        indices = Some(assignment_subscript_indices(&part.subs, const_scope, comp.span)?
            .ok_or_else(|| {
                unsupported_at(
                    format!(
                        "dynamic assignment target `{}` is unsupported in solve-IR function lowering",
                        comp.to_var_name().as_str()
                    ),
                    comp.span,
                )
            })?);
    }

    Ok(AssignmentTarget {
        base: rumoca_core::component_ref_to_base_reference(comp)
            .as_str()
            .to_string(),
        indices,
    })
}

fn assignment_subscript_indices(
    subscripts: &[rumoca_core::Subscript],
    const_scope: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    if let Some(indices) = static_subscript_indices_with_owner(subscripts, owner_span)? {
        return Ok(Some(indices));
    }
    let mut indices = crate::lower_vec_with_capacity(
        subscripts.len(),
        "assignment subscript index count",
        subscripts_span_with_owner(subscripts, owner_span),
    )?;
    for subscript in subscripts {
        let Ok(index) = compile_time_subscript_index_with_owner(subscript, const_scope, owner_span)
        else {
            return Ok(None);
        };
        indices.push(index);
    }
    Ok(Some(indices))
}

pub(super) fn eval_literal(literal: &rumoca_core::Literal) -> Result<f64, LowerError> {
    match literal {
        rumoca_core::Literal::Real(v) => Ok(*v),
        rumoca_core::Literal::Integer(v) => Ok(*v as f64),
        rumoca_core::Literal::Boolean(v) => {
            if *v {
                Ok(1.0)
            } else {
                Ok(0.0)
            }
        }
        rumoca_core::Literal::String(_) => Err(LowerError::Unsupported {
            reason: "string literal is unsupported in compile-time numeric context".to_string(),
        }),
    }
}

pub(super) fn expr_tag(expr: &rumoca_core::Expression) -> &'static str {
    match expr {
        rumoca_core::Expression::Binary { .. } => "Binary",
        rumoca_core::Expression::Unary { .. } => "Unary",
        rumoca_core::Expression::VarRef { .. } => "VarRef",
        rumoca_core::Expression::BuiltinCall { .. } => "BuiltinCall",
        rumoca_core::Expression::FunctionCall { .. } => "FunctionCall",
        rumoca_core::Expression::Literal { value: _, .. } => "Literal",
        rumoca_core::Expression::If { .. } => "If",
        rumoca_core::Expression::Array { .. } => "Array",
        rumoca_core::Expression::Tuple { .. } => "Tuple",
        rumoca_core::Expression::Range { .. } => "Range",
        rumoca_core::Expression::ArrayComprehension { .. } => "ArrayComprehension",
        rumoca_core::Expression::Index { .. } => "Index",
        rumoca_core::Expression::FieldAccess { .. } => "FieldAccess",
        rumoca_core::Expression::Empty { .. } => "Empty",
    }
}

pub(super) fn short_expr(expr: &rumoca_core::Expression, max_len: usize) -> String {
    let rendered = format!("{expr:?}");
    if rendered.len() <= max_len {
        return rendered;
    }
    format!("{}...", &rendered[..max_len])
}

pub(super) fn statement_tag(statement: &rumoca_core::Statement) -> &'static str {
    match statement {
        rumoca_core::Statement::Empty { .. } => "Empty",
        rumoca_core::Statement::Assignment { .. } => "Assignment",
        rumoca_core::Statement::Return { .. } => "Return",
        rumoca_core::Statement::Break { .. } => "Break",
        rumoca_core::Statement::For { .. } => "For",
        rumoca_core::Statement::While { .. } => "While",
        rumoca_core::Statement::If { .. } => "If",
        rumoca_core::Statement::When { .. } => "When",
        rumoca_core::Statement::FunctionCall { .. } => "FunctionCall",
        rumoca_core::Statement::Reinit { .. } => "Reinit",
        rumoca_core::Statement::Assert { .. } => "Assert",
    }
}

pub(super) fn resolve_intrinsic_builtin(name: &str) -> Option<rumoca_core::BuiltinFunction> {
    if let Some(builtin) = rumoca_core::BuiltinFunction::from_name(name) {
        return Some(builtin);
    }
    let builtin = rumoca_core::BuiltinFunction::from_name(crate::path_utils::leaf_segment(name))?;
    is_qualified_standard_math_intrinsic(name, builtin).then_some(builtin)
}

fn is_qualified_standard_math_intrinsic(name: &str, builtin: rumoca_core::BuiltinFunction) -> bool {
    let Some(short) = name.strip_prefix("Modelica.Math.") else {
        return false;
    };
    !short.contains('.') && rumoca_core::BuiltinFunction::from_name(short) == Some(builtin)
}

pub(super) fn intrinsic_short_name(name: &str) -> &str {
    crate::path_utils::leaf_segment(name)
}

pub(super) fn is_stream_passthrough_intrinsic(name: &str) -> bool {
    matches!(intrinsic_short_name(name), "actualStream" | "inStream")
}

pub(super) fn collect_scope_names(
    entry: &Scope,
    branches: &[Scope],
    else_scope: &Scope,
    span: rumoca_core::Span,
) -> Result<Vec<ComponentReferenceKey>, LowerError> {
    let entry_names = entry.keys_checked("branch merge entry scope name count", span)?;
    let scoped_count = branches.len().checked_add(1).ok_or_else(|| {
        LowerError::contract_violation(
            "branch merge scoped-name snapshot count exceeds host limits",
            span,
        )
    })?;
    let mut scoped_names = crate::lower_vec_with_capacity(
        scoped_count,
        "branch merge scoped-name snapshot count",
        span,
    )?;
    let mut capacity = entry_names.len();
    for scoped in branches.iter().chain(std::iter::once(else_scope)) {
        let names = scoped.keys_checked("branch merge scope name count", span)?;
        capacity = capacity.checked_add(names.len()).ok_or_else(|| {
            LowerError::contract_violation(
                "branch merge scope name count exceeds host limits",
                span,
            )
        })?;
        scoped_names.push(names);
    }
    let mut names =
        crate::lower_vec_with_capacity(capacity, "branch merge scope name count", span)?;
    names.extend(entry_names);
    for scoped in scoped_names {
        for name in scoped {
            if !entry.contains_key(&name) {
                names.push(name);
            }
        }
    }
    Ok(names)
}

pub(super) fn merge_branch_select(
    builder: &mut LowerBuilder<'_>,
    cond: Reg,
    span: rumoca_core::Span,
    branch_scope: &Scope,
    name: &ComponentReferenceKey,
    merged: Reg,
) -> Result<Reg, LowerError> {
    match branch_scope.get(name).copied() {
        Some(branch_value) => builder.emit_select_at(cond, branch_value, merged, span),
        None => Ok(merged),
    }
}

pub(super) fn build_range_values(start: i64, end: i64, step: i64) -> Vec<f64> {
    let mut values = Vec::new();
    let mut current = start;
    while if step > 0 {
        current <= end
    } else {
        current >= end
    } {
        values.push(current as f64);
        let Some(next) = current.checked_add(step) else {
            break;
        };
        current = next;
    }
    values
}

pub(super) fn eval_builtin_arg(
    builder: &LowerBuilder<'_>,
    args: &[rumoca_core::Expression],
    idx: usize,
    const_scope: &IndexMap<String, f64>,
) -> Result<f64, LowerError> {
    let Some(expr) = args.get(idx) else {
        return Ok(0.0);
    };
    builder.eval_compile_time_expr(expr, const_scope)
}

pub(super) fn bool_to_f64(value: bool) -> f64 {
    if value { 1.0 } else { 0.0 }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indexed_binding_key_uses_structured_scalar_name_parser() {
        assert_eq!(
            parse_indexed_binding_key("x[2]"),
            Some(("x".to_string(), vec![2]))
        );
        assert_eq!(
            parse_indexed_binding_key("a[index.with.dot].x[1, 3]"),
            Some(("a[index.with.dot].x".to_string(), vec![1, 3]))
        );
        assert_eq!(parse_indexed_binding_key("x[0]"), None);
        assert_eq!(parse_indexed_binding_key("x[-1]"), None);
        assert_eq!(parse_indexed_binding_key("x[1"), None);
        assert_eq!(parse_indexed_binding_key("[1]"), None);
    }

    #[test]
    fn assignment_target_keeps_indices_separate_from_base_path() {
        let comp = rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: "angles".to_string(),
                span: rumoca_core::Span::DUMMY,
                subs: vec![rumoca_core::Subscript::generated_index(
                    2,
                    rumoca_core::Span::DUMMY,
                )],
            }],
            def_id: None,
        };

        let target = assignment_target(&comp, &IndexMap::new()).expect("assignment target");

        assert_eq!(target.base, "angles");
        assert_eq!(target.indices, Some(vec![2]));
    }

    #[test]
    fn eval_literal_rejects_string_literals() {
        let err = eval_literal(&rumoca_core::Literal::String("not numeric".to_string()))
            .expect_err("string literal should not become a numeric zero");

        assert!(
            err.to_string()
                .contains("string literal is unsupported in compile-time numeric context"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn static_index_expr_rejects_host_index_overflow_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_lower_helpers_source_11.mo"),
            3,
            9,
        );
        let expr = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(usize::MAX as f64),
            span,
        };

        let err = lower_static_index_expr(&expr)
            .expect_err("oversized static index should not be narrowed");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
        assert!(err.reason().contains("exceeds host index range"));
    }

    #[test]
    fn positive_i64_index_rejects_negative_without_fabricating_span() {
        let err = positive_i64_index(-1, rumoca_core::Span::DUMMY)
            .expect_err("negative subscript index must fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(err.reason().contains("exceeds host index range"));
    }

    #[test]
    fn positive_f64_index_rejects_host_overflow_without_fabricating_span() {
        let err = positive_f64_index(usize::MAX as f64, rumoca_core::Span::DUMMY)
            .expect_err("oversized subscript index must fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(err.reason().contains("exceeds host index range"));
    }

    #[test]
    fn lower_subscript_index_rejects_colon_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("lower_subscript_colon.mo"),
            4,
            5,
        );
        let err = lower_subscript_index(&rumoca_core::Subscript::colon(span))
            .expect_err("colon subscript cannot lower as scalar index");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(err.reason(), "slice subscript `:` is unsupported");
    }

    #[test]
    fn compile_time_index_expr_rejects_host_index_overflow_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_lower_helpers_source_12.mo"),
            4,
            12,
        );
        let expr = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(usize::MAX as f64),
            span,
        };

        let err = compile_time_index_expr_with_owner(&expr, &IndexMap::new(), span)
            .expect_err("oversized compile-time index should not be narrowed");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
        assert!(err.reason().contains("exceeds host index range"));
    }

    #[test]
    fn compile_time_index_expr_rejects_host_index_overflow_without_fabricating_span() {
        let expr = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(usize::MAX as f64),
            span: rumoca_core::Span::DUMMY,
        };

        let err =
            compile_time_index_expr_with_owner(&expr, &IndexMap::new(), rumoca_core::Span::DUMMY)
                .expect_err("oversized compile-time index should not be narrowed");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason()
                .contains("missing source provenance for compile-time subscript expression")
        );
    }

    #[test]
    fn compile_time_index_expr_rejects_valid_index_without_source_provenance() {
        let expr = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(2),
            span: rumoca_core::Span::DUMMY,
        };

        let err =
            compile_time_index_expr_with_owner(&expr, &IndexMap::new(), rumoca_core::Span::DUMMY)
                .expect_err("valid unspanned compile-time index should fail provenance checks");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason()
                .contains("missing source provenance for compile-time subscript expression")
        );
    }

    #[test]
    fn compile_time_index_expr_uses_owner_span_for_generated_integer_index() {
        let owner_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("generated_integer_index_owner.mo"),
            6,
            11,
        );
        let expr = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(2),
            span: rumoca_core::Span::DUMMY,
        };

        let index = compile_time_index_expr_with_owner(&expr, &IndexMap::new(), owner_span)
            .expect("generated integer index should use owner span");

        assert_eq!(index, 2);
    }

    #[test]
    fn compile_time_index_raw_rejects_unspanned_expression() {
        let expr = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(2),
            span: rumoca_core::Span::DUMMY,
        };

        let err = compile_time_index_raw(&expr, &IndexMap::new())
            .expect_err("ownerless raw compile-time index should require provenance");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason()
                .contains("missing source provenance for compile-time index expression")
        );
    }

    #[test]
    fn compile_time_index_expr_uses_owner_span_for_generated_expression_overflow() {
        let owner_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("generated_subscript_owner.mo"),
            20,
            24,
        );
        let expr = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(usize::MAX as f64),
            span: rumoca_core::Span::DUMMY,
        };

        let err = compile_time_index_expr_with_owner(&expr, &IndexMap::new(), owner_span)
            .expect_err("oversized generated compile-time index should use owner span");

        assert_eq!(err.source_span(), Some(owner_span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
        assert!(err.reason().contains("exceeds host index range"));
    }

    #[test]
    fn compile_time_subscript_index_rejects_non_positive_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("compile_time_non_positive.mo"),
            9,
            10,
        );
        let err =
            compile_time_subscript_index(&rumoca_core::Subscript::index(0, span), &IndexMap::new())
                .expect_err("zero compile-time subscript must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "non-positive subscript is unsupported in compile-time context"
        );
    }

    #[test]
    fn compile_time_index_expr_rejects_unbound_var_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("compile_time_var.mo"),
            12,
            13,
        );
        let expr = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("i"),
            subscripts: Vec::new(),
            span,
        };

        let err = compile_time_index_expr_with_owner(&expr, &IndexMap::new(), span)
            .expect_err("unbound compile-time subscript variable must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "subscript variable `i` is not compile-time bound"
        );
    }

    #[test]
    fn positive_f64_index_rejects_non_positive_value_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_lower_helpers_source_13.mo"),
            1,
            2,
        );
        let err =
            positive_f64_index(0.0, span).expect_err("zero is not a positive Modelica subscript");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "subscript expression did not evaluate to a positive integer"
        );
    }
}
