use super::{IndexedBinding, LowerBuilder, LowerError, Scope};
use indexmap::IndexMap;
use rumoca_core::ComponentPath;
use rumoca_ir_solve::Reg;
use rumoca_ir_solve::VarLayout;

pub(super) fn build_indexed_binding_map(
    layout: &VarLayout,
) -> IndexMap<ComponentPath, Vec<IndexedBinding>> {
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

pub(super) fn indexed_entries_for_key(
    grouped: &IndexMap<ComponentPath, Vec<IndexedBinding>>,
    key: &str,
) -> Vec<IndexedBinding> {
    grouped
        .get(&ComponentPath::from_flat_path(key))
        .cloned()
        .unwrap_or_default()
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

pub(crate) fn is_record_constructor_signature(
    name: &str,
    function: &rumoca_core::Function,
) -> bool {
    // MLS §12.6: record constructors are ordinary function calls. Compiled
    // lowering must therefore recognize constructor-shaped functions even when
    // the parser/front-end did not preserve an explicit constructor marker.
    if function.is_constructor {
        return true;
    }
    if !function.locals.is_empty() || !function.body.is_empty() || function.external.is_some() {
        return false;
    }

    let function_leaf = rumoca_core::top_level_last_segment(name);
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

    let output_leaf = rumoca_core::top_level_last_segment(&output.type_name);
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

pub(super) fn dims_scalar_count(dims: &[i64]) -> usize {
    let count = dims.iter().try_fold(1usize, |acc, dim| {
        usize::try_from(*dim)
            .ok()
            .filter(|dim| *dim > 0)?
            .checked_mul(acc)
    });
    count.unwrap_or(1).max(1)
}

pub(super) fn resolve_array_dims_for_value_count(dims: &[i64], value_count: usize) -> Vec<i64> {
    let unknown_count = dims.iter().filter(|dim| **dim <= 0).count();
    if dims.is_empty() || unknown_count != 1 || value_count == 0 {
        return dims.to_vec();
    }
    let Some(known_product) = dims.iter().try_fold(1usize, |acc, dim| {
        if *dim > 0 {
            acc.checked_mul(*dim as usize)
        } else {
            Some(acc)
        }
    }) else {
        return dims.to_vec();
    };
    if known_product == 0 || !value_count.is_multiple_of(known_product) {
        return dims.to_vec();
    }
    let inferred = (value_count / known_product) as i64;
    dims.iter()
        .map(|dim| if *dim > 0 { *dim } else { inferred })
        .collect()
}

pub(super) fn static_subscript_indices(
    subscripts: &[rumoca_core::Subscript],
) -> Result<Option<Vec<usize>>, LowerError> {
    if subscripts.is_empty() {
        return Ok(Some(Vec::new()));
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        match sub {
            rumoca_core::Subscript::Index { value: v, .. } if *v > 0 => indices.push(*v as usize),
            rumoca_core::Subscript::Expr { expr, .. } => match lower_static_index_expr(expr)? {
                Some(value) => indices.push(value),
                None => return Ok(None),
            },
            rumoca_core::Subscript::Colon { .. } => {
                return Err(LowerError::Unsupported {
                    reason: "slice subscript `:` is unsupported".to_string(),
                });
            }
            _ => {
                return Err(LowerError::Unsupported {
                    reason: "non-positive subscript is unsupported".to_string(),
                });
            }
        }
    }
    Ok(Some(indices))
}

pub(super) fn is_static_singleton_scalar_projection(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
) -> Result<bool, LowerError> {
    let Some(indices) = static_subscript_indices(subscripts)? else {
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
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                return Ok(name.as_str().to_string());
            }
            append_subscripts_to_key(name.as_str().to_string(), subscripts)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let base_key = dynamic_binding_base_key(base)?;
            append_subscripts_to_key(base_key, subscripts)
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let base_key = dynamic_binding_base_key(base)?;
            Ok(format!("{base_key}.{field}"))
        }
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => {
            Ok("__rumoca_literal__".to_string())
        }
        _ => Err(LowerError::Unsupported {
            reason: format!(
                "unsupported base expression for dynamic binding path: {}",
                expr_tag(expr)
            ),
        }),
    }
}

pub(super) fn lower_subscript_index(
    subscript: &rumoca_core::Subscript,
) -> Result<usize, LowerError> {
    match subscript {
        rumoca_core::Subscript::Index { value: v, .. } if *v > 0 => Ok(*v as usize),
        rumoca_core::Subscript::Expr { expr, .. } => lower_index_expr(expr),
        rumoca_core::Subscript::Colon { .. } => Err(LowerError::Unsupported {
            reason: "slice subscript `:` is unsupported".to_string(),
        }),
        _ => Err(LowerError::Unsupported {
            reason: "non-positive subscript is unsupported".to_string(),
        }),
    }
}

pub(super) fn indexed_binding_key(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
) -> Result<String, LowerError> {
    let base_key = binding_base_key(base)?;
    append_subscripts_to_key(base_key, subscripts)
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
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                Ok(name.as_str().to_string())
            } else {
                append_subscripts_to_key(name.as_str().to_string(), subscripts)
            }
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => indexed_binding_key(base, subscripts),
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            field_access_binding_key(base, field)
        }
        _ => Err(LowerError::Unsupported {
            reason: format!(
                "unsupported base expression for binding path: {}",
                expr_tag(expr)
            ),
        }),
    }
}

pub(super) fn append_subscripts_to_key(
    base: String,
    subscripts: &[rumoca_core::Subscript],
) -> Result<String, LowerError> {
    if subscripts.is_empty() {
        return Ok(base);
    }

    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        indices.push(lower_subscript_index(sub)?);
    }

    if indices.len() == 1 {
        return Ok(format!("{base}[{}]", indices[0]));
    }

    let suffix = indices
        .iter()
        .map(std::string::ToString::to_string)
        .collect::<Vec<_>>()
        .join(",");
    Ok(format!("{base}[{suffix}]"))
}

pub(super) fn constructor_positional_field_index(field: &str) -> Option<usize> {
    match field {
        "re" => Some(0),
        "im" => Some(1),
        _ => None,
    }
}

pub(super) fn lower_index_expr(expr: &rumoca_core::Expression) -> Result<usize, LowerError> {
    match lower_static_index_expr(expr)? {
        Some(index) => Ok(index),
        None => Err(LowerError::Unsupported {
            reason: format!("dynamic subscript expressions are unsupported: {expr:?}"),
        }),
    }
}

pub(super) fn lower_static_index_expr(
    expr: &rumoca_core::Expression,
) -> Result<Option<usize>, LowerError> {
    let Some(raw) = lower_static_index_numeric(expr)? else {
        return Ok(None);
    };

    let rounded = raw.round();
    if rounded.is_finite() && rounded > 0.0 && (rounded - raw).abs() < f64::EPSILON {
        return Ok(Some(rounded as usize));
    }

    Err(LowerError::Unsupported {
        reason: "subscript expression did not evaluate to a positive integer".to_string(),
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
            Ok(Some(match op {
                rumoca_core::OpBinary::Eq => l == r,
                rumoca_core::OpBinary::Neq => l != r,
                rumoca_core::OpBinary::Lt => l < r,
                rumoca_core::OpBinary::Le => l <= r,
                rumoca_core::OpBinary::Gt => l > r,
                rumoca_core::OpBinary::Ge => l >= r,
                _ => unreachable!("filtered comparison op"),
            }))
        }
        _ => Ok(None),
    }
}

pub(super) fn compile_time_var_key(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    const_scope: &IndexMap<String, f64>,
) -> Result<String, LowerError> {
    let name = name.as_str();
    if subscripts.is_empty() {
        return Ok(name.to_string());
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        let index = compile_time_subscript_index(sub, const_scope)?;
        indices.push(index.to_string());
    }
    if indices.len() == 1 {
        Ok(format!("{}[{}]", name, indices[0]))
    } else {
        Ok(format!("{}[{}]", name, indices.join(",")))
    }
}

pub(super) fn compile_time_subscript_index(
    subscript: &rumoca_core::Subscript,
    const_scope: &IndexMap<String, f64>,
) -> Result<usize, LowerError> {
    match subscript {
        rumoca_core::Subscript::Index { value, .. } if *value > 0 => Ok(*value as usize),
        rumoca_core::Subscript::Expr { expr, .. } => compile_time_index_expr(expr, const_scope),
        rumoca_core::Subscript::Colon { .. } => Err(LowerError::Unsupported {
            reason: "slice subscript `:` is unsupported in compile-time context".to_string(),
        }),
        _ => Err(LowerError::Unsupported {
            reason: "non-positive subscript is unsupported in compile-time context".to_string(),
        }),
    }
}

pub(super) fn compile_time_index_expr(
    expr: &rumoca_core::Expression,
    const_scope: &IndexMap<String, f64>,
) -> Result<usize, LowerError> {
    let raw = compile_time_index_raw(expr, const_scope)?;

    let rounded = raw.round();
    if rounded.is_finite() && rounded > 0.0 && (rounded - raw).abs() < f64::EPSILON {
        return Ok(rounded as usize);
    }

    Err(LowerError::Unsupported {
        reason: "subscript expression did not evaluate to a positive integer".to_string(),
    })
}

pub(super) fn compile_time_index_raw(
    expr: &rumoca_core::Expression,
    const_scope: &IndexMap<String, f64>,
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
            name, subscripts, ..
        } if subscripts.is_empty() => {
            Ok(*const_scope
                .get(name.as_str())
                .ok_or_else(|| LowerError::Unsupported {
                    reason: format!(
                        "subscript variable `{}` is not compile-time bound",
                        name.as_str()
                    ),
                })?)
        }
        rumoca_core::Expression::Unary {
            op:
                rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus | rumoca_core::OpUnary::Empty,
            rhs,
            ..
        } => compile_time_index_raw(rhs, const_scope),
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus,
            rhs,
            ..
        } => Ok(-compile_time_index_raw(rhs, const_scope)?),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let l = compile_time_index_raw(lhs, const_scope)?;
            let r = compile_time_index_raw(rhs, const_scope)?;
            match op {
                rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => Ok(l + r),
                rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => Ok(l - r),
                rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => Ok(l * r),
                rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => Ok(l / r),
                rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => Ok(l.powf(r)),
                _ => Err(LowerError::Unsupported {
                    reason: "unsupported operator in compile-time subscript expression".to_string(),
                }),
            }
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            compile_time_index_builtin(*function, args, const_scope)
        }
        _ => Err(LowerError::Unsupported {
            reason: "dynamic subscript expressions are unsupported in compile-time context"
                .to_string(),
        }),
    }
}

fn compile_time_index_builtin(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    const_scope: &IndexMap<String, f64>,
) -> Result<f64, LowerError> {
    let Some(arg) = args.first() else {
        return Err(LowerError::Unsupported {
            reason: "compile-time subscript builtin requires an argument".to_string(),
        });
    };
    let value = compile_time_index_raw(arg, const_scope)?;
    match function {
        rumoca_core::BuiltinFunction::Floor | rumoca_core::BuiltinFunction::Integer => {
            Ok(value.floor())
        }
        rumoca_core::BuiltinFunction::Ceil => Ok(value.ceil()),
        _ => Err(LowerError::Unsupported {
            reason: format!(
                "builtin `{}` is unsupported in compile-time subscript expression",
                function.name()
            ),
        }),
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
            return Err(LowerError::Unsupported {
                reason: format!(
                    "assignment target `{}` has unsupported nested subscripts",
                    comp.to_var_name().as_str()
                ),
            });
        }
        indices = Some(assignment_subscript_indices(&part.subs, const_scope)?.ok_or_else(
            || LowerError::Unsupported {
                reason: format!(
                    "dynamic assignment target `{}` is unsupported in solve-IR function lowering",
                    comp.to_var_name().as_str()
                ),
            },
        )?);
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
) -> Result<Option<Vec<usize>>, LowerError> {
    if let Some(indices) = static_subscript_indices(subscripts)? {
        return Ok(Some(indices));
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let Ok(index) = compile_time_subscript_index(subscript, const_scope) else {
            return Ok(None);
        };
        indices.push(index);
    }
    Ok(Some(indices))
}

pub(super) fn eval_literal(literal: &rumoca_core::Literal) -> f64 {
    match literal {
        rumoca_core::Literal::Real(v) => *v,
        rumoca_core::Literal::Integer(v) => *v as f64,
        rumoca_core::Literal::Boolean(v) => {
            if *v {
                1.0
            } else {
                0.0
            }
        }
        rumoca_core::Literal::String(_) => 0.0,
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
    rumoca_core::BuiltinFunction::from_name(name).or_else(|| {
        rumoca_core::BuiltinFunction::from_name(rumoca_core::top_level_last_segment(name))
    })
}

pub(super) fn intrinsic_short_name(name: &str) -> &str {
    rumoca_core::top_level_last_segment(name)
}

pub(super) fn is_stream_passthrough_intrinsic(name: &str) -> bool {
    matches!(intrinsic_short_name(name), "actualStream" | "inStream")
}

pub(super) fn collect_scope_names(
    entry: &Scope,
    branches: &[Scope],
    else_scope: &Scope,
) -> Vec<ComponentPath> {
    let mut names = entry.keys();
    for scoped in branches.iter().chain(std::iter::once(else_scope)) {
        names.extend(
            scoped
                .keys()
                .into_iter()
                .filter(|name| !entry.contains_key(name)),
        );
    }
    names
}

pub(super) fn merge_branch_select(
    builder: &mut LowerBuilder<'_>,
    cond: Reg,
    branch_scope: &Scope,
    name: &ComponentPath,
    merged: Reg,
) -> Reg {
    match branch_scope.get(name).copied() {
        Some(branch_value) => builder.emit_select(cond, branch_value, merged),
        None => merged,
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
}
