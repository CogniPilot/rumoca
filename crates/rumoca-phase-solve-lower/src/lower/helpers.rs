use super::{IndexedBinding, LowerBuilder, LowerError, Scope};
use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_solve::Reg;
use rumoca_ir_solve::VarLayout;

pub(super) fn build_indexed_binding_map(
    layout: &VarLayout,
) -> IndexMap<String, Vec<IndexedBinding>> {
    let mut grouped: IndexMap<String, Vec<IndexedBinding>> = IndexMap::new();
    for (key, slot) in layout.bindings() {
        let Some((base, indices)) = parse_indexed_binding_key(key) else {
            continue;
        };
        grouped.entry(base).or_default().push(IndexedBinding {
            slot: *slot,
            indices,
        });
    }
    grouped
}

pub(super) fn indexed_entries_for_key(
    layout: &VarLayout,
    grouped: &IndexMap<String, Vec<IndexedBinding>>,
    key: &str,
) -> Vec<IndexedBinding> {
    if let Some(entries) = grouped.get(key) {
        return entries.clone();
    }

    let mut rebuilt = Vec::new();
    for (binding_key, slot) in layout.bindings() {
        let Some((base, indices)) = parse_indexed_binding_key(binding_key) else {
            continue;
        };
        if base == key {
            rebuilt.push(IndexedBinding {
                slot: *slot,
                indices,
            });
        }
    }
    rebuilt
}

pub(super) fn parse_indexed_binding_key(key: &str) -> Option<(String, Vec<usize>)> {
    let open = key.rfind('[')?;
    if !key.ends_with(']') || open >= key.len().saturating_sub(1) {
        return None;
    }
    let base = key[..open].to_string();
    if base.is_empty() {
        return None;
    }
    let contents = &key[open + 1..key.len() - 1];
    let mut indices = Vec::new();
    for raw in contents.split(',') {
        let parsed = raw.trim().parse::<usize>().ok()?;
        if parsed == 0 {
            return None;
        }
        indices.push(parsed);
    }
    if indices.is_empty() {
        return None;
    }
    Some((base, indices))
}

pub(super) fn is_record_constructor_signature(
    name: &dae::VarName,
    function: &dae::Function,
) -> bool {
    // MLS §12.6: record constructors are ordinary function calls. Compiled
    // lowering must therefore recognize constructor-shaped functions even when
    // the parser/front-end did not preserve an explicit constructor marker.
    if !function.locals.is_empty() || !function.body.is_empty() || function.external.is_some() {
        return false;
    }

    let function_leaf = name.as_str().rsplit('.').next().unwrap_or_default();
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

    let output_leaf = output.type_name.rsplit('.').next().unwrap_or_default();
    !output_leaf.is_empty() && output_leaf == function_leaf && output.name == "res"
}

pub(super) fn sorted_flat_entries(entries: &[IndexedBinding]) -> Vec<&IndexedBinding> {
    let mut flat = entries
        .iter()
        .filter(|entry| entry.indices.len() == 1)
        .collect::<Vec<_>>();
    flat.sort_by_key(|entry| entry.indices[0]);
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

pub(super) fn static_subscript_indices(
    subscripts: &[dae::Subscript],
) -> Result<Option<Vec<usize>>, LowerError> {
    if subscripts.is_empty() {
        return Ok(Some(Vec::new()));
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        match sub {
            dae::Subscript::Index(v) if *v > 0 => indices.push(*v as usize),
            dae::Subscript::Expr(expr) => match lower_static_index_expr(expr)? {
                Some(value) => indices.push(value),
                None => return Ok(None),
            },
            dae::Subscript::Colon => {
                return Err(LowerError::Unsupported {
                    reason: "slice subscript `:` is unsupported in PR2".to_string(),
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

pub(super) fn dynamic_binding_base_key(expr: &dae::Expression) -> Result<String, LowerError> {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            if subscripts.is_empty() {
                return Ok(name.as_str().to_string());
            }
            append_subscripts_to_key(name.as_str().to_string(), subscripts)
        }
        dae::Expression::Index { base, subscripts } => {
            let base_key = dynamic_binding_base_key(base)?;
            append_subscripts_to_key(base_key, subscripts)
        }
        dae::Expression::FieldAccess { base, field } => {
            let base_key = dynamic_binding_base_key(base)?;
            Ok(format!("{base_key}.{field}"))
        }
        _ => Err(LowerError::Unsupported {
            reason: format!(
                "unsupported base expression for dynamic binding path: {}",
                expr_tag(expr)
            ),
        }),
    }
}

pub(super) fn lower_subscript_index(subscript: &dae::Subscript) -> Result<usize, LowerError> {
    match subscript {
        dae::Subscript::Index(v) if *v > 0 => Ok(*v as usize),
        dae::Subscript::Expr(expr) => lower_index_expr(expr),
        dae::Subscript::Colon => Err(LowerError::Unsupported {
            reason: "slice subscript `:` is unsupported in PR2".to_string(),
        }),
        _ => Err(LowerError::Unsupported {
            reason: "non-positive subscript is unsupported".to_string(),
        }),
    }
}

pub(super) fn indexed_binding_key(
    base: &dae::Expression,
    subscripts: &[dae::Subscript],
) -> Result<String, LowerError> {
    let base_key = binding_base_key(base)?;
    append_subscripts_to_key(base_key, subscripts)
}

pub(super) fn field_access_binding_key(
    base: &dae::Expression,
    field: &str,
) -> Result<String, LowerError> {
    let base_key = binding_base_key(base)?;
    Ok(format!("{base_key}.{field}"))
}

pub(super) fn binding_base_key(expr: &dae::Expression) -> Result<String, LowerError> {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            if subscripts.is_empty() {
                Ok(name.as_str().to_string())
            } else {
                append_subscripts_to_key(name.as_str().to_string(), subscripts)
            }
        }
        dae::Expression::Index { base, subscripts } => indexed_binding_key(base, subscripts),
        dae::Expression::FieldAccess { base, field } => field_access_binding_key(base, field),
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
    subscripts: &[dae::Subscript],
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

pub(super) fn lower_index_expr(expr: &dae::Expression) -> Result<usize, LowerError> {
    match lower_static_index_expr(expr)? {
        Some(index) => Ok(index),
        None => Err(LowerError::Unsupported {
            reason: "dynamic subscript expressions are unsupported in PR2".to_string(),
        }),
    }
}

pub(super) fn lower_static_index_expr(expr: &dae::Expression) -> Result<Option<usize>, LowerError> {
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
    expr: &dae::Expression,
) -> Result<Option<f64>, LowerError> {
    match expr {
        dae::Expression::Literal(dae::Literal::Integer(v)) => Ok(Some(*v as f64)),
        dae::Expression::Literal(dae::Literal::Real(v)) => Ok(Some(*v)),
        dae::Expression::Unary {
            op:
                rumoca_ir_core::OpUnary::Plus(_)
                | rumoca_ir_core::OpUnary::DotPlus(_)
                | rumoca_ir_core::OpUnary::Empty,
            rhs,
        } => lower_static_index_numeric(rhs),
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_) | rumoca_ir_core::OpUnary::DotMinus(_),
            rhs,
        } => Ok(lower_static_index_numeric(rhs)?.map(|value| -value)),
        dae::Expression::Binary { op, lhs, rhs } => {
            let Some(l) = lower_static_index_numeric(lhs)? else {
                return Ok(None);
            };
            let Some(r) = lower_static_index_numeric(rhs)? else {
                return Ok(None);
            };
            let value = match op {
                rumoca_ir_core::OpBinary::Add(_) | rumoca_ir_core::OpBinary::AddElem(_) => l + r,
                rumoca_ir_core::OpBinary::Sub(_) | rumoca_ir_core::OpBinary::SubElem(_) => l - r,
                rumoca_ir_core::OpBinary::Mul(_) | rumoca_ir_core::OpBinary::MulElem(_) => l * r,
                rumoca_ir_core::OpBinary::Div(_) | rumoca_ir_core::OpBinary::DivElem(_) => l / r,
                rumoca_ir_core::OpBinary::Exp(_) | rumoca_ir_core::OpBinary::ExpElem(_) => {
                    l.powf(r)
                }
                _ => return Ok(None),
            };
            Ok(Some(value))
        }
        _ => Ok(None),
    }
}

pub(super) fn compile_time_var_key(
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
    const_scope: &IndexMap<String, f64>,
) -> Result<String, LowerError> {
    if subscripts.is_empty() {
        return Ok(name.as_str().to_string());
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        let index = compile_time_subscript_index(sub, const_scope)?;
        indices.push(index.to_string());
    }
    if indices.len() == 1 {
        Ok(format!("{}[{}]", name.as_str(), indices[0]))
    } else {
        Ok(format!("{}[{}]", name.as_str(), indices.join(",")))
    }
}

pub(super) fn compile_time_subscript_index(
    subscript: &dae::Subscript,
    const_scope: &IndexMap<String, f64>,
) -> Result<usize, LowerError> {
    match subscript {
        dae::Subscript::Index(value) if *value > 0 => Ok(*value as usize),
        dae::Subscript::Expr(expr) => compile_time_index_expr(expr, const_scope),
        dae::Subscript::Colon => Err(LowerError::Unsupported {
            reason: "slice subscript `:` is unsupported in compile-time context".to_string(),
        }),
        _ => Err(LowerError::Unsupported {
            reason: "non-positive subscript is unsupported in compile-time context".to_string(),
        }),
    }
}

pub(super) fn compile_time_index_expr(
    expr: &dae::Expression,
    const_scope: &IndexMap<String, f64>,
) -> Result<usize, LowerError> {
    let raw = match expr {
        dae::Expression::Literal(dae::Literal::Integer(v)) => *v as f64,
        dae::Expression::Literal(dae::Literal::Real(v)) => *v,
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => *const_scope
            .get(name.as_str())
            .ok_or_else(|| LowerError::Unsupported {
                reason: format!(
                    "subscript variable `{}` is not compile-time bound",
                    name.as_str()
                ),
            })?,
        dae::Expression::Unary {
            op:
                rumoca_ir_core::OpUnary::Plus(_)
                | rumoca_ir_core::OpUnary::DotPlus(_)
                | rumoca_ir_core::OpUnary::Empty,
            rhs,
        } => compile_time_index_expr(rhs, const_scope)? as f64,
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_) | rumoca_ir_core::OpUnary::DotMinus(_),
            rhs,
        } => -(compile_time_index_expr(rhs, const_scope)? as f64),
        _ => {
            return Err(LowerError::Unsupported {
                reason: "dynamic subscript expressions are unsupported in compile-time context"
                    .to_string(),
            });
        }
    };

    let rounded = raw.round();
    if rounded.is_finite() && rounded > 0.0 && (rounded - raw).abs() < f64::EPSILON {
        return Ok(rounded as usize);
    }

    Err(LowerError::Unsupported {
        reason: "subscript expression did not evaluate to a positive integer".to_string(),
    })
}

pub(super) fn assignment_target_name(comp: &dae::ComponentReference) -> Result<String, LowerError> {
    if comp.parts.is_empty() {
        return Err(LowerError::InvalidFunction {
            name: "<anonymous>".to_string(),
            reason: "assignment target has no path parts".to_string(),
        });
    }
    if comp.parts.iter().any(|part| !part.subs.is_empty()) {
        return Err(LowerError::Unsupported {
            reason: format!(
                "subscripted assignment target `{}` is unsupported in PR2",
                comp.to_var_name().as_str()
            ),
        });
    }
    Ok(comp.to_var_name().as_str().to_string())
}

pub(super) fn eval_literal(literal: &dae::Literal) -> f64 {
    match literal {
        dae::Literal::Real(v) => *v,
        dae::Literal::Integer(v) => *v as f64,
        dae::Literal::Boolean(v) => {
            if *v {
                1.0
            } else {
                0.0
            }
        }
        dae::Literal::String(_) => 0.0,
    }
}

pub(super) fn expr_tag(expr: &dae::Expression) -> &'static str {
    match expr {
        dae::Expression::Binary { .. } => "Binary",
        dae::Expression::Unary { .. } => "Unary",
        dae::Expression::VarRef { .. } => "VarRef",
        dae::Expression::BuiltinCall { .. } => "BuiltinCall",
        dae::Expression::FunctionCall { .. } => "FunctionCall",
        dae::Expression::Literal(_) => "Literal",
        dae::Expression::If { .. } => "If",
        dae::Expression::Array { .. } => "Array",
        dae::Expression::Tuple { .. } => "Tuple",
        dae::Expression::Range { .. } => "Range",
        dae::Expression::ArrayComprehension { .. } => "ArrayComprehension",
        dae::Expression::Index { .. } => "Index",
        dae::Expression::FieldAccess { .. } => "FieldAccess",
        dae::Expression::Empty => "Empty",
    }
}

pub(super) fn statement_tag(statement: &dae::Statement) -> &'static str {
    match statement {
        dae::Statement::Empty => "Empty",
        dae::Statement::Assignment { .. } => "Assignment",
        dae::Statement::Return => "Return",
        dae::Statement::Break => "Break",
        dae::Statement::For { .. } => "For",
        dae::Statement::While { .. } => "While",
        dae::Statement::If { .. } => "If",
        dae::Statement::When { .. } => "When",
        dae::Statement::FunctionCall { .. } => "FunctionCall",
        dae::Statement::Reinit { .. } => "Reinit",
        dae::Statement::Assert { .. } => "Assert",
    }
}

pub(super) fn unsupported_conditional_return() -> LowerError {
    LowerError::Unsupported {
        reason: "conditional return in function if-statement is unsupported in PR6".to_string(),
    }
}

pub(super) fn resolve_intrinsic_builtin(name: &str) -> Option<dae::BuiltinFunction> {
    dae::BuiltinFunction::from_name(name).or_else(|| {
        name.rsplit('.')
            .next()
            .and_then(dae::BuiltinFunction::from_name)
    })
}

pub(super) fn intrinsic_short_name(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}

pub(super) fn collect_scope_names(
    entry: &Scope,
    branches: &[Scope],
    else_scope: &Scope,
) -> Vec<String> {
    let mut names: Vec<String> = entry.keys().cloned().collect();
    for scoped in branches.iter().chain(std::iter::once(else_scope)) {
        names.extend(
            scoped
                .keys()
                .filter(|name| !entry.contains_key(*name))
                .cloned(),
        );
    }
    names
}

pub(super) fn merge_branch_select(
    builder: &mut LowerBuilder<'_>,
    cond: Reg,
    branch_scope: &Scope,
    name: &str,
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
    args: &[dae::Expression],
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
