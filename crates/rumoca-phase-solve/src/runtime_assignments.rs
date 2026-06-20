use std::collections::{HashSet, VecDeque};

use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::lower::normalized_discrete_update_equations;
use crate::{LowerError, checked_literal_positive_indices};

pub(crate) fn runtime_assignment_equations(
    dae_model: &dae::Dae,
    runtime_tail_updates: &HashSet<String>,
) -> Result<Vec<dae::Equation>, LowerError> {
    let state_derivative_rows = crate::lower::state_derivative_equation_flags(dae_model)?;
    let mut equations = Vec::new();
    for (row_idx, eq) in dae_model.continuous.equations.iter().enumerate() {
        let Some(&is_state_derivative_row) = state_derivative_rows.get(row_idx) else {
            return Err(runtime_assignment_contract_violation(
                format!("missing state-derivative flag for runtime-assignment equation {row_idx}"),
                eq.span,
            ));
        };
        if !is_state_derivative_row
            && let Some(equation) =
                runtime_assignment_equation(dae_model, runtime_tail_updates, eq)?
        {
            equations.push(equation);
        }
    }
    orient_runtime_tail_aliases(dae_model, equations)
}

fn runtime_assignment_contract_violation(reason: String, span: rumoca_core::Span) -> LowerError {
    if span.is_dummy() {
        LowerError::UnspannedContractViolation { reason }
    } else {
        LowerError::ContractViolation { reason, span }
    }
}

pub(crate) fn runtime_assignment_equation(
    dae_model: &dae::Dae,
    runtime_tail_updates: &HashSet<String>,
    eq: &dae::Equation,
) -> Result<Option<dae::Equation>, LowerError> {
    let (target, rhs) = if let Some(lhs) = eq.lhs.as_ref() {
        let Some(assignment) = explicit_runtime_assignment(
            dae_model,
            runtime_tail_updates,
            lhs,
            &eq.rhs,
            eq.scalar_count.max(1),
            eq.span,
        )?
        else {
            return Ok(None);
        };
        assignment
    } else {
        let Some(assignment) = residual_runtime_assignment(
            dae_model,
            runtime_tail_updates,
            &eq.rhs,
            eq.scalar_count.max(1),
            eq.span,
        )?
        else {
            return Ok(None);
        };
        assignment
    };
    Ok(Some(dae::Equation {
        lhs: Some(target.into()),
        rhs,
        span: eq.span,
        origin: eq.origin.clone(),
        scalar_count: eq.scalar_count.max(1),
    }))
}

pub(crate) fn static_runtime_tail_equation(
    dae_model: &dae::Dae,
    runtime_tail_updates: &HashSet<String>,
    eq: &dae::Equation,
) -> Result<bool, LowerError> {
    if let Some(lhs) = eq.lhs.as_ref() {
        return Ok(is_static_runtime_tail_name(
            dae_model,
            runtime_tail_updates,
            lhs.as_str(),
        ));
    }
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs: _,
        ..
    } = &eq.rhs
    else {
        return Ok(false);
    };
    Ok(target_expr_name(lhs)?.is_some_and(|name| {
        is_static_runtime_tail_name(dae_model, runtime_tail_updates, name.as_str())
    }))
}

fn explicit_runtime_assignment(
    dae_model: &dae::Dae,
    runtime_tail_updates: &HashSet<String>,
    lhs: &rumoca_core::Reference,
    rhs: &rumoca_core::Expression,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<Option<(rumoca_core::VarName, rumoca_core::Expression)>, LowerError> {
    if is_writable_runtime_tail_name(dae_model, lhs.as_str())
        && !runtime_tail_has_event_update(runtime_tail_updates, lhs.as_str())
        && !is_static_runtime_tail_name(dae_model, runtime_tail_updates, lhs.as_str())
        && runtime_assignment_shape_compatible(dae_model, lhs.as_str(), rhs, scalar_count, span)?
    {
        return Ok(Some((
            array_target_name(lhs.var_name().clone(), scalar_count),
            rhs.clone(),
        )));
    }
    let Some(rhs_target) = target_expr_name(rhs)? else {
        return Ok(None);
    };
    if is_writable_runtime_tail_name(dae_model, rhs_target.as_str())
        && !runtime_tail_has_event_update(runtime_tail_updates, rhs_target.as_str())
        && !is_static_runtime_tail_name(dae_model, runtime_tail_updates, rhs_target.as_str())
        && runtime_assignment_name_shape_compatible(
            dae_model,
            rhs_target.as_str(),
            lhs.as_str(),
            scalar_count,
            span,
        )?
    {
        return Ok(Some((
            rhs_target,
            rumoca_core::Expression::VarRef {
                name: lhs.clone(),
                subscripts: Vec::new(),
                span,
            },
        )));
    }
    if scalar_count > 1
        && let Some(rhs_base) = dae::component_base_name(rhs_target.as_str())
        && is_writable_runtime_tail_name(dae_model, &rhs_base)
        && !runtime_tail_has_event_update(runtime_tail_updates, &rhs_base)
        && !is_static_runtime_tail_name(dae_model, runtime_tail_updates, &rhs_base)
        && runtime_assignment_name_shape_compatible(
            dae_model,
            &rhs_base,
            lhs.as_str(),
            scalar_count,
            span,
        )?
    {
        return Ok(Some((
            rumoca_core::VarName::new(rhs_base),
            rumoca_core::Expression::VarRef {
                name: lhs.clone(),
                subscripts: Vec::new(),
                span,
            },
        )));
    }
    Ok(None)
}

fn residual_runtime_assignment(
    dae_model: &dae::Dae,
    runtime_tail_updates: &HashSet<String>,
    expr: &rumoca_core::Expression,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<Option<(rumoca_core::VarName, rumoca_core::Expression)>, LowerError> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        span: _,
    } = expr
    else {
        return Ok(None);
    };
    let lhs_target = target_expr_name(lhs)?;
    let rhs_target = target_expr_name(rhs)?;
    if let Some(target) = lhs_target
        && is_writable_runtime_tail_name(dae_model, target.as_str())
        && !runtime_tail_has_event_update(runtime_tail_updates, target.as_str())
        && !is_static_runtime_tail_name(dae_model, runtime_tail_updates, target.as_str())
        && runtime_assignment_shape_compatible(
            dae_model,
            target.as_str(),
            rhs.as_ref(),
            scalar_count,
            span,
        )?
    {
        return Ok(Some((
            array_target_name(target, scalar_count),
            rhs.as_ref().clone(),
        )));
    }
    if let Some(target) = rhs_target
        && is_writable_runtime_tail_name(dae_model, target.as_str())
        && !runtime_tail_has_event_update(runtime_tail_updates, target.as_str())
        && !is_static_runtime_tail_name(dae_model, runtime_tail_updates, target.as_str())
        && runtime_assignment_shape_compatible(
            dae_model,
            target.as_str(),
            lhs.as_ref(),
            scalar_count,
            span,
        )?
    {
        return Ok(Some((
            array_target_name(target, scalar_count),
            lhs.as_ref().clone(),
        )));
    }
    Ok(None)
}

fn is_static_runtime_tail_name(
    dae_model: &dae::Dae,
    runtime_tail_updates: &HashSet<String>,
    name: &str,
) -> bool {
    is_runtime_tail_name(dae_model, name)
        && !runtime_tail_has_event_update(runtime_tail_updates, name)
        && static_runtime_tail_binding(dae_model, name)
            .is_some_and(|expr| runtime_assignment_expr_is_static_runtime_tail(dae_model, expr))
}

fn static_runtime_tail_binding<'a>(
    dae_model: &'a dae::Dae,
    name: &str,
) -> Option<&'a rumoca_core::Expression> {
    dae_model.continuous.equations.iter().find_map(|eq| {
        let lhs = eq.lhs.as_ref()?;
        (lhs.as_str() == name).then_some(&eq.rhs)
    })
}

fn runtime_assignment_expr_is_static_runtime_tail(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> bool {
    match expr {
        rumoca_core::Expression::Literal { .. } => true,
        rumoca_core::Expression::VarRef { name, .. } => {
            runtime_assignment_var_dims(dae_model, name.as_str()).is_some()
                || dae_model.variables.constants.contains_key(name.var_name())
                || dae_model.variables.parameters.contains_key(name.var_name())
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            runtime_assignment_expr_is_static_runtime_tail(dae_model, rhs)
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            runtime_assignment_expr_is_static_runtime_tail(dae_model, lhs)
                && runtime_assignment_expr_is_static_runtime_tail(dae_model, rhs)
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => elements
            .iter()
            .all(|element| runtime_assignment_expr_is_static_runtime_tail(dae_model, element)),
        _ => false,
    }
}

fn runtime_assignment_shape_compatible(
    dae_model: &dae::Dae,
    target: &str,
    source: &rumoca_core::Expression,
    equation_scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    let target_dims = runtime_assignment_target_dims(dae_model, target);
    let source_dims = runtime_assignment_expr_dims(dae_model, source)?;
    runtime_assignment_dims_compatible(
        target_dims.as_deref(),
        source_dims.as_deref(),
        equation_scalar_count,
        span,
    )
}

fn runtime_assignment_name_shape_compatible(
    dae_model: &dae::Dae,
    target: &str,
    source: &str,
    equation_scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    let target_dims = runtime_assignment_target_dims(dae_model, target);
    let source_dims = runtime_assignment_target_dims(dae_model, source);
    runtime_assignment_dims_compatible(
        target_dims.as_deref(),
        source_dims.as_deref(),
        equation_scalar_count,
        span,
    )
}

fn runtime_assignment_dims_compatible(
    target_dims: Option<&[i64]>,
    source_dims: Option<&[i64]>,
    equation_scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    match (target_dims, source_dims) {
        (Some(target_dims), Some(source_dims)) => {
            dims_assignable_to_target(target_dims, source_dims, span)
        }
        (Some(target_dims), None) => {
            let target_size = concrete_shape_size(target_dims, span)?;
            Ok(equation_scalar_count <= 1 || target_size == equation_scalar_count)
        }
        (None, Some(source_dims)) => {
            let source_size = concrete_shape_size(source_dims, span)?;
            Ok(equation_scalar_count <= 1 || source_size <= 1)
        }
        (None, None) => Ok(true),
    }
}

fn dims_assignable_to_target(
    target_dims: &[i64],
    source_dims: &[i64],
    span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    if target_dims == source_dims || source_dims.is_empty() {
        return Ok(true);
    }
    if target_dims.len() == source_dims.len() + 1 {
        return Ok(target_dims.get(1..).is_some_and(|tail| tail == source_dims));
    }
    Ok(concrete_shape_size(target_dims, span)? == concrete_shape_size(source_dims, span)?)
}

fn concrete_shape_size(dims: &[i64], span: rumoca_core::Span) -> Result<usize, LowerError> {
    if dims.is_empty() {
        return Ok(1);
    }
    let mut total = 1usize;
    for dim in dims {
        let dim = usize::try_from(*dim).map_err(|_| {
            runtime_assignment_contract_violation(
                format!("runtime assignment dimension {dim} is invalid"),
                span,
            )
        })?;
        total = total.checked_mul(dim).ok_or_else(|| {
            runtime_assignment_contract_violation(
                "runtime assignment shape scalar count overflows".to_string(),
                span,
            )
        })?;
    }
    Ok(total)
}

fn runtime_assignment_target_dims(dae_model: &dae::Dae, target: &str) -> Option<Vec<i64>> {
    runtime_assignment_var_dims(dae_model, target).or_else(|| {
        let base = dae::component_base_name(target)?;
        runtime_assignment_var_dims(dae_model, &base)
    })
}

fn runtime_assignment_var_dims(dae_model: &dae::Dae, name: &str) -> Option<Vec<i64>> {
    let key = rumoca_core::VarName::new(name);
    dae_model
        .variables
        .inputs
        .get(&key)
        .or_else(|| dae_model.variables.discrete_reals.get(&key))
        .or_else(|| dae_model.variables.discrete_valued.get(&key))
        .or_else(|| dae_model.variables.states.get(&key))
        .or_else(|| dae_model.variables.algebraics.get(&key))
        .or_else(|| dae_model.variables.outputs.get(&key))
        .map(|var| var.dims.clone())
}

fn runtime_assignment_expr_dims(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> Result<Option<Vec<i64>>, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Ok(runtime_assignment_target_dims(dae_model, name.as_str())),
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            span,
        } => runtime_assignment_array_dims(elements, *is_matrix, *span),
        _ => Ok(None),
    }
}

fn runtime_assignment_array_dims(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
    span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    if !is_matrix {
        return Ok(Some(vec![runtime_assignment_len_dim(
            elements.len(),
            "runtime assignment array length",
            span,
        )?]));
    }
    let Some(first) = elements.first() else {
        return Ok(None);
    };
    let rumoca_core::Expression::Array {
        elements: first_row,
        span: row_span,
        ..
    } = first
    else {
        return Ok(None);
    };
    Ok(Some(vec![
        runtime_assignment_len_dim(elements.len(), "runtime assignment matrix row count", span)?,
        runtime_assignment_len_dim(
            first_row.len(),
            "runtime assignment matrix column count",
            *row_span,
        )?,
    ]))
}

fn runtime_assignment_len_dim(
    len: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<i64, LowerError> {
    i64::try_from(len).map_err(|_| {
        runtime_assignment_contract_violation(
            format!("{context} {len} exceeds Modelica dimension range"),
            span,
        )
    })
}

fn orient_runtime_tail_aliases(
    dae_model: &dae::Dae,
    equations: Vec<dae::Equation>,
) -> Result<Vec<dae::Equation>, LowerError> {
    if equations.is_empty() {
        return Ok(Vec::new());
    }
    let source_distances = runtime_tail_source_distances(dae_model, &equations)?;
    let span = equations
        .iter()
        .find_map(|equation| (!equation.span.is_dummy()).then_some(equation.span))
        .unwrap_or(equations[0].span);
    let mut oriented =
        crate::lower_vec_with_capacity(equations.len(), "runtime tail alias equation count", span)?;
    for eq in equations {
        oriented.push(orient_runtime_tail_alias(dae_model, &source_distances, eq)?);
    }
    Ok(oriented)
}

fn runtime_tail_source_distances(
    dae_model: &dae::Dae,
    equations: &[dae::Equation],
) -> Result<IndexMap<String, usize>, LowerError> {
    let mut graph: IndexMap<String, Vec<String>> = IndexMap::new();
    let mut queue = VecDeque::new();
    let mut distances = IndexMap::new();
    for eq in equations {
        let Some(lhs) = eq.lhs.as_ref() else { continue };
        let lhs_name = lhs.as_str().to_string();
        let rhs_tail = target_expr_name(&eq.rhs)?
            .filter(|name| is_runtime_tail_name(dae_model, name.as_str()))
            .map(|name| name.as_str().to_string());
        if let Some(rhs_name) = rhs_tail {
            graph
                .entry(lhs_name.clone())
                .or_default()
                .push(rhs_name.clone());
            graph.entry(rhs_name).or_default().push(lhs_name);
        } else if distances.insert(lhs_name.clone(), 0).is_none() {
            queue.push_back(lhs_name);
        }
    }

    while let Some(name) = queue.pop_front() {
        let next_distance = distances[&name] + 1;
        for neighbor in graph.get(&name).into_iter().flatten() {
            if distances
                .get(neighbor)
                .is_none_or(|current| next_distance < *current)
            {
                distances.insert(neighbor.clone(), next_distance);
                queue.push_back(neighbor.clone());
            }
        }
    }
    Ok(distances)
}

fn orient_runtime_tail_alias(
    dae_model: &dae::Dae,
    source_distances: &IndexMap<String, usize>,
    eq: dae::Equation,
) -> Result<dae::Equation, LowerError> {
    let Some(lhs) = eq.lhs.as_ref() else {
        return Ok(eq);
    };
    let Some(rhs_name) =
        target_expr_name(&eq.rhs)?.filter(|name| is_runtime_tail_name(dae_model, name.as_str()))
    else {
        return Ok(eq);
    };
    let Some(&lhs_distance) = source_distances.get(lhs.as_str()) else {
        return Ok(eq);
    };
    let Some(&rhs_distance) = source_distances.get(rhs_name.as_str()) else {
        return Ok(eq);
    };
    if rhs_distance <= lhs_distance {
        return Ok(eq);
    }

    // MLS §8.3: connection-generated alias equations are symmetric. Runtime
    // tail solve-IR rows orient aliases away from variables with non-alias
    // equations so a computed source is not overwritten by stale connector
    // aliases during fixed-point iteration.
    Ok(dae::Equation {
        lhs: Some(rhs_name.into()),
        rhs: rumoca_core::Expression::VarRef {
            name: lhs.clone(),
            subscripts: Vec::new(),
            span: eq.span,
        },
        span: eq.span,
        origin: eq.origin,
        scalar_count: eq.scalar_count,
    })
}

pub(crate) fn runtime_tail_update_names(
    dae_model: &dae::Dae,
) -> Result<HashSet<String>, LowerError> {
    let mut names = HashSet::new();
    for eq in normalized_discrete_update_equations(dae_model)? {
        let Some(lhs) = eq.lhs.as_ref() else {
            continue;
        };
        names.insert(lhs.as_str().to_string());
        let scalar_count = eq.scalar_count.max(1);
        for flat_index in 0..scalar_count {
            names.insert(runtime_assignment_scalar_name(
                dae_model,
                lhs.var_name(),
                flat_index,
                scalar_count,
                eq.span,
            )?);
        }
    }
    Ok(names)
}

fn runtime_tail_has_event_update(runtime_tail_updates: &HashSet<String>, name: &str) -> bool {
    runtime_tail_updates.contains(name)
        || dae::component_base_name(name).is_some_and(|base| runtime_tail_updates.contains(&base))
}

fn array_target_name(target: rumoca_core::VarName, scalar_count: usize) -> rumoca_core::VarName {
    if scalar_count <= 1 {
        return target;
    }
    dae::component_base_name(target.as_str()).map_or(target, rumoca_core::VarName::new)
}

fn target_expr_name(
    expr: &rumoca_core::Expression,
) -> Result<Option<rumoca_core::VarName>, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                return Ok(Some(name.var_name().clone()));
            }
            let Some(indices) = checked_literal_positive_indices(subscripts, expr.span())? else {
                return Ok(None);
            };
            Ok(Some(rumoca_core::VarName::new(dae::format_subscript_key(
                name.as_str(),
                &indices,
            ))))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let rumoca_core::Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
            else {
                return Ok(None);
            };
            if !base_subscripts.is_empty() {
                return Ok(None);
            }
            let Some(indices) =
                checked_literal_positive_indices(subscripts, expr.span().or_else(|| base.span()))?
            else {
                return Ok(None);
            };
            Ok(Some(rumoca_core::VarName::new(dae::format_subscript_key(
                name.as_str(),
                &indices,
            ))))
        }
        _ => Ok(None),
    }
}

fn is_runtime_tail_name(dae_model: &dae::Dae, name: &str) -> bool {
    let key = rumoca_core::VarName::new(name);
    dae_model.variables.inputs.contains_key(&key)
        || dae_model.variables.discrete_reals.contains_key(&key)
        || dae_model.variables.discrete_valued.contains_key(&key)
        || dae::component_base_name(name).is_some_and(|base| {
            let key = rumoca_core::VarName::new(base);
            dae_model.variables.inputs.contains_key(&key)
                || dae_model.variables.discrete_reals.contains_key(&key)
                || dae_model.variables.discrete_valued.contains_key(&key)
        })
}

fn is_writable_runtime_tail_name(dae_model: &dae::Dae, name: &str) -> bool {
    // MLS §4.4.4 and Appendix B: top-level input variables are externally
    // supplied known quantities during continuous solving, so solve-IR must
    // not generate runtime assignments that overwrite them. Discrete runtime
    // tails, by contrast, are internal event/observation state and may be
    // refreshed from solver-backed aliases.
    let key = rumoca_core::VarName::new(name);
    dae_model.variables.discrete_reals.contains_key(&key)
        || dae_model.variables.discrete_valued.contains_key(&key)
        || dae::component_base_name(name).is_some_and(|base| {
            let key = rumoca_core::VarName::new(base);
            dae_model.variables.discrete_reals.contains_key(&key)
                || dae_model.variables.discrete_valued.contains_key(&key)
        })
}

pub(crate) fn lower_runtime_assignment_targets(
    dae_model: &dae::Dae,
    equations: &[dae::Equation],
    layout: &solve::VarLayout,
) -> Result<Vec<solve::ScalarSlot>, LowerError> {
    let mut targets = Vec::new();
    for eq in equations {
        let Some(lhs) = eq.lhs.as_ref() else {
            continue;
        };
        let scalar_count = eq.scalar_count.max(1);
        for flat_index in 0..scalar_count {
            let name = runtime_assignment_scalar_name(
                dae_model,
                lhs.var_name(),
                flat_index,
                scalar_count,
                eq.span,
            )?;
            let Some(slot) = layout.binding(name.as_str()) else {
                return Err(LowerError::UnsupportedAt {
                    reason: format!("missing runtime assignment target binding `{name}`"),
                    contexts: Vec::new(),
                    span: eq.span,
                });
            };
            targets.push(slot);
        }
    }
    Ok(targets)
}

fn runtime_assignment_scalar_name(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    flat_index: usize,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<String, LowerError> {
    if scalar_count <= 1 {
        Ok(lhs.as_str().to_string())
    } else {
        let dims = runtime_assignment_dims(dae_model, lhs).ok_or_else(|| {
            runtime_assignment_contract_violation(
                format!(
                    "runtime assignment array LHS `{}` must be a known DAE variable",
                    lhs.as_str()
                ),
                span,
            )
        })?;
        Ok(dae::scalar_name_text_for_flat_index(
            lhs.as_str(),
            dims,
            flat_index,
        ))
    }
}

fn runtime_assignment_dims<'a>(
    dae_model: &'a dae::Dae,
    lhs: &rumoca_core::VarName,
) -> Option<&'a [i64]> {
    dae_model
        .variables
        .inputs
        .get(lhs)
        .or_else(|| dae_model.variables.discrete_reals.get(lhs))
        .or_else(|| dae_model.variables.discrete_valued.get(lhs))
        .or_else(|| dae_model.variables.states.get(lhs))
        .or_else(|| dae_model.variables.algebraics.get(lhs))
        .or_else(|| dae_model.variables.outputs.get(lhs))
        .map(|var| var.dims.as_slice())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dae_with_discrete_reals(names: &[&str]) -> dae::Dae {
        let mut dae_model = dae::Dae::new();
        for name in names {
            dae_model.variables.discrete_reals.insert(
                rumoca_core::VarName::new(*name),
                dae::Variable {
                    name: rumoca_core::VarName::new(*name),
                    ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ))
                },
            );
        }
        dae_model
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn real_literal(value: f64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn runtime_assignment_scalar_name_reports_missing_array_lhs_with_span() {
        let dae_model = dae::Dae::new();
        let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(7), 11, 19);

        let err = runtime_assignment_scalar_name(
            &dae_model,
            &rumoca_core::VarName::new("missingArray"),
            0,
            2,
            span,
        )
        .expect_err("array runtime assignment target must resolve to a DAE variable");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
        assert!(
            err.reason()
                .contains("runtime assignment array LHS `missingArray`")
        );
    }

    #[test]
    fn runtime_assignment_scalar_name_reports_missing_array_lhs_without_dummy_span() {
        let dae_model = dae::Dae::new();

        let err = runtime_assignment_scalar_name(
            &dae_model,
            &rumoca_core::VarName::new("missingArray"),
            0,
            2,
            rumoca_core::Span::DUMMY,
        )
        .expect_err("unspanned array runtime assignment target must fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason()
                .contains("runtime assignment array LHS `missingArray`")
        );
    }

    #[test]
    fn runtime_tail_aliases_orient_away_from_non_alias_source() -> Result<(), LowerError> {
        let dae_model = dae_with_discrete_reals(&["a", "b"]);
        let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(8), 4, 13);
        let equations = vec![
            dae::Equation::explicit(
                rumoca_core::Reference::new("b"),
                var_ref("a"),
                span,
                "runtime alias",
            ),
            dae::Equation::explicit(
                rumoca_core::Reference::new("b"),
                real_literal(1.0),
                span,
                "runtime source",
            ),
        ];

        let oriented = orient_runtime_tail_aliases(&dae_model, equations)?;

        assert_eq!(oriented[0].lhs.as_ref().map(|lhs| lhs.as_str()), Some("a"));
        let rhs_name = target_expr_name(&oriented[0].rhs)?;
        assert_eq!(rhs_name.as_ref().map(|name| name.as_str()), Some("b"));
        assert_eq!(oriented[0].rhs.span(), Some(span));
        Ok(())
    }

    #[test]
    fn target_expr_name_accepts_spanned_index_base_ref() -> Result<(), LowerError> {
        let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(9), 2, 10);
        let expr = rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("tail"),
                subscripts: Vec::new(),
                span,
            }),
            subscripts: vec![rumoca_core::Subscript::index(2, span)],
            span,
        };

        let name = target_expr_name(&expr)?;

        assert_eq!(name.as_ref().map(|name| name.as_str()), Some("tail[2]"));
        Ok(())
    }

    #[test]
    fn runtime_tail_aliases_leave_disconnected_aliases_unchanged() -> Result<(), LowerError> {
        let dae_model = dae_with_discrete_reals(&["a", "b"]);
        let equations = vec![dae::Equation::explicit(
            rumoca_core::Reference::new("a"),
            var_ref("b"),
            rumoca_core::Span::DUMMY,
            "runtime alias",
        )];

        let oriented = orient_runtime_tail_aliases(&dae_model, equations)?;

        assert_eq!(oriented[0].lhs.as_ref().map(|lhs| lhs.as_str()), Some("a"));
        let rhs_name = target_expr_name(&oriented[0].rhs)?;
        assert_eq!(rhs_name.as_ref().map(|name| name.as_str()), Some("b"));
        Ok(())
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn runtime_assignment_len_dim_rejects_modelica_dimension_overflow_with_span() {
        let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(19), 8, 16);

        let err = runtime_assignment_len_dim(usize::MAX, "runtime assignment array length", span)
            .expect_err("oversized runtime assignment dimension should fail");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
        assert!(err.reason().contains("runtime assignment array length"));
        assert!(err.reason().contains("exceeds Modelica dimension range"));
    }

    #[test]
    fn runtime_assignment_equation_reports_shape_size_overflow_with_span() {
        let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(17), 3, 14);
        let mut dae_model = dae::Dae::new();
        dae_model.variables.discrete_reals.insert(
            rumoca_core::VarName::new("tail"),
            dae::Variable {
                name: rumoca_core::VarName::new("tail"),
                dims: vec![i64::MAX, 3],
                source_span: span,
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );
        let eq = dae::Equation::explicit(
            rumoca_core::Reference::new("tail"),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span,
            },
            span,
            "runtime tail overflow",
        );

        let err = runtime_assignment_equation(&dae_model, &HashSet::new(), &eq)
            .expect_err("oversized runtime assignment target shape should fail");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
        assert!(
            err.reason()
                .contains("runtime assignment shape scalar count overflows")
        );
    }

    #[test]
    fn runtime_assignment_equation_reports_negative_dimension_with_span() {
        let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(18), 5, 21);
        let mut dae_model = dae::Dae::new();
        dae_model.variables.discrete_reals.insert(
            rumoca_core::VarName::new("tail"),
            dae::Variable {
                name: rumoca_core::VarName::new("tail"),
                dims: vec![-1],
                source_span: span,
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );
        let eq = dae::Equation::explicit(
            rumoca_core::Reference::new("tail"),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span,
            },
            span,
            "runtime tail negative dimension",
        );

        let err = runtime_assignment_equation(&dae_model, &HashSet::new(), &eq)
            .expect_err("negative runtime assignment target dimension should fail");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
        assert!(
            err.reason()
                .contains("runtime assignment dimension -1 is invalid")
        );
    }
}
