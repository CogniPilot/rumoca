use std::collections::{HashMap, HashSet, VecDeque};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::lower::normalized_discrete_update_equations;
use crate::{LowerError, literal_positive_indices};

pub(crate) fn runtime_assignment_equations(
    dae_model: &dae::Dae,
    runtime_tail_updates: &HashSet<String>,
) -> Vec<dae::Equation> {
    let state_derivative_rows = crate::lower::state_derivative_equation_flags(dae_model);
    let equations = dae_model
        .continuous
        .equations
        .iter()
        .enumerate()
        .filter(|(row_idx, _)| {
            !state_derivative_rows
                .get(*row_idx)
                .copied()
                .unwrap_or(false)
        })
        .map(|(_, eq)| eq)
        .filter_map(|eq| runtime_assignment_equation(dae_model, runtime_tail_updates, eq))
        .collect();
    orient_runtime_tail_aliases(dae_model, equations)
}

pub(crate) fn runtime_assignment_equation(
    dae_model: &dae::Dae,
    runtime_tail_updates: &HashSet<String>,
    eq: &dae::Equation,
) -> Option<dae::Equation> {
    let (target, rhs) = if let Some(lhs) = eq.lhs.as_ref() {
        explicit_runtime_assignment(
            dae_model,
            runtime_tail_updates,
            lhs,
            &eq.rhs,
            eq.scalar_count.max(1),
        )?
    } else {
        residual_runtime_assignment(
            dae_model,
            runtime_tail_updates,
            &eq.rhs,
            eq.scalar_count.max(1),
        )?
    };
    Some(dae::Equation {
        lhs: Some(target),
        rhs,
        span: eq.span,
        origin: eq.origin.clone(),
        scalar_count: eq.scalar_count.max(1),
    })
}

pub(crate) fn static_runtime_tail_equation(
    dae_model: &dae::Dae,
    runtime_tail_updates: &HashSet<String>,
    eq: &dae::Equation,
) -> bool {
    if let Some(lhs) = eq.lhs.as_ref() {
        return is_static_runtime_tail_name(dae_model, runtime_tail_updates, lhs.as_str());
    }
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs: _,
        ..
    } = &eq.rhs
    else {
        return false;
    };
    target_expr_name(lhs).is_some_and(|name| {
        is_static_runtime_tail_name(dae_model, runtime_tail_updates, name.as_str())
    })
}

fn explicit_runtime_assignment(
    dae_model: &dae::Dae,
    runtime_tail_updates: &HashSet<String>,
    lhs: &rumoca_core::VarName,
    rhs: &rumoca_core::Expression,
    scalar_count: usize,
) -> Option<(rumoca_core::VarName, rumoca_core::Expression)> {
    if is_writable_runtime_tail_name(dae_model, lhs.as_str())
        && !runtime_tail_has_event_update(runtime_tail_updates, lhs.as_str())
        && !is_static_runtime_tail_name(dae_model, runtime_tail_updates, lhs.as_str())
        && runtime_assignment_shape_compatible(dae_model, lhs.as_str(), rhs, scalar_count)
    {
        return Some((array_target_name(lhs.clone(), scalar_count), rhs.clone()));
    }
    let rhs_target = target_expr_name(rhs)?;
    if is_writable_runtime_tail_name(dae_model, rhs_target.as_str())
        && !runtime_tail_has_event_update(runtime_tail_updates, rhs_target.as_str())
        && !is_static_runtime_tail_name(dae_model, runtime_tail_updates, rhs_target.as_str())
        && runtime_assignment_name_shape_compatible(
            dae_model,
            rhs_target.as_str(),
            lhs.as_str(),
            scalar_count,
        )
    {
        return Some((
            rhs_target,
            rumoca_core::Expression::VarRef {
                name: lhs.clone().into(),
                subscripts: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            },
        ));
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
        )
    {
        return Some((
            rumoca_core::VarName::new(rhs_base),
            rumoca_core::Expression::VarRef {
                name: lhs.clone().into(),
                subscripts: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            },
        ));
    }
    None
}

fn residual_runtime_assignment(
    dae_model: &dae::Dae,
    runtime_tail_updates: &HashSet<String>,
    expr: &rumoca_core::Expression,
    scalar_count: usize,
) -> Option<(rumoca_core::VarName, rumoca_core::Expression)> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        span: _,
    } = expr
    else {
        return None;
    };
    let lhs_target = target_expr_name(lhs);
    let rhs_target = target_expr_name(rhs);
    match (lhs_target, rhs_target) {
        (Some(target), _)
            if is_writable_runtime_tail_name(dae_model, target.as_str())
                && !runtime_tail_has_event_update(runtime_tail_updates, target.as_str())
                && !is_static_runtime_tail_name(
                    dae_model,
                    runtime_tail_updates,
                    target.as_str(),
                )
                && runtime_assignment_shape_compatible(
                    dae_model,
                    target.as_str(),
                    rhs.as_ref(),
                    scalar_count,
                ) =>
        {
            Some((
                array_target_name(target, scalar_count),
                rhs.as_ref().clone(),
            ))
        }
        (_, Some(target))
            if is_writable_runtime_tail_name(dae_model, target.as_str())
                && !runtime_tail_has_event_update(runtime_tail_updates, target.as_str())
                && !is_static_runtime_tail_name(
                    dae_model,
                    runtime_tail_updates,
                    target.as_str(),
                )
                && runtime_assignment_shape_compatible(
                    dae_model,
                    target.as_str(),
                    lhs.as_ref(),
                    scalar_count,
                ) =>
        {
            Some((
                array_target_name(target, scalar_count),
                lhs.as_ref().clone(),
            ))
        }
        _ => None,
    }
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
) -> bool {
    let target_dims = runtime_assignment_target_dims(dae_model, target);
    let source_dims = runtime_assignment_expr_dims(dae_model, source);
    runtime_assignment_dims_compatible(
        target_dims.as_deref(),
        source_dims.as_deref(),
        equation_scalar_count,
    )
}

fn runtime_assignment_name_shape_compatible(
    dae_model: &dae::Dae,
    target: &str,
    source: &str,
    equation_scalar_count: usize,
) -> bool {
    let target_dims = runtime_assignment_target_dims(dae_model, target);
    let source_dims = runtime_assignment_target_dims(dae_model, source);
    runtime_assignment_dims_compatible(
        target_dims.as_deref(),
        source_dims.as_deref(),
        equation_scalar_count,
    )
}

fn runtime_assignment_dims_compatible(
    target_dims: Option<&[i64]>,
    source_dims: Option<&[i64]>,
    equation_scalar_count: usize,
) -> bool {
    match (target_dims, source_dims) {
        (Some(target_dims), Some(source_dims)) => {
            dims_assignable_to_target(target_dims, source_dims)
        }
        (Some(target_dims), None) => {
            let target_size = positive_shape_size(target_dims);
            equation_scalar_count <= 1 || target_size == equation_scalar_count
        }
        (None, Some(source_dims)) => {
            equation_scalar_count <= 1 || positive_shape_size(source_dims) <= 1
        }
        (None, None) => true,
    }
}

fn dims_assignable_to_target(target_dims: &[i64], source_dims: &[i64]) -> bool {
    if target_dims == source_dims || source_dims.is_empty() {
        return true;
    }
    if target_dims.len() == source_dims.len() + 1 {
        return target_dims.get(1..).is_some_and(|tail| tail == source_dims);
    }
    positive_shape_size(target_dims) == positive_shape_size(source_dims)
}

fn positive_shape_size(dims: &[i64]) -> usize {
    dims.iter()
        .filter_map(|dim| usize::try_from(*dim).ok())
        .filter(|dim| *dim > 0)
        .product::<usize>()
        .max(1)
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
) -> Option<Vec<i64>> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => runtime_assignment_target_dims(dae_model, name.as_str()),
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            ..
        } => runtime_assignment_array_dims(elements, *is_matrix),
        _ => None,
    }
}

fn runtime_assignment_array_dims(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
) -> Option<Vec<i64>> {
    if !is_matrix {
        return Some(vec![elements.len() as i64]);
    }
    let rumoca_core::Expression::Array {
        elements: first_row,
        ..
    } = elements.first()?
    else {
        return None;
    };
    Some(vec![elements.len() as i64, first_row.len() as i64])
}

fn orient_runtime_tail_aliases(
    dae_model: &dae::Dae,
    equations: Vec<dae::Equation>,
) -> Vec<dae::Equation> {
    let source_distances = runtime_tail_source_distances(dae_model, &equations);
    equations
        .into_iter()
        .map(|eq| orient_runtime_tail_alias(dae_model, &source_distances, eq))
        .collect()
}

fn runtime_tail_source_distances(
    dae_model: &dae::Dae,
    equations: &[dae::Equation],
) -> HashMap<String, usize> {
    let mut graph: HashMap<String, Vec<String>> = HashMap::new();
    let mut queue = VecDeque::new();
    let mut distances = HashMap::new();
    for eq in equations {
        let Some(lhs) = eq.lhs.as_ref() else { continue };
        let lhs_name = lhs.as_str().to_string();
        let rhs_tail = target_expr_name(&eq.rhs)
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
    distances
}

fn orient_runtime_tail_alias(
    dae_model: &dae::Dae,
    source_distances: &HashMap<String, usize>,
    eq: dae::Equation,
) -> dae::Equation {
    let Some(lhs) = eq.lhs.as_ref() else {
        return eq;
    };
    let Some(rhs_name) =
        target_expr_name(&eq.rhs).filter(|name| is_runtime_tail_name(dae_model, name.as_str()))
    else {
        return eq;
    };
    let lhs_distance = source_distances
        .get(lhs.as_str())
        .copied()
        .unwrap_or(usize::MAX);
    let rhs_distance = source_distances
        .get(rhs_name.as_str())
        .copied()
        .unwrap_or(usize::MAX);
    if rhs_distance <= lhs_distance {
        return eq;
    }

    // MLS §8.3: connection-generated alias equations are symmetric. Runtime
    // tail solve-IR rows orient aliases away from variables with non-alias
    // equations so a computed source is not overwritten by stale connector
    // aliases during fixed-point iteration.
    dae::Equation {
        lhs: Some(rhs_name),
        rhs: rumoca_core::Expression::VarRef {
            name: lhs.clone().into(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        },
        span: eq.span,
        origin: eq.origin,
        scalar_count: eq.scalar_count,
    }
}

pub(crate) fn runtime_tail_update_names(dae_model: &dae::Dae) -> HashSet<String> {
    let mut names = HashSet::new();
    for eq in normalized_discrete_update_equations(dae_model) {
        let Some(lhs) = eq.lhs.as_ref() else {
            continue;
        };
        names.insert(lhs.as_str().to_string());
        let scalar_count = eq.scalar_count.max(1);
        for flat_index in 0..scalar_count {
            names.insert(runtime_assignment_scalar_name(
                dae_model,
                lhs,
                flat_index,
                scalar_count,
            ));
        }
    }
    names
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

fn target_expr_name(expr: &rumoca_core::Expression) -> Option<rumoca_core::VarName> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                return Some(name.var_name().clone());
            }
            let indices = literal_positive_indices(subscripts)?;
            Some(rumoca_core::VarName::new(dae::format_subscript_key(
                name.as_str(),
                &indices,
            )))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let rumoca_core::Expression::VarRef {
                name,
                subscripts: base_subscripts,
                span: rumoca_core::Span::DUMMY,
            } = base.as_ref()
            else {
                return None;
            };
            if !base_subscripts.is_empty() {
                return None;
            }
            let indices = literal_positive_indices(subscripts)?;
            Some(rumoca_core::VarName::new(dae::format_subscript_key(
                name.as_str(),
                &indices,
            )))
        }
        _ => None,
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
            let name = runtime_assignment_scalar_name(dae_model, lhs, flat_index, scalar_count);
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
) -> String {
    if scalar_count <= 1 {
        lhs.as_str().to_string()
    } else {
        let dims = runtime_assignment_dims(dae_model, lhs).unwrap_or(&[]);
        dae::scalar_name_text_for_flat_index(lhs.as_str(), dims, flat_index)
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
