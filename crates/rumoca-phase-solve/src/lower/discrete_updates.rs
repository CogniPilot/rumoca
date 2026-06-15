use std::collections::VecDeque;

use super::helpers::{dims_scalar_count, static_subscript_indices};
use super::{LowerError, compile_time, expression_rows};
use indexmap::{IndexMap, IndexSet};
use rumoca_core::ExpressionRewriter;
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{LinearOp, VarLayout};

#[derive(Debug, Clone)]
struct DiscreteAliasEdge {
    lhs: DiscreteTarget,
    rhs: DiscreteTarget,
    source: dae::Equation,
}

#[derive(Debug, Clone)]
struct DiscreteTarget {
    name: rumoca_core::VarName,
    reference: rumoca_core::Reference,
}

pub(crate) fn normalized_discrete_update_equations(
    dae_model: &dae::Dae,
) -> Result<Vec<dae::Equation>, LowerError> {
    let protected_lhs_names = protected_discrete_update_lhs_names(dae_model)?;
    let mut occupied_lhs_names = IndexSet::new();
    let mut normalized_equations = Vec::new();
    let mut alias_edges = Vec::new();
    for eq in dae_model
        .discrete
        .real_updates
        .iter()
        .chain(dae_model.discrete.valued_updates.iter())
    {
        collect_normalized_discrete_update_equation(
            dae_model,
            &protected_lhs_names,
            &mut occupied_lhs_names,
            &mut normalized_equations,
            &mut alias_edges,
            eq,
            false,
        )?;
    }
    for eq in &dae_model.conditions.equations {
        collect_normalized_discrete_update_equation(
            dae_model,
            &protected_lhs_names,
            &mut occupied_lhs_names,
            &mut normalized_equations,
            &mut alias_edges,
            eq,
            true,
        )?;
    }
    normalized_equations.extend(oriented_discrete_alias_equations(
        &alias_edges,
        &protected_lhs_names,
    )?);
    Ok(normalized_equations)
}

pub(crate) fn initial_condition_update_equations(dae_model: &dae::Dae) -> Vec<dae::Equation> {
    dae_model
        .conditions
        .equations
        .iter()
        .filter(|eq| {
            eq.lhs
                .as_ref()
                .is_some_and(|lhs| is_condition_memory_lhs(dae_model, lhs.var_name()))
        })
        .cloned()
        .collect()
}

fn is_condition_memory_lhs(dae_model: &dae::Dae, lhs: &rumoca_core::VarName) -> bool {
    crate::condition_memory_base_name(dae_model).is_some_and(|condition_name| {
        dae::component_base_name(lhs.as_str()).as_deref() == Some(condition_name.as_str())
    })
}

pub(crate) fn lower_initial_update_rhs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let equations = initial_condition_update_equations(dae_model);
    let structural_bindings = compile_time::structural_bindings(dae_model);
    Ok(rumoca_eval_solve::to_scalar_program_block(
        &expression_rows::lower_expression_rows_with_mode(
            equations.iter(),
            layout,
            &dae_model.symbols.functions,
            expression_rows::RuntimeRowMetadata {
                clock_intervals: &dae_model.clocks.intervals,
                clock_timings: &dae_model.clocks.timings,
                triggered_clock_conditions: &dae_model.clocks.triggered_conditions,
                discrete_valued_names: &dae_model.variables.discrete_valued,
                variable_starts: &dae_model.metadata.variable_starts,
                dae_variables: Some(&dae_model.variables),
                structural_bindings: Some(std::sync::Arc::new(structural_bindings)),
                guard_target_start_before_first_clock_tick: false,
            },
            true,
        )?,
    )
    .programs)
}

fn collect_normalized_discrete_update_equation(
    dae_model: &dae::Dae,
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
    occupied_lhs_names: &mut IndexSet<rumoca_core::VarName>,
    normalized_equations: &mut Vec<dae::Equation>,
    alias_edges: &mut Vec<DiscreteAliasEdge>,
    eq: &dae::Equation,
    skip_untargeted_condition_row: bool,
) -> Result<(), LowerError> {
    let rewritten = rewrite_discrete_update_equation(dae_model, eq)?;
    if let Some(expanded) = expand_tuple_function_residual(dae_model, &rewritten)? {
        for expanded_eq in expanded {
            collect_normalized_discrete_update_equation(
                dae_model,
                protected_lhs_names,
                occupied_lhs_names,
                normalized_equations,
                alias_edges,
                &expanded_eq,
                skip_untargeted_condition_row,
            )?;
        }
        return Ok(());
    }

    if let Some(edges) = explicit_discrete_alias_edges(dae_model, &rewritten)? {
        alias_edges.extend(edges);
        return Ok(());
    }
    if let Some(edges) = residual_discrete_alias_edges(dae_model, &rewritten)? {
        alias_edges.extend(edges);
        return Ok(());
    }

    let normalized = normalize_discrete_update_equation(
        dae_model,
        &rewritten,
        protected_lhs_names,
        occupied_lhs_names,
    )?;
    if normalized.lhs.is_none() && skip_untargeted_condition_row {
        // MLS Appendix B: relation and clock root rows in f_c detect events,
        // while only condition-memory rows with assignment targets update
        // discrete state during event iteration.
        return Ok(());
    }
    if let Some(lhs) = normalized.lhs.as_ref() {
        occupied_lhs_names.extend(discrete_update_lhs_names(
            dae_model,
            lhs.var_name(),
            normalized.scalar_count.max(1),
        )?);
    }
    normalized_equations.push(normalized);
    Ok(())
}

fn expand_tuple_function_residual(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
) -> Result<Option<Vec<dae::Equation>>, LowerError> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        span,
    } = &eq.rhs
    else {
        return Ok(None);
    };
    let rumoca_core::Expression::Tuple { elements, .. } = lhs.as_ref() else {
        return Ok(None);
    };
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor,
        ..
    } = rhs.as_ref()
    else {
        return Ok(None);
    };
    let Some(function) = dae_model.symbols.functions.get(name.var_name()) else {
        return Ok(None);
    };
    if elements.len() != function.outputs.len() {
        return Ok(None);
    }

    let mut equations = Vec::new();
    for (target_expr, output) in elements.iter().zip(&function.outputs) {
        let Some(target_names) = tuple_assignment_target_names(dae_model, target_expr, output)?
        else {
            return Ok(None);
        };
        for (idx, target_name) in target_names.into_iter().enumerate() {
            let Some(output_name) = projected_function_output_name(name.var_name(), output, idx)
            else {
                return Ok(None);
            };
            let target_reference = reference_for_discrete_target(dae_model, &target_name, eq.span)?;
            equations.push(dae::Equation {
                lhs: Some(target_reference),
                rhs: rumoca_core::Expression::FunctionCall {
                    name: output_name.into(),
                    args: args.clone(),
                    is_constructor: *is_constructor,
                    span: *span,
                },
                span: eq.span,
                origin: eq.origin.clone(),
                scalar_count: 1,
            });
        }
    }
    Ok(Some(equations))
}

fn tuple_assignment_target_names(
    dae_model: &dae::Dae,
    target_expr: &rumoca_core::Expression,
    output: &rumoca_core::FunctionParam,
) -> Result<Option<Vec<rumoca_core::VarName>>, LowerError> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = target_expr
    else {
        return Ok(None);
    };
    if !subscripts.is_empty() {
        if dims_scalar_count(
            &output.dims,
            format!("function output `{}`", output.name),
            output.span,
        )? != 1
        {
            return Ok(None);
        }
        let Some(indices) = static_subscript_indices(subscripts).ok().flatten() else {
            return Ok(None);
        };
        return Ok(Some(vec![rumoca_core::VarName::new(
            dae::format_subscript_key(name.as_str(), &indices),
        )]));
    }
    let output_count = tuple_assignment_output_count(dae_model, name, output)?;
    if output_count == 1 {
        return Ok(Some(vec![name.var_name().clone()]));
    }
    Ok(Some(discrete_update_scalar_names(
        dae_model,
        name.var_name(),
        output_count,
    )?))
}

fn tuple_assignment_output_count(
    dae_model: &dae::Dae,
    target_name: &rumoca_core::Reference,
    output: &rumoca_core::FunctionParam,
) -> Result<usize, LowerError> {
    if output.dims.iter().any(|dim| *dim < 0)
        || (!output.shape_expr.is_empty() && output.dims.iter().any(|dim| *dim <= 0))
    {
        // MLS §12.4.3: for a tuple assignment, the left-hand target and the
        // corresponding output component determine the same value shape. DAE
        // uses 0 for unresolved shape-expression dimensions, so use the
        // concrete target declaration once lowering has scalarized the
        // assignment.
        let dims = discrete_update_dims(dae_model, target_name.var_name()).ok_or_else(|| {
            LowerError::ContractViolation {
                reason: format!(
                    "tuple assignment target `{}` must have DAE variable dimensions",
                    target_name.as_str()
                ),
                span: output.span,
            }
        })?;
        return dims_scalar_count(
            dims,
            format!("tuple assignment target `{}`", target_name.as_str()),
            output.span,
        );
    }
    dims_scalar_count(
        &output.dims,
        format!("function output `{}`", output.name),
        output.span,
    )
}

fn projected_function_output_name(
    function_name: &rumoca_core::VarName,
    output: &rumoca_core::FunctionParam,
    flat_index: usize,
) -> Option<rumoca_core::VarName> {
    let base = format!("{}.{}", function_name.as_str(), output.name);
    if output.dims.is_empty() {
        (flat_index == 0).then(|| rumoca_core::VarName::new(base))
    } else {
        Some(rumoca_core::VarName::new(dae::format_subscript_key(
            &base,
            &[flat_index + 1],
        )))
    }
}

fn rewrite_discrete_update_equation(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
) -> Result<dae::Equation, LowerError> {
    let mut rewritten = eq.clone();
    rewritten.rhs = rewrite_relation_event_memory_expr(dae_model, &rewritten.rhs)?;
    Ok(rewritten)
}

fn normalize_discrete_update_equation(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
    occupied_lhs_names: &IndexSet<rumoca_core::VarName>,
) -> Result<dae::Equation, LowerError> {
    let mut normalized = eq.clone();
    if let Some(lhs) = normalized.lhs.as_ref()
        && let Some(alias_target) =
            plain_discrete_target(dae_model, &normalized.rhs, normalized.span)?
        && should_reorient_discrete_alias(
            dae_model,
            lhs.var_name(),
            &alias_target.name,
            normalized.scalar_count.max(1),
            protected_lhs_names,
            occupied_lhs_names,
        )?
    {
        let old_lhs = lhs.clone();
        normalized.lhs = Some(alias_target.reference);
        normalized.rhs = rumoca_core::Expression::VarRef {
            name: old_lhs,
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };
        return Ok(normalized);
    }
    if normalized.lhs.is_none()
        && let Some((lhs, rhs)) = extract_residual_discrete_assignment(
            dae_model,
            &normalized.rhs,
            normalized.scalar_count.max(1),
            protected_lhs_names,
            occupied_lhs_names,
        )?
    {
        normalized.lhs = Some(lhs.reference);
        normalized.rhs = rhs;
    }
    Ok(normalized)
}

fn explicit_discrete_alias_edges(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
) -> Result<Option<Vec<DiscreteAliasEdge>>, LowerError> {
    let Some(lhs) = eq.lhs.as_ref() else {
        return Ok(None);
    };
    let Some(rhs) = plain_discrete_target(dae_model, &eq.rhs, eq.span)? else {
        return Ok(None);
    };
    let lhs = discrete_target_from_reference(dae_model, lhs, &[], eq.span)?;
    discrete_alias_edges(dae_model, &lhs, &rhs, eq.scalar_count.max(1), eq)
}

fn residual_discrete_alias_edges(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
) -> Result<Option<Vec<DiscreteAliasEdge>>, LowerError> {
    if eq.lhs.is_some() {
        return Ok(None);
    }
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &eq.rhs
    else {
        return Ok(None);
    };
    let Some(lhs) = plain_discrete_target(dae_model, lhs, eq.span)? else {
        return Ok(None);
    };
    let Some(rhs) = plain_discrete_target(dae_model, rhs, eq.span)? else {
        return Ok(None);
    };
    discrete_alias_edges(dae_model, &lhs, &rhs, eq.scalar_count.max(1), eq)
}

fn discrete_alias_edges(
    dae_model: &dae::Dae,
    lhs: &DiscreteTarget,
    rhs: &DiscreteTarget,
    scalar_count: usize,
    source: &dae::Equation,
) -> Result<Option<Vec<DiscreteAliasEdge>>, LowerError> {
    let lhs_targets = discrete_update_scalar_targets(dae_model, lhs, scalar_count, source.span)?;
    let rhs_targets = discrete_update_scalar_targets(dae_model, rhs, scalar_count, source.span)?;
    if lhs_targets.len() != rhs_targets.len() {
        return Ok(None);
    }
    Ok(Some(
        lhs_targets
            .into_iter()
            .zip(rhs_targets)
            .filter(|(lhs, rhs)| lhs.name != rhs.name)
            .map(|(lhs, rhs)| DiscreteAliasEdge {
                lhs,
                rhs,
                source: source.clone(),
            })
            .collect(),
    ))
}

fn oriented_discrete_alias_equations(
    edges: &[DiscreteAliasEdge],
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
) -> Result<Vec<dae::Equation>, LowerError> {
    let mut adjacency: IndexMap<rumoca_core::VarName, Vec<(rumoca_core::VarName, usize)>> =
        IndexMap::new();
    let mut target_by_name: IndexMap<rumoca_core::VarName, DiscreteTarget> = IndexMap::new();
    let mut ordered_names = Vec::new();
    let mut seen_names = IndexSet::new();
    for (idx, edge) in edges.iter().enumerate() {
        for target in [&edge.lhs, &edge.rhs] {
            let name = &target.name;
            target_by_name
                .entry(name.clone())
                .or_insert_with(|| target.clone());
            if seen_names.insert(name.clone()) {
                ordered_names.push(name.clone());
            }
        }
        adjacency
            .entry(edge.lhs.name.clone())
            .or_default()
            .push((edge.rhs.name.clone(), idx));
        adjacency
            .entry(edge.rhs.name.clone())
            .or_default()
            .push((edge.lhs.name.clone(), idx));
    }

    let mut visited = IndexSet::new();
    let mut directed = Vec::new();
    for root_candidate in ordered_names {
        if visited.contains(&root_candidate) {
            continue;
        }
        let component = collect_alias_component(&root_candidate, &adjacency);
        let roots = alias_component_roots(&component, protected_lhs_names);
        for root in &roots {
            visited.insert(root.clone());
        }

        let mut queue = VecDeque::from(roots);
        while let Some(parent) = queue.pop_front() {
            visit_alias_neighbors(&parent, &adjacency, &mut visited, &mut queue, &mut directed);
        }
    }

    directed.sort_by_key(|(edge_idx, _, _)| *edge_idx);
    directed
        .into_iter()
        .map(|(edge_idx, parent, child)| {
            let edge = &edges[edge_idx];
            let parent = target_by_name.get(&parent).cloned().ok_or_else(|| {
                LowerError::ContractViolation {
                    reason: format!("discrete alias parent `{parent}` lost target metadata"),
                    span: edge.source.span,
                }
            })?;
            let child = target_by_name.get(&child).cloned().ok_or_else(|| {
                LowerError::ContractViolation {
                    reason: format!("discrete alias child `{child}` lost target metadata"),
                    span: edge.source.span,
                }
            })?;
            Ok(dae::Equation {
                lhs: Some(child.reference),
                rhs: rumoca_core::Expression::VarRef {
                    name: parent.reference,
                    subscripts: Vec::new(),
                    span: edge.source.span,
                },
                span: edge.source.span,
                origin: edge.source.origin.clone(),
                scalar_count: 1,
            })
        })
        .collect::<Result<Vec<_>, _>>()
}

fn visit_alias_neighbors(
    parent: &rumoca_core::VarName,
    adjacency: &IndexMap<rumoca_core::VarName, Vec<(rumoca_core::VarName, usize)>>,
    visited: &mut IndexSet<rumoca_core::VarName>,
    queue: &mut VecDeque<rumoca_core::VarName>,
    directed: &mut Vec<(usize, rumoca_core::VarName, rumoca_core::VarName)>,
) {
    let Some(neighbors) = adjacency.get(parent) else {
        return;
    };
    for (child, edge_idx) in neighbors {
        if !visited.insert(child.clone()) {
            continue;
        }
        queue.push_back(child.clone());
        directed.push((*edge_idx, parent.clone(), child.clone()));
    }
}

fn collect_alias_component(
    root: &rumoca_core::VarName,
    adjacency: &IndexMap<rumoca_core::VarName, Vec<(rumoca_core::VarName, usize)>>,
) -> Vec<rumoca_core::VarName> {
    let mut component = Vec::new();
    let mut seen = IndexSet::new();
    let mut queue = VecDeque::from([root.clone()]);
    while let Some(name) = queue.pop_front() {
        if !seen.insert(name.clone()) {
            continue;
        }
        if let Some(neighbors) = adjacency.get(&name) {
            for (neighbor, _) in neighbors {
                queue.push_back(neighbor.clone());
            }
        }
        component.push(name);
    }
    component
}

fn alias_component_roots(
    component: &[rumoca_core::VarName],
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
) -> Vec<rumoca_core::VarName> {
    let protected = component
        .iter()
        .filter(|name| protected_lhs_names.contains(*name))
        .cloned()
        .collect::<Vec<_>>();
    if !protected.is_empty() {
        return protected;
    }
    component.first().cloned().into_iter().collect::<Vec<_>>()
}

fn protected_discrete_update_lhs_names(
    dae_model: &dae::Dae,
) -> Result<IndexSet<rumoca_core::VarName>, LowerError> {
    let mut protected = IndexSet::new();
    for eq in dae_model
        .discrete
        .real_updates
        .iter()
        .chain(dae_model.discrete.valued_updates.iter())
        .chain(dae_model.conditions.equations.iter())
    {
        if let Some(lhs) = eq.lhs.as_ref() {
            if is_plain_discrete_alias_rhs(dae_model, &eq.rhs, eq.span)? {
                continue;
            }
            for name in
                discrete_update_lhs_names(dae_model, lhs.var_name(), eq.scalar_count.max(1))?
            {
                protected.insert(name);
            }
        } else if let Some(target) = residual_non_alias_update_target(dae_model, &eq.rhs, eq.span)?
        {
            protected.insert(target);
        }
    }
    Ok(protected)
}

fn residual_non_alias_update_target(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
    _span: rumoca_core::Span,
) -> Result<Option<rumoca_core::VarName>, LowerError> {
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            span,
        } => binary_residual_non_alias_update_target(dae_model, lhs, rhs, *span),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            span,
        } => if_residual_non_alias_update_target(dae_model, branches, else_branch, *span),
        _ => Ok(None),
    }
}

fn binary_residual_non_alias_update_target(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
) -> Result<Option<rumoca_core::VarName>, LowerError> {
    match (
        plain_discrete_target(dae_model, lhs, span)?,
        plain_discrete_target(dae_model, rhs, span)?,
    ) {
        (Some(target), None) | (None, Some(target)) => Ok(Some(target.name)),
        (Some(_), Some(_)) | (None, None) => Ok(None),
    }
}

fn if_residual_non_alias_update_target(
    dae_model: &dae::Dae,
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    span: rumoca_core::Span,
) -> Result<Option<rumoca_core::VarName>, LowerError> {
    let mut common_target = None;
    for (_, value) in branches {
        let Some(target) = residual_non_alias_update_target(dae_model, value, span)? else {
            return Ok(None);
        };
        if update_common_discrete_target(&mut common_target, target).is_none() {
            return Ok(None);
        }
    }
    let Some(target) = residual_non_alias_update_target(dae_model, else_branch, span)? else {
        return Ok(None);
    };
    if update_common_discrete_target(&mut common_target, target).is_none() {
        return Ok(None);
    }
    Ok(common_target)
}

fn discrete_update_lhs_names(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    scalar_count: usize,
) -> Result<Vec<rumoca_core::VarName>, LowerError> {
    let scalar_names = discrete_update_scalar_names(dae_model, lhs, scalar_count)?;
    if scalar_count <= 1 {
        return Ok(scalar_names);
    }
    let mut names = Vec::with_capacity(scalar_names.len() + 1);
    names.push(lhs.clone());
    names.extend(scalar_names);
    Ok(names)
}

fn discrete_update_scalar_names(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    scalar_count: usize,
) -> Result<Vec<rumoca_core::VarName>, LowerError> {
    if scalar_count <= 1 {
        return Ok(vec![lhs.clone()]);
    }
    let dims =
        discrete_update_dims(dae_model, lhs).ok_or_else(|| LowerError::ContractViolation {
            reason: format!(
                "array discrete update `{}` must have DAE variable dimensions",
                lhs.as_str()
            ),
            span: rumoca_core::Span::DUMMY,
        })?;
    Ok((0..scalar_count)
        .map(|idx| {
            rumoca_core::VarName::new(dae::scalar_name_text_for_flat_index(
                lhs.as_str(),
                dims,
                idx,
            ))
        })
        .collect())
}

fn is_plain_discrete_alias_rhs(
    dae_model: &dae::Dae,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    Ok(plain_discrete_target(dae_model, rhs, span)?.is_some())
}

fn should_reorient_discrete_alias(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    alias_target: &rumoca_core::VarName,
    scalar_count: usize,
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
    occupied_lhs_names: &IndexSet<rumoca_core::VarName>,
) -> Result<bool, LowerError> {
    let lhs_names = discrete_update_lhs_names(dae_model, lhs, scalar_count)?;
    let target_names = discrete_update_lhs_names(dae_model, alias_target, scalar_count)?;
    let lhs_protected = any_name_in_set(&lhs_names, protected_lhs_names);
    let target_protected = any_name_in_set(&target_names, protected_lhs_names);
    if lhs_protected != target_protected {
        return Ok(lhs_protected);
    }

    // MLS §8.3: alias equations are simultaneous equations, not ordered
    // assignments. Choose an orientation that keeps the solve-IR update target
    // set single-writer when either side of the alias is still available.
    let lhs_occupied = any_name_in_set(&lhs_names, occupied_lhs_names);
    let target_occupied = any_name_in_set(&target_names, occupied_lhs_names);
    Ok(lhs_occupied && !target_occupied)
}

fn any_name_in_set(names: &[rumoca_core::VarName], set: &IndexSet<rumoca_core::VarName>) -> bool {
    names.iter().any(|name| set.contains(name))
}

fn extract_residual_discrete_assignment(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
    scalar_count: usize,
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
    occupied_lhs_names: &IndexSet<rumoca_core::VarName>,
) -> Result<Option<(DiscreteTarget, rumoca_core::Expression)>, LowerError> {
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            span,
        } => extract_binary_residual_discrete_assignment(
            dae_model,
            lhs,
            rhs,
            *span,
            scalar_count,
            protected_lhs_names,
            occupied_lhs_names,
        ),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            span,
        } => extract_if_residual_discrete_assignment(
            dae_model,
            branches,
            else_branch,
            *span,
            scalar_count,
            protected_lhs_names,
            occupied_lhs_names,
        ),
        _ => Ok(None),
    }
}

fn extract_binary_residual_discrete_assignment(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    scalar_count: usize,
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
    occupied_lhs_names: &IndexSet<rumoca_core::VarName>,
) -> Result<Option<(DiscreteTarget, rumoca_core::Expression)>, LowerError> {
    let lhs_target = plain_discrete_target(dae_model, lhs, span)?;
    let rhs_target = plain_discrete_target(dae_model, rhs, span)?;
    match (lhs_target, rhs_target) {
        (Some(lhs_target), Some(rhs_target)) => {
            if should_reorient_discrete_alias(
                dae_model,
                &lhs_target.name,
                &rhs_target.name,
                scalar_count,
                protected_lhs_names,
                occupied_lhs_names,
            )? {
                Ok(Some((rhs_target, lhs.clone())))
            } else {
                Ok(Some((lhs_target, rhs.clone())))
            }
        }
        (Some(lhs_target), _) => Ok(Some((lhs_target, rhs.clone()))),
        (None, Some(rhs_target)) => Ok(Some((rhs_target, lhs.clone()))),
        (None, None) => Ok(None),
    }
}

fn extract_if_residual_discrete_assignment(
    dae_model: &dae::Dae,
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    span: rumoca_core::Span,
    scalar_count: usize,
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
    occupied_lhs_names: &IndexSet<rumoca_core::VarName>,
) -> Result<Option<(DiscreteTarget, rumoca_core::Expression)>, LowerError> {
    let mut common_target: Option<DiscreteTarget> = None;
    let mut rhs_branches = Vec::with_capacity(branches.len());
    for (condition, value) in branches {
        let Some((target, rhs)) = extract_residual_discrete_assignment(
            dae_model,
            value,
            scalar_count,
            protected_lhs_names,
            occupied_lhs_names,
        )?
        else {
            return Ok(None);
        };
        match common_target.as_ref() {
            Some(existing) if existing.name != target.name => return Ok(None),
            Some(_) => {}
            None => common_target = Some(target),
        }
        rhs_branches.push((condition.clone(), rhs));
    }

    let Some((target, else_rhs)) = extract_residual_discrete_assignment(
        dae_model,
        else_branch,
        scalar_count,
        protected_lhs_names,
        occupied_lhs_names,
    )?
    else {
        return Ok(None);
    };
    match common_target.as_ref() {
        Some(existing) if existing.name != target.name => return Ok(None),
        Some(_) => {}
        None => common_target = Some(target),
    }
    let Some(target) = common_target else {
        return Ok(None);
    };

    Ok(Some((
        target,
        rumoca_core::Expression::If {
            branches: rhs_branches,
            else_branch: Box::new(else_rhs),
            span,
        },
    )))
}

fn update_common_discrete_target(
    common_target: &mut Option<rumoca_core::VarName>,
    target: rumoca_core::VarName,
) -> Option<()> {
    match common_target {
        Some(existing) if *existing != target => None,
        Some(_) => Some(()),
        None => {
            *common_target = Some(target);
            Some(())
        }
    }
}

fn plain_discrete_target(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
    span: rumoca_core::Span,
) -> Result<Option<DiscreteTarget>, LowerError> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return Ok(None);
    };
    let Some(target_name) = target_name_from_var_ref(name, subscripts)? else {
        return Ok(None);
    };
    if !is_discrete_update_name(dae_model, &target_name) {
        return Ok(None);
    }
    Ok(Some(discrete_target_from_var_ref(
        dae_model,
        name,
        subscripts,
        target_name,
        span,
    )?))
}

fn target_name_from_var_ref(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
) -> Result<Option<rumoca_core::VarName>, LowerError> {
    if subscripts.is_empty() {
        return Ok(Some(name.var_name().clone()));
    }
    let Some(indices) = static_subscript_indices(subscripts)? else {
        return Ok(None);
    };
    Ok(Some(rumoca_core::VarName::new(dae::format_subscript_key(
        name.as_str(),
        &indices,
    ))))
}

fn discrete_target_from_var_ref(
    dae_model: &dae::Dae,
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    target_name: rumoca_core::VarName,
    span: rumoca_core::Span,
) -> Result<DiscreteTarget, LowerError> {
    if subscripts.is_empty() {
        return Ok(DiscreteTarget {
            reference: reference_for_discrete_target(dae_model, &target_name, span)?,
            name: target_name,
        });
    }
    let reference = match name.component_ref() {
        Some(component_ref) => rumoca_core::Reference::with_component_reference(
            target_name.as_str(),
            component_reference_with_subscripts(component_ref.clone(), subscripts.to_vec()),
        ),
        None => reference_for_discrete_target(dae_model, &target_name, span)?,
    };
    Ok(DiscreteTarget {
        name: target_name,
        reference,
    })
}

fn discrete_target_from_reference(
    dae_model: &dae::Dae,
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
) -> Result<DiscreteTarget, LowerError> {
    let Some(target_name) = target_name_from_var_ref(name, subscripts)? else {
        return Err(LowerError::ContractViolation {
            reason: format!(
                "discrete update target `{}` has dynamic subscripts and cannot be oriented as a scalar target",
                name.as_str()
            ),
            span,
        });
    };
    discrete_target_from_var_ref(dae_model, name, subscripts, target_name, span)
}

fn discrete_update_scalar_targets(
    dae_model: &dae::Dae,
    target: &DiscreteTarget,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<Vec<DiscreteTarget>, LowerError> {
    if scalar_count <= 1 {
        return Ok(vec![target.clone()]);
    }
    discrete_update_scalar_names(dae_model, &target.name, scalar_count)?
        .into_iter()
        .map(|name| {
            Ok(DiscreteTarget {
                reference: reference_for_discrete_target(dae_model, &name, span)?,
                name,
            })
        })
        .collect()
}

fn reference_for_discrete_target(
    dae_model: &dae::Dae,
    target: &rumoca_core::VarName,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Reference, LowerError> {
    if let Some(variable) = discrete_update_variable(dae_model, target) {
        return reference_for_discrete_variable(target, variable, span);
    }
    if let Some(scalar_name) = rumoca_core::parse_scalar_name(target.as_str()) {
        let base = rumoca_core::VarName::new(scalar_name.base);
        if let Some(variable) = discrete_update_variable(dae_model, &base) {
            return match variable.origin {
                dae::VariableOrigin::Generated => {
                    Ok(rumoca_core::Reference::generated(target.as_str()))
                }
                dae::VariableOrigin::Source => {
                    let component_ref =
                        source_component_ref_for_discrete_variable(&base, variable, span)?;
                    Ok(rumoca_core::Reference::with_component_reference(
                        target.as_str(),
                        component_reference_with_scalar_indices(
                            component_ref,
                            &scalar_name.indices,
                            variable.source_span,
                        ),
                    ))
                }
            };
        }
    }
    Err(LowerError::ContractViolation {
        reason: format!("discrete update target `{target}` has no DAE variable metadata"),
        span,
    })
}

fn reference_for_discrete_variable(
    target: &rumoca_core::VarName,
    variable: &dae::Variable,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Reference, LowerError> {
    match variable.origin {
        dae::VariableOrigin::Source => {
            #[cfg(test)]
            if variable.source_span.is_dummy() {
                return Ok(rumoca_core::Reference::generated(target.as_str()));
            }
            Ok(rumoca_core::Reference::with_component_reference(
                target.as_str(),
                source_component_ref_for_discrete_variable(target, variable, span)?,
            ))
        }
        dae::VariableOrigin::Generated => Ok(rumoca_core::Reference::generated(target.as_str())),
    }
}

fn source_component_ref_for_discrete_variable(
    target: &rumoca_core::VarName,
    variable: &dae::Variable,
    span: rumoca_core::Span,
) -> Result<rumoca_core::ComponentReference, LowerError> {
    variable
        .component_ref
        .clone()
        .ok_or_else(|| LowerError::ContractViolation {
            reason: format!(
                "source discrete update target `{target}` lost structured component-reference metadata"
            ),
            span,
        })
}

fn component_reference_with_scalar_indices(
    component_ref: rumoca_core::ComponentReference,
    indices: &[i64],
    span: rumoca_core::Span,
) -> rumoca_core::ComponentReference {
    component_reference_with_subscripts(
        component_ref,
        indices
            .iter()
            .map(|index| rumoca_core::Subscript::generated_index(*index, span))
            .collect(),
    )
}

fn component_reference_with_subscripts(
    mut component_ref: rumoca_core::ComponentReference,
    subscripts: Vec<rumoca_core::Subscript>,
) -> rumoca_core::ComponentReference {
    if let Some(part) = component_ref.parts.last_mut() {
        part.subs.extend(subscripts);
    }
    component_ref
}

fn is_discrete_update_name(dae_model: &dae::Dae, name: &rumoca_core::VarName) -> bool {
    dae_model.variables.discrete_reals.contains_key(name)
        || dae_model.variables.discrete_valued.contains_key(name)
        || dae_model.variables.inputs.contains_key(name)
        || dae::component_base_name(name.as_str()).is_some_and(|base| {
            let base = rumoca_core::VarName::new(base);
            dae_model.variables.discrete_reals.contains_key(&base)
                || dae_model.variables.discrete_valued.contains_key(&base)
                || dae_model.variables.inputs.contains_key(&base)
        })
}

fn discrete_update_variable<'a>(
    dae_model: &'a dae::Dae,
    lhs: &rumoca_core::VarName,
) -> Option<&'a dae::Variable> {
    dae_model
        .variables
        .inputs
        .get(lhs)
        .or_else(|| dae_model.variables.discrete_reals.get(lhs))
        .or_else(|| dae_model.variables.discrete_valued.get(lhs))
}

fn discrete_update_dims<'a>(
    dae_model: &'a dae::Dae,
    lhs: &rumoca_core::VarName,
) -> Option<&'a [i64]> {
    discrete_update_variable(dae_model, lhs)
        .or_else(|| discrete_update_base_dims(dae_model, lhs))
        .map(|var| var.dims.as_slice())
}

fn discrete_update_base_dims<'a>(
    dae_model: &'a dae::Dae,
    lhs: &rumoca_core::VarName,
) -> Option<&'a dae::Variable> {
    let base = dae::component_base_name(lhs.as_str())?;
    let base = rumoca_core::VarName::new(base);
    dae_model
        .variables
        .inputs
        .get(&base)
        .or_else(|| dae_model.variables.discrete_reals.get(&base))
        .or_else(|| dae_model.variables.discrete_valued.get(&base))
}

fn rewrite_relation_event_memory_expr(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> Result<rumoca_core::Expression, LowerError> {
    let mut rewriter = RelationEventMemoryRewriter {
        dae_model,
        error: None,
    };
    let rewritten = rewriter.rewrite_expression(expr);
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(rewritten),
    }
}

struct RelationEventMemoryRewriter<'a> {
    dae_model: &'a dae::Dae,
    error: Option<LowerError>,
}

impl ExpressionRewriter for RelationEventMemoryRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if self.error.is_some() {
            return expr.clone();
        }
        let rumoca_core::Expression::BuiltinCall {
            function,
            args,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        if *function == rumoca_core::BuiltinFunction::Pre && args.len() == 1 {
            let Some(memory) = self.relation_memory_expr_for_condition(&args[0]) else {
                return self.walk_expression(expr);
            };
            // MLS §3.7.5 and §8.6: pre(relation(v)) reads relation memory from
            // the previous event-iteration pass, not the live value currently
            // being settled.
            return pre_relation_memory_expr(memory, *span);
        }
        if *function == rumoca_core::BuiltinFunction::Edge && args.len() == 1 {
            let Some(memory) = self.relation_memory_expr_for_condition(&args[0]) else {
                return self.walk_expression(expr);
            };
            return self.relation_edge_expr(memory, *span);
        }
        self.walk_expression(expr)
    }
}

impl RelationEventMemoryRewriter<'_> {
    fn relation_memory_expr_for_condition(
        &mut self,
        condition: &rumoca_core::Expression,
    ) -> Option<rumoca_core::Expression> {
        match relation_memory_expr_for_condition(self.dae_model, condition) {
            Ok(memory) => memory,
            Err(error) => {
                self.error = Some(error);
                None
            }
        }
    }

    fn relation_edge_expr(
        &mut self,
        memory: rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let previous_memory = pre_relation_memory_expr(memory.clone(), span);
        let edge = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::And,
            lhs: Box::new(memory),
            rhs: Box::new(rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Not,
                rhs: Box::new(previous_memory),
                span,
            }),
            span,
        };
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::And,
            lhs: Box::new(edge),
            rhs: Box::new(rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Not,
                rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Initial,
                    args: vec![],
                    span,
                }),
                span,
            }),
            span,
        }
    }
}

fn pre_relation_memory_expr(
    memory: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    match memory {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(format!("__pre__.{}", name.as_str())),
            subscripts,
            span,
        },
        other => other,
    }
}

fn relation_memory_expr_for_condition(
    dae_model: &dae::Dae,
    condition: &rumoca_core::Expression,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let relation_idx = dae_model
        .conditions
        .relations
        .iter()
        .position(|relation| relation == condition);
    let Some(relation_idx) = relation_idx else {
        return Ok(None);
    };
    let mut offset = 0usize;
    for eq in &dae_model.conditions.equations {
        let scalar_count = eq.scalar_count.max(1);
        if relation_idx < offset + scalar_count {
            let Some(lhs) = eq.lhs.as_ref() else {
                return Ok(None);
            };
            return Ok(Some(relation_memory_scalar_expr(
                dae_model,
                lhs.var_name(),
                relation_idx - offset,
                scalar_count,
                eq.span,
            )?));
        }
        offset += scalar_count;
    }
    Ok(None)
}

fn relation_memory_scalar_expr(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    flat_index: usize,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, LowerError> {
    if scalar_count <= 1 {
        return Ok(rumoca_core::Expression::VarRef {
            name: lhs.clone().into(),
            subscripts: Vec::new(),
            span,
        });
    }
    let Some(dims) = dae_model
        .variables
        .discrete_valued
        .get(lhs)
        .or_else(|| dae_model.variables.discrete_reals.get(lhs))
        .map(|var| var.dims.as_slice())
    else {
        return Err(LowerError::ContractViolation {
            reason: format!(
                "relation memory target `{lhs}` has scalar_count={scalar_count} but no DAE variable metadata"
            ),
            span,
        });
    };
    let Some(indices) = dae::flat_index_to_subscripts(dims, flat_index) else {
        return Err(LowerError::ContractViolation {
            reason: format!(
                "relation memory target `{lhs}` cannot map flat index {flat_index} of {scalar_count} through dims {dims:?}"
            ),
            span,
        });
    };
    let subscripts = indices
        .into_iter()
        .map(|index| rumoca_core::Subscript::generated_index(index as i64, span))
        .collect();
    Ok(rumoca_core::Expression::VarRef {
        name: lhs.clone().into(),
        subscripts,
        span,
    })
}
