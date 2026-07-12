use std::collections::VecDeque;

use super::helpers::{dims_scalar_count, static_subscript_indices_with_owner};
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

fn discrete_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    reserve_discrete_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

fn reserve_discrete_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    values.try_reserve_exact(additional).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn reserve_discrete_index_map_capacity<K, V>(
    values: &mut IndexMap<K, V>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError>
where
    K: std::hash::Hash + Eq,
{
    values.try_reserve(additional).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn reserve_discrete_index_set_capacity<T>(
    values: &mut IndexSet<T>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError>
where
    T: std::hash::Hash + Eq,
{
    values.try_reserve(additional).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

pub(crate) fn normalized_discrete_update_equations(
    dae_model: &dae::Dae,
) -> Result<Vec<dae::Equation>, LowerError> {
    let protected_lhs_names = protected_discrete_update_lhs_names(dae_model)?;
    let mut occupied_lhs_names = IndexSet::new();
    let Some(first_equation) = dae_model
        .discrete
        .real_updates
        .first()
        .or_else(|| dae_model.discrete.valued_updates.first())
        .or_else(|| dae_model.conditions.equations.first())
    else {
        return Ok(Vec::new());
    };
    let span = first_equation.span;
    let equation_capacity = dae_model
        .discrete
        .real_updates
        .len()
        .checked_add(dae_model.discrete.valued_updates.len())
        .and_then(|count| count.checked_add(dae_model.conditions.equations.len()))
        .ok_or_else(|| {
            LowerError::contract_violation(
                "discrete update equation count exceeds host index range",
                span,
            )
        })?;
    let mut normalized_equations = discrete_vec_with_capacity(
        equation_capacity,
        "normalized discrete equation count",
        span,
    )?;
    let mut alias_edges =
        discrete_vec_with_capacity(equation_capacity, "discrete alias edge count", span)?;
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
    let alias_equations =
        oriented_discrete_alias_equations(dae_model, &alias_edges, &protected_lhs_names)?;
    reserve_discrete_capacity(
        &mut normalized_equations,
        alias_equations.len(),
        "normalized discrete equation count",
        span,
    )?;
    normalized_equations.extend(alias_equations);
    Ok(normalized_equations)
}

pub(crate) fn initial_condition_update_equations(
    dae_model: &dae::Dae,
) -> Result<Vec<dae::Equation>, LowerError> {
    let Some(first_equation) = dae_model.conditions.equations.first() else {
        return Ok(Vec::new());
    };
    let span = first_equation.span;
    let mut equations = discrete_vec_with_capacity(
        dae_model.conditions.equations.len(),
        "initial condition update equation count",
        span,
    )?;
    for eq in &dae_model.conditions.equations {
        if eq
            .lhs
            .as_ref()
            .is_some_and(|lhs| is_condition_memory_lhs(dae_model, lhs.var_name()))
        {
            equations.push(eq.clone());
        }
    }
    Ok(equations)
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
    let equations = initial_condition_update_equations(dae_model)?;
    let structural_bindings = compile_time::structural_bindings(dae_model)?;
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
                direct_assignments: None,
                guard_target_start_before_first_clock_tick: false,
            },
            true,
        )?,
    )?
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
        reserve_discrete_capacity(
            alias_edges,
            edges.len(),
            "discrete alias edge count",
            eq.span,
        )?;
        alias_edges.extend(edges);
        return Ok(());
    }
    if let Some(edges) = residual_discrete_alias_edges(dae_model, &rewritten)? {
        reserve_discrete_capacity(
            alias_edges,
            edges.len(),
            "discrete alias edge count",
            eq.span,
        )?;
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
        let lhs_names = discrete_update_lhs_names(
            dae_model,
            lhs.var_name(),
            normalized.scalar_count.max(1),
            normalized.span,
        )?;
        reserve_discrete_index_set_capacity(
            occupied_lhs_names,
            lhs_names.len(),
            "occupied discrete LHS name count",
            normalized.span,
        )?;
        occupied_lhs_names.extend(lhs_names);
    }
    reserve_discrete_capacity(
        normalized_equations,
        1,
        "normalized discrete equation count",
        normalized.span,
    )?;
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

    let mut equations =
        discrete_vec_with_capacity(elements.len(), "tuple residual expansion count", eq.span)?;
    for (target_expr, output) in elements.iter().zip(&function.outputs) {
        let Some(target_names) = tuple_assignment_target_names(dae_model, target_expr, output)?
        else {
            return Ok(None);
        };
        reserve_discrete_capacity(
            &mut equations,
            target_names.len(),
            "tuple residual expansion count",
            eq.span,
        )?;
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
        let owner_span =
            target_expr_or_subscript_span(target_expr, subscripts, "tuple assignment target")?;
        let Some(indices) = static_subscript_indices_with_owner(subscripts, owner_span)
            .ok()
            .flatten()
        else {
            return Ok(None);
        };
        let mut names =
            discrete_vec_with_capacity(1, "tuple assignment target name count", output.span)?;
        names.push(rumoca_core::VarName::new(dae::format_subscript_key(
            name.as_str(),
            &indices,
        )));
        return Ok(Some(names));
    }
    let output_count = tuple_assignment_output_count(dae_model, name, output)?;
    if output_count == 1 {
        let mut names =
            discrete_vec_with_capacity(1, "tuple assignment target name count", output.span)?;
        names.push(name.var_name().clone());
        return Ok(Some(names));
    }
    Ok(Some(discrete_update_scalar_names(
        dae_model,
        name.var_name(),
        output_count,
        output.span,
    )?))
}

fn target_expr_or_subscript_span(
    expr: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    expr.span()
        .or_else(|| {
            subscripts
                .iter()
                .map(rumoca_core::Subscript::span)
                .find(|span| !span.is_dummy())
        })
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: format!("missing source provenance for {context}"),
        })
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
            LowerError::contract_violation(
                format!(
                    "tuple assignment target `{}` must have DAE variable dimensions",
                    target_name.as_str()
                ),
                output.span,
            )
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
            normalized.span,
        )?
    {
        let old_lhs = lhs.clone();
        normalized.lhs = Some(alias_target.reference);
        normalized.rhs = rumoca_core::Expression::VarRef {
            name: old_lhs,
            subscripts: Vec::new(),
            span: normalized.span,
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
    let mut edges =
        discrete_vec_with_capacity(lhs_targets.len(), "discrete alias edge count", source.span)?;
    for (lhs, rhs) in lhs_targets.into_iter().zip(rhs_targets) {
        if lhs.name != rhs.name {
            edges.push(DiscreteAliasEdge {
                lhs,
                rhs,
                source: source.clone(),
            });
        }
    }
    Ok(Some(edges))
}

// SPEC_0021: Exception - alias orientation builds the graph, validates
// components, and emits equations in deterministic order.
#[allow(clippy::too_many_lines)]
fn oriented_discrete_alias_equations(
    dae_model: &dae::Dae,
    edges: &[DiscreteAliasEdge],
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
) -> Result<Vec<dae::Equation>, LowerError> {
    let Some(first_edge) = edges.first() else {
        return Ok(Vec::new());
    };
    let mut adjacency: IndexMap<rumoca_core::VarName, Vec<(rumoca_core::VarName, usize)>> =
        IndexMap::new();
    let span = first_edge.source.span;
    let mut target_by_name: IndexMap<rumoca_core::VarName, DiscreteTarget> = IndexMap::new();
    let mut ordered_names =
        discrete_vec_with_capacity(edges.len(), "discrete alias ordered name count", span)?;
    let mut seen_names = IndexSet::new();
    for (idx, edge) in edges.iter().enumerate() {
        for target in [&edge.lhs, &edge.rhs] {
            let name = &target.name;
            if !target_by_name.contains_key(name) {
                reserve_discrete_index_map_capacity(
                    &mut target_by_name,
                    1,
                    "discrete alias target metadata count",
                    edge.source.span,
                )?;
                target_by_name.insert(name.clone(), target.clone());
            }
            if !seen_names.contains(name) {
                reserve_discrete_index_set_capacity(
                    &mut seen_names,
                    1,
                    "discrete alias seen name count",
                    edge.source.span,
                )?;
                seen_names.insert(name.clone());
                reserve_discrete_capacity(
                    &mut ordered_names,
                    1,
                    "discrete alias ordered name count",
                    edge.source.span,
                )?;
                ordered_names.push(name.clone());
            }
        }
        push_alias_neighbor(
            &mut adjacency,
            &edge.lhs.name,
            &edge.rhs.name,
            idx,
            edge.source.span,
        )?;
        push_alias_neighbor(
            &mut adjacency,
            &edge.rhs.name,
            &edge.lhs.name,
            idx,
            edge.source.span,
        )?;
    }

    let mut visited = IndexSet::new();
    let mut directed = discrete_vec_with_capacity(edges.len(), "directed alias edge count", span)?;
    for root_candidate in ordered_names {
        if visited.contains(&root_candidate) {
            continue;
        }
        let component = collect_alias_component(&root_candidate, &adjacency, span)?;
        let roots = alias_component_roots(dae_model, &component, protected_lhs_names, span)?;
        for root in &roots {
            if !visited.contains(root) {
                reserve_discrete_index_set_capacity(
                    &mut visited,
                    1,
                    "visited alias root count",
                    span,
                )?;
                visited.insert(root.clone());
            }
        }

        let mut queue = VecDeque::from(roots);
        while let Some(parent) = queue.pop_front() {
            visit_alias_neighbors(
                &parent,
                &adjacency,
                &mut visited,
                &mut queue,
                &mut directed,
                span,
            )?;
        }
    }

    directed.sort_by_key(|(edge_idx, _, _)| *edge_idx);
    let mut equations = discrete_vec_with_capacity(
        directed.len(),
        "oriented discrete alias equation count",
        span,
    )?;
    for (edge_idx, parent, child) in directed {
        let edge = &edges[edge_idx];
        let parent = target_by_name.get(&parent).cloned().ok_or_else(|| {
            LowerError::contract_violation(
                format!("discrete alias parent `{parent}` lost target metadata"),
                edge.source.span,
            )
        })?;
        let child = target_by_name.get(&child).cloned().ok_or_else(|| {
            LowerError::contract_violation(
                format!("discrete alias child `{child}` lost target metadata"),
                edge.source.span,
            )
        })?;
        equations.push(dae::Equation {
            lhs: Some(child.reference),
            rhs: rumoca_core::Expression::VarRef {
                name: parent.reference,
                subscripts: Vec::new(),
                span: edge.source.span,
            },
            span: edge.source.span,
            origin: edge.source.origin.clone(),
            scalar_count: 1,
        });
    }
    Ok(equations)
}

fn push_alias_neighbor(
    adjacency: &mut IndexMap<rumoca_core::VarName, Vec<(rumoca_core::VarName, usize)>>,
    source: &rumoca_core::VarName,
    target: &rumoca_core::VarName,
    edge_idx: usize,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    if let Some(neighbors) = adjacency.get_mut(source) {
        reserve_discrete_capacity(neighbors, 1, "discrete alias adjacency count", span)?;
        neighbors.push((target.clone(), edge_idx));
        return Ok(());
    }
    reserve_discrete_index_map_capacity(adjacency, 1, "discrete alias adjacency map count", span)?;
    let mut neighbors = discrete_vec_with_capacity(1, "discrete alias adjacency count", span)?;
    neighbors.push((target.clone(), edge_idx));
    adjacency.insert(source.clone(), neighbors);
    Ok(())
}

fn visit_alias_neighbors(
    parent: &rumoca_core::VarName,
    adjacency: &IndexMap<rumoca_core::VarName, Vec<(rumoca_core::VarName, usize)>>,
    visited: &mut IndexSet<rumoca_core::VarName>,
    queue: &mut VecDeque<rumoca_core::VarName>,
    directed: &mut Vec<(usize, rumoca_core::VarName, rumoca_core::VarName)>,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    let Some(neighbors) = adjacency.get(parent) else {
        return Ok(());
    };
    for (child, edge_idx) in neighbors {
        if visited.contains(child) {
            continue;
        }
        reserve_discrete_index_set_capacity(visited, 1, "visited alias name count", span)?;
        visited.insert(child.clone());
        queue.push_back(child.clone());
        reserve_discrete_capacity(directed, 1, "directed alias edge count", span)?;
        directed.push((*edge_idx, parent.clone(), child.clone()));
    }
    Ok(())
}

fn collect_alias_component(
    root: &rumoca_core::VarName,
    adjacency: &IndexMap<rumoca_core::VarName, Vec<(rumoca_core::VarName, usize)>>,
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::VarName>, LowerError> {
    let mut component = Vec::new();
    let mut seen = IndexSet::new();
    let mut queue = VecDeque::from([root.clone()]);
    while let Some(name) = queue.pop_front() {
        if seen.contains(&name) {
            continue;
        }
        reserve_discrete_index_set_capacity(&mut seen, 1, "alias component seen count", span)?;
        seen.insert(name.clone());
        if let Some(neighbors) = adjacency.get(&name) {
            for (neighbor, _) in neighbors {
                queue.push_back(neighbor.clone());
            }
        }
        reserve_discrete_capacity(&mut component, 1, "alias component name count", span)?;
        component.push(name);
    }
    Ok(component)
}

fn alias_component_roots(
    dae_model: &dae::Dae,
    component: &[rumoca_core::VarName],
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::VarName>, LowerError> {
    // Inputs are read-only sources: force them to be roots so alias edges always
    // orient *away* from them (`state := input`), never toward them. Protected
    // names (non-alias update targets) are likewise forced roots.
    let mut forced =
        discrete_vec_with_capacity(component.len(), "alias component forced root count", span)?;
    for name in component {
        if protected_lhs_names.contains(name) || is_discrete_input_name(dae_model, name) {
            forced.push(name.clone());
        }
    }
    if !forced.is_empty() {
        return Ok(forced);
    }
    let mut roots = discrete_vec_with_capacity(1, "alias component root count", span)?;
    if let Some(name) = component.first() {
        roots.push(name.clone());
    }
    Ok(roots)
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
            let names = discrete_update_lhs_names(
                dae_model,
                lhs.var_name(),
                eq.scalar_count.max(1),
                eq.span,
            )?;
            reserve_discrete_index_set_capacity(
                &mut protected,
                names.len(),
                "protected discrete LHS name count",
                eq.span,
            )?;
            for name in names {
                protected.insert(name);
            }
        } else if let Some(target) = residual_non_alias_update_target(dae_model, &eq.rhs, eq.span)?
        {
            reserve_discrete_index_set_capacity(
                &mut protected,
                1,
                "protected discrete LHS name count",
                eq.span,
            )?;
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
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::VarName>, LowerError> {
    let scalar_names = discrete_update_scalar_names(dae_model, lhs, scalar_count, span)?;
    if scalar_count <= 1 {
        return Ok(scalar_names);
    }
    let capacity = scalar_names.len().checked_add(1).ok_or_else(|| {
        LowerError::contract_violation(
            "discrete update LHS name count exceeds host index range",
            span,
        )
    })?;
    let mut names = discrete_vec_with_capacity(capacity, "discrete update LHS name count", span)?;
    names.push(lhs.clone());
    reserve_discrete_capacity(
        &mut names,
        scalar_names.len(),
        "discrete update LHS name count",
        span,
    )?;
    names.extend(scalar_names);
    Ok(names)
}

fn discrete_update_scalar_names(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::VarName>, LowerError> {
    if scalar_count <= 1 {
        let mut names = discrete_vec_with_capacity(1, "discrete update scalar name count", span)?;
        names.push(lhs.clone());
        return Ok(names);
    }
    let dims = discrete_update_dims(dae_model, lhs).ok_or_else(|| {
        LowerError::contract_violation(
            format!(
                "array discrete update `{}` must have DAE variable dimensions",
                lhs.as_str()
            ),
            span,
        )
    })?;
    let mut names =
        discrete_vec_with_capacity(scalar_count, "discrete update scalar name count", span)?;
    for idx in 0..scalar_count {
        names.push(rumoca_core::VarName::new(
            dae::scalar_name_text_for_flat_index(lhs.as_str(), dims, idx),
        ));
    }
    Ok(names)
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
    span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    let lhs_names = discrete_update_lhs_names(dae_model, lhs, scalar_count, span)?;
    let target_names = discrete_update_lhs_names(dae_model, alias_target, scalar_count, span)?;

    // Inputs are read-only: never reorient an alias so an input becomes the write
    // target, and always reorient away from one if the current target is an input.
    let lhs_is_input = lhs_names
        .iter()
        .any(|n| is_discrete_input_name(dae_model, n));
    let target_is_input = target_names
        .iter()
        .any(|n| is_discrete_input_name(dae_model, n));
    if target_is_input {
        return Ok(false);
    }
    if lhs_is_input {
        return Ok(true);
    }

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
                span,
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
    let mut rhs_branches =
        discrete_vec_with_capacity(branches.len(), "discrete residual branch count", span)?;
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
    let Some(target_name) = target_name_from_var_ref(name, subscripts, span)? else {
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
    owner_span: rumoca_core::Span,
) -> Result<Option<rumoca_core::VarName>, LowerError> {
    if subscripts.is_empty() {
        return Ok(Some(name.var_name().clone()));
    }
    let Some(indices) = static_subscript_indices_with_owner(subscripts, owner_span)? else {
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
        Some(component_ref) => {
            let copied_subscripts = copy_discrete_subscripts(
                subscripts,
                "discrete target copied subscript count",
                span,
            )?;
            rumoca_core::Reference::with_component_reference(
                target_name.as_str(),
                component_reference_with_subscripts(
                    component_ref.clone(),
                    copied_subscripts,
                    span,
                )?,
            )
        }
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
    let Some(target_name) = target_name_from_var_ref(name, subscripts, span)? else {
        return Err(LowerError::contract_violation(
            format!(
                "discrete update target `{}` has dynamic subscripts and cannot be oriented as a scalar target",
                name.as_str()
            ),
            span,
        ));
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
        let mut targets = discrete_vec_with_capacity(1, "discrete scalar target count", span)?;
        targets.push(target.clone());
        return Ok(targets);
    }
    let scalar_names = discrete_update_scalar_names(dae_model, &target.name, scalar_count, span)?;
    let mut targets =
        discrete_vec_with_capacity(scalar_names.len(), "discrete scalar target count", span)?;
    for name in scalar_names {
        targets.push(DiscreteTarget {
            reference: reference_for_discrete_target(dae_model, &name, span)?,
            name,
        });
    }
    Ok(targets)
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
                    let scalar_span = discrete_target_scalar_span(variable, span)?;
                    Ok(rumoca_core::Reference::with_component_reference(
                        target.as_str(),
                        component_reference_with_scalar_indices(
                            component_ref,
                            &scalar_name.indices,
                            scalar_span,
                        )?,
                    ))
                }
            };
        }
    }
    Err(LowerError::contract_violation(
        format!("discrete update target `{target}` has no DAE variable metadata"),
        span,
    ))
}

fn discrete_target_scalar_span(
    variable: &dae::Variable,
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::ProvenanceSpan, LowerError> {
    let span = if variable.source_span.is_dummy() {
        owner_span
    } else {
        variable.source_span
    };
    Ok(span.require_provenance("discrete target scalar subscript")?)
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
        .ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                "source discrete update target `{target}` lost structured component-reference metadata"
            ),
                span,
            )
        })
}

fn component_reference_with_scalar_indices(
    component_ref: rumoca_core::ComponentReference,
    indices: &[i64],
    span: rumoca_core::ProvenanceSpan,
) -> Result<rumoca_core::ComponentReference, LowerError> {
    let mut subscripts = discrete_vec_with_capacity(
        indices.len(),
        "discrete target subscript count",
        span.span(),
    )?;
    for index in indices {
        subscripts.push(rumoca_core::Subscript::generated_index_with_provenance(
            *index, span,
        ));
    }
    component_reference_with_subscripts(component_ref, subscripts, span.span())
}

fn component_reference_with_subscripts(
    mut component_ref: rumoca_core::ComponentReference,
    subscripts: Vec<rumoca_core::Subscript>,
    span: rumoca_core::Span,
) -> Result<rumoca_core::ComponentReference, LowerError> {
    if let Some(part) = component_ref.parts.last_mut() {
        reserve_discrete_capacity(
            &mut part.subs,
            subscripts.len(),
            "component reference subscript count",
            span,
        )?;
        part.subs.extend(subscripts);
    }
    Ok(component_ref)
}

fn copy_discrete_subscripts(
    subscripts: &[rumoca_core::Subscript],
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Subscript>, LowerError> {
    let mut copied = discrete_vec_with_capacity(subscripts.len(), context, span)?;
    copied.extend(subscripts.iter().cloned());
    Ok(copied)
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

/// An input variable is known externally each tick and is read-only inside the
/// model: it can be an alias *endpoint* (the read side of `state := input`) but
/// must never be selected as an update target. Orienting an alias edge so an
/// input is written would clobber the caller's per-tick input every step.
fn is_discrete_input_name(dae_model: &dae::Dae, name: &rumoca_core::VarName) -> bool {
    dae_model.variables.inputs.contains_key(name)
        || dae::component_base_name(name.as_str()).is_some_and(|base| {
            dae_model
                .variables
                .inputs
                .contains_key(&rumoca_core::VarName::new(base))
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
            name: rumoca_core::Reference::from_var_name(rumoca_core::pre_slot_name(name.as_str())),
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
        let end = checked_relation_offset_end(offset, scalar_count, eq.span)?;
        if relation_idx < end {
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
        offset = end;
    }
    Ok(None)
}

fn checked_relation_offset_end(
    offset: usize,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    offset.checked_add(scalar_count).ok_or_else(|| {
        LowerError::contract_violation(
            "discrete relation scalar offset overflows host index range",
            span,
        )
    })
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
        return Err(LowerError::contract_violation(
            format!(
                "relation memory target `{lhs}` has scalar_count={scalar_count} but no DAE variable metadata"
            ),
            span,
        ));
    };
    let Some(indices) = dae::flat_index_to_subscripts(dims, flat_index) else {
        return Err(LowerError::contract_violation(
            format!(
                "relation memory target `{lhs}` cannot map flat index {flat_index} of {scalar_count} through dims {dims:?}"
            ),
            span,
        ));
    };
    let mut subscripts =
        discrete_vec_with_capacity(indices.len(), "relation memory subscript count", span)?;
    for index in indices {
        subscripts.push(checked_relation_subscript(index, span)?);
    }
    Ok(rumoca_core::Expression::VarRef {
        name: lhs.clone().into(),
        subscripts,
        span,
    })
}

fn checked_relation_subscript(
    index: usize,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Subscript, LowerError> {
    let index = i64::try_from(index).map_err(|_| {
        LowerError::contract_violation(
            format!("discrete relation subscript index {index} exceeds i64 range"),
            span,
        )
    })?;
    Ok(rumoca_core::Subscript::try_generated_index(
        index,
        span,
        "discrete relation subscript",
    )?)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unspanned_discrete_update_test_span() -> rumoca_core::Span {
        rumoca_core::Span::DUMMY
    }

    #[test]
    fn discrete_vec_with_capacity_reports_capacity_overflow() -> Result<(), LowerError> {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_discrete_updates_source_95.mo",
            ),
            4,
            12,
        );

        let err = match discrete_vec_with_capacity::<u8>(usize::MAX, "discrete test vector", span) {
            Ok(_) => {
                return Err(LowerError::contract_violation(
                    "oversized discrete test vector should fail before allocating",
                    span,
                ));
            }
            Err(err) => err,
        };

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.reason()
                .contains("discrete test vector capacity exceeds host memory limits"),
            "unexpected error: {err}"
        );
        Ok(())
    }

    #[test]
    fn discrete_vec_with_capacity_reports_capacity_overflow_without_fabricating_span() {
        let err = discrete_vec_with_capacity::<u8>(
            usize::MAX,
            "discrete test vector",
            unspanned_discrete_update_test_span(),
        )
        .expect_err("oversized discrete test vector must fail before allocating");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason()
                .contains("discrete test vector capacity exceeds host memory limits"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn checked_relation_offset_end_rejects_overflow_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_discrete_updates_source_42.mo",
            ),
            5,
            13,
        );
        let err = checked_relation_offset_end(usize::MAX, 1, span)
            .expect_err("relation offset overflow must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: discrete relation scalar offset overflows host index range"
        );
    }

    #[test]
    fn checked_relation_offset_end_rejects_overflow_without_fabricating_span() {
        let err = checked_relation_offset_end(usize::MAX, 1, unspanned_discrete_update_test_span())
            .expect_err("relation offset overflow must fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: discrete relation scalar offset overflows host index range"
        );
    }

    #[test]
    fn checked_relation_subscript_rejects_i64_overflow_with_span() {
        let Some(index) = usize::try_from(i64::MAX)
            .ok()
            .and_then(|value| value.checked_add(1))
        else {
            return;
        };
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_discrete_updates_source_44.mo",
            ),
            9,
            19,
        );

        let err = checked_relation_subscript(index, span)
            .expect_err("relation subscript must fit in Modelica integer range");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            format!(
                "invalid IR contract: discrete relation subscript index {index} exceeds i64 range"
            )
        );
    }

    #[test]
    fn checked_relation_subscript_rejects_i64_overflow_without_fabricating_span() {
        let Some(index) = usize::try_from(i64::MAX)
            .ok()
            .and_then(|value| value.checked_add(1))
        else {
            return;
        };

        let err = checked_relation_subscript(index, unspanned_discrete_update_test_span())
            .expect_err("relation subscript must fit in Modelica integer range");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            format!(
                "invalid IR contract: discrete relation subscript index {index} exceeds i64 range"
            )
        );
    }

    #[test]
    fn component_reference_with_scalar_indices_preserves_source_span() -> Result<(), LowerError> {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_discrete_updates_source_47.mo",
            ),
            3,
            17,
        );
        let provenance_span = span.require_provenance("test scalar target subscript")?;
        let name = rumoca_core::VarName::new("x");
        let component_ref = rumoca_core::component_reference_from_flat_name(&name, span)
            .ok_or_else(|| LowerError::contract_violation("test component ref", span))?;

        let indexed =
            component_reference_with_scalar_indices(component_ref, &[2, 3], provenance_span)?;
        let Some(last) = indexed.parts.last() else {
            return Err(LowerError::contract_violation(
                "indexed ref has no parts",
                span,
            ));
        };

        assert_eq!(last.subs.len(), 2);
        assert!(last.subs.iter().all(|subscript| subscript.span() == span));
        Ok(())
    }

    #[test]
    fn component_reference_with_scalar_indices_rejects_dummy_span() {
        let variable = dae::Variable {
            source_span: unspanned_discrete_update_test_span(),
            ..dae::Variable::new(
                rumoca_core::VarName::new("x"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            )
        };

        let err = discrete_target_scalar_span(&variable, unspanned_discrete_update_test_span())
            .expect_err("source-derived scalar target subscript requires provenance");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason().contains("discrete target scalar subscript"),
            "unexpected error: {err}"
        );
    }
}
