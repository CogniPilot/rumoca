use std::collections::VecDeque;

use super::helpers::{dims_scalar_count, static_subscript_indices};
use super::{LowerError, compile_time, expression_rows};
use indexmap::{IndexMap, IndexSet};
use rumoca_core::ExpressionRewriter;
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{LinearOp, VarLayout};

#[derive(Debug, Clone)]
struct DiscreteAliasEdge {
    lhs: rumoca_core::VarName,
    rhs: rumoca_core::VarName,
    source: dae::Equation,
}

pub(crate) fn normalized_discrete_update_equations(dae_model: &dae::Dae) -> Vec<dae::Equation> {
    let protected_lhs_names = protected_discrete_update_lhs_names(dae_model);
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
        );
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
        );
    }
    normalized_equations.extend(oriented_discrete_alias_equations(
        &alias_edges,
        &protected_lhs_names,
    ));
    normalized_equations
}

pub(crate) fn initial_condition_update_equations(dae_model: &dae::Dae) -> Vec<dae::Equation> {
    dae_model
        .conditions
        .equations
        .iter()
        .filter(|eq| {
            eq.lhs
                .as_ref()
                .is_some_and(|lhs| is_condition_memory_lhs(dae_model, lhs))
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
                structural_bindings: Some(&structural_bindings),
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
) {
    let rewritten = rewrite_discrete_update_equation(dae_model, eq);
    if let Some(expanded) = expand_tuple_function_residual(dae_model, &rewritten) {
        for expanded_eq in expanded {
            collect_normalized_discrete_update_equation(
                dae_model,
                protected_lhs_names,
                occupied_lhs_names,
                normalized_equations,
                alias_edges,
                &expanded_eq,
                skip_untargeted_condition_row,
            );
        }
        return;
    }

    if let Some(edges) = explicit_discrete_alias_edges(dae_model, &rewritten) {
        alias_edges.extend(edges);
        return;
    }

    let normalized = normalize_discrete_update_equation(
        dae_model,
        &rewritten,
        protected_lhs_names,
        occupied_lhs_names,
    );
    if normalized.lhs.is_none() && skip_untargeted_condition_row {
        // MLS Appendix B: relation and clock root rows in f_c detect events,
        // while only condition-memory rows with assignment targets update
        // discrete state during event iteration.
        return;
    }
    if let Some(lhs) = normalized.lhs.as_ref() {
        occupied_lhs_names.extend(discrete_update_lhs_names(
            dae_model,
            lhs,
            normalized.scalar_count.max(1),
        ));
    }
    normalized_equations.push(normalized);
}

fn expand_tuple_function_residual(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
) -> Option<Vec<dae::Equation>> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        span,
    } = &eq.rhs
    else {
        return None;
    };
    let rumoca_core::Expression::Tuple { elements, .. } = lhs.as_ref() else {
        return None;
    };
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor,
        ..
    } = rhs.as_ref()
    else {
        return None;
    };
    let function = dae_model.symbols.functions.get(name.var_name())?;
    if elements.len() != function.outputs.len() {
        return None;
    }

    let mut equations = Vec::new();
    for (target_expr, output) in elements.iter().zip(&function.outputs) {
        let target_names = tuple_assignment_target_names(dae_model, target_expr, output)?;
        for (idx, target_name) in target_names.into_iter().enumerate() {
            equations.push(dae::Equation {
                lhs: Some(target_name),
                rhs: rumoca_core::Expression::FunctionCall {
                    name: projected_function_output_name(name.var_name(), output, idx)?.into(),
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
    Some(equations)
}

fn tuple_assignment_target_names(
    dae_model: &dae::Dae,
    target_expr: &rumoca_core::Expression,
    output: &rumoca_core::FunctionParam,
) -> Option<Vec<rumoca_core::VarName>> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = target_expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        if dims_scalar_count(&output.dims) != 1 {
            return None;
        }
        let indices = static_subscript_indices(subscripts).ok().flatten()?;
        return Some(vec![rumoca_core::VarName::new(dae::format_subscript_key(
            name.as_str(),
            &indices,
        ))]);
    }
    let output_count = tuple_assignment_output_count(dae_model, name, output);
    if output_count == 1 {
        return Some(vec![name.var_name().clone()]);
    }
    Some(discrete_update_scalar_names(
        dae_model,
        name.var_name(),
        output_count,
    ))
}

fn tuple_assignment_output_count(
    dae_model: &dae::Dae,
    target_name: &rumoca_core::Reference,
    output: &rumoca_core::FunctionParam,
) -> usize {
    if output.dims.iter().any(|dim| *dim <= 0) {
        // MLS §12.4.3: for a tuple assignment, the left-hand target and the
        // corresponding output component determine the same value shape. DAE
        // uses 0 for symbolic function-output dimensions, so use the concrete
        // target declaration once lowering has scalarized the assignment.
        return discrete_update_dims(dae_model, target_name.var_name())
            .map(dims_scalar_count)
            .unwrap_or(1);
    }
    dims_scalar_count(&output.dims)
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

fn rewrite_discrete_update_equation(dae_model: &dae::Dae, eq: &dae::Equation) -> dae::Equation {
    let mut rewritten = eq.clone();
    rewritten.rhs = rewrite_relation_event_memory_expr(dae_model, &rewritten.rhs);
    rewritten
}

fn normalize_discrete_update_equation(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
    occupied_lhs_names: &IndexSet<rumoca_core::VarName>,
) -> dae::Equation {
    let mut normalized = eq.clone();
    if let Some(lhs) = normalized.lhs.as_ref()
        && let Some(alias_target) = plain_discrete_target_name(dae_model, &normalized.rhs)
        && should_reorient_discrete_alias(
            dae_model,
            lhs,
            &alias_target,
            normalized.scalar_count.max(1),
            protected_lhs_names,
            occupied_lhs_names,
        )
    {
        let old_lhs = lhs.clone();
        normalized.lhs = Some(alias_target);
        normalized.rhs = rumoca_core::Expression::VarRef {
            name: old_lhs.into(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };
        return normalized;
    }
    if normalized.lhs.is_none()
        && let Some((lhs, rhs)) = extract_residual_discrete_assignment(
            dae_model,
            &normalized.rhs,
            normalized.scalar_count.max(1),
            protected_lhs_names,
            occupied_lhs_names,
        )
    {
        normalized.lhs = Some(lhs);
        normalized.rhs = rhs;
    }
    normalized
}

fn explicit_discrete_alias_edges(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
) -> Option<Vec<DiscreteAliasEdge>> {
    let lhs = eq.lhs.as_ref()?;
    let rhs = plain_discrete_target_name(dae_model, &eq.rhs)?;
    discrete_alias_edges(dae_model, lhs, &rhs, eq.scalar_count.max(1), eq)
}

fn discrete_alias_edges(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    rhs: &rumoca_core::VarName,
    scalar_count: usize,
    source: &dae::Equation,
) -> Option<Vec<DiscreteAliasEdge>> {
    let lhs_names = discrete_update_scalar_names(dae_model, lhs, scalar_count);
    let rhs_names = discrete_update_scalar_names(dae_model, rhs, scalar_count);
    if lhs_names.len() != rhs_names.len() {
        return None;
    }
    Some(
        lhs_names
            .into_iter()
            .zip(rhs_names)
            .filter(|(lhs, rhs)| lhs != rhs)
            .map(|(lhs, rhs)| DiscreteAliasEdge {
                lhs,
                rhs,
                source: source.clone(),
            })
            .collect(),
    )
}

fn oriented_discrete_alias_equations(
    edges: &[DiscreteAliasEdge],
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
) -> Vec<dae::Equation> {
    let mut adjacency: IndexMap<rumoca_core::VarName, Vec<(rumoca_core::VarName, usize)>> =
        IndexMap::new();
    let mut ordered_names = Vec::new();
    let mut seen_names = IndexSet::new();
    for (idx, edge) in edges.iter().enumerate() {
        for name in [&edge.lhs, &edge.rhs] {
            if seen_names.insert(name.clone()) {
                ordered_names.push(name.clone());
            }
        }
        adjacency
            .entry(edge.lhs.clone())
            .or_default()
            .push((edge.rhs.clone(), idx));
        adjacency
            .entry(edge.rhs.clone())
            .or_default()
            .push((edge.lhs.clone(), idx));
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
            dae::Equation {
                lhs: Some(child),
                rhs: rumoca_core::Expression::VarRef {
                    name: parent.into(),
                    subscripts: Vec::new(),
                    span: rumoca_core::Span::DUMMY,
                },
                span: edge.source.span,
                origin: edge.source.origin.clone(),
                scalar_count: 1,
            }
        })
        .collect()
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

fn protected_discrete_update_lhs_names(dae_model: &dae::Dae) -> IndexSet<rumoca_core::VarName> {
    let mut protected = IndexSet::new();
    for eq in dae_model
        .discrete
        .real_updates
        .iter()
        .chain(dae_model.discrete.valued_updates.iter())
        .chain(dae_model.conditions.equations.iter())
    {
        if let Some(lhs) = eq.lhs.as_ref() {
            if is_plain_discrete_alias_rhs(dae_model, &eq.rhs) {
                continue;
            }
            for name in discrete_update_lhs_names(dae_model, lhs, eq.scalar_count.max(1)) {
                protected.insert(name);
            }
        } else if let Some(target) = residual_non_alias_update_target(dae_model, &eq.rhs) {
            protected.insert(target);
        }
    }
    protected
}

fn residual_non_alias_update_target(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> Option<rumoca_core::VarName> {
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => binary_residual_non_alias_update_target(dae_model, lhs, rhs),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => if_residual_non_alias_update_target(dae_model, branches, else_branch),
        _ => None,
    }
}

fn binary_residual_non_alias_update_target(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
) -> Option<rumoca_core::VarName> {
    match (
        plain_discrete_target_name(dae_model, lhs),
        plain_discrete_target_name(dae_model, rhs),
    ) {
        (Some(target), None) | (None, Some(target)) => Some(target),
        (Some(_), Some(_)) | (None, None) => None,
    }
}

fn if_residual_non_alias_update_target(
    dae_model: &dae::Dae,
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
) -> Option<rumoca_core::VarName> {
    let mut common_target = None;
    for (_, value) in branches {
        let target = residual_non_alias_update_target(dae_model, value)?;
        update_common_discrete_target(&mut common_target, target)?;
    }
    let target = residual_non_alias_update_target(dae_model, else_branch)?;
    update_common_discrete_target(&mut common_target, target)?;
    common_target
}

fn discrete_update_lhs_names(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    scalar_count: usize,
) -> Vec<rumoca_core::VarName> {
    let scalar_names = discrete_update_scalar_names(dae_model, lhs, scalar_count);
    if scalar_count <= 1 {
        return scalar_names;
    }
    let mut names = Vec::with_capacity(scalar_names.len() + 1);
    names.push(lhs.clone());
    names.extend(scalar_names);
    names
}

fn discrete_update_scalar_names(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    scalar_count: usize,
) -> Vec<rumoca_core::VarName> {
    if scalar_count <= 1 {
        return vec![lhs.clone()];
    }
    let dims = discrete_update_dims(dae_model, lhs).unwrap_or(&[]);
    (0..scalar_count)
        .map(|idx| {
            rumoca_core::VarName::new(dae::scalar_name_text_for_flat_index(
                lhs.as_str(),
                dims,
                idx,
            ))
        })
        .collect()
}

fn is_plain_discrete_alias_rhs(dae_model: &dae::Dae, rhs: &rumoca_core::Expression) -> bool {
    plain_discrete_target_name(dae_model, rhs).is_some()
}

fn should_reorient_discrete_alias(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    alias_target: &rumoca_core::VarName,
    scalar_count: usize,
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
    occupied_lhs_names: &IndexSet<rumoca_core::VarName>,
) -> bool {
    let lhs_names = discrete_update_lhs_names(dae_model, lhs, scalar_count);
    let target_names = discrete_update_lhs_names(dae_model, alias_target, scalar_count);
    let lhs_protected = any_name_in_set(&lhs_names, protected_lhs_names);
    let target_protected = any_name_in_set(&target_names, protected_lhs_names);
    if lhs_protected != target_protected {
        return lhs_protected;
    }

    // MLS §8.3: alias equations are simultaneous equations, not ordered
    // assignments. Choose an orientation that keeps the solve-IR update target
    // set single-writer when either side of the alias is still available.
    let lhs_occupied = any_name_in_set(&lhs_names, occupied_lhs_names);
    let target_occupied = any_name_in_set(&target_names, occupied_lhs_names);
    lhs_occupied && !target_occupied
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
) -> Option<(rumoca_core::VarName, rumoca_core::Expression)> {
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => extract_binary_residual_discrete_assignment(
            dae_model,
            lhs,
            rhs,
            scalar_count,
            protected_lhs_names,
            occupied_lhs_names,
        ),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => extract_if_residual_discrete_assignment(
            dae_model,
            branches,
            else_branch,
            scalar_count,
            protected_lhs_names,
            occupied_lhs_names,
        ),
        _ => None,
    }
}

fn extract_binary_residual_discrete_assignment(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    scalar_count: usize,
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
    occupied_lhs_names: &IndexSet<rumoca_core::VarName>,
) -> Option<(rumoca_core::VarName, rumoca_core::Expression)> {
    let lhs_target = plain_discrete_target_name(dae_model, lhs);
    let rhs_target = plain_discrete_target_name(dae_model, rhs);
    match (lhs_target, rhs_target) {
        (Some(lhs_target), Some(rhs_target)) => {
            if should_reorient_discrete_alias(
                dae_model,
                &lhs_target,
                &rhs_target,
                scalar_count,
                protected_lhs_names,
                occupied_lhs_names,
            ) {
                Some((rhs_target, lhs.clone()))
            } else {
                Some((lhs_target, rhs.clone()))
            }
        }
        (Some(lhs_target), _) => Some((lhs_target, rhs.clone())),
        (None, Some(rhs_target)) => Some((rhs_target, lhs.clone())),
        (None, None) => None,
    }
}

fn extract_if_residual_discrete_assignment(
    dae_model: &dae::Dae,
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    scalar_count: usize,
    protected_lhs_names: &IndexSet<rumoca_core::VarName>,
    occupied_lhs_names: &IndexSet<rumoca_core::VarName>,
) -> Option<(rumoca_core::VarName, rumoca_core::Expression)> {
    let mut common_target = None;
    let mut rhs_branches = Vec::with_capacity(branches.len());
    for (condition, value) in branches {
        let (target, rhs) = extract_residual_discrete_assignment(
            dae_model,
            value,
            scalar_count,
            protected_lhs_names,
            occupied_lhs_names,
        )?;
        update_common_discrete_target(&mut common_target, target)?;
        rhs_branches.push((condition.clone(), rhs));
    }

    let (target, else_rhs) = extract_residual_discrete_assignment(
        dae_model,
        else_branch,
        scalar_count,
        protected_lhs_names,
        occupied_lhs_names,
    )?;
    update_common_discrete_target(&mut common_target, target)?;
    let target = common_target?;

    Some((
        target,
        rumoca_core::Expression::If {
            branches: rhs_branches,
            else_branch: Box::new(else_rhs),
            span: rumoca_core::Span::DUMMY,
        },
    ))
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

fn plain_discrete_target_name(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> Option<rumoca_core::VarName> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() || !is_discrete_update_name(dae_model, name.var_name()) {
        return None;
    }
    Some(name.var_name().clone())
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

fn discrete_update_dims<'a>(
    dae_model: &'a dae::Dae,
    lhs: &rumoca_core::VarName,
) -> Option<&'a [i64]> {
    dae_model
        .variables
        .inputs
        .get(lhs)
        .or_else(|| dae_model.variables.discrete_reals.get(lhs))
        .or_else(|| dae_model.variables.discrete_valued.get(lhs))
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
) -> rumoca_core::Expression {
    RelationEventMemoryRewriter { dae_model }.rewrite_expression(expr)
}

struct RelationEventMemoryRewriter<'a> {
    dae_model: &'a dae::Dae,
}

impl ExpressionRewriter for RelationEventMemoryRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        let rumoca_core::Expression::BuiltinCall {
            function,
            args,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        if *function == rumoca_core::BuiltinFunction::Pre
            && args.len() == 1
            && let Some(memory) = relation_memory_expr_for_condition(self.dae_model, &args[0])
        {
            // MLS §3.7.5 and §8.6: pre(relation(v)) reads the relation memory
            // from the event-entry left limit, not the live value being settled.
            return pre_relation_memory_expr(memory, *span);
        }
        if *function == rumoca_core::BuiltinFunction::Edge
            && args.len() == 1
            && let Some(memory) = relation_memory_expr_for_condition(self.dae_model, &args[0])
        {
            return self.relation_edge_expr(memory, *span);
        }
        self.walk_expression(expr)
    }
}

impl RelationEventMemoryRewriter<'_> {
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
) -> Option<rumoca_core::Expression> {
    let relation_idx = dae_model
        .conditions
        .relations
        .iter()
        .position(|relation| relation == condition)?;
    let mut offset = 0usize;
    for eq in &dae_model.conditions.equations {
        let scalar_count = eq.scalar_count.max(1);
        if relation_idx < offset + scalar_count {
            let lhs = eq.lhs.as_ref()?;
            return Some(relation_memory_scalar_expr(
                dae_model,
                lhs,
                relation_idx - offset,
                scalar_count,
                eq.span,
            ));
        }
        offset += scalar_count;
    }
    None
}

fn relation_memory_scalar_expr(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    flat_index: usize,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    if scalar_count <= 1 {
        return rumoca_core::Expression::VarRef {
            name: lhs.clone().into(),
            subscripts: Vec::new(),
            span,
        };
    }
    let dims = dae_model
        .variables
        .discrete_valued
        .get(lhs)
        .or_else(|| dae_model.variables.discrete_reals.get(lhs))
        .map(|var| var.dims.as_slice())
        .unwrap_or(&[]);
    let subscripts = dae::flat_index_to_subscripts(dims, flat_index)
        .unwrap_or_else(|| vec![flat_index.saturating_add(1)])
        .into_iter()
        .map(|index| rumoca_core::Subscript::generated_index(index as i64, span))
        .collect();
    rumoca_core::Expression::VarRef {
        name: lhs.clone().into(),
        subscripts,
        span,
    }
}
