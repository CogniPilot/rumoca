use std::collections::{HashMap, HashSet};

use crate::runtime::assignment::{canonical_var_ref_key, direct_assignment_from_equation};
use rumoca_eval_dae::runtime::VarEnv;
use rumoca_ir_dae as dae;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AliasConsensus {
    Missing,
    Inconsistent,
    Value(f64),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AliasPropagationOutcome {
    Missing,
    Inconsistent,
    Applied { updates: usize, anchor_value: f64 },
}

#[derive(Debug, Clone, PartialEq)]
pub struct DiscreteAliasUpdate {
    pub dst: String,
    pub old_value: f64,
    pub new_value: f64,
    pub lhs: String,
    pub rhs: String,
    pub origin: String,
}

pub fn runtime_assignment_equations<'a>(
    dae_model: &'a dae::Dae,
    n_x: usize,
) -> impl Iterator<Item = &'a dae::Equation> + 'a {
    dae_model
        .f_x
        .iter()
        .skip(n_x)
        .chain(dae_model.f_z.iter())
        .chain(dae_model.f_m.iter())
}

pub fn is_zero_literal(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::Literal(dae::Literal::Integer(0)) => true,
        dae::Expression::Literal(dae::Literal::Real(v)) => v.abs() <= f64::EPSILON,
        _ => false,
    }
}

pub fn build_runtime_alias_adjacency(
    dae_model: &dae::Dae,
    n_x: usize,
    is_known_assignment_name: &impl Fn(&str) -> bool,
) -> HashMap<String, Vec<String>> {
    let mut adjacency: HashMap<String, Vec<String>> = HashMap::new();
    for eq in runtime_assignment_equations(dae_model, n_x) {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let Some((target, solution)) = direct_assignment_from_equation(eq) else {
            continue;
        };
        let dae::Expression::VarRef {
            name: source,
            subscripts,
        } = solution
        else {
            continue;
        };
        let Some(source_key) = canonical_var_ref_key(source, subscripts) else {
            continue;
        };
        if !is_known_assignment_name(target.as_str())
            || !is_known_assignment_name(source_key.as_str())
        {
            continue;
        }
        insert_runtime_alias_edges(
            dae_model,
            &mut adjacency,
            target.as_str(),
            source_key.as_str(),
        );
    }
    adjacency
}

pub fn build_runtime_alias_adjacency_with_known_assignments(
    dae_model: &dae::Dae,
    n_x: usize,
) -> HashMap<String, Vec<String>> {
    build_runtime_alias_adjacency(dae_model, n_x, &|name| {
        crate::runtime::assignment::is_known_assignment_name(dae_model, name)
    })
}

pub fn insert_name_and_base(set: &mut HashSet<String>, name: &str) {
    set.insert(name.to_string());
    if let Some(base) = dae::component_base_name(name)
        && base != name
    {
        set.insert(base);
    }
}

fn lookup_known_assignment_variable<'a>(
    dae_model: &'a dae::Dae,
    name: &str,
) -> Option<&'a dae::Variable> {
    let base = dae::component_base_name(name).unwrap_or_else(|| name.to_string());
    dae_model
        .states
        .get(&dae::VarName::new(base.clone()))
        .or_else(|| dae_model.algebraics.get(&dae::VarName::new(base.clone())))
        .or_else(|| {
            dae_model
                .discrete_reals
                .get(&dae::VarName::new(base.clone()))
        })
        .or_else(|| {
            dae_model
                .discrete_valued
                .get(&dae::VarName::new(base.clone()))
        })
        .or_else(|| dae_model.inputs.get(&dae::VarName::new(base.clone())))
        .or_else(|| dae_model.outputs.get(&dae::VarName::new(base.clone())))
        .or_else(|| dae_model.parameters.get(&dae::VarName::new(base.clone())))
        .or_else(|| dae_model.constants.get(&dae::VarName::new(base)))
}

fn array_alias_linear_size(dae_model: &dae::Dae, lhs: &str, rhs: &str) -> Option<usize> {
    if lhs.contains('[') || rhs.contains('[') {
        return None;
    }

    let lhs_var = lookup_known_assignment_variable(dae_model, lhs)?;
    let rhs_var = lookup_known_assignment_variable(dae_model, rhs)?;
    if lhs_var.dims != rhs_var.dims || lhs_var.dims.is_empty() {
        return None;
    }

    lhs_var.dims.iter().try_fold(1usize, |acc, &dim| {
        let width = usize::try_from(dim).ok()?;
        acc.checked_mul(width)
    })
}

fn expand_array_anchor_names(dae_model: &dae::Dae, anchors: &mut HashSet<String>) {
    let existing: Vec<String> = anchors.iter().cloned().collect();
    for name in existing {
        if name.contains('[') {
            continue;
        }
        let has_explicit_indexed_anchor = anchors.iter().any(|candidate| {
            candidate != &name
                && candidate.contains('[')
                && dae::component_base_name(candidate.as_str()).is_some_and(|base| base == name)
        });
        if has_explicit_indexed_anchor {
            continue;
        }
        let Some(var) = lookup_known_assignment_variable(dae_model, name.as_str()) else {
            continue;
        };
        if var.dims.is_empty() {
            continue;
        }
        let Some(total) = var.dims.iter().try_fold(1usize, |acc, &dim| {
            let width = usize::try_from(dim).ok()?;
            acc.checked_mul(width)
        }) else {
            continue;
        };
        if total <= 1 {
            continue;
        }
        for index in 1..=total {
            anchors.insert(format!("{name}[{index}]"));
        }
    }
}

fn insert_alias_edge(adjacency: &mut HashMap<String, Vec<String>>, lhs: String, rhs: String) {
    if lhs == rhs {
        return;
    }
    adjacency.entry(lhs.clone()).or_default().push(rhs.clone());
    adjacency.entry(rhs).or_default().push(lhs);
}

fn insert_runtime_alias_edges(
    dae_model: &dae::Dae,
    adjacency: &mut HashMap<String, Vec<String>>,
    target: &str,
    source: &str,
) {
    // MLS §9.2 / §10.1: array connector equalities preserve per-element
    // equality. Expand `x = G1.x` into elementwise alias edges so runtime
    // propagation follows the actual array members, not just the base name.
    if let Some(total) = array_alias_linear_size(dae_model, target, source).filter(|&n| n > 1) {
        insert_alias_edge(adjacency, target.to_string(), format!("{target}[1]"));
        insert_alias_edge(adjacency, source.to_string(), format!("{source}[1]"));
        for index in 1..=total {
            insert_alias_edge(
                adjacency,
                format!("{target}[{index}]"),
                format!("{source}[{index}]"),
            );
        }
        return;
    }

    insert_alias_edge(adjacency, target.to_string(), source.to_string());
}

fn collect_explicit_array_alias_values(
    dae_model: &dae::Dae,
    env: &VarEnv<f64>,
    source: &str,
    dst: &str,
) -> Option<(Vec<i64>, Vec<f64>)> {
    if source.contains('[') || dst.contains('[') {
        return None;
    }

    let source_var = lookup_known_assignment_variable(dae_model, source)?;
    let dst_var = lookup_known_assignment_variable(dae_model, dst)?;
    if source_var.dims != dst_var.dims || source_var.dims.is_empty() {
        return None;
    }

    let total = source_var.dims.iter().try_fold(1usize, |acc, &dim| {
        let width = usize::try_from(dim).ok()?;
        acc.checked_mul(width)
    })?;
    if total <= 1 {
        return None;
    }

    let mut values = Vec::with_capacity(total);
    for idx in 0..total {
        let key = format!("{source}[{}]", idx + 1);
        values.push(env.vars.get(key.as_str()).copied()?);
    }
    Some((source_var.dims.clone(), values))
}

fn apply_alias_update(
    dae_model: &dae::Dae,
    env: &mut VarEnv<f64>,
    explicit_updates: &mut HashSet<String>,
    dst: &str,
    source: &str,
    scalar_value: f64,
) -> usize {
    if let Some((dims, values)) = collect_explicit_array_alias_values(dae_model, env, source, dst) {
        let mut staged = VarEnv::new();
        rumoca_eval_dae::runtime::set_array_entries(&mut staged, dst, &dims, &values);
        let mut applied = 0usize;
        for (name, value) in staged.vars {
            if env
                .vars
                .get(name.as_str())
                .is_none_or(|existing| (existing - value).abs() > 1.0e-12)
            {
                env.set(name.as_str(), value);
                insert_name_and_base(explicit_updates, name.as_str());
                applied += 1;
            }
        }
        if applied > 0 {
            insert_name_and_base(explicit_updates, dst);
        }
        return applied;
    }

    let old_value = env.vars.get(dst).copied().unwrap_or(0.0);
    if (old_value - scalar_value).abs() <= 1.0e-12 {
        return 0;
    }
    env.set(dst, scalar_value);
    insert_name_and_base(explicit_updates, dst);
    1
}

fn collect_non_alias_assignment_targets_from_expr(
    dae_model: &dae::Dae,
    expr: &dae::Expression,
    targets: &mut HashSet<String>,
) {
    if let Some(tuple_assignment) =
        crate::runtime::tuple::extract_direct_tuple_function_assignment(expr)
    {
        for target in tuple_assignment.targets {
            insert_name_and_base(targets, target.key.as_str());
        }
        return;
    }

    if let Some((target, solution)) = crate::runtime::assignment::extract_direct_assignment(expr) {
        if !crate::runtime::assignment::assignment_solution_is_alias_varref(dae_model, solution) {
            insert_name_and_base(targets, target.as_str());
        }
        return;
    }

    match expr {
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => collect_non_alias_assignment_targets_from_expr(dae_model, rhs, targets),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (_condition, value) in branches {
                collect_non_alias_assignment_targets_from_expr(dae_model, value, targets);
            }
            collect_non_alias_assignment_targets_from_expr(dae_model, else_branch, targets);
        }
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            if is_zero_literal(lhs) {
                collect_non_alias_assignment_targets_from_expr(dae_model, rhs, targets);
            } else if is_zero_literal(rhs) {
                collect_non_alias_assignment_targets_from_expr(dae_model, lhs, targets);
            }
        }
        _ => {}
    }
}

fn collect_structural_assignment_targets(expr: &dae::Expression, targets: &mut HashSet<String>) {
    if let Some(tuple_assignment) =
        crate::runtime::tuple::extract_direct_tuple_function_assignment(expr)
    {
        for target in tuple_assignment.targets {
            targets.insert(target.key);
        }
        return;
    }

    if let Some((target, _solution)) = crate::runtime::assignment::extract_direct_assignment(expr) {
        targets.insert(target);
        return;
    }

    match expr {
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => collect_structural_assignment_targets(rhs, targets),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (_condition, value) in branches {
                collect_structural_assignment_targets(value, targets);
            }
            collect_structural_assignment_targets(else_branch, targets);
        }
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            if is_zero_literal(lhs) {
                collect_structural_assignment_targets(rhs, targets);
            } else if is_zero_literal(rhs) {
                collect_structural_assignment_targets(lhs, targets);
            }
        }
        _ => {}
    }
}

fn collect_explicit_runtime_targets(dae_model: &dae::Dae) -> HashSet<String> {
    let mut targets = HashSet::new();

    // Include discrete partition assignment targets (f_z/f_m), including
    // targets nested under If-rewritten residual branches.
    let mut partition_targets = HashSet::new();
    for eq in dae_model.f_z.iter().chain(dae_model.f_m.iter()) {
        if let Some(lhs) = eq.lhs.as_ref() {
            partition_targets.insert(lhs.as_str().to_string());
        }
        collect_structural_assignment_targets(&eq.rhs, &mut partition_targets);
    }
    for target in partition_targets {
        insert_name_and_base(&mut targets, target.as_str());
    }

    targets
}

pub fn collect_non_alias_runtime_assignment_targets(
    dae_model: &dae::Dae,
    n_x: usize,
) -> HashSet<String> {
    let mut targets = HashSet::new();
    for eq in runtime_assignment_equations(dae_model, n_x) {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        if let Some((target, solution)) =
            crate::runtime::assignment::direct_assignment_from_equation(eq)
        {
            if !crate::runtime::assignment::assignment_solution_is_alias_varref(dae_model, solution)
            {
                insert_name_and_base(&mut targets, target.as_str());
            }
            continue;
        }
        collect_non_alias_assignment_targets_from_expr(dae_model, &eq.rhs, &mut targets);
    }
    targets
}

pub fn collect_runtime_alias_anchor_names(dae_model: &dae::Dae, n_x: usize) -> HashSet<String> {
    let mut anchors = collect_explicit_runtime_targets(dae_model);
    let non_alias_targets = collect_non_alias_runtime_assignment_targets(dae_model, n_x);
    let mut alias_members: HashSet<String> = HashSet::new();

    for eq in runtime_assignment_equations(dae_model, n_x) {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let Some((target, solution)) =
            crate::runtime::assignment::direct_assignment_from_equation(eq)
        else {
            continue;
        };
        if let dae::Expression::VarRef {
            name: source,
            subscripts,
        } = solution
        {
            if crate::runtime::assignment::is_known_assignment_name(dae_model, target.as_str()) {
                insert_name_and_base(&mut alias_members, target.as_str());
            }
            if let Some(source_key) =
                crate::runtime::assignment::canonical_var_ref_key(source, subscripts)
                && crate::runtime::assignment::is_known_assignment_name(
                    dae_model,
                    source_key.as_str(),
                )
            {
                insert_name_and_base(&mut alias_members, source_key.as_str());
            }
            if crate::runtime::assignment::is_known_assignment_name(dae_model, target.as_str())
                && crate::runtime::assignment::canonical_var_ref_key(source, subscripts)
                    .is_some_and(|source_key| {
                        crate::runtime::assignment::is_known_assignment_name(
                            dae_model,
                            source_key.as_str(),
                        )
                    })
            {
                continue;
            }
        }
        insert_name_and_base(&mut anchors, target.as_str());
    }

    // Alias-only partition targets (from f_z/f_m) are equalities, not anchor
    // sources. Keep alias members only when they have a non-alias assignment
    // definition.
    anchors.retain(|name| !alias_members.contains(name) || non_alias_targets.contains(name));

    let adjacency = build_runtime_alias_adjacency_with_known_assignments(dae_model, n_x);
    let mut visited = HashSet::new();
    let mut component_has_anchor = HashMap::new();
    for node in adjacency.keys() {
        if !visited.insert(node.clone()) {
            continue;
        }
        let component = collect_alias_component(node, &adjacency, &mut visited);
        let has_anchor = component.iter().any(|name| anchors.contains(name));
        for name in component {
            component_has_anchor.insert(name, has_anchor);
        }
    }

    for name in &alias_members {
        // MLS §8 equality semantics: when an alias component already has a
        // non-alias runtime anchor, keep alias-only peer names out of the
        // anchor set so stale connector/input aliases cannot overwrite the
        // defining right-hand side during runtime capture.
        if !crate::runtime::assignment::is_runtime_unknown_name(dae_model, name.as_str())
            && !component_has_anchor.get(name).copied().unwrap_or(false)
        {
            insert_name_and_base(&mut anchors, name.as_str());
        }
    }

    for name in rumoca_eval_dae::analysis::runtime_defined_unknown_names(dae_model) {
        if alias_members.contains(name.as_str()) {
            continue;
        }
        insert_name_and_base(&mut anchors, name.as_str());
    }

    expand_array_anchor_names(dae_model, &mut anchors);
    anchors
}

pub fn collect_alias_component(
    start: &str,
    adjacency: &HashMap<String, Vec<String>>,
    visited: &mut HashSet<String>,
) -> Vec<String> {
    let mut stack = vec![start.to_string()];
    let mut component = Vec::new();
    while let Some(current) = stack.pop() {
        component.push(current.clone());
        enqueue_unvisited_neighbors(&current, adjacency, visited, &mut stack);
    }
    component.sort();
    component
}

fn enqueue_unvisited_neighbors(
    current: &str,
    adjacency: &HashMap<String, Vec<String>>,
    visited: &mut HashSet<String>,
    stack: &mut Vec<String>,
) {
    let Some(neighbors) = adjacency.get(current) else {
        return;
    };
    for neighbor in neighbors {
        if !visited.insert(neighbor.clone()) {
            continue;
        }
        stack.push(neighbor.clone());
    }
}

pub fn runtime_alias_consensus_value(
    component: &[String],
    env: &VarEnv<f64>,
    is_runtime_defined: &impl Fn(&str) -> bool,
) -> AliasConsensus {
    let runtime_values: Vec<f64> = component
        .iter()
        .filter(|name| is_runtime_defined(name.as_str()))
        .filter_map(|name| env.vars.get(name).copied())
        .filter(|v| v.is_finite())
        .collect();
    if runtime_values.is_empty() {
        return AliasConsensus::Missing;
    }

    let anchor_value = runtime_values.iter().sum::<f64>() / runtime_values.len() as f64;
    let tol = 1.0e-9 * (1.0 + anchor_value.abs());
    if runtime_values
        .iter()
        .any(|value| (value - anchor_value).abs() > tol)
    {
        return AliasConsensus::Inconsistent;
    }
    AliasConsensus::Value(anchor_value)
}

pub fn apply_alias_component_anchor(
    component: &[String],
    anchor_value: f64,
    y: &mut [f64],
    n_x: usize,
    env: &mut VarEnv<f64>,
    name_to_idx: &HashMap<String, usize>,
) -> usize {
    let mut updates = 0usize;
    for name in component {
        let mut changed = false;

        if let Some(idx) = crate::runtime::layout::solver_idx_for_target(name, name_to_idx)
            && idx >= n_x
            && idx < y.len()
            && (y[idx] - anchor_value).abs() > 1e-12
        {
            y[idx] = anchor_value;
            changed = true;
        }

        let env_changed = env
            .vars
            .get(name)
            .is_none_or(|value| (value - anchor_value).abs() > 1e-12);
        if env_changed {
            env.set(name, anchor_value);
            changed = true;
        }

        if changed {
            updates += 1;
        }
    }
    updates
}

pub fn propagate_alias_component_from_env(
    component: &[String],
    env: &mut VarEnv<f64>,
    y: &mut [f64],
    n_x: usize,
    name_to_idx: &HashMap<String, usize>,
    is_runtime_anchor: &impl Fn(&str) -> bool,
) -> AliasPropagationOutcome {
    match runtime_alias_consensus_value(component, env, is_runtime_anchor) {
        AliasConsensus::Missing => AliasPropagationOutcome::Missing,
        AliasConsensus::Inconsistent => AliasPropagationOutcome::Inconsistent,
        AliasConsensus::Value(anchor_value) => AliasPropagationOutcome::Applied {
            updates: apply_alias_component_anchor(
                component,
                anchor_value,
                y,
                n_x,
                env,
                name_to_idx,
            ),
            anchor_value,
        },
    }
}

pub fn propagate_discrete_alias_equalities(
    dae_model: &dae::Dae,
    env: &mut VarEnv<f64>,
    explicit_updates: &mut HashSet<String>,
    mut on_update: impl FnMut(&DiscreteAliasUpdate),
) -> bool {
    let allow_discrete_bias = !explicit_updates.is_empty();
    let non_alias_runtime_targets = collect_non_alias_runtime_assignment_targets(dae_model, 0);
    let mut changed_any = false;
    let max_passes = (dae_model.f_z.len() + dae_model.f_m.len() + dae_model.f_x.len()).clamp(1, 64);
    for _ in 0..max_passes {
        let mut changed_pass = false;
        for eq in dae_model
            .f_z
            .iter()
            .chain(dae_model.f_m.iter())
            .chain(dae_model.f_x.iter())
        {
            let Some((lhs, rhs)) =
                crate::runtime::assignment::extract_alias_pair_from_equation(dae_model, eq)
            else {
                continue;
            };
            let lhs_value = env.vars.get(lhs.as_str()).copied().unwrap_or(0.0);
            let rhs_value = env.vars.get(rhs.as_str()).copied().unwrap_or(0.0);
            if (lhs_value - rhs_value).abs() <= 1.0e-12 {
                continue;
            }

            let lhs_explicit = explicit_updates.contains(lhs.as_str());
            let rhs_explicit = explicit_updates.contains(rhs.as_str());
            let direction = if lhs_explicit && !rhs_explicit {
                Some((rhs.as_str(), lhs_value))
            } else if rhs_explicit && !lhs_explicit {
                Some((lhs.as_str(), rhs_value))
            } else if eq.lhs.is_some()
                && !lhs_explicit
                && crate::runtime::assignment::is_runtime_unknown_name(dae_model, rhs.as_str())
                && !crate::runtime::assignment::is_runtime_unknown_name(dae_model, lhs.as_str())
            {
                // Preserve explicit equation direction for connector-style
                // aliases such as `assignClock1.u[i] = add.y` during event
                // settle. Those are not undirected equalities; the LHS should
                // materialize the current RHS value.
                Some((lhs.as_str(), rhs_value))
            } else if allow_discrete_bias
                && crate::runtime::assignment::is_discrete_name(dae_model, lhs.as_str())
                && !crate::runtime::assignment::is_discrete_name(dae_model, rhs.as_str())
            {
                Some((rhs.as_str(), lhs_value))
            } else if allow_discrete_bias
                && crate::runtime::assignment::is_discrete_name(dae_model, rhs.as_str())
                && !crate::runtime::assignment::is_discrete_name(dae_model, lhs.as_str())
            {
                Some((lhs.as_str(), rhs_value))
            } else {
                None
            };
            let Some((dst, value)) = direction else {
                continue;
            };
            let source = if dst == lhs {
                rhs.as_str()
            } else {
                lhs.as_str()
            };
            if non_alias_runtime_targets.contains(dst)
                && !non_alias_runtime_targets.contains(source)
            {
                continue;
            }
            let old_value = env.vars.get(dst).copied().unwrap_or(0.0);
            let updates = apply_alias_update(dae_model, env, explicit_updates, dst, source, value);
            if updates == 0 {
                continue;
            }
            changed_pass = true;
            changed_any = true;
            on_update(&DiscreteAliasUpdate {
                dst: dst.to_string(),
                old_value,
                new_value: value,
                lhs,
                rhs,
                origin: eq.origin.clone(),
            });
        }
        if !changed_pass {
            break;
        }
    }

    changed_any
}

pub fn collect_component_values(component: &[String], env: &VarEnv<f64>) -> Vec<String> {
    component
        .iter()
        .map(|name| {
            let value = env.vars.get(name).copied().unwrap_or(0.0);
            format!("{name}={value}")
        })
        .collect()
}

pub fn collect_component_anchor_values<F>(
    component: &[String],
    env: &VarEnv<f64>,
    is_runtime_anchor: &F,
) -> Vec<String>
where
    F: Fn(&str) -> bool,
{
    component
        .iter()
        .filter(|name| is_runtime_anchor(name.as_str()))
        .map(|name| {
            let value = env.vars.get(name).copied().unwrap_or(0.0);
            format!("{name}={value}")
        })
        .collect()
}

pub fn propagate_runtime_alias_components_from_env(
    dae_model: &dae::Dae,
    y: &mut [f64],
    n_x: usize,
    env: &mut VarEnv<f64>,
) -> usize {
    let ctx = build_runtime_alias_propagation_context(dae_model, y.len(), n_x);
    propagate_runtime_alias_components_from_env_with_context(&ctx, y, n_x, env)
}

pub(crate) struct RuntimeAliasPropagationContext {
    solver_maps: crate::runtime::layout::SolverNameIndexMaps,
    runtime_anchors: HashSet<String>,
    adjacency: HashMap<String, Vec<String>>,
}

pub(crate) fn build_runtime_alias_propagation_context(
    dae_model: &dae::Dae,
    y_len: usize,
    n_x: usize,
) -> RuntimeAliasPropagationContext {
    RuntimeAliasPropagationContext {
        solver_maps: crate::runtime::layout::build_solver_name_index_maps(dae_model, y_len),
        runtime_anchors: collect_runtime_alias_anchor_names(dae_model, n_x),
        adjacency: build_runtime_alias_adjacency_with_known_assignments(dae_model, n_x),
    }
}

pub(crate) fn propagate_runtime_alias_components_from_env_with_context(
    ctx: &RuntimeAliasPropagationContext,
    y: &mut [f64],
    n_x: usize,
    env: &mut VarEnv<f64>,
) -> usize {
    if ctx.adjacency.is_empty() {
        return 0;
    }

    let names = &ctx.solver_maps.names;
    let name_to_idx = &ctx.solver_maps.name_to_idx;
    let runtime_anchors = &ctx.runtime_anchors;
    let is_runtime_anchor = |name: &str| {
        runtime_anchors.contains(name)
            || (!name.contains('[')
                && crate::runtime::layout::solver_idx_for_target(name, name_to_idx)
                    .and_then(|idx| names.get(idx))
                    .is_some_and(|solver_name| runtime_anchors.contains(solver_name)))
    };

    let mut visited: HashSet<String> = HashSet::new();
    let mut updates = 0usize;
    for node in ctx.adjacency.keys() {
        if !visited.insert(node.clone()) {
            continue;
        }
        let component = collect_alias_component(node, &ctx.adjacency, &mut visited);
        let outcome = propagate_alias_component_from_env(
            &component,
            env,
            y,
            n_x,
            name_to_idx,
            &is_runtime_anchor,
        );
        if std::env::var_os("RUMOCA_DEBUG_DIGITAL_START").is_some()
            && component.iter().any(|name| {
                matches!(
                    name.as_str(),
                    "a.y" | "b.y" | "Adder.a" | "Adder.b" | "Enable.y" | "FF.j" | "FF.k" | "MUX.d"
                )
            })
        {
            let anchors = collect_component_anchor_values(&component, env, &is_runtime_anchor);
            let values = collect_component_values(&component, env);
            eprintln!(
                "DEBUG alias component={component:?} anchors={anchors:?} values={values:?} outcome={outcome:?}"
            );
        }
        if let AliasPropagationOutcome::Applied {
            updates: component_updates,
            ..
        } = outcome
        {
            updates += component_updates;
        }
    }

    updates
}

fn insert_alias_dependency_name_and_base(names: &mut HashSet<String>, name: &str) -> bool {
    let mut changed = names.insert(name.to_string());
    if let Some(base) = dae::component_base_name(name)
        && base != name
    {
        changed |= names.insert(base);
    }
    changed
}

pub(crate) fn extend_runtime_alias_dependency_closure(
    ctx: &RuntimeAliasPropagationContext,
    names: &mut HashSet<String>,
) -> bool {
    if ctx.adjacency.is_empty() {
        return false;
    }

    let mut changed_any = false;
    let mut visited = HashSet::new();
    let seeds: Vec<String> = names.iter().cloned().collect();
    for seed in seeds {
        if !ctx.adjacency.contains_key(seed.as_str()) || !visited.insert(seed.clone()) {
            continue;
        }
        let component = collect_alias_component(seed.as_str(), &ctx.adjacency, &mut visited);
        for member in component {
            changed_any |= insert_alias_dependency_name_and_base(names, member.as_str());
        }
    }

    changed_any
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    #[test]
    fn collect_alias_component_walks_connected_graph() {
        let mut adjacency: HashMap<String, Vec<String>> = HashMap::new();
        adjacency.insert("a".to_string(), vec!["b".to_string()]);
        adjacency.insert("b".to_string(), vec!["a".to_string(), "c".to_string()]);
        adjacency.insert("c".to_string(), vec!["b".to_string()]);
        let mut visited = HashSet::from(["a".to_string()]);
        let component = collect_alias_component("a", &adjacency, &mut visited);
        assert_eq!(
            component,
            vec!["a".to_string(), "b".to_string(), "c".to_string()]
        );
    }

    #[test]
    fn runtime_alias_consensus_value_detects_consistent_values() {
        let mut env = VarEnv::<f64>::new();
        env.set("x", 2.0);
        env.set("y", 2.0);
        let component = vec!["x".to_string(), "y".to_string()];
        let consensus = runtime_alias_consensus_value(&component, &env, &|name| name == "x");
        assert_eq!(consensus, AliasConsensus::Value(2.0));
    }

    #[test]
    fn apply_alias_component_anchor_updates_solver_and_env() {
        let component = vec!["x".to_string(), "x[1]".to_string()];
        let mut y = vec![0.0, 1.0];
        let mut env = VarEnv::<f64>::new();
        let name_to_idx = HashMap::from([(String::from("x"), 1usize)]);

        let updates =
            apply_alias_component_anchor(&component, 3.5, &mut y, 0, &mut env, &name_to_idx);
        assert!(updates >= 1);
        assert!((y[1] - 3.5).abs() < 1.0e-12);
        assert!((env.vars.get("x").copied().unwrap_or(0.0) - 3.5).abs() < 1.0e-12);
        assert!((env.vars.get("x[1]").copied().unwrap_or(0.0) - 3.5).abs() < 1.0e-12);
    }

    #[test]
    fn propagate_discrete_alias_equalities_pushes_explicit_value_to_alias_peer() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_reals.insert(
            dae::VarName::new("a"),
            dae::Variable::new(dae::VarName::new("a")),
        );
        dae_model.discrete_reals.insert(
            dae::VarName::new("b"),
            dae::Variable::new(dae::VarName::new("b")),
        );
        dae_model.f_z.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("a"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("b"),
                    subscripts: vec![],
                }),
            },
            Span::DUMMY,
            "alias_eq",
        ));

        let mut env = VarEnv::<f64>::new();
        env.set("a", 2.5);
        env.set("b", 0.0);
        let mut explicit_updates = HashSet::from(["a".to_string()]);
        let mut updates = Vec::new();

        let changed = propagate_discrete_alias_equalities(
            &dae_model,
            &mut env,
            &mut explicit_updates,
            |update| updates.push(update.clone()),
        );

        assert!(changed);
        assert_eq!(env.vars.get("b").copied().unwrap_or(0.0), 2.5);
        assert_eq!(updates.len(), 1);
        assert_eq!(updates[0].dst, "b");
    }

    #[test]
    fn collect_runtime_alias_anchor_names_prefers_non_alias_targets_over_alias_only_members() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_reals.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        dae_model.discrete_reals.insert(
            dae::VarName::new("y"),
            dae::Variable::new(dae::VarName::new("y")),
        );
        // Alias-only equality: x = y
        dae_model.f_z.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("x"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("y"),
                    subscripts: vec![],
                }),
            },
            Span::DUMMY,
            "alias_eq",
        ));
        // Non-alias assignment target: y = 2.0
        dae_model.f_z.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("y"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
            },
            Span::DUMMY,
            "direct_eq",
        ));

        let anchors = collect_runtime_alias_anchor_names(&dae_model, 0);
        assert!(anchors.contains("y"));
        assert!(!anchors.contains("x"));
    }

    #[test]
    fn propagate_discrete_alias_equalities_preserves_direct_lhs_direction_for_indexed_connectors() {
        let mut dae_model = dae::Dae::default();
        dae_model.inputs.insert(
            dae::VarName::new("assignClock1.u"),
            dae::Variable {
                name: dae::VarName::new("assignClock1.u"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae_model.outputs.insert(
            dae::VarName::new("add.y"),
            dae::Variable::new(dae::VarName::new("add.y")),
        );
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("assignClock1.u[1]"),
            dae::Expression::VarRef {
                name: dae::VarName::new("add.y"),
                subscripts: vec![],
            },
            Span::DUMMY,
            "assignClock input alias",
        ));

        let mut env = VarEnv::<f64>::new();
        env.set("assignClock1.u[1]", 0.0);
        env.set("add.y", 1.0);
        let mut explicit_updates = HashSet::new();

        assert_eq!(
            crate::runtime::assignment::extract_alias_pair_from_equation(
                &dae_model,
                &dae_model.f_z[0]
            ),
            Some(("assignClock1.u[1]".to_string(), "add.y".to_string()))
        );
        assert!(!crate::runtime::assignment::is_runtime_unknown_name(
            &dae_model,
            "assignClock1.u[1]"
        ));
        assert!(crate::runtime::assignment::is_runtime_unknown_name(
            &dae_model, "add.y"
        ));

        let changed = propagate_discrete_alias_equalities(
            &dae_model,
            &mut env,
            &mut explicit_updates,
            |_| {},
        );

        assert!(changed);
        assert_eq!(
            env.vars.get("assignClock1.u[1]").copied().unwrap_or(0.0),
            1.0
        );
        assert!(explicit_updates.contains("assignClock1.u[1]"));
    }

    #[test]
    fn propagate_discrete_alias_equalities_copies_explicit_array_alias_values() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_valued.insert(
            dae::VarName::new("src"),
            dae::Variable {
                name: dae::VarName::new("src"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("dst"),
            dae::Variable {
                name: dae::VarName::new("dst"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae_model.f_m.push(dae::Equation::explicit(
            dae::VarName::new("dst"),
            dae::Expression::VarRef {
                name: dae::VarName::new("src"),
                subscripts: vec![],
            },
            Span::DUMMY,
            "dst = src",
        ));

        let mut env = VarEnv::<f64>::new();
        rumoca_eval_dae::runtime::set_array_entries(&mut env, "src", &[2], &[2.0, 4.0]);
        rumoca_eval_dae::runtime::set_array_entries(&mut env, "dst", &[2], &[0.0, 0.0]);
        let mut explicit_updates = HashSet::new();
        insert_name_and_base(&mut explicit_updates, "src");

        let changed = propagate_discrete_alias_equalities(
            &dae_model,
            &mut env,
            &mut explicit_updates,
            |_| {},
        );

        assert!(changed);
        assert_eq!(env.vars.get("dst").copied(), Some(2.0));
        assert_eq!(env.vars.get("dst[1]").copied(), Some(2.0));
        assert_eq!(env.vars.get("dst[2]").copied(), Some(4.0));
        assert!(explicit_updates.contains("dst"));
    }

    #[test]
    fn propagate_discrete_alias_equalities_reaches_long_explicit_chain() {
        let mut dae_model = dae::Dae::default();
        for name in ["a", "b", "c", "d", "e", "f"] {
            dae_model.discrete_valued.insert(
                dae::VarName::new(name),
                dae::Variable::new(dae::VarName::new(name)),
            );
        }
        for (lhs, rhs) in [("b", "a"), ("c", "b"), ("d", "c"), ("e", "d"), ("f", "e")] {
            dae_model.f_m.push(dae::Equation::explicit(
                dae::VarName::new(lhs),
                dae::Expression::VarRef {
                    name: dae::VarName::new(rhs),
                    subscripts: vec![],
                },
                Span::DUMMY,
                format!("{lhs} = {rhs}"),
            ));
        }

        let mut env = VarEnv::<f64>::new();
        for name in ["b", "c", "d", "e", "f"] {
            env.set(name, 0.0);
        }
        env.set("a", 3.0);
        let mut explicit_updates = HashSet::from([String::from("a")]);

        let changed = propagate_discrete_alias_equalities(
            &dae_model,
            &mut env,
            &mut explicit_updates,
            |_| {},
        );

        assert!(changed);
        assert_eq!(env.vars.get("f").copied().unwrap_or(0.0), 3.0);
    }

    #[test]
    fn propagate_runtime_alias_components_updates_env_only_alias_peer_without_solver_slots() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_valued.insert(
            dae::VarName::new("src"),
            dae::Variable::new(dae::VarName::new("src")),
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("dst"),
            dae::Variable::new(dae::VarName::new("dst")),
        );
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("src"),
            dae::Expression::Literal(dae::Literal::Real(3.0)),
            Span::DUMMY,
            "src = 3.0",
        ));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("dst"),
            dae::Expression::VarRef {
                name: dae::VarName::new("src"),
                subscripts: vec![],
            },
            Span::DUMMY,
            "dst = src",
        ));

        let ctx = build_runtime_alias_propagation_context(&dae_model, 0, 0);
        let mut y = Vec::new();
        let mut env = VarEnv::<f64>::new();
        env.set("src", 3.0);
        env.set("dst", 0.0);

        let updates =
            propagate_runtime_alias_components_from_env_with_context(&ctx, &mut y, 0, &mut env);

        assert_eq!(updates, 1);
        assert_eq!(env.vars.get("dst").copied(), Some(3.0));
    }

    #[test]
    fn propagate_runtime_alias_components_updates_indexed_connector_chain_from_non_alias_source() {
        let mut dae_model = dae::Dae::default();
        for name in ["b.y", "Adder.b", "Adder.XOR.x[1]", "Adder.XOR.G1.x[1]"] {
            dae_model.discrete_valued.insert(
                dae::VarName::new(name),
                dae::Variable::new(dae::VarName::new(name)),
            );
        }
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("b.y"),
            dae::Expression::Literal(dae::Literal::Real(3.0)),
            Span::DUMMY,
            "b.y = 3.0",
        ));
        for (lhs, rhs) in [
            ("Adder.b", "b.y"),
            ("Adder.XOR.x[1]", "Adder.b"),
            ("Adder.XOR.G1.x[1]", "Adder.XOR.x[1]"),
        ] {
            dae_model.f_z.push(dae::Equation::explicit(
                dae::VarName::new(lhs),
                dae::Expression::VarRef {
                    name: dae::VarName::new(rhs),
                    subscripts: vec![],
                },
                Span::DUMMY,
                format!("{lhs} = {rhs}"),
            ));
        }

        let ctx = build_runtime_alias_propagation_context(&dae_model, 0, 0);
        let mut y = Vec::new();
        let mut env = VarEnv::<f64>::new();
        env.set("b.y", 3.0);
        env.set("Adder.b", 0.0);
        env.set("Adder.XOR.x[1]", 0.0);
        env.set("Adder.XOR.G1.x[1]", 0.0);

        let updates =
            propagate_runtime_alias_components_from_env_with_context(&ctx, &mut y, 0, &mut env);

        assert_eq!(updates, 3);
        assert_eq!(env.vars.get("Adder.b").copied(), Some(3.0));
        assert_eq!(env.vars.get("Adder.XOR.x[1]").copied(), Some(3.0));
        assert_eq!(env.vars.get("Adder.XOR.G1.x[1]").copied(), Some(3.0));
    }

    #[test]
    fn propagate_runtime_alias_components_expand_array_alias_equation_elementwise() {
        let mut dae_model = dae::Dae::default();
        for name in ["src", "dst"] {
            dae_model.discrete_valued.insert(
                dae::VarName::new(name),
                dae::Variable {
                    name: dae::VarName::new(name),
                    dims: vec![2],
                    ..Default::default()
                },
            );
        }
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("src"),
            dae::Expression::Literal(dae::Literal::Real(0.0)),
            Span::DUMMY,
            "src = 0.0",
        ));
        dae_model.f_m.push(dae::Equation::explicit(
            dae::VarName::new("dst"),
            dae::Expression::VarRef {
                name: dae::VarName::new("src"),
                subscripts: vec![],
            },
            Span::DUMMY,
            "dst = src",
        ));

        let ctx = build_runtime_alias_propagation_context(&dae_model, 0, 0);
        let mut y = Vec::new();
        let mut env = VarEnv::<f64>::new();
        rumoca_eval_dae::runtime::set_array_entries(&mut env, "src", &[2], &[2.0, 4.0]);
        rumoca_eval_dae::runtime::set_array_entries(&mut env, "dst", &[2], &[0.0, 0.0]);

        let updates =
            propagate_runtime_alias_components_from_env_with_context(&ctx, &mut y, 0, &mut env);

        assert_eq!(updates, 3);
        assert_eq!(env.vars.get("dst").copied(), Some(2.0));
        assert_eq!(env.vars.get("dst[1]").copied(), Some(2.0));
        assert_eq!(env.vars.get("dst[2]").copied(), Some(4.0));
    }

    #[test]
    fn propagate_discrete_alias_equalities_does_not_demote_direct_runtime_anchor_from_alias_peer() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_valued.insert(
            dae::VarName::new("a.y"),
            dae::Variable::new(dae::VarName::new("a.y")),
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("Adder.AND.x[2]"),
            dae::Variable::new(dae::VarName::new("Adder.AND.x[2]")),
        );
        dae_model.f_x.push(dae::Equation::explicit(
            dae::VarName::new("a.y"),
            dae::Expression::Literal(dae::Literal::Real(3.0)),
            Span::DUMMY,
            "a.y = 3",
        ));
        dae_model.f_m.push(dae::Equation::explicit(
            dae::VarName::new("Adder.AND.x[2]"),
            dae::Expression::VarRef {
                name: dae::VarName::new("a.y"),
                subscripts: vec![],
            },
            Span::DUMMY,
            "explicit connection equation: Adder.AND.x[2] = a.y",
        ));

        let mut env = VarEnv::<f64>::new();
        env.set("a.y", 3.0);
        env.set("Adder.AND.x[2]", 0.0);
        let mut explicit_updates = HashSet::from([String::from("Adder.AND.x[2]")]);

        let changed = propagate_discrete_alias_equalities(
            &dae_model,
            &mut env,
            &mut explicit_updates,
            |_| {},
        );

        assert!(!changed);
        assert_eq!(env.get("a.y"), 3.0);
        assert_eq!(env.get("Adder.AND.x[2]"), 0.0);
    }

    #[test]
    fn collect_runtime_alias_anchor_names_includes_non_runtime_unknown_alias_sources() {
        let mut dae_model = dae::Dae::default();
        dae_model.inputs.insert(
            dae::VarName::new("Enable.x"),
            dae::Variable::new(dae::VarName::new("Enable.x")),
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("Enable.y"),
            dae::Variable::new(dae::VarName::new("Enable.y")),
        );
        dae_model.f_m.push(dae::Equation::explicit(
            dae::VarName::new("Enable.y"),
            dae::Expression::VarRef {
                name: dae::VarName::new("Enable.x"),
                subscripts: vec![],
            },
            Span::DUMMY,
            "Enable.y = Enable.x",
        ));

        let anchors = collect_runtime_alias_anchor_names(&dae_model, 0);
        assert!(anchors.contains("Enable.x"));
    }

    #[test]
    fn collect_runtime_alias_anchor_names_does_not_readd_alias_only_input_when_component_is_anchored()
     {
        let mut dae_model = dae::Dae::default();
        dae_model.inputs.insert(
            dae::VarName::new("feedback.u2"),
            dae::Variable::new(dae::VarName::new("feedback.u2")),
        );
        dae_model.discrete_reals.insert(
            dae::VarName::new("sample1.y"),
            dae::Variable::new(dae::VarName::new("sample1.y")),
        );
        dae_model.f_z.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("sample1.y"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
            },
            Span::DUMMY,
            "sample1.y = 2.0",
        ));
        dae_model.f_z.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("sample1.y"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("feedback.u2"),
                    subscripts: vec![],
                }),
            },
            Span::DUMMY,
            "sample1.y = feedback.u2",
        ));

        let anchors = collect_runtime_alias_anchor_names(&dae_model, 0);
        assert!(anchors.contains("sample1.y"));
        assert!(!anchors.contains("feedback.u2"));
    }
}
