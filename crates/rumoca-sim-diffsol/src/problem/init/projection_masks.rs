use super::*;

pub(crate) struct RuntimeProjectionMasks {
    pub(crate) fixed_cols: Vec<bool>,
    pub(crate) ignored_rows: Vec<bool>,
    pub(crate) branch_local_analog_unknowns: Vec<Option<usize>>,
    pub(crate) branch_local_analog_row_pairs: Vec<(usize, usize, [usize; 2])>,
    pub(crate) branch_local_analog_cols: Vec<bool>,
}

pub(crate) fn build_runtime_projection_masks(
    dae: &Dae,
    n_x: usize,
    n_eq: usize,
) -> RuntimeProjectionMasks {
    let (mut fixed_cols, mut ignored_rows) = initialize_runtime_projection_masks(n_x, n_eq);
    let target_assignment_stats =
        rumoca_sim::runtime::assignment::collect_direct_assignment_target_stats(dae, n_x, false);
    let names = solver_vector_names(dae, n_eq);
    let name_to_idx: std::collections::HashMap<String, usize> = names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect();
    apply_runtime_projection_assignment_masks(
        dae,
        n_x,
        n_eq,
        &name_to_idx,
        &target_assignment_stats,
        &mut fixed_cols,
        &mut ignored_rows,
    );
    propagate_runtime_projection_state_dependencies(dae, n_x, &name_to_idx, &mut fixed_cols);
    mark_runtime_projection_discrete_fixed_cols(dae, n_x, &names, &mut fixed_cols);
    propagate_runtime_projection_fixed_alias_closure(
        dae,
        n_x,
        &name_to_idx,
        &mut fixed_cols,
        &mut ignored_rows,
    );
    let branch_local_analog_unknowns = build_runtime_projection_branch_local_analog_unknowns(
        dae,
        n_x,
        &name_to_idx,
        &fixed_cols,
        &ignored_rows,
    );
    let branch_local_analog_row_pairs = build_runtime_projection_branch_local_analog_row_pairs(
        dae,
        n_x,
        &name_to_idx,
        &fixed_cols,
        &ignored_rows,
    );
    let mut branch_local_analog_cols = build_branch_local_analog_cols(
        n_eq,
        &branch_local_analog_unknowns,
        &branch_local_analog_row_pairs,
    );
    mark_branch_local_analog_alias_groups(
        dae,
        n_x,
        &name_to_idx,
        &fixed_cols,
        &ignored_rows,
        &mut branch_local_analog_cols,
    );
    RuntimeProjectionMasks {
        fixed_cols,
        ignored_rows,
        branch_local_analog_unknowns,
        branch_local_analog_row_pairs,
        branch_local_analog_cols,
    }
}

pub(crate) fn initialize_runtime_projection_masks(
    n_x: usize,
    n_eq: usize,
) -> (Vec<bool>, Vec<bool>) {
    let mut fixed_cols = vec![false; n_eq];
    let mut ignored_rows = vec![false; n_eq];
    for flag in fixed_cols.iter_mut().take(n_x.min(n_eq)) {
        *flag = true;
    }
    for flag in ignored_rows.iter_mut().take(n_x.min(n_eq)) {
        *flag = true;
    }
    (fixed_cols, ignored_rows)
}

pub(crate) fn apply_runtime_projection_assignment_masks(
    dae: &Dae,
    n_x: usize,
    n_eq: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    target_assignment_stats: &std::collections::HashMap<
        String,
        rumoca_sim::runtime::assignment::DirectAssignmentTargetStats,
    >,
    fixed_cols: &mut [bool],
    ignored_rows: &mut [bool],
) {
    for (row, eq) in dae.f_x.iter().enumerate().skip(n_x) {
        let Some((target, solution)) = runtime_direct_assignment(dae, eq) else {
            if eq.origin == "orphaned_variable_pin" && row < ignored_rows.len() {
                ignored_rows[row] = true;
            }
            continue;
        };
        let is_alias_solution =
            rumoca_sim::runtime::assignment::assignment_solution_is_alias_varref(dae, solution);
        if !runtime_projection_target_can_be_seeded(
            target.as_str(),
            is_alias_solution,
            target_assignment_stats,
        ) {
            continue;
        }
        mark_runtime_projection_discrete_source(
            dae,
            row,
            target.as_str(),
            solution,
            name_to_idx,
            fixed_cols,
            ignored_rows,
        );
        let mut masks = RuntimeProjectionMaskSlices {
            fixed_cols,
            ignored_rows,
        };
        let assignment = RuntimeProjectionAssignment {
            row,
            target: target.as_str(),
            solution,
        };
        apply_runtime_projection_exogenous_mask(
            dae,
            &assignment,
            n_x,
            n_eq,
            name_to_idx,
            &mut masks,
        );
    }
}

pub(crate) struct RuntimeProjectionMaskSlices<'a> {
    fixed_cols: &'a mut [bool],
    ignored_rows: &'a mut [bool],
}

pub(crate) struct RuntimeProjectionAssignment<'a> {
    row: usize,
    target: &'a str,
    solution: &'a Expression,
}

pub(crate) fn mark_runtime_projection_discrete_source(
    dae: &Dae,
    row: usize,
    target: &str,
    solution: &Expression,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &mut [bool],
    ignored_rows: &mut [bool],
) {
    if !runtime_projection_target_is_discrete(dae, target) {
        return;
    }
    if row < ignored_rows.len() {
        ignored_rows[row] = true;
    }
    if let Some(source_idx) = runtime_projection_alias_source_solver_idx(solution, name_to_idx)
        && source_idx < fixed_cols.len()
    {
        fixed_cols[source_idx] = true;
    }
}

pub(crate) fn apply_runtime_projection_exogenous_mask(
    dae: &Dae,
    assignment: &RuntimeProjectionAssignment<'_>,
    n_x: usize,
    n_eq: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    masks: &mut RuntimeProjectionMaskSlices<'_>,
) {
    if !runtime_projection_target_is_exogenous(dae, assignment.solution, n_x, n_eq, name_to_idx) {
        return;
    }
    let Some(idx) = solver_idx_for_target(assignment.target, name_to_idx) else {
        if assignment.row < masks.ignored_rows.len() {
            masks.ignored_rows[assignment.row] = true;
        }
        return;
    };
    if idx < n_x {
        masks.fixed_cols[idx] = false;
    } else if idx < n_eq {
        masks.fixed_cols[idx] = true;
        if assignment.row < masks.ignored_rows.len() {
            masks.ignored_rows[assignment.row] = true;
        }
    }
}

pub(crate) fn mark_runtime_projection_discrete_fixed_cols(
    dae: &Dae,
    n_x: usize,
    names: &[String],
    fixed_cols: &mut [bool],
) {
    for (idx, name) in names.iter().enumerate().skip(n_x) {
        if dae
            .f_x
            .get(idx)
            .is_some_and(|eq| eq.origin == "orphaned_variable_pin")
        {
            fixed_cols[idx] = true;
            continue;
        }
        let base = component_base_name(name).unwrap_or_else(|| name.to_string());
        let key = VarName::new(base);
        if dae.discrete_reals.contains_key(&key) || dae.discrete_valued.contains_key(&key) {
            fixed_cols[idx] = true;
        }
    }
}

pub(crate) fn build_branch_local_analog_cols(
    n_eq: usize,
    branch_local_analog_unknowns: &[Option<usize>],
    branch_local_analog_row_pairs: &[(usize, usize, [usize; 2])],
) -> Vec<bool> {
    let mut cols = vec![false; n_eq];
    for idx in branch_local_analog_unknowns.iter().flatten().copied() {
        if idx < cols.len() {
            cols[idx] = true;
        }
    }
    for (_, _, pair) in branch_local_analog_row_pairs {
        for &idx in pair {
            if idx < cols.len() {
                cols[idx] = true;
            }
        }
    }
    cols
}

pub(crate) fn build_runtime_projection_branch_local_analog_unknowns(
    dae: &Dae,
    n_x: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
    ignored_rows: &[bool],
) -> Vec<Option<usize>> {
    dae.f_x
        .iter()
        .enumerate()
        .map(|(row, eq)| {
            if row < n_x || ignored_rows.get(row).copied().unwrap_or(false) {
                return None;
            }
            runtime_projection_branch_local_analog_unknown(&eq.rhs, n_x, name_to_idx, fixed_cols)
        })
        .collect()
}

pub(crate) fn build_runtime_projection_branch_local_analog_row_pairs(
    dae: &Dae,
    n_x: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
    ignored_rows: &[bool],
) -> Vec<(usize, usize, [usize; 2])> {
    let free_unknown_pairs: Vec<Option<[usize; 2]>> = dae
        .f_x
        .iter()
        .enumerate()
        .map(|(row, eq)| {
            if row < n_x || ignored_rows.get(row).copied().unwrap_or(false) {
                return None;
            }
            runtime_projection_free_unknown_pair(&eq.rhs, n_x, name_to_idx, fixed_cols)
        })
        .collect();

    let mut out = Vec::new();
    let mut seen = std::collections::BTreeSet::new();
    for row in n_x..dae.f_x.len() {
        if ignored_rows.get(row).copied().unwrap_or(false)
            || !dae
                .f_x
                .get(row)
                .is_some_and(|eq| expr_contains_branch_local_analog_operator(&eq.rhs))
        {
            continue;
        }
        let Some(pair) = free_unknown_pairs.get(row).copied().flatten() else {
            continue;
        };
        let Some(partner) = ((row + 1)..dae.f_x.len()).find(|&other| {
            !ignored_rows.get(other).copied().unwrap_or(false)
                && free_unknown_pairs.get(other).copied().flatten() == Some(pair)
        }) else {
            continue;
        };
        if seen.insert((row, partner, pair)) {
            out.push((row, partner, pair));
        }
    }
    out
}

pub(crate) fn runtime_projection_branch_local_analog_unknown(
    expr: &Expression,
    n_x: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
) -> Option<usize> {
    if !expr_contains_branch_local_analog_operator(expr) {
        return None;
    }
    let mut free_unknowns = std::collections::BTreeSet::new();
    collect_runtime_projection_free_solver_refs(
        expr,
        n_x,
        name_to_idx,
        fixed_cols,
        &mut free_unknowns,
    );
    (free_unknowns.len() == 1)
        .then(|| free_unknowns.into_iter().next())
        .flatten()
}

pub(crate) fn runtime_projection_free_unknown_pair(
    expr: &Expression,
    n_x: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
) -> Option<[usize; 2]> {
    let mut free_unknowns = std::collections::BTreeSet::new();
    collect_runtime_projection_free_solver_refs(
        expr,
        n_x,
        name_to_idx,
        fixed_cols,
        &mut free_unknowns,
    );
    if free_unknowns.len() != 2 {
        return None;
    }
    let mut iter = free_unknowns.into_iter();
    Some([iter.next()?, iter.next()?])
}

pub(crate) fn expr_contains_branch_local_analog_operator(expr: &Expression) -> bool {
    match expr {
        Expression::BuiltinCall { function, args } => {
            matches!(function, BuiltinFunction::Smooth | BuiltinFunction::NoEvent)
                || args.iter().any(expr_contains_branch_local_analog_operator)
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_branch_local_analog_operator(lhs)
                || expr_contains_branch_local_analog_operator(rhs)
        }
        Expression::Unary { rhs, .. } | Expression::FieldAccess { base: rhs, .. } => {
            expr_contains_branch_local_analog_operator(rhs)
        }
        Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_contains_branch_local_analog_operator)
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_contains_branch_local_analog_operator(cond)
                    || expr_contains_branch_local_analog_operator(value)
            }) || expr_contains_branch_local_analog_operator(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => elements
            .iter()
            .any(expr_contains_branch_local_analog_operator),
        Expression::Range { start, step, end } => {
            expr_contains_branch_local_analog_operator(start)
                || step
                    .as_deref()
                    .is_some_and(expr_contains_branch_local_analog_operator)
                || expr_contains_branch_local_analog_operator(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_branch_local_analog_operator(expr)
                || indices
                    .iter()
                    .any(|index| expr_contains_branch_local_analog_operator(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_contains_branch_local_analog_operator)
        }
        Expression::Index { base, subscripts } => {
            expr_contains_branch_local_analog_operator(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_contains_branch_local_analog_operator(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => false,
    }
}

pub(crate) fn collect_runtime_projection_free_solver_refs(
    expr: &Expression,
    n_x: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
    out: &mut std::collections::BTreeSet<usize>,
) {
    match expr {
        Expression::VarRef { name, subscripts } => {
            collect_runtime_projection_var_ref(name, subscripts, n_x, name_to_idx, fixed_cols, out)
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_runtime_projection_free_solver_refs(lhs, n_x, name_to_idx, fixed_cols, out);
            collect_runtime_projection_free_solver_refs(rhs, n_x, name_to_idx, fixed_cols, out);
        }
        Expression::Unary { rhs, .. } | Expression::FieldAccess { base: rhs, .. } => {
            collect_runtime_projection_free_solver_refs(rhs, n_x, name_to_idx, fixed_cols, out);
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            collect_runtime_projection_exprs(args, n_x, name_to_idx, fixed_cols, out);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            collect_runtime_projection_if_branches(
                branches,
                else_branch,
                n_x,
                name_to_idx,
                fixed_cols,
                out,
            );
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            collect_runtime_projection_exprs(elements, n_x, name_to_idx, fixed_cols, out);
        }
        Expression::Range { start, step, end } => {
            collect_runtime_projection_free_solver_refs(start, n_x, name_to_idx, fixed_cols, out);
            if let Some(step) = step.as_deref() {
                collect_runtime_projection_free_solver_refs(
                    step,
                    n_x,
                    name_to_idx,
                    fixed_cols,
                    out,
                );
            }
            collect_runtime_projection_free_solver_refs(end, n_x, name_to_idx, fixed_cols, out);
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_runtime_projection_free_solver_refs(expr, n_x, name_to_idx, fixed_cols, out);
            for index in indices {
                collect_runtime_projection_free_solver_refs(
                    &index.range,
                    n_x,
                    name_to_idx,
                    fixed_cols,
                    out,
                );
            }
            if let Some(filter) = filter.as_deref() {
                collect_runtime_projection_free_solver_refs(
                    filter,
                    n_x,
                    name_to_idx,
                    fixed_cols,
                    out,
                );
            }
        }
        Expression::Index { base, subscripts } => {
            collect_runtime_projection_free_solver_refs(base, n_x, name_to_idx, fixed_cols, out);
            collect_runtime_projection_subscripts(subscripts, n_x, name_to_idx, fixed_cols, out);
        }
        Expression::Literal(_) | Expression::Empty => {}
    }
}

pub(crate) fn collect_runtime_projection_var_ref(
    name: &VarName,
    subscripts: &[dae::Subscript],
    n_x: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
    out: &mut std::collections::BTreeSet<usize>,
) {
    if let Some(key) = rumoca_sim::runtime::assignment::canonical_var_ref_key(name, subscripts)
        && let Some(idx) = solver_idx_for_target(key.as_str(), name_to_idx)
        && idx >= n_x
        && idx < fixed_cols.len()
        && !fixed_cols[idx]
    {
        out.insert(idx);
    }
}

pub(crate) fn collect_runtime_projection_exprs(
    exprs: &[Expression],
    n_x: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
    out: &mut std::collections::BTreeSet<usize>,
) {
    for expr in exprs {
        collect_runtime_projection_free_solver_refs(expr, n_x, name_to_idx, fixed_cols, out);
    }
}

pub(crate) fn collect_runtime_projection_if_branches(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    n_x: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
    out: &mut std::collections::BTreeSet<usize>,
) {
    for (cond, value) in branches {
        collect_runtime_projection_free_solver_refs(cond, n_x, name_to_idx, fixed_cols, out);
        collect_runtime_projection_free_solver_refs(value, n_x, name_to_idx, fixed_cols, out);
    }
    collect_runtime_projection_free_solver_refs(else_branch, n_x, name_to_idx, fixed_cols, out);
}

pub(crate) fn collect_runtime_projection_subscripts(
    subscripts: &[dae::Subscript],
    n_x: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
    out: &mut std::collections::BTreeSet<usize>,
) {
    for subscript in subscripts {
        if let dae::Subscript::Expr(expr) = subscript {
            collect_runtime_projection_free_solver_refs(expr, n_x, name_to_idx, fixed_cols, out);
        }
    }
}

pub(crate) fn runtime_projection_target_can_be_seeded(
    target: &str,
    is_alias_solution: bool,
    stats: &std::collections::HashMap<
        String,
        rumoca_sim::runtime::assignment::DirectAssignmentTargetStats,
    >,
) -> bool {
    let target_stats = stats.get(target).copied().unwrap_or_default();
    if target_stats.total > 1 && target_stats.non_alias != 1 {
        return false;
    }
    !(target_stats.total > 1 && target_stats.non_alias == 1 && is_alias_solution)
}

pub(crate) fn runtime_projection_target_is_exogenous(
    dae: &Dae,
    solution: &Expression,
    n_x: usize,
    n_eq: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
) -> bool {
    rumoca_sim::runtime::assignment::direct_assignment_source_is_known(
        dae,
        solution,
        n_x,
        n_eq,
        |target| solver_idx_for_target(target, name_to_idx),
    )
}

pub(crate) fn runtime_projection_target_is_discrete(dae: &Dae, target: &str) -> bool {
    let base = component_base_name(target).unwrap_or_else(|| target.to_string());
    let key = VarName::new(base);
    dae.discrete_reals.contains_key(&key) || dae.discrete_valued.contains_key(&key)
}

pub(crate) fn runtime_projection_alias_source_solver_idx(
    solution: &Expression,
    name_to_idx: &std::collections::HashMap<String, usize>,
) -> Option<usize> {
    let Expression::VarRef { name, subscripts } = solution else {
        return None;
    };
    let source = rumoca_sim::runtime::assignment::canonical_var_ref_key(name, subscripts)?;
    solver_idx_for_target(source.as_str(), name_to_idx)
}

pub(crate) fn propagate_runtime_projection_state_dependencies(
    dae: &Dae,
    n_x: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed: &mut [bool],
) {
    let max_rows = n_x.min(dae.f_x.len()).min(fixed.len());
    let mut changed = true;
    while changed {
        changed = false;
        for row in 0..max_rows {
            if fixed[row] {
                continue;
            }
            let mut refs = std::collections::HashSet::new();
            dae.f_x[row].rhs.collect_var_refs(&mut refs);
            for ref_name in refs {
                changed |= clear_fixed_dependency(ref_name.as_str(), name_to_idx, max_rows, fixed);
            }
        }
    }
}

pub(crate) fn clear_fixed_dependency(
    ref_name: &str,
    name_to_idx: &std::collections::HashMap<String, usize>,
    max_rows: usize,
    fixed: &mut [bool],
) -> bool {
    let Some(idx) = dependency_to_unfix(ref_name, name_to_idx, max_rows, fixed) else {
        return false;
    };
    fixed[idx] = false;
    true
}

pub(crate) fn propagate_runtime_projection_fixed_alias_closure(
    dae: &Dae,
    n_x: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &mut [bool],
    ignored_rows: &mut [bool],
) {
    // MLS §8.3 / §9: equation and connection equalities remain simultaneous
    // constraints. During runtime projection, values already fixed by
    // states/exogenous inputs must stay fixed across continuous alias chains,
    // including rows that reduce to aliases after grounded zero terms are
    // substituted (for example x = y - 0).
    let strict_alias_rows =
        runtime_projection_collect_exact_continuous_alias_rows(dae, name_to_idx, fixed_cols, None);
    if strict_alias_rows.is_empty() {
        return;
    }
    let zero_fixed_cols =
        runtime_projection_fixed_zero_cols(dae, name_to_idx, fixed_cols, &strict_alias_rows);
    let alias_rows = if zero_fixed_cols.iter().any(|&is_zero_fixed| is_zero_fixed) {
        runtime_projection_collect_exact_continuous_alias_rows(
            dae,
            name_to_idx,
            fixed_cols,
            Some(&zero_fixed_cols),
        )
    } else {
        strict_alias_rows
    };

    let mut adjacency = vec![Vec::new(); fixed_cols.len()];
    for &(_, lhs_idx, rhs_idx) in &alias_rows {
        adjacency[lhs_idx].push(rhs_idx);
        adjacency[rhs_idx].push(lhs_idx);
    }

    let mut closure_fixed = vec![false; fixed_cols.len()];
    let mut queue = std::collections::VecDeque::new();
    for (idx, &is_fixed) in fixed_cols.iter().enumerate() {
        let state_fixed = idx < n_x && is_fixed;
        let zero_fixed = zero_fixed_cols.get(idx).copied().unwrap_or(false);
        if state_fixed || zero_fixed {
            closure_fixed[idx] = true;
            queue.push_back(idx);
        }
    }
    while let Some(idx) = queue.pop_front() {
        for &neighbor in &adjacency[idx] {
            if closure_fixed[neighbor] {
                continue;
            }
            closure_fixed[neighbor] = true;
            queue.push_back(neighbor);
        }
    }
    for (idx, &is_closure_fixed) in closure_fixed.iter().enumerate() {
        if is_closure_fixed {
            fixed_cols[idx] = true;
        }
    }

    for (row, lhs_idx, rhs_idx) in alias_rows {
        if fixed_cols[lhs_idx] && fixed_cols[rhs_idx] && row < ignored_rows.len() {
            ignored_rows[row] = true;
        }
    }
}

pub(crate) fn runtime_projection_exact_continuous_alias_rows(
    dae: &Dae,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
) -> Vec<(usize, usize, usize)> {
    let strict_alias_rows =
        runtime_projection_collect_exact_continuous_alias_rows(dae, name_to_idx, fixed_cols, None);
    let zero_fixed_cols =
        runtime_projection_fixed_zero_cols(dae, name_to_idx, fixed_cols, &strict_alias_rows);
    if !zero_fixed_cols.iter().any(|&is_zero_fixed| is_zero_fixed) {
        return strict_alias_rows;
    }

    runtime_projection_collect_exact_continuous_alias_rows(
        dae,
        name_to_idx,
        fixed_cols,
        Some(&zero_fixed_cols),
    )
}

pub(crate) fn runtime_projection_collect_exact_continuous_alias_rows(
    dae: &Dae,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
    zero_fixed_cols: Option<&[bool]>,
) -> Vec<(usize, usize, usize)> {
    dae.f_x
        .iter()
        .enumerate()
        .filter_map(|(row, eq)| {
            let (lhs, rhs) =
                runtime_projection_exact_alias_pair(dae, eq, name_to_idx, zero_fixed_cols)?;
            runtime_projection_exact_continuous_alias_row(
                dae,
                row,
                lhs.as_str(),
                rhs.as_str(),
                name_to_idx,
                fixed_cols,
            )
        })
        .collect()
}

pub(crate) fn runtime_projection_fixed_zero_cols(
    dae: &Dae,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
    strict_alias_rows: &[(usize, usize, usize)],
) -> Vec<bool> {
    let mut zero_fixed_cols = vec![false; fixed_cols.len()];
    for eq in &dae.f_x {
        let Some(idx) = runtime_projection_zero_literal_target_idx(eq, name_to_idx) else {
            continue;
        };
        if idx < fixed_cols.len() && fixed_cols[idx] {
            zero_fixed_cols[idx] = true;
        }
    }

    let mut adjacency = vec![Vec::new(); fixed_cols.len()];
    for &(_, lhs_idx, rhs_idx) in strict_alias_rows {
        adjacency[lhs_idx].push(rhs_idx);
        adjacency[rhs_idx].push(lhs_idx);
    }

    let mut queue: std::collections::VecDeque<_> = zero_fixed_cols
        .iter()
        .enumerate()
        .filter_map(|(idx, &is_zero_fixed)| is_zero_fixed.then_some(idx))
        .collect();
    while let Some(idx) = queue.pop_front() {
        for &neighbor in &adjacency[idx] {
            if neighbor >= zero_fixed_cols.len()
                || !fixed_cols[neighbor]
                || zero_fixed_cols[neighbor]
            {
                continue;
            }
            zero_fixed_cols[neighbor] = true;
            queue.push_back(neighbor);
        }
    }
    zero_fixed_cols
}

pub(crate) fn runtime_projection_zero_literal_target_idx(
    eq: &Equation,
    name_to_idx: &std::collections::HashMap<String, usize>,
) -> Option<usize> {
    let target = if let Some(lhs) = eq.lhs.as_ref() {
        lhs.as_str().to_string()
    } else {
        let Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } = &eq.rhs
        else {
            return None;
        };
        if runtime_projection_is_zero_literal(rhs.as_ref()) {
            runtime_projection_exact_alias_name(lhs)?
        } else if runtime_projection_is_zero_literal(lhs.as_ref()) {
            runtime_projection_exact_alias_name(rhs)?
        } else {
            return None;
        }
    };
    solver_idx_for_target(target.as_str(), name_to_idx)
}

pub(crate) fn runtime_projection_exact_alias_pair(
    dae: &Dae,
    eq: &Equation,
    name_to_idx: &std::collections::HashMap<String, usize>,
    zero_fixed_cols: Option<&[bool]>,
) -> Option<(String, String)> {
    if let Some(lhs) = eq.lhs.as_ref() {
        let rhs = runtime_projection_effective_alias_name(&eq.rhs, name_to_idx, zero_fixed_cols)?;
        return runtime_projection_exact_alias_names(dae, lhs.as_str().to_string(), rhs);
    }

    let Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(_),
        lhs,
        rhs,
    } = &eq.rhs
    else {
        return None;
    };
    let lhs = runtime_projection_effective_alias_name(lhs, name_to_idx, zero_fixed_cols)?;
    let rhs = runtime_projection_effective_alias_name(rhs, name_to_idx, zero_fixed_cols)?;
    runtime_projection_exact_alias_names(dae, lhs, rhs)
}

pub(crate) fn runtime_projection_exact_alias_names(
    dae: &Dae,
    lhs: String,
    rhs: String,
) -> Option<(String, String)> {
    if rumoca_sim::runtime::assignment::is_known_assignment_name(dae, lhs.as_str())
        && rumoca_sim::runtime::assignment::is_known_assignment_name(dae, rhs.as_str())
    {
        return Some((lhs, rhs));
    }
    None
}

pub(crate) fn runtime_projection_exact_alias_name(expr: &Expression) -> Option<String> {
    match expr {
        Expression::VarRef { name, subscripts } => {
            rumoca_sim::runtime::assignment::canonical_var_ref_key(name, subscripts)
        }
        Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } if runtime_projection_is_zero_literal(rhs.as_ref()) => {
            runtime_projection_exact_alias_name(lhs)
        }
        Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(_),
            lhs,
            rhs,
        } if runtime_projection_is_zero_literal(lhs.as_ref()) => {
            runtime_projection_exact_alias_name(rhs)
        }
        Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(_),
            lhs,
            rhs,
        } if runtime_projection_is_zero_literal(rhs.as_ref()) => {
            runtime_projection_exact_alias_name(lhs)
        }
        _ => None,
    }
}

pub(crate) fn runtime_projection_effective_alias_name(
    expr: &Expression,
    name_to_idx: &std::collections::HashMap<String, usize>,
    zero_fixed_cols: Option<&[bool]>,
) -> Option<String> {
    match expr {
        Expression::VarRef { name, subscripts } => {
            rumoca_sim::runtime::assignment::canonical_var_ref_key(name, subscripts)
        }
        Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } if runtime_projection_is_zero_reducer(rhs.as_ref(), name_to_idx, zero_fixed_cols) => {
            runtime_projection_effective_alias_name(lhs, name_to_idx, zero_fixed_cols)
        }
        Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(_),
            lhs,
            rhs,
        } if runtime_projection_is_zero_reducer(lhs.as_ref(), name_to_idx, zero_fixed_cols) => {
            runtime_projection_effective_alias_name(rhs, name_to_idx, zero_fixed_cols)
        }
        Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(_),
            lhs,
            rhs,
        } if runtime_projection_is_zero_reducer(rhs.as_ref(), name_to_idx, zero_fixed_cols) => {
            runtime_projection_effective_alias_name(lhs, name_to_idx, zero_fixed_cols)
        }
        _ => None,
    }
}

pub(crate) fn runtime_projection_is_zero_reducer(
    expr: &Expression,
    name_to_idx: &std::collections::HashMap<String, usize>,
    zero_fixed_cols: Option<&[bool]>,
) -> bool {
    runtime_projection_is_zero_literal(expr)
        || zero_fixed_cols.is_some_and(|zero_fixed_cols| {
            runtime_projection_var_ref_is_zero_fixed(expr, name_to_idx, zero_fixed_cols)
        })
}

pub(crate) fn runtime_projection_var_ref_is_zero_fixed(
    expr: &Expression,
    name_to_idx: &std::collections::HashMap<String, usize>,
    zero_fixed_cols: &[bool],
) -> bool {
    let Expression::VarRef { name, subscripts } = expr else {
        return false;
    };
    let Some(key) = rumoca_sim::runtime::assignment::canonical_var_ref_key(name, subscripts) else {
        return false;
    };
    let Some(idx) = solver_idx_for_target(key.as_str(), name_to_idx) else {
        return false;
    };
    zero_fixed_cols.get(idx).copied().unwrap_or(false)
}

pub(crate) fn runtime_projection_is_zero_literal(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(dae::Literal::Integer(0)) => true,
        Expression::Literal(dae::Literal::Real(v)) => v.abs() <= f64::EPSILON,
        _ => false,
    }
}

pub(crate) fn runtime_projection_exact_continuous_alias_row(
    dae: &Dae,
    row: usize,
    lhs: &str,
    rhs: &str,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
) -> Option<(usize, usize, usize)> {
    if rumoca_sim::runtime::assignment::is_discrete_name(dae, lhs)
        || rumoca_sim::runtime::assignment::is_discrete_name(dae, rhs)
    {
        return None;
    }
    let lhs_idx = solver_idx_for_target(lhs, name_to_idx)?;
    let rhs_idx = solver_idx_for_target(rhs, name_to_idx)?;
    if lhs_idx >= fixed_cols.len() || rhs_idx >= fixed_cols.len() || lhs_idx == rhs_idx {
        return None;
    }
    Some((row, lhs_idx, rhs_idx))
}

pub(crate) fn mark_branch_local_analog_alias_groups(
    dae: &Dae,
    n_x: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
    ignored_rows: &[bool],
    branch_local_analog_cols: &mut [bool],
) {
    let groups = runtime_projection_exact_continuous_alias_groups(dae, name_to_idx, fixed_cols);
    if groups.is_empty() {
        return;
    }

    for (row, eq) in dae.f_x.iter().enumerate() {
        if row < n_x
            || ignored_rows.get(row).copied().unwrap_or(false)
            || !expr_contains_branch_local_analog_operator(&eq.rhs)
        {
            continue;
        }

        let mut free_unknowns = std::collections::BTreeSet::new();
        collect_runtime_projection_free_solver_refs(
            &eq.rhs,
            n_x,
            name_to_idx,
            fixed_cols,
            &mut free_unknowns,
        );
        let free_groups: std::collections::BTreeSet<_> = free_unknowns
            .iter()
            .copied()
            .map(|idx| groups[idx])
            .collect();
        if free_groups.is_empty() || free_groups.len() > 2 {
            continue;
        }

        for idx in free_unknowns {
            let group = groups[idx];
            for (member_idx, &member_group) in groups.iter().enumerate() {
                try_mark_branch_local_analog_col(
                    branch_local_analog_cols,
                    member_idx,
                    member_group,
                    group,
                );
            }
        }
    }
}

pub(crate) fn runtime_projection_exact_continuous_alias_groups(
    dae: &Dae,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed_cols: &[bool],
) -> Vec<usize> {
    let alias_rows = runtime_projection_exact_continuous_alias_rows(dae, name_to_idx, fixed_cols);
    let mut groups: Vec<_> = (0..fixed_cols.len()).collect();
    for (_, lhs_idx, rhs_idx) in alias_rows {
        let lhs_group = groups[lhs_idx];
        let rhs_group = groups[rhs_idx];
        if lhs_group == rhs_group {
            continue;
        }
        let keep = lhs_group.min(rhs_group);
        let replace = lhs_group.max(rhs_group);
        for group in &mut groups {
            if *group == replace {
                *group = keep;
            }
        }
    }
    groups
}

pub(crate) fn dependency_to_unfix(
    ref_name: &str,
    name_to_idx: &std::collections::HashMap<String, usize>,
    max_rows: usize,
    fixed: &[bool],
) -> Option<usize> {
    solver_idx_for_target(ref_name, name_to_idx).filter(|&idx| idx < max_rows && fixed[idx])
}

pub(crate) fn variable_size_for_target(dae: &Dae, target: &str) -> Option<usize> {
    let lookup = |name: &str| {
        dae.states
            .get(&VarName::new(name))
            .or_else(|| dae.algebraics.get(&VarName::new(name)))
            .or_else(|| dae.outputs.get(&VarName::new(name)))
            .or_else(|| dae.discrete_reals.get(&VarName::new(name)))
            .or_else(|| dae.discrete_valued.get(&VarName::new(name)))
            .map(|var| var.size())
    };
    lookup(target).or_else(|| component_base_name(target).and_then(|base| lookup(&base)))
}

pub(crate) fn runtime_direct_assignment<'a>(
    dae: &Dae,
    eq: &'a Equation,
) -> Option<(String, &'a Expression)> {
    if eq.origin == "orphaned_variable_pin" {
        return None;
    }

    if let Some((target, solution)) = extract_direct_assignment(&eq.rhs) {
        let target_size = variable_size_for_target(dae, target.as_str())?;
        if !target.contains('[') && target_size > 1 {
            return None;
        }
        return Some((target, solution));
    }

    if let Some(lhs) = eq.lhs.as_ref() {
        let lhs_size = variable_size_for_target(dae, lhs.as_str())?;
        if lhs_size > 1 {
            return None;
        }
        return Some((lhs.as_str().to_string(), &eq.rhs));
    }

    None
}

#[cfg(test)]
pub(crate) fn direct_assignment_graph_has_cycle(
    edges: &std::collections::HashMap<usize, Vec<usize>>,
) -> bool {
    let mut indegree: std::collections::HashMap<usize, usize> = std::collections::HashMap::new();
    for (&src, deps) in edges {
        indegree.entry(src).or_insert(0);
        for &dst in deps {
            *indegree.entry(dst).or_insert(0) += 1;
        }
    }

    let mut queue = std::collections::VecDeque::new();
    for (&node, &deg) in &indegree {
        if deg == 0 {
            queue.push_back(node);
        }
    }

    let mut visited = 0usize;
    while let Some(node) = queue.pop_front() {
        visited += 1;
        if let Some(deps) = edges.get(&node) {
            for &dst in deps {
                decrement_indegree_and_enqueue(&mut indegree, &mut queue, dst);
            }
        }
    }

    visited != indegree.len()
}

pub(crate) fn no_state_runtime_direct_assignment<'a>(
    dae: &Dae,
    eq: &'a Equation,
    name_to_idx: &std::collections::HashMap<String, usize>,
) -> Option<(String, &'a Expression)> {
    if eq.origin == "orphaned_variable_pin" {
        return None;
    }

    let Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(_),
        lhs,
        rhs,
    } = &eq.rhs
    else {
        return eq.lhs.as_ref().and_then(|lhs| {
            let lhs_size = rumoca_sim::runtime::assignment::variable_size_for_assignment_name(
                dae,
                lhs.as_str(),
            )?;
            (lhs_size <= 1).then(|| (lhs.as_str().to_string(), &eq.rhs))
        });
    };

    let lhs_key = match lhs.as_ref() {
        Expression::VarRef { name, subscripts } => {
            rumoca_sim::runtime::assignment::canonical_var_ref_key(name, subscripts)
        }
        _ => None,
    };
    let rhs_key = match rhs.as_ref() {
        Expression::VarRef { name, subscripts } => {
            rumoca_sim::runtime::assignment::canonical_var_ref_key(name, subscripts)
        }
        _ => None,
    };
    if let (Some(lhs_key), Some(rhs_key)) = (&lhs_key, &rhs_key) {
        let lhs_solver = solver_idx_for_target(lhs_key.as_str(), name_to_idx).is_some();
        let rhs_solver = solver_idx_for_target(rhs_key.as_str(), name_to_idx).is_some();
        match (lhs_solver, rhs_solver) {
            (true, false)
                if rumoca_sim::runtime::assignment::is_runtime_unknown_name(
                    dae,
                    rhs_key.as_str(),
                ) =>
            {
                return Some((rhs_key.clone(), lhs.as_ref()));
            }
            (false, true)
                if rumoca_sim::runtime::assignment::is_runtime_unknown_name(
                    dae,
                    lhs_key.as_str(),
                ) =>
            {
                return Some((lhs_key.clone(), rhs.as_ref()));
            }
            _ => {}
        }
    }

    if let Some((target, solution)) = extract_direct_assignment(&eq.rhs) {
        let target_size = rumoca_sim::runtime::assignment::variable_size_for_assignment_name(
            dae,
            target.as_str(),
        )?;
        if !target.contains('[') && target_size > 1 {
            return None;
        }
        return Some((target, solution));
    }

    eq.lhs.as_ref().and_then(|lhs| {
        let lhs_size =
            rumoca_sim::runtime::assignment::variable_size_for_assignment_name(dae, lhs.as_str())?;
        (lhs_size <= 1).then(|| (lhs.as_str().to_string(), &eq.rhs))
    })
}

pub(crate) fn direct_assignment_name_graph_has_cycle(
    edges: &std::collections::HashMap<String, Vec<String>>,
) -> bool {
    let mut indegree: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    for (src, deps) in edges {
        indegree.entry(src.clone()).or_insert(0);
        for dst in deps {
            *indegree.entry(dst.clone()).or_insert(0) += 1;
        }
    }

    let mut queue = std::collections::VecDeque::new();
    for (node, deg) in &indegree {
        if *deg == 0 {
            queue.push_back(node.clone());
        }
    }

    let mut visited = 0usize;
    while let Some(node) = queue.pop_front() {
        visited += 1;
        if let Some(deps) = edges.get(&node) {
            for dst in deps {
                decrement_named_indegree_and_enqueue(&mut indegree, &mut queue, dst);
            }
        }
    }

    visited != indegree.len()
}

pub(crate) fn no_state_runtime_function_call_supported(name: &str) -> bool {
    let short = name.rsplit('.').next().unwrap_or(name);
    if matches!(
        short,
        // MLS §8.6: no-state continuous observation points may refresh pure
        // table interpolation helpers between sample points without Newton
        // projection. These getters depend only on the current time and table
        // parameters; they do not carry event/history state.
        "getTimeTableValueNoDer"
            | "getTimeTableValueNoDer2"
            | "getTimeTableValue"
            | "getTable1DValueNoDer"
            | "getTable1DValueNoDer2"
            | "getTable1DValue"
            | "getTimeTableTmax"
            | "getTimeTableTmin"
            | "getTable1DAbscissaUmax"
            | "getTable1DAbscissaUmin"
    ) {
        return true;
    }
    matches!(
        short,
        // MLS Chapter 12 function calls and Complex operator functions are
        // pure algebraic helpers here. They do not carry event/history state,
        // so the no-state direct-assignment refresh path can reevaluate them
        // between sample points without Newton projection.
        "Complex"
            | "conj"
            | "powerOfJ"
            | "abs"
            | "sign"
            | "sqrt"
            | "sin"
            | "cos"
            | "tan"
            | "asin"
            | "acos"
            | "atan"
            | "atan2"
            | "sinh"
            | "cosh"
            | "tanh"
            | "exp"
            | "log"
            | "log10"
            | "min"
            | "max"
            | "previous"
            | "hold"
            | "noClock"
            | "firstTick"
            | "Clock"
            | "subSample"
            | "superSample"
            | "shiftSample"
            | "backSample"
    )
}

pub(crate) fn no_state_runtime_builtin_supported(function: BuiltinFunction) -> bool {
    !matches!(
        function,
        // MLS §3.7.2 / §8.6 / §16.5.1: these builtins depend on event, clock,
        // or relation-triggered state. The no-state direct-refresh path cannot
        // treat them as pure algebraic helpers between observation points.
        BuiltinFunction::Div
            | BuiltinFunction::Floor
            | BuiltinFunction::Ceil
            | BuiltinFunction::Integer
            | BuiltinFunction::Pre
            | BuiltinFunction::Sample
            | BuiltinFunction::Edge
            | BuiltinFunction::Change
            | BuiltinFunction::Reinit
            | BuiltinFunction::Initial
    )
}

pub(crate) fn no_state_solution_is_fast_refresh_safe(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(_) | Expression::VarRef { .. } | Expression::Empty => true,
        Expression::Unary { rhs, .. } | Expression::FieldAccess { base: rhs, .. } => {
            no_state_solution_is_fast_refresh_safe(rhs)
        }
        Expression::Binary { lhs, rhs, .. } => {
            no_state_solution_is_fast_refresh_safe(lhs)
                && no_state_solution_is_fast_refresh_safe(rhs)
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().all(|(condition, value)| {
                no_state_solution_is_fast_refresh_safe(condition)
                    && no_state_solution_is_fast_refresh_safe(value)
            }) && no_state_solution_is_fast_refresh_safe(else_branch)
        }
        Expression::BuiltinCall { function, args } => {
            no_state_runtime_builtin_supported(*function)
                && args.iter().all(no_state_solution_is_fast_refresh_safe)
        }
        Expression::FunctionCall { name, args, .. } => {
            no_state_runtime_function_call_supported(name.as_str())
                && args.iter().all(no_state_solution_is_fast_refresh_safe)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().all(no_state_solution_is_fast_refresh_safe)
        }
        Expression::Range { start, step, end } => {
            no_state_solution_is_fast_refresh_safe(start)
                && step
                    .as_deref()
                    .is_none_or(no_state_solution_is_fast_refresh_safe)
                && no_state_solution_is_fast_refresh_safe(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            no_state_solution_is_fast_refresh_safe(expr)
                && indices
                    .iter()
                    .all(|index| no_state_solution_is_fast_refresh_safe(&index.range))
                && filter
                    .as_deref()
                    .is_none_or(no_state_solution_is_fast_refresh_safe)
        }
        Expression::Index { base, subscripts } => {
            no_state_solution_is_fast_refresh_safe(base)
                && subscripts.iter().all(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => no_state_solution_is_fast_refresh_safe(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => true,
                })
        }
    }
}

pub(crate) fn decrement_named_indegree_and_enqueue(
    indegree: &mut std::collections::HashMap<String, usize>,
    queue: &mut std::collections::VecDeque<String>,
    dst: &str,
) {
    let Some(deg) = indegree.get_mut(dst) else {
        return;
    };
    *deg -= 1;
    if *deg == 0 {
        queue.push_back(dst.to_string());
    }
}

pub(crate) fn solver_target_has_runtime_alias_anchor(
    target: &str,
    adjacency: &std::collections::HashMap<String, Vec<String>>,
    runtime_anchors: &std::collections::HashSet<String>,
    assigned_targets: &std::collections::HashSet<String>,
) -> bool {
    let mut component_roots = vec![target.to_string()];
    if !target.contains('[') {
        component_roots.push(format!("{target}[1]"));
    }
    for root in component_roots {
        if !adjacency.contains_key(root.as_str()) {
            continue;
        }
        let mut visited = std::collections::HashSet::from([root.clone()]);
        if rumoca_sim::runtime::alias::collect_alias_component(
            root.as_str(),
            adjacency,
            &mut visited,
        )
        .iter()
        .any(|name| runtime_anchors.contains(name) || assigned_targets.contains(name))
        {
            return true;
        }
    }
    false
}

pub(crate) fn runtime_settle_materializes_name(dae: &Dae, target: &str) -> bool {
    // MLS Appendix B / §8.6 / §16.5.1: no-state event iteration and clocked
    // discrete settle already materialize z/m targets on the runtime path. If
    // a solver-backed visible name is updated there, it does not by itself
    // require Newton projection.
    dae.f_z
        .iter()
        .chain(dae.f_m.iter())
        .filter_map(|eq| eq.lhs.as_ref())
        .any(|lhs| lhs.as_str() == target)
}

#[cfg(test)]
pub(crate) fn decrement_indegree_and_enqueue(
    indegree: &mut std::collections::HashMap<usize, usize>,
    queue: &mut std::collections::VecDeque<usize>,
    dst: usize,
) {
    let Some(deg) = indegree.get_mut(&dst) else {
        return;
    };
    *deg -= 1;
    if *deg == 0 {
        queue.push_back(dst);
    }
}

#[cfg(test)]
pub(crate) fn runtime_projection_required(dae: &Dae, n_x: usize) -> bool {
    let n_total = dae.f_x.len();
    if n_x >= n_total {
        return false;
    }

    let names = solver_vector_names(dae, n_total);
    let name_to_idx: std::collections::HashMap<String, usize> = names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect();
    let mut assigned_targets: std::collections::HashSet<usize> = std::collections::HashSet::new();
    let mut edges: std::collections::HashMap<usize, Vec<usize>> = std::collections::HashMap::new();

    for eq in dae.f_x.iter().skip(n_x) {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let Some((target, solution)) = runtime_direct_assignment(dae, eq) else {
            return true;
        };
        let Some(target_idx) = solver_idx_for_target(target.as_str(), &name_to_idx) else {
            return true;
        };
        if target_idx < n_x || target_idx >= n_total {
            return true;
        }
        if !assigned_targets.insert(target_idx) {
            return true;
        }

        let mut refs = std::collections::HashSet::new();
        solution.collect_var_refs(&mut refs);
        for ref_name in refs {
            let Some(dep_idx) = solver_idx_for_target(ref_name.as_str(), &name_to_idx) else {
                continue;
            };
            if dep_idx < n_x || dep_idx >= n_total {
                continue;
            }
            if dep_idx == target_idx {
                return true;
            }
            edges.entry(target_idx).or_default().push(dep_idx);
        }
    }

    for idx in n_x..n_total {
        let is_pinned = dae
            .f_x
            .get(idx)
            .is_some_and(|eq| eq.origin == "orphaned_variable_pin");
        if !is_pinned && !assigned_targets.contains(&idx) {
            return true;
        }
    }

    direct_assignment_graph_has_cycle(&edges)
}

pub(crate) fn no_state_runtime_projection_required(dae: &Dae, n_x: usize) -> bool {
    let n_total = dae.f_x.len();
    if n_x >= n_total {
        return false;
    }

    let names = solver_vector_names(dae, n_total);
    let solver_len = names.len();
    let name_to_idx: std::collections::HashMap<String, usize> = names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect();
    let target_assignment_stats =
        rumoca_sim::runtime::assignment::collect_direct_assignment_target_stats(dae, n_x, false);
    let alias_adjacency =
        rumoca_sim::runtime::alias::build_runtime_alias_adjacency_with_known_assignments(dae, n_x);
    let runtime_anchors = rumoca_sim::runtime::alias::collect_runtime_alias_anchor_names(dae, n_x);
    let Some(info) = collect_no_state_runtime_assignment_info(
        dae,
        n_x,
        solver_len,
        &name_to_idx,
        &target_assignment_stats,
    ) else {
        return true;
    };
    let projection_sources = RuntimeProjectionSupport {
        alias_adjacency: &alias_adjacency,
        runtime_anchors: &runtime_anchors,
        assigned_targets: &info.assigned_targets,
    };
    if solver_targets_need_projection(
        dae,
        n_x,
        solver_len,
        &names,
        &projection_sources,
        &info.assigned_solver_targets,
    ) {
        return true;
    }
    if visible_non_solver_targets_need_projection(
        dae,
        solver_len,
        &name_to_idx,
        &projection_sources,
    ) {
        return true;
    }
    runtime_assignments_have_cycle(&info.assigned_targets, &info.assignments)
}

pub(crate) struct NoStateRuntimeAssignmentInfo<'a> {
    assigned_solver_targets: std::collections::HashSet<usize>,
    assigned_targets: std::collections::HashSet<String>,
    assignments: Vec<(String, &'a Expression)>,
}

pub(crate) fn collect_no_state_runtime_assignment_info<'a>(
    dae: &'a Dae,
    n_x: usize,
    solver_len: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    target_assignment_stats: &std::collections::HashMap<
        String,
        rumoca_sim::runtime::assignment::DirectAssignmentTargetStats,
    >,
) -> Option<NoStateRuntimeAssignmentInfo<'a>> {
    let mut assigned_solver_targets = std::collections::HashSet::new();
    let mut assigned_targets = std::collections::HashSet::new();
    let mut assignments = Vec::new();
    for eq in dae.f_x.iter().skip(n_x) {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let (target, solution) = no_state_runtime_direct_assignment(dae, eq, name_to_idx)?;
        let target_stats = target_assignment_stats
            .get(target.as_str())
            .copied()
            .unwrap_or_default();
        if target_stats.total > 1
            && target_stats.non_alias == 1
            && rumoca_sim::runtime::assignment::assignment_solution_is_alias_varref(dae, solution)
        {
            continue;
        }
        if !no_state_assignment_is_projection_safe(
            dae,
            solution,
            target.as_str(),
            target_assignment_stats,
        ) {
            return None;
        }
        if !assigned_targets.insert(target.clone()) {
            return None;
        }
        if let Some(target_idx) = solver_idx_for_target(target.as_str(), name_to_idx) {
            if target_idx < n_x || target_idx >= solver_len {
                return None;
            }
            assigned_solver_targets.insert(target_idx);
        }
        assignments.push((target, solution));
    }
    Some(NoStateRuntimeAssignmentInfo {
        assigned_solver_targets,
        assigned_targets,
        assignments,
    })
}

pub(crate) fn no_state_assignment_is_projection_safe(
    dae: &Dae,
    solution: &Expression,
    target: &str,
    target_assignment_stats: &std::collections::HashMap<
        String,
        rumoca_sim::runtime::assignment::DirectAssignmentTargetStats,
    >,
) -> bool {
    if !no_state_solution_is_fast_refresh_safe(solution) {
        return false;
    }
    let is_alias_solution =
        rumoca_sim::runtime::assignment::assignment_solution_is_alias_varref(dae, solution);
    let target_stats = target_assignment_stats
        .get(target)
        .copied()
        .unwrap_or_default();
    if target_stats.total > 1 && target_stats.non_alias != 1 {
        return false;
    }
    !(target_stats.total > 1 && is_alias_solution)
}

pub(crate) struct RuntimeProjectionSupport<'a> {
    alias_adjacency: &'a std::collections::HashMap<String, Vec<String>>,
    runtime_anchors: &'a std::collections::HashSet<String>,
    assigned_targets: &'a std::collections::HashSet<String>,
}

pub(crate) fn solver_targets_need_projection(
    dae: &Dae,
    n_x: usize,
    solver_len: usize,
    names: &[String],
    support: &RuntimeProjectionSupport<'_>,
    assigned_solver_targets: &std::collections::HashSet<usize>,
) -> bool {
    for idx in n_x..solver_len {
        if assigned_solver_targets.contains(&idx) {
            continue;
        }
        let Some(name) = names.get(idx) else {
            return true;
        };
        if runtime_settle_materializes_name(dae, name) {
            continue;
        }
        if !solver_target_has_runtime_alias_anchor(
            name,
            support.alias_adjacency,
            support.runtime_anchors,
            support.assigned_targets,
        ) {
            return true;
        }
    }
    false
}

pub(crate) fn visible_non_solver_targets_need_projection(
    dae: &Dae,
    solver_len: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    support: &RuntimeProjectionSupport<'_>,
) -> bool {
    for name in dae
        .outputs
        .keys()
        .chain(dae.discrete_reals.keys())
        .chain(dae.discrete_valued.keys())
        .map(|name| name.as_str())
    {
        if solver_idx_for_target(name, name_to_idx).is_some_and(|idx| idx < solver_len) {
            continue;
        }
        if support.assigned_targets.contains(name)
            || support.runtime_anchors.contains(name)
            || solver_target_has_runtime_alias_anchor(
                name,
                support.alias_adjacency,
                support.runtime_anchors,
                support.assigned_targets,
            )
        {
            continue;
        }
        return true;
    }
    false
}

pub(crate) fn runtime_assignments_have_cycle(
    assigned_targets: &std::collections::HashSet<String>,
    assignments: &[(String, &Expression)],
) -> bool {
    let mut edges: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();
    for (target, solution) in assignments {
        let mut refs = std::collections::HashSet::new();
        solution.collect_var_refs(&mut refs);
        for ref_name in refs {
            let dependency = ref_name.as_str();
            if !assigned_targets.contains(dependency) {
                continue;
            }
            if dependency == target {
                return true;
            }
            edges
                .entry(target.clone())
                .or_default()
                .push(dependency.to_string());
        }
    }
    direct_assignment_name_graph_has_cycle(&edges)
}

#[cfg(test)]
pub(crate) fn seed_runtime_direct_assignments(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    t_eval: f64,
) -> usize {
    seed_runtime_direct_assignment_values(dae, y, p, n_x, t_eval)
}
