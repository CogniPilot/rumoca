use super::*;

pub(crate) type SharedInputOverrides =
    std::rc::Rc<std::cell::RefCell<std::collections::HashMap<String, f64>>>;

#[derive(Clone)]
pub(crate) struct CompiledEvalContext {
    dae: Dae,
    sim_context: rumoca_sim_core::runtime::layout::SimulationContext,
    pub(crate) input_overrides: Option<SharedInputOverrides>,
    param_scratch: std::rc::Rc<std::cell::RefCell<Vec<f64>>>,
}

pub(crate) fn build_compiled_eval_context(dae: &Dae, n_total: usize) -> CompiledEvalContext {
    let sim_context = rumoca_sim_core::runtime::layout::SimulationContext::from_dae(dae, n_total);
    CompiledEvalContext {
        dae: dae.clone(),
        sim_context,
        input_overrides: None,
        param_scratch: std::rc::Rc::new(std::cell::RefCell::new(Vec::new())),
    }
}

#[derive(Clone)]
struct HiddenDirectAssignmentSubstitution {
    target: String,
    expr: dae::Expression,
    preserve_defining_row: bool,
}

fn expr_exact_var_ref_key(expr: &dae::Expression) -> Option<String> {
    let dae::Expression::VarRef { name, subscripts } = expr else {
        return None;
    };
    rumoca_sim_core::runtime::assignment::canonical_var_ref_key(name, subscripts)
}

fn dae_has_compiled_binding(dae: &Dae, name: &str, solver_len: usize) -> bool {
    rumoca_sim_core::phase_solve_lower::build_var_layout_with_solver_len(dae, solver_len)
        .binding(name)
        .is_some()
}

fn assignment_variable_dims(dae: &Dae, name: &str) -> Option<Vec<i64>> {
    let base = dae::component_base_name(name).unwrap_or_else(|| name.to_string());
    dae.states
        .get(&dae::VarName::new(base.clone()))
        .or_else(|| dae.algebraics.get(&dae::VarName::new(base.clone())))
        .or_else(|| dae.outputs.get(&dae::VarName::new(base.clone())))
        .or_else(|| dae.inputs.get(&dae::VarName::new(base.clone())))
        .or_else(|| dae.parameters.get(&dae::VarName::new(base.clone())))
        .or_else(|| dae.constants.get(&dae::VarName::new(base.clone())))
        .or_else(|| dae.discrete_reals.get(&dae::VarName::new(base.clone())))
        .or_else(|| dae.discrete_valued.get(&dae::VarName::new(base)))
        .map(|var| var.dims.clone())
}

fn flat_index_to_subscripts(flat_index: usize, dims: &[i64]) -> Option<Vec<usize>> {
    if dims.is_empty() {
        return None;
    }
    let mut dims_usize = Vec::with_capacity(dims.len());
    for &dim in dims {
        let dim = usize::try_from(dim).ok()?;
        if dim == 0 {
            return None;
        }
        dims_usize.push(dim);
    }

    let mut remainder = flat_index;
    let mut subs_rev = Vec::with_capacity(dims_usize.len());
    for dim in dims_usize.iter().rev().copied() {
        subs_rev.push((remainder % dim) + 1);
        remainder /= dim;
    }
    if remainder != 0 {
        return None;
    }
    subs_rev.reverse();
    Some(subs_rev)
}

fn format_subscript_key(name: &str, subscripts: &[usize]) -> String {
    let joined = subscripts
        .iter()
        .map(|subscript| subscript.to_string())
        .collect::<Vec<_>>()
        .join(",");
    format!("{name}[{joined}]")
}

fn flat_scalar_substitution_alias(dae: &Dae, target: &str) -> Option<String> {
    let base = dae::component_base_name(target)?;
    let dims = assignment_variable_dims(dae, base.as_str())?;
    if dims.len() <= 1 {
        return None;
    }
    let open = target.rfind('[')?;
    let close = target.rfind(']')?;
    if close <= open || close != target.len() - 1 {
        return None;
    }
    let index_text = &target[open + 1..close];
    if index_text.contains(',') {
        return None;
    }
    let flat_index = index_text.parse::<usize>().ok()?.checked_sub(1)?;
    let subscripts = flat_index_to_subscripts(flat_index, &dims)?;
    (subscripts.len() > 1).then(|| format_subscript_key(base.as_str(), &subscripts))
}

fn subscripts_to_flat_index(subscripts: &[usize], dims: &[i64]) -> Option<usize> {
    if subscripts.len() != dims.len() {
        return None;
    }
    let mut flat = 0usize;
    for (idx, (&subscript, &dim)) in subscripts.iter().zip(dims).enumerate() {
        let dim = usize::try_from(dim).ok()?;
        if subscript == 0 || subscript > dim {
            return None;
        }
        flat = if idx == 0 {
            subscript - 1
        } else {
            flat.checked_mul(dim)?.checked_add(subscript - 1)?
        };
    }
    Some(flat)
}

fn indexed_key_base_and_flat(dae: &Dae, key: &str) -> Option<(String, usize)> {
    let base = dae::component_base_name(key)?;
    let dims = assignment_variable_dims(dae, base.as_str())?;
    let open = key.rfind('[')?;
    let close = key.rfind(']')?;
    if close <= open || close != key.len() - 1 {
        return None;
    }
    let index_text = &key[open + 1..close];
    let indices = index_text
        .split(',')
        .map(|part| part.trim().parse::<usize>().ok())
        .collect::<Option<Vec<_>>>()?;
    let flat = if indices.len() == 1 {
        indices[0].checked_sub(1)?
    } else {
        subscripts_to_flat_index(&indices, &dims)?
    };
    Some((base, flat))
}

fn var_ref_for_linear_index(base: &str, dims: &[i64], flat_index: usize) -> dae::Expression {
    let subscripts = flat_index_to_subscripts(flat_index, dims)
        .unwrap_or_else(|| vec![flat_index.saturating_add(1)]);
    dae::Expression::VarRef {
        name: dae::VarName::new(base),
        subscripts: subscripts
            .into_iter()
            .map(|index| dae::Subscript::Index(index as i64))
            .collect(),
    }
}

fn push_array_alias_substitutions(
    substitutions: &mut Vec<HiddenDirectAssignmentSubstitution>,
    dae: &Dae,
    target: &str,
    source: &str,
    preserve_defining_row: bool,
) -> bool {
    let Some((target_base, target_flat)) = indexed_key_base_and_flat(dae, target) else {
        return false;
    };
    let Some((source_base, source_flat)) = indexed_key_base_and_flat(dae, source) else {
        return false;
    };
    if target_flat != 0 || source_flat != 0 || target_base == source_base {
        return false;
    }
    let Some(target_dims) = assignment_variable_dims(dae, target_base.as_str()) else {
        return false;
    };
    let Some(source_dims) = assignment_variable_dims(dae, source_base.as_str()) else {
        return false;
    };
    if target_dims != source_dims {
        return false;
    }
    let Some(size) = target_dims.iter().try_fold(1usize, |acc, &dim| {
        usize::try_from(dim)
            .ok()
            .and_then(|dim| acc.checked_mul(dim))
    }) else {
        return false;
    };
    if size <= 1 {
        return false;
    }
    for idx in 0..size {
        let source_expr = var_ref_for_linear_index(source_base.as_str(), &source_dims, idx);
        substitutions.push(HiddenDirectAssignmentSubstitution {
            target: format!("{}[{}]", target_base, idx + 1),
            expr: source_expr.clone(),
            preserve_defining_row,
        });
        if let Some(subscripts) = flat_index_to_subscripts(idx, &target_dims)
            && subscripts.len() > 1
        {
            substitutions.push(HiddenDirectAssignmentSubstitution {
                target: format_subscript_key(target_base.as_str(), &subscripts),
                expr: source_expr,
                preserve_defining_row,
            });
        }
    }
    true
}

fn expr_contains_exact_var_ref(expr: &dae::Expression, target: &str) -> bool {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            rumoca_sim_core::runtime::assignment::canonical_var_ref_key(name, subscripts)
                .is_some_and(|key| key == target)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_exact_var_ref(lhs, target) || expr_contains_exact_var_ref(rhs, target)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_exact_var_ref(rhs, target),
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter()
                .any(|arg| expr_contains_exact_var_ref(arg, target))
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_contains_exact_var_ref(cond, target)
                    || expr_contains_exact_var_ref(value, target)
            }) || expr_contains_exact_var_ref(else_branch, target)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(|element| expr_contains_exact_var_ref(element, target)),
        dae::Expression::Range { start, step, end } => {
            expr_contains_exact_var_ref(start, target)
                || step
                    .as_deref()
                    .is_some_and(|expr| expr_contains_exact_var_ref(expr, target))
                || expr_contains_exact_var_ref(end, target)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_exact_var_ref(expr, target)
                || indices
                    .iter()
                    .any(|index| expr_contains_exact_var_ref(&index.range, target))
                || filter
                    .as_deref()
                    .is_some_and(|expr| expr_contains_exact_var_ref(expr, target))
        }
        dae::Expression::Index { base, subscripts } => {
            expr_contains_exact_var_ref(base, target)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_contains_exact_var_ref(expr, target),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_contains_exact_var_ref(base, target),
        dae::Expression::Literal(_) | dae::Expression::Empty => false,
    }
}

fn expr_contains_event_or_clock_operator(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            matches!(
                function,
                dae::BuiltinFunction::Pre
                    | dae::BuiltinFunction::Edge
                    | dae::BuiltinFunction::Change
                    | dae::BuiltinFunction::Sample
                    | dae::BuiltinFunction::NoEvent
                    | dae::BuiltinFunction::Smooth
            ) || args.iter().any(expr_contains_event_or_clock_operator)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_event_or_clock_operator(lhs) || expr_contains_event_or_clock_operator(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_event_or_clock_operator(rhs),
        dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_contains_event_or_clock_operator)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_contains_event_or_clock_operator(cond)
                    || expr_contains_event_or_clock_operator(value)
            }) || expr_contains_event_or_clock_operator(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_contains_event_or_clock_operator)
        }
        dae::Expression::Range { start, step, end } => {
            expr_contains_event_or_clock_operator(start)
                || step
                    .as_deref()
                    .is_some_and(expr_contains_event_or_clock_operator)
                || expr_contains_event_or_clock_operator(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_event_or_clock_operator(expr)
                || indices
                    .iter()
                    .any(|index| expr_contains_event_or_clock_operator(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_contains_event_or_clock_operator)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_contains_event_or_clock_operator(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_contains_event_or_clock_operator(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_contains_event_or_clock_operator(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn expr_contains_derivative_operator(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            *function == dae::BuiltinFunction::Der
                || args.iter().any(expr_contains_derivative_operator)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_derivative_operator(lhs) || expr_contains_derivative_operator(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_derivative_operator(rhs),
        dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_contains_derivative_operator)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_contains_derivative_operator(cond) || expr_contains_derivative_operator(value)
            }) || expr_contains_derivative_operator(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_contains_derivative_operator)
        }
        dae::Expression::Range { start, step, end } => {
            expr_contains_derivative_operator(start)
                || step
                    .as_deref()
                    .is_some_and(expr_contains_derivative_operator)
                || expr_contains_derivative_operator(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_derivative_operator(expr)
                || indices
                    .iter()
                    .any(|index| expr_contains_derivative_operator(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_contains_derivative_operator)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_contains_derivative_operator(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_contains_derivative_operator(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_contains_derivative_operator(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn hidden_direct_assignment_target_matches(
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
    target: &str,
) -> bool {
    rumoca_sim_core::runtime::assignment::canonical_var_ref_key(name, subscripts)
        .is_some_and(|key| key == target)
}

fn rewrite_hidden_direct_assignment_exprs(
    exprs: &[dae::Expression],
    target: &str,
    replacement: &dae::Expression,
) -> Vec<dae::Expression> {
    exprs
        .iter()
        .map(|expr| rewrite_hidden_direct_assignment_expr(expr, target, replacement))
        .collect()
}

fn rewrite_hidden_direct_assignment_subscripts(
    subscripts: &[dae::Subscript],
    target: &str,
    replacement: &dae::Expression,
) -> Vec<dae::Subscript> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            dae::Subscript::Expr(expr) => dae::Subscript::Expr(Box::new(
                rewrite_hidden_direct_assignment_expr(expr, target, replacement),
            )),
            dae::Subscript::Index(index) => dae::Subscript::Index(*index),
            dae::Subscript::Colon => dae::Subscript::Colon,
        })
        .collect()
}

fn rewrite_hidden_direct_assignment_if_branches(
    branches: &[(dae::Expression, dae::Expression)],
    target: &str,
    replacement: &dae::Expression,
) -> Vec<(dae::Expression, dae::Expression)> {
    branches
        .iter()
        .map(|(cond, value)| {
            (
                rewrite_hidden_direct_assignment_expr(cond, target, replacement),
                rewrite_hidden_direct_assignment_expr(value, target, replacement),
            )
        })
        .collect()
}

fn rewrite_hidden_direct_assignment_comprehension_indices(
    indices: &[dae::ComprehensionIndex],
    target: &str,
    replacement: &dae::Expression,
) -> Vec<dae::ComprehensionIndex> {
    indices
        .iter()
        .map(|index| dae::ComprehensionIndex {
            name: index.name.clone(),
            range: rewrite_hidden_direct_assignment_expr(&index.range, target, replacement),
        })
        .collect()
}

fn rewrite_hidden_direct_assignment_binary(
    op: &rumoca_sim_core::ir_core::OpBinary,
    lhs: &dae::Expression,
    rhs: &dae::Expression,
    target: &str,
    replacement: &dae::Expression,
) -> dae::Expression {
    dae::Expression::Binary {
        op: op.clone(),
        lhs: Box::new(rewrite_hidden_direct_assignment_expr(
            lhs,
            target,
            replacement,
        )),
        rhs: Box::new(rewrite_hidden_direct_assignment_expr(
            rhs,
            target,
            replacement,
        )),
    }
}

fn rewrite_hidden_direct_assignment_unary(
    op: &rumoca_sim_core::ir_core::OpUnary,
    rhs: &dae::Expression,
    target: &str,
    replacement: &dae::Expression,
) -> dae::Expression {
    dae::Expression::Unary {
        op: op.clone(),
        rhs: Box::new(rewrite_hidden_direct_assignment_expr(
            rhs,
            target,
            replacement,
        )),
    }
}

fn rewrite_hidden_direct_assignment_range(
    start: &dae::Expression,
    step: Option<&dae::Expression>,
    end: &dae::Expression,
    target: &str,
    replacement: &dae::Expression,
) -> dae::Expression {
    dae::Expression::Range {
        start: Box::new(rewrite_hidden_direct_assignment_expr(
            start,
            target,
            replacement,
        )),
        step: step.map(|expr| {
            Box::new(rewrite_hidden_direct_assignment_expr(
                expr,
                target,
                replacement,
            ))
        }),
        end: Box::new(rewrite_hidden_direct_assignment_expr(
            end,
            target,
            replacement,
        )),
    }
}

fn rewrite_hidden_direct_assignment_array_comprehension(
    expr: &dae::Expression,
    indices: &[dae::ComprehensionIndex],
    filter: Option<&dae::Expression>,
    target: &str,
    replacement: &dae::Expression,
) -> dae::Expression {
    dae::Expression::ArrayComprehension {
        expr: Box::new(rewrite_hidden_direct_assignment_expr(
            expr,
            target,
            replacement,
        )),
        indices: rewrite_hidden_direct_assignment_comprehension_indices(
            indices,
            target,
            replacement,
        ),
        filter: filter.map(|expr| {
            Box::new(rewrite_hidden_direct_assignment_expr(
                expr,
                target,
                replacement,
            ))
        }),
    }
}

fn rewrite_hidden_direct_assignment_index(
    base: &dae::Expression,
    subscripts: &[dae::Subscript],
    target: &str,
    replacement: &dae::Expression,
) -> dae::Expression {
    dae::Expression::Index {
        base: Box::new(rewrite_hidden_direct_assignment_expr(
            base,
            target,
            replacement,
        )),
        subscripts: rewrite_hidden_direct_assignment_subscripts(subscripts, target, replacement),
    }
}

fn rewrite_hidden_direct_assignment_field_access(
    base: &dae::Expression,
    field: &str,
    target: &str,
    replacement: &dae::Expression,
) -> dae::Expression {
    dae::Expression::FieldAccess {
        base: Box::new(rewrite_hidden_direct_assignment_expr(
            base,
            target,
            replacement,
        )),
        field: field.to_string(),
    }
}

fn rewrite_hidden_direct_assignment_expr(
    expr: &dae::Expression,
    target: &str,
    replacement: &dae::Expression,
) -> dae::Expression {
    match expr {
        dae::Expression::VarRef { name, subscripts }
            if hidden_direct_assignment_target_matches(name, subscripts, target) =>
        {
            replacement.clone()
        }
        dae::Expression::Binary { op, lhs, rhs } => {
            rewrite_hidden_direct_assignment_binary(op, lhs, rhs, target, replacement)
        }
        dae::Expression::Unary { op, rhs } => {
            rewrite_hidden_direct_assignment_unary(op, rhs, target, replacement)
        }
        dae::Expression::BuiltinCall { function, args } => dae::Expression::BuiltinCall {
            function: *function,
            args: rewrite_hidden_direct_assignment_exprs(args, target, replacement),
        },
        dae::Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => dae::Expression::FunctionCall {
            name: name.clone(),
            args: rewrite_hidden_direct_assignment_exprs(args, target, replacement),
            is_constructor: *is_constructor,
        },
        dae::Expression::If {
            branches,
            else_branch,
        } => dae::Expression::If {
            branches: rewrite_hidden_direct_assignment_if_branches(branches, target, replacement),
            else_branch: Box::new(rewrite_hidden_direct_assignment_expr(
                else_branch,
                target,
                replacement,
            )),
        },
        dae::Expression::Array {
            elements,
            is_matrix,
        } => dae::Expression::Array {
            elements: rewrite_hidden_direct_assignment_exprs(elements, target, replacement),
            is_matrix: *is_matrix,
        },
        dae::Expression::Tuple { elements } => dae::Expression::Tuple {
            elements: rewrite_hidden_direct_assignment_exprs(elements, target, replacement),
        },
        dae::Expression::Range { start, step, end } => {
            rewrite_hidden_direct_assignment_range(start, step.as_deref(), end, target, replacement)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => rewrite_hidden_direct_assignment_array_comprehension(
            expr,
            indices,
            filter.as_deref(),
            target,
            replacement,
        ),
        dae::Expression::Index { base, subscripts } => {
            rewrite_hidden_direct_assignment_index(base, subscripts, target, replacement)
        }
        dae::Expression::FieldAccess { base, field } => {
            rewrite_hidden_direct_assignment_field_access(base, field, target, replacement)
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            expr.clone()
        }
    }
}

fn apply_hidden_direct_assignment_substitutions(
    dae: &mut Dae,
    substitutions: &[HiddenDirectAssignmentSubstitution],
) {
    if substitutions.is_empty() {
        return;
    }
    for eq in &mut dae.f_x {
        let defining_target =
            rumoca_sim_core::runtime::assignment::direct_assignment_from_equation(eq)
                .map(|(target, _)| target);
        for substitution in substitutions {
            if substitution.preserve_defining_row
                && defining_target.as_deref() == Some(substitution.target.as_str())
            {
                continue;
            }
            if expr_contains_exact_var_ref(&eq.rhs, substitution.target.as_str()) {
                eq.rhs = rewrite_hidden_direct_assignment_expr(
                    &eq.rhs,
                    substitution.target.as_str(),
                    &substitution.expr,
                );
            }
        }
    }
}

fn apply_other_hidden_substitution(
    expr: dae::Expression,
    idx: usize,
    other_idx: usize,
    other: &HiddenDirectAssignmentSubstitution,
) -> dae::Expression {
    if idx == other_idx || !expr_contains_exact_var_ref(&expr, other.target.as_str()) {
        return expr;
    }
    rewrite_hidden_direct_assignment_expr(&expr, other.target.as_str(), &other.expr)
}

fn resolve_hidden_direct_assignment_substitutions(
    substitutions: &mut [HiddenDirectAssignmentSubstitution],
) {
    let max_passes = substitutions.len().max(1);
    for _ in 0..max_passes {
        let mut changed = false;
        for idx in 0..substitutions.len() {
            let mut rewritten = substitutions[idx].expr.clone();
            for (other_idx, other) in substitutions.iter().enumerate() {
                rewritten = apply_other_hidden_substitution(rewritten, idx, other_idx, other);
            }
            if rewritten != substitutions[idx].expr {
                substitutions[idx].expr = rewritten;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
}

fn collect_target_stats_from_equations<'a>(
    dae: &Dae,
    equations: impl IntoIterator<Item = &'a Equation>,
    skip_alias_pairs: bool,
) -> std::collections::HashMap<
    String,
    rumoca_sim_core::runtime::assignment::DirectAssignmentTargetStats,
> {
    let mut stats: std::collections::HashMap<
        String,
        rumoca_sim_core::runtime::assignment::DirectAssignmentTargetStats,
    > = std::collections::HashMap::new();
    for eq in equations {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let Some((target, solution)) =
            rumoca_sim_core::runtime::assignment::direct_assignment_from_equation(eq)
        else {
            continue;
        };
        if expr_contains_derivative_operator(solution) {
            continue;
        }
        let is_alias_solution =
            rumoca_sim_core::runtime::assignment::assignment_solution_is_alias_varref(
                dae, solution,
            );
        if skip_alias_pairs && is_alias_solution {
            continue;
        }
        let entry = stats.entry(target).or_default();
        entry.total += 1;
        if !is_alias_solution {
            entry.non_alias += 1;
        }
    }
    stats
}

fn build_direct_assignment_substitutions(
    dae: &Dae,
    include_known_assignments: bool,
    solver_len: usize,
) -> Vec<HiddenDirectAssignmentSubstitution> {
    let equations = if include_known_assignments {
        rumoca_sim_core::runtime::alias::runtime_assignment_equations(dae, 0).collect::<Vec<_>>()
    } else {
        dae.f_x.iter().collect::<Vec<_>>()
    };
    let target_stats = collect_target_stats_from_equations(dae, equations.iter().copied(), false);
    let scalarization =
        rumoca_sim_core::phase_structural::scalarize::build_expression_scalarization_context(dae);
    let mut substitutions = Vec::new();

    for eq in equations {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let Some((target, solution)) =
            rumoca_sim_core::runtime::assignment::direct_assignment_from_equation(eq)
        else {
            continue;
        };
        let target_has_compiled_binding =
            dae_has_compiled_binding(dae, target.as_str(), solver_len);
        let stats = target_stats
            .get(target.as_str())
            .copied()
            .unwrap_or_default();
        if stats.total != 1
            || expr_contains_event_or_clock_operator(solution)
            || expr_contains_derivative_operator(solution)
        {
            continue;
        }
        if expr_contains_exact_var_ref(solution, target.as_str()) {
            continue;
        }
        if let Some(source_key) = expr_exact_var_ref_key(solution)
            && push_array_alias_substitutions(
                &mut substitutions,
                dae,
                target.as_str(),
                source_key.as_str(),
                target_has_compiled_binding,
            )
        {
            continue;
        }
        let target_size = rumoca_sim_core::runtime::assignment::variable_size_for_assignment_name(
            dae,
            target.as_str(),
        )
        .unwrap_or(1);
        if target_size > 1 && !target.contains('[') {
            let target_dims = assignment_variable_dims(dae, target.as_str()).unwrap_or_default();
            let solution_rows =
                rumoca_sim_core::phase_structural::scalarize::scalarize_expression_rows(
                    solution,
                    target_size,
                    &scalarization,
                );
            for idx in 0..target_size {
                let scalar_solution = solution_rows
                    .get(idx)
                    .cloned()
                    .unwrap_or_else(|| solution.clone());
                substitutions.push(HiddenDirectAssignmentSubstitution {
                    target: format!("{target}[{}]", idx + 1),
                    expr: scalar_solution,
                    preserve_defining_row: target_has_compiled_binding,
                });
                if let Some(subscripts) = flat_index_to_subscripts(idx, &target_dims)
                    && subscripts.len() > 1
                {
                    substitutions.push(HiddenDirectAssignmentSubstitution {
                        target: format_subscript_key(target.as_str(), &subscripts),
                        expr: solution_rows
                            .get(idx)
                            .cloned()
                            .unwrap_or_else(|| solution.clone()),
                        preserve_defining_row: target_has_compiled_binding,
                    });
                }
            }
        } else {
            substitutions.push(HiddenDirectAssignmentSubstitution {
                target: target.clone(),
                expr: solution.clone(),
                preserve_defining_row: target_has_compiled_binding,
            });
            if let Some(alias_target) = flat_scalar_substitution_alias(dae, target.as_str()) {
                substitutions.push(HiddenDirectAssignmentSubstitution {
                    target: alias_target,
                    expr: solution.clone(),
                    preserve_defining_row: target_has_compiled_binding,
                });
            }
        }

        let Some(reverse_target) = expr_exact_var_ref_key(solution) else {
            continue;
        };
        if !include_known_assignments
            || reverse_target == target
            || dae_has_compiled_binding(dae, reverse_target.as_str(), solver_len)
        {
            continue;
        }
        if substitutions
            .iter()
            .any(|substitution| substitution.target == reverse_target)
        {
            continue;
        }
        substitutions.push(HiddenDirectAssignmentSubstitution {
            target: reverse_target,
            expr: dae::Expression::VarRef {
                name: dae::VarName::new(target),
                subscripts: Vec::new(),
            },
            preserve_defining_row: false,
        });
    }

    resolve_hidden_direct_assignment_substitutions(&mut substitutions);
    substitutions
}

fn rewrite_hidden_direct_assignment_expressions(
    expressions: &[dae::Expression],
    substitutions: &[HiddenDirectAssignmentSubstitution],
) -> Vec<dae::Expression> {
    if substitutions.is_empty() {
        return expressions.to_vec();
    }

    expressions
        .iter()
        .map(|expr| {
            substitutions
                .iter()
                .fold(expr.clone(), |rewritten, substitution| {
                    if expr_contains_exact_var_ref(&rewritten, substitution.target.as_str()) {
                        rewrite_hidden_direct_assignment_expr(
                            &rewritten,
                            substitution.target.as_str(),
                            &substitution.expr,
                        )
                    } else {
                        rewritten
                    }
                })
        })
        .collect()
}

pub(super) fn build_compiled_newton_dae(dae: &Dae, solver_len: usize) -> Dae {
    let mut compiled_dae = dae.clone();
    let substitutions = build_direct_assignment_substitutions(dae, false, solver_len);
    apply_hidden_direct_assignment_substitutions(&mut compiled_dae, &substitutions);
    compiled_dae
}

pub(super) fn build_compiled_problem_dae(dae: &Dae, solver_len: usize) -> Dae {
    let (mut compiled_dae, rewritten_roots) = rewrite_expression_context_for_direct_assignments(
        dae,
        &dae.synthetic_root_conditions,
        false,
        solver_len,
    );
    compiled_dae.synthetic_root_conditions = rewritten_roots;
    compiled_dae
}

fn rewrite_expression_context_for_direct_assignments(
    dae: &Dae,
    expressions: &[dae::Expression],
    include_known_assignments: bool,
    solver_len: usize,
) -> (Dae, Vec<dae::Expression>) {
    let substitutions =
        build_direct_assignment_substitutions(dae, include_known_assignments, solver_len);
    let mut compiled_dae = dae.clone();
    if !substitutions.is_empty() {
        apply_hidden_direct_assignment_substitutions(&mut compiled_dae, &substitutions);
    }
    let rewritten_expressions =
        rewrite_hidden_direct_assignment_expressions(expressions, &substitutions);
    (compiled_dae, rewritten_expressions)
}

#[cfg(not(target_arch = "wasm32"))]
fn compile_expression_rows_with_mode(
    dae: &Dae,
    expressions: &[dae::Expression],
    use_initial: bool,
    solver_len: usize,
) -> Result<rumoca_sim_core::phase_solve_lower::CompiledExpressionRows, String> {
    let backend = rumoca_sim_core::phase_solve_lower::Backend::Cranelift;
    if use_initial {
        rumoca_sim_core::phase_solve_lower::compile_initial_expressions_with_solver_len(
            dae,
            expressions,
            backend,
            solver_len,
        )
    } else {
        rumoca_sim_core::phase_solve_lower::compile_expressions_with_solver_len(
            dae,
            expressions,
            backend,
            solver_len,
        )
    }
    .map_err(|err| err.to_string())
}

#[cfg(not(target_arch = "wasm32"))]
fn expression_binding_summary(dae: &Dae, expr: &dae::Expression, solver_len: usize) -> String {
    let layout =
        rumoca_sim_core::phase_solve_lower::build_var_layout_with_solver_len(dae, solver_len);
    let mut refs = std::collections::HashSet::new();
    expr.collect_var_refs(&mut refs);
    let mut refs = refs.into_iter().collect::<Vec<_>>();
    refs.sort_by(|a, b| a.as_str().cmp(b.as_str()));
    refs.into_iter()
        .map(|name| format!("{name}={:?}", layout.binding(name.as_str())))
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(not(target_arch = "wasm32"))]
fn annotate_expression_context_compile_error(
    dae: &Dae,
    expressions: &[dae::Expression],
    use_initial: bool,
    solver_len: usize,
    err: String,
) -> SimError {
    let layout = rumoca_sim_core::phase_solve_lower::build_var_layout(dae);
    for (idx, expr) in expressions.iter().enumerate() {
        if let Err(row_err) = compile_expression_rows_with_mode(
            dae,
            std::slice::from_ref(expr),
            use_initial,
            solver_len,
        ) {
            let binding_note = expr_exact_var_ref_key(expr)
                .map(|name| format!(" binding={:?}", layout.binding(name.as_str())))
                .unwrap_or_default();
            let refs = expression_binding_summary(dae, expr, solver_len);
            return SimError::CompiledEval(format!(
                "{err}; row {idx} failed: {row_err}; refs=[{refs}]; expr={expr:?};{binding_note}"
            ));
        }
    }
    SimError::CompiledEval(err)
}

#[cfg(not(target_arch = "wasm32"))]
fn annotate_residual_compile_error(
    dae: &Dae,
    solver_len: usize,
    err: String,
    use_initial: bool,
) -> SimError {
    let expressions = dae.f_x.iter().map(|eq| eq.rhs.clone()).collect::<Vec<_>>();
    let mut annotated =
        annotate_expression_context_compile_error(dae, &expressions, use_initial, solver_len, err);
    if let SimError::CompiledEval(message) = &mut annotated {
        for (idx, eq) in dae.f_x.iter().enumerate() {
            let marker = format!("row {idx} failed");
            if message.contains(&marker) {
                *message = format!("{message}; origin='{}'", eq.origin);
                break;
            }
        }
    }
    annotated
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct CompiledRuntimeExpressionContext {
    compiled_eval_ctx: CompiledEvalContext,
    compiled_rows: rumoca_sim_core::phase_solve_lower::CompiledExpressionRows,
    row_count: usize,
}

#[cfg(target_arch = "wasm32")]
pub(crate) struct CompiledRuntimeExpressionContext {
    compiled_eval_ctx: CompiledEvalContext,
    compiled_rows: rumoca_sim_core::phase_solve_lower::CompiledExpressionRowsWasm,
    row_count: usize,
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct CompiledRuntimeNewtonContext {
    compiled_eval_ctx_rhs: CompiledEvalContext,
    compiled_eval_ctx_jac: CompiledEvalContext,
    compiled_residual: rumoca_sim_core::phase_solve_lower::CompiledResidual,
    compiled_jacobian: rumoca_sim_core::phase_solve_lower::CompiledJacobianV,
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct CompiledInitialNewtonContext {
    compiled_eval_ctx_rhs: CompiledEvalContext,
    compiled_eval_ctx_jac: CompiledEvalContext,
    compiled_residual: rumoca_sim_core::phase_solve_lower::CompiledExpressionRows,
    compiled_jacobian: rumoca_sim_core::phase_solve_lower::CompiledJacobianV,
}

#[cfg(target_arch = "wasm32")]
pub(crate) struct CompiledRuntimeNewtonContext {
    compiled_eval_ctx_rhs: CompiledEvalContext,
    compiled_eval_ctx_jac: CompiledEvalContext,
    compiled_residual: rumoca_sim_core::phase_solve_lower::CompiledResidualWasm,
    compiled_jacobian: rumoca_sim_core::phase_solve_lower::CompiledJacobianVWasm,
}

#[cfg(target_arch = "wasm32")]
pub(crate) struct CompiledInitialNewtonContext {
    compiled_eval_ctx_rhs: CompiledEvalContext,
    compiled_eval_ctx_jac: CompiledEvalContext,
    compiled_residual: rumoca_sim_core::phase_solve_lower::CompiledExpressionRowsWasm,
    compiled_jacobian: rumoca_sim_core::phase_solve_lower::CompiledJacobianVWasm,
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct CompiledSyntheticRootContext {
    compiled_eval_ctx_root: CompiledEvalContext,
    compiled_root_conditions: rumoca_sim_core::phase_solve_lower::CompiledExpressionRows,
}

#[cfg(target_arch = "wasm32")]
pub(crate) struct CompiledSyntheticRootContext {
    compiled_eval_ctx_root: CompiledEvalContext,
    compiled_root_conditions: rumoca_sim_core::phase_solve_lower::CompiledExpressionRowsWasm,
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn build_compiled_runtime_newton_context(
    dae: &Dae,
    n_total: usize,
) -> Result<CompiledRuntimeNewtonContext, SimError> {
    let compiled_dae = build_compiled_newton_dae(dae, n_total);
    let compiled_eval_ctx = build_compiled_eval_context(&compiled_dae, n_total);
    let compiled_eval_ctx_rhs = compiled_eval_ctx.clone();
    let compiled_eval_ctx_jac = compiled_eval_ctx.clone();
    let compiled_residual = rumoca_sim_core::phase_solve_lower::compile_residual_with_solver_len(
        &compiled_dae,
        rumoca_sim_core::phase_solve_lower::Backend::Cranelift,
        n_total,
    )
    .map_err(|err| {
        annotate_residual_compile_error(&compiled_dae, n_total, err.to_string(), false)
    })?;
    let compiled_jacobian = rumoca_sim_core::phase_solve_lower::compile_jacobian_v_with_solver_len(
        &compiled_dae,
        rumoca_sim_core::phase_solve_lower::Backend::Cranelift,
        n_total,
    )
    .map_err(|err| SimError::CompiledEval(err.to_string()))?;
    Ok(CompiledRuntimeNewtonContext {
        compiled_eval_ctx_rhs,
        compiled_eval_ctx_jac,
        compiled_residual,
        compiled_jacobian,
    })
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn build_compiled_initial_newton_context(
    dae: &Dae,
    n_total: usize,
) -> Result<CompiledInitialNewtonContext, SimError> {
    let compiled_dae = build_compiled_newton_dae(dae, n_total);
    let compiled_eval_ctx = build_compiled_eval_context(&compiled_dae, n_total);
    let compiled_eval_ctx_rhs = compiled_eval_ctx.clone();
    let compiled_eval_ctx_jac = compiled_eval_ctx.clone();
    let compiled_residual =
        rumoca_sim_core::phase_solve_lower::compile_initial_residual_with_solver_len(
            &compiled_dae,
            rumoca_sim_core::phase_solve_lower::Backend::Cranelift,
            n_total,
        )
        .map_err(|err| {
            annotate_residual_compile_error(&compiled_dae, n_total, err.to_string(), true)
        })?;
    let compiled_jacobian =
        rumoca_sim_core::phase_solve_lower::compile_initial_jacobian_v_with_solver_len(
            &compiled_dae,
            rumoca_sim_core::phase_solve_lower::Backend::Cranelift,
            n_total,
        )
        .map_err(|err| SimError::CompiledEval(err.to_string()))?;
    Ok(CompiledInitialNewtonContext {
        compiled_eval_ctx_rhs,
        compiled_eval_ctx_jac,
        compiled_residual,
        compiled_jacobian,
    })
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn build_compiled_synthetic_root_context(
    dae: &Dae,
    n_total: usize,
) -> Result<CompiledSyntheticRootContext, SimError> {
    let compiled_dae = build_compiled_problem_dae(dae, n_total);
    let compiled_eval_ctx_root = build_compiled_eval_context(&compiled_dae, n_total);
    let compiled_root_conditions =
        rumoca_sim_core::phase_solve_lower::compile_root_conditions_with_solver_len(
            &compiled_dae,
            rumoca_sim_core::phase_solve_lower::Backend::Cranelift,
            n_total,
        )
        .map_err(|err| {
            annotate_expression_context_compile_error(
                &compiled_dae,
                &compiled_dae.synthetic_root_conditions,
                false,
                n_total,
                err.to_string(),
            )
        })?;
    Ok(CompiledSyntheticRootContext {
        compiled_eval_ctx_root,
        compiled_root_conditions,
    })
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn build_compiled_runtime_expression_context(
    dae: &Dae,
    n_total: usize,
    expressions: &[dae::Expression],
    use_initial: bool,
    rewrite_hidden_direct_assignments: bool,
) -> Result<CompiledRuntimeExpressionContext, SimError> {
    let (compiled_dae, rewritten_expressions) = if rewrite_hidden_direct_assignments {
        rewrite_expression_context_for_direct_assignments(dae, expressions, false, n_total)
    } else {
        (dae.clone(), expressions.to_vec())
    };
    let compiled_eval_ctx = build_compiled_eval_context(&compiled_dae, n_total);
    let compiled_rows = if use_initial {
        // MLS §8.6: initial() is true while evaluating initialization-mode
        // direct-seed/startup expressions.
        rumoca_sim_core::phase_solve_lower::compile_initial_expressions_with_solver_len(
            &compiled_dae,
            &rewritten_expressions,
            rumoca_sim_core::phase_solve_lower::Backend::Cranelift,
            n_total,
        )
    } else {
        rumoca_sim_core::phase_solve_lower::compile_expressions_with_solver_len(
            &compiled_dae,
            &rewritten_expressions,
            rumoca_sim_core::phase_solve_lower::Backend::Cranelift,
            n_total,
        )
    }
    .map_err(|err| SimError::CompiledEval(err.to_string()))?;
    Ok(CompiledRuntimeExpressionContext {
        compiled_eval_ctx,
        compiled_rows,
        row_count: rewritten_expressions.len(),
    })
}

#[cfg(not(target_arch = "wasm32"))]
pub(super) fn build_compiled_runtime_expression_context_for_start_rows(
    dae: &Dae,
    n_total: usize,
    expressions: &[dae::Expression],
    use_initial: bool,
) -> Result<CompiledRuntimeExpressionContext, SimError> {
    let (compiled_dae, rewritten_expressions) =
        rewrite_expression_context_for_direct_assignments(dae, expressions, true, n_total);
    let compiled_eval_ctx = build_compiled_eval_context(&compiled_dae, n_total);
    let compiled_rows = if use_initial {
        rumoca_sim_core::phase_solve_lower::compile_initial_expressions_with_solver_len(
            &compiled_dae,
            &rewritten_expressions,
            rumoca_sim_core::phase_solve_lower::Backend::Cranelift,
            n_total,
        )
    } else {
        rumoca_sim_core::phase_solve_lower::compile_expressions_with_solver_len(
            &compiled_dae,
            &rewritten_expressions,
            rumoca_sim_core::phase_solve_lower::Backend::Cranelift,
            n_total,
        )
    }
    .map_err(|err| {
        annotate_expression_context_compile_error(
            &compiled_dae,
            &rewritten_expressions,
            use_initial,
            n_total,
            err.to_string(),
        )
    })?;
    Ok(CompiledRuntimeExpressionContext {
        compiled_eval_ctx,
        compiled_rows,
        row_count: rewritten_expressions.len(),
    })
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn build_compiled_runtime_expression_context(
    dae: &Dae,
    n_total: usize,
    expressions: &[dae::Expression],
    use_initial: bool,
    rewrite_hidden_direct_assignments: bool,
) -> Result<CompiledRuntimeExpressionContext, SimError> {
    let (compiled_dae, rewritten_expressions) = if rewrite_hidden_direct_assignments {
        rewrite_expression_context_for_direct_assignments(dae, expressions, false, n_total)
    } else {
        (dae.clone(), expressions.to_vec())
    };
    let compiled_eval_ctx = build_compiled_eval_context(&compiled_dae, n_total);
    let compiled_rows = if use_initial {
        rumoca_sim_core::phase_solve_lower::compile_initial_expressions_wasm(
            &compiled_dae,
            &rewritten_expressions,
        )
    } else {
        rumoca_sim_core::phase_solve_lower::compile_expressions_wasm(
            &compiled_dae,
            &rewritten_expressions,
        )
    }
    .map_err(|err| SimError::CompiledEval(err.to_string()))?;
    Ok(CompiledRuntimeExpressionContext {
        compiled_eval_ctx,
        compiled_rows,
        row_count: rewritten_expressions.len(),
    })
}

#[cfg(target_arch = "wasm32")]
pub(super) fn build_compiled_runtime_expression_context_for_start_rows(
    dae: &Dae,
    n_total: usize,
    expressions: &[dae::Expression],
    use_initial: bool,
) -> Result<CompiledRuntimeExpressionContext, SimError> {
    let (compiled_dae, rewritten_expressions) =
        rewrite_expression_context_for_direct_assignments(dae, expressions, true, n_total);
    let compiled_eval_ctx = build_compiled_eval_context(&compiled_dae, n_total);
    let compiled_rows = if use_initial {
        rumoca_sim_core::phase_solve_lower::compile_initial_expressions_wasm(
            &compiled_dae,
            &rewritten_expressions,
        )
    } else {
        rumoca_sim_core::phase_solve_lower::compile_expressions_wasm(
            &compiled_dae,
            &rewritten_expressions,
        )
    }
    .map_err(|err| SimError::CompiledEval(err.to_string()))?;
    Ok(CompiledRuntimeExpressionContext {
        compiled_eval_ctx,
        compiled_rows,
        row_count: rewritten_expressions.len(),
    })
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn build_compiled_runtime_newton_context(
    dae: &Dae,
    n_total: usize,
) -> Result<CompiledRuntimeNewtonContext, SimError> {
    let compiled_dae = build_compiled_newton_dae(dae, n_total);
    let compiled_eval_ctx = build_compiled_eval_context(&compiled_dae, n_total);
    let compiled_eval_ctx_rhs = compiled_eval_ctx.clone();
    let compiled_eval_ctx_jac = compiled_eval_ctx.clone();
    let compiled_residual =
        rumoca_sim_core::phase_solve_lower::compile_residual_wasm(&compiled_dae)
            .map_err(|err| SimError::CompiledEval(err.to_string()))?;
    let compiled_jacobian =
        rumoca_sim_core::phase_solve_lower::compile_jacobian_v_wasm(&compiled_dae)
            .map_err(|err| SimError::CompiledEval(err.to_string()))?;
    Ok(CompiledRuntimeNewtonContext {
        compiled_eval_ctx_rhs,
        compiled_eval_ctx_jac,
        compiled_residual,
        compiled_jacobian,
    })
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn build_compiled_initial_newton_context(
    dae: &Dae,
    n_total: usize,
) -> Result<CompiledInitialNewtonContext, SimError> {
    let compiled_dae = build_compiled_newton_dae(dae, n_total);
    let compiled_eval_ctx = build_compiled_eval_context(&compiled_dae, n_total);
    let compiled_eval_ctx_rhs = compiled_eval_ctx.clone();
    let compiled_eval_ctx_jac = compiled_eval_ctx.clone();
    let compiled_residual =
        rumoca_sim_core::phase_solve_lower::compile_initial_residual_wasm(&compiled_dae)
            .map_err(|err| SimError::CompiledEval(err.to_string()))?;
    let compiled_jacobian =
        rumoca_sim_core::phase_solve_lower::compile_initial_jacobian_v_wasm(&compiled_dae)
            .map_err(|err| SimError::CompiledEval(err.to_string()))?;
    Ok(CompiledInitialNewtonContext {
        compiled_eval_ctx_rhs,
        compiled_eval_ctx_jac,
        compiled_residual,
        compiled_jacobian,
    })
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn build_compiled_synthetic_root_context(
    dae: &Dae,
    n_total: usize,
) -> Result<CompiledSyntheticRootContext, SimError> {
    let compiled_dae = build_compiled_problem_dae(dae, n_total);
    let compiled_eval_ctx_root = build_compiled_eval_context(&compiled_dae, n_total);
    let compiled_root_conditions =
        rumoca_sim_core::phase_solve_lower::compile_root_conditions_wasm(&compiled_dae)
            .map_err(|err| SimError::CompiledEval(err.to_string()))?;
    Ok(CompiledSyntheticRootContext {
        compiled_eval_ctx_root,
        compiled_root_conditions,
    })
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn call_compiled_residual(
    compiled_residual: &rumoca_sim_core::phase_solve_lower::CompiledResidual,
    ctx: &CompiledEvalContext,
    y: &[f64],
    p: &[f64],
    t: f64,
    out: &mut [f64],
) {
    with_compiled_eval_param_slice(ctx, p, t, |compiled_p| {
        if let Err(err) = compiled_residual.call(y, compiled_p, t, out) {
            panic!("compiled residual call failed: {err}");
        }
    });
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn call_compiled_residual(
    compiled_residual: &rumoca_sim_core::phase_solve_lower::CompiledResidualWasm,
    ctx: &CompiledEvalContext,
    y: &[f64],
    p: &[f64],
    t: f64,
    out: &mut [f64],
) {
    with_compiled_eval_param_slice(ctx, p, t, |compiled_p| {
        if let Err(err) = compiled_residual.call(y, compiled_p, t, out) {
            panic!("compiled residual call failed: {err}");
        }
    });
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn call_compiled_jacobian(
    compiled_jacobian: &rumoca_sim_core::phase_solve_lower::CompiledJacobianV,
    ctx: &CompiledEvalContext,
    y: &[f64],
    p: &[f64],
    t: f64,
    v: &[f64],
    out: &mut [f64],
) {
    with_compiled_eval_param_slice(ctx, p, t, |compiled_p| {
        if let Err(err) = compiled_jacobian.call(y, compiled_p, t, v, out) {
            panic!("compiled Jacobian-vector call failed: {err}");
        }
    });
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn call_compiled_jacobian(
    compiled_jacobian: &rumoca_sim_core::phase_solve_lower::CompiledJacobianVWasm,
    ctx: &CompiledEvalContext,
    y: &[f64],
    p: &[f64],
    t: f64,
    v: &[f64],
    out: &mut [f64],
) {
    with_compiled_eval_param_slice(ctx, p, t, |compiled_p| {
        if let Err(err) = compiled_jacobian.call(y, compiled_p, t, v, out) {
            panic!("compiled Jacobian-vector call failed: {err}");
        }
    });
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn call_compiled_expression_rows(
    compiled_rows: &rumoca_sim_core::phase_solve_lower::CompiledExpressionRows,
    ctx: &CompiledEvalContext,
    y: &[f64],
    p: &[f64],
    t: f64,
    out: &mut [f64],
) {
    with_compiled_eval_param_slice(ctx, p, t, |compiled_p| {
        if let Err(err) = compiled_rows.call(y, compiled_p, t, out) {
            panic!("compiled expression rows call failed: {err}");
        }
    });
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn call_compiled_expression_rows(
    compiled_rows: &rumoca_sim_core::phase_solve_lower::CompiledExpressionRowsWasm,
    ctx: &CompiledEvalContext,
    y: &[f64],
    p: &[f64],
    t: f64,
    out: &mut [f64],
) {
    with_compiled_eval_param_slice(ctx, p, t, |compiled_p| {
        if let Err(err) = compiled_rows.call(y, compiled_p, t, out) {
            panic!("compiled expression rows call failed: {err}");
        }
    });
}

pub(crate) fn eval_compiled_runtime_residual(
    ctx: &CompiledRuntimeNewtonContext,
    y: &[f64],
    p: &[f64],
    t: f64,
    out: &mut [f64],
) {
    call_compiled_residual(
        &ctx.compiled_residual,
        &ctx.compiled_eval_ctx_rhs,
        y,
        p,
        t,
        out,
    );
}

pub(crate) fn eval_compiled_runtime_jacobian(
    ctx: &CompiledRuntimeNewtonContext,
    y: &[f64],
    p: &[f64],
    t: f64,
    v: &[f64],
    out: &mut [f64],
) {
    call_compiled_jacobian(
        &ctx.compiled_jacobian,
        &ctx.compiled_eval_ctx_jac,
        y,
        p,
        t,
        v,
        out,
    );
}

pub(crate) fn eval_compiled_initial_residual(
    ctx: &CompiledInitialNewtonContext,
    y: &[f64],
    p: &[f64],
    t: f64,
    out: &mut [f64],
) {
    call_compiled_expression_rows(
        &ctx.compiled_residual,
        &ctx.compiled_eval_ctx_rhs,
        y,
        p,
        t,
        out,
    );
}

pub(crate) fn eval_compiled_initial_jacobian(
    ctx: &CompiledInitialNewtonContext,
    y: &[f64],
    p: &[f64],
    t: f64,
    v: &[f64],
    out: &mut [f64],
) {
    call_compiled_jacobian(
        &ctx.compiled_jacobian,
        &ctx.compiled_eval_ctx_jac,
        y,
        p,
        t,
        v,
        out,
    );
}

pub(crate) fn eval_compiled_runtime_expressions_from_env<'a>(
    ctx: &CompiledRuntimeExpressionContext,
    y: &[f64],
    env: &VarEnv<f64>,
    p: &[f64],
    t: f64,
    y_scratch: &mut Vec<f64>,
    out_scratch: &'a mut Vec<f64>,
) -> &'a [f64] {
    y_scratch.clear();
    y_scratch.extend_from_slice(y);
    ctx.compiled_eval_ctx
        .sim_context
        .sync_solver_values_from_env(y_scratch.as_mut_slice(), env);
    let compiled_p = ctx
        .compiled_eval_ctx
        .sim_context
        .compiled_parameter_vector_from_env(p, env);
    out_scratch.resize(ctx.row_count, 0.0);
    if let Err(err) = ctx
        .compiled_rows
        .call(y_scratch, &compiled_p, t, out_scratch)
    {
        panic!("compiled expression rows call failed: {err}");
    }
    &out_scratch[..ctx.row_count]
}

pub(crate) fn compiled_synthetic_roots_still_armed(
    ctx: &CompiledSyntheticRootContext,
    y: &[f64],
    p: &[f64],
    t: f64,
    clearance: f64,
) -> bool {
    let n_roots = ctx.compiled_root_conditions.rows();
    if n_roots == 0 {
        return false;
    }

    let mut roots = vec![0.0_f64; n_roots];
    call_compiled_expression_rows(
        &ctx.compiled_root_conditions,
        &ctx.compiled_eval_ctx_root,
        y,
        p,
        t,
        roots.as_mut_slice(),
    );
    roots
        .into_iter()
        .any(|root| root.is_finite() && root.abs() <= clearance)
}

fn with_compiled_eval_param_slice<R>(
    ctx: &CompiledEvalContext,
    p: &[f64],
    t: f64,
    f: impl FnOnce(&[f64]) -> R,
) -> R {
    if !ctx.sim_context.has_runtime_parameter_tail() && ctx.input_overrides.is_none() {
        return f(p);
    }
    // The compiled parameter vector only carries parameters plus the runtime
    // tail (inputs/discretes), so the lean tail env is sufficient here.
    let mut env =
        rumoca_sim_core::phase_solve_lower::build_runtime_parameter_tail_env(&ctx.dae, p, t);
    if let Some(overrides) = &ctx.input_overrides {
        for (name, value) in overrides.borrow().iter() {
            env.set(name, *value);
        }
    }
    let mut compiled_p = ctx.param_scratch.borrow_mut();
    ctx.sim_context
        .fill_compiled_parameter_vector_from_env(&mut compiled_p, p, &env);
    f(compiled_p.as_slice())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: Vec::new(),
        }
    }

    fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: rumoca_sim_core::ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn expr_has_ref(expr: &dae::Expression, target: &str) -> bool {
        expr_contains_exact_var_ref(expr, target)
    }

    #[test]
    fn compiled_problem_dae_substitutes_input_alias_without_erasing_defining_row() {
        let mut dae = dae::Dae::default();
        dae.inputs.insert(
            dae::VarName::new("u"),
            dae::Variable::new(dae::VarName::new("u")),
        );
        dae.algebraics.insert(
            dae::VarName::new("plant.u"),
            dae::Variable::new(dae::VarName::new("plant.u")),
        );
        dae.algebraics.insert(
            dae::VarName::new("residual"),
            dae::Variable::new(dae::VarName::new("residual")),
        );
        dae.f_x.push(dae::Equation::residual(
            sub(var("plant.u"), var("u")),
            rumoca_sim_core::core::Span::default(),
            "alias",
        ));
        dae.f_x.push(dae::Equation::residual(
            sub(var("residual"), var("plant.u")),
            rumoca_sim_core::core::Span::default(),
            "uses alias",
        ));

        let compiled = build_compiled_problem_dae(&dae, 2);

        assert!(expr_has_ref(&compiled.f_x[0].rhs, "plant.u"));
        assert!(expr_has_ref(&compiled.f_x[0].rhs, "u"));
        assert!(!expr_has_ref(&compiled.f_x[1].rhs, "plant.u"));
        assert!(expr_has_ref(&compiled.f_x[1].rhs, "u"));
    }

    #[test]
    fn compiled_eval_param_slice_borrows_plain_params_without_runtime_tail() {
        let dae = dae::Dae::default();
        let ctx = build_compiled_eval_context(&dae, 0);
        let params = vec![1.0, 2.0, 3.0];

        let slice_ptr = with_compiled_eval_param_slice(&ctx, &params, 0.0, |compiled_p| {
            assert_eq!(compiled_p, params.as_slice());
            compiled_p.as_ptr()
        });

        assert_eq!(slice_ptr, params.as_ptr());
    }

    #[test]
    fn compiled_eval_param_slice_reuses_runtime_tail_scratch_with_overrides() {
        let mut dae = dae::Dae::default();
        dae.inputs.insert(
            dae::VarName::new("u"),
            dae::Variable::new(dae::VarName::new("u")),
        );
        let mut ctx = build_compiled_eval_context(&dae, 0);
        let overrides: SharedInputOverrides =
            std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new()));
        overrides.borrow_mut().insert("u".to_string(), 3.5);
        ctx.input_overrides = Some(overrides);

        let first_ptr = with_compiled_eval_param_slice(&ctx, &[], 0.0, |compiled_p| {
            assert_eq!(compiled_p, &[3.5]);
            compiled_p.as_ptr()
        });
        let second_ptr = with_compiled_eval_param_slice(&ctx, &[], 1.0, |compiled_p| {
            assert_eq!(compiled_p, &[3.5]);
            compiled_p.as_ptr()
        });

        assert_eq!(first_ptr, second_ptr);
    }
}
