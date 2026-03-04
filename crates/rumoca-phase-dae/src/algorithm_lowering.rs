use super::*;
use crate::when_guard::when_guard_activation_expr;
mod substitution;
use substitution::*;

enum DiscreteEquationBucket {
    DiscreteReal,
    DiscreteValued,
}

fn discrete_equation_bucket_for_lhs(dae: &Dae, lhs: &VarName) -> Option<DiscreteEquationBucket> {
    // MLS Appendix B notes reinit as a special case outside B.1b/B.1c.
    // Solver-facing DAE stores state resets in the event partition (`f_z`) so
    // runtime event updates can apply them without high-level when clauses.
    if dae.states.contains_key(lhs)
        || subscript_fallback_chain(lhs)
            .into_iter()
            .any(|candidate| dae.states.contains_key(&candidate))
    {
        return Some(DiscreteEquationBucket::DiscreteReal);
    }
    if dae.discrete_valued.contains_key(lhs)
        || subscript_fallback_chain(lhs)
            .into_iter()
            .any(|candidate| dae.discrete_valued.contains_key(&candidate))
    {
        return Some(DiscreteEquationBucket::DiscreteValued);
    }
    if dae.discrete_reals.contains_key(lhs)
        || subscript_fallback_chain(lhs)
            .into_iter()
            .any(|candidate| dae.discrete_reals.contains_key(&candidate))
    {
        return Some(DiscreteEquationBucket::DiscreteReal);
    }
    None
}

fn guarded_when_rhs(
    dae: &Dae,
    when_condition: &Expression,
    lhs: &VarName,
    rhs: &Expression,
    use_pre_else: bool,
) -> Expression {
    let guard = when_guard_activation_expr(dae, when_condition);
    let else_expr = if use_pre_else {
        Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args: vec![Expression::VarRef {
                name: lhs.clone(),
                subscripts: vec![],
            }],
        }
    } else {
        Expression::VarRef {
            name: lhs.clone(),
            subscripts: vec![],
        }
    };
    Expression::If {
        branches: vec![(guard, rhs.clone())],
        else_branch: Box::new(else_expr),
    }
}

pub(super) fn route_discrete_event_equations(
    dae: &mut Dae,
    when_clause: &rumoca_ir_dae::WhenClause,
) {
    for eq in &when_clause.equations {
        let Some(lhs) = &eq.lhs else {
            continue;
        };
        let use_pre_else = !eq.origin.starts_with("reinit");
        let guarded = rumoca_ir_dae::Equation::explicit_with_scalar_count(
            lhs.clone(),
            guarded_when_rhs(dae, &when_clause.condition, lhs, &eq.rhs, use_pre_else),
            eq.span,
            format!("guarded {}", eq.origin),
            eq.scalar_count,
        );
        match discrete_equation_bucket_for_lhs(dae, lhs) {
            Some(DiscreteEquationBucket::DiscreteValued) => dae.f_m.push(guarded),
            Some(DiscreteEquationBucket::DiscreteReal) => dae.f_z.push(guarded),
            None => {}
        }
    }
}

fn is_connection_equation_origin(origin: &str) -> bool {
    origin.contains("connection equation:")
}

fn discrete_assignment_rhs_var_name(expr: &Expression) -> Option<VarName> {
    let Expression::VarRef { name, subscripts } = expr else {
        return None;
    };
    Some(varref_with_subscripts(name, subscripts))
}

fn rewrite_discrete_self_refs_to_pre(expr: &Expression, target: &VarName) -> Expression {
    match expr {
        Expression::VarRef { name, subscripts } => {
            rewrite_discrete_var_ref(name, subscripts, target)
        }
        Expression::Binary { op, lhs, rhs } => rewrite_discrete_binary_expr(op, lhs, rhs, target),
        Expression::Unary { op, rhs } => rewrite_discrete_unary_expr(op, rhs, target),
        Expression::BuiltinCall { function, args } => {
            rewrite_discrete_builtin_call(*function, args, target)
        }
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => rewrite_discrete_function_call(name, args, *is_constructor, target),
        Expression::If {
            branches,
            else_branch,
        } => rewrite_discrete_if_expr(branches, else_branch, target),
        Expression::Array {
            elements,
            is_matrix,
        } => rewrite_discrete_array_expr(elements, *is_matrix, target),
        Expression::Tuple { elements } => rewrite_discrete_tuple_expr(elements, target),
        Expression::Range { start, step, end } => {
            rewrite_discrete_range_expr(start, step, end, target)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => rewrite_discrete_array_comprehension(expr, indices, filter, target),
        Expression::Index { base, subscripts } => {
            rewrite_discrete_index_expr(base, subscripts, target)
        }
        Expression::FieldAccess { base, field } => {
            rewrite_discrete_field_access_expr(base, field, target)
        }
        Expression::Literal(literal) => Expression::Literal(literal.clone()),
        Expression::Empty => Expression::Empty,
    }
}

fn rewrite_discrete_var_ref(
    name: &VarName,
    subscripts: &[Subscript],
    target: &VarName,
) -> Expression {
    if name == target {
        pre_target_expr(target)
    } else {
        Expression::VarRef {
            name: name.clone(),
            subscripts: subscripts.to_vec(),
        }
    }
}

fn rewrite_discrete_binary_expr(
    op: &ast::OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    target: &VarName,
) -> Expression {
    Expression::Binary {
        op: op.clone(),
        lhs: Box::new(rewrite_discrete_self_refs_to_pre(lhs, target)),
        rhs: Box::new(rewrite_discrete_self_refs_to_pre(rhs, target)),
    }
}

fn rewrite_discrete_unary_expr(
    op: &ast::OpUnary,
    rhs: &Expression,
    target: &VarName,
) -> Expression {
    Expression::Unary {
        op: op.clone(),
        rhs: Box::new(rewrite_discrete_self_refs_to_pre(rhs, target)),
    }
}

fn rewrite_discrete_builtin_call(
    function: BuiltinFunction,
    args: &[Expression],
    target: &VarName,
) -> Expression {
    if matches!(function, BuiltinFunction::Pre) {
        return Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args: args.to_vec(),
        };
    }
    Expression::BuiltinCall {
        function,
        args: args
            .iter()
            .map(|arg| rewrite_discrete_self_refs_to_pre(arg, target))
            .collect(),
    }
}

fn rewrite_discrete_function_call(
    name: &VarName,
    args: &[Expression],
    is_constructor: bool,
    target: &VarName,
) -> Expression {
    Expression::FunctionCall {
        name: name.clone(),
        args: args
            .iter()
            .map(|arg| rewrite_discrete_self_refs_to_pre(arg, target))
            .collect(),
        is_constructor,
    }
}

fn rewrite_discrete_if_expr(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    target: &VarName,
) -> Expression {
    Expression::If {
        branches: branches
            .iter()
            .map(|(cond, value)| {
                (
                    rewrite_discrete_self_refs_to_pre(cond, target),
                    rewrite_discrete_self_refs_to_pre(value, target),
                )
            })
            .collect(),
        else_branch: Box::new(rewrite_discrete_self_refs_to_pre(else_branch, target)),
    }
}

fn rewrite_discrete_array_expr(
    elements: &[Expression],
    is_matrix: bool,
    target: &VarName,
) -> Expression {
    Expression::Array {
        elements: elements
            .iter()
            .map(|element| rewrite_discrete_self_refs_to_pre(element, target))
            .collect(),
        is_matrix,
    }
}

fn rewrite_discrete_tuple_expr(elements: &[Expression], target: &VarName) -> Expression {
    Expression::Tuple {
        elements: elements
            .iter()
            .map(|element| rewrite_discrete_self_refs_to_pre(element, target))
            .collect(),
    }
}

fn rewrite_discrete_range_expr(
    start: &Expression,
    step: &Option<Box<Expression>>,
    end: &Expression,
    target: &VarName,
) -> Expression {
    Expression::Range {
        start: Box::new(rewrite_discrete_self_refs_to_pre(start, target)),
        step: step
            .as_ref()
            .map(|step_expr| Box::new(rewrite_discrete_self_refs_to_pre(step_expr, target))),
        end: Box::new(rewrite_discrete_self_refs_to_pre(end, target)),
    }
}

fn rewrite_discrete_array_comprehension(
    expr: &Expression,
    indices: &[ComprehensionIndex],
    filter: &Option<Box<Expression>>,
    target: &VarName,
) -> Expression {
    Expression::ArrayComprehension {
        expr: Box::new(rewrite_discrete_self_refs_to_pre(expr, target)),
        indices: indices
            .iter()
            .map(|index| ComprehensionIndex {
                name: index.name.clone(),
                range: rewrite_discrete_self_refs_to_pre(&index.range, target),
            })
            .collect(),
        filter: filter
            .as_ref()
            .map(|filter_expr| Box::new(rewrite_discrete_self_refs_to_pre(filter_expr, target))),
    }
}

fn rewrite_discrete_index_expr(
    base: &Expression,
    subscripts: &[Subscript],
    target: &VarName,
) -> Expression {
    Expression::Index {
        base: Box::new(rewrite_discrete_self_refs_to_pre(base, target)),
        subscripts: subscripts.to_vec(),
    }
}

fn rewrite_discrete_field_access_expr(
    base: &Expression,
    field: &str,
    target: &VarName,
) -> Expression {
    Expression::FieldAccess {
        base: Box::new(rewrite_discrete_self_refs_to_pre(base, target)),
        field: field.to_string(),
    }
}

fn anchored_collision_targets(
    grouped: &IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
) -> std::collections::HashSet<VarName> {
    grouped
        .iter()
        .filter(|(_, equations)| {
            equations
                .iter()
                .any(|equation| !is_connection_equation_origin(&equation.origin))
        })
        .map(|(lhs, _)| lhs.clone())
        .collect()
}

fn flip_edge_key(origin: &str, lhs: &VarName, rhs: &VarName) -> (String, String, String) {
    (
        origin.to_string(),
        lhs.as_str().to_string(),
        rhs.as_str().to_string(),
    )
}

fn choose_collision_keep_index(
    equations: &[rumoca_ir_dae::Equation],
    lhs: &VarName,
    anchored_targets: &std::collections::HashSet<VarName>,
    flipped_once: &std::collections::HashSet<(String, String, String)>,
) -> Option<usize> {
    for (idx, equation) in equations.iter().enumerate() {
        let Some(rhs_target) = discrete_assignment_rhs_var_name(&equation.rhs) else {
            continue;
        };
        let reverse_key = flip_edge_key(&equation.origin, &rhs_target, lhs);
        if flipped_once.contains(&reverse_key) {
            return Some(idx);
        }
    }

    for (idx, equation) in equations.iter().enumerate() {
        let Some(rhs_target) = discrete_assignment_rhs_var_name(&equation.rhs) else {
            continue;
        };
        if anchored_targets.contains(&rhs_target) {
            return Some(idx);
        }
    }
    None
}

fn has_reverse_connection_alias(
    grouped: &IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    lhs: &VarName,
    rhs_target: &VarName,
) -> bool {
    grouped.get(rhs_target).is_some_and(|rhs_equations| {
        rhs_equations.iter().any(|rhs_equation| {
            if !is_connection_equation_origin(&rhs_equation.origin) {
                return false;
            }
            discrete_assignment_rhs_var_name(&rhs_equation.rhs)
                .as_ref()
                .is_some_and(|candidate| candidate == lhs)
        })
    })
}

fn keep_collision_equation_without_flip(dae: &Dae, lhs: &VarName, rhs_target: &VarName) -> bool {
    rhs_target == lhs || discrete_equation_bucket_for_lhs(dae, rhs_target).is_none()
}

fn resolve_collision_equation(
    dae: &Dae,
    grouped: &IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    lhs: &VarName,
    equation: &rumoca_ir_dae::Equation,
    rhs_target: &VarName,
    flipped_once: &std::collections::HashSet<(String, String, String)>,
) -> Option<VarName> {
    if keep_collision_equation_without_flip(dae, lhs, rhs_target) {
        return None;
    }
    if has_reverse_connection_alias(grouped, lhs, rhs_target) {
        return Some(lhs.clone());
    }
    let reverse_flip_key = flip_edge_key(&equation.origin, rhs_target, lhs);
    if flipped_once.contains(&reverse_flip_key) {
        return Some(lhs.clone());
    }
    Some(rhs_target.clone())
}

fn process_collision_equation(
    dae: &Dae,
    grouped: &mut IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    lhs: &VarName,
    equation: rumoca_ir_dae::Equation,
    flipped_once: &mut std::collections::HashSet<(String, String, String)>,
    debug_canonicalize: bool,
) -> bool {
    let Some(rhs_target) = discrete_assignment_rhs_var_name(&equation.rhs) else {
        if debug_canonicalize {
            eprintln!(
                "f_m canonicalize no-rhs-var lhs={} origin={}",
                lhs, equation.origin
            );
        }
        grouped.entry(lhs.clone()).or_default().push(equation);
        return false;
    };

    let Some(resolved_target) =
        resolve_collision_equation(dae, grouped, lhs, &equation, &rhs_target, flipped_once)
    else {
        if debug_canonicalize {
            eprintln!(
                "f_m canonicalize keep-collision lhs={} rhs={} origin={}",
                lhs, rhs_target, equation.origin
            );
        }
        grouped.entry(lhs.clone()).or_default().push(equation);
        return false;
    };

    if resolved_target == *lhs {
        if debug_canonicalize {
            eprintln!(
                "f_m canonicalize drop-redundant-edge lhs={} rhs={} origin={}",
                lhs, rhs_target, equation.origin
            );
        }
        return true;
    }

    flipped_once.insert(flip_edge_key(&equation.origin, lhs, &rhs_target));
    let mut flipped = equation;
    flipped.lhs = Some(resolved_target.clone());
    flipped.rhs = Expression::VarRef {
        name: lhs.clone(),
        subscripts: vec![],
    };
    if debug_canonicalize {
        eprintln!(
            "f_m canonicalize flip lhs={} -> lhs={} origin={}",
            lhs, resolved_target, flipped.origin
        );
    }
    grouped.entry(resolved_target).or_default().push(flipped);
    true
}

fn resolve_connection_alias_target_collisions(
    grouped: &mut IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    dae: &Dae,
) {
    let debug_canonicalize = std::env::var("RUMOCA_DEBUG_FM_CANON").is_ok();
    let anchored_targets = anchored_collision_targets(grouped);

    let mut iteration = 0usize;
    let max_iterations = 16usize;
    let mut flipped_once: std::collections::HashSet<(String, String, String)> =
        std::collections::HashSet::new();
    loop {
        iteration += 1;
        if iteration > max_iterations {
            if debug_canonicalize {
                eprintln!("f_m canonicalize collision-pass reached max iterations");
            }
            break;
        }

        let mut changed = false;
        let keys: Vec<VarName> = grouped.keys().cloned().collect();

        for lhs in keys {
            let Some(equations) = grouped.get(&lhs) else {
                continue;
            };
            if equations.len() <= 1 {
                continue;
            }
            if equations
                .iter()
                .any(|equation| !is_connection_equation_origin(&equation.origin))
            {
                continue;
            }

            let candidate_keep_idx =
                choose_collision_keep_index(equations, &lhs, &anchored_targets, &flipped_once);

            let Some(mut current_equations) = grouped.swap_remove(&lhs) else {
                continue;
            };
            if current_equations.len() <= 1 {
                grouped.insert(lhs.clone(), current_equations);
                continue;
            }

            let keep_idx = candidate_keep_idx
                .unwrap_or(0)
                .min(current_equations.len() - 1);
            let keep_equation = current_equations.remove(keep_idx);
            grouped.insert(lhs.clone(), vec![keep_equation]);

            for equation in current_equations {
                changed |= process_collision_equation(
                    dae,
                    grouped,
                    &lhs,
                    equation,
                    &mut flipped_once,
                    debug_canonicalize,
                );
            }
        }

        if !changed {
            break;
        }
    }
}

pub(super) fn canonicalize_discrete_assignment_equations(dae: &mut Dae) {
    let debug_canonicalize = std::env::var("RUMOCA_DEBUG_FM_CANON").is_ok();
    let (mut grouped, passthrough) = drain_grouped_discrete_assignments(dae);
    reroute_connection_aliases_for_defined_targets(&mut grouped, dae);
    resolve_connection_alias_target_collisions(&mut grouped, dae);
    debug_log_grouped_duplicates(debug_canonicalize, &grouped);

    let mut rebuilt = Vec::with_capacity(passthrough.len() + grouped.len());
    rebuilt.extend(passthrough);
    for (lhs, equations) in grouped {
        rebuilt.extend(canonicalize_assignment_group(lhs, equations));
    }
    dae.f_m = rebuilt;
    debug_log_final_duplicates(debug_canonicalize, &dae.f_m);
}

fn drain_grouped_discrete_assignments(
    dae: &mut Dae,
) -> (
    IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    Vec<rumoca_ir_dae::Equation>,
) {
    let mut grouped: IndexMap<VarName, Vec<rumoca_ir_dae::Equation>> = IndexMap::new();
    let mut passthrough = Vec::new();
    for equation in dae.f_m.drain(..) {
        if let Some(lhs) = equation.lhs.as_ref() {
            grouped.entry(lhs.clone()).or_default().push(equation);
        } else {
            passthrough.push(equation);
        }
    }
    (grouped, passthrough)
}

fn reroute_connection_aliases_for_defined_targets(
    grouped: &mut IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    dae: &Dae,
) {
    let mut rerouted: IndexMap<VarName, Vec<rumoca_ir_dae::Equation>> = IndexMap::new();
    for (lhs, equations) in grouped.iter_mut() {
        if equations.len() <= 1
            || !equations
                .iter()
                .any(|equation| !is_connection_equation_origin(&equation.origin))
        {
            continue;
        }

        for equation in equations
            .iter()
            .filter(|equation| is_connection_equation_origin(&equation.origin))
        {
            let Some(rhs_target) = discrete_assignment_rhs_var_name(&equation.rhs) else {
                continue;
            };
            if keep_collision_equation_without_flip(dae, lhs, &rhs_target) {
                continue;
            }
            let mut flipped = equation.clone();
            flipped.lhs = Some(rhs_target.clone());
            flipped.rhs = Expression::VarRef {
                name: lhs.clone(),
                subscripts: vec![],
            };
            rerouted.entry(rhs_target).or_default().push(flipped);
        }
    }
    for (lhs, aliases) in rerouted {
        grouped.entry(lhs).or_default().extend(aliases);
    }
}

fn canonicalize_assignment_group(
    lhs: VarName,
    mut equations: Vec<rumoca_ir_dae::Equation>,
) -> Vec<rumoca_ir_dae::Equation> {
    if equations.len() == 1 {
        let mut equation = equations.remove(0);
        equation.rhs = rewrite_discrete_self_refs_to_pre(&equation.rhs, &lhs);
        return vec![equation];
    }

    if equations
        .iter()
        .any(|equation| !is_connection_equation_origin(&equation.origin))
    {
        equations.retain(|equation| !is_connection_equation_origin(&equation.origin));
    }

    let mut seen_origin = HashSet::<String>::default();
    equations.retain(|equation| seen_origin.insert(equation.origin.clone()));

    let mut seen_rhs = HashSet::<String>::default();
    let mut deduped = Vec::new();
    for mut equation in equations {
        equation.rhs = rewrite_discrete_self_refs_to_pre(&equation.rhs, &lhs);
        let rhs_key = format!("{:?}", equation.rhs);
        if seen_rhs.insert(rhs_key) {
            deduped.push(equation);
        }
    }
    deduped
}

fn debug_log_grouped_duplicates(
    debug_canonicalize: bool,
    grouped: &IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
) {
    if !debug_canonicalize {
        return;
    }
    for (lhs, equations) in grouped.iter().filter(|(_, eqs)| eqs.len() > 1) {
        eprintln!(
            "f_m canonicalize grouped-dup lhs={} count={} origins={:?}",
            lhs,
            equations.len(),
            equations
                .iter()
                .map(|eq| eq.origin.clone())
                .collect::<Vec<_>>()
        );
    }
}

fn debug_log_final_duplicates(debug_canonicalize: bool, equations: &[rumoca_ir_dae::Equation]) {
    if !debug_canonicalize {
        return;
    }
    let mut counts: IndexMap<String, usize> = IndexMap::new();
    for equation in equations {
        if let Some(lhs) = equation.lhs.as_ref() {
            *counts.entry(lhs.as_str().to_string()).or_default() += 1;
        }
    }
    for (lhs, count) in counts.into_iter().filter(|(_, count)| *count > 1) {
        eprintln!("f_m canonicalize final-dup lhs={} count={}", lhs, count);
    }
}

fn lookup_algorithm_target_scalar_count(dae: &Dae, target: &VarName) -> usize {
    dae.states
        .get(target)
        .or_else(|| dae.algebraics.get(target))
        .or_else(|| dae.outputs.get(target))
        .or_else(|| dae.inputs.get(target))
        .or_else(|| dae.discrete_reals.get(target))
        .or_else(|| dae.discrete_valued.get(target))
        .or_else(|| dae.derivative_aliases.get(target))
        .map(|var| var.size())
        .unwrap_or(1)
        .max(1)
}

fn varref_with_subscripts(name: &VarName, subscripts: &[Subscript]) -> VarName {
    if subscripts.is_empty() {
        return name.clone();
    }
    fn render_subscript(subscript: &Subscript) -> String {
        match subscript {
            Subscript::Index(index) => index.to_string(),
            Subscript::Colon => ":".to_string(),
            Subscript::Expr(expr) => format!("{expr:?}"),
        }
    }
    let rendered = subscripts
        .iter()
        .map(render_subscript)
        .collect::<Vec<_>>()
        .join(",");
    VarName::new(format!("{}[{rendered}]", name.as_str()))
}

fn algorithm_output_target_name(output: &Expression) -> Option<VarName> {
    match output {
        Expression::VarRef { name, subscripts } => Some(varref_with_subscripts(name, subscripts)),
        Expression::FieldAccess { base, field } => {
            if let Expression::VarRef { name, subscripts } = base.as_ref() {
                let base = varref_with_subscripts(name, subscripts);
                Some(VarName::new(format!("{}.{}", base.as_str(), field)))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn bool_expr(value: bool) -> Expression {
    Expression::Literal(Literal::Boolean(value))
}

fn is_bool_expr(expr: &Expression, expected: bool) -> bool {
    matches!(expr, Expression::Literal(Literal::Boolean(v)) if *v == expected)
}

fn not_expr(expr: Expression) -> Expression {
    if let Expression::Literal(Literal::Boolean(flag)) = expr {
        return bool_expr(!flag);
    }
    Expression::Unary {
        op: ast::OpUnary::Not(Default::default()),
        rhs: Box::new(expr),
    }
}

fn and_expr(lhs: Expression, rhs: Expression) -> Expression {
    if is_bool_expr(&lhs, false) || is_bool_expr(&rhs, false) {
        return bool_expr(false);
    }
    if is_bool_expr(&lhs, true) {
        return rhs;
    }
    if is_bool_expr(&rhs, true) {
        return lhs;
    }
    Expression::Binary {
        op: ast::OpBinary::And(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn or_expr(lhs: Expression, rhs: Expression) -> Expression {
    if is_bool_expr(&lhs, true) || is_bool_expr(&rhs, true) {
        return bool_expr(true);
    }
    if is_bool_expr(&lhs, false) {
        return rhs;
    }
    if is_bool_expr(&rhs, false) {
        return lhs;
    }
    Expression::Binary {
        op: ast::OpBinary::Or(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn guarded_expr(guard: Expression, value: Expression, fallback: Expression) -> Expression {
    if is_bool_expr(&guard, true) {
        return value;
    }
    if is_bool_expr(&guard, false) {
        return fallback;
    }
    Expression::If {
        branches: vec![(guard, value)],
        else_branch: Box::new(fallback),
    }
}

fn eval_integer_expr(expr: &Expression, flat: &Model) -> Option<i64> {
    scalar_size::try_eval_flat_expr_i64(expr, flat, 0)
}

fn eval_for_range_values(range: &Expression, flat: &Model) -> Result<Vec<i64>, String> {
    const MAX_LOOP_EXPANSION: usize = 100_000;
    let (start, step, end) = match range {
        Expression::Range { start, step, end } => {
            let start_value = eval_integer_expr(start, flat)
                .ok_or_else(|| format!("ForRangeStartNotConstant({start:?})"))?;
            let end_value = eval_integer_expr(end, flat)
                .ok_or_else(|| format!("ForRangeEndNotConstant({end:?})"))?;
            let step_value = if let Some(step_expr) = step {
                eval_integer_expr(step_expr, flat)
                    .ok_or_else(|| format!("ForRangeStepNotConstant({step_expr:?})"))?
            } else {
                1
            };
            (start_value, step_value, end_value)
        }
        _ => {
            let end_value = eval_integer_expr(range, flat)
                .ok_or_else(|| format!("ForRangeNotConstant({range:?})"))?;
            (1, 1, end_value)
        }
    };
    if step == 0 {
        return Err("ForRangeZeroStep".to_string());
    }
    let mut values = Vec::new();
    if step > 0 {
        let mut value = start;
        while value <= end {
            values.push(value);
            if values.len() > MAX_LOOP_EXPANSION {
                return Err(format!("ForRangeTooLarge({MAX_LOOP_EXPANSION})"));
            }
            value += step;
        }
    } else {
        let mut value = start;
        while value >= end {
            values.push(value);
            if values.len() > MAX_LOOP_EXPANSION {
                return Err(format!("ForRangeTooLarge({MAX_LOOP_EXPANSION})"));
            }
            value += step;
        }
    }
    Ok(values)
}

fn expand_for_bindings(
    indices: &[flat::ForIndex],
    flat: &Model,
    bindings: &HashMap<String, i64>,
) -> Result<Vec<HashMap<String, i64>>, String> {
    if indices.is_empty() {
        return Ok(vec![bindings.clone()]);
    }

    let mut expanded = Vec::new();
    let first = &indices[0];
    let range = substitute_expr_with_bindings(&first.range, bindings);
    let values = eval_for_range_values(&range, flat)?;
    for value in values {
        let mut next_bindings = bindings.clone();
        next_bindings.insert(first.ident.clone(), value);
        expanded.extend(expand_for_bindings(&indices[1..], flat, &next_bindings)?);
    }
    Ok(expanded)
}

fn resolve_multi_output_projection_names(
    flat: &Model,
    function_name: &VarName,
    requested_outputs: usize,
) -> Result<Vec<VarName>, String> {
    let Some(function) = resolve_flat_function(function_name, flat) else {
        return Err("FunctionCallMultiOutputUnresolved".to_string());
    };
    if function.outputs.len() < requested_outputs {
        return Err("FunctionCallMultiOutputArityMismatch".to_string());
    }
    Ok(function
        .outputs
        .iter()
        .take(requested_outputs)
        .map(|output| {
            VarName::new(format!(
                "{}.{}",
                function_name.as_str(),
                output.name.as_str()
            ))
        })
        .collect())
}

fn pre_target_expr(target: &VarName) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Pre,
        args: vec![Expression::VarRef {
            name: target.clone(),
            subscripts: vec![],
        }],
    }
}

fn current_target_expr(target: &VarName) -> Expression {
    Expression::VarRef {
        name: target.clone(),
        subscripts: vec![],
    }
}

fn algorithm_if_fallback_expr(dae: &Dae, target: &VarName) -> Expression {
    let is_discrete_target = dae.discrete_reals.contains_key(target)
        || dae.discrete_valued.contains_key(target)
        || subscript_fallback_chain(target)
            .into_iter()
            .any(|candidate| {
                dae.discrete_reals.contains_key(&candidate)
                    || dae.discrete_valued.contains_key(&candidate)
            });
    if is_discrete_target {
        pre_target_expr(target)
    } else {
        current_target_expr(target)
    }
}

fn algorithm_assignment_to_target_expr(
    dae: &Dae,
    statement: &Statement,
) -> Option<(VarName, Expression, Span, String)> {
    match statement {
        Statement::Assignment { comp, value } => Some((
            component_reference_to_var_name(comp),
            value.clone(),
            Span::DUMMY,
            "algorithm assignment".to_string(),
        )),
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            let [output] = outputs.as_slice() else {
                return None;
            };
            let target = algorithm_output_target_name(output)?;
            Some((
                target,
                Expression::FunctionCall {
                    name: component_reference_to_var_name(comp),
                    args: args.clone(),
                    is_constructor: false,
                },
                Span::DUMMY,
                "algorithm function call assignment".to_string(),
            ))
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            let mut branches = Vec::new();
            let mut target: Option<VarName> = None;
            for block in cond_blocks {
                let [single] = block.stmts.as_slice() else {
                    return None;
                };
                let (block_target, block_value, _, _) =
                    algorithm_assignment_to_target_expr(dae, single)?;
                if target
                    .as_ref()
                    .is_some_and(|existing| existing != &block_target)
                {
                    return None;
                }
                if target.is_none() {
                    target = Some(block_target);
                }
                branches.push((block.cond.clone(), block_value));
            }
            let else_value = if let Some(else_stmts) = else_block.as_ref() {
                let [else_single] = else_stmts.as_slice() else {
                    return None;
                };
                let (else_target, else_value, _, _) =
                    algorithm_assignment_to_target_expr(dae, else_single)?;
                if target.as_ref() != Some(&else_target) {
                    return None;
                }
                else_value
            } else {
                let target_name = target.as_ref()?;
                if !dae.discrete_reals.contains_key(target_name)
                    && !dae.discrete_valued.contains_key(target_name)
                {
                    return None;
                }
                pre_target_expr(target_name)
            };
            Some((
                target?,
                Expression::If {
                    branches,
                    else_branch: Box::new(else_value),
                },
                Span::DUMMY,
                "algorithm if-assignment".to_string(),
            ))
        }
        Statement::Empty => None,
        Statement::For { .. }
        | Statement::While(_)
        | Statement::When(_)
        | Statement::Reinit { .. }
        | Statement::Assert { .. }
        | Statement::Return
        | Statement::Break => None,
    }
}

type AlgorithmAssignment = (VarName, Expression, Span, String);

fn is_noop_algorithm_statement(statement: &Statement) -> bool {
    match statement {
        Statement::Empty | Statement::Assert { .. } | Statement::Return | Statement::Break => true,
        Statement::FunctionCall { outputs, .. } => outputs.is_empty(),
        Statement::For { equations, .. } => equations.iter().all(is_noop_algorithm_statement),
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            cond_blocks
                .iter()
                .all(|block| block.stmts.iter().all(is_noop_algorithm_statement))
                && else_block
                    .as_ref()
                    .is_none_or(|stmts| stmts.iter().all(is_noop_algorithm_statement))
        }
        Statement::When(blocks) => blocks
            .iter()
            .all(|block| block.stmts.iter().all(is_noop_algorithm_statement)),
        Statement::Assignment { .. } | Statement::While(_) | Statement::Reinit { .. } => false,
    }
}

fn collect_algorithm_block_assignments(
    dae: &Dae,
    flat: &Model,
    statements: &[Statement],
) -> Result<IndexMap<VarName, AlgorithmAssignment>, String> {
    let mut assignments: IndexMap<VarName, AlgorithmAssignment> = IndexMap::new();
    for statement in statements {
        for (target, value, span, origin) in lower_statement_assignments(dae, flat, statement)? {
            assignments.insert(target.clone(), (target, value, span, origin));
        }
    }
    Ok(assignments)
}

fn lower_if_statement_assignments(
    dae: &Dae,
    flat: &Model,
    cond_blocks: &[StatementBlock],
    else_block: &Option<Vec<Statement>>,
) -> Result<Vec<AlgorithmAssignment>, String> {
    let mut branch_maps: Vec<(Expression, IndexMap<VarName, AlgorithmAssignment>)> = Vec::new();
    for block in cond_blocks {
        let assignments = collect_algorithm_block_assignments(dae, flat, &block.stmts)?;
        branch_maps.push((block.cond.clone(), assignments));
    }

    let else_assignments = else_block
        .as_ref()
        .map(|statements| collect_algorithm_block_assignments(dae, flat, statements))
        .transpose()?
        .unwrap_or_default();

    let mut targets: IndexSet<VarName> = IndexSet::new();
    for (_, assignments) in &branch_maps {
        for target in assignments.keys() {
            targets.insert(target.clone());
        }
    }
    for target in else_assignments.keys() {
        targets.insert(target.clone());
    }

    let mut lowered = Vec::new();
    for target in targets {
        let branches = branch_maps
            .iter()
            .map(|(condition, assignments)| {
                let rhs = assignments
                    .get(&target)
                    .map(|(_, value, _, _)| value.clone())
                    .unwrap_or_else(|| algorithm_if_fallback_expr(dae, &target));
                (condition.clone(), rhs)
            })
            .collect();
        let else_rhs = else_assignments
            .get(&target)
            .map(|(_, value, _, _)| value.clone())
            .unwrap_or_else(|| algorithm_if_fallback_expr(dae, &target));
        lowered.push((
            target,
            Expression::If {
                branches,
                else_branch: Box::new(else_rhs),
            },
            Span::DUMMY,
            "algorithm if-assignment".to_string(),
        ));
    }

    Ok(lowered)
}

#[derive(Default)]
struct ForLoopLowerState {
    assignments: IndexMap<VarName, Expression>,
    break_condition: Option<Expression>,
}

fn loop_active_guard(base_guard: Expression, break_condition: &Option<Expression>) -> Expression {
    if let Some(break_expr) = break_condition {
        and_expr(base_guard, not_expr(break_expr.clone()))
    } else {
        base_guard
    }
}

fn apply_guarded_loop_assignment(
    dae: &Dae,
    state: &mut ForLoopLowerState,
    target: VarName,
    value: Expression,
    guard: Expression,
) {
    let fallback = state
        .assignments
        .get(&target)
        .cloned()
        .unwrap_or_else(|| algorithm_if_fallback_expr(dae, &target));
    let merged = guarded_expr(guard, value, fallback);
    state.assignments.insert(target, merged);
}

fn mark_loop_break(state: &mut ForLoopLowerState, guard: Expression) {
    state.break_condition = Some(match state.break_condition.take() {
        Some(existing) => or_expr(existing, guard),
        None => guard,
    });
}

fn lower_for_statement_sequence_with_guard(
    dae: &Dae,
    flat: &Model,
    statements: &[Statement],
    guard: Expression,
    bindings: &HashMap<String, i64>,
    state: &mut ForLoopLowerState,
) -> Result<(), String> {
    for statement in statements {
        lower_for_statement_with_guard(dae, flat, statement, guard.clone(), bindings, state)?;
    }
    Ok(())
}

fn lower_for_if_with_guard(
    dae: &Dae,
    flat: &Model,
    cond_blocks: &[StatementBlock],
    else_block: &Option<Vec<Statement>>,
    guard: Expression,
    bindings: &HashMap<String, i64>,
    state: &mut ForLoopLowerState,
) -> Result<(), String> {
    let mut taken = bool_expr(false);
    for block in cond_blocks {
        let cond = substitute_expr_with_bindings(&block.cond, bindings);
        let branch_guard = and_expr(
            guard.clone(),
            and_expr(not_expr(taken.clone()), cond.clone()),
        );
        lower_for_statement_sequence_with_guard(
            dae,
            flat,
            &block.stmts,
            branch_guard,
            bindings,
            state,
        )?;
        taken = or_expr(taken, cond);
    }

    if let Some(stmts) = else_block {
        let else_guard = and_expr(guard, not_expr(taken));
        lower_for_statement_sequence_with_guard(dae, flat, stmts, else_guard, bindings, state)?;
    }
    Ok(())
}

fn lower_nested_for_with_guard(
    dae: &Dae,
    flat: &Model,
    indices: &[flat::ForIndex],
    equations: &[Statement],
    guard: Expression,
    bindings: &HashMap<String, i64>,
    state: &mut ForLoopLowerState,
) -> Result<(), String> {
    let mut merged_bindings = bindings.clone();
    let iteration_bindings = expand_for_bindings(indices, flat, &merged_bindings)?;
    for iteration in iteration_bindings {
        merged_bindings = iteration;
        let iteration_guard = loop_active_guard(guard.clone(), &state.break_condition);
        lower_for_statement_sequence_with_guard(
            dae,
            flat,
            equations,
            iteration_guard,
            &merged_bindings,
            state,
        )?;
    }
    Ok(())
}

fn lower_for_statement_with_guard(
    dae: &Dae,
    flat: &Model,
    statement: &Statement,
    guard: Expression,
    bindings: &HashMap<String, i64>,
    state: &mut ForLoopLowerState,
) -> Result<(), String> {
    let active_guard = loop_active_guard(guard, &state.break_condition);
    if is_bool_expr(&active_guard, false) {
        return Ok(());
    }

    let substituted = substitute_statement_with_bindings(statement, bindings);
    match &substituted {
        Statement::Break => {
            mark_loop_break(state, active_guard);
            Ok(())
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => lower_for_if_with_guard(
            dae,
            flat,
            cond_blocks,
            else_block,
            active_guard,
            bindings,
            state,
        ),
        Statement::For { indices, equations } => lower_nested_for_with_guard(
            dae,
            flat,
            indices,
            equations,
            active_guard,
            bindings,
            state,
        ),
        Statement::While(_) => Err("ForContainsWhile".to_string()),
        Statement::When(_) => Err("ForContainsWhen".to_string()),
        _ => {
            let lowered = lower_statement_assignments(dae, flat, &substituted)?;
            for (target, value, _, _) in lowered {
                apply_guarded_loop_assignment(dae, state, target, value, active_guard.clone());
            }
            Ok(())
        }
    }
}

fn lower_for_statement_assignments(
    dae: &Dae,
    flat: &Model,
    indices: &[flat::ForIndex],
    equations: &[Statement],
) -> Result<Vec<AlgorithmAssignment>, String> {
    let mut state = ForLoopLowerState::default();
    let bindings = HashMap::new();
    let iteration_bindings = expand_for_bindings(indices, flat, &bindings)?;
    for iteration in iteration_bindings {
        let iteration_guard = loop_active_guard(bool_expr(true), &state.break_condition);
        lower_for_statement_sequence_with_guard(
            dae,
            flat,
            equations,
            iteration_guard,
            &iteration,
            &mut state,
        )?;
    }

    Ok(state
        .assignments
        .into_iter()
        .map(|(target, value)| {
            (
                target,
                value,
                Span::DUMMY,
                "algorithm for-assignment".to_string(),
            )
        })
        .collect())
}

fn lower_statement_assignments(
    dae: &Dae,
    flat: &Model,
    statement: &Statement,
) -> Result<Vec<AlgorithmAssignment>, String> {
    if is_noop_algorithm_statement(statement) {
        return Ok(Vec::new());
    }

    match statement {
        Statement::When(_) => Err("When".to_string()),
        Statement::If {
            cond_blocks,
            else_block,
        } => lower_if_statement_assignments(dae, flat, cond_blocks, else_block),
        Statement::For { indices, equations } => {
            lower_for_statement_assignments(dae, flat, indices, equations)
        }
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } if outputs.len() > 1 => {
            let function_name = component_reference_to_var_name(comp);
            let projection_names =
                resolve_multi_output_projection_names(flat, &function_name, outputs.len())?;
            let mut lowered = Vec::with_capacity(outputs.len());
            for (output_expr, projection_name) in outputs.iter().zip(projection_names.iter()) {
                let Some(target) = algorithm_output_target_name(output_expr) else {
                    return Err("FunctionCallMultiOutputTarget".to_string());
                };
                lowered.push((
                    target,
                    Expression::FunctionCall {
                        name: projection_name.clone(),
                        args: args.clone(),
                        is_constructor: false,
                    },
                    Span::DUMMY,
                    "algorithm function call assignment".to_string(),
                ));
            }
            Ok(lowered)
        }
        _ => algorithm_assignment_to_target_expr(dae, statement)
            .map(|assignment| vec![assignment])
            .ok_or_else(|| {
                format!(
                    "{} {:?}",
                    unsupported_algorithm_statement_tag(statement),
                    statement
                )
            }),
    }
}

fn unsupported_algorithm_statement_tag(statement: &Statement) -> &'static str {
    match statement {
        Statement::Assignment { .. } => "Assignment",
        Statement::If { .. } => "If",
        Statement::For { .. } => "For",
        Statement::While(_) => "While",
        Statement::When(_) => "When",
        Statement::FunctionCall { .. } => "FunctionCall",
        Statement::Reinit { .. } => "Reinit",
        Statement::Assert { .. } => "Assert",
        Statement::Return => "Return",
        Statement::Break => "Break",
        Statement::Empty => "Empty",
    }
}

#[derive(Default)]
struct LoweredAlgorithmPartitions {
    main: Vec<rumoca_ir_dae::Equation>,
    f_z: Vec<rumoca_ir_dae::Equation>,
    f_m: Vec<rumoca_ir_dae::Equation>,
}

fn lower_when_statement_to_event_equations(
    dae: &Dae,
    flat: &Model,
    blocks: &[StatementBlock],
    algorithm_origin: &str,
) -> Result<Vec<rumoca_ir_dae::Equation>, String> {
    let mut targets: IndexMap<VarName, Vec<(Expression, Expression)>> = IndexMap::new();

    for block in blocks {
        let block_assignments = collect_algorithm_block_assignments(dae, flat, &block.stmts)?;
        for (_, (target, value, _, _)) in block_assignments {
            targets
                .entry(target)
                .or_default()
                .push((when_guard_activation_expr(dae, &block.cond), value));
        }
    }

    let mut lowered = Vec::with_capacity(targets.len());
    for (target, branches) in targets {
        let eq = rumoca_ir_dae::Equation::explicit_with_scalar_count(
            target.clone(),
            Expression::If {
                branches,
                else_branch: Box::new(pre_target_expr(&target)),
            },
            Span::DUMMY,
            format!("algorithm when-assignment ({algorithm_origin})"),
            lookup_algorithm_target_scalar_count(dae, &target),
        );
        lowered.push(eq);
    }

    Ok(lowered)
}

fn lower_algorithm_to_equations(
    dae: &Dae,
    flat: &Model,
    algorithm: &rumoca_ir_flat::Algorithm,
) -> Result<LoweredAlgorithmPartitions, String> {
    if algorithm.statements.is_empty() {
        return Ok(LoweredAlgorithmPartitions::default());
    }

    let mut lowered = LoweredAlgorithmPartitions::default();
    let mut main_assignments: IndexMap<VarName, AlgorithmAssignment> = IndexMap::new();

    for statement in &algorithm.statements {
        if let Statement::When(blocks) = statement {
            for eq in lower_when_statement_to_event_equations(dae, flat, blocks, &algorithm.origin)?
            {
                route_lowered_when_equation(dae, &mut lowered, eq);
            }
            continue;
        }

        for (target, value, span, origin) in lower_statement_assignments(dae, flat, statement)? {
            main_assignments.insert(target.clone(), (target, value, span, origin));
        }
    }

    for (target, (_, value, span, origin)) in main_assignments {
        let lhs = Expression::VarRef {
            name: target.clone(),
            subscripts: vec![],
        };
        let residual = Expression::Binary {
            op: rumoca_ir_ast::OpBinary::Sub(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(value),
        };

        lowered.main.push(rumoca_ir_dae::Equation::residual_array(
            residual,
            span,
            format!("{} ({})", origin, algorithm.origin),
            lookup_algorithm_target_scalar_count(dae, &target),
        ));
    }

    Ok(lowered)
}

fn route_lowered_when_equation(
    dae: &Dae,
    lowered: &mut LoweredAlgorithmPartitions,
    eq: rumoca_ir_dae::Equation,
) {
    let Some(lhs) = eq.lhs.as_ref() else {
        return;
    };
    match discrete_equation_bucket_for_lhs(dae, lhs) {
        Some(DiscreteEquationBucket::DiscreteValued) => lowered.f_m.push(eq),
        Some(DiscreteEquationBucket::DiscreteReal) => lowered.f_z.push(eq),
        None => lowered.main.push(eq),
    }
}

pub(super) fn lower_algorithms_to_equations(dae: &mut Dae, flat: &Model) -> Result<(), ToDaeError> {
    for algorithm in &flat.algorithms {
        match lower_algorithm_to_equations(dae, flat, algorithm) {
            Ok(lowered) => {
                dae.f_x.extend(lowered.main);
                dae.f_z.extend(lowered.f_z);
                dae.f_m.extend(lowered.f_m);
            }
            Err(kind) => {
                return Err(ToDaeError::unsupported_algorithm(
                    "model".to_string(),
                    format!("{} (statement={kind})", algorithm.origin),
                    algorithm.span,
                ));
            }
        }
    }

    for algorithm in &flat.initial_algorithms {
        match lower_algorithm_to_equations(dae, flat, algorithm) {
            Ok(lowered) => {
                if !lowered.f_z.is_empty() || !lowered.f_m.is_empty() {
                    return Err(ToDaeError::unsupported_algorithm(
                        "initial".to_string(),
                        format!("{} (statement=WhenInInitial)", algorithm.origin),
                        algorithm.span,
                    ));
                }
                dae.initial_equations.extend(lowered.main);
            }
            Err(kind) => {
                return Err(ToDaeError::unsupported_algorithm(
                    "initial".to_string(),
                    format!("{} (statement={kind})", algorithm.origin),
                    algorithm.span,
                ));
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn explicit(lhs: &str, rhs: Expression, origin: &str) -> rumoca_ir_dae::Equation {
        rumoca_ir_dae::Equation::explicit(VarName::new(lhs), rhs, Span::DUMMY, origin.to_string())
    }

    fn reaches_source_alias_chain(
        start: &str,
        aliases: &std::collections::HashMap<String, String>,
    ) -> bool {
        let mut cur = start.to_string();
        let mut visited = std::collections::HashSet::<String>::new();
        for _ in 0..16 {
            if cur == "src" || !visited.insert(cur.clone()) {
                return cur == "src";
            }
            let next = match aliases.get(&cur) {
                Some(next) => next,
                None => return false,
            };
            cur = next.clone();
        }
        false
    }

    #[test]
    fn canonicalize_discrete_assignments_reroutes_connection_aliases_for_defined_target() {
        let mut dae = Dae::new();
        for name in ["y", "u", "v"] {
            dae.discrete_valued
                .insert(VarName::new(name), dae::Variable::new(VarName::new(name)));
        }

        dae.f_m.push(explicit(
            "y",
            Expression::Literal(Literal::Integer(1)),
            "explicit equation from source",
        ));
        dae.f_m.push(explicit(
            "y",
            Expression::VarRef {
                name: VarName::new("u"),
                subscripts: vec![],
            },
            "connection equation: y = u",
        ));
        dae.f_m.push(explicit(
            "y",
            Expression::VarRef {
                name: VarName::new("v"),
                subscripts: vec![],
            },
            "connection equation: y = v",
        ));

        canonicalize_discrete_assignment_equations(&mut dae);

        let mut has_source = false;
        let mut has_u_alias = false;
        let mut has_v_alias = false;
        for eq in &dae.f_m {
            let Some(lhs) = eq.lhs.as_ref() else {
                continue;
            };
            match lhs.as_str() {
                "y" => {
                    has_source |= matches!(eq.rhs, Expression::Literal(Literal::Integer(1)));
                    assert!(
                        !is_connection_equation_origin(&eq.origin),
                        "y should keep only non-connection defining equation"
                    );
                }
                "u" => {
                    has_u_alias |= matches!(
                        eq.rhs,
                        Expression::VarRef { ref name, ref subscripts }
                            if name.as_str() == "y" && subscripts.is_empty()
                    );
                }
                "v" => {
                    has_v_alias |= matches!(
                        eq.rhs,
                        Expression::VarRef { ref name, ref subscripts }
                            if name.as_str() == "y" && subscripts.is_empty()
                    );
                }
                _ => {}
            }
        }

        assert!(has_source, "expected source definition for y");
        assert!(has_u_alias, "expected rerouted alias u = y");
        assert!(has_v_alias, "expected rerouted alias v = y");
    }

    #[test]
    fn canonicalize_discrete_assignments_resolves_reroute_target_collisions() {
        let mut dae = Dae::new();
        for name in ["y", "u", "v"] {
            dae.discrete_valued
                .insert(VarName::new(name), dae::Variable::new(VarName::new(name)));
        }

        // Source definition + alias into u.
        dae.f_m.push(explicit(
            "y",
            Expression::Literal(Literal::Integer(1)),
            "explicit equation from source",
        ));
        dae.f_m.push(explicit(
            "y",
            Expression::VarRef {
                name: VarName::new("u"),
                subscripts: vec![],
            },
            "connection equation: y = u",
        ));
        // Existing alias chain from u to v.
        dae.f_m.push(explicit(
            "u",
            Expression::VarRef {
                name: VarName::new("v"),
                subscripts: vec![],
            },
            "connection equation: u = v",
        ));

        canonicalize_discrete_assignment_equations(&mut dae);

        let mut lhs_counts: IndexMap<String, usize> = IndexMap::new();
        let mut has_u_from_y = false;
        let mut has_v_from_u = false;
        let mut has_y_source = false;

        for eq in &dae.f_m {
            if let Some(lhs) = eq.lhs.as_ref() {
                *lhs_counts.entry(lhs.as_str().to_string()).or_default() += 1;
            }
            match eq.lhs.as_ref().map(VarName::as_str) {
                Some("y") => {
                    has_y_source |= matches!(eq.rhs, Expression::Literal(Literal::Integer(1)));
                }
                Some("u") => {
                    has_u_from_y |= matches!(
                        eq.rhs,
                        Expression::VarRef { ref name, ref subscripts }
                            if name.as_str() == "y" && subscripts.is_empty()
                    );
                }
                Some("v") => {
                    has_v_from_u |= matches!(
                        eq.rhs,
                        Expression::VarRef { ref name, ref subscripts }
                            if name.as_str() == "u" && subscripts.is_empty()
                    );
                }
                _ => {}
            }
        }

        for (lhs, count) in lhs_counts {
            assert!(
                count <= 1,
                "duplicate discrete assignment target after canonicalize: {lhs} ({count})"
            );
        }
        assert!(has_y_source, "expected source definition for y");
        assert!(has_u_from_y, "expected rerouted alias u = y");
        assert!(has_v_from_u, "expected collision resolution alias v = u");
    }

    #[test]
    fn canonicalize_discrete_assignments_preserves_chain_connectivity_to_source() {
        let mut dae = Dae::new();
        for name in ["src", "a", "b", "c"] {
            dae.discrete_valued
                .insert(VarName::new(name), dae::Variable::new(VarName::new(name)));
        }

        // Source definition plus a connection chain that can oscillate under
        // flip-based collision resolution if keep-edge selection is unstable.
        dae.f_m.push(explicit(
            "src",
            Expression::Literal(Literal::Integer(1)),
            "explicit equation from source",
        ));
        dae.f_m.push(explicit(
            "src",
            Expression::VarRef {
                name: VarName::new("a"),
                subscripts: vec![],
            },
            "connection equation: src = a",
        ));
        dae.f_m.push(explicit(
            "a",
            Expression::VarRef {
                name: VarName::new("b"),
                subscripts: vec![],
            },
            "connection equation: a = b",
        ));
        dae.f_m.push(explicit(
            "b",
            Expression::VarRef {
                name: VarName::new("c"),
                subscripts: vec![],
            },
            "connection equation: b = c",
        ));

        canonicalize_discrete_assignment_equations(&mut dae);

        let mut lhs_counts: IndexMap<String, usize> = IndexMap::new();
        let mut aliases = std::collections::HashMap::<String, String>::new();
        let mut has_source = false;
        for eq in &dae.f_m {
            let Some(lhs) = eq.lhs.as_ref() else {
                continue;
            };
            *lhs_counts.entry(lhs.as_str().to_string()).or_default() += 1;
            if lhs.as_str() == "src" {
                has_source |= matches!(eq.rhs, Expression::Literal(Literal::Integer(1)));
            }
            if let Expression::VarRef { name, subscripts } = &eq.rhs
                && subscripts.is_empty()
            {
                aliases.insert(lhs.as_str().to_string(), name.as_str().to_string());
            }
        }

        for (lhs, count) in lhs_counts {
            assert!(
                count <= 1,
                "duplicate discrete assignment target after canonicalize: {lhs} ({count})"
            );
        }
        assert!(has_source, "expected source definition for src");

        assert!(
            reaches_source_alias_chain("a", &aliases),
            "expected a to remain connected to src, aliases={aliases:?}"
        );
        assert!(
            reaches_source_alias_chain("b", &aliases),
            "expected b to remain connected to src, aliases={aliases:?}"
        );
        assert!(
            reaches_source_alias_chain("c", &aliases),
            "expected c to remain connected to src, aliases={aliases:?}"
        );
    }
}
