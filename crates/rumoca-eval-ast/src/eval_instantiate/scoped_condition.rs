use super::*;

pub(super) fn eval_scoped_string_condition_with_depth(
    condition: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<bool> {
    if depth > MAX_CONDITION_DEPTH {
        return None;
    }

    if let Some(val) = expr_to_bool(condition) {
        return Some(val);
    }

    let recurse = |expr: &ast::Expression| {
        eval_scoped_string_condition_with_depth(expr, env, scope_prefix, depth + 1)
    };

    match condition {
        ast::Expression::ComponentReference(comp_ref) => resolve_component_ref_expr(
            comp_ref,
            env.mod_env,
            env.effective_components,
            env.tree,
            env.resolve_class_components,
            scope_prefix,
        )
        .and_then(|(resolved_expr, next_scope)| {
            eval_scoped_string_condition_with_depth(
                &resolved_expr,
                env,
                next_scope.as_deref(),
                depth + 1,
            )
        })
        .or_else(|| {
            evaluate_component_condition_with_depth(
                condition,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                depth + 1,
            )
        }),
        ast::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs,
            ..
        } => recurse(rhs).map(|v| !v),
        ast::Expression::Parenthesized { inner, .. } => recurse(inner),
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (branch_condition, branch_value) in branches {
                match recurse(branch_condition) {
                    Some(true) => return recurse(branch_value),
                    Some(false) => continue,
                    None => return None,
                }
            }
            recurse(else_branch)
        }
        ast::Expression::Binary { op, lhs, rhs, .. } => eval_scoped_string_binary_condition(
            op,
            lhs,
            rhs,
            env,
            ScopedEvalState {
                scope_prefix,
                depth: depth + 1,
            },
        )
        .or_else(|| {
            evaluate_component_condition_with_depth(
                condition,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                depth + 1,
            )
        }),
        _ => evaluate_component_condition_with_depth(
            condition,
            env.mod_env,
            env.effective_components,
            env.tree,
            env.resolve_class_components,
            depth + 1,
        ),
    }
}

struct ScopedEvalState<'a> {
    scope_prefix: Option<&'a str>,
    depth: usize,
}

fn eval_scoped_string_binary_condition(
    op: &rumoca_core::OpBinary,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    state: ScopedEvalState<'_>,
) -> Option<bool> {
    match op {
        rumoca_core::OpBinary::Or => eval_scoped_or(lhs, rhs, env, state.scope_prefix, state.depth),
        rumoca_core::OpBinary::And => {
            eval_scoped_and(lhs, rhs, env, state.scope_prefix, state.depth)
        }
        rumoca_core::OpBinary::Eq => {
            eval_scoped_enum_equality(lhs, rhs, env, state.scope_prefix, state.depth)
        }
        rumoca_core::OpBinary::Neq => {
            eval_scoped_enum_equality(lhs, rhs, env, state.scope_prefix, state.depth).map(|v| !v)
        }
        _ => None,
    }
}

fn eval_scoped_or(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<bool> {
    let recurse = |expr| eval_scoped_string_condition_with_depth(expr, env, scope_prefix, depth);
    let (l, r) = (recurse(lhs), recurse(rhs));
    if l == Some(true) || r == Some(true) {
        return Some(true);
    }
    if l == Some(false) && r == Some(false) {
        return Some(false);
    }
    None
}

fn eval_scoped_and(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<bool> {
    let recurse = |expr| eval_scoped_string_condition_with_depth(expr, env, scope_prefix, depth);
    let (l, r) = (recurse(lhs), recurse(rhs));
    if l == Some(false) || r == Some(false) {
        return Some(false);
    }
    if l == Some(true) && r == Some(true) {
        return Some(true);
    }
    None
}

fn eval_scoped_enum_equality(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<bool> {
    let lhs_val = enum_value_for_comparison_with_depth(
        lhs,
        env.mod_env,
        env.effective_components,
        env.tree,
        env.resolve_class_components,
        scope_prefix,
        depth,
    )?;
    let rhs_val = enum_value_for_comparison_with_depth(
        rhs,
        env.mod_env,
        env.effective_components,
        env.tree,
        env.resolve_class_components,
        scope_prefix,
        depth,
    )?;
    Some(enum_values_equal(&lhs_val, &rhs_val))
}
