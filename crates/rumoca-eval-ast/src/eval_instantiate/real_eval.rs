use super::*;

pub(super) fn try_eval_real_expr_with_depth_and_scope(
    expr: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<f64> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }

    let recurse =
        |expr| try_eval_real_expr_with_depth_and_scope(expr, env, scope_prefix, depth + 1);

    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedReal | ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse::<f64>().ok(),
        ast::Expression::ComponentReference(comp_ref) => {
            let (resolved_expr, next_scope) = resolve_component_ref_expr(
                comp_ref,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                scope_prefix,
            )?;
            try_eval_real_expr_with_depth_and_scope(
                &resolved_expr,
                env,
                next_scope.as_deref(),
                depth + 1,
            )
        }
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            let l = recurse(lhs)?;
            let r = recurse(rhs)?;
            match op {
                rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => Some(l + r),
                rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => Some(l - r),
                rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => Some(l * r),
                rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => {
                    (r != 0.0).then_some(l / r)
                }
                rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => Some(l.powf(r)),
                _ => None,
            }
        }
        ast::Expression::Unary { op, rhs, .. } => {
            let r = recurse(rhs)?;
            match op {
                rumoca_core::OpUnary::Minus => Some(-r),
                rumoca_core::OpUnary::Plus => Some(r),
                _ => None,
            }
        }
        ast::Expression::Parenthesized { inner, .. } => recurse(inner),
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, branch_expr) in branches {
                match eval_scoped_string_condition_with_depth(
                    condition,
                    env,
                    scope_prefix,
                    depth + 1,
                ) {
                    Some(true) => return recurse(branch_expr),
                    Some(false) => continue,
                    None => return None,
                }
            }
            recurse(else_branch)
        }
        _ => None,
    }
}
