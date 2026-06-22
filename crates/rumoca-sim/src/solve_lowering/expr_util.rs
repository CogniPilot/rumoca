//! Expression / equation helpers shared by the structural lowering and the
//! structural diagnosis stages: duplicate-equation pruning, structural keys for
//! deduplication, and the compact `--trace` rendering.

use rumoca_ir_dae as dae;

pub(super) fn remove_duplicate_continuous_equations(dae: &mut dae::Dae) {
    let mut unique = Vec::with_capacity(dae.continuous.equations.len());
    let mut keys = Vec::with_capacity(dae.continuous.equations.len());
    for eq in dae.continuous.equations.drain(..) {
        let key = duplicate_equation_key(&eq);
        if keys.contains(&key) {
            continue;
        }
        keys.push(key);
        unique.push(eq);
    }
    dae.continuous.equations = unique;
}

fn duplicate_equation_key(eq: &dae::Equation) -> String {
    if let Some((state_name, rhs)) = derivative_residual_signature(&eq.rhs) {
        return format!("der:{state_name}:{}", expression_key(rhs));
    }
    format!("lhs:{:?}:rhs:{}", eq.lhs, expression_key(&eq.rhs))
}

fn derivative_residual_signature(
    expr: &rumoca_core::Expression,
) -> Option<(String, &rumoca_core::Expression)> {
    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = expr else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }
    derivative_target_name(lhs).map(|target| (target, rhs.as_ref()))
}

fn derivative_target_name(expr: &rumoca_core::Expression) -> Option<String> {
    let rumoca_core::Expression::BuiltinCall { function, args, .. } = expr else {
        return None;
    };
    if *function != rumoca_core::BuiltinFunction::Der {
        return None;
    }
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = args.first()?
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    Some(name.as_str().to_string())
}

fn expression_key(expr: &rumoca_core::Expression) -> String {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => format!("var:{}:{subscripts:?}", name.as_str()),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            format!("bin:{op:?}:{}:{}", expression_key(lhs), expression_key(rhs))
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            format!("un:{op:?}:{}", expression_key(rhs))
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => format!(
            "builtin:{function:?}:{}",
            args.iter()
                .map(expression_key)
                .collect::<Vec<_>>()
                .join(",")
        ),
        _ => format!("{expr:?}"),
    }
}

pub(super) fn equation_lhs_prefix(eq: &dae::Equation) -> String {
    match eq.lhs.as_ref() {
        Some(lhs) => format!("{} = ", lhs.as_str()),
        None => String::new(),
    }
}

/// Compact Modelica-ish rendering for `--trace` diagnostics only.
pub(super) fn debug_render_expr(expr: &rumoca_core::Expression) -> String {
    use rumoca_core::Expression as E;
    match expr {
        E::Literal { value, .. } => format!("{value:?}"),
        E::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                name.as_str().to_string()
            } else {
                format!("{}[{} subs]", name.as_str(), subscripts.len())
            }
        }
        E::Binary { op, lhs, rhs, .. } => format!(
            "({} {op:?} {})",
            debug_render_expr(lhs),
            debug_render_expr(rhs)
        ),
        E::Unary { op, rhs, .. } => format!("({op:?} {})", debug_render_expr(rhs)),
        E::BuiltinCall { function, args, .. } => format!(
            "{function:?}({})",
            args.iter()
                .map(debug_render_expr)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        E::FunctionCall { name, args, .. } => format!(
            "{}({})",
            name.as_str(),
            args.iter()
                .map(debug_render_expr)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        E::If {
            branches,
            else_branch,
            ..
        } => format!(
            "if({} branches, else {})",
            branches.len(),
            debug_render_expr(else_branch)
        ),
        other => format!("<{}>", std::any::type_name_of_val(other)),
    }
}
