//! Boolean expression evaluation for the flatten phase.
//!
//! This module handles evaluation of boolean expressions during flattening:
//! - Literal boolean evaluation
//! - Parameter-based boolean evaluation
//! - Structural parameter checking (MLS §18.3)
//! - Enum value comparison

use rumoca_ir_ast as ast;

use crate::Context;
use crate::equations::build_qualified_name;
use crate::path_utils::parent_scope;

/// Try to evaluate an expression to a constant boolean for structural branch selection.
///
/// Per MLS §18.3, compile-time branch selection is only safe for structural
/// quantities (e.g., parameters marked `Evaluate=true`, `final` parameters, and
/// constants). Non-structural parameters must not be folded here.
///
/// Check if an expression only references structural parameters (Evaluate=true or final).
///
/// Returns true if:
/// - The expression is a literal (no variable references)
/// - All variable references are to structural parameters
///
/// This is used to determine if an expression is safe for compile-time evaluation
/// per MLS §18.3.
pub(crate) fn is_structural_expression(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> bool {
    match expr {
        // Literals are always safe
        ast::Expression::Terminal { .. } => true,

        // Check if component reference is a parameter with a known value
        ast::Expression::ComponentReference(cr) => {
            let cref_name = cr.to_string();

            // Parameters explicitly declared with `fixed=false` are
            // non-structural and must not be folded.
            if scoped_set_contains(&ctx.non_structural_params, &cref_name, prefix) {
                return false;
            }

            // Check if it's a structural parameter (Evaluate=true or final)
            if scoped_set_contains(&ctx.structural_params, &cref_name, prefix) {
                return true;
            }

            // Compile-time constants injected from package/class scopes are
            // structural by definition.
            if scoped_lookup_map(&ctx.constant_values, &cref_name, prefix).is_some() {
                return true;
            }

            // Fallback for constants/parameters injected from class/package scopes
            // that are tracked in scalar lookup maps.
            if scoped_lookup_map(&ctx.parameter_values, &cref_name, prefix).is_some()
                || scoped_lookup_map(&ctx.boolean_parameter_values, &cref_name, prefix).is_some()
                || scoped_lookup_map(&ctx.enum_parameter_values, &cref_name, prefix).is_some()
            {
                return true;
            }

            // Check if it's likely an enumeration literal (not a known parameter)
            // Enumeration literals like Types.FilterType.LowPass are constants
            // They have multiple parts and don't exist as parameters
            if cr.parts.len() > 1 {
                // If it has multiple parts and isn't a known parameter,
                // treat as constant (likely an enumeration literal)
                return true;
            }

            false
        }

        // Recursively check sub-expressions
        ast::Expression::Unary { rhs, .. } => is_structural_expression(ctx, rhs, prefix),

        ast::Expression::Parenthesized { inner } => is_structural_expression(ctx, inner, prefix),

        ast::Expression::Binary { lhs, rhs, .. } => {
            is_structural_expression(ctx, lhs, prefix) && is_structural_expression(ctx, rhs, prefix)
        }

        // Connection graph functions and cardinality are structural (MLS §9.4, §3.7.2.3)
        // They determine connectivity which is known at compile time
        ast::Expression::FunctionCall { comp, .. } => {
            let func_name = comp.to_string();
            matches!(
                func_name.as_str(),
                "Connections.isRoot" | "Connections.rooted" | "cardinality"
            )
        }

        // Other expressions are not safe (other function calls, arrays, etc.)
        _ => false,
    }
}

/// Try to evaluate a boolean expression, but only if it uses structural parameters.
///
/// This is the safe version that respects the Evaluate=true annotation (MLS §18.3).
/// It first checks that all variable references are structural parameters before
/// attempting evaluation. This prevents regressions from evaluating non-structural
/// parameters that may have different values in different contexts.
///
/// Returns Some(value) if:
/// - ast::Expression is a literal
/// - ast::Expression only uses structural parameters and can be evaluated
///
/// Returns None if:
/// - ast::Expression uses non-structural parameters
/// - ast::Expression cannot be evaluated
pub(crate) fn try_eval_structural_boolean(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> Option<bool> {
    // Check if expression only uses structural parameters
    if !is_structural_expression(ctx, expr, prefix) {
        return None;
    }

    // Safe to evaluate with context
    try_eval_boolean_with_ctx_inner(expr, Some(ctx), prefix)
}

/// Inner implementation for boolean evaluation.
pub(crate) fn try_eval_boolean_with_ctx_inner(
    expr: &ast::Expression,
    ctx: Option<&Context>,
    prefix: &ast::QualifiedName,
) -> Option<bool> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
        } => match &*token.text {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },

        ast::Expression::ComponentReference(cr) => {
            let ctx = ctx?;
            let cref_name = cr.to_string();
            scoped_lookup_map(&ctx.boolean_parameter_values, &cref_name, prefix)
        }

        ast::Expression::Unary {
            op: ast::OpUnary::Not(_),
            rhs,
        } => {
            let val = try_eval_boolean_with_ctx_inner(rhs, ctx, prefix)?;
            Some(!val)
        }

        ast::Expression::Parenthesized { inner } => {
            try_eval_boolean_with_ctx_inner(inner, ctx, prefix)
        }

        ast::Expression::Binary { op, lhs, rhs } => {
            eval_boolean_binary_op(op, lhs, rhs, ctx, prefix)
        }

        // Handle connection graph functions (MLS §9.4)
        ast::Expression::FunctionCall { comp, args } => {
            let func_name = comp.to_string();
            match func_name.as_str() {
                "Connections.isRoot" => lookup_vcg_is_root(ctx, args, prefix),
                "Connections.rooted" => lookup_vcg_rooted(ctx, args, prefix),
                _ => None,
            }
        }

        _ => None,
    }
}

/// Evaluate a binary boolean operation.
pub(crate) fn eval_boolean_binary_op(
    op: &ast::OpBinary,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    ctx: Option<&Context>,
    prefix: &ast::QualifiedName,
) -> Option<bool> {
    match op {
        ast::OpBinary::And(_) => {
            let l = try_eval_boolean_with_ctx_inner(lhs, ctx, prefix)?;
            let r = try_eval_boolean_with_ctx_inner(rhs, ctx, prefix)?;
            Some(l && r)
        }
        ast::OpBinary::Or(_) => {
            let l = try_eval_boolean_with_ctx_inner(lhs, ctx, prefix)?;
            let r = try_eval_boolean_with_ctx_inner(rhs, ctx, prefix)?;
            Some(l || r)
        }
        ast::OpBinary::Lt(_) => {
            let l = try_eval_integer_for_comparison(ctx, lhs, prefix)?;
            let r = try_eval_integer_for_comparison(ctx, rhs, prefix)?;
            Some(l < r)
        }
        ast::OpBinary::Le(_) => {
            let l = try_eval_integer_for_comparison(ctx, lhs, prefix)?;
            let r = try_eval_integer_for_comparison(ctx, rhs, prefix)?;
            Some(l <= r)
        }
        ast::OpBinary::Gt(_) => {
            let l = try_eval_integer_for_comparison(ctx, lhs, prefix)?;
            let r = try_eval_integer_for_comparison(ctx, rhs, prefix)?;
            Some(l > r)
        }
        ast::OpBinary::Ge(_) => {
            let l = try_eval_integer_for_comparison(ctx, lhs, prefix)?;
            let r = try_eval_integer_for_comparison(ctx, rhs, prefix)?;
            Some(l >= r)
        }
        ast::OpBinary::Eq(_) => eval_equality(lhs, rhs, ctx, prefix, true),
        ast::OpBinary::Neq(_) => eval_equality(lhs, rhs, ctx, prefix, false),
        _ => None,
    }
}

/// Evaluate equality/inequality between two expressions.
pub(crate) fn eval_equality(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    ctx: Option<&Context>,
    prefix: &ast::QualifiedName,
    is_eq: bool,
) -> Option<bool> {
    // Try integers first, then booleans, then enumerations
    if let (Some(l), Some(r)) = (
        try_eval_integer_for_comparison(ctx, lhs, prefix),
        try_eval_integer_for_comparison(ctx, rhs, prefix),
    ) {
        return Some(if is_eq { l == r } else { l != r });
    }
    if let (Some(l), Some(r)) = (
        try_eval_boolean_with_ctx_inner(lhs, ctx, prefix),
        try_eval_boolean_with_ctx_inner(rhs, ctx, prefix),
    ) {
        return Some(if is_eq { l == r } else { l != r });
    }
    if let (Some(l), Some(r)) = (
        try_resolve_enum_value(ctx, lhs, prefix),
        try_resolve_enum_value(ctx, rhs, prefix),
    ) {
        let equal = rumoca_core::enum_values_equal(&l, &r);
        return Some(if is_eq { equal } else { !equal });
    }
    None
}

/// Try to resolve an expression to an enumeration value string.
///
/// For enumeration literals like `Types.FilterType.LowPass`, returns the qualified name.
/// For parameter references, tries to look up their bound enumeration value.
pub(crate) fn try_resolve_enum_value(
    ctx: Option<&Context>,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> Option<String> {
    match expr {
        ast::Expression::ComponentReference(cr) => {
            let qualified_name = build_qualified_name(prefix, cr);

            // Check if this is a parameter with a known enumeration value
            if let Some(ctx) = ctx
                && let Some(enum_val) = ctx.get_enum_param(&qualified_name)
            {
                return Some(enum_val);
            }
            if let Some(ctx) = ctx
                && cr.parts.len() >= 2
            {
                let cref_name = cr.to_string();
                if let Some(enum_val) =
                    scoped_lookup_map(&ctx.enum_parameter_values, &cref_name, prefix)
                {
                    return Some(enum_val);
                }
                let tail_name = cr
                    .parts
                    .iter()
                    .skip(1)
                    .map(|p| p.ident.text.to_string())
                    .collect::<Vec<_>>()
                    .join(".");
                let alt_name = if prefix.parts.is_empty() {
                    tail_name.clone()
                } else {
                    format!("{prefix}.{tail_name}")
                };
                if let Some(enum_val) = ctx.get_enum_param(&alt_name) {
                    return Some(enum_val);
                }
            }

            // If it looks like an enumeration literal (multiple parts, not a known parameter),
            // return the qualified name directly
            if cr.parts.len() > 1 {
                // It's likely an enumeration literal like Types.FilterType.LowPass
                // Return just the CR parts (not the prefix)
                let literal_name: Vec<String> =
                    cr.parts.iter().map(|p| p.ident.text.to_string()).collect();
                return Some(literal_name.join("."));
            }

            None
        }
        _ => None,
    }
}

/// Try to evaluate an integer expression for comparison purposes.
///
/// This is a simplified version that handles common cases needed for
/// boolean comparison operations (e.g., `n > 0`).
fn try_eval_integer_for_comparison(
    ctx: Option<&Context>,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> Option<i64> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
        } => token.text.parse::<i64>().ok(),

        ast::Expression::ComponentReference(cr) => {
            let ctx = ctx?;
            let cref_name = cr.to_string();
            scoped_lookup_map(&ctx.parameter_values, &cref_name, prefix)
        }

        ast::Expression::Unary {
            op: ast::OpUnary::Minus(_),
            rhs,
        } => {
            let val = try_eval_integer_for_comparison(ctx, rhs, prefix)?;
            Some(-val)
        }

        ast::Expression::Parenthesized { inner } => {
            try_eval_integer_for_comparison(ctx, inner, prefix)
        }

        // MLS §3.7.2.3: cardinality(c) returns the number of connect() statements referencing c
        ast::Expression::FunctionCall { comp, args } => {
            let func_name = comp.to_string();
            if func_name == "cardinality" {
                return lookup_cardinality(ctx, args, prefix);
            }
            None
        }

        _ => None,
    }
}

/// Look up `cardinality(c)` in the pre-computed cardinality counts (MLS §3.7.2.3).
///
/// Qualifies the connector argument using the current prefix and looks up the count
/// in `ctx.cardinality_counts`. Returns 0 for connectors with no connections.
fn lookup_cardinality(
    ctx: Option<&Context>,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
) -> Option<i64> {
    let ctx = ctx?;
    let path = extract_vcg_arg_path(args, prefix)?;
    Some(*ctx.cardinality_counts.get(&path).unwrap_or(&0))
}

fn scoped_lookup_map<T: Clone>(
    map: &rustc_hash::FxHashMap<String, T>,
    name: &str,
    prefix: &ast::QualifiedName,
) -> Option<T> {
    let mut scope = prefix.to_flat_string();
    loop {
        let candidate = if scope.is_empty() {
            name.to_string()
        } else {
            format!("{scope}.{name}")
        };
        if let Some(value) = map.get(&candidate) {
            return Some(value.clone());
        }
        if let Some(parent) = parent_scope(scope.as_str()) {
            scope.truncate(parent.len());
        } else if !scope.is_empty() {
            scope.clear();
        } else {
            break;
        }
    }
    map.get(name).cloned()
}

fn scoped_set_contains(
    set: &std::collections::HashSet<String>,
    name: &str,
    prefix: &ast::QualifiedName,
) -> bool {
    let mut scope = prefix.to_flat_string();
    loop {
        let candidate = if scope.is_empty() {
            name.to_string()
        } else {
            format!("{scope}.{name}")
        };
        if set.contains(&candidate) {
            return true;
        }
        if let Some(parent) = parent_scope(scope.as_str()) {
            scope.truncate(parent.len());
        } else if !scope.is_empty() {
            scope.clear();
        } else {
            break;
        }
    }
    set.contains(name)
}

/// Look up `Connections.isRoot(R)` in the VCG map (MLS §9.4).
///
/// Qualifies the argument using the current prefix and looks up the result
/// in `ctx.vcg_is_root`. Falls back to `Some(true)` when no VCG data exists
/// (preserves previous behavior for models without overconstrained connectors).
fn lookup_vcg_is_root(
    ctx: Option<&Context>,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
) -> Option<bool> {
    let ctx = ctx?;
    let path = extract_vcg_arg_path(args, prefix)?;
    match ctx.vcg_is_root.get(&path) {
        Some(&val) => Some(val),
        None => Some(true), // Fallback for models without VCG data
    }
}

/// Look up `Connections.rooted(R)` in the VCG map (MLS §9.4).
///
/// Qualifies the argument using the current prefix and looks up the result
/// in `ctx.vcg_rooted`. Falls back to `Some(false)` when no VCG data exists
/// (preserves previous behavior for models without overconstrained connectors).
fn lookup_vcg_rooted(
    ctx: Option<&Context>,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
) -> Option<bool> {
    let ctx = ctx?;
    let path = extract_vcg_arg_path(args, prefix)?;
    match ctx.vcg_rooted.get(&path) {
        Some(&val) => Some(val),
        None => Some(false), // Fallback for models without VCG data
    }
}

/// Extract the qualified path from the first argument of a VCG function call.
///
/// `Connections.isRoot(frame_a.R)` with prefix "body" → "body.frame_a.R"
fn extract_vcg_arg_path(args: &[ast::Expression], prefix: &ast::QualifiedName) -> Option<String> {
    match args.first()? {
        ast::Expression::ComponentReference(cr) => Some(build_qualified_name(prefix, cr)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::sync::Arc;

    use super::*;
    use rumoca_ir_ast as ast;
    use rustc_hash::FxHashMap;

    fn prefix_with_subscript_dot() -> ast::QualifiedName {
        let mut prefix = ast::QualifiedName::new();
        prefix.push("pkg".to_string(), vec![]);
        prefix.push("arr[data.medium]".to_string(), vec![]);
        prefix
    }

    #[test]
    fn scoped_lookup_map_ignores_dot_inside_subscript_scope() {
        let mut map = FxHashMap::default();
        map.insert("pkg.arr[data.value".to_string(), 99);
        map.insert("pkg.value".to_string(), 7);

        let resolved = scoped_lookup_map(&map, "value", &prefix_with_subscript_dot());
        assert_eq!(resolved, Some(7));
    }

    #[test]
    fn scoped_set_contains_ignores_dot_inside_subscript_scope() {
        let set = HashSet::from(["pkg.arr[data.flag".to_string()]);
        assert!(
            !scoped_set_contains(&set, "flag", &prefix_with_subscript_dot()),
            "dot inside bracketed subscripts must not create a valid parent scope"
        );
    }

    fn token(text: &str) -> ast::Token {
        ast::Token {
            text: Arc::from(text.to_string()),
            ..ast::Token::default()
        }
    }

    fn comp_ref(path: &str) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: path
                .split('.')
                .map(|part| ast::ComponentRefPart {
                    ident: token(part),
                    subs: None,
                })
                .collect(),
            def_id: None,
        }
    }

    fn cref_expr(path: &str) -> ast::Expression {
        ast::Expression::ComponentReference(comp_ref(path))
    }

    fn eq_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
        ast::Expression::Binary {
            op: ast::OpBinary::Eq(token("==")),
            lhs: Arc::new(lhs),
            rhs: Arc::new(rhs),
        }
    }

    #[test]
    fn enum_equality_accepts_suffix_qualification() {
        let mut ctx = Context::new();
        ctx.enum_parameter_values.insert(
            "controllerType".to_string(),
            "Modelica.Blocks.Types.SimpleController.PI".to_string(),
        );

        let expr = eq_expr(
            cref_expr("controllerType"),
            cref_expr("SimpleController.PI"),
        );
        let value = try_eval_boolean_with_ctx_inner(&expr, Some(&ctx), &ast::QualifiedName::new());
        assert_eq!(value, Some(true));
    }

    #[test]
    fn enum_equality_accepts_shared_type_literal_tail() {
        let mut ctx = Context::new();
        ctx.enum_parameter_values.insert(
            "frameResolve".to_string(),
            "sensor_frame_a2.MultiBody.Types.ResolveInFrameA.frame_resolve".to_string(),
        );

        let expr = eq_expr(
            cref_expr("frameResolve"),
            cref_expr("Modelica.Mechanics.MultiBody.Types.ResolveInFrameA.frame_resolve"),
        );
        let value = try_eval_boolean_with_ctx_inner(&expr, Some(&ctx), &ast::QualifiedName::new());
        assert_eq!(value, Some(true));
    }

    #[test]
    fn enum_equality_rejects_different_enum_type() {
        let mut ctx = Context::new();
        ctx.enum_parameter_values.insert(
            "mode".to_string(),
            "Modelica.Blocks.Types.Init.PI".to_string(),
        );

        let expr = eq_expr(
            cref_expr("mode"),
            cref_expr("Modelica.Blocks.Types.SimpleController.PI"),
        );
        let value = try_eval_boolean_with_ctx_inner(&expr, Some(&ctx), &ast::QualifiedName::new());
        assert_eq!(value, Some(false));
    }

    #[test]
    fn non_structural_boolean_parameter_is_not_structural() {
        let mut ctx = Context::new();
        ctx.boolean_parameter_values.insert("cp".to_string(), false);
        ctx.non_structural_params.insert("cp".to_string());

        let expr = cref_expr("cp");
        assert!(
            !is_structural_expression(&ctx, &expr, &ast::QualifiedName::new()),
            "plain known boolean parameter must not be treated as structural"
        );
        assert_eq!(
            try_eval_structural_boolean(&ctx, &expr, &ast::QualifiedName::new()),
            None
        );
    }

    #[test]
    fn structural_parameter_can_be_evaluated_for_branch_selection() {
        let mut ctx = Context::new();
        ctx.boolean_parameter_values.insert("cp".to_string(), false);
        ctx.structural_params.insert("cp".to_string());

        let expr = cref_expr("cp");
        assert!(is_structural_expression(
            &ctx,
            &expr,
            &ast::QualifiedName::new()
        ));
        assert_eq!(
            try_eval_structural_boolean(&ctx, &expr, &ast::QualifiedName::new()),
            Some(false)
        );
    }

    #[test]
    fn known_constant_boolean_from_lookup_is_structural() {
        let mut ctx = Context::new();
        ctx.boolean_parameter_values
            .insert("fixedX".to_string(), false);

        let expr = cref_expr("fixedX");
        assert!(is_structural_expression(
            &ctx,
            &expr,
            &ast::QualifiedName::new()
        ));
        assert_eq!(
            try_eval_structural_boolean(&ctx, &expr, &ast::QualifiedName::new()),
            Some(false)
        );
    }
}
