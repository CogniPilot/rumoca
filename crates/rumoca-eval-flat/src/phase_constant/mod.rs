//! Flat expression evaluation for the flatten phase.
//!
//! SPEC_0021 file-size exception: split plan is to move focused evaluation
//! helpers into owned submodules after BOPTEST parity stabilization.
//!
//! This module provides evaluation functions for flat expressions during the
//! flattening phase. It handles:
//! - Integer expression evaluation (parameters, builtins, user functions)
//! - Real expression evaluation
//! - Boolean expression evaluation (comparisons, logical operations)
//! - Array dimension inference from bindings
//! - Enumeration value resolution
//!
//! These functions are used for compile-time constant evaluation per MLS §4.4.

use rustc_hash::FxHashMap;

use crate::constant::{EvalContext, Value};
use rumoca_core::eval_integer_div_builtin;
use rumoca_ir_flat as flat;

use rumoca_core::{ComponentPath, scoped_component_path_candidates, split_path_with_indices};

const NAMED_CALL_ARG_PREFIX: &str = "__rumoca_named_arg__.";

// Conditional tracing support (SPEC_0024)
#[cfg(feature = "tracing")]
use tracing::{debug, warn};

/// Build an EvalContext from known parameter values and functions.
pub fn build_eval_context(
    known_ints: &FxHashMap<String, i64>,
    known_reals: &FxHashMap<String, f64>,
    known_bools: &FxHashMap<String, bool>,
    array_dims: &FxHashMap<String, Vec<i64>>,
    functions: &FxHashMap<String, rumoca_core::Function>,
) -> EvalContext {
    let parameter_capacity =
        known_ints.len() + known_reals.len() + known_bools.len() + array_dims.len();
    let mut eval_ctx = EvalContext::with_capacity(parameter_capacity, 0, functions.len() * 2);
    for (k, v) in known_ints {
        eval_ctx.add_parameter(k.clone(), Value::Integer(*v));
    }
    for (k, v) in known_reals {
        eval_ctx.add_parameter(k.clone(), Value::Real(*v));
    }
    for (k, v) in known_bools {
        eval_ctx.add_parameter(k.clone(), Value::Bool(*v));
    }
    for (k, v) in array_dims {
        if v.len() == 1 {
            let arr: Vec<Value> = (0..v[0]).map(|_| Value::Integer(0)).collect();
            eval_ctx.add_parameter(k.clone(), Value::Array(arr));
        }
    }
    for func in functions.values() {
        eval_ctx.add_function(func.clone());
    }
    eval_ctx
}

/// Context for compile-time parameter expression evaluation (MLS §4.4).
pub struct ParamEvalContext<'a> {
    pub known_ints: &'a FxHashMap<String, i64>,
    pub known_reals: &'a FxHashMap<String, f64>,
    pub known_bools: &'a FxHashMap<String, bool>,
    pub known_enums: &'a FxHashMap<String, String>,
    pub array_dims: &'a FxHashMap<String, Vec<i64>>,
    /// Functions available for evaluation.
    pub functions: &'a FxHashMap<String, rumoca_core::Function>,
    /// The fully qualified name of the variable whose binding we're evaluating.
    /// Used to resolve unqualified modification bindings to parent scope (MLS §7.2).
    pub var_context: Option<&'a str>,
}

impl<'a> ParamEvalContext<'a> {
    pub fn new(
        known_ints: &'a FxHashMap<String, i64>,
        known_reals: &'a FxHashMap<String, f64>,
        known_bools: &'a FxHashMap<String, bool>,
        known_enums: &'a FxHashMap<String, String>,
        array_dims: &'a FxHashMap<String, Vec<i64>>,
        functions: &'a FxHashMap<String, rumoca_core::Function>,
        var_context: Option<&'a str>,
    ) -> Self {
        Self {
            known_ints,
            known_reals,
            known_bools,
            known_enums,
            array_dims,
            functions,
            var_context,
        }
    }
}

/// Try to evaluate a flat expression to an integer value with context and array dimensions.
///
/// Same as try_eval_flat_expr_integer but also handles size() calls using array dimensions.
pub fn try_eval_flat_expr_integer_with_dims(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<i64> {
    // Call with empty bools/enums/functions (convenience for callers without those contexts)
    let ctx = ParamEvalContext {
        known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &FxHashMap::default(),
        array_dims,
        functions: &FxHashMap::default(),
        var_context: None,
    };
    try_eval_integer_with_context(expr, &ctx)
}

/// Integer evaluation with full context.
pub fn try_eval_integer_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext,
) -> Option<i64> {
    let result = match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(n),
            ..
        } => Some(*n),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(r),
            ..
        } => {
            if r.fract() == 0.0 {
                Some(*r as i64)
            } else {
                None
            }
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => resolve_varref_integer(name.as_str(), ctx),
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let base_name = flatten_field_access_path(base)?;
            let field_name = format!("{base_name}.{field}");
            resolve_varref_integer(&field_name, ctx)
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            let val = try_eval_integer_with_context(rhs, ctx)?;
            match op {
                rumoca_core::OpUnary::Minus => Some(-val),
                rumoca_core::OpUnary::Plus => Some(val),
                _ => None,
            }
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let l = try_eval_integer_with_context(lhs, ctx)?;
            let r = try_eval_integer_with_context(rhs, ctx)?;
            rumoca_core::eval_ast_integer_binary(op, l, r)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => eval_integer_if_expression(branches, else_branch, ctx),
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            #[cfg(feature = "tracing")]
            debug!(function = ?function, arg_count = args.len(), "evaluating builtin call");
            eval_builtin_integer_with_context(function, args, ctx)
        }
        rumoca_core::Expression::FunctionCall { name, args, .. } => {
            #[cfg(feature = "tracing")]
            debug!(function = %name, arg_count = args.len(), "evaluating user function call");
            eval_user_func_integer(name, args, ctx)
        }
        _ => {
            #[cfg(feature = "tracing")]
            warn!(
                expr_kind = std::any::type_name_of_val(expr),
                "unhandled expression kind"
            );
            None
        }
    };

    #[cfg(feature = "tracing")]
    if result.is_some() {
        debug!(result = ?result, "expression evaluated successfully");
    }

    result
}

fn flatten_field_access_path(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name.to_string()),
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let base_path = flatten_field_access_path(base)?;
            Some(format!("{base_path}.{field}"))
        }
        _ => None,
    }
}

fn eval_integer_if_expression(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    ctx: &ParamEvalContext,
) -> Option<i64> {
    let mut unknown_branch_values: Vec<i64> = Vec::new();
    for (cond, then_expr) in branches {
        match try_eval_flat_expr_boolean_with_context(cond, ctx) {
            Some(true) => return try_eval_integer_with_context(then_expr, ctx),
            Some(false) => continue,
            None => unknown_branch_values.push(try_eval_integer_with_context(then_expr, ctx)?),
        }
    }

    let else_value = try_eval_integer_with_context(else_branch, ctx)?;
    if unknown_branch_values.is_empty() {
        return Some(else_value);
    }
    unknown_branch_values
        .iter()
        .all(|value| *value == else_value)
        .then_some(else_value)
}

/// Try to evaluate a flat expression to a boolean value with full context.
///
/// This extends `try_eval_flat_expr_boolean` with scoped VarRef resolution
/// via `var_context` (MLS §7.2), so unqualified enum/bool refs in parameter
/// bindings can be evaluated while computing integer if-expressions.
pub fn try_eval_flat_expr_boolean_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext,
) -> Option<bool> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(b),
            ..
        } => Some(*b),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => resolve_varref_boolean(name.as_str(), ctx),
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs,
            ..
        } => try_eval_flat_expr_boolean_with_context(rhs, ctx).map(|v| !v),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            try_eval_flat_expr_boolean_binary_with_context(op, lhs, rhs, ctx)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                match try_eval_flat_expr_boolean_with_context(cond, ctx) {
                    Some(true) => return try_eval_flat_expr_boolean_with_context(then_expr, ctx),
                    Some(false) => continue,
                    None => return None,
                }
            }
            try_eval_flat_expr_boolean_with_context(else_branch, ctx)
        }
        _ => None,
    }
}

fn try_eval_flat_expr_boolean_binary_with_context(
    op: &rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    ctx: &ParamEvalContext,
) -> Option<bool> {
    match op {
        rumoca_core::OpBinary::And => Some(
            try_eval_flat_expr_boolean_with_context(lhs, ctx)?
                && try_eval_flat_expr_boolean_with_context(rhs, ctx)?,
        ),
        rumoca_core::OpBinary::Or => Some(
            try_eval_flat_expr_boolean_with_context(lhs, ctx)?
                || try_eval_flat_expr_boolean_with_context(rhs, ctx)?,
        ),
        rumoca_core::OpBinary::Eq | rumoca_core::OpBinary::Neq => {
            let is_eq = matches!(op, rumoca_core::OpBinary::Eq);

            // Try integer comparison first.
            if let (Some(l), Some(r)) = (
                try_eval_integer_with_context(lhs, ctx),
                try_eval_integer_with_context(rhs, ctx),
            ) {
                return Some(if is_eq { l == r } else { l != r });
            }

            // Then boolean comparison.
            if let (Some(l), Some(r)) = (
                try_eval_flat_expr_boolean_with_context(lhs, ctx),
                try_eval_flat_expr_boolean_with_context(rhs, ctx),
            ) {
                return Some(if is_eq { l == r } else { l != r });
            }

            // Finally enum comparison (with scoped reference lookup).
            if let (Some(l), Some(r)) = (
                resolve_enum_value_with_context(lhs, ctx),
                resolve_enum_value_with_context(rhs, ctx),
            ) {
                let l_norm = canonicalize_enum_literal(&l, ctx.known_enums);
                let r_norm = canonicalize_enum_literal(&r, ctx.known_enums);
                let equal = rumoca_core::enum_values_equal(&l_norm, &r_norm);
                return Some(if is_eq { equal } else { !equal });
            }

            None
        }
        rumoca_core::OpBinary::Lt => Some(
            try_eval_integer_with_context(lhs, ctx)? < try_eval_integer_with_context(rhs, ctx)?,
        ),
        rumoca_core::OpBinary::Le => Some(
            try_eval_integer_with_context(lhs, ctx)? <= try_eval_integer_with_context(rhs, ctx)?,
        ),
        rumoca_core::OpBinary::Gt => Some(
            try_eval_integer_with_context(lhs, ctx)? > try_eval_integer_with_context(rhs, ctx)?,
        ),
        rumoca_core::OpBinary::Ge => Some(
            try_eval_integer_with_context(lhs, ctx)? >= try_eval_integer_with_context(rhs, ctx)?,
        ),
        _ => None,
    }
}

fn lookup_scoped_copy<T: Copy>(
    values: &FxHashMap<String, T>,
    name: &str,
    var_context: Option<&str>,
) -> Option<T> {
    let scope = var_context
        .map(ComponentPath::from_flat_path)
        .and_then(|path| path.parent())?;
    let name_path = ComponentPath::from_flat_path(name);
    for candidate in scoped_component_path_candidates(&name_path, &scope) {
        if let Some(val) = values.get(&candidate).copied() {
            return Some(val);
        }
    }
    values.get(name).copied()
}

fn lookup_scoped_cloned<T: Clone>(
    values: &FxHashMap<String, T>,
    name: &str,
    var_context: Option<&str>,
) -> Option<T> {
    let scope = var_context
        .map(ComponentPath::from_flat_path)
        .and_then(|path| path.parent())?;
    let name_path = ComponentPath::from_flat_path(name);
    for candidate in scoped_component_path_candidates(&name_path, &scope) {
        if let Some(val) = values.get(&candidate) {
            return Some(val.clone());
        }
    }
    values.get(name).cloned()
}

fn is_unqualified_component_name(name: &str) -> bool {
    ComponentPath::from_flat_path(name).len() == 1
}

fn resolve_varref_boolean(name_str: &str, ctx: &ParamEvalContext) -> Option<bool> {
    if is_unqualified_component_name(name_str)
        && let Some(val) = lookup_scoped_copy(ctx.known_bools, name_str, ctx.var_context)
    {
        return Some(val);
    }

    if let Some(val) = ctx.known_bools.get(name_str).copied() {
        return Some(val);
    }

    if let Some(val) = lookup_scoped_copy(ctx.known_bools, name_str, ctx.var_context) {
        return Some(val);
    }

    lookup_unique_suffix_copy(name_str, ctx.known_bools)
}

fn resolve_enum_value_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext,
) -> Option<String> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }

    let name_str = name.to_string();
    if is_unqualified_component_name(&name_str)
        && let Some(enum_val) = lookup_scoped_cloned(ctx.known_enums, &name_str, ctx.var_context)
    {
        return Some(enum_val);
    }

    if let Some(enum_val) = ctx.known_enums.get(&name_str) {
        return Some(enum_val.clone());
    }

    if let Some(enum_val) = lookup_scoped_cloned(ctx.known_enums, &name_str, ctx.var_context) {
        return Some(enum_val);
    }

    if let Some(enum_val) = lookup_unique_suffix_cloned(&name_str, ctx.known_enums) {
        return Some(enum_val);
    }

    try_extract_enum_value(expr).map(|literal| canonicalize_enum_literal(&literal, ctx.known_enums))
}

/// Resolve an unqualified variable reference in parent scopes (MLS §7.2).
///
/// For modification bindings like `G1(n=n)`, the RHS `n` references the outer scope
/// where the modification was written. This function walks up the parent chain
/// to find the value.
fn resolve_in_parent_scope(
    name: &str,
    var_context: &str,
    known_ints: &FxHashMap<String, i64>,
) -> Option<i64> {
    let scope = ComponentPath::from_flat_path(var_context).parent()?;
    let name_path = ComponentPath::from_flat_path(name);
    if let Some(candidate) = lowercase_type_ref_candidate(&name_path, &scope)
        && let Some(val) = known_ints.get(&candidate).copied()
    {
        return Some(val);
    }
    for candidate in scoped_component_path_candidates(&name_path, &scope) {
        if let Some(val) = known_ints.get(&candidate).copied() {
            return Some(val);
        }
    }

    known_ints.get(name).copied()
}

fn lowercase_type_ref_candidate(name: &ComponentPath, scope: &ComponentPath) -> Option<String> {
    let first = name.parts().first()?;
    if !first.starts_with(char::is_uppercase) {
        return None;
    }
    let lowered = first[..1].to_lowercase() + &first[1..];
    let mut parts = scope.parts().to_vec();
    parts.push(lowered);
    parts.extend(name.parts().iter().skip(1).cloned());
    Some(parts.join("."))
}

/// Resolve a qualified name by stripping leading segments only when unambiguous.
///
/// Modification nesting can produce over-qualified bindings. For example,
/// `MultiStarResistance(m=data.m)` contains `MultiStar(m=m)` internally.
/// The inner binding for `multiStar.multiStar.m` becomes `multiStar.data.m`
/// when the actual parameter is `data.m` at the top level.
///
/// The fallback is intentionally conservative: if more than one stripped
/// suffix is known, the lookup fails instead of silently choosing a shorter or
/// longer suffix. That avoids resolving `a.b.n` to `b.n` when both `b.n` and
/// `n` are in scope.
fn resolve_by_suffix_stripping(name: &str, known_ints: &FxHashMap<String, i64>) -> Option<i64> {
    lookup_unique_suffix_copy(name, known_ints)
}

fn lookup_unique_suffix_copy<T: Copy>(name: &str, values: &FxHashMap<String, T>) -> Option<T> {
    let mut found = None;
    for suffix in ComponentPath::from_flat_path(name).suffixes_excluding_self() {
        let candidate = suffix.to_flat_string();
        if let Some(val) = values.get(&candidate).copied() {
            if found.is_some() {
                return None;
            }
            found = Some(val);
        }
    }
    found
}

fn lookup_unique_suffix_cloned<T: Clone>(name: &str, values: &FxHashMap<String, T>) -> Option<T> {
    let mut found = None;
    for suffix in ComponentPath::from_flat_path(name).suffixes_excluding_self() {
        let candidate = suffix.to_flat_string();
        if let Some(val) = values.get(&candidate).cloned() {
            if found.is_some() {
                return None;
            }
            found = Some(val);
        }
    }
    found
}

/// Resolve a VarRef to an integer value using all available strategies.
///
/// Tries in order: lexical/component scope for unqualified refs (MLS §7.2),
/// direct integer lookup, real-to-integer conversion, parent scope resolution
/// for qualified refs, and suffix stripping for over-qualified refs.
fn resolve_varref_integer(name_str: &str, ctx: &ParamEvalContext) -> Option<i64> {
    if is_unqualified_component_name(name_str)
        && let Some(var_ctx) = ctx.var_context
        && let Some(val) = resolve_in_parent_scope(name_str, var_ctx, ctx.known_ints)
    {
        return Some(val);
    }

    // Direct lookup in integers
    if let Some(val) = ctx.known_ints.get(name_str).copied() {
        return Some(val);
    }
    // Try real parameters that are whole numbers (e.g., Real m = 3)
    if let Some(val) = ctx.known_reals.get(name_str).copied()
        && val.fract() == 0.0
        && val.is_finite()
    {
        return Some(val as i64);
    }
    // For modification bindings, try parent scope resolution (MLS §7.2)
    if let Some(var_ctx) = ctx.var_context
        && let Some(val) = resolve_in_parent_scope(name_str, var_ctx, ctx.known_ints)
    {
        return Some(val);
    }
    // Fallback: try stripping leading segments from qualified refs.
    // Modification nesting can produce over-qualified bindings like
    // "multiStar.data.m" when the actual parameter is "data.m".
    resolve_by_suffix_stripping(name_str, ctx.known_ints)
}

/// Resolve a VarRef to a real value using all available strategies.
fn resolve_varref_real(name_str: &str, ctx: &ParamEvalContext) -> Option<f64> {
    if is_unqualified_component_name(name_str)
        && let Some(var_ctx) = ctx.var_context
        && let Some(scope) = ComponentPath::from_flat_path(var_ctx).parent()
    {
        let name_path = ComponentPath::from_flat_path(name_str);
        for candidate in scoped_component_path_candidates(&name_path, &scope) {
            if let Some(val) = ctx.known_reals.get(&candidate).copied() {
                return Some(val);
            }
            if let Some(val) = ctx.known_ints.get(&candidate).copied() {
                return Some(val as f64);
            }
        }
    }

    if let Some(val) = ctx.known_reals.get(name_str).copied() {
        return Some(val);
    }
    if let Some(val) = ctx.known_ints.get(name_str).copied() {
        return Some(val as f64);
    }

    if let Some(var_ctx) = ctx.var_context
        && let Some(scope) = ComponentPath::from_flat_path(var_ctx).parent()
    {
        let name_path = ComponentPath::from_flat_path(name_str);
        for candidate in scoped_component_path_candidates(&name_path, &scope) {
            if let Some(val) = ctx.known_reals.get(&candidate).copied() {
                return Some(val);
            }
            if let Some(val) = ctx.known_ints.get(&candidate).copied() {
                return Some(val as f64);
            }
        }
    }

    lookup_unique_suffix_real(name_str, ctx)
}

fn lookup_unique_suffix_real(name: &str, ctx: &ParamEvalContext) -> Option<f64> {
    let mut found = None;
    for suffix in ComponentPath::from_flat_path(name).suffixes_excluding_self() {
        let candidate = suffix.to_flat_string();
        let value = ctx
            .known_reals
            .get(&candidate)
            .copied()
            .or_else(|| ctx.known_ints.get(&candidate).map(|val| *val as f64));
        if let Some(value) = value {
            if found.is_some() {
                return None;
            }
            found = Some(value);
        }
    }
    found
}

/// Evaluate a flat expression to a real using scoped lookup context.
fn try_eval_real_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext,
) -> Option<f64> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(v),
            ..
        } => Some(*v),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(v),
            ..
        } => Some(*v as f64),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => resolve_varref_real(name.as_str(), ctx),
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let base_name = flatten_field_access_path(base)?;
            resolve_varref_real(&format!("{base_name}.{field}"), ctx)
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            let val = try_eval_real_with_context(rhs, ctx)?;
            match op {
                rumoca_core::OpUnary::Minus => Some(-val),
                rumoca_core::OpUnary::Plus => Some(val),
                _ => None,
            }
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let l = try_eval_real_with_context(lhs, ctx)?;
            let r = try_eval_real_with_context(rhs, ctx)?;
            match op {
                rumoca_core::OpBinary::Add => Some(l + r),
                rumoca_core::OpBinary::Sub => Some(l - r),
                rumoca_core::OpBinary::Mul => Some(l * r),
                rumoca_core::OpBinary::Div => (r != 0.0).then_some(l / r),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Evaluate builtin function calls that return integer, with full context for scope resolution.
fn eval_builtin_integer_with_context(
    function: &rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext,
) -> Option<i64> {
    let known_ints = ctx.known_ints;
    let array_dims = ctx.array_dims;
    let result = match function {
        rumoca_core::BuiltinFunction::Floor => {
            let arg = args.first()?;
            try_eval_real_with_context(arg, ctx).map(|v| v.floor() as i64)
        }
        rumoca_core::BuiltinFunction::Ceil => {
            let arg = args.first()?;
            try_eval_real_with_context(arg, ctx).map(|v| v.ceil() as i64)
        }
        rumoca_core::BuiltinFunction::Integer => {
            let arg = args.first()?;
            try_eval_integer_with_context(arg, ctx)
                .or_else(|| try_eval_real_with_context(arg, ctx).map(|v| v as i64))
        }
        rumoca_core::BuiltinFunction::Abs => {
            let arg = args.first()?;
            try_eval_integer_with_context(arg, ctx).map(|v| v.abs())
        }
        rumoca_core::BuiltinFunction::Div if args.len() >= 2 => {
            let x = try_eval_integer_with_context(&args[0], ctx)?;
            let y = try_eval_integer_with_context(&args[1], ctx)?;
            eval_integer_div_builtin(x, y)
        }
        rumoca_core::BuiltinFunction::Mod if args.len() >= 2 => {
            let x = try_eval_integer_with_context(&args[0], ctx)?;
            let y = try_eval_integer_with_context(&args[1], ctx)?;
            (y != 0).then(|| eval_integer_mod_builtin(x, y))
        }
        rumoca_core::BuiltinFunction::Rem if args.len() >= 2 => {
            let x = try_eval_integer_with_context(&args[0], ctx)?;
            let y = try_eval_integer_with_context(&args[1], ctx)?;
            (y != 0).then_some(x % y)
        }
        rumoca_core::BuiltinFunction::Max => {
            #[cfg(feature = "tracing")]
            debug!("evaluating max() builtin");
            eval_max_min_integer_with_context(args, ctx, true)
        }
        rumoca_core::BuiltinFunction::Min => {
            #[cfg(feature = "tracing")]
            debug!("evaluating min() builtin");
            eval_max_min_integer_with_context(args, ctx, false)
        }
        rumoca_core::BuiltinFunction::Sum => {
            #[cfg(feature = "tracing")]
            debug!("evaluating sum() builtin");
            eval_sum_product_integer_with_dims(args, known_ints, array_dims, true)
        }
        rumoca_core::BuiltinFunction::Product => {
            #[cfg(feature = "tracing")]
            debug!("evaluating product() builtin");
            eval_sum_product_integer_with_dims(args, known_ints, array_dims, false)
        }
        rumoca_core::BuiltinFunction::Size => {
            #[cfg(feature = "tracing")]
            debug!("evaluating size() builtin");
            eval_size_integer_with_context(args, ctx)
        }
        _ => {
            #[cfg(feature = "tracing")]
            warn!(function = ?function, "unhandled builtin function");
            None
        }
    };

    #[cfg(feature = "tracing")]
    match &result {
        Some(v) => debug!(function = ?function, result = v, "builtin evaluated"),
        None => debug!(function = ?function, "builtin evaluation deferred (value not yet known)"),
    }

    result
}

fn eval_integer_mod_builtin(x: i64, y: i64) -> i64 {
    let remainder = x % y;
    if remainder != 0 && (remainder > 0) != (y > 0) {
        remainder + y
    } else {
        remainder
    }
}

/// Evaluate max/min functions that return integer, with array dimension support.
fn eval_max_min_integer_with_context(
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext,
    is_max: bool,
) -> Option<i64> {
    if args.is_empty() {
        #[cfg(feature = "tracing")]
        warn!("max/min called with no arguments");
        return None;
    }

    #[cfg(feature = "tracing")]
    let func_name = if is_max { "max" } else { "min" };

    if args.len() >= 2 {
        // Binary form: max(a, b) or min(a, b)
        #[cfg(feature = "tracing")]
        debug!(func = func_name, "evaluating binary form");
        let x = try_eval_integer_with_context(&args[0], ctx)?;
        let y = try_eval_integer_with_context(&args[1], ctx)?;
        #[cfg(feature = "tracing")]
        debug!(x = x, y = y, "binary form operands");
        Some(if is_max { x.max(y) } else { x.min(y) })
    } else {
        // Array form: max([a; b; c]) - single argument that's an array
        match &args[0] {
            rumoca_core::Expression::Array { elements, .. } => {
                #[cfg(feature = "tracing")]
                debug!(
                    func = func_name,
                    element_count = elements.len(),
                    "evaluating array form"
                );
                let flat_elements = flatten_array_elements(elements);
                #[cfg(feature = "tracing")]
                debug!(
                    func = func_name,
                    flat_count = flat_elements.len(),
                    "flattened array elements"
                );
                let values: Option<Vec<i64>> = flat_elements
                    .iter()
                    .map(|e| try_eval_integer_with_context(e, ctx))
                    .collect();
                let values = values?;
                #[cfg(feature = "tracing")]
                debug!(values = ?values, "array elements evaluated");
                if values.is_empty() {
                    None
                } else if is_max {
                    values.into_iter().max()
                } else {
                    values.into_iter().min()
                }
            }
            _other => {
                #[cfg(feature = "tracing")]
                debug!(
                    func = func_name,
                    expr_kind = std::any::type_name_of_val(_other),
                    "single argument (non-array)"
                );
                try_eval_integer_with_context(&args[0], ctx)
            }
        }
    }
}

/// Flatten nested array elements into a single vector of scalar expressions.
fn flatten_array_elements(elements: &[rumoca_core::Expression]) -> Vec<&rumoca_core::Expression> {
    let mut result = Vec::new();
    for elem in elements {
        match elem {
            rumoca_core::Expression::Array {
                elements: inner, ..
            } => {
                result.extend(flatten_array_elements(inner));
            }
            _ => {
                result.push(elem);
            }
        }
    }
    result
}

/// Evaluate sum/product functions that return integer, with array dimension support.
fn eval_sum_product_integer_with_dims(
    args: &[rumoca_core::Expression],
    known_ints: &FxHashMap<String, i64>,
    array_dims: &FxHashMap<String, Vec<i64>>,
    is_sum: bool,
) -> Option<i64> {
    if args.is_empty() {
        #[cfg(feature = "tracing")]
        warn!("sum/product called with no arguments");
        return None;
    }

    #[cfg(feature = "tracing")]
    let func_name = if is_sum { "sum" } else { "product" };

    match &args[0] {
        rumoca_core::Expression::Array { elements, .. } => {
            #[cfg(feature = "tracing")]
            debug!(
                func = func_name,
                element_count = elements.len(),
                "evaluating array form"
            );
            let flat_elements = flatten_array_elements(elements);
            #[cfg(feature = "tracing")]
            debug!(
                func = func_name,
                flat_count = flat_elements.len(),
                "flattened array elements"
            );
            let values: Option<Vec<i64>> = flat_elements
                .iter()
                .map(|e| try_eval_flat_expr_integer_with_dims(e, known_ints, array_dims))
                .collect();
            let values = values?;
            #[cfg(feature = "tracing")]
            debug!(values = ?values, "array elements evaluated");
            if values.is_empty() {
                Some(if is_sum { 0 } else { 1 })
            } else if is_sum {
                Some(values.into_iter().sum())
            } else {
                Some(values.into_iter().product())
            }
        }
        _other => {
            #[cfg(feature = "tracing")]
            debug!(
                func = func_name,
                "single argument (non-array) - returning as-is"
            );
            try_eval_flat_expr_integer_with_dims(&args[0], known_ints, array_dims)
        }
    }
}

/// Infer array dimensions from an array literal binding.
pub fn try_infer_better_dims(var: &flat::Variable) -> Vec<i64> {
    if let Some(binding) = &var.binding
        && let Some(inferred) = infer_array_dimensions(binding)
        && inferred.len() > var.dims.len()
    {
        return inferred;
    }
    var.dims.clone()
}

/// MLS §10.1: When a variable is declared with unspecified dimensions (`:`) and
/// bound to an array literal, the dimensions can be inferred from the literal's structure.
pub fn infer_array_dimensions(expr: &rumoca_core::Expression) -> Option<Vec<i64>> {
    infer_array_dimensions_full_with_conds(
        expr,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
    )
}

/// Infer array dimensions with full context including conditional expression support.
pub fn infer_array_dimensions_full_with_conds(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    let known_reals = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = ParamEvalContext {
        known_ints,
        known_reals: &known_reals,
        known_bools,
        known_enums,
        array_dims,
        functions: &functions,
        var_context: None,
    };
    infer_array_dimensions_with_context(expr, &ctx)
}

/// Infer array dimensions with function output shape metadata available.
pub fn infer_array_dimensions_full_with_functions(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    infer_array_dimensions_with_context(expr, ctx)
}

fn infer_array_dimensions_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    match expr {
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            ..
        } => infer_array_literal_dimensions_with_context(elements, *is_matrix, ctx),
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            infer_builtin_call_dimensions_with_context(*function, args, ctx)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => infer_range_dimensions_with_context(start, step.as_deref(), end, ctx),
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            infer_array_comprehension_dimensions_with_context(expr, indices, filter.as_deref(), ctx)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => infer_if_dimensions_with_context(branches, else_branch, ctx),
        rumoca_core::Expression::FunctionCall { name, args, .. } => {
            infer_user_function_call_dimensions(name, args, ctx)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let dims = infer_array_dimensions_with_context(base, ctx)?;
            project_dims_by_subscripts(&dims, subscripts, ctx)
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let dims = lookup_array_dims_in_scope(name.as_str(), ctx.var_context, ctx.array_dims)?;
            project_dims_by_subscripts(&dims, subscripts, ctx)
        }
        _ => None,
    }
}

fn project_dims_by_subscripts(
    dims: &[i64],
    subscripts: &[rumoca_core::Subscript],
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    let mut projected = Vec::new();
    let mut dim_index = 0usize;
    for subscript in subscripts {
        let dim = *dims.get(dim_index)?;
        match subscript {
            rumoca_core::Subscript::Index { .. } => {}
            rumoca_core::Subscript::Expr { expr, .. } => {
                try_eval_integer_with_context(expr, ctx)?;
            }
            rumoca_core::Subscript::Colon { .. } => projected.push(dim),
        }
        dim_index += 1;
    }
    projected.extend_from_slice(&dims[dim_index..]);
    Some(projected)
}

fn infer_user_function_call_dimensions(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    let func = ctx.functions.get(name.as_str())?;
    let output = func.outputs.first()?;
    if output.shape_expr.is_empty() {
        return concrete_param_dims(output).or_else(|| broadcast_function_arg_dims(args, ctx));
    }

    let mut local_ints = ctx.known_ints.clone();
    let mut local_reals = ctx.known_reals.clone();
    let mut local_bools = ctx.known_bools.clone();
    bind_function_dimension_args(
        func,
        args,
        ctx,
        &mut local_ints,
        &mut local_reals,
        &mut local_bools,
    )?;

    let local_ctx = ParamEvalContext {
        known_ints: &local_ints,
        known_reals: &local_reals,
        known_bools: &local_bools,
        known_enums: ctx.known_enums,
        array_dims: ctx.array_dims,
        functions: ctx.functions,
        var_context: None,
    };

    output
        .shape_expr
        .iter()
        .enumerate()
        .map(|(index, subscript)| eval_param_shape_subscript(output, index, subscript, &local_ctx))
        .collect()
}

fn concrete_param_dims(param: &rumoca_core::FunctionParam) -> Option<Vec<i64>> {
    if param.dims.is_empty() || param.dims.iter().any(|dim| *dim < 0) {
        return None;
    }
    Some(param.dims.clone())
}

fn broadcast_function_arg_dims(
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    args.iter()
        .map(function_arg_value)
        .filter_map(|arg| infer_function_arg_dims(arg, ctx))
        .max_by_key(Vec::len)
        .filter(|dims| !dims.is_empty())
}

fn function_arg_value(arg: &rumoca_core::Expression) -> &rumoca_core::Expression {
    if let Some((_, value)) = named_call_arg(arg) {
        value
    } else {
        arg
    }
}

fn infer_function_arg_dims(
    arg: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    if let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = arg
        && subscripts.is_empty()
    {
        return lookup_array_dims_in_scope(name.as_str(), ctx.var_context, ctx.array_dims);
    }

    infer_array_dimensions_full_with_functions(arg, ctx)
}

fn infer_array_literal_dimensions_with_context(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    if elements.is_empty() {
        return Some(vec![0]);
    }

    if is_matrix {
        return infer_matrix_constructor_dimensions_with_context(elements, ctx);
    }

    let mut dims = vec![elements.len() as i64];
    if let Some(first) = elements.first()
        && let Some(inner_dims) = infer_array_dimensions_with_context(first, ctx)
    {
        dims.extend(inner_dims);
    }
    Some(dims)
}

fn infer_matrix_constructor_dimensions_with_context(
    elements: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    let has_nested_rows = matches!(
        elements.first(),
        Some(rumoca_core::Expression::Array { .. })
    );
    if !has_nested_rows {
        return infer_matrix_row_dimensions_with_context(elements, ctx)
            .map(|(rows, cols)| vec![rows, cols]);
    }

    let mut rows = 0i64;
    let mut expected_cols = None;
    for row in elements {
        let rumoca_core::Expression::Array {
            elements: row_elements,
            ..
        } = row
        else {
            return None;
        };
        let (row_count, col_count) = infer_matrix_row_dimensions_with_context(row_elements, ctx)?;
        match expected_cols {
            Some(expected) if expected != col_count => return None,
            None => expected_cols = Some(col_count),
            _ => {}
        }
        rows += row_count;
    }

    Some(vec![rows, expected_cols?])
}

fn infer_matrix_row_dimensions_with_context(
    elements: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Option<(i64, i64)> {
    let single_entry = elements.len() == 1;
    let mut expected_rows = None;
    let mut cols = 0i64;
    for element in elements {
        let dims = infer_array_dimensions_with_context(element, ctx)
            .or_else(|| scalar_matrix_entry_dims(element))?;
        let (entry_rows, entry_cols) = matrix_entry_dimensions(&dims, single_entry)?;
        match expected_rows {
            Some(expected) if expected != entry_rows => return None,
            None => expected_rows = Some(entry_rows),
            _ => {}
        }
        cols += entry_cols;
    }
    Some((expected_rows?, cols))
}

fn scalar_matrix_entry_dims(expr: &rumoca_core::Expression) -> Option<Vec<i64>> {
    matches!(expr, rumoca_core::Expression::Literal { .. }).then(Vec::new)
}

fn infer_array_comprehension_dimensions_with_context(
    expr: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    if filter.is_some() {
        return None;
    }

    let mut dims = Vec::with_capacity(indices.len().saturating_add(1));
    for index in indices {
        let range_dims = infer_array_dimensions_with_context(&index.range, ctx)?;
        if range_dims.is_empty() {
            return None;
        }
        let iter_size = range_dims
            .iter()
            .copied()
            .fold(1i64, |acc, dim| acc.saturating_mul(dim.max(0)));
        dims.push(iter_size);
    }

    if let Some(mut inner_dims) = infer_array_dimensions_with_context(expr, ctx) {
        dims.append(&mut inner_dims);
    }

    Some(dims)
}

fn infer_builtin_call_dimensions_with_context(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    match function {
        rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Ones => {
            eval_dimension_args_with_context(args, ctx)
        }
        rumoca_core::BuiltinFunction::Fill => {
            if args.len() < 2 {
                return None;
            }
            eval_dimension_args_with_context(&args[1..], ctx)
        }
        rumoca_core::BuiltinFunction::Linspace => {
            if args.len() != 3 {
                return None;
            }
            let n = try_eval_integer_with_context(&args[2], ctx)?;
            (n >= 2).then_some(vec![n])
        }
        rumoca_core::BuiltinFunction::Identity => {
            if args.len() != 1 {
                return None;
            }
            let n = try_eval_integer_with_context(&args[0], ctx)?;
            Some(vec![n, n])
        }
        rumoca_core::BuiltinFunction::Vector => {
            if args.len() != 1 {
                return None;
            }
            let dims = infer_array_dimensions_with_context(&args[0], ctx)?;
            Some(vec![dims.iter().copied().product()])
        }
        rumoca_core::BuiltinFunction::Matrix => {
            if args.len() != 1 {
                return None;
            }
            let dims = infer_array_dimensions_with_context(&args[0], ctx)?;
            match dims.as_slice() {
                [] => Some(vec![1, 1]),
                [len] => Some(vec![*len, 1]),
                [_, _] => Some(dims),
                _ => None,
            }
        }
        _ => None,
    }
}

fn eval_dimension_args_with_context(
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    let mut dims = Vec::with_capacity(args.len());
    for arg in args {
        dims.push(try_eval_integer_with_context(arg, ctx)?);
    }
    (!dims.is_empty()).then_some(dims)
}

fn infer_if_dimensions_with_context(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    for (cond, then_expr) in branches {
        match try_eval_flat_expr_boolean_with_context(cond, ctx) {
            Some(true) => return infer_array_dimensions_with_context(then_expr, ctx),
            Some(false) => continue,
            None => return None,
        }
    }
    infer_array_dimensions_with_context(else_branch, ctx)
}

fn bind_function_dimension_args(
    func: &rumoca_core::Function,
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
    local_ints: &mut FxHashMap<String, i64>,
    local_reals: &mut FxHashMap<String, f64>,
    local_bools: &mut FxHashMap<String, bool>,
) -> Option<()> {
    let mut positional = 0usize;
    for arg in args {
        let (param_name, value_expr) = if let Some((name, value)) = named_call_arg(arg) {
            (name, value)
        } else {
            let param = func.inputs.get(positional)?;
            positional += 1;
            (param.name.as_str(), arg)
        };
        bind_dimension_arg_value(
            param_name,
            value_expr,
            ctx,
            local_ints,
            local_reals,
            local_bools,
        )?;
    }

    for param in &func.inputs {
        if local_ints.contains_key(&param.name)
            || local_reals.contains_key(&param.name)
            || local_bools.contains_key(&param.name)
        {
            continue;
        }
        let default = param.default.as_ref()?;
        bind_dimension_arg_value(
            &param.name,
            default,
            ctx,
            local_ints,
            local_reals,
            local_bools,
        )?;
    }
    Some(())
}

fn bind_dimension_arg_value(
    name: &str,
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
    local_ints: &mut FxHashMap<String, i64>,
    local_reals: &mut FxHashMap<String, f64>,
    local_bools: &mut FxHashMap<String, bool>,
) -> Option<()> {
    if let Some(value) = try_eval_integer_with_context(expr, ctx) {
        local_ints.insert(name.to_string(), value);
        return Some(());
    }
    if let Some(value) = try_eval_real_with_context(expr, ctx) {
        local_reals.insert(name.to_string(), value);
        return Some(());
    }
    if let Some(value) = try_eval_flat_expr_boolean_with_context(expr, ctx) {
        local_bools.insert(name.to_string(), value);
        return Some(());
    }
    None
}

fn eval_param_shape_subscript(
    param: &rumoca_core::FunctionParam,
    index: usize,
    subscript: &rumoca_core::Subscript,
    ctx: &ParamEvalContext<'_>,
) -> Option<i64> {
    match subscript {
        rumoca_core::Subscript::Index { value, .. } => Some(*value),
        rumoca_core::Subscript::Expr { expr, .. } => try_eval_integer_with_context(expr, ctx),
        rumoca_core::Subscript::Colon { .. } => {
            param.dims.get(index).copied().filter(|dim| *dim >= 0)
        }
    }
}

fn matrix_entry_dimensions(dims: &[i64], single_entry: bool) -> Option<(i64, i64)> {
    match dims {
        [] => Some((1, 1)),
        [len] if single_entry => Some((*len, 1)),
        [len] => Some((*len, 1)),
        [rows, cols] => Some((*rows, *cols)),
        _ => None,
    }
}

fn infer_range_dimensions_with_context(
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    let start_val = try_eval_integer_with_context(start, ctx)?;
    let end_val = try_eval_integer_with_context(end, ctx)?;
    let step_val = step
        .map(|s| try_eval_integer_with_context(s, ctx))
        .unwrap_or(Some(1))?;

    if step_val == 0 {
        return None;
    }

    let len = if step_val > 0 {
        if end_val >= start_val {
            (end_val - start_val) / step_val + 1
        } else {
            0
        }
    } else if start_val >= end_val {
        (start_val - end_val) / (-step_val) + 1
    } else {
        0
    };

    Some(vec![len])
}

/// Evaluate size(array, dim) builtin function with scope resolution.
///
/// Uses the variable context to resolve unqualified array names to their
/// fully qualified form when looking up dimensions (MLS §5.1).
fn eval_size_integer_with_context(
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext,
) -> Option<i64> {
    if args.is_empty() || args.len() > 2 {
        #[cfg(feature = "tracing")]
        warn!(arg_count = args.len(), "size() requires 1 or 2 arguments");
        return None;
    }

    let array_name = match &args[0] {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => name.to_string(),
        _ => {
            #[cfg(feature = "tracing")]
            warn!(
                arg0_kind = std::any::type_name_of_val(&args[0]),
                "size() first arg must be a simple VarRef"
            );
            return None;
        }
    };

    #[cfg(feature = "tracing")]
    debug!(array = %array_name, var_context = ?ctx.var_context, "looking up array dimensions with scope resolution");

    // Try scope-aware lookup for array dimensions (MLS §5.1)
    let dims = lookup_array_dims_in_scope(&array_name, ctx.var_context, ctx.array_dims)?;

    if args.len() == 1 {
        if dims.len() == 1 {
            #[cfg(feature = "tracing")]
            debug!(array = %array_name, size = dims[0], "size(A) for 1D array");
            Some(dims[0])
        } else {
            #[cfg(feature = "tracing")]
            warn!(array = %array_name, ndims = dims.len(), "size(A) requires explicit dimension for multi-dimensional arrays");
            None
        }
    } else {
        let dim = try_eval_integer_with_context(&args[1], ctx)?;
        if dim >= 1 && (dim as usize) <= dims.len() {
            let result = dims[(dim as usize) - 1];
            #[cfg(feature = "tracing")]
            debug!(array = %array_name, dim = dim, result = result, "size(A, dim) evaluated");
            Some(result)
        } else {
            #[cfg(feature = "tracing")]
            warn!(array = %array_name, dim = dim, ndims = dims.len(), "dimension out of range");
            None
        }
    }
}

/// Walk up the scope chain looking for array dimensions.
fn lookup_dims_in_ancestors(
    array_name: &str,
    start_scope: &str,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    let scope = ComponentPath::from_flat_path(start_scope);
    let array_path = ComponentPath::from_flat_path(array_name);
    for candidate in scoped_component_path_candidates(&array_path, &scope)
        .into_iter()
        .skip(1)
    {
        if let Some(dims) = array_dims.get(&candidate) {
            #[cfg(feature = "tracing")]
            debug!(array = %array_name, qualified = %candidate, dims = ?dims, "found in ancestor");
            return Some(dims.clone());
        }
    }
    None
}

/// Look up array dimensions with scope resolution.
///
/// Tries to find array dimensions by:
/// 1. Direct lookup (for already qualified names)
/// 2. Qualified with var_context scope (e.g., `lines` -> `world.x_label.lines`)
/// 3. Parent scope resolution (walking up the scope chain)
fn lookup_array_dims_in_scope(
    array_name: &str,
    var_context: Option<&str>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    // 1. Try direct lookup first
    if let Some(dims) = array_dims.get(array_name) {
        #[cfg(feature = "tracing")]
        debug!(array = %array_name, dims = ?dims, "found array dimensions (direct)");
        return Some(dims.clone());
    }

    // 2. If we have var_context, try scoped lookups
    let context = var_context?;
    let parent_scope = ComponentPath::from_flat_path(context).parent()?;
    let array_path = ComponentPath::from_flat_path(array_name);

    // Try parent scope first
    let qualified = parent_scope.join(&array_path).to_flat_string();
    if let Some(dims) = array_dims.get(&qualified) {
        #[cfg(feature = "tracing")]
        debug!(array = %array_name, qualified = %qualified, dims = ?dims, "found in parent scope");
        return Some(dims.clone());
    }

    // 3. Walk up ancestor scopes
    lookup_dims_in_ancestors(array_name, &parent_scope.to_flat_string(), array_dims)
}

/// Evaluate user function calls that return integer.
fn eval_user_func_integer(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext,
) -> Option<i64> {
    let name_str = name.as_str();

    // Handle integer() function (user function that converts to integer)
    if name_str == "integer" {
        let arg = args.first()?;
        return try_eval_real_with_context(arg, ctx).map(|v| v as i64);
    }

    // Try to find and evaluate the user-defined function using rumoca_eval_const
    let func = ctx.functions.get(name_str)?;
    let eval_ctx = build_user_func_eval_ctx(ctx);
    let arg_values = eval_func_args(args, ctx)?;

    let result = crate::constant::function_eval::eval_function_with_call_args(
        func,
        arg_values,
        &eval_ctx,
        &crate::constant::function_eval::EvalLimits::default(),
        0,
        rumoca_core::Span::DUMMY,
    );

    match result {
        Ok(value) => value.as_integer(),
        Err(_) => None,
    }
}

/// Evaluate user function calls that return a real value.
pub fn eval_user_func_real(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext,
) -> Option<f64> {
    let name_str = name.as_str();
    let func = ctx.functions.get(name_str)?;
    let eval_ctx = build_user_func_eval_ctx(ctx);
    let arg_values = eval_func_args(args, ctx)?;

    let result = crate::constant::function_eval::eval_function_with_call_args(
        func,
        arg_values,
        &eval_ctx,
        &crate::constant::function_eval::EvalLimits::default(),
        0,
        rumoca_core::Span::DUMMY,
    );

    match result {
        Ok(value) => value
            .as_real()
            .or_else(|| value.as_integer().map(|i| i as f64)),
        Err(_) => None,
    }
}

/// Build an EvalContext for user function evaluation.
fn build_user_func_eval_ctx(ctx: &ParamEvalContext) -> EvalContext {
    let parameter_capacity = ctx.known_ints.len() + ctx.known_reals.len() + ctx.known_bools.len();
    let mut eval_ctx = EvalContext::with_capacity(parameter_capacity, 0, ctx.functions.len() * 2);
    for (param_name, value) in ctx.known_ints {
        eval_ctx.add_parameter(param_name.clone(), Value::Integer(*value));
    }
    for (param_name, value) in ctx.known_reals {
        eval_ctx.add_parameter(param_name.clone(), Value::Real(*value));
    }
    for (param_name, value) in ctx.known_bools {
        eval_ctx.add_parameter(param_name.clone(), Value::Bool(*value));
    }
    for func_def in ctx.functions.values() {
        eval_ctx.add_function(func_def.clone());
    }
    eval_ctx
}

/// Evaluate function arguments to positional/named values.
fn eval_func_args(
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext,
) -> Option<Vec<crate::constant::function_eval::FunctionCallArg>> {
    let mut arg_values = Vec::new();
    for arg in args {
        if let Some((name, value_expr)) = named_call_arg(arg) {
            let value = eval_func_arg_value(value_expr, ctx)?;
            arg_values.push(crate::constant::function_eval::FunctionCallArg::named(
                name.to_string(),
                value,
            ));
            continue;
        }
        let value = eval_func_arg_value(arg, ctx)?;
        arg_values.push(crate::constant::function_eval::FunctionCallArg::positional(
            value,
        ));
    }
    Some(arg_values)
}

fn named_call_arg(expr: &rumoca_core::Expression) -> Option<(&str, &rumoca_core::Expression)> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
        ..
    } = expr
    else {
        return None;
    };
    let arg_name = name.as_str().strip_prefix(NAMED_CALL_ARG_PREFIX)?;
    let value = args.first()?;
    (args.len() == 1).then_some((arg_name, value))
}

fn eval_func_arg_value(expr: &rumoca_core::Expression, ctx: &ParamEvalContext) -> Option<Value> {
    if let Some(int_val) = try_eval_integer_with_context(expr, ctx) {
        return Some(Value::Integer(int_val));
    }
    try_eval_real_with_context(expr, ctx).map(Value::Real)
}

/// Try to evaluate a flat expression to a real value.
pub fn try_eval_flat_expr_real(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_reals: &FxHashMap<String, f64>,
) -> Option<f64> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(n),
            ..
        } => Some(*n as f64),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(r),
            ..
        } => Some(*r),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            let name_str = name.to_string();
            if let Some(v) = known_reals.get(&name_str) {
                return Some(*v);
            }
            known_ints.get(&name_str).map(|&v| v as f64)
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            let val = try_eval_flat_expr_real(rhs, known_ints, known_reals)?;
            match op {
                rumoca_core::OpUnary::Minus => Some(-val),
                rumoca_core::OpUnary::Plus => Some(val),
                _ => None,
            }
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let l = try_eval_flat_expr_real(lhs, known_ints, known_reals)?;
            let r = try_eval_flat_expr_real(rhs, known_ints, known_reals)?;
            match op {
                rumoca_core::OpBinary::Add => Some(l + r),
                rumoca_core::OpBinary::Sub => Some(l - r),
                rumoca_core::OpBinary::Mul => Some(l * r),
                rumoca_core::OpBinary::Div => {
                    if r != 0.0 {
                        Some(l / r)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// Try to extract an enumeration value from a flat expression.
pub fn try_extract_enum_value(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let name_str = name.to_string();
            if subscripts.is_empty() && looks_like_enum_literal_path(&name_str) {
                Some(name_str)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Try to evaluate a flat expression to an enum literal with context.
///
/// This supports:
/// - direct enum literals (`Types.Dynamics.SteadyState`)
/// - enum parameter references
/// - conditional enum expressions where conditions are compile-time evaluable
///   (MLS §4.9.5, §8.3.4).
pub fn try_eval_flat_expr_enum(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
) -> Option<String> {
    eval_enum_inner(expr, known_ints, known_bools, known_enums)
}

/// Check whether a dotted path is likely an enum literal reference.
///
/// Enum literals can be globally qualified (`Modelica.Fluid.Types.Dynamics.X`)
/// or scope-qualified (`pipe.Types.ModelStructure.a_v_b`). To avoid misclassifying
/// plain dotted parameter refs (e.g. `pipe1.system.energyDynamics`), require at
/// least one non-final path segment to be type-like (uppercase-initial).
pub fn looks_like_enum_literal_path(path: &str) -> bool {
    let parts = split_path_with_indices(path);
    if parts.len() < 2 {
        return false;
    }

    parts[..parts.len() - 1]
        .iter()
        .any(|segment| segment.chars().next().is_some_and(char::is_uppercase))
}

/// Resolve an expression to its enum value string (MLS §4.9.5).
fn resolve_enum_value(
    expr: &rumoca_core::Expression,
    known_enums: &FxHashMap<String, String>,
) -> Option<String> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }

    let name_str = name.to_string();
    if let Some(enum_val) = known_enums.get(&name_str) {
        return Some(enum_val.clone());
    }

    try_extract_enum_value(expr).map(|literal| canonicalize_enum_literal(&literal, known_enums))
}

/// Inner enum evaluation.
fn eval_enum_inner(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
) -> Option<String> {
    match expr {
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => eval_enum_if(branches, else_branch, known_ints, known_bools, known_enums),
        _ => resolve_enum_value(expr, known_enums),
    }
}

/// Evaluate enum if-expressions with compile-time conditions.
fn eval_enum_if(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
) -> Option<String> {
    let mut unknown_branch_values: Vec<String> = Vec::new();
    for (cond, then_expr) in branches {
        match try_eval_flat_expr_boolean(cond, known_ints, known_bools, known_enums) {
            Some(true) => return eval_enum_inner(then_expr, known_ints, known_bools, known_enums),
            Some(false) => continue,
            None => unknown_branch_values.push(eval_enum_inner(
                then_expr,
                known_ints,
                known_bools,
                known_enums,
            )?),
        }
    }

    let else_value = eval_enum_inner(else_branch, known_ints, known_bools, known_enums)?;
    if unknown_branch_values.is_empty() {
        return Some(else_value);
    }

    let all_same = unknown_branch_values
        .iter()
        .all(|value| enum_values_equivalent(value, &else_value, known_enums));
    if all_same { Some(else_value) } else { None }
}

fn enum_values_equivalent(lhs: &str, rhs: &str, known_enums: &FxHashMap<String, String>) -> bool {
    let lhs_norm = canonicalize_enum_literal(lhs, known_enums);
    let rhs_norm = canonicalize_enum_literal(rhs, known_enums);
    rumoca_core::enum_values_equal(&lhs_norm, &rhs_norm)
}

/// Canonicalize a potentially partially-qualified enum literal using known enum values.
///
/// Example:
/// - known value: `Modelica.Fluid.Types.ModelStructure.a_vb`
/// - literal: `pipe.Types.ModelStructure.a_vb`
/// - canonicalized: `Modelica.Fluid.Types.ModelStructure.a_vb`
///
/// This preserves MLS §4.9.5 enum identity across equivalent qualification paths.
pub fn canonicalize_enum_literal(literal: &str, known_enums: &FxHashMap<String, String>) -> String {
    let parts = split_path_with_indices(literal);
    if parts.len() < 2 {
        return literal.to_string();
    }

    // Try progressively shorter suffixes and prefer the first unambiguous match.
    // This handles local package aliases like `pipe.Types.X` vs global `Modelica...Types.X`.
    // Only consider suffixes with at least two segments (`Type.Literal`) to
    // avoid over-broad matches on single identifiers.
    for start in 0..parts.len().saturating_sub(1) {
        let suffix = parts[start..].join(".");
        let mut best_match: Option<&str> = None;
        let mut best_segments = 0usize;
        let mut ambiguous_best = false;
        for value in known_enums.values() {
            if !value.ends_with(&suffix) {
                continue;
            }
            let candidate = value.as_str();
            let candidate_segments = split_path_with_indices(candidate).len();
            if candidate_segments > best_segments {
                best_match = Some(candidate);
                best_segments = candidate_segments;
                ambiguous_best = false;
            } else if candidate_segments == best_segments
                && best_match.is_some_and(|existing| existing != candidate)
            {
                ambiguous_best = true;
            }
        }
        if !ambiguous_best && let Some(value) = best_match {
            return value.to_string();
        }
    }

    literal.to_string()
}

/// Context for boolean expression evaluation.
struct BoolEvalContext<'a> {
    known_ints: &'a FxHashMap<String, i64>,
    known_bools: &'a FxHashMap<String, bool>,
    known_enums: &'a FxHashMap<String, String>,
}

/// Try to evaluate a flat expression to a boolean value with context.
pub fn try_eval_flat_expr_boolean(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
) -> Option<bool> {
    let ctx = BoolEvalContext {
        known_ints,
        known_bools,
        known_enums,
    };
    eval_bool_inner(expr, &ctx)
}

/// Inner boolean evaluation.
fn eval_bool_inner(expr: &rumoca_core::Expression, ctx: &BoolEvalContext) -> Option<bool> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(b),
            ..
        } => Some(*b),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => ctx.known_bools.get(&name.to_string()).copied(),
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs,
            ..
        } => eval_bool_inner(rhs, ctx).map(|v| !v),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => eval_bool_binary(op, lhs, rhs, ctx),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => eval_bool_if(branches, else_branch, ctx),
        _ => None,
    }
}

/// Evaluate binary boolean operations.
fn eval_bool_binary(
    op: &rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    ctx: &BoolEvalContext,
) -> Option<bool> {
    match op {
        rumoca_core::OpBinary::And => {
            Some(eval_bool_inner(lhs, ctx)? && eval_bool_inner(rhs, ctx)?)
        }
        rumoca_core::OpBinary::Or => Some(eval_bool_inner(lhs, ctx)? || eval_bool_inner(rhs, ctx)?),
        rumoca_core::OpBinary::Eq => eval_equality(lhs, rhs, ctx, true),
        rumoca_core::OpBinary::Neq => eval_equality(lhs, rhs, ctx, false),
        rumoca_core::OpBinary::Lt => eval_int_compare(lhs, rhs, ctx.known_ints, |l, r| l < r),
        rumoca_core::OpBinary::Le => eval_int_compare(lhs, rhs, ctx.known_ints, |l, r| l <= r),
        rumoca_core::OpBinary::Gt => eval_int_compare(lhs, rhs, ctx.known_ints, |l, r| l > r),
        rumoca_core::OpBinary::Ge => eval_int_compare(lhs, rhs, ctx.known_ints, |l, r| l >= r),
        _ => None,
    }
}

/// Evaluate equality/inequality comparisons across types.
fn eval_equality(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    ctx: &BoolEvalContext,
    eq: bool,
) -> Option<bool> {
    // Try integer comparison
    if let (Some(l), Some(r)) = (
        try_eval_flat_expr_integer_with_dims(lhs, ctx.known_ints, &FxHashMap::default()),
        try_eval_flat_expr_integer_with_dims(rhs, ctx.known_ints, &FxHashMap::default()),
    ) {
        return Some(if eq { l == r } else { l != r });
    }
    // Try boolean comparison
    if let (Some(l), Some(r)) = (eval_bool_inner(lhs, ctx), eval_bool_inner(rhs, ctx)) {
        return Some(if eq { l == r } else { l != r });
    }
    // Try enum comparison
    if let (Some(l), Some(r)) = (
        resolve_enum_value(lhs, ctx.known_enums),
        resolve_enum_value(rhs, ctx.known_enums),
    ) {
        let l_norm = canonicalize_enum_literal(&l, ctx.known_enums);
        let r_norm = canonicalize_enum_literal(&r, ctx.known_enums);
        let equal = rumoca_core::enum_values_equal(&l_norm, &r_norm);
        return Some(if eq { equal } else { !equal });
    }
    None
}

/// Evaluate integer comparisons.
fn eval_int_compare(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    cmp: fn(i64, i64) -> bool,
) -> Option<bool> {
    let l = try_eval_flat_expr_integer_with_dims(lhs, known_ints, &FxHashMap::default())?;
    let r = try_eval_flat_expr_integer_with_dims(rhs, known_ints, &FxHashMap::default())?;
    Some(cmp(l, r))
}

/// Evaluate if-expression branches.
fn eval_bool_if(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    ctx: &BoolEvalContext,
) -> Option<bool> {
    for (cond, then_expr) in branches {
        match eval_bool_inner(cond, ctx) {
            Some(true) => return eval_bool_inner(then_expr, ctx),
            Some(false) => continue,
            None => return None,
        }
    }
    eval_bool_inner(else_branch, ctx)
}
#[cfg(test)]
mod tests;
