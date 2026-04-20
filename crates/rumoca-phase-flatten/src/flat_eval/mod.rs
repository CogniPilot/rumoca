//! Flat expression evaluation for the flatten phase.
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

use rumoca_core::{IntegerBinaryOperator, eval_integer_binary, eval_integer_div_builtin};
use rumoca_eval_flat::constant::{EvalContext, Value};
use rumoca_ir_flat as flat;

use crate::path_utils::{parent_scope, split_path_with_indices};

// Conditional tracing support (SPEC_0024)
#[cfg(feature = "tracing")]
use tracing::{debug, warn};

/// Build an EvalContext from known parameter values and functions.
pub(crate) fn build_eval_context(
    known_ints: &FxHashMap<String, i64>,
    known_reals: &FxHashMap<String, f64>,
    known_bools: &FxHashMap<String, bool>,
    array_dims: &FxHashMap<String, Vec<i64>>,
    functions: &FxHashMap<String, flat::Function>,
) -> EvalContext {
    let mut eval_ctx = EvalContext::new();
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
pub(crate) struct ParamEvalContext<'a> {
    pub known_ints: &'a FxHashMap<String, i64>,
    pub known_reals: &'a FxHashMap<String, f64>,
    pub known_bools: &'a FxHashMap<String, bool>,
    pub known_enums: &'a FxHashMap<String, String>,
    pub array_dims: &'a FxHashMap<String, Vec<i64>>,
    /// Functions available for evaluation.
    pub functions: &'a FxHashMap<String, flat::Function>,
    /// The fully qualified name of the variable whose binding we're evaluating.
    /// Used to resolve unqualified modification bindings to parent scope (MLS §7.2).
    pub var_context: Option<&'a str>,
}

/// Try to evaluate a flat expression to an integer value with context and array dimensions.
///
/// Same as try_eval_flat_expr_integer but also handles size() calls using array dimensions.
pub(crate) fn try_eval_flat_expr_integer_with_dims(
    expr: &flat::Expression,
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
pub(crate) fn try_eval_integer_with_context(
    expr: &flat::Expression,
    ctx: &ParamEvalContext,
) -> Option<i64> {
    let result = match expr {
        flat::Expression::Literal(flat::Literal::Integer(n)) => Some(*n),
        flat::Expression::Literal(flat::Literal::Real(r)) => {
            if r.fract() == 0.0 {
                Some(*r as i64)
            } else {
                None
            }
        }
        flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            resolve_varref_integer(&name.to_string(), ctx)
        }
        flat::Expression::FieldAccess { base, field } => {
            let base_name = flatten_field_access_path(base)?;
            let field_name = format!("{base_name}.{field}");
            resolve_varref_integer(&field_name, ctx)
        }
        flat::Expression::Unary { op, rhs } => {
            let val = try_eval_integer_with_context(rhs, ctx)?;
            match op {
                flat::OpUnary::Minus(_) => Some(-val),
                flat::OpUnary::Plus(_) => Some(val),
                _ => None,
            }
        }
        flat::Expression::Binary { op, lhs, rhs } => {
            let l = try_eval_integer_with_context(lhs, ctx)?;
            let r = try_eval_integer_with_context(rhs, ctx)?;
            eval_ast_integer_binary(op, l, r)
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => eval_integer_if_expression(branches, else_branch, ctx),
        flat::Expression::BuiltinCall { function, args } => {
            #[cfg(feature = "tracing")]
            debug!(function = ?function, arg_count = args.len(), "evaluating builtin call");
            eval_builtin_integer_with_context(function, args, ctx)
        }
        flat::Expression::FunctionCall { name, args, .. } => {
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

fn eval_ast_integer_binary(op: &flat::OpBinary, lhs: i64, rhs: i64) -> Option<i64> {
    let operator = match op {
        flat::OpBinary::Add(_) => IntegerBinaryOperator::Add,
        flat::OpBinary::Sub(_) => IntegerBinaryOperator::Sub,
        flat::OpBinary::Mul(_) => IntegerBinaryOperator::Mul,
        flat::OpBinary::Div(_) => IntegerBinaryOperator::Div,
        _ => return None,
    };
    eval_integer_binary(operator, lhs, rhs)
}

fn flatten_field_access_path(expr: &flat::Expression) -> Option<String> {
    match expr {
        flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            Some(name.to_string())
        }
        flat::Expression::FieldAccess { base, field } => {
            let base_path = flatten_field_access_path(base)?;
            Some(format!("{base_path}.{field}"))
        }
        _ => None,
    }
}

fn size_argument_path(expr: &flat::Expression) -> Option<String> {
    match expr {
        flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            Some(name.to_string())
        }
        flat::Expression::FieldAccess { .. } => flatten_field_access_path(expr),
        _ => None,
    }
}

fn eval_integer_if_expression(
    branches: &[(flat::Expression, flat::Expression)],
    else_branch: &flat::Expression,
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
fn try_eval_flat_expr_boolean_with_context(
    expr: &flat::Expression,
    ctx: &ParamEvalContext,
) -> Option<bool> {
    match expr {
        flat::Expression::Literal(flat::Literal::Boolean(b)) => Some(*b),
        flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            resolve_varref_boolean(&name.to_string(), ctx)
        }
        flat::Expression::Unary {
            op: flat::OpUnary::Not(_),
            rhs,
        } => try_eval_flat_expr_boolean_with_context(rhs, ctx).map(|v| !v),
        flat::Expression::Binary { op, lhs, rhs } => {
            try_eval_flat_expr_boolean_binary_with_context(op, lhs, rhs, ctx)
        }
        flat::Expression::If {
            branches,
            else_branch,
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
    op: &flat::OpBinary,
    lhs: &flat::Expression,
    rhs: &flat::Expression,
    ctx: &ParamEvalContext,
) -> Option<bool> {
    match op {
        flat::OpBinary::And(_) => Some(
            try_eval_flat_expr_boolean_with_context(lhs, ctx)?
                && try_eval_flat_expr_boolean_with_context(rhs, ctx)?,
        ),
        flat::OpBinary::Or(_) => Some(
            try_eval_flat_expr_boolean_with_context(lhs, ctx)?
                || try_eval_flat_expr_boolean_with_context(rhs, ctx)?,
        ),
        flat::OpBinary::Eq(_) | flat::OpBinary::Neq(_) => {
            let is_eq = matches!(op, flat::OpBinary::Eq(_));

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
        flat::OpBinary::Lt(_) => Some(
            try_eval_integer_with_context(lhs, ctx)? < try_eval_integer_with_context(rhs, ctx)?,
        ),
        flat::OpBinary::Le(_) => Some(
            try_eval_integer_with_context(lhs, ctx)? <= try_eval_integer_with_context(rhs, ctx)?,
        ),
        flat::OpBinary::Gt(_) => Some(
            try_eval_integer_with_context(lhs, ctx)? > try_eval_integer_with_context(rhs, ctx)?,
        ),
        flat::OpBinary::Ge(_) => Some(
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
    let mut scope = var_context.and_then(get_parent_scope)?;
    loop {
        let qualified = format!("{scope}.{name}");
        if let Some(val) = values.get(&qualified).copied() {
            return Some(val);
        }
        match get_parent_scope(scope) {
            Some(parent) => scope = parent,
            None => break,
        }
    }
    values.get(name).copied()
}

fn lookup_scoped_cloned<T: Clone>(
    values: &FxHashMap<String, T>,
    name: &str,
    var_context: Option<&str>,
) -> Option<T> {
    let mut scope = var_context.and_then(get_parent_scope)?;
    loop {
        let qualified = format!("{scope}.{name}");
        if let Some(val) = values.get(&qualified) {
            return Some(val.clone());
        }
        match get_parent_scope(scope) {
            Some(parent) => scope = parent,
            None => break,
        }
    }
    values.get(name).cloned()
}

fn resolve_varref_boolean(name_str: &str, ctx: &ParamEvalContext) -> Option<bool> {
    if let Some(val) = ctx.known_bools.get(name_str).copied() {
        return Some(val);
    }

    if let Some(val) = lookup_scoped_copy(ctx.known_bools, name_str, ctx.var_context) {
        return Some(val);
    }

    let segments = split_path_with_indices(name_str);
    for suffix_start in 1..segments.len() {
        let candidate = segments[suffix_start..].join(".");
        if let Some(val) = ctx.known_bools.get(&candidate).copied() {
            return Some(val);
        }
    }

    None
}

fn resolve_enum_value_with_context(
    expr: &flat::Expression,
    ctx: &ParamEvalContext,
) -> Option<String> {
    let flat::Expression::VarRef { name, subscripts } = expr else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }

    let name_str = name.to_string();
    if let Some(enum_val) = ctx.known_enums.get(&name_str) {
        return Some(enum_val.clone());
    }

    if let Some(enum_val) = lookup_scoped_cloned(ctx.known_enums, &name_str, ctx.var_context) {
        return Some(enum_val);
    }

    let segments = split_path_with_indices(&name_str);
    for suffix_start in 1..segments.len() {
        let candidate = segments[suffix_start..].join(".");
        if let Some(enum_val) = ctx.known_enums.get(&candidate) {
            return Some(enum_val.clone());
        }
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
    // Start from the parent of the current variable
    let mut scope = get_parent_scope(var_context)?;

    loop {
        // Try the name in this scope
        let qualified = format!("{}.{}", scope, name);
        if let Some(val) = known_ints.get(&qualified).copied() {
            return Some(val);
        }

        // Move to parent scope
        match get_parent_scope(scope) {
            Some(parent) => scope = parent,
            None => break,
        }
    }

    // Try unqualified (root scope)
    known_ints.get(name).copied()
}

/// Resolve a qualified name by progressively stripping leading segments.
///
/// Modification nesting can produce over-qualified bindings. For example,
/// `MultiStarResistance(m=data.m)` contains `MultiStar(m=m)` internally.
/// The inner binding for `multiStar.multiStar.m` becomes `multiStar.data.m`
/// when the actual parameter is `data.m` at the top level.
/// This function tries `data.m`, then `m` by stripping leading segments.
fn resolve_by_suffix_stripping(name: &str, known_ints: &FxHashMap<String, i64>) -> Option<i64> {
    let segments = split_path_with_indices(name);
    for suffix_start in 1..segments.len() {
        let candidate = segments[suffix_start..].join(".");
        if let Some(val) = known_ints.get(&candidate).copied() {
            return Some(val);
        }
    }
    None
}

/// Resolve a VarRef to an integer value using all available strategies.
///
/// Tries in order: direct integer lookup, real-to-integer conversion, parent
/// scope resolution (MLS §7.2), and suffix stripping for over-qualified refs.
fn resolve_varref_integer(name_str: &str, ctx: &ParamEvalContext) -> Option<i64> {
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
    if let Some(val) = ctx.known_reals.get(name_str).copied() {
        return Some(val);
    }
    if let Some(val) = ctx.known_ints.get(name_str).copied() {
        return Some(val as f64);
    }

    if let Some(var_ctx) = ctx.var_context
        && let Some(mut scope) = get_parent_scope(var_ctx)
    {
        loop {
            let qualified = format!("{scope}.{name_str}");
            if let Some(val) = ctx.known_reals.get(&qualified).copied() {
                return Some(val);
            }
            if let Some(val) = ctx.known_ints.get(&qualified).copied() {
                return Some(val as f64);
            }
            match get_parent_scope(scope) {
                Some(parent) => scope = parent,
                None => break,
            }
        }
    }

    let segments = split_path_with_indices(name_str);
    for suffix_start in 1..segments.len() {
        let candidate = segments[suffix_start..].join(".");
        if let Some(val) = ctx.known_reals.get(&candidate).copied() {
            return Some(val);
        }
        if let Some(val) = ctx.known_ints.get(&candidate).copied() {
            return Some(val as f64);
        }
    }

    None
}

/// Evaluate a flat expression to a real using scoped lookup context.
fn try_eval_real_with_context(expr: &flat::Expression, ctx: &ParamEvalContext) -> Option<f64> {
    match expr {
        flat::Expression::Literal(flat::Literal::Real(v)) => Some(*v),
        flat::Expression::Literal(flat::Literal::Integer(v)) => Some(*v as f64),
        flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            resolve_varref_real(&name.to_string(), ctx)
        }
        flat::Expression::FieldAccess { base, field } => {
            let base_name = flatten_field_access_path(base)?;
            resolve_varref_real(&format!("{base_name}.{field}"), ctx)
        }
        flat::Expression::Unary { op, rhs } => {
            let val = try_eval_real_with_context(rhs, ctx)?;
            match op {
                flat::OpUnary::Minus(_) => Some(-val),
                flat::OpUnary::Plus(_) => Some(val),
                _ => None,
            }
        }
        flat::Expression::Binary { op, lhs, rhs } => {
            let l = try_eval_real_with_context(lhs, ctx)?;
            let r = try_eval_real_with_context(rhs, ctx)?;
            match op {
                flat::OpBinary::Add(_) => Some(l + r),
                flat::OpBinary::Sub(_) => Some(l - r),
                flat::OpBinary::Mul(_) => Some(l * r),
                flat::OpBinary::Div(_) => (r != 0.0).then_some(l / r),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Get the parent scope of a qualified name.
fn get_parent_scope(name: &str) -> Option<&str> {
    parent_scope(name)
}

/// Evaluate builtin function calls that return integer, with full context for scope resolution.
fn eval_builtin_integer_with_context(
    function: &rumoca_ir_flat::BuiltinFunction,
    args: &[flat::Expression],
    ctx: &ParamEvalContext,
) -> Option<i64> {
    let known_ints = ctx.known_ints;
    let array_dims = ctx.array_dims;
    let result = match function {
        flat::BuiltinFunction::Floor => {
            let arg = args.first()?;
            try_eval_real_with_context(arg, ctx).map(|v| v.floor() as i64)
        }
        flat::BuiltinFunction::Ceil => {
            let arg = args.first()?;
            try_eval_real_with_context(arg, ctx).map(|v| v.ceil() as i64)
        }
        flat::BuiltinFunction::Integer => {
            let arg = args.first()?;
            try_eval_real_with_context(arg, ctx).map(|v| v as i64)
        }
        flat::BuiltinFunction::Abs => {
            let arg = args.first()?;
            try_eval_flat_expr_integer_with_dims(arg, known_ints, array_dims).map(|v| v.abs())
        }
        flat::BuiltinFunction::Div if args.len() >= 2 => {
            let x = try_eval_flat_expr_integer_with_dims(&args[0], known_ints, array_dims)?;
            let y = try_eval_flat_expr_integer_with_dims(&args[1], known_ints, array_dims)?;
            eval_integer_div_builtin(x, y)
        }
        flat::BuiltinFunction::Mod if args.len() >= 2 => {
            let x = try_eval_flat_expr_integer_with_dims(&args[0], known_ints, array_dims)?;
            let y = try_eval_flat_expr_integer_with_dims(&args[1], known_ints, array_dims)?;
            (y != 0).then_some(x % y)
        }
        flat::BuiltinFunction::Max => {
            #[cfg(feature = "tracing")]
            debug!("evaluating max() builtin");
            eval_max_min_integer_with_dims(args, known_ints, array_dims, true)
        }
        flat::BuiltinFunction::Min => {
            #[cfg(feature = "tracing")]
            debug!("evaluating min() builtin");
            eval_max_min_integer_with_dims(args, known_ints, array_dims, false)
        }
        flat::BuiltinFunction::Sum => {
            #[cfg(feature = "tracing")]
            debug!("evaluating sum() builtin");
            eval_sum_product_integer_with_dims(args, known_ints, array_dims, true)
        }
        flat::BuiltinFunction::Product => {
            #[cfg(feature = "tracing")]
            debug!("evaluating product() builtin");
            eval_sum_product_integer_with_dims(args, known_ints, array_dims, false)
        }
        flat::BuiltinFunction::Size => {
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

/// Evaluate max/min functions that return integer, with array dimension support.
fn eval_max_min_integer_with_dims(
    args: &[flat::Expression],
    known_ints: &FxHashMap<String, i64>,
    array_dims: &FxHashMap<String, Vec<i64>>,
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
        let x = try_eval_flat_expr_integer_with_dims(&args[0], known_ints, array_dims)?;
        let y = try_eval_flat_expr_integer_with_dims(&args[1], known_ints, array_dims)?;
        #[cfg(feature = "tracing")]
        debug!(x = x, y = y, "binary form operands");
        Some(if is_max { x.max(y) } else { x.min(y) })
    } else {
        // Array form: max([a; b; c]) - single argument that's an array
        match &args[0] {
            flat::Expression::Array { elements, .. } => {
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
                try_eval_flat_expr_integer_with_dims(&args[0], known_ints, array_dims)
            }
        }
    }
}

/// Flatten nested array elements into a single vector of scalar expressions.
fn flatten_array_elements(elements: &[flat::Expression]) -> Vec<&flat::Expression> {
    let mut result = Vec::new();
    for elem in elements {
        match elem {
            flat::Expression::Array {
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
    args: &[flat::Expression],
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
        flat::Expression::Array { elements, .. } => {
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
pub(crate) fn try_infer_better_dims(var: &flat::Variable) -> Vec<i64> {
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
pub(crate) fn infer_array_dimensions(expr: &flat::Expression) -> Option<Vec<i64>> {
    infer_array_dimensions_full_with_conds(
        expr,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
    )
}

/// Infer array dimensions with full context including conditional expression support.
pub(crate) fn infer_array_dimensions_full_with_conds(
    expr: &flat::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    match expr {
        flat::Expression::Array {
            elements,
            is_matrix,
        } => infer_array_literal_dimensions(
            elements,
            *is_matrix,
            known_ints,
            known_bools,
            known_enums,
            array_dims,
        ),
        flat::Expression::BuiltinCall { function, args } => {
            infer_builtin_call_dimensions(*function, args, known_ints, array_dims)
        }
        flat::Expression::Range { start, step, end } => {
            infer_range_dimensions(start, step.as_deref(), end, known_ints, array_dims)
        }

        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => infer_array_comprehension_dimensions(
            expr,
            indices,
            filter.as_deref(),
            known_ints,
            known_bools,
            known_enums,
            array_dims,
        ),

        flat::Expression::If {
            branches,
            else_branch,
        } => infer_if_dimensions(
            branches,
            else_branch,
            known_ints,
            known_bools,
            known_enums,
            array_dims,
        ),

        flat::Expression::VarRef { name, subscripts } => {
            if subscripts.is_empty() {
                array_dims.get(&name.to_string()).cloned()
            } else {
                None
            }
        }

        _ => None,
    }
}

fn infer_array_literal_dimensions(
    elements: &[flat::Expression],
    is_matrix: bool,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    if elements.is_empty() {
        return Some(vec![0]);
    }

    if is_matrix {
        let num_rows = elements.len() as i64;
        return match elements.first() {
            Some(flat::Expression::Array {
                elements: row_elements,
                ..
            }) => Some(vec![num_rows, row_elements.len() as i64]),
            // Single-row matrix literal `[a, b, c]` is represented as
            // `Array { is_matrix: true, elements: [...] }`.
            _ => Some(vec![1, num_rows]),
        };
    }

    let mut dims = vec![elements.len() as i64];
    if let Some(first) = elements.first()
        && let Some(inner_dims) = infer_array_dimensions_full_with_conds(
            first,
            known_ints,
            known_bools,
            known_enums,
            array_dims,
        )
    {
        dims.extend(inner_dims);
    }
    Some(dims)
}

fn infer_builtin_call_dimensions(
    function: rumoca_ir_flat::BuiltinFunction,
    args: &[flat::Expression],
    known_ints: &FxHashMap<String, i64>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    match function {
        flat::BuiltinFunction::Zeros | flat::BuiltinFunction::Ones => {
            eval_dimension_args_with_dims(args, known_ints, array_dims)
        }
        flat::BuiltinFunction::Fill => {
            if args.len() < 2 {
                return None;
            }
            eval_dimension_args_with_dims(&args[1..], known_ints, array_dims)
        }
        flat::BuiltinFunction::Linspace => {
            if args.len() != 3 {
                return None;
            }
            let n = try_eval_flat_expr_integer_with_dims(&args[2], known_ints, array_dims)?;
            if n < 2 {
                return None;
            }
            Some(vec![n])
        }
        flat::BuiltinFunction::Identity => {
            if args.len() != 1 {
                return None;
            }
            let n = try_eval_flat_expr_integer_with_dims(&args[0], known_ints, array_dims)?;
            Some(vec![n, n])
        }
        _ => None,
    }
}

fn infer_range_dimensions(
    start: &flat::Expression,
    step: Option<&flat::Expression>,
    end: &flat::Expression,
    known_ints: &FxHashMap<String, i64>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    let start_val = try_eval_flat_expr_integer_with_dims(start, known_ints, array_dims)?;
    let end_val = try_eval_flat_expr_integer_with_dims(end, known_ints, array_dims)?;
    let step_val = step
        .map(|s| try_eval_flat_expr_integer_with_dims(s, known_ints, array_dims))
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

fn infer_array_comprehension_dimensions(
    expr: &flat::Expression,
    indices: &[flat::ComprehensionIndex],
    filter: Option<&flat::Expression>,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    // Cardinality is condition-dependent when filters are present.
    if filter.is_some() {
        return None;
    }

    let mut dims = Vec::with_capacity(indices.len().saturating_add(1));
    for index in indices {
        let range_dims = infer_array_dimensions_full_with_conds(
            &index.range,
            known_ints,
            known_bools,
            known_enums,
            array_dims,
        )?;
        if range_dims.is_empty() {
            return None;
        }
        let iter_size = range_dims
            .iter()
            .copied()
            .fold(1i64, |acc, dim| acc.saturating_mul(dim.max(0)));
        dims.push(iter_size);
    }

    if let Some(mut inner_dims) = infer_array_dimensions_full_with_conds(
        expr,
        known_ints,
        known_bools,
        known_enums,
        array_dims,
    ) {
        dims.append(&mut inner_dims);
    }

    Some(dims)
}

/// Helper to infer dimensions from conditional expressions.
fn infer_if_dimensions(
    branches: &[(flat::Expression, flat::Expression)],
    else_branch: &flat::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    for (cond, then_expr) in branches {
        match try_eval_flat_expr_boolean(cond, known_ints, known_bools, known_enums) {
            Some(true) => {
                return infer_array_dimensions_full_with_conds(
                    then_expr,
                    known_ints,
                    known_bools,
                    known_enums,
                    array_dims,
                );
            }
            Some(false) => continue,
            None => return None,
        }
    }
    infer_array_dimensions_full_with_conds(
        else_branch,
        known_ints,
        known_bools,
        known_enums,
        array_dims,
    )
}

/// Evaluate dimension arguments with access to array dimensions for size() calls.
fn eval_dimension_args_with_dims(
    args: &[flat::Expression],
    known_ints: &FxHashMap<String, i64>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    let mut dims = Vec::with_capacity(args.len());
    for arg in args {
        let dim = try_eval_flat_expr_integer_with_dims(arg, known_ints, array_dims)?;
        dims.push(dim);
    }
    if dims.is_empty() { None } else { Some(dims) }
}

/// Evaluate size(array, dim) builtin function with scope resolution.
///
/// Uses the variable context to resolve unqualified array names to their
/// fully qualified form when looking up dimensions (MLS §5.1).
fn eval_size_integer_with_context(
    args: &[flat::Expression],
    ctx: &ParamEvalContext,
) -> Option<i64> {
    if args.is_empty() || args.len() > 2 {
        #[cfg(feature = "tracing")]
        warn!(arg_count = args.len(), "size() requires 1 or 2 arguments");
        return None;
    }

    // MLS §10.3.1: size(A, i) queries the dimensions of the array expression.
    // During flattening, nested component references can arrive as FieldAccess
    // chains as well as dotted VarRef names.
    let array_name = size_argument_path(&args[0])?;

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
        let dim = try_eval_flat_expr_integer_with_dims(&args[1], ctx.known_ints, ctx.array_dims)?;
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
    let mut scope = start_scope;
    while let Some(parent) = get_parent_scope(scope) {
        let qualified = format!("{}.{}", parent, array_name);
        if let Some(dims) = array_dims.get(&qualified) {
            #[cfg(feature = "tracing")]
            debug!(array = %array_name, qualified = %qualified, dims = ?dims, "found in ancestor");
            return Some(dims.clone());
        }
        scope = parent;
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
    let parent_scope = get_parent_scope(context)?;

    // Try parent scope first
    let qualified = format!("{}.{}", parent_scope, array_name);
    if let Some(dims) = array_dims.get(&qualified) {
        #[cfg(feature = "tracing")]
        debug!(array = %array_name, qualified = %qualified, dims = ?dims, "found in parent scope");
        return Some(dims.clone());
    }

    // 3. Walk up ancestor scopes
    lookup_dims_in_ancestors(array_name, parent_scope, array_dims)
}

/// Evaluate user function calls that return integer.
fn eval_user_func_integer(
    name: &flat::VarName,
    args: &[flat::Expression],
    ctx: &ParamEvalContext,
) -> Option<i64> {
    let name_str = name.to_string();

    // Handle integer() function (user function that converts to integer)
    if name_str == "integer" {
        let arg = args.first()?;
        return try_eval_real_with_context(arg, ctx).map(|v| v as i64);
    }

    // Try to find and evaluate the user-defined function using rumoca_eval_const
    let func = ctx.functions.get(&name_str)?;
    let eval_ctx = build_user_func_eval_ctx(ctx);
    let arg_values = eval_func_args(args, ctx)?;

    let result = rumoca_eval_flat::constant::function_eval::eval_function(
        func,
        arg_values,
        &eval_ctx,
        &rumoca_eval_flat::constant::function_eval::EvalLimits::default(),
        0,
        rumoca_core::Span::DUMMY,
    );

    match result {
        Ok(value) => value.as_integer(),
        Err(_) => None,
    }
}

/// Evaluate user function calls that return a real value.
pub(crate) fn eval_user_func_real(
    name: &flat::VarName,
    args: &[flat::Expression],
    ctx: &ParamEvalContext,
) -> Option<f64> {
    let name_str = name.to_string();
    let func = ctx.functions.get(&name_str)?;
    let eval_ctx = build_user_func_eval_ctx(ctx);
    let arg_values = eval_func_args(args, ctx)?;

    let result = rumoca_eval_flat::constant::function_eval::eval_function(
        func,
        arg_values,
        &eval_ctx,
        &rumoca_eval_flat::constant::function_eval::EvalLimits::default(),
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
    let mut eval_ctx = EvalContext::new();
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

/// Evaluate function arguments to Value list.
fn eval_func_args(args: &[flat::Expression], ctx: &ParamEvalContext) -> Option<Vec<Value>> {
    let mut arg_values = Vec::new();
    for arg in args {
        if let Some(int_val) = try_eval_integer_with_context(arg, ctx) {
            arg_values.push(Value::Integer(int_val));
        } else if let Some(real_val) = try_eval_real_with_context(arg, ctx) {
            arg_values.push(Value::Real(real_val));
        } else {
            return None;
        }
    }
    Some(arg_values)
}

/// Try to evaluate a flat expression to a real value.
pub(crate) fn try_eval_flat_expr_real(
    expr: &flat::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_reals: &FxHashMap<String, f64>,
) -> Option<f64> {
    match expr {
        flat::Expression::Literal(flat::Literal::Integer(n)) => Some(*n as f64),
        flat::Expression::Literal(flat::Literal::Real(r)) => Some(*r),
        flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            let name_str = name.to_string();
            if let Some(v) = known_reals.get(&name_str) {
                return Some(*v);
            }
            known_ints.get(&name_str).map(|&v| v as f64)
        }
        flat::Expression::Unary { op, rhs } => {
            let val = try_eval_flat_expr_real(rhs, known_ints, known_reals)?;
            match op {
                flat::OpUnary::Minus(_) => Some(-val),
                flat::OpUnary::Plus(_) => Some(val),
                _ => None,
            }
        }
        flat::Expression::Binary { op, lhs, rhs } => {
            let l = try_eval_flat_expr_real(lhs, known_ints, known_reals)?;
            let r = try_eval_flat_expr_real(rhs, known_ints, known_reals)?;
            match op {
                flat::OpBinary::Add(_) => Some(l + r),
                flat::OpBinary::Sub(_) => Some(l - r),
                flat::OpBinary::Mul(_) => Some(l * r),
                flat::OpBinary::Div(_) => {
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
pub(crate) fn try_extract_enum_value(expr: &flat::Expression) -> Option<String> {
    match expr {
        flat::Expression::VarRef { name, subscripts } => {
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
pub(crate) fn try_eval_flat_expr_enum(
    expr: &flat::Expression,
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
pub(crate) fn looks_like_enum_literal_path(path: &str) -> bool {
    let parts = crate::path_utils::split_path_with_indices(path);
    if parts.len() < 2 {
        return false;
    }

    parts[..parts.len() - 1]
        .iter()
        .any(|segment| segment.chars().next().is_some_and(char::is_uppercase))
}

/// Resolve an expression to its enum value string (MLS §4.9.5).
fn resolve_enum_value(
    expr: &flat::Expression,
    known_enums: &FxHashMap<String, String>,
) -> Option<String> {
    let flat::Expression::VarRef { name, subscripts } = expr else {
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
    expr: &flat::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
) -> Option<String> {
    match expr {
        flat::Expression::If {
            branches,
            else_branch,
        } => eval_enum_if(branches, else_branch, known_ints, known_bools, known_enums),
        _ => resolve_enum_value(expr, known_enums),
    }
}

/// Evaluate enum if-expressions with compile-time conditions.
fn eval_enum_if(
    branches: &[(flat::Expression, flat::Expression)],
    else_branch: &flat::Expression,
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
pub(crate) fn canonicalize_enum_literal(
    literal: &str,
    known_enums: &FxHashMap<String, String>,
) -> String {
    let parts = crate::path_utils::split_path_with_indices(literal);
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
            let candidate_segments = crate::path_utils::split_path_with_indices(candidate).len();
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
pub(crate) fn try_eval_flat_expr_boolean(
    expr: &flat::Expression,
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
fn eval_bool_inner(expr: &flat::Expression, ctx: &BoolEvalContext) -> Option<bool> {
    match expr {
        flat::Expression::Literal(flat::Literal::Boolean(b)) => Some(*b),
        flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            ctx.known_bools.get(&name.to_string()).copied()
        }
        flat::Expression::Unary {
            op: flat::OpUnary::Not(_),
            rhs,
        } => eval_bool_inner(rhs, ctx).map(|v| !v),
        flat::Expression::Binary { op, lhs, rhs } => eval_bool_binary(op, lhs, rhs, ctx),
        flat::Expression::If {
            branches,
            else_branch,
        } => eval_bool_if(branches, else_branch, ctx),
        _ => None,
    }
}

/// Evaluate binary boolean operations.
fn eval_bool_binary(
    op: &flat::OpBinary,
    lhs: &flat::Expression,
    rhs: &flat::Expression,
    ctx: &BoolEvalContext,
) -> Option<bool> {
    match op {
        flat::OpBinary::And(_) => Some(eval_bool_inner(lhs, ctx)? && eval_bool_inner(rhs, ctx)?),
        flat::OpBinary::Or(_) => Some(eval_bool_inner(lhs, ctx)? || eval_bool_inner(rhs, ctx)?),
        flat::OpBinary::Eq(_) => eval_equality(lhs, rhs, ctx, true),
        flat::OpBinary::Neq(_) => eval_equality(lhs, rhs, ctx, false),
        flat::OpBinary::Lt(_) => eval_int_compare(lhs, rhs, ctx.known_ints, |l, r| l < r),
        flat::OpBinary::Le(_) => eval_int_compare(lhs, rhs, ctx.known_ints, |l, r| l <= r),
        flat::OpBinary::Gt(_) => eval_int_compare(lhs, rhs, ctx.known_ints, |l, r| l > r),
        flat::OpBinary::Ge(_) => eval_int_compare(lhs, rhs, ctx.known_ints, |l, r| l >= r),
        _ => None,
    }
}

/// Evaluate equality/inequality comparisons across types.
fn eval_equality(
    lhs: &flat::Expression,
    rhs: &flat::Expression,
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
    lhs: &flat::Expression,
    rhs: &flat::Expression,
    known_ints: &FxHashMap<String, i64>,
    cmp: fn(i64, i64) -> bool,
) -> Option<bool> {
    let l = try_eval_flat_expr_integer_with_dims(lhs, known_ints, &FxHashMap::default())?;
    let r = try_eval_flat_expr_integer_with_dims(rhs, known_ints, &FxHashMap::default())?;
    Some(cmp(l, r))
}

/// Evaluate if-expression branches.
fn eval_bool_if(
    branches: &[(flat::Expression, flat::Expression)],
    else_branch: &flat::Expression,
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
