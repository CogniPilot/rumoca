//! Expression rendering for the DAE IR expression tree.
//!
//! This module handles recursive rendering of `Expression` variants
//! (Binary, Unary, VarRef, BuiltinCall, FunctionCall, Literal, If,
//! Array, Tuple, Range, ArrayComprehension, Index, FieldAccess).

use super::{ExprConfig, IfStyle, RenderResult, render_vec_with_capacity};
use crate::errors::render_err;
use minijinja::Value;

/// Access a named field from a Value, checking that it exists (not undefined/none).
///
/// Serialized Rust enums produce map-like Values where variant names are keys.
/// minijinja maps return `Ok(undefined)` for missing keys instead of `Err`,
/// so we must check that the returned value is not undefined/none.
/// We try both `get_attr` and `get_item` — `get_attr` may return `Ok(undefined)`
/// for map-like Values even when `get_item` would succeed.
pub(crate) fn get_field(value: &Value, name: &str) -> Result<Value, minijinja::Error> {
    // Try get_attr first
    if let Ok(result) = value.get_attr(name)
        && !result.is_undefined()
        && !result.is_none()
    {
        return Ok(result);
    }
    // Fall back to get_item (maps, sequences)
    if let Ok(result) = value.get_item(&Value::from(name))
        && !result.is_undefined()
        && !result.is_none()
    {
        return Ok(result);
    }
    Err(minijinja::Error::new(
        minijinja::ErrorKind::UndefinedError,
        format!("field '{}' not found", name),
    ))
}

pub(crate) fn is_variant(value: &Value, name: &str) -> bool {
    get_field(value, name).is_ok() || super::value_to_string(value) == name
}

/// Recursively render an expression to a string.
pub(crate) fn render_expression(expr: &Value, cfg: &ExprConfig) -> RenderResult {
    if let Ok(inner) = get_field(expr, "expr") {
        return render_expression(&inner, cfg);
    }
    if let Ok(binary) = get_field(expr, "Binary") {
        return render_binary(&binary, cfg);
    }
    if let Ok(unary) = get_field(expr, "Unary") {
        return render_unary(&unary, cfg);
    }
    if let Ok(var_ref) = get_field(expr, "VarRef") {
        return render_var_ref(&var_ref, cfg);
    }
    if let Ok(builtin) = get_field(expr, "BuiltinCall") {
        return render_builtin(&builtin, cfg);
    }
    if let Ok(func_call) = get_field(expr, "FunctionCall") {
        return render_function_call(&func_call, cfg);
    }
    if let Ok(literal) = get_field(expr, "Literal") {
        return render_literal(&literal, cfg);
    }
    if let Ok(if_expr) = get_field(expr, "If") {
        return render_if(&if_expr, cfg);
    }
    if let Ok(array) = get_field(expr, "Array") {
        return render_array(&array, cfg);
    }
    if let Ok(tuple) = get_field(expr, "Tuple") {
        return render_tuple(&tuple, cfg);
    }
    if let Ok(range) = get_field(expr, "Range") {
        return render_range(&range, cfg);
    }
    if let Ok(array_comp) = get_field(expr, "ArrayComprehension") {
        return render_array_comprehension(&array_comp, cfg);
    }
    if let Ok(index) = get_field(expr, "Index") {
        return render_index(&index, cfg);
    }
    if let Ok(field_access) = get_field(expr, "FieldAccess") {
        return render_field_access(&field_access, cfg);
    }
    if let Ok(named) = get_field(expr, "NamedArgument") {
        return render_named_argument(&named, cfg);
    }
    if let Ok(component_ref) = get_field(expr, "ComponentReference") {
        return super::render_stmt::render_component_ref(&component_ref, cfg);
    }
    if let Ok(component_ref) = get_field(expr, "component_ref") {
        return super::render_stmt::render_component_ref(&component_ref, cfg);
    }
    if get_field(expr, "parts").is_ok() {
        return super::render_stmt::render_component_ref(expr, cfg);
    }
    // Unit variants (e.g. Empty) serialize as plain strings, not objects,
    // so get_field() won't match them — check string representation instead.
    let s = expr.to_string();
    if s == "Empty" {
        return Ok("0".to_string());
    }
    Err(render_err(format!("unhandled Expression variant: {expr}")))
}

fn render_named_argument(named: &Value, cfg: &ExprConfig) -> RenderResult {
    let value = get_field(named, "value")
        .or_else(|_| get_field(named, "expr"))
        .or_else(|_| get_field(named, "arg"))
        .map_err(|_| render_err("NamedArgument expression missing value field"))?;
    render_expression(&value, cfg)
}

fn render_binary(binary: &Value, cfg: &ExprConfig) -> RenderResult {
    let lhs = get_field(binary, "lhs")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("Binary expression missing 'lhs' field"))?;
    let rhs = get_field(binary, "rhs")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("Binary expression missing 'rhs' field"))?;
    let op_value =
        get_field(binary, "op").map_err(|_| render_err("Binary expression missing 'op' field"))?;
    if is_mul_elem_op(&op_value)
        && let Some(func) = &cfg.mul_elem_fn
    {
        return Ok(format!("{func}({lhs}, {rhs})"));
    }
    // Use function-call form for power when power_fn is configured
    // (avoids CasADi MX __pow__ SystemError with integer exponents),
    // or when the power config starts with an alphabetic character
    // (e.g., "pow" for C targets).
    if is_exp_op(&op_value) {
        if let Some(ref power_fn) = cfg.power_fn {
            return Ok(format!("{power_fn}({lhs}, {rhs})"));
        }
        if cfg
            .power
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphabetic())
        {
            return Ok(format!("{}({lhs}, {rhs})", cfg.power));
        }
    }
    // Use function-call form for logical operators when the op string
    // looks like a qualified function name (e.g. "ca.logic_and").
    if is_variant(&op_value, "And") && rumoca_core::has_top_level_dot(&cfg.and_op) {
        return Ok(format!("{}({}, {})", cfg.and_op, lhs, rhs));
    }
    if is_variant(&op_value, "Or") && rumoca_core::has_top_level_dot(&cfg.or_op) {
        return Ok(format!("{}({}, {})", cfg.or_op, lhs, rhs));
    }
    let op_str = get_binop_string(&op_value, cfg)?;
    Ok(format!("({lhs} {op_str} {rhs})"))
}

pub(crate) fn is_exp_op(op: &Value) -> bool {
    is_variant(op, "Exp") || is_variant(op, "ExpElem")
}

pub(crate) fn is_mul_elem_op(op: &Value) -> bool {
    is_variant(op, "MulElem")
}

pub(crate) fn get_binop_string(op: &Value, cfg: &ExprConfig) -> RenderResult {
    if is_variant(op, "Add") || is_variant(op, "AddElem") {
        return Ok("+".to_string());
    }
    if is_variant(op, "Sub") || is_variant(op, "SubElem") {
        return Ok("-".to_string());
    }
    if is_variant(op, "Mul") || is_variant(op, "MulElem") {
        return Ok("*".to_string());
    }
    if is_variant(op, "Div") || is_variant(op, "DivElem") {
        return Ok("/".to_string());
    }
    if is_variant(op, "Exp") || is_variant(op, "ExpElem") {
        return Ok(cfg.power.clone());
    }
    if is_variant(op, "And") {
        return Ok(cfg.and_op.clone());
    }
    if is_variant(op, "Or") {
        return Ok(cfg.or_op.clone());
    }
    if is_variant(op, "Lt") {
        return Ok("<".to_string());
    }
    if is_variant(op, "Le") {
        return Ok("<=".to_string());
    }
    if is_variant(op, "Gt") {
        return Ok(">".to_string());
    }
    if is_variant(op, "Ge") {
        return Ok(">=".to_string());
    }
    if is_variant(op, "Eq") {
        return Ok("==".to_string());
    }
    if is_variant(op, "Neq") {
        return Ok("!=".to_string());
    }
    Err(render_err(format!(
        "unhandled binary operator variant: {op}"
    )))
}

fn render_unary(unary: &Value, cfg: &ExprConfig) -> RenderResult {
    let rhs = get_field(unary, "rhs")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("Unary expression missing 'rhs' field"))?;
    let op =
        get_field(unary, "op").map_err(|_| render_err("Unary expression missing 'op' field"))?;
    // Use function-call form for Not when not_op is a qualified function name.
    if is_variant(&op, "Not") && rumoca_core::has_top_level_dot(&cfg.not_op) {
        return Ok(format!("{}({})", cfg.not_op, rhs));
    }
    let op_str = get_unop_string(&op, cfg)?;
    Ok(format!("({op_str}{rhs})"))
}

pub(crate) fn get_unop_string(op: &Value, cfg: &ExprConfig) -> RenderResult {
    if is_variant(op, "Minus") || is_variant(op, "DotMinus") {
        return Ok("-".to_string());
    }
    if is_variant(op, "Plus") || is_variant(op, "DotPlus") {
        return Ok("+".to_string());
    }
    if is_variant(op, "Not") {
        return Ok(cfg.not_op.clone());
    }
    Err(render_err(format!(
        "unhandled unary operator variant: {op}"
    )))
}

fn render_var_ref(var_ref: &Value, cfg: &ExprConfig) -> RenderResult {
    let raw_name = render_name_field(var_ref, "name", "VarRef")?;
    let source_ref = var_ref_source_ref(&raw_name, var_ref)?;
    if let Some(relation) = condition_alias_relation(cfg.condition_aliases.as_ref(), &source_ref)? {
        return render_expression(&relation, cfg);
    }

    let Some(subs) = get_field(var_ref, "subscripts").ok() else {
        return render_unsubscripted_var_ref_with_source(&raw_name, &source_ref, cfg);
    };
    let len = subs
        .len()
        .ok_or_else(|| render_err("VarRef subscripts field is not a sequence"))?;
    if len == 0 {
        return render_unsubscripted_var_ref_with_source(&raw_name, &source_ref, cfg);
    }

    let all_static = (0..len).all(|i| {
        subs.get_item(&Value::from(i))
            .ok()
            .and_then(|sub| get_field(&sub, "Index").ok())
            .and_then(|idx| subscript_index_value(&idx).ok())
            .is_some()
    });

    let subscripts = render_subscripts(var_ref, cfg)?;
    if cfg.subscript_underscore && all_static {
        // Underscore style: x[1] -> x_1, x[1,2] -> x_1_2.
        let compact_subscripts = subscripts.replace(' ', "");
        let source_ref = format!("{raw_name}[{compact_subscripts}]");
        if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &source_ref) {
            return Ok(symbol);
        }
        let name = super::emitted_symbol(&raw_name, cfg)?;
        Ok(format!("{}_{}", name, compact_subscripts.replace(',', "_")))
    } else if cfg.subscript_underscore {
        if len > 1 {
            return Err(render_err(format!(
                "dynamic multi-dimensional array access is not supported for C aliases: {var_ref}"
            )));
        }
        let name = super::emitted_symbol(&raw_name, cfg)?;
        let pointer_subscripts = render_pointer_subscripts(&subs, cfg)?;
        Ok(format!("{}[{}]", name, pointer_subscripts))
    } else {
        let name = super::emitted_symbol(&raw_name, cfg)?;
        Ok(format!("{}[{}]", name, subscripts))
    }
}

fn render_unsubscripted_var_ref(raw_name: &str, cfg: &ExprConfig) -> RenderResult {
    if let Some((_, value)) = cfg
        .substitutions
        .iter()
        .rev()
        .find(|(name, _)| name == raw_name)
    {
        return Ok(value.clone());
    }
    if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), raw_name) {
        return Ok(symbol);
    }
    if let Some(source_name) = one_based_serialized_component_name(raw_name)
        && let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &source_name)
    {
        return Ok(symbol);
    }
    if cfg.symbols.is_none()
        && let Some(source_name) = zero_component_name_to_one_based(raw_name)
    {
        return super::emitted_symbol(&source_name, cfg);
    }
    super::emitted_symbol(raw_name, cfg)
}

fn render_unsubscripted_var_ref_with_source(
    raw_name: &str,
    source_ref: &str,
    cfg: &ExprConfig,
) -> RenderResult {
    if let Some(scope) = cfg.source_scope.as_deref()
        && let Some(rest) = source_ref.strip_prefix(scope)
        && rest.starts_with('.')
        && super::lookup_symbol_value(cfg.symbols.as_ref(), source_ref).is_some()
    {
        return Ok(super::sanitize_name(source_ref));
    }
    if source_ref != raw_name
        && let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), source_ref)
    {
        return Ok(symbol);
    }
    if source_ref == raw_name
        && !rumoca_core::has_top_level_dot(raw_name)
        && let Some(scope) = cfg.source_scope.as_deref()
    {
        let scoped_ref = format!("{scope}.{raw_name}");
        if super::lookup_symbol_value(cfg.symbols.as_ref(), &scoped_ref).is_some() {
            return Ok(super::sanitize_name(&scoped_ref));
        }
    }
    render_unsubscripted_var_ref(raw_name, cfg)
}

fn one_based_serialized_component_name(name: &str) -> Option<String> {
    canonicalize_serialized_component_indices(name, false)
}

fn zero_component_name_to_one_based(name: &str) -> Option<String> {
    canonicalize_serialized_component_indices(name, true)
}

fn canonicalize_serialized_component_indices(name: &str, zeros_only: bool) -> Option<String> {
    let bytes = name.as_bytes();
    let mut rendered = String::with_capacity(name.len());
    let mut cursor = 0;
    let mut changed = false;

    while cursor < bytes.len() {
        if bytes[cursor] != b'[' {
            let next = name[cursor..]
                .find('[')
                .map(|offset| cursor + offset)
                .unwrap_or(bytes.len());
            rendered.push_str(&name[cursor..next]);
            cursor = next;
            continue;
        }

        let index_start = cursor + 1;
        let mut index_end = index_start;
        while index_end < bytes.len() && bytes[index_end].is_ascii_digit() {
            index_end += 1;
        }
        if index_end == index_start || index_end >= bytes.len() || bytes[index_end] != b']' {
            rendered.push('[');
            cursor += 1;
            continue;
        }

        let index_text = &name[index_start..index_end];
        let Ok(index) = index_text.parse::<u64>() else {
            rendered.push_str(&name[cursor..=index_end]);
            cursor = index_end + 1;
            continue;
        };
        if zeros_only && index != 0 {
            rendered.push_str(&name[cursor..=index_end]);
        } else {
            rendered.push('[');
            rendered.push_str(&(index + 1).to_string());
            rendered.push(']');
            changed = true;
        }
        cursor = index_end + 1;
    }

    changed.then_some(rendered)
}

fn var_ref_source_ref(raw_name: &str, var_ref: &Value) -> RenderResult {
    let base_name = if let Ok(name) = get_field(var_ref, "name")
        && let Ok(component_ref) = get_field(&name, "component_ref")
    {
        render_component_ref_source_name(&component_ref, "VarRef", "name")?
    } else {
        raw_name.to_string()
    };
    let subscripts = render_source_subscripts(var_ref)?;
    if subscripts.is_empty() {
        Ok(base_name)
    } else {
        Ok(format!("{base_name}[{subscripts}]"))
    }
}

fn render_source_subscripts(var_ref: &Value) -> RenderResult {
    let Some(subs) = get_field(var_ref, "subscripts").ok() else {
        return Ok(String::new());
    };
    let len = subs
        .len()
        .ok_or_else(|| render_err("VarRef subscripts field is not a sequence"))?;
    if len == 0 {
        return Ok(String::new());
    }

    let cfg = ExprConfig {
        one_based_index: true,
        ..Default::default()
    };
    let mut sub_strs = render_vec_with_capacity(len, "VarRef rendered subscript count")?;
    for i in 0..len {
        let sub = subs
            .get_item(&Value::from(i))
            .map_err(|err| render_err(format!("VarRef subscript {i} is inaccessible: {err}")))?;
        if sub.is_undefined() || sub.is_none() {
            return Err(render_err(format!("VarRef subscript {i} is missing")));
        }
        sub_strs.push(render_subscript(&sub, &cfg)?);
    }
    Ok(sub_strs.join(","))
}

fn condition_alias_relation(
    aliases: Option<&Value>,
    source_ref: &str,
) -> Result<Option<Value>, minijinja::Error> {
    let Some(aliases) = aliases else {
        return no_condition_alias_relation();
    };
    let len = aliases
        .len()
        .ok_or_else(|| render_err("condition_aliases field is not a sequence"))?;
    for i in 0..len {
        let alias = aliases.get_item(&Value::from(i)).map_err(|err| {
            render_err(format!(
                "condition_aliases entry {i} is inaccessible: {err}"
            ))
        })?;
        if alias.is_undefined() || alias.is_none() {
            return Err(render_err(format!(
                "condition_aliases entry {i} is missing"
            )));
        }
        let condition = get_field(&alias, "condition").map_err(|err| {
            render_err(format!(
                "condition_aliases entry {i} missing condition: {err}"
            ))
        })?;
        let condition_ref = condition_source_ref(&condition)?;
        if condition_ref == source_ref {
            return get_field(&alias, "relation").map(Some).map_err(|err| {
                render_err(format!(
                    "condition_aliases entry {i} missing relation: {err}"
                ))
            });
        }
    }
    no_condition_alias_relation()
}

fn no_condition_alias_relation() -> Result<Option<Value>, minijinja::Error> {
    Ok(Option::None)
}

fn condition_source_ref(condition: &Value) -> Result<String, minijinja::Error> {
    let var_ref = get_field(condition, "VarRef")
        .map_err(|err| render_err(format!("condition alias is not a VarRef: {err}")))?;
    let raw_name = render_name_field(&var_ref, "name", "condition alias VarRef")?;
    var_ref_source_ref(&raw_name, &var_ref)
}

fn render_name_field(value: &Value, field: &str, context: &str) -> RenderResult {
    let name = get_field(value, field)
        .map_err(|err| render_err(format!("{context} missing '{field}' field: {err}")))?;
    let rendered = render_serialized_name(&name);
    if rendered.is_empty() {
        return Err(render_err(format!(
            "{context} '{field}' field resolved to an empty name"
        )));
    }
    Ok(rendered)
}

fn render_component_ref_source_name(
    component_ref: &Value,
    context: &str,
    field: &str,
) -> RenderResult {
    let cfg = ExprConfig {
        one_based_index: true,
        sanitize_dots: false,
        ..ExprConfig::default()
    };
    let rendered = super::render_stmt::render_component_ref(component_ref, &cfg)?;
    if rendered.is_empty() {
        return Err(render_err(format!(
            "{context} '{field}' component_ref resolved to an empty name"
        )));
    }
    Ok(rendered)
}

pub(crate) fn render_serialized_name(value: &Value) -> String {
    if let Ok(name) = get_field(value, "name") {
        return render_serialized_name(&name);
    }
    if let Ok(name) = get_field(value, "0") {
        return super::value_to_string(&name);
    }
    super::value_to_string(value)
}

fn render_subscripts(var_ref: &Value, cfg: &ExprConfig) -> RenderResult {
    let Some(subs) = get_field(var_ref, "subscripts").ok() else {
        return Ok(String::new());
    };
    let len = subs
        .len()
        .ok_or_else(|| render_err("VarRef subscripts field is not a sequence"))?;
    if len == 0 {
        return Ok(String::new());
    }

    let mut sub_strs = Vec::new();
    for i in 0..len {
        match subs.get_item(&Value::from(i)) {
            Ok(sub) if !sub.is_undefined() && !sub.is_none() => {
                sub_strs.push(render_subscript(&sub, cfg)?);
            }
            Ok(_) => return Err(render_err(format!("VarRef subscript {i} is missing"))),
            Err(err) => {
                return Err(render_err(format!(
                    "VarRef subscript {i} is inaccessible: {err}"
                )));
            }
        }
    }

    Ok(sub_strs.join(", "))
}

fn render_pointer_subscripts(subs: &Value, cfg: &ExprConfig) -> RenderResult {
    let len = subs
        .len()
        .ok_or_else(|| render_err("VarRef subscripts field is not a sequence"))?;
    let index_cfg = ExprConfig {
        one_based_index: false,
        subscript_underscore: false,
        ..cfg.clone()
    };
    let mut sub_strs = render_vec_with_capacity(len, "VarRef pointer subscript count")?;
    for i in 0..len {
        let sub = subs
            .get_item(&Value::from(i))
            .map_err(|err| render_err(format!("VarRef subscript {i} is inaccessible: {err}")))?;
        if sub.is_undefined() || sub.is_none() {
            return Err(render_err(format!("VarRef subscript {i} is missing")));
        }
        sub_strs.push(render_pointer_subscript(&sub, &index_cfg)?);
    }
    Ok(sub_strs.join(", "))
}

fn render_pointer_subscript(sub: &Value, cfg: &ExprConfig) -> RenderResult {
    if let Ok(idx) = get_field(sub, "Index") {
        return Ok(format!("{}", subscript_index_value(&idx)? - 1));
    }
    if get_field(sub, "Colon").is_ok() {
        return Err(render_err(
            "slice subscripts are not supported in C array aliases",
        ));
    }
    if let Ok(expr) = get_field(sub, "Expr") {
        let expr = get_field(&expr, "expr").unwrap_or(expr);
        let rendered = render_expression(&expr, cfg)?;
        return Ok(format!("(({}) - 1)", rendered));
    }
    Err(render_err(format!("unhandled Subscript variant: {sub}")))
}

pub(crate) fn render_subscript(sub: &Value, cfg: &ExprConfig) -> RenderResult {
    if let Ok(idx) = get_field(sub, "Index") {
        let val = subscript_index_value(&idx)?;
        return if cfg.one_based_index || cfg.subscript_underscore {
            Ok(format!("{}", val))
        } else {
            Ok(format!("{}", val - 1))
        };
    }
    if get_field(sub, "Colon").is_ok() {
        return Ok(":".to_string());
    }
    if let Ok(expr) = get_field(sub, "Expr") {
        let expr = get_field(&expr, "expr").unwrap_or(expr);
        return render_expression(&expr, cfg);
    }
    Err(render_err(format!("unhandled Subscript variant: {sub}")))
}

pub(crate) fn subscript_index_value(index: &Value) -> Result<i64, minijinja::Error> {
    if let Some(value) = index.as_i64() {
        return Ok(value);
    }
    get_field(index, "value")?
        .as_i64()
        .ok_or_else(|| render_err("subscript Index value is not an integer"))
}

fn render_builtin(builtin: &Value, cfg: &ExprConfig) -> RenderResult {
    let func_name = render_builtin_name(builtin)?;

    // Strip semantic wrappers that don't map to any runtime function.
    // These must be handled before render_args() to avoid rendering the
    // wrapper arguments as a flat list.
    match func_name.as_str() {
        "Smooth" | "NoEvent" | "Homotopy" => {
            let args_val = get_field(builtin, "args")?;
            // Smooth: arg[1] is the expression (arg[0] is smoothness order)
            // Homotopy: arg[0] is the actual expression (arg[1] is simplified)
            // NoEvent: arg[0] is the expression
            let idx = if func_name == "Smooth" { 1 } else { 0 };
            let inner = required_arg(&args_val, idx, &format!("BuiltinCall {func_name}"))?;
            return render_expression(&inner, cfg);
        }
        "Sample" => {
            let args_val = get_field(builtin, "args")?;
            match args_val.len() {
                Some(0) => {
                    return Err(render_err("BuiltinCall Sample missing required argument 0"));
                }
                Some(1) => {
                    let inner = required_arg(&args_val, 0, "BuiltinCall Sample")?;
                    return render_expression(&inner, cfg);
                }
                Some(count) => {
                    return Err(render_err(format!(
                        "BuiltinCall Sample with {count} arguments must be lowered before template rendering"
                    )));
                }
                None => {
                    return Err(render_err(
                        "BuiltinCall Sample args field is not a sequence",
                    ));
                }
            }
        }
        "Clock" => {
            // Clock() constructor (MLS §16.3). In continuous simulation
            // context this is not meaningful; return 0 as a stub.
            return Ok("0".to_string());
        }
        "Previous" => {
            // previous(x) — clocked partition operator (MLS §16.4).
            // In continuous simulation, treat like pre(): return the
            // argument unchanged.
            let args_val = get_field(builtin, "args")?;
            let inner = required_arg(&args_val, 0, "BuiltinCall Previous")?;
            return render_expression(&inner, cfg);
        }
        "Hold" => {
            // hold(x) — clocked-to-continuous (MLS §16.5.1).
            // Pass through the argument.
            let args_val = get_field(builtin, "args")?;
            let inner = required_arg(&args_val, 0, "BuiltinCall Hold")?;
            return render_expression(&inner, cfg);
        }
        "FirstTick" => {
            // firstTick(u) — true at the first clock tick (MLS §16.10).
            // Stub: return false for continuous simulation.
            require_min_arg_count(builtin, 1, "BuiltinCall FirstTick")?;
            return Ok(cfg.false_val.clone());
        }
        "NoClock" | "SubSample" | "SuperSample" | "ShiftSample" | "BackSample" => {
            // Clocked partition operators (MLS §16). In continuous
            // simulation, pass through the first argument.
            let args_val = get_field(builtin, "args")?;
            let inner = required_arg(&args_val, 0, &format!("BuiltinCall {func_name}"))?;
            return render_expression(&inner, cfg);
        }
        _ => {}
    }

    // Handle Min/Max/Sum with single Array argument: expand to chained calls.
    // Modelica `min({a,b,c})` → C `fmin(fmin(a,b),c)` (not `fmin((double[]){a,b,c})`)
    if matches!(func_name.as_str(), "Min" | "Max" | "Sum")
        && let args_val = get_field(builtin, "args")?
        && args_val.len() == Some(1)
        && let Ok(first_arg) = args_val.get_item(&Value::from(0))
    {
        // Direct Array argument: expand inline
        if let Ok(array) = get_field(&first_arg, "Array")
            && let Ok(elements) = get_field(&array, "elements")
        {
            let len = elements.len().unwrap_or(0);
            if len > 0 {
                return render_chained_minmaxsum(&func_name, &elements, len, cfg);
            }
        }
        // ArrayComprehension argument for C targets: unroll to chained sum
        if func_name == "Sum"
            && matches!(cfg.if_style, super::IfStyle::Ternary)
            && get_field(&first_arg, "ArrayComprehension").is_ok()
        {
            let unrolled = render_expression(&first_arg, cfg)?;
            // If the comprehension unrolled to a scalar (e.g., REAL_C(0.0)
            // for empty range), return it directly
            if !unrolled.starts_with(&cfg.array_start) {
                return Ok(unrolled);
            }
            // Otherwise it's a C array literal — not valid for __rumoca_sum
            // since it needs (arr, n). For now, return 0 for empty results.
            return Ok(format!("({unrolled})"));
        }
    }

    let args = render_args(builtin, cfg)?;

    if cfg.modelica_builtins {
        return Ok(render_builtin_modelica(&func_name, &args, cfg));
    }
    Ok(render_builtin_python(&func_name, &args, cfg))
}

fn render_builtin_name(builtin: &Value) -> RenderResult {
    let name = get_field(builtin, "function")
        .map_err(|err| render_err(format!("BuiltinCall missing 'function' field: {err}")))?;
    let rendered = super::value_to_string(&name);
    if rendered.is_empty() {
        return Err(render_err(
            "BuiltinCall 'function' field resolved to an empty name",
        ));
    }
    Ok(rendered)
}

fn required_arg(args: &Value, index: usize, context: &str) -> Result<Value, minijinja::Error> {
    let len = args
        .len()
        .ok_or_else(|| render_err(format!("{context} args is not a sequence")))?;
    if index >= len {
        return Err(render_err(format!(
            "{context} missing required argument {index}"
        )));
    }
    let arg = args
        .get_item(&Value::from(index))
        .map_err(|err| render_err(format!("{context} argument {index} is inaccessible: {err}")))?;
    if arg.is_undefined() || arg.is_none() {
        return Err(render_err(format!(
            "{context} missing required argument {index}"
        )));
    }
    Ok(arg)
}

fn require_min_arg_count(call: &Value, min: usize, context: &str) -> Result<(), minijinja::Error> {
    let args = get_field(call, "args")
        .map_err(|err| render_err(format!("{context} missing 'args' field: {err}")))?;
    let len = args
        .len()
        .ok_or_else(|| render_err(format!("{context} args is not a sequence")))?;
    if len < min {
        return Err(render_err(format!(
            "{context} expected at least {min} argument(s), got {len}"
        )));
    }
    Ok(())
}

/// Render builtins using Modelica names (abs, min, max, etc.).
fn render_builtin_modelica(func_name: &str, args: &str, _cfg: &ExprConfig) -> String {
    match func_name {
        "Der" => format!("der({})", args),
        "Pre" => format!("pre({})", args),
        "Abs" => format!("abs({})", args),
        "Sign" => format!("sign({})", args),
        "Sqrt" => format!("sqrt({})", args),
        "Sin" => format!("sin({})", args),
        "Cos" => format!("cos({})", args),
        "Tan" => format!("tan({})", args),
        "Asin" => format!("asin({})", args),
        "Acos" => format!("acos({})", args),
        "Atan" => format!("atan({})", args),
        "Atan2" => format!("atan2({})", args),
        "Sinh" => format!("sinh({})", args),
        "Cosh" => format!("cosh({})", args),
        "Tanh" => format!("tanh({})", args),
        "Exp" => format!("exp({})", args),
        "Log" => format!("log({})", args),
        "Log10" => format!("log10({})", args),
        "Floor" => format!("floor({})", args),
        "Integer" => format!("integer({})", args),
        "Ceil" => format!("ceil({})", args),
        "Min" => format!("min({})", args),
        "Max" => format!("max({})", args),
        "Sum" => format!("sum({})", args),
        "Transpose" => format!("transpose({})", args),
        "Zeros" => format!("zeros({})", args),
        "Ones" => format!("ones({})", args),
        "Identity" => format!("identity({})", args),
        "Cross" => format!("cross({})", args),
        "Div" => format!("div({})", args),
        "Mod" => format!("mod({})", args),
        "Rem" => format!("rem({})", args),
        _ => format!("{}({})", func_name.to_lowercase(), args),
    }
}

/// Render builtins using Python/CasADi names (fabs, fmin, fmax, etc.).
fn render_builtin_python(func_name: &str, args: &str, cfg: &ExprConfig) -> String {
    match func_name {
        "Der" => format!("der({})", args),
        "Pre" => format!("pre({})", args),
        "Abs" => format!("{}fabs({})", cfg.prefix, args),
        "Sign" => format!("{}sign({})", cfg.prefix, args),
        "Sqrt" => format!("{}sqrt({})", cfg.prefix, args),
        "Sin" => format!("{}sin({})", cfg.prefix, args),
        "Cos" => format!("{}cos({})", cfg.prefix, args),
        "Tan" => format!("{}tan({})", cfg.prefix, args),
        "Asin" => format!("{}asin({})", cfg.prefix, args),
        "Acos" => format!("{}acos({})", cfg.prefix, args),
        "Atan" => format!("{}atan({})", cfg.prefix, args),
        "Atan2" => format!("{}atan2({})", cfg.prefix, args),
        "Sinh" => format!("{}sinh({})", cfg.prefix, args),
        "Cosh" => format!("{}cosh({})", cfg.prefix, args),
        "Tanh" => format!("{}tanh({})", cfg.prefix, args),
        "Exp" => format!("{}exp({})", cfg.prefix, args),
        "Log" => format!("{}log({})", cfg.prefix, args),
        "Log10" => format!("{}log10({})", cfg.prefix, args),
        "Floor" => format!("{}floor({})", cfg.prefix, args),
        "Integer" => format!("{}trunc({})", cfg.prefix, args),
        "Ceil" => format!("{}ceil({})", cfg.prefix, args),
        "Min" => format!("{}fmin({})", cfg.prefix, args),
        "Max" => format!("{}fmax({})", cfg.prefix, args),
        "Sum" => {
            if cfg.sum_fn == "sum1" {
                // Default: use prefix (e.g., ca.sum1 for CasADi, sum1 for others)
                format!("{}sum1({})", cfg.prefix, args)
            } else {
                // Template-configured: use sum_fn as-is (e.g., __rumoca_sum, _sum)
                format!("{}({})", cfg.sum_fn, args)
            }
        }
        "Transpose" => format!("({}).T", args),
        "Zeros" => format!("{}zeros({})", cfg.prefix, args),
        "Ones" => format!("{}ones({})", cfg.prefix, args),
        "Identity" => format!("{}eye({})", cfg.prefix, args),
        "Cross" => format!("{}cross({})", cfg.prefix, args),
        "Div" => format!("{}div({})", cfg.prefix, args),
        "Mod" => format!("{}fmod({})", cfg.prefix, args),
        "Rem" => format!("{}remainder({})", cfg.prefix, args),
        "Fill" => {
            // fill(val, n) → val (scalar broadcast; array fill not supported yet)
            if let Some(comma_pos) = args.find(',') {
                args[..comma_pos].trim().to_string()
            } else {
                format!("{}fill({})", cfg.prefix, args)
            }
        }
        "Size" => {
            // size(arr, dim) — not directly representable in Python, return 0
            "0".to_string()
        }
        "Interval" => {
            // interval(u) — clocked partition intrinsic (MLS §16.10)
            // In continuous simulation, return the clock period if known
            "0.0".to_string()
        }
        _ => format!("{}({})", func_name.to_lowercase(), args),
    }
}

/// Expand `min({a,b,c})` → `fmin(fmin(a,b),c)` (or `fmax`, or `((a)+(b)+(c))` for sum).
fn render_chained_minmaxsum(
    func_name: &str,
    elements: &Value,
    len: usize,
    cfg: &ExprConfig,
) -> RenderResult {
    let mut elem_strs = Vec::new();
    for i in 0..len {
        match elements.get_item(&Value::from(i)) {
            Ok(elem) if !elem.is_undefined() && !elem.is_none() => {
                elem_strs.push(render_expression(&elem, cfg)?);
            }
            Ok(_) => {
                return Err(render_err(format!(
                    "{func_name} array argument missing element {i}"
                )));
            }
            Err(err) => {
                return Err(render_err(format!(
                    "{func_name} array argument element {i} is inaccessible: {err}"
                )));
            }
        }
    }
    if elem_strs.is_empty() {
        if func_name == "Sum" {
            return Ok("0".to_string());
        }
        return Err(render_err(format!("{func_name} array argument is empty")));
    }
    if elem_strs.len() == 1 {
        return Ok(elem_strs.remove(0));
    }
    match func_name {
        "Sum" => {
            // sum({a,b,c}) → ((a) + (b) + (c))
            let parts: Vec<String> = elem_strs.iter().map(|s| format!("({})", s)).collect();
            Ok(format!("({})", parts.join(" + ")))
        }
        _ => {
            // Min/Max: chain fmin/fmax calls
            let fn_name = if func_name == "Min" {
                if cfg.modelica_builtins { "min" } else { "fmin" }
            } else {
                if cfg.modelica_builtins { "max" } else { "fmax" }
            };
            let prefix = &cfg.prefix;
            let mut result = elem_strs[0].clone();
            for elem in &elem_strs[1..] {
                result = format!("{prefix}{fn_name}({result}, {elem})");
            }
            Ok(result)
        }
    }
}

fn render_function_call(func_call: &Value, cfg: &ExprConfig) -> RenderResult {
    let raw_name = render_name_field(func_call, "name", "FunctionCall")?;

    // Map Modelica standard library math functions to builtins
    if let Some(builtin) = resolve_modelica_math_function(&raw_name) {
        let args = render_args(func_call, cfg)?;
        return Ok(render_builtin_python(builtin, &args, cfg));
    }

    let name = super::emitted_symbol(&raw_name, cfg)?;

    let args = render_args(func_call, cfg)?;
    Ok(format!("{}({})", name, args))
}

/// Map Modelica.Math.* function names to their BuiltinCall equivalents.
/// Returns the builtin function name (e.g., "Sin", "Cos") if recognized.
fn resolve_modelica_math_function(name: &str) -> Option<&'static str> {
    match name {
        "Modelica.Math.sin" => Some("Sin"),
        "Modelica.Math.cos" => Some("Cos"),
        "Modelica.Math.tan" => Some("Tan"),
        "Modelica.Math.asin" => Some("Asin"),
        "Modelica.Math.acos" => Some("Acos"),
        "Modelica.Math.atan" => Some("Atan"),
        "Modelica.Math.atan2" => Some("Atan2"),
        "Modelica.Math.sinh" => Some("Sinh"),
        "Modelica.Math.cosh" => Some("Cosh"),
        "Modelica.Math.tanh" => Some("Tanh"),
        "Modelica.Math.exp" => Some("Exp"),
        "Modelica.Math.log" => Some("Log"),
        "Modelica.Math.log10" => Some("Log10"),
        _ => None,
    }
}

pub(crate) fn render_args(call: &Value, cfg: &ExprConfig) -> RenderResult {
    let Some(args) = get_field(call, "args").ok() else {
        return Ok(String::new());
    };
    let len = args
        .len()
        .ok_or_else(|| render_err("function arguments field is not a sequence"))?;

    let mut arg_strs = Vec::new();
    for i in 0..len {
        match args.get_item(&Value::from(i)) {
            Ok(arg) if !arg.is_undefined() && !arg.is_none() => {
                arg_strs.push(render_function_argument(&arg, cfg)?);
            }
            Ok(_) => return Err(render_err(format!("function argument {i} is missing"))),
            Err(err) => {
                return Err(render_err(format!(
                    "function argument {i} is inaccessible: {err}"
                )));
            }
        }
    }

    Ok(arg_strs.join(", "))
}

fn render_function_argument(arg: &Value, cfg: &ExprConfig) -> RenderResult {
    if let Ok(call) = get_field(arg, "FunctionCall")
        && let Ok(name) = render_name_field(&call, "name", "FunctionCall")
        && name.starts_with("__rumoca_named_arg__.")
    {
        let args = get_field(&call, "args")
            .map_err(|err| render_err(format!("named function argument missing args: {err}")))?;
        let value = required_arg(&args, 0, "named function argument")?;
        return render_expression(&value, cfg);
    }
    render_expression(arg, cfg)
}

fn render_literal(literal: &Value, cfg: &ExprConfig) -> RenderResult {
    let literal_value = get_field(literal, "value").unwrap_or_else(|_| literal.clone());
    if let Ok(real) = get_field(&literal_value, "Real") {
        if cfg.float_literals {
            let s = real.to_string();
            return Ok(render_c_float_literal(&s));
        }
        return Ok(real.to_string());
    }
    if let Ok(int) = get_field(&literal_value, "Integer") {
        if cfg.float_literals {
            return Ok(format!("{}.0f", int));
        }
        return Ok(int.to_string());
    }
    if let Ok(b) = get_field(&literal_value, "Boolean") {
        return Ok(if b.is_true() {
            cfg.true_val.clone()
        } else {
            cfg.false_val.clone()
        });
    }
    if let Ok(s) = get_field(&literal_value, "String") {
        return Ok(format!("\"{}\"", s));
    }
    Err(render_err(format!(
        "unhandled literal variant: {literal_value}"
    )))
}

fn render_c_float_literal(literal_text: &str) -> String {
    if literal_text.contains(['.', 'e', 'E']) {
        format!("{literal_text}f")
    } else {
        format!("{literal_text}.0f")
    }
}

fn render_if(if_expr: &Value, cfg: &ExprConfig) -> RenderResult {
    let else_branch = get_field(if_expr, "else_branch")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|err| render_err(format!("If expression invalid else_branch: {err}")))?;

    let branches = get_field(if_expr, "branches")
        .map_err(|err| render_err(format!("If expression missing branches: {err}")))?;
    let len = branches
        .len()
        .ok_or_else(|| render_err("If expression branches is not a sequence"))?;

    render_if_branches(&branches, len, &else_branch, cfg)
}

fn render_if_branches(
    branches: &Value,
    len: usize,
    else_branch: &str,
    cfg: &ExprConfig,
) -> RenderResult {
    let mut result = else_branch.to_string();

    for i in (0..len).rev() {
        let branch = branches
            .get_item(&Value::from(i))
            .map_err(|err| render_err(format!("If expression branch {i} missing: {err}")))?;
        if branch.is_undefined() || branch.is_none() {
            return Err(render_err(format!("If expression branch {i} missing")));
        }
        let cond = branch.get_item(&Value::from(0)).map_err(|err| {
            render_err(format!("If expression branch {i} missing condition: {err}"))
        })?;
        if cond.is_undefined() || cond.is_none() {
            return Err(render_err(format!(
                "If expression branch {i} missing condition"
            )));
        }
        let then = branch
            .get_item(&Value::from(1))
            .map_err(|err| render_err(format!("If expression branch {i} missing value: {err}")))?;
        if then.is_undefined() || then.is_none() {
            return Err(render_err(format!(
                "If expression branch {i} missing value"
            )));
        }

        let cond_str = render_expression(&cond, cfg)?;
        let then_str = render_expression(&then, cfg)?;

        result = match cfg.if_style {
            IfStyle::Function => {
                let fn_name = cfg.if_else_fn.as_deref().unwrap_or("if_else");
                format!(
                    "{}{}({}, {}, {})",
                    cfg.prefix, fn_name, cond_str, then_str, result
                )
            }
            IfStyle::Ternary => {
                format!("({} ? {} : {})", cond_str, then_str, result)
            }
            IfStyle::Modelica => {
                format!("(if {} then {} else {})", cond_str, then_str, result)
            }
        };
    }

    Ok(result)
}

fn render_array(array: &Value, cfg: &ExprConfig) -> RenderResult {
    let Some(elements) = get_field(array, "elements").ok() else {
        return Ok(format!("{}{}", cfg.array_start, cfg.array_end));
    };
    let Some(len) = elements.len() else {
        return Ok(format!("{}{}", cfg.array_start, cfg.array_end));
    };

    let mut elem_strs = Vec::new();
    for i in 0..len {
        if let Ok(elem) = elements.get_item(&Value::from(i)) {
            elem_strs.push(render_expression(&elem, cfg)?);
        }
    }

    Ok(format!(
        "{}{}{}",
        cfg.array_start,
        elem_strs.join(", "),
        cfg.array_end
    ))
}

/// Render a tuple expression as `(e1, e2, ...)` (MLS §8.3.1 multi-output function calls).
fn render_tuple(tuple: &Value, cfg: &ExprConfig) -> RenderResult {
    let elements =
        get_field(tuple, "elements").map_err(|_| render_err("Tuple missing 'elements' field"))?;
    let len = elements
        .len()
        .ok_or_else(|| render_err("Tuple 'elements' has no length"))?;

    let mut elem_strs = Vec::new();
    for i in 0..len {
        if let Ok(elem) = elements.get_item(&Value::from(i)) {
            elem_strs.push(render_expression(&elem, cfg)?);
        }
    }

    Ok(format!("({})", elem_strs.join(", ")))
}

/// Render a range expression as `start:step:end` or `start:end`.
/// For Python targets (`python_range = true`), renders as `range(start, end + 1)`
/// or `range(start, end + 1, step)` since Modelica ranges are 1-based inclusive.
fn render_range(range: &Value, cfg: &ExprConfig) -> RenderResult {
    let start = get_field(range, "start")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("Range missing 'start' field"))?;
    let end = get_field(range, "end")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("Range missing 'end' field"))?;
    if cfg.python_range {
        let end_plus1 = python_range_end(&end);
        if let Ok(step) = get_field(range, "step") {
            let step_str = render_expression(&step, cfg)?;
            Ok(format!("range({start}, {end_plus1}, {step_str})"))
        } else {
            Ok(format!("range({start}, {end_plus1})"))
        }
    } else if let Ok(step) = get_field(range, "step") {
        let step_str = render_expression(&step, cfg)?;
        Ok(format!("{start}:{step_str}:{end}"))
    } else {
        Ok(format!("{start}:{end}"))
    }
}

/// Compute `end + 1` for Python range (Modelica ranges are inclusive).
/// If end is a simple integer literal, fold it at render time.
fn python_range_end(end: &str) -> String {
    if let Ok(n) = end.parse::<i64>() {
        format!("{}", n + 1)
    } else {
        format!("{end} + 1")
    }
}

/// Render an array-comprehension expression as `{expr for i in range ... if filter}`.
///
/// For C targets (`IfStyle::Ternary`), attempts to unroll the comprehension
/// into an array literal when the range has statically-known integer bounds.
/// Falls back to rendering a 0 literal for empty ranges.
fn render_array_comprehension(array_comp: &Value, cfg: &ExprConfig) -> RenderResult {
    let indices = get_field(array_comp, "indices")
        .map_err(|_| render_err("ArrayComprehension missing 'indices' field"))?;
    let len = indices.len().unwrap_or(0);

    // For C targets, try to unroll the comprehension at render time
    if matches!(cfg.if_style, super::IfStyle::Ternary)
        && len == 1
        && let Ok(unrolled) = try_unroll_c_comprehension(array_comp, cfg)
    {
        return Ok(unrolled);
    }

    let body = get_field(array_comp, "expr")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("ArrayComprehension missing 'expr' field"))?;

    let mut index_clauses = Vec::new();
    for i in 0..len {
        let index = indices
            .get_item(&Value::from(i))
            .map_err(|_| render_err("ArrayComprehension index entry missing"))?;
        let name = get_field(&index, "name")
            .map(|v| v.to_string())
            .map_err(|_| render_err("ArrayComprehension index missing 'name' field"))?;
        let range = get_field(&index, "range")
            .and_then(|v| render_expression(&v, cfg))
            .map_err(|_| render_err("ArrayComprehension index missing 'range' field"))?;
        index_clauses.push(format!("{name} in {range}"));
    }

    let for_clause = if index_clauses.is_empty() {
        String::new()
    } else {
        format!(" for {}", index_clauses.join(", "))
    };
    let filter_clause = if let Ok(filter) = get_field(array_comp, "filter") {
        let cond = render_expression(&filter, cfg)?;
        format!(" if {cond}")
    } else {
        String::new()
    };

    if cfg.python_range {
        Ok(format!("[{body}{for_clause}{filter_clause}]"))
    } else {
        Ok(format!("{{{body}{for_clause}{filter_clause}}}"))
    }
}

/// Try to unroll an array comprehension for C targets.
/// Returns the unrolled expression if the range is statically known,
/// or Err if unrolling is not possible.
fn try_unroll_c_comprehension(
    array_comp: &Value,
    cfg: &ExprConfig,
) -> Result<String, minijinja::Error> {
    let indices = get_field(array_comp, "indices")?;
    let index = indices.get_item(&Value::from(0))?;
    let var_name = get_field(&index, "name")
        .map(|v| v.to_string())
        .unwrap_or_else(|_| "i".to_string());

    // Get the range and try to extract integer bounds
    let range_val = get_field(&index, "range")?;
    let range_str = render_expression(&range_val, cfg)?;
    let parts: Vec<&str> = range_str.splitn(2, ':').collect();
    if parts.len() != 2 {
        return Err(render_err("cannot unroll: non-simple range"));
    }
    let start: i64 = parts[0]
        .trim()
        .parse()
        .map_err(|_| render_err("cannot unroll: non-integer start"))?;
    let end: i64 = parts[1]
        .trim()
        .parse()
        .map_err(|_| render_err("cannot unroll: non-integer end"))?;

    // Empty range
    if start > end {
        return Ok("REAL_C(0.0)".to_string());
    }

    // Get the body expression node (not yet rendered — we need to re-render per iteration)
    let body_node = get_field(array_comp, "expr")?;

    let mut elements = Vec::new();
    for val in start..=end {
        let mut iter_cfg = cfg.clone();
        iter_cfg
            .substitutions
            .push((var_name.clone(), val.to_string()));
        elements.push(render_expression(&body_node, &iter_cfg)?);
    }

    Ok(format!(
        "{}{}{}",
        cfg.array_start,
        elements.join(", "),
        cfg.array_end
    ))
}

/// Render an index expression as `base[subscripts]`.
/// For C targets (subscript_underscore=true), bracket subscripts are 0-based
/// since they access via pointer/array (unlike VarRef underscore subscripts
/// which are 1-based naming).
fn render_index(index: &Value, cfg: &ExprConfig) -> RenderResult {
    let base_value =
        get_field(index, "base").map_err(|_| render_err("Index missing 'base' field"))?;
    let base = render_expression(&base_value, cfg).map_err(|err| {
        render_err(format!(
            "Index base render failed: {err}; base: {base_value}"
        ))
    })?;
    let subs = get_field(index, "subscripts")
        .map_err(|_| render_err("Index missing 'subscripts' field"))?;
    let len = subs.len().unwrap_or(0);
    let mut sub_strs = Vec::new();
    // For bracket-style Index access on C targets, use 0-based subscripts
    let index_cfg = if cfg.subscript_underscore {
        ExprConfig {
            one_based_index: false,
            subscript_underscore: false, // don't trigger 1-based override
            ..cfg.clone()
        }
    } else {
        cfg.clone()
    };
    for i in 0..len {
        if let Ok(sub) = subs.get_item(&Value::from(i)) {
            sub_strs.push(render_subscript(&sub, &index_cfg)?);
        }
    }
    Ok(format!("{}[{}]", base, sub_strs.join(", ")))
}

/// Render a field access expression as `base.field`.
fn render_field_access(fa: &Value, cfg: &ExprConfig) -> RenderResult {
    if let Some(symbol) = render_indexed_field_symbol(fa, cfg)? {
        return Ok(symbol);
    }
    let base_value =
        get_field(fa, "base").map_err(|_| render_err("FieldAccess missing 'base' field"))?;
    let base = render_expression(&base_value, cfg).map_err(|err| {
        render_err(format!(
            "FieldAccess base render failed: {err}; base: {base_value}"
        ))
    })?;
    let field = get_field(fa, "field")
        .map(|v| v.to_string())
        .map_err(|_| render_err("FieldAccess missing 'field'"))?;
    Ok(format!("{base}.{field}"))
}

fn render_indexed_field_symbol(
    fa: &Value,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Ok(base) = get_field(fa, "base") else {
        return no_expr_render_match();
    };
    let Ok(index) = get_field(&base, "Index") else {
        return no_expr_render_match();
    };
    let Ok(index_base) = get_field(&index, "base") else {
        return no_expr_render_match();
    };
    let Ok(var_ref) = get_field(&index_base, "VarRef") else {
        return no_expr_render_match();
    };
    let raw_name = render_name_field(&var_ref, "name", "indexed FieldAccess base")?;
    let Ok(subscripts) = get_field(&index, "subscripts") else {
        return no_expr_render_match();
    };
    let Some(len) = subscripts.len() else {
        return no_expr_render_match();
    };
    let mut values = Vec::with_capacity(len);
    for i in 0..len {
        let sub = subscripts.get_item(&Value::from(i))?;
        let Some(value) = static_subscript_value(&sub)? else {
            return no_expr_render_match();
        };
        values.push(value.to_string());
    }
    let field = get_field(fa, "field")?.to_string();
    let source_ref = format!("{raw_name}[{}].{field}", values.join(","));
    Ok(super::lookup_symbol_value(
        cfg.symbols.as_ref(),
        &source_ref,
    ))
}

fn static_subscript_value(sub: &Value) -> Result<Option<i64>, minijinja::Error> {
    if let Ok(idx) = get_field(sub, "Index") {
        return subscript_index_value(&idx).map(Some);
    }
    if let Ok(expr) = get_field(sub, "Expr") {
        let expr = get_field(&expr, "expr").unwrap_or(expr);
        return static_integer_expr_value(&expr);
    }
    no_expr_render_match()
}

fn static_integer_expr_value(expr: &Value) -> Result<Option<i64>, minijinja::Error> {
    if let Ok(literal) = get_field(expr, "Literal") {
        let literal_value = get_field(&literal, "value").unwrap_or(literal);
        if let Ok(integer) = get_field(&literal_value, "Integer") {
            return integer
                .as_i64()
                .ok_or_else(|| render_err("integer literal is not an i64"))
                .map(Some);
        }
    }
    if let Ok(binary) = get_field(expr, "Binary") {
        return static_binary_integer_expr_value(&binary);
    }
    no_expr_render_match()
}

fn static_binary_integer_expr_value(binary: &Value) -> Result<Option<i64>, minijinja::Error> {
    let lhs = get_field(binary, "lhs")?;
    let rhs = get_field(binary, "rhs")?;
    let Some(lhs) = static_integer_expr_value(&lhs)? else {
        return no_expr_render_match();
    };
    let Some(rhs) = static_integer_expr_value(&rhs)? else {
        return no_expr_render_match();
    };
    let op = get_field(binary, "op")?;
    if is_variant(&op, "Add") || is_variant(&op, "AddElem") {
        return Ok(Some(lhs + rhs));
    }
    if is_variant(&op, "Sub") || is_variant(&op, "SubElem") {
        return Ok(Some(lhs - rhs));
    }
    no_expr_render_match()
}

fn no_expr_render_match<T>() -> Result<Option<T>, minijinja::Error> {
    Ok(Option::None)
}

#[cfg(test)]
mod tests {
    use super::{render_c_float_literal, render_expression};
    use crate::codegen::ExprConfig;
    use minijinja::Value;

    #[test]
    fn test_render_c_float_literal_preserves_scientific_notation() {
        assert_eq!(render_c_float_literal("1e-6"), "1e-6f");
        assert_eq!(render_c_float_literal("1E-6"), "1E-6f");
    }

    #[test]
    fn test_render_c_float_literal_adds_fraction_to_integer_text() {
        assert_eq!(render_c_float_literal("1"), "1.0f");
        assert_eq!(render_c_float_literal("1.25"), "1.25f");
    }

    #[test]
    fn test_render_expr_can_inline_condition_aliases_at_backend_boundary() {
        let condition_ref = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::generated_component(
                "c",
                Vec::new(),
                rumoca_core::Span::DUMMY,
            ),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                1,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        };
        let relation = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Lt,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::generated_component(
                    "time",
                    Vec::new(),
                    rumoca_core::Span::DUMMY,
                ),
                subscripts: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        };
        let aliases = serde_json::json!([
            {
                "condition": {
                    "VarRef": {
                        "name": {
                            "name": "c[1]",
                            "generated": true
                        }
                    }
                },
                "relation": relation
            }
        ]);
        let cfg = ExprConfig {
            condition_aliases: Some(Value::from_serialize(aliases)),
            ..ExprConfig::default()
        };

        let rendered = render_expression(&Value::from_serialize(&condition_ref), &cfg).unwrap();
        assert_eq!(rendered, "(time < 1)");
    }
}
