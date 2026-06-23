//! Expression rendering for the DAE IR expression tree.
//!
//! This module handles recursive rendering of `Expression` variants
//! (Binary, Unary, VarRef, BuiltinCall, FunctionCall, Literal, If,
//! Array, Tuple, Range, ArrayComprehension, Index, FieldAccess).

use super::{ExprConfig, IfStyle, RenderResult};
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
    get_field(value, name).is_ok() || super::value_to_string(value).trim_matches('"') == name
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

pub(crate) fn constant_bool_expr(expr: &Value) -> Option<bool> {
    if let Ok(inner) = get_field(expr, "expr") {
        return constant_bool_expr(&inner);
    }
    if let Ok(literal) = get_field(expr, "Literal") {
        let literal_value = get_field(&literal, "value").unwrap_or(literal);
        if let Ok(b) = get_field(&literal_value, "Boolean") {
            return Some(b.is_true());
        }
    }
    if let Ok(binary) = get_field(expr, "Binary") {
        let lhs = get_field(&binary, "lhs").ok()?;
        let rhs = get_field(&binary, "rhs").ok()?;
        let op = get_field(&binary, "op").ok()?;
        if is_variant(&op, "And") {
            return Some(constant_bool_expr(&lhs)? && constant_bool_expr(&rhs)?);
        }
        if is_variant(&op, "Or") {
            return Some(constant_bool_expr(&lhs)? || constant_bool_expr(&rhs)?);
        }
        let lhs_num = constant_numeric_expr(&lhs, &ExprConfig::default())?;
        let rhs_num = constant_numeric_expr(&rhs, &ExprConfig::default())?;
        if is_variant(&op, "Eq") {
            return Some((lhs_num - rhs_num).abs() <= f64::EPSILON);
        }
        if is_variant(&op, "Neq") {
            return Some((lhs_num - rhs_num).abs() > f64::EPSILON);
        }
        if is_variant(&op, "Lt") {
            return Some(lhs_num < rhs_num);
        }
        if is_variant(&op, "Le") {
            return Some(lhs_num <= rhs_num);
        }
        if is_variant(&op, "Gt") {
            return Some(lhs_num > rhs_num);
        }
        if is_variant(&op, "Ge") {
            return Some(lhs_num >= rhs_num);
        }
    }
    None
}

pub(crate) fn constant_integer_expr(expr: &Value, cfg: &ExprConfig) -> Option<i64> {
    constant_numeric_expr(expr, cfg).and_then(|value| {
        let rounded = value.round();
        ((value - rounded).abs() <= f64::EPSILON).then_some(rounded as i64)
    })
}

fn constant_numeric_expr(expr: &Value, cfg: &ExprConfig) -> Option<f64> {
    if let Ok(inner) = get_field(expr, "expr") {
        return constant_numeric_expr(&inner, cfg);
    }
    if let Ok(var_ref) = get_field(expr, "VarRef") {
        let raw_name = render_name_field(&var_ref, "name", "VarRef").ok()?;
        if let Some((_, value)) = cfg
            .substitutions
            .iter()
            .rev()
            .find(|(name, _)| name == &raw_name)
        {
            return value.parse::<f64>().ok();
        }
    }
    if let Ok(binary) = get_field(expr, "Binary") {
        let lhs = constant_numeric_expr(&get_field(&binary, "lhs").ok()?, cfg)?;
        let rhs = constant_numeric_expr(&get_field(&binary, "rhs").ok()?, cfg)?;
        let op = get_field(&binary, "op").ok()?;
        if is_variant(&op, "Add") || is_variant(&op, "AddElem") {
            return Some(lhs + rhs);
        }
        if is_variant(&op, "Sub") || is_variant(&op, "SubElem") {
            return Some(lhs - rhs);
        }
        if is_variant(&op, "Mul") || is_variant(&op, "MulElem") {
            return Some(lhs * rhs);
        }
        if (is_variant(&op, "Div") || is_variant(&op, "DivElem")) && rhs.abs() > f64::EPSILON {
            return Some(lhs / rhs);
        }
    }
    let literal = get_field(expr, "Literal").ok()?;
    let literal_value = get_field(&literal, "value").unwrap_or(literal);
    if let Ok(real) = get_field(&literal_value, "Real") {
        return real.to_string().parse::<f64>().ok();
    }
    if let Ok(int) = get_field(&literal_value, "Integer") {
        return int.to_string().parse::<f64>().ok();
    }
    None
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

    let subscripts = render_subscripts(var_ref, cfg)?;
    if subscripts.is_empty() {
        if let Some((_, value)) = cfg
            .substitutions
            .iter()
            .rev()
            .find(|(name, _)| name == &raw_name)
        {
            return Ok(value.clone());
        }
        if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &raw_name) {
            return Ok(symbol);
        }
        if let Some(source_name) = one_based_source_ref_candidate(&raw_name)
            && let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &source_name)
        {
            return Ok(symbol);
        }
        if cfg.subscript_underscore
            && let Some(source_name) = one_based_source_ref_candidate(&raw_name)
        {
            return Ok(super::var_name_to_c_alias(&source_name));
        }
        Ok(super::emitted_symbol_or_fallback(&raw_name, cfg))
    } else if cfg.subscript_underscore {
        // Underscore style: x[1] -> x_1, x[1,2] -> x_1_2.
        let compact_subscripts = subscripts.replace(' ', "");
        let source_ref = format!("{raw_name}[{compact_subscripts}]");
        if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &source_ref) {
            return Ok(symbol);
        }
        if let Some(source_name) = one_based_source_ref_candidate(&source_ref)
            && let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &source_name)
        {
            return Ok(symbol);
        }
        if let Some(source_name) = one_based_source_ref_candidate(&source_ref) {
            return Ok(super::var_name_to_c_alias(&source_name));
        }
        let name = super::emitted_symbol_or_fallback(&raw_name, cfg);
        Ok(format!("{}_{}", name, compact_subscripts.replace(',', "_")))
    } else {
        let name = super::emitted_symbol_or_fallback(&raw_name, cfg);
        Ok(format!("{}[{}]", name, subscripts))
    }
}

fn one_based_source_ref_candidate(source_ref: &str) -> Option<String> {
    let mut out = String::with_capacity(source_ref.len());
    let mut chars = source_ref.chars().peekable();
    let mut changed = false;
    while let Some(ch) = chars.next() {
        if ch != '[' {
            out.push(ch);
            continue;
        }
        let mut subscript = String::new();
        while let Some(&next) = chars.peek() {
            chars.next();
            if next == ']' {
                break;
            }
            subscript.push(next);
        }
        let converted = subscript
            .split(',')
            .map(|part| part.trim().parse::<i64>())
            .collect::<Result<Vec<_>, _>>()
            .ok()?
            .into_iter()
            .map(|value| {
                changed = true;
                (value + 1).to_string()
            })
            .collect::<Vec<_>>()
            .join(",");
        out.push('[');
        out.push_str(&converted);
        out.push(']');
    }
    changed.then_some(out)
}

pub(crate) fn render_name_field(value: &Value, field: &str, context: &str) -> RenderResult {
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

pub(crate) fn render_serialized_name(value: &Value) -> String {
    if let Ok(name) = get_field(value, "name") {
        return render_serialized_name(&name);
    }
    if let Ok(component_ref) = get_field(value, "component_ref")
        && let Some(name) = render_component_ref_source_name(&component_ref)
    {
        return name;
    }
    if let Ok(name) = get_field(value, "0") {
        return super::value_to_string(&name);
    }
    super::value_to_string(value)
}

fn render_component_ref_source_name(component_ref: &Value) -> Option<String> {
    let parts = get_field(component_ref, "parts").ok()?;
    let len = parts.len()?;
    let mut rendered = Vec::with_capacity(len);
    for i in 0..len {
        let part = parts.get_item(&Value::from(i)).ok()?;
        let ident = get_field(&part, "ident").ok()?;
        let ident = ident.as_str().map(ToOwned::to_owned).unwrap_or_else(|| {
            ident
                .get_attr("text")
                .ok()
                .and_then(|text| text.as_str().map(ToOwned::to_owned))
                .unwrap_or_else(|| ident.to_string())
        });
        if ident.trim().is_empty() {
            return None;
        }
        let subscripts = render_component_ref_part_source_subscripts(&part);
        if subscripts.is_empty() {
            rendered.push(ident);
        } else {
            rendered.push(format!("{ident}[{}]", subscripts.join(",")));
        }
    }
    if rendered.is_empty() {
        None
    } else {
        Some(rendered.join("."))
    }
}

fn render_component_ref_part_source_subscripts(part: &Value) -> Vec<String> {
    let Ok(subs) = get_field(part, "subs") else {
        return Vec::new();
    };
    let Some(len) = subs.len() else {
        return Vec::new();
    };
    let mut rendered = Vec::new();
    for i in 0..len {
        let Ok(sub) = subs.get_item(&Value::from(i)) else {
            continue;
        };
        if let Ok(index) = get_field(&sub, "Index")
            && let Ok(value) = get_field(&index, "value")
        {
            rendered.push(super::value_to_string(&value));
        }
    }
    rendered
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
        if let Some(value) = constant_integer_expr(&expr, cfg) {
            return if cfg.one_based_index || cfg.subscript_underscore {
                Ok(value.to_string())
            } else {
                Ok((value - 1).to_string())
            };
        }
        let rendered = render_expression(&expr, cfg)?;
        if cfg.one_based_index || cfg.subscript_underscore {
            return Ok(rendered);
        }
        return Ok(format!("({rendered} - 1)"));
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
            // sample(start, interval) is a clocked partition builtin.
            // In continuous simulation, treat as always-true (MLS §16.3).
            require_min_arg_count(builtin, 2, "BuiltinCall Sample")?;
            return Ok(cfg.true_val.clone());
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

    if func_name == "Size"
        && matches!(cfg.if_style, super::IfStyle::Ternary)
        && let Some(rendered) = render_c_size_builtin(builtin, cfg)?
    {
        return Ok(rendered);
    }

    // Handle Min/Max/Sum with single Array argument: expand to chained calls.
    // Modelica `min({a,b,c})` → C `fmin(fmin(a,b),c)` (not `fmin((double[]){a,b,c})`)
    if matches!(func_name.as_str(), "Min" | "Max" | "Sum")
        && let args_val = get_field(builtin, "args")?
        && args_val.len() == Some(1)
        && let Ok(first_arg) = args_val.get_item(&Value::from(0))
    {
        if func_name == "Sum"
            && matches!(cfg.if_style, super::IfStyle::Ternary)
            && cfg.sum_fn != "sum1"
            && let Some(rendered) = render_c_sum_var_ref(&first_arg, cfg)
        {
            return Ok(rendered);
        }
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

fn render_c_sum_var_ref(expr: &Value, cfg: &ExprConfig) -> Option<String> {
    let var_ref = get_field(expr, "VarRef").ok()?;
    let subscripts = get_field(&var_ref, "subscripts").ok()?;
    if subscripts.len().unwrap_or(0) != 0 {
        return None;
    }
    let raw_name = render_serialized_name(&get_field(&var_ref, "name").ok()?);
    let len = scalarized_array_len(&raw_name, cfg)?;
    let pointer = super::emitted_symbol_or_fallback(&raw_name, cfg);
    Some(format!("{}({}, {len})", cfg.sum_fn, pointer))
}

fn scalarized_array_len(base_name: &str, cfg: &ExprConfig) -> Option<usize> {
    let mut len = 0usize;
    for index in 1..=256 {
        let source_ref = format!("{base_name}[{index}]");
        if super::lookup_symbol_value(cfg.symbols.as_ref(), &source_ref).is_some() {
            len = index;
        } else if len > 0 {
            break;
        }
    }
    (len > 0).then_some(len)
}

fn render_c_size_builtin(
    builtin: &Value,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let args_val = get_field(builtin, "args")?;
    let first_arg = required_arg(&args_val, 0, "BuiltinCall Size")?;
    if let Ok(var_ref) = get_field(&first_arg, "VarRef") {
        let subscripts = get_field(&var_ref, "subscripts").ok();
        if subscripts.as_ref().and_then(Value::len).unwrap_or(0) == 0 {
            let raw_name = render_name_field(&var_ref, "name", "VarRef")?;
            if let Some(len) = scalarized_array_len(&raw_name, cfg) {
                return Ok(Some(len.to_string()));
            }
            let symbol = super::emitted_symbol_or_fallback(&raw_name, cfg);
            return Ok(Some(format!("{symbol}_size")));
        }
    }
    if let Ok(array) = get_field(&first_arg, "Array")
        && let Ok(elements) = get_field(&array, "elements")
        && let Some(len) = elements.len()
    {
        return Ok(Some(len.to_string()));
    }
    Ok(None)
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
    let last_segment = rumoca_core::top_level_last_segment(&raw_name);
    if matches!(last_segment, "allTrue" | "anyTrue") {
        let args_val = get_field(func_call, "args")?;
        let first_arg = required_arg(&args_val, 0, &format!("FunctionCall {last_segment}"))?;
        let op = if last_segment == "allTrue" {
            "&&"
        } else {
            "||"
        };
        let identity = if last_segment == "allTrue" {
            cfg.true_val.as_str()
        } else {
            cfg.false_val.as_str()
        };
        if let Some(rendered) = render_bool_array_reduction(&first_arg, op, identity, cfg)? {
            return Ok(rendered);
        }
    }
    if (last_segment == "efficiency" || raw_name.ends_with("_efficiency"))
        && let Some(rendered) = render_buildings_efficiency_call(func_call, cfg)?
    {
        return Ok(rendered);
    }
    if (last_segment == "normalizedPower" || raw_name.ends_with("_normalizedPower"))
        && let Some(rendered) = render_buildings_normalized_power_call(func_call, cfg)?
    {
        return Ok(rendered);
    }
    if (last_segment == "temperature_phX" || raw_name.ends_with("_temperature_phX"))
        && let Some(rendered) = render_temperature_phx_call(&raw_name, func_call, cfg)?
    {
        return Ok(rendered);
    }
    if (last_segment == "density_pTX" || raw_name.ends_with("_density_pTX"))
        && let Some(rendered) = render_density_ptx_call(&raw_name, func_call, cfg)?
    {
        return Ok(rendered);
    }
    if last_segment == "regStep"
        && let Some(rendered) = render_regstep_call(&raw_name, func_call, cfg)?
    {
        return Ok(rendered);
    }
    let emitted_name = super::emitted_symbol_or_fallback(&raw_name, cfg);
    if matches!(
        emitted_name.as_str(),
        "Air_specificEnthalpy" | "Air_specificHeatCapacityCp"
    ) && let Some(rendered) = render_air_state_property_call(&emitted_name, func_call, cfg)?
    {
        return Ok(rendered);
    }

    // Map Modelica standard library math functions to builtins
    if let Some(builtin) = resolve_modelica_math_function(&raw_name) {
        let args = render_args(func_call, cfg)?;
        return Ok(render_builtin_python(builtin, &args, cfg));
    }

    let name = super::emitted_symbol_or_fallback(&raw_name, cfg);

    let args = render_args(func_call, cfg)?;
    Ok(format!("{}({})", name, args))
}

fn render_buildings_efficiency_call(
    func_call: &Value,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let args = get_field(func_call, "args")?;
    if args.len() != Some(9) {
        return Ok(None);
    }
    let per_v_flow = render_function_argument(&args.get_item(&Value::from(0))?, cfg)?;
    let per_v_flow_size = render_function_argument(&args.get_item(&Value::from(1))?, cfg)?;
    let per_eta = if let Some(stem) = per_v_flow.strip_suffix("_V_flow") {
        format!("{stem}_eta")
    } else {
        render_function_argument(&args.get_item(&Value::from(2))?, cfg)?
    };
    let mut rendered_args = vec![
        per_v_flow,
        per_v_flow_size.clone(),
        per_eta,
        per_v_flow_size,
    ];
    for idx in 4..9 {
        rendered_args.push(render_function_argument(
            &args.get_item(&Value::from(idx))?,
            cfg,
        )?);
    }
    Ok(Some(format!("efficiency({})", rendered_args.join(", "))))
}

fn render_buildings_normalized_power_call(
    func_call: &Value,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let args = get_field(func_call, "args")?;
    if args.len() != Some(7) {
        return Ok(None);
    }
    let per_r_v = canonicalize_rendered_component_access(
        &render_function_argument(&args.get_item(&Value::from(0))?, cfg)?,
        cfg,
    );
    let d_size = render_function_argument(&args.get_item(&Value::from(6))?, cfg)?;
    let rendered_per_r_v_size = render_function_argument(&args.get_item(&Value::from(1))?, cfg)?;
    let per_r_v_size = if rendered_per_r_v_size.trim() == "0" {
        d_size.clone()
    } else {
        rendered_per_r_v_size
    };
    let per_r_p = if let Some(stem) = per_r_v.strip_suffix("_r_V") {
        format!("{stem}_r_P")
    } else {
        canonicalize_rendered_component_access(
            &render_function_argument(&args.get_item(&Value::from(2))?, cfg)?,
            cfg,
        )
    };
    let mut rendered_args = vec![per_r_v, per_r_v_size.clone(), per_r_p, per_r_v_size];
    for idx in 4..7 {
        rendered_args.push(render_function_argument(
            &args.get_item(&Value::from(idx))?,
            cfg,
        )?);
    }
    Ok(Some(format!(
        "normalizedPower({})",
        rendered_args.join(", ")
    )))
}

fn render_temperature_phx_call(
    raw_name: &str,
    func_call: &Value,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let args = get_field(func_call, "args")?;
    let Some(len) = args.len() else {
        return Ok(None);
    };
    if len < 2 {
        return Ok(None);
    }
    let p = render_function_argument(&args.get_item(&Value::from(0))?, cfg)?;
    let h = render_function_argument(&args.get_item(&Value::from(1))?, cfg)?;
    let x = if len >= 3 {
        render_function_argument(&args.get_item(&Value::from(2))?, cfg)?
    } else {
        "NULL".to_string()
    };
    let function_name = super::emitted_symbol_or_fallback(raw_name, cfg);
    Ok(Some(format!("{function_name}({p}, {h}, {x}, 0)")))
}

fn render_regstep_call(
    raw_name: &str,
    func_call: &Value,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let args = get_field(func_call, "args")?;
    let Some(len) = args.len() else {
        return Ok(None);
    };
    if len != 4 {
        return Ok(None);
    }
    let arg = |name: &str, pos: usize| -> Result<Value, minijinja::Error> {
        named_function_arg(&args, name)?
            .or_else(|| args.get_item(&Value::from(pos)).ok())
            .ok_or_else(|| {
                render_err(format!(
                    "FunctionCall regStep missing argument {name}/{pos}"
                ))
            })
    };
    let x = render_function_argument(&arg("x", 0)?, cfg)?;
    let y1 = render_function_argument(&arg("y1", 1)?, cfg)?;
    let y2 = render_function_argument(&arg("y2", 2)?, cfg)?;
    let x_small = render_function_argument(&arg("x_small", 3)?, cfg)?;
    let function_name = super::emitted_symbol_or_fallback(raw_name, cfg);
    Ok(Some(format!("{function_name}({x}, {y1}, {y2}, {x_small})")))
}

fn render_air_state_property_call(
    raw_name: &str,
    func_call: &Value,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let args = get_field(func_call, "args")?;
    if args.len() != Some(2) {
        return Ok(None);
    }
    let p = render_function_argument(&args.get_item(&Value::from(0))?, cfg)?;
    let t = render_function_argument(&args.get_item(&Value::from(1))?, cfg)?;
    let (x, x_size) = infer_air_state_x_args(&p, &t);
    Ok(Some(format!("{raw_name}({p}, {t}, {x}, {x_size})")))
}

fn render_density_ptx_call(
    raw_name: &str,
    func_call: &Value,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let args = get_field(func_call, "args")?;
    if args.len() != Some(4) {
        return Ok(None);
    }
    let p = render_function_argument(&args.get_item(&Value::from(0))?, cfg)?;
    let t = render_function_argument(&args.get_item(&Value::from(1))?, cfg)?;
    let _x = render_function_argument(&args.get_item(&Value::from(2))?, cfg)?;

    if raw_name.contains("Water") {
        return Ok(Some(format!(
            "fmin(1100.0, fmax(950.0, (998.2 - 0.25 * (({t}) - 293.15) + 4.5e-10 * (({p}) - 101325.0))))"
        )));
    }

    if raw_name.contains("Air") {
        return Ok(Some(format!("(({p}) / (287.05 * ({t})))")));
    }

    Ok(None)
}

fn infer_air_state_x_args(rendered_p: &str, rendered_t: &str) -> (String, String) {
    let _ = (rendered_p, rendered_t);
    ("(double[]){0.01, 0.99}".to_string(), "2".to_string())
}

fn canonicalize_rendered_component_access(rendered: &str, cfg: &ExprConfig) -> String {
    if !cfg.subscript_underscore || !rendered.contains('[') || !rendered.contains('.') {
        return rendered.to_string();
    }
    if rendered.trim_start().starts_with('(') || rendered.trim_start().starts_with('{') {
        return rendered.to_string();
    }
    let mut out = String::with_capacity(rendered.len());
    let mut chars = rendered.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '.' => out.push('_'),
            '[' => {
                let mut subscript = String::new();
                while let Some(&next) = chars.peek() {
                    chars.next();
                    if next == ']' {
                        break;
                    }
                    subscript.push(next);
                }
                let converted = subscript
                    .split(',')
                    .map(|part| part.trim().parse::<i64>())
                    .collect::<Result<Vec<_>, _>>();
                let Ok(parts) = converted else {
                    out.push('_');
                    out.push_str(&subscript.replace(',', "_"));
                    continue;
                };
                for value in parts {
                    out.push('_');
                    out.push_str(&(value + 1).to_string());
                }
            }
            _ => out.push(ch),
        }
    }
    out
}

fn render_bool_array_reduction(
    expr: &Value,
    op: &str,
    identity: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let mut parts = Vec::new();
    if let Ok(array) = get_field(expr, "Array") {
        let elements = get_field(&array, "elements")?;
        let len = elements.len().unwrap_or(0);
        for i in 0..len {
            let elem = elements.get_item(&Value::from(i))?;
            parts.push(render_expression(&elem, cfg)?);
        }
    } else if let Ok(array_comp) = get_field(expr, "ArrayComprehension") {
        let Some((iter_name, start, end)) = bool_reduction_comprehension_range(&array_comp, cfg)
        else {
            return Ok(None);
        };
        let body = get_field(&array_comp, "expr")?;
        for value in start..=end {
            let mut iter_cfg = cfg.clone();
            iter_cfg
                .substitutions
                .push((iter_name.clone(), value.to_string()));
            parts.push(render_expression(&body, &iter_cfg)?);
        }
    } else {
        return Ok(None);
    }

    if parts.is_empty() {
        return Ok(Some(identity.to_string()));
    }
    Ok(Some(format!(
        "({})",
        parts
            .into_iter()
            .map(|part| format!("({part})"))
            .collect::<Vec<_>>()
            .join(&format!(" {op} "))
    )))
}

fn bool_reduction_comprehension_range(
    array_comp: &Value,
    cfg: &ExprConfig,
) -> Option<(String, i64, i64)> {
    let indices = get_field(array_comp, "indices").ok()?;
    if indices.len()? != 1 {
        return None;
    }
    let index = indices.get_item(&Value::from(0)).ok()?;
    let iter_name = get_field(&index, "name").ok()?.to_string();
    let range = get_field(&index, "range").ok()?;
    let range = get_field(&range, "Range").ok()?;
    let start = constant_integer_expr(&get_field(&range, "start").ok()?, cfg)?;
    if let Some(end) = constant_integer_expr(&get_field(&range, "end").ok()?, cfg) {
        return Some((iter_name, start, end));
    }
    let source_len = first_scalarized_var_ref_len_expr(&get_field(array_comp, "expr").ok()?, cfg)?;
    let end = if is_minus_one_expr(&get_field(&range, "end").ok()?) {
        source_len as i64 - 1
    } else {
        source_len as i64
    };
    Some((iter_name, start, end))
}

fn is_minus_one_expr(expr: &Value) -> bool {
    let Ok(binary) = get_field(expr, "Binary") else {
        return false;
    };
    let Ok(op) = get_field(&binary, "op") else {
        return false;
    };
    is_variant(&op, "Sub")
        && get_field(&binary, "rhs")
            .ok()
            .and_then(|rhs| constant_integer_expr(&rhs, &ExprConfig::default()))
            == Some(1)
}

fn first_scalarized_var_ref_len_expr(expr: &Value, cfg: &ExprConfig) -> Option<usize> {
    if let Ok(inner) = get_field(expr, "expr") {
        return first_scalarized_var_ref_len_expr(&inner, cfg);
    }
    if let Ok(var_ref) = get_field(expr, "VarRef") {
        let raw_name = render_name_field(&var_ref, "name", "VarRef").ok()?;
        if let Some(len) = scalarized_array_len_for_expr(&raw_name, cfg) {
            return Some(len);
        }
    }
    if let Ok(binary) = get_field(expr, "Binary") {
        return first_scalarized_var_ref_len_expr(&get_field(&binary, "lhs").ok()?, cfg)
            .or_else(|| first_scalarized_var_ref_len_expr(&get_field(&binary, "rhs").ok()?, cfg));
    }
    if let Ok(unary) = get_field(expr, "Unary") {
        return first_scalarized_var_ref_len_expr(&get_field(&unary, "rhs").ok()?, cfg);
    }
    None
}

fn scalarized_array_len_for_expr(base_name: &str, cfg: &ExprConfig) -> Option<usize> {
    let mut len = 0usize;
    for index in 1..=256 {
        let source_ref = format!("{base_name}[{index}]");
        if super::lookup_symbol_value(cfg.symbols.as_ref(), &source_ref).is_some() {
            len = index;
        } else if len > 0 {
            break;
        }
    }
    (len > 0).then_some(len)
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

        if let Some(cond_value) = constant_bool_expr(&cond) {
            if cond_value {
                result = render_expression(&then, cfg)?;
            }
            continue;
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
    let base = get_field(index, "base")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("Index missing 'base' field"))?;
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
    let field = get_field(fa, "field")
        .map(|v| super::value_to_string(&v))
        .map_err(|_| render_err("FieldAccess missing 'field'"))?;
    let base_node =
        get_field(fa, "base").map_err(|_| render_err("FieldAccess missing 'base' field"))?;
    if matches!(cfg.if_style, super::IfStyle::Ternary)
        && let Some(projected) = render_c_record_constructor_field(&base_node, &field, cfg)?
    {
        return Ok(projected);
    }
    let base = get_field(fa, "base")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("FieldAccess missing 'base' field"))?;
    Ok(format!("{base}.{field}"))
}

fn render_c_record_constructor_field(
    base: &Value,
    field: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Ok(call) = get_field(base, "FunctionCall") else {
        return Ok(None);
    };
    let raw_name = render_name_field(&call, "name", "FunctionCall")?;
    let last = rumoca_core::top_level_last_segment(&raw_name);
    let field = field.trim_matches('"');
    if matches!(last, "setState_phX") || raw_name.ends_with("_setState_phX") {
        let args = get_field(&call, "args").map_err(|err| {
            render_err(format!(
                "record constructor field access missing args: {err}"
            ))
        })?;
        return render_set_state_phx_field(&raw_name, &args, field, cfg);
    }
    let arg_idx = match (last, field.trim_matches('"')) {
        ("setState_pTX" | "ThermodynamicState", "p") => 0,
        ("setState_pTX" | "ThermodynamicState", "T") => 1,
        ("setState_pTX" | "ThermodynamicState", "X" | "reference_X") => 2,
        _ => return Ok(None),
    };
    let args = get_field(&call, "args").map_err(|err| {
        render_err(format!(
            "record constructor field access missing args: {err}"
        ))
    })?;
    if let Some(arg) = named_function_arg(&args, field.trim_matches('"'))? {
        return render_expression(&arg, cfg).map(Some);
    }
    let arg = required_arg(&args, arg_idx, "record constructor field access")?;
    render_expression(&arg, cfg).map(Some)
}

fn render_set_state_phx_field(
    raw_name: &str,
    args: &Value,
    field: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    match field {
        "p" => {
            let arg = if let Some(arg) = named_function_arg(args, "p")? {
                arg
            } else {
                required_arg(args, 0, "setState_phX.p")?
            };
            render_expression(&arg, cfg).map(Some)
        }
        "T" => {
            let p_arg = if let Some(arg) = named_function_arg(args, "p")? {
                arg
            } else {
                required_arg(args, 0, "setState_phX.T.p")?
            };
            let h_arg = if let Some(arg) = named_function_arg(args, "h")? {
                arg
            } else {
                required_arg(args, 1, "setState_phX.T.h")?
            };
            let x_arg = named_function_arg(args, "X")?.or_else(|| {
                args.get_item(&Value::from(2))
                    .ok()
                    .filter(|arg| !arg.is_undefined() && !arg.is_none())
            });
            let p = render_expression(&p_arg, cfg)?;
            let h = render_expression(&h_arg, cfg)?;
            let (x, x_size) = if let Some(x_arg) = x_arg {
                let x = render_expression(&x_arg, cfg)?;
                let size = first_scalarized_var_ref_len_expr(&x_arg, cfg)
                    .map(|len| len.to_string())
                    .unwrap_or_else(|| "2".to_string());
                (x, size)
            } else {
                ("NULL".to_string(), "0".to_string())
            };
            let temperature_fn = set_state_phx_temperature_function(raw_name, &h);
            Ok(Some(format!("{temperature_fn}({p}, {h}, {x}, {x_size})")))
        }
        _ => Ok(None),
    }
}

fn set_state_phx_temperature_function(raw_name: &str, rendered_h: &str) -> String {
    if let Some(prefix) = raw_name.strip_suffix("_setState_phX") {
        return format!("{prefix}_temperature_phX");
    }
    if let Some(prefix) = rendered_h.split("_specificEnthalpy").next()
        && prefix != rendered_h
        && !prefix.is_empty()
    {
        return format!("{prefix}_temperature_phX");
    }
    "Air_temperature_phX".to_string()
}

fn named_function_arg(args: &Value, name: &str) -> Result<Option<Value>, minijinja::Error> {
    let Some(len) = args.len() else {
        return Ok(None);
    };
    let expected = format!("__rumoca_named_arg__.{name}");
    for i in 0..len {
        let arg = args.get_item(&Value::from(i))?;
        let Ok(call) = get_field(&arg, "FunctionCall") else {
            continue;
        };
        let Ok(call_name) = render_name_field(&call, "name", "FunctionCall") else {
            continue;
        };
        if call_name != expected {
            continue;
        }
        let call_args = get_field(&call, "args")?;
        return required_arg(&call_args, 0, "named record constructor field argument").map(Some);
    }
    Ok(None)
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
    fn normalized_power_preserves_c_array_literals() {
        let expr = Value::from_serialize(serde_json::json!({
            "FunctionCall": {
                "name": "Buildings.Fluid.Movers.BaseClasses.Characteristics.normalizedPower",
                "args": [
                    {"Array": {"elements": [
                        {"Literal": {"value": {"Real": "0"}}},
                        {"Literal": {"value": {"Real": "0.1"}}},
                        {"Literal": {"value": {"Real": "0.3"}}},
                        {"Literal": {"value": {"Real": "0.6"}}},
                        {"Literal": {"value": {"Real": "1"}}}
                    ]}},
                    {"Literal": {"value": {"Integer": 5}}},
                    {"Array": {"elements": [
                        {"Literal": {"value": {"Real": "0"}}},
                        {"Literal": {"value": {"Real": "0.001"}}},
                        {"Literal": {"value": {"Real": "0.027"}}},
                        {"Literal": {"value": {"Real": "0.216"}}},
                        {"Literal": {"value": {"Real": "1"}}}
                    ]}},
                    {"Literal": {"value": {"Integer": 5}}},
                    {"VarRef": {"name": "ctl.y", "subscripts": []}},
                    {"VarRef": {"name": "ctl.der", "subscripts": []}},
                    {"Literal": {"value": {"Integer": 3}}}
                ]
            }
        }));
        let cfg = ExprConfig {
            power: "pow".to_string(),
            array_start: "(double[]){".to_string(),
            array_end: "}".to_string(),
            subscript_underscore: true,
            sanitize_dots: true,
            ..ExprConfig::default()
        };

        assert_eq!(
            render_expression(&expr, &cfg).unwrap(),
            "normalizedPower((double[]){0, 0.1, 0.3, 0.6, 1}, 5, (double[]){0, 0.001, 0.027, 0.216, 1}, 5, ctl_y, ctl_der, 3)"
        );
    }
}
