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

/// Recursively render an expression to a string.
pub(crate) fn render_expression(expr: &Value, cfg: &ExprConfig) -> RenderResult {
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
    // Unit variants (e.g. Empty) serialize as plain strings, not objects,
    // so get_field() won't match them — check string representation instead.
    let s = expr.to_string();
    if s == "Empty" {
        return Ok("0".to_string());
    }
    Err(render_err(format!("unhandled Expression variant: {expr}")))
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
    // looks like a function name (contains '.', e.g. "ca.logic_and").
    if get_field(&op_value, "And").is_ok() && cfg.and_op.contains('.') {
        return Ok(format!("{}({}, {})", cfg.and_op, lhs, rhs));
    }
    if get_field(&op_value, "Or").is_ok() && cfg.or_op.contains('.') {
        return Ok(format!("{}({}, {})", cfg.or_op, lhs, rhs));
    }
    let op_str = get_binop_string(&op_value, cfg)?;
    Ok(format!("({lhs} {op_str} {rhs})"))
}

pub(crate) fn is_exp_op(op: &Value) -> bool {
    get_field(op, "Exp").is_ok() || get_field(op, "ExpElem").is_ok()
}

pub(crate) fn is_mul_elem_op(op: &Value) -> bool {
    get_field(op, "MulElem").is_ok()
}

pub(crate) fn get_binop_string(op: &Value, cfg: &ExprConfig) -> RenderResult {
    if get_field(op, "Add").is_ok() || get_field(op, "AddElem").is_ok() {
        return Ok("+".to_string());
    }
    if get_field(op, "Sub").is_ok() || get_field(op, "SubElem").is_ok() {
        return Ok("-".to_string());
    }
    if get_field(op, "Mul").is_ok() || get_field(op, "MulElem").is_ok() {
        return Ok("*".to_string());
    }
    if get_field(op, "Div").is_ok() || get_field(op, "DivElem").is_ok() {
        return Ok("/".to_string());
    }
    if get_field(op, "Exp").is_ok() || get_field(op, "ExpElem").is_ok() {
        return Ok(cfg.power.clone());
    }
    if get_field(op, "And").is_ok() {
        return Ok(cfg.and_op.clone());
    }
    if get_field(op, "Or").is_ok() {
        return Ok(cfg.or_op.clone());
    }
    if get_field(op, "Lt").is_ok() {
        return Ok("<".to_string());
    }
    if get_field(op, "Le").is_ok() {
        return Ok("<=".to_string());
    }
    if get_field(op, "Gt").is_ok() {
        return Ok(">".to_string());
    }
    if get_field(op, "Ge").is_ok() {
        return Ok(">=".to_string());
    }
    if get_field(op, "Eq").is_ok() {
        return Ok("==".to_string());
    }
    if get_field(op, "Neq").is_ok() {
        return Ok("!=".to_string());
    }
    Err(render_err(format!(
        "unhandled binary operator variant: {op}"
    )))
}

fn render_unary(unary: &Value, cfg: &ExprConfig) -> RenderResult {
    let rhs_value = get_field(unary, "rhs")
        .or_else(|_| get_field(unary, "arg"))
        .map_err(|_| render_err("Unary expression missing 'rhs' field"))?;
    let rhs = render_expression(&rhs_value, cfg)
        .map_err(|_| render_err("Unary expression missing 'rhs' field"))?;
    let op =
        get_field(unary, "op").map_err(|_| render_err("Unary expression missing 'op' field"))?;
    // Use function-call form for Not when not_op is a function (contains '.').
    if get_field(&op, "Not").is_ok() && cfg.not_op.contains('.') {
        return Ok(format!("{}({})", cfg.not_op, rhs));
    }
    let op_str = get_unop_string(&op, cfg)?;
    Ok(format!("({op_str}{rhs})"))
}

pub(crate) fn get_unop_string(op: &Value, cfg: &ExprConfig) -> RenderResult {
    if let Some(op_str) = op.as_str() {
        return match op_str {
            "-" => Ok("-".to_string()),
            "+" => Ok("+".to_string()),
            "not" => Ok(cfg.not_op.clone()),
            _ => Err(render_err(format!(
                "unhandled unary operator string variant: {op_str}"
            ))),
        };
    }
    if get_field(op, "Minus").is_ok() || get_field(op, "DotMinus").is_ok() {
        return Ok("-".to_string());
    }
    if get_field(op, "Plus").is_ok() || get_field(op, "DotPlus").is_ok() {
        return Ok("+".to_string());
    }
    if get_field(op, "Not").is_ok() {
        return Ok(cfg.not_op.clone());
    }
    Err(render_err(format!(
        "unhandled unary operator variant: {op}"
    )))
}

fn render_var_ref(var_ref: &Value, cfg: &ExprConfig) -> RenderResult {
    let raw_name = get_field(var_ref, "name")
        .ok()
        .map(|n| {
            // VarName serializes as a plain string (newtype struct)
            // or as {"0": "name"} depending on serialization format
            get_field(&n, "0")
                .map(|v| v.to_string())
                .unwrap_or_else(|_| n.to_string())
        })
        .unwrap_or_default();
    let name = if cfg.sanitize_dots {
        super::sanitize_name(&raw_name)
    } else {
        super::escape_reserved_keyword(&raw_name)
    };

    let Some(subs) = get_field(var_ref, "subscripts").ok() else {
        return Ok(name);
    };
    let Some(len) = subs.len() else {
        return Ok(name);
    };
    if len == 0 {
        return Ok(name);
    }

    let all_static = (0..len).all(|i| {
        subs.get_item(&Value::from(i))
            .ok()
            .and_then(|sub| get_field(&sub, "Index").ok())
            .and_then(|idx| idx.as_i64())
            .is_some()
    });

    if cfg.subscript_underscore && all_static {
        let subscripts = render_subscripts(var_ref, cfg)?;
        // Underscore style: x[1] → x_1 (1-based, matches C template unpack_vars naming)
        Ok(format!("{}_{}", name, subscripts))
    } else if cfg.subscript_underscore {
        if len > 1 {
            return Err(render_err(format!(
                "dynamic multi-dimensional array access is not supported for C aliases: {var_ref}"
            )));
        }
        let subscripts = render_pointer_subscripts(&subs, cfg)?;
        Ok(format!("{}[{}]", name, subscripts))
    } else {
        let subscripts = render_subscripts(var_ref, cfg)?;
        Ok(format!("{}[{}]", name, subscripts))
    }
}

fn render_subscripts(var_ref: &Value, cfg: &ExprConfig) -> RenderResult {
    let Some(subs) = get_field(var_ref, "subscripts").ok() else {
        return Ok(String::new());
    };
    let Some(len) = subs.len() else {
        return Ok(String::new());
    };
    if len == 0 {
        return Ok(String::new());
    }

    let mut sub_strs = Vec::new();
    for i in 0..len {
        if let Ok(sub) = subs.get_item(&Value::from(i)) {
            sub_strs.push(render_subscript(&sub, cfg)?);
        }
    }

    Ok(sub_strs.join(", "))
}

fn render_pointer_subscripts(subs: &Value, cfg: &ExprConfig) -> RenderResult {
    let Some(len) = subs.len() else {
        return Ok(String::new());
    };
    let mut sub_strs = Vec::new();
    let index_cfg = ExprConfig {
        one_based_index: false,
        subscript_underscore: false,
        ..cfg.clone()
    };
    for i in 0..len {
        if let Ok(sub) = subs.get_item(&Value::from(i)) {
            sub_strs.push(render_pointer_subscript(&sub, &index_cfg)?);
        }
    }
    Ok(sub_strs.join(", "))
}

fn render_pointer_subscript(sub: &Value, cfg: &ExprConfig) -> RenderResult {
    if let Ok(idx) = get_field(sub, "Index") {
        let val = idx
            .as_i64()
            .ok_or_else(|| render_err("subscript Index is not an integer"))?;
        return Ok(format!("{}", val - 1));
    }
    if get_field(sub, "Colon").is_ok() {
        return Err(render_err(
            "slice subscripts are not supported in C array aliases",
        ));
    }
    if let Ok(expr) = get_field(sub, "Expr") {
        let rendered = render_expression(&expr, cfg)?;
        return Ok(format!("(({}) - 1)", rendered));
    }
    Err(render_err(format!("unhandled Subscript variant: {sub}")))
}

pub(crate) fn render_subscript(sub: &Value, cfg: &ExprConfig) -> RenderResult {
    if let Ok(idx) = get_field(sub, "Index") {
        let val = idx
            .as_i64()
            .ok_or_else(|| render_err("subscript Index is not an integer"))?;
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
        return render_expression(&expr, cfg);
    }
    Err(render_err(format!("unhandled Subscript variant: {sub}")))
}

fn render_builtin(builtin: &Value, cfg: &ExprConfig) -> RenderResult {
    let func_name = get_field(builtin, "function")
        .ok()
        .map(|f| f.to_string())
        .unwrap_or_default();

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
            if let Ok(inner) = args_val.get_item(&Value::from(idx)) {
                return render_expression(&inner, cfg);
            }
            if let Ok(inner) = args_val.get_item(&Value::from(0)) {
                return render_expression(&inner, cfg);
            }
            return Ok("0".to_string());
        }
        "Sample" => {
            // sample(start, interval) is a clocked partition builtin.
            // In continuous simulation, treat as always-true (MLS §16.3).
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
            if let Ok(inner) = args_val.get_item(&Value::from(0)) {
                return render_expression(&inner, cfg);
            }
            return Ok("0".to_string());
        }
        "Hold" => {
            // hold(x) — clocked-to-continuous (MLS §16.5.1).
            // Pass through the argument.
            let args_val = get_field(builtin, "args")?;
            if let Ok(inner) = args_val.get_item(&Value::from(0)) {
                return render_expression(&inner, cfg);
            }
            return Ok("0".to_string());
        }
        "FirstTick" => {
            // firstTick(u) — true at the first clock tick (MLS §16.10).
            // Stub: return false for continuous simulation.
            return Ok(cfg.false_val.clone());
        }
        "NoClock" | "SubSample" | "SuperSample" | "ShiftSample" | "BackSample" => {
            // Clocked partition operators (MLS §16). In continuous
            // simulation, pass through the first argument.
            let args_val = get_field(builtin, "args")?;
            if let Ok(inner) = args_val.get_item(&Value::from(0)) {
                return render_expression(&inner, cfg);
            }
            return Ok("0".to_string());
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

    if func_name == "Sum"
        && cfg.sum_fn != "sum1"
        && let args_val = get_field(builtin, "args")?
        && args_val.len() == Some(1)
        && let Ok(first_arg) = args_val.get_item(&Value::from(0))
        && let Ok(var_ref) = get_field(&first_arg, "VarRef")
    {
        let subs = get_field(&var_ref, "subscripts")?;
        if subs.len() == Some(0) {
            let arr_name = render_var_ref(&var_ref, cfg)?;
            return Ok(format!("{}({}, {}__len)", cfg.sum_fn, arr_name, arr_name));
        }
    }

    let args = render_args(builtin, cfg)?;

    if cfg.modelica_builtins {
        return Ok(render_builtin_modelica(&func_name, &args, cfg));
    }
    Ok(render_builtin_python(&func_name, &args, cfg))
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
        "Floor" | "Integer" => format!("floor({})", args),
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
        "Floor" | "Integer" => format!("{}floor({})", cfg.prefix, args),
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
        if let Ok(elem) = elements.get_item(&Value::from(i)) {
            elem_strs.push(render_expression(&elem, cfg)?);
        }
    }
    if elem_strs.is_empty() {
        return Ok("0".to_string());
    }
    if elem_strs.len() == 1 {
        return Ok(elem_strs.into_iter().next().unwrap());
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
    let raw_name = get_field(func_call, "name")
        .ok()
        .map(|n| {
            // VarName serializes as a plain string (newtype struct)
            get_field(&n, "0")
                .map(|v| v.to_string())
                .unwrap_or_else(|_| n.to_string())
        })
        .unwrap_or_default();

    // Map Modelica standard library math functions to builtins
    if let Some(builtin) = resolve_modelica_math_function(&raw_name) {
        let args = render_args(func_call, cfg)?;
        return Ok(render_builtin_python(builtin, &args, cfg));
    }

    let name = if cfg.sanitize_dots {
        raw_name.replace('.', "_")
    } else {
        raw_name
    };

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
    let Some(len) = args.len() else {
        return Ok(String::new());
    };

    let mut arg_strs = Vec::new();
    for i in 0..len {
        if let Ok(arg) = args.get_item(&Value::from(i)) {
            arg_strs.push(render_expression(&arg, cfg)?);
        }
    }

    Ok(arg_strs.join(", "))
}

fn render_literal(literal: &Value, cfg: &ExprConfig) -> RenderResult {
    if let Ok(real) = get_field(literal, "Real") {
        if cfg.float_literals {
            let s = real.to_string();
            return Ok(render_c_float_literal(&s));
        }
        return Ok(real.to_string());
    }
    if let Ok(int) = get_field(literal, "Integer") {
        if cfg.float_literals {
            return Ok(format!("{}.0f", int));
        }
        return Ok(int.to_string());
    }
    if let Ok(b) = get_field(literal, "Boolean") {
        return Ok(if b.is_true() {
            cfg.true_val.clone()
        } else {
            cfg.false_val.clone()
        });
    }
    if let Ok(s) = get_field(literal, "String") {
        return Ok(format!("\"{}\"", s));
    }
    Ok("0".to_string())
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
        .unwrap_or_else(|_| "0".to_string());

    let Some(branches) = get_field(if_expr, "branches").ok() else {
        return Ok(else_branch);
    };
    let Some(len) = branches.len() else {
        return Ok(else_branch);
    };

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
        let Some(branch) = branches.get_item(&Value::from(i)).ok() else {
            continue;
        };
        let Ok(cond) = branch.get_item(&Value::from(0)) else {
            continue;
        };
        let Ok(then) = branch.get_item(&Value::from(1)) else {
            continue;
        };

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

    // Unroll: render body with the loop variable substituted for each value
    // We do a simple textual substitution on the rendered body
    let mut elements = Vec::new();
    for val in start..=end {
        // Render the body expression, then substitute the loop variable
        let body_rendered = render_expression(&body_node, cfg)?;
        // Replace occurrences of the loop variable name with the concrete value
        let substituted = body_rendered.replace(&var_name, &val.to_string());
        elements.push(substituted);
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
    let base = get_field(fa, "base")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("FieldAccess missing 'base' field"))?;
    let field = get_field(fa, "field")
        .map(|v| v.to_string())
        .map_err(|_| render_err("FieldAccess missing 'field'"))?;
    Ok(format!("{base}.{field}"))
}

#[cfg(test)]
mod tests {
    use super::render_c_float_literal;

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
}
