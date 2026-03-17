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
pub(crate) fn get_field(value: &Value, name: &str) -> Result<Value, minijinja::Error> {
    let result = value
        .get_attr(name)
        .or_else(|_| value.get_item(&Value::from(name)))?;
    if result.is_undefined() || result.is_none() {
        Err(minijinja::Error::new(
            minijinja::ErrorKind::UndefinedError,
            format!("field '{}' not found", name),
        ))
    } else {
        Ok(result)
    }
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
    let op_str = get_binop_string(&op_value, cfg)?;
    Ok(format!("({lhs} {op_str} {rhs})"))
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
    let rhs = get_field(unary, "rhs")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("Unary expression missing 'rhs' field"))?;
    let op_str = get_field(unary, "op")
        .and_then(|o| get_unop_string(&o, cfg))
        .map_err(|_| render_err("Unary expression missing 'op' field"))?;
    Ok(format!("({op_str}{rhs})"))
}

pub(crate) fn get_unop_string(op: &Value, cfg: &ExprConfig) -> RenderResult {
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
        raw_name.replace('.', "_")
    } else {
        raw_name
    };

    let subscripts = render_subscripts(var_ref, cfg)?;
    if subscripts.is_empty() {
        Ok(name)
    } else {
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

pub(crate) fn render_subscript(sub: &Value, cfg: &ExprConfig) -> RenderResult {
    if let Ok(idx) = get_field(sub, "Index") {
        let val = idx
            .as_i64()
            .ok_or_else(|| render_err("subscript Index is not an integer"))?;
        return if cfg.one_based_index {
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
        "Floor" => format!("floor({})", args),
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
        "Ceil" => format!("{}ceil({})", cfg.prefix, args),
        "Min" => format!("{}fmin({})", cfg.prefix, args),
        "Max" => format!("{}fmax({})", cfg.prefix, args),
        "Sum" => format!("{}sum1({})", cfg.prefix, args),
        "Transpose" => format!("({}).T", args),
        "Zeros" => format!("{}zeros({})", cfg.prefix, args),
        "Ones" => format!("{}ones({})", cfg.prefix, args),
        "Identity" => format!("{}eye({})", cfg.prefix, args),
        "Cross" => format!("{}cross({})", cfg.prefix, args),
        _ => format!("{}({})", func_name.to_lowercase(), args),
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
    let name = if cfg.sanitize_dots {
        raw_name.replace('.', "_")
    } else {
        raw_name
    };

    let args = render_args(func_call, cfg)?;
    Ok(format!("{}({})", name, args))
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
        return Ok(real.to_string());
    }
    if let Ok(int) = get_field(literal, "Integer") {
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
                format!(
                    "{}if_else({}, {}, {})",
                    cfg.prefix, cond_str, then_str, result
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
fn render_range(range: &Value, cfg: &ExprConfig) -> RenderResult {
    let start = get_field(range, "start")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("Range missing 'start' field"))?;
    let end = get_field(range, "end")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("Range missing 'end' field"))?;
    if let Ok(step) = get_field(range, "step") {
        let step_str = render_expression(&step, cfg)?;
        Ok(format!("{start}:{step_str}:{end}"))
    } else {
        Ok(format!("{start}:{end}"))
    }
}

/// Render an array-comprehension expression as `{expr for i in range ... if filter}`.
fn render_array_comprehension(array_comp: &Value, cfg: &ExprConfig) -> RenderResult {
    let body = get_field(array_comp, "expr")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("ArrayComprehension missing 'expr' field"))?;
    let indices = get_field(array_comp, "indices")
        .map_err(|_| render_err("ArrayComprehension missing 'indices' field"))?;
    let len = indices.len().unwrap_or(0);

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

    Ok(format!("{{{body}{for_clause}{filter_clause}}}"))
}

/// Render an index expression as `base[subscripts]`.
fn render_index(index: &Value, cfg: &ExprConfig) -> RenderResult {
    let base = get_field(index, "base")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("Index missing 'base' field"))?;
    let subs = get_field(index, "subscripts")
        .map_err(|_| render_err("Index missing 'subscripts' field"))?;
    let len = subs.len().unwrap_or(0);
    let mut sub_strs = Vec::new();
    for i in 0..len {
        if let Ok(sub) = subs.get_item(&Value::from(i)) {
            sub_strs.push(render_subscript(&sub, cfg)?);
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
