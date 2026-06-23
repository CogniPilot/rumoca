//! Per-dialect op renderers for the solve-row IR.
//!
//! Extracted from `render_solve` per SPEC_0021: standalone `match op { ... }`
//! tables for C, Rust, WGSL and Python dialects, called from the
//! `SolveRowDialect` dispatch methods that remain in the parent module.

use crate::errors::render_err;

use super::RenderResult;

// Python (CasADi / JAX)

/// Unary ops for the Python (CasADi/JAX) dialect. Emits bare function names the
/// consuming template binds to `ca.*` / `jnp.*`. Mirrors the reference
/// `rumoca_backend` interpreter's op table.
pub(super) fn render_solve_unary_py(op: &str, arg: String) -> RenderResult {
    match op {
        "Neg" => Ok(format!("(-({arg}))")),
        "Not" => Ok(format!("if_else(({arg}) == 0.0, 1.0, 0.0)")),
        "Abs" => Ok(format!("fabs({arg})")),
        "Sign" => Ok(format!("sign({arg})")),
        "Sqrt" => Ok(format!("sqrt({arg})")),
        "Floor" => Ok(format!("floor({arg})")),
        "Ceil" => Ok(format!("ceil({arg})")),
        "Trunc" => Ok(format!("trunc({arg})")),
        "Sin" => Ok(format!("sin({arg})")),
        "Cos" => Ok(format!("cos({arg})")),
        "Tan" => Ok(format!("tan({arg})")),
        "Asin" => Ok(format!("asin({arg})")),
        "Acos" => Ok(format!("acos({arg})")),
        "Atan" => Ok(format!("atan({arg})")),
        "Sinh" => Ok(format!("sinh({arg})")),
        "Cosh" => Ok(format!("cosh({arg})")),
        "Tanh" => Ok(format!("tanh({arg})")),
        "Exp" => Ok(format!("exp({arg})")),
        "Log" => Ok(format!("log({arg})")),
        "Log10" => Ok(format!("log10({arg})")),
        _ => Err(render_err(format!("unsupported solve unary op: {op}"))),
    }
}

/// Binary ops for the Python (CasADi/JAX) dialect.
pub(super) fn render_solve_binary_py(op: &str, lhs: String, rhs: String) -> RenderResult {
    match op {
        "Add" => Ok(format!("(({lhs}) + ({rhs}))")),
        "Sub" => Ok(format!("(({lhs}) - ({rhs}))")),
        "Mul" => Ok(format!("(({lhs}) * ({rhs}))")),
        "Div" => Ok(format!("(({lhs}) / ({rhs}))")),
        "Pow" => Ok(format!("(({lhs}) ** ({rhs}))")),
        // Numeric 0/1 result, independent of casadi (0/1) vs jax (bool) compare
        // semantics, so downstream arithmetic is well-typed in both.
        "And" => Ok(format!(
            "if_else(logical_and(({lhs}) != 0.0, ({rhs}) != 0.0), 1.0, 0.0)"
        )),
        "Or" => Ok(format!(
            "if_else(logical_or(({lhs}) != 0.0, ({rhs}) != 0.0), 1.0, 0.0)"
        )),
        "Atan2" => Ok(format!("atan2({lhs}, {rhs})")),
        "Min" => Ok(format!("fmin({lhs}, {rhs})")),
        "Max" => Ok(format!("fmax({lhs}, {rhs})")),
        _ => Err(render_err(format!("unsupported solve binary op: {op}"))),
    }
}

/// Comparison ops for the Python (CasADi/JAX) dialect. Bare relational
/// operators, matching `rumoca_backend` (CasADi yields 0/1, JAX yields bool;
/// both feed `if_else`/`where` and numeric use correctly).
pub(super) fn render_solve_compare_py(op: &str, lhs: String, rhs: String) -> RenderResult {
    let op = match op {
        "Lt" => "<",
        "Le" => "<=",
        "Gt" => ">",
        "Ge" => ">=",
        "Eq" => "==",
        "Ne" | "Neq" => "!=",
        _ => return Err(render_err(format!("unsupported solve compare op: {op}"))),
    };
    Ok(format!("(({lhs}) {op} ({rhs}))"))
}

// WGSL (WebGPU compute shader)

pub(super) fn render_solve_unary_wgsl(op: &str, arg: String) -> RenderResult {
    match op {
        "Neg" => Ok(format!("(-({arg}))")),
        "Not" => Ok(format!("select(0.0, 1.0, ({arg}) == 0.0)")),
        "Abs" => Ok(format!("abs({arg})")),
        "Sign" => Ok(format!("sign({arg})")),
        "Sqrt" => Ok(format!("sqrt({arg})")),
        "Floor" => Ok(format!("floor({arg})")),
        "Ceil" => Ok(format!("ceil({arg})")),
        "Trunc" => Ok(format!("trunc({arg})")),
        "Sin" => Ok(format!("sin({arg})")),
        "Cos" => Ok(format!("cos({arg})")),
        "Tan" => Ok(format!("tan({arg})")),
        "Asin" => Ok(format!("asin({arg})")),
        "Acos" => Ok(format!("acos({arg})")),
        "Atan" => Ok(format!("atan({arg})")),
        "Sinh" => Ok(format!("sinh({arg})")),
        "Cosh" => Ok(format!("cosh({arg})")),
        "Tanh" => Ok(format!("tanh({arg})")),
        "Exp" => Ok(format!("exp({arg})")),
        "Log" => Ok(format!("log({arg})")),
        "Log10" => Ok(format!("(log({arg}) / log(10.0))")),
        _ => Err(render_err(format!("unsupported solve unary op: {op}"))),
    }
}

pub(super) fn render_solve_binary_wgsl(op: &str, lhs: String, rhs: String) -> RenderResult {
    match op {
        "Add" => Ok(format!("(({lhs}) + ({rhs}))")),
        "Sub" => Ok(format!("(({lhs}) - ({rhs}))")),
        "Mul" => Ok(format!("(({lhs}) * ({rhs}))")),
        "Div" => Ok(format!("(({lhs}) / ({rhs}))")),
        "Pow" => Ok(format!("pow({lhs}, {rhs})")),
        "And" => Ok(format!(
            "select(0.0, 1.0, (({lhs}) != 0.0) && (({rhs}) != 0.0))"
        )),
        "Or" => Ok(format!(
            "select(0.0, 1.0, (({lhs}) != 0.0) || (({rhs}) != 0.0))"
        )),
        "Atan2" => Ok(format!("atan2({lhs}, {rhs})")),
        "Min" => Ok(format!("min({lhs}, {rhs})")),
        "Max" => Ok(format!("max({lhs}, {rhs})")),
        _ => Err(render_err(format!("unsupported solve binary op: {op}"))),
    }
}

pub(super) fn render_solve_compare_wgsl(op: &str, lhs: String, rhs: String) -> RenderResult {
    let op = match op {
        "Lt" => "<",
        "Le" => "<=",
        "Gt" => ">",
        "Ge" => ">=",
        "Eq" => "==",
        "Ne" => "!=",
        _ => return Err(render_err(format!("unsupported solve compare op: {op}"))),
    };
    Ok(format!("select(0.0, 1.0, ({lhs}) {op} ({rhs}))"))
}

// C

pub(super) fn render_solve_unary_c(op: &str, arg: String) -> RenderResult {
    match op {
        "Neg" => Ok(format!("(-({arg}))")),
        "Not" => Ok(format!("(({arg}) == 0.0 ? 1.0 : 0.0)")),
        "Abs" => Ok(format!("fabs({arg})")),
        "Sign" => Ok(format!(
            "(({arg}) > 0.0 ? 1.0 : (({arg}) < 0.0 ? -1.0 : 0.0))"
        )),
        "Sqrt" => Ok(format!("sqrt({arg})")),
        "Floor" => Ok(format!("floor({arg})")),
        "Ceil" => Ok(format!("ceil({arg})")),
        "Trunc" => Ok(format!("trunc({arg})")),
        "Sin" => Ok(format!("sin({arg})")),
        "Cos" => Ok(format!("cos({arg})")),
        "Tan" => Ok(format!("tan({arg})")),
        "Asin" => Ok(format!("asin({arg})")),
        "Acos" => Ok(format!("acos({arg})")),
        "Atan" => Ok(format!("atan({arg})")),
        "Sinh" => Ok(format!("sinh({arg})")),
        "Cosh" => Ok(format!("cosh({arg})")),
        "Tanh" => Ok(format!("tanh({arg})")),
        "Exp" => Ok(format!("exp({arg})")),
        "Log" => Ok(format!("log({arg})")),
        "Log10" => Ok(format!("log10({arg})")),
        _ => Err(render_err(format!("unsupported solve unary op: {op}"))),
    }
}

pub(super) fn render_solve_binary_c(op: &str, lhs: String, rhs: String) -> RenderResult {
    match op {
        "Add" => Ok(format!("(({lhs}) + ({rhs}))")),
        "Sub" => Ok(format!("(({lhs}) - ({rhs}))")),
        "Mul" => Ok(format!("(({lhs}) * ({rhs}))")),
        "Div" => Ok(format!("(({lhs}) / ({rhs}))")),
        "Pow" => Ok(format!("pow({lhs}, {rhs})")),
        "And" => Ok(format!("((({lhs}) != 0.0 && ({rhs}) != 0.0) ? 1.0 : 0.0)")),
        "Or" => Ok(format!("((({lhs}) != 0.0 || ({rhs}) != 0.0) ? 1.0 : 0.0)")),
        "Atan2" => Ok(format!("atan2({lhs}, {rhs})")),
        "Min" => Ok(format!("fmin({lhs}, {rhs})")),
        "Max" => Ok(format!("fmax({lhs}, {rhs})")),
        _ => Err(render_err(format!("unsupported solve binary op: {op}"))),
    }
}

// Rust

pub(super) fn render_solve_unary_rust(op: &str, arg: String) -> RenderResult {
    match op {
        "Neg" => Ok(format!("(-({arg}))")),
        "Not" => Ok(format!("if ({arg}) == 0.0 {{ 1.0 }} else {{ 0.0 }}")),
        "Abs" => Ok(format!("({arg}).abs()")),
        "Sign" => Ok(format!(
            "if ({arg}) > 0.0 {{ 1.0 }} else if ({arg}) < 0.0 {{ -1.0 }} else {{ 0.0 }}"
        )),
        "Sqrt" => Ok(format!("({arg}).sqrt()")),
        "Floor" => Ok(format!("({arg}).floor()")),
        "Ceil" => Ok(format!("({arg}).ceil()")),
        "Trunc" => Ok(format!("({arg}).trunc()")),
        "Sin" => Ok(format!("({arg}).sin()")),
        "Cos" => Ok(format!("({arg}).cos()")),
        "Tan" => Ok(format!("({arg}).tan()")),
        "Asin" => Ok(format!("({arg}).asin()")),
        "Acos" => Ok(format!("({arg}).acos()")),
        "Atan" => Ok(format!("({arg}).atan()")),
        "Sinh" => Ok(format!("({arg}).sinh()")),
        "Cosh" => Ok(format!("({arg}).cosh()")),
        "Tanh" => Ok(format!("({arg}).tanh()")),
        "Exp" => Ok(format!("({arg}).exp()")),
        "Log" => Ok(format!("({arg}).ln()")),
        "Log10" => Ok(format!("({arg}).log10()")),
        _ => Err(render_err(format!("unsupported solve unary op: {op}"))),
    }
}

pub(super) fn render_solve_binary_rust(op: &str, lhs: String, rhs: String) -> RenderResult {
    match op {
        "Add" => Ok(format!("(({lhs}) + ({rhs}))")),
        "Sub" => Ok(format!("(({lhs}) - ({rhs}))")),
        "Mul" => Ok(format!("(({lhs}) * ({rhs}))")),
        "Div" => Ok(format!("(({lhs}) / ({rhs}))")),
        "Pow" => Ok(format!("({lhs}).powf({rhs})")),
        "And" => Ok(format!(
            "if ({lhs}) != 0.0 && ({rhs}) != 0.0 {{ 1.0 }} else {{ 0.0 }}"
        )),
        "Or" => Ok(format!(
            "if ({lhs}) != 0.0 || ({rhs}) != 0.0 {{ 1.0 }} else {{ 0.0 }}"
        )),
        "Atan2" => Ok(format!("({lhs}).atan2({rhs})")),
        "Min" => Ok(format!("({lhs}).min({rhs})")),
        "Max" => Ok(format!("({lhs}).max({rhs})")),
        _ => Err(render_err(format!("unsupported solve binary op: {op}"))),
    }
}

// C / Rust shared compare (ternary-style)

pub(super) fn render_solve_compare(op: &str, lhs: String, rhs: String) -> RenderResult {
    let op = match op {
        "Lt" => "<",
        "Le" => "<=",
        "Gt" => ">",
        "Ge" => ">=",
        "Eq" => "==",
        "Ne" => "!=",
        _ => return Err(render_err(format!("unsupported solve compare op: {op}"))),
    };
    Ok(format!("((({lhs}) {op} ({rhs})) ? 1.0 : 0.0)"))
}
