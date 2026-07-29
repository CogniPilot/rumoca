//! Per-dialect op renderers for the solve-row IR.
//!
//! Extracted from `render_solve` per SPEC_0021: standalone `match op { ... }`
//! tables for C, Rust, WGSL and Python dialects, called from the
//! `SolveRowDialect` dispatch methods that remain in the parent module.

use crate::errors::render_err;

use super::RenderResult;

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
