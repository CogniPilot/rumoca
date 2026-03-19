//! Statement and equation rendering.
//!
//! This module handles rendering of:
//! - DAE equations (`render_equation`, `render_flat_equation`)
//! - Algorithm statements (`render_statement`, `render_statements`)
//! - AST expression variants used within statements

use super::render_expr::{
    get_binop_string, get_field, get_unop_string, is_mul_elem_op, render_args, render_expression,
};
use super::{ExprConfig, IfStyle, RenderResult};
use crate::errors::render_err;
use minijinja::Value;

// ── Equation rendering ───────────────────────────────────────────────

/// Render an equation to `lhs = rhs` form.
pub(crate) fn render_equation(eq: &Value, cfg: &ExprConfig) -> RenderResult {
    // Check for explicit form: eq.lhs is set
    if let Ok(lhs_val) = eq.get_attr("lhs")
        && !lhs_val.is_none()
        && !lhs_val.is_undefined()
    {
        let lhs_str = render_explicit_lhs(&lhs_val, cfg);
        let rhs_str = eq
            .get_attr("rhs")
            .and_then(|v| render_expression(&v, cfg))
            .unwrap_or_default();
        return Ok(format!("{lhs_str} = {rhs_str}"));
    }

    // Residual form: try to decompose top-level Binary Sub into lhs = rhs
    let rhs = eq.get_attr("rhs").unwrap_or(Value::UNDEFINED);
    if let Ok(binary) = get_field(&rhs, "Binary")
        && is_sub_op(&binary)
    {
        let lhs_expr = get_field(&binary, "lhs")
            .and_then(|v| render_expression(&v, cfg))
            .unwrap_or_default();
        let rhs_expr = get_field(&binary, "rhs")
            .and_then(|v| render_expression(&v, cfg))
            .unwrap_or_default();
        return Ok(format!("{lhs_expr} = {rhs_expr}"));
    }

    // Fallback: 0 = expression
    let expr_str = render_expression(&rhs, cfg)?;
    Ok(format!("0 = {expr_str}"))
}

/// Render a Equation (residual form) to `lhs = rhs`.
///
/// Equation has a `residual` field (not `rhs`/`lhs`).
/// Decomposes top-level `Binary::Sub` into `lhs = rhs` form.
/// Falls back to `0 = expr` if no subtraction.
pub(crate) fn render_flat_equation(eq: &Value, cfg: &ExprConfig) -> RenderResult {
    let residual = eq.get_attr("residual").unwrap_or(Value::UNDEFINED);
    if let Ok(binary) = get_field(&residual, "Binary")
        && is_sub_op(&binary)
    {
        let lhs_expr = get_field(&binary, "lhs")
            .and_then(|v| render_expression(&v, cfg))
            .unwrap_or_default();
        let rhs_expr = get_field(&binary, "rhs")
            .and_then(|v| render_expression(&v, cfg))
            .unwrap_or_default();
        return Ok(format!("{lhs_expr} = {rhs_expr}"));
    }

    // Fallback: 0 = expression
    let expr_str = render_expression(&residual, cfg)?;
    Ok(format!("0 = {expr_str}"))
}

/// Render the LHS of an explicit equation (VarName).
fn render_explicit_lhs(lhs: &Value, cfg: &ExprConfig) -> String {
    // VarName serializes as a string or {"0": "name"}
    let raw = get_field(lhs, "0")
        .map(|v| v.to_string())
        .unwrap_or_else(|_| lhs.to_string());
    if cfg.sanitize_dots {
        raw.replace('.', "_")
    } else {
        raw
    }
}

/// Check if a Binary expression's op is Sub or SubElem.
fn is_sub_op(binary: &Value) -> bool {
    if let Ok(op) = get_field(binary, "op") {
        return get_field(&op, "Sub").is_ok() || get_field(&op, "SubElem").is_ok();
    }
    false
}

// ── Statement rendering ──────────────────────────────────────────────

/// Render a list of statements to a string.
pub(crate) fn render_statements(stmts: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
    let Some(len) = stmts.len() else {
        return Ok(String::new());
    };

    let mut stmt_strs = Vec::new();
    for i in 0..len {
        if let Ok(stmt) = stmts.get_item(&Value::from(i)) {
            let s = render_statement(&stmt, cfg, indent)?;
            if !s.is_empty() {
                stmt_strs.push(s);
            }
        }
    }

    Ok(stmt_strs.join("\n"))
}

/// Render a single statement to a string.
pub(crate) fn render_statement(stmt: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
    // Unit variants serialize as strings (e.g., "Empty")
    if let Some(s) = stmt.as_str() {
        return match s {
            "Empty" => Ok(String::new()),
            "Return" => Ok(format!("{indent}return")),
            "Break" => Ok(format!("{indent}break")),
            _ => Err(render_err(format!("unhandled statement variant: {s}"))),
        };
    }

    // Struct variants serialize as {"VariantName": {fields...}}
    if let Ok(assign) = get_field(stmt, "Assignment") {
        return render_assignment(&assign, cfg, indent);
    }
    if let Ok(ret) = get_field(stmt, "Return") {
        let _ = ret;
        return Ok(format!("{indent}return"));
    }
    if let Ok(brk) = get_field(stmt, "Break") {
        let _ = brk;
        return Ok(format!("{indent}break"));
    }
    if let Ok(for_stmt) = get_field(stmt, "For") {
        return render_for_statement(&for_stmt, cfg, indent);
    }
    if let Ok(while_stmt) = get_field(stmt, "While") {
        return render_while_statement(&while_stmt, cfg, indent);
    }
    if let Ok(if_stmt) = get_field(stmt, "If") {
        return render_if_statement(&if_stmt, cfg, indent);
    }
    if let Ok(when_stmt) = get_field(stmt, "When") {
        return render_when_statement(&when_stmt, cfg, indent);
    }
    if let Ok(func_call) = get_field(stmt, "FunctionCall") {
        return render_function_call_statement(&func_call, cfg, indent);
    }
    if let Ok(reinit) = get_field(stmt, "Reinit") {
        return render_reinit_statement(&reinit, cfg, indent);
    }
    if let Ok(assert) = get_field(stmt, "Assert") {
        return render_assert_statement(&assert, cfg, indent);
    }

    Err(render_err(format!("unhandled statement: {stmt}")))
}

/// Render an assignment statement: comp := value
fn render_assignment(assign: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
    let comp_val = get_field(assign, "comp")
        .map_err(|_| render_err(format!("Assignment missing 'comp' field: {assign}")))?;
    let comp = render_component_ref(&comp_val);
    if comp.trim().is_empty() {
        return Err(render_err(format!(
            "Assignment target resolved to empty component reference: {assign}"
        )));
    }

    let value = get_field(assign, "value")
        .map_err(|_| render_err(format!("Assignment missing 'value' field: {assign}")))
        .and_then(|v| render_expression(&v, cfg))?;
    let semi = if matches!(cfg.if_style, IfStyle::Ternary | IfStyle::Modelica) {
        ";"
    } else {
        ""
    };
    Ok(format!("{indent}{comp} = {value}{semi}"))
}

/// Render a for loop statement.
fn render_for_statement(for_stmt: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
    let mut result = String::new();

    // Get indices (loop variables)
    let indices = for_stmt.get_attr("indices").ok();
    let equations = for_stmt.get_attr("equations").ok();

    // Extract first index (simplification: handle one index for now)
    let (loop_var, range_str) = extract_for_loop_index(&indices, cfg)?;

    // Generate for loop header based on config
    match cfg.if_style {
        IfStyle::Ternary => {
            result.push_str(&format!("{indent}for (int {loop_var} = 0; {loop_var} < /* {range_str} */; {loop_var}++) {{\n"));
        }
        IfStyle::Function => {
            result.push_str(&format!("{indent}for {loop_var} in range({range_str}):\n"));
        }
        IfStyle::Modelica => {
            result.push_str(&format!("{indent}for {loop_var} in {range_str} loop\n"));
        }
    }

    // Render body statements
    let next_indent = format!("{indent}    ");
    if let Some(ref eqs) = equations {
        let body = render_statements(eqs, cfg, &next_indent)?;
        result.push_str(&body);
    }

    // Close the loop
    match cfg.if_style {
        IfStyle::Ternary => {
            result.push_str(&format!("\n{indent}}}"));
        }
        IfStyle::Function => {
            // Python doesn't need closing brace
        }
        IfStyle::Modelica => {
            result.push_str(&format!("\n{indent}end for;"));
        }
    }

    Ok(result)
}

/// Extract loop variable and range from for loop indices.
fn extract_for_loop_index(
    indices: &Option<Value>,
    cfg: &ExprConfig,
) -> Result<(String, String), minijinja::Error> {
    let default = ("i".to_string(), "1:1".to_string());

    let Some(indices_val) = indices else {
        return Ok(default);
    };
    let Ok(first) = indices_val.get_item(&Value::from(0)) else {
        return Ok(default);
    };

    let ident = first
        .get_attr("ident")
        .ok()
        .and_then(|i| i.get_attr("text").ok())
        .map(|t| t.to_string())
        .unwrap_or_else(|| "i".to_string());

    let range = first
        .get_attr("range")
        .and_then(|r| render_ast_expression(&r, cfg))
        .unwrap_or_else(|_| "1:1".to_string());

    Ok((ident, range))
}

/// Render a while loop statement.
fn render_while_statement(while_stmt: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
    let mut result = String::new();

    let cond = while_stmt
        .get_attr("cond")
        .and_then(|c| render_ast_expression(&c, cfg))
        .unwrap_or_else(|_| "true".to_string());
    let stmts = while_stmt.get_attr("statements").ok();

    match cfg.if_style {
        IfStyle::Ternary => {
            result.push_str(&format!("{indent}while ({cond}) {{\n"));
        }
        IfStyle::Function => {
            result.push_str(&format!("{indent}while {cond}:\n"));
        }
        IfStyle::Modelica => {
            result.push_str(&format!("{indent}while {cond} loop\n"));
        }
    }

    let next_indent = format!("{indent}    ");
    if let Some(ref stmts_val) = stmts {
        let body = render_statements(stmts_val, cfg, &next_indent)?;
        result.push_str(&body);
    }

    match cfg.if_style {
        IfStyle::Ternary => result.push_str(&format!("\n{indent}}}")),
        IfStyle::Modelica => result.push_str(&format!("\n{indent}end while;")),
        IfStyle::Function => {}
    }

    Ok(result)
}

/// Render an if statement.
fn render_if_statement(if_stmt: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
    let mut result = String::new();
    let next_indent = format!("{indent}    ");

    let cond_blocks = if_stmt.get_attr("cond_blocks").ok();
    let else_block = if_stmt.get_attr("else_block").ok();

    // Render condition blocks
    if let Some(ref blocks) = cond_blocks {
        result.push_str(&render_if_cond_blocks(blocks, cfg, indent, &next_indent)?);
    }

    // Handle else block
    if let Some(ref else_val) = else_block
        && !else_val.is_none()
    {
        result.push_str(&render_if_else_block(else_val, cfg, indent, &next_indent)?);
    }

    Ok(result)
}

/// Render the condition blocks of an if statement.
fn render_if_cond_blocks(
    blocks: &Value,
    cfg: &ExprConfig,
    indent: &str,
    next_indent: &str,
) -> RenderResult {
    let Some(len) = blocks.len() else {
        return Ok(String::new());
    };

    let mut result = String::new();
    for i in 0..len {
        let Ok(block) = blocks.get_item(&Value::from(i)) else {
            continue;
        };
        result.push_str(&render_if_branch(&block, i, cfg, indent, next_indent)?);
    }
    Ok(result)
}

/// Render a single if/elseif branch.
fn render_if_branch(
    block: &Value,
    index: usize,
    cfg: &ExprConfig,
    indent: &str,
    next_indent: &str,
) -> RenderResult {
    let mut result = String::new();

    let cond = block
        .get_attr("cond")
        .and_then(|c| render_ast_expression(&c, cfg))
        .unwrap_or_else(|_| "true".to_string());
    let stmts = block.get_attr("statements").ok();

    match cfg.if_style {
        IfStyle::Ternary => {
            let prefix = if index > 0 { " " } else { indent };
            let keyword = if index == 0 { "if" } else { "else if" };
            result.push_str(&format!("{prefix}{keyword} ({cond}) {{\n"));
        }
        IfStyle::Function => {
            let kw = if index == 0 { "if" } else { "elif" };
            result.push_str(&format!("{indent}{kw} {cond}:\n"));
        }
        IfStyle::Modelica => {
            let kw = if index == 0 { "if" } else { "elseif" };
            result.push_str(&format!("{indent}{kw} {cond} then\n"));
        }
    }

    let body = if let Some(ref stmts_val) = stmts {
        render_statements(stmts_val, cfg, next_indent)?
    } else {
        String::new()
    };
    if body.trim().is_empty() && matches!(cfg.if_style, IfStyle::Function) {
        result.push_str(&format!("{next_indent}pass"));
    } else {
        result.push_str(&body);
    }

    if matches!(cfg.if_style, IfStyle::Ternary) {
        result.push_str(&format!("\n{indent}}}"));
    }

    Ok(result)
}

/// Render the else block of an if statement.
fn render_if_else_block(
    else_val: &Value,
    cfg: &ExprConfig,
    indent: &str,
    next_indent: &str,
) -> RenderResult {
    let mut result = String::new();

    match cfg.if_style {
        IfStyle::Ternary => result.push_str(" else {\n"),
        IfStyle::Function => result.push_str(&format!("{indent}else:\n")),
        IfStyle::Modelica => result.push_str(&format!("{indent}else\n")),
    }

    result.push_str(&render_statements(else_val, cfg, next_indent)?);

    match cfg.if_style {
        IfStyle::Ternary => result.push_str(&format!("\n{indent}}}")),
        IfStyle::Modelica => result.push_str(&format!("\n{indent}end if;")),
        IfStyle::Function => {}
    }

    Ok(result)
}

/// Render a when statement (in algorithms).
fn render_when_statement(_when_stmt: &Value, _cfg: &ExprConfig, _indent: &str) -> RenderResult {
    Err(render_err(
        "when statements in algorithms not yet implemented (requires event handling)",
    ))
}

/// Render a function call statement.
fn render_function_call_statement(
    func_call: &Value,
    cfg: &ExprConfig,
    indent: &str,
) -> RenderResult {
    let comp = func_call
        .get_attr("comp")
        .map(|c| render_component_ref(&c))
        .unwrap_or_default();

    let args = render_args(func_call, cfg).unwrap_or_default();

    // Check if there are output assignments: (a, b) := func(x)
    if let Some(out_strs) = extract_output_assignments(func_call, cfg)? {
        return Ok(format_func_call_with_outputs(
            indent, &out_strs, &comp, &args,
        ));
    }

    // Simple function call without outputs
    Ok(format!("{indent}{}({});", comp, args))
}

/// Extract output assignments from a function call if present.
fn extract_output_assignments(
    func_call: &Value,
    cfg: &ExprConfig,
) -> Result<Option<Vec<String>>, minijinja::Error> {
    let Some(outputs) = func_call.get_attr("outputs").ok() else {
        return Ok(None);
    };
    let Some(len) = outputs.len() else {
        return Ok(None);
    };
    if len == 0 {
        return Ok(None);
    }

    let mut out_strs = Vec::new();
    for i in 0..len {
        if let Ok(o) = outputs.get_item(&Value::from(i)) {
            out_strs.push(render_expression(&o, cfg)?);
        }
    }

    Ok(Some(out_strs))
}

/// Format a function call with output assignments.
fn format_func_call_with_outputs(
    indent: &str,
    outputs: &[String],
    comp: &str,
    args: &str,
) -> String {
    if outputs.len() == 1 {
        format!("{indent}{} = {}({});", outputs[0], comp, args)
    } else {
        format!("{indent}({}) = {}({});", outputs.join(", "), comp, args)
    }
}

/// Render a reinit statement.
fn render_reinit_statement(reinit: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
    let var = reinit
        .get_attr("variable")
        .map(|v| render_component_ref(&v))
        .unwrap_or_default();
    let value = reinit
        .get_attr("value")
        .and_then(|v| render_ast_expression(&v, cfg))
        .unwrap_or_default();
    Ok(format!("{indent}/* reinit({var}, {value}) */"))
}

/// Render an assert statement.
fn render_assert_statement(assert: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
    let cond = assert
        .get_attr("condition")
        .and_then(|c| render_ast_expression(&c, cfg))
        .unwrap_or_default();
    let msg = assert
        .get_attr("message")
        .and_then(|m| render_ast_expression(&m, cfg))
        .unwrap_or_else(|_| "\"assertion failed\"".to_string());

    Ok(match cfg.if_style {
        IfStyle::Ternary => format!("{indent}assert({cond}); /* {msg} */"),
        IfStyle::Function => format!("{indent}assert {cond}, {msg}"),
        IfStyle::Modelica => format!("{indent}assert({cond}, {msg});"),
    })
}

// ── Component reference rendering ────────────────────────────────────

/// Render an AST ComponentReference to a string.
fn render_component_ref(comp: &Value) -> String {
    if let Some(s) = comp.as_str() {
        return s.replace('.', "_");
    }

    let Some(parts_val) = get_field(comp, "parts").ok() else {
        return String::new();
    };
    let Some(len) = parts_val.len() else {
        return String::new();
    };

    let part_strs: Vec<_> = (0..len)
        .filter_map(|i| parts_val.get_item(&Value::from(i)).ok())
        .map(|part| render_component_ref_part(&part))
        .collect();

    part_strs.join("_")
}

/// Render a single component reference part (identifier + optional subscripts).
fn render_component_ref_part(part: &Value) -> String {
    let ident = get_field(part, "ident")
        .ok()
        .map(|i| {
            if let Ok(text) = i.get_attr("text")
                && !text.is_undefined()
                && !text.is_none()
            {
                return text.to_string();
            }
            i.as_str()
                .map(ToOwned::to_owned)
                .unwrap_or_else(|| i.to_string())
        })
        .unwrap_or_default();

    let sub_str = render_part_subscripts(part);
    if sub_str.is_empty() {
        ident
    } else {
        format!("{}[{}]", ident, sub_str)
    }
}

/// Render subscripts for a component reference part.
fn render_part_subscripts(part: &Value) -> String {
    let subs_val = get_field(part, "subscripts")
        .ok()
        .or_else(|| get_field(part, "subs").ok());
    let Some(subs_val) = subs_val else {
        return String::new();
    };
    let Some(sub_len) = subs_val.len() else {
        return String::new();
    };
    if sub_len == 0 {
        return String::new();
    }

    let sub_strs: Vec<_> = (0..sub_len)
        .filter_map(|j| subs_val.get_item(&Value::from(j)).ok())
        .map(|s| render_ast_subscript(&s))
        .collect();

    sub_strs.join(", ")
}

/// Render an AST subscript.
fn render_ast_subscript(sub: &Value) -> String {
    if let Ok(index) = get_field(sub, "Index") {
        return index.to_string();
    }
    if let Ok(expr) = get_field(sub, "Expr") {
        return render_expression(&expr, &ExprConfig::default()).unwrap_or_default();
    }
    // Subscripts can be Expression, Range, or Empty (colon)
    if let Ok(expr) = get_field(sub, "Expression") {
        return render_ast_expression(&expr, &ExprConfig::default()).unwrap_or_default();
    }
    ":".to_string()
}

// ── AST Expression rendering ─────────────────────────────────────────

/// Render AST Expression (different from Expression).
pub(crate) fn render_ast_expression(expr: &Value, cfg: &ExprConfig) -> RenderResult {
    // Handle None
    if expr.is_none() {
        return Ok("0".to_string());
    }

    // Unit variants serialize as strings (e.g., "Empty")
    if let Some(s) = expr.as_str() {
        if s == "Empty" {
            return Ok("0".to_string());
        }
        // Could be an identifier string
        return Ok(s.replace('.', "_"));
    }

    // Struct variants serialize as {"VariantName": {fields...}}
    if let Ok(binary) = get_field(expr, "Binary") {
        return render_ast_binary(&binary, cfg);
    }
    if let Ok(unary) = get_field(expr, "Unary") {
        return render_ast_unary(&unary, cfg);
    }
    if let Ok(terminal) = get_field(expr, "Terminal") {
        return Ok(render_ast_terminal(&terminal, cfg));
    }
    if let Ok(comp_ref) = get_field(expr, "ComponentReference") {
        return Ok(render_component_ref(&comp_ref));
    }
    if let Ok(if_expr) = get_field(expr, "If") {
        return render_ast_if_expr(&if_expr, cfg);
    }
    if let Ok(func_call) = get_field(expr, "FunctionCall") {
        return render_ast_func_call(&func_call, cfg);
    }
    if let Ok(array) = get_field(expr, "Array") {
        return render_ast_array(&array, cfg);
    }
    if let Ok(tuple) = get_field(expr, "Tuple") {
        return render_ast_tuple(&tuple, cfg);
    }
    if let Ok(range) = get_field(expr, "Range") {
        return render_ast_range(&range, cfg);
    }
    if let Ok(named) = get_field(expr, "NamedArgument") {
        return render_ast_named_arg(&named, cfg);
    }

    // Fallback: try DAE IR expression renderer (function bodies may contain
    // DAE-style expressions like VarRef, Literal, BuiltinCall, etc.)
    if let Ok(result) = render_expression(expr, cfg) {
        return Ok(result);
    }

    // Last resort: try to convert to string
    let s = expr.to_string();
    if s != "none" && !s.is_empty() {
        return Ok(s);
    }

    Err(render_err(format!(
        "unhandled AST expression variant: {expr}"
    )))
}

fn render_ast_binary(binary: &Value, cfg: &ExprConfig) -> RenderResult {
    let lhs = binary
        .get_attr("lhs")
        .map_err(|_| render_err("AST Binary expression missing 'lhs' field"))
        .and_then(|v| render_ast_expression(&v, cfg))?;
    let rhs = binary
        .get_attr("rhs")
        .map_err(|_| render_err("AST Binary expression missing 'rhs' field"))
        .and_then(|v| render_ast_expression(&v, cfg))?;
    let op = binary
        .get_attr("op")
        .map_err(|_| render_err("AST Binary expression missing 'op' field"))?;
    if is_mul_elem_op(&op)
        && let Some(func) = &cfg.mul_elem_fn
    {
        return Ok(format!("{func}({lhs}, {rhs})"));
    }
    let op = get_binop_string(&op, cfg)?;
    Ok(format!("({lhs} {op} {rhs})"))
}

fn render_ast_unary(unary: &Value, cfg: &ExprConfig) -> RenderResult {
    let rhs = unary
        .get_attr("rhs")
        .map_err(|_| render_err("AST Unary expression missing 'rhs' field"))
        .and_then(|v| render_ast_expression(&v, cfg))?;
    let op = unary
        .get_attr("op")
        .map_err(|_| render_err("AST Unary expression missing 'op' field"))
        .and_then(|o| get_unop_string(&o, cfg))?;
    Ok(format!("({op}{rhs})"))
}

fn render_ast_terminal(terminal: &Value, cfg: &ExprConfig) -> String {
    // Get terminal_type and token
    let term_type = terminal
        .get_attr("terminal_type")
        .ok()
        .map(|t| t.to_string())
        .unwrap_or_default();
    let token = terminal.get_attr("token").ok();

    let text = token
        .as_ref()
        .and_then(|t| t.get_attr("text").ok())
        .map(|t| t.to_string())
        .unwrap_or_default();

    match term_type.as_str() {
        "UnsignedInteger" | "UnsignedReal" => text,
        "\"True\"" | "True" => cfg.true_val.clone(),
        "\"False\"" | "False" => cfg.false_val.clone(),
        "String" => format!("\"{}\"", text),
        _ => text.replace('.', "_"), // Identifier - sanitize dots
    }
}

fn render_ast_if_expr(if_expr: &Value, cfg: &ExprConfig) -> RenderResult {
    let cond = if_expr
        .get_attr("cond")
        .and_then(|c| render_ast_expression(&c, cfg))
        .unwrap_or_else(|_| "true".to_string());
    let then_expr = if_expr
        .get_attr("then_expr")
        .and_then(|t| render_ast_expression(&t, cfg))
        .unwrap_or_else(|_| "0".to_string());
    let else_expr = if_expr
        .get_attr("else_expr")
        .and_then(|e| render_ast_expression(&e, cfg))
        .unwrap_or_else(|_| "0".to_string());

    Ok(match cfg.if_style {
        IfStyle::Function => {
            let fn_name = cfg.if_else_fn.as_deref().unwrap_or("if_else");
            format!(
                "{}{}({}, {}, {})",
                cfg.prefix, fn_name, cond, then_expr, else_expr
            )
        }
        IfStyle::Ternary => format!("({} ? {} : {})", cond, then_expr, else_expr),
        IfStyle::Modelica => {
            format!("(if {} then {} else {})", cond, then_expr, else_expr)
        }
    })
}

fn render_ast_func_call(func_call: &Value, cfg: &ExprConfig) -> RenderResult {
    let name = func_call
        .get_attr("name")
        .map(|n| render_component_ref(&n))
        .unwrap_or_default();
    let args = func_call
        .get_attr("args")
        .and_then(|a| render_ast_args(&a, cfg))
        .unwrap_or_default();
    Ok(format!("{}({})", name, args))
}

fn render_ast_args(args: &Value, cfg: &ExprConfig) -> RenderResult {
    let Some(len) = args.len() else {
        return Ok(String::new());
    };

    let mut arg_strs = Vec::new();
    for i in 0..len {
        if let Ok(arg) = args.get_item(&Value::from(i)) {
            arg_strs.push(render_ast_expression(&arg, cfg)?);
        }
    }

    Ok(arg_strs.join(", "))
}

fn render_ast_array(array: &Value, cfg: &ExprConfig) -> RenderResult {
    let elements = array.get_attr("elements").ok();
    if let Some(ref elems) = elements
        && let Some(len) = elems.len()
    {
        let mut elem_strs = Vec::new();
        for i in 0..len {
            if let Ok(e) = elems.get_item(&Value::from(i)) {
                elem_strs.push(render_ast_expression(&e, cfg)?);
            }
        }
        return Ok(format!(
            "{}{}{}",
            cfg.array_start,
            elem_strs.join(", "),
            cfg.array_end
        ));
    }
    Ok(format!("{}{}", cfg.array_start, cfg.array_end))
}

fn render_ast_tuple(tuple: &Value, cfg: &ExprConfig) -> RenderResult {
    let elements = tuple.get_attr("elements").ok();
    if let Some(ref elems) = elements
        && let Some(len) = elems.len()
    {
        let mut elem_strs = Vec::new();
        for i in 0..len {
            if let Ok(e) = elems.get_item(&Value::from(i)) {
                elem_strs.push(render_ast_expression(&e, cfg)?);
            }
        }
        return Ok(format!("({})", elem_strs.join(", ")));
    }
    Ok("()".to_string())
}

fn render_ast_range(range: &Value, cfg: &ExprConfig) -> RenderResult {
    let start = range
        .get_attr("start")
        .and_then(|s| render_ast_expression(&s, cfg))
        .unwrap_or_else(|_| "1".to_string());
    let end = range
        .get_attr("end")
        .and_then(|e| render_ast_expression(&e, cfg))
        .unwrap_or_else(|_| "1".to_string());
    let step = range.get_attr("step").ok();

    if let Some(ref step_val) = step
        && !step_val.is_none()
    {
        let step_str = render_ast_expression(step_val, cfg)?;
        return Ok(format!("{}:{}:{}", start, step_str, end));
    }
    Ok(format!("{}:{}", start, end))
}

fn render_ast_named_arg(named: &Value, cfg: &ExprConfig) -> RenderResult {
    let name = named
        .get_attr("name")
        .ok()
        .and_then(|n| n.get_attr("text").ok())
        .map(|t| t.to_string())
        .unwrap_or_default();
    let value = named
        .get_attr("value")
        .and_then(|v| render_ast_expression(&v, cfg))
        .unwrap_or_default();
    Ok(format!("{name}={value}"))
}
