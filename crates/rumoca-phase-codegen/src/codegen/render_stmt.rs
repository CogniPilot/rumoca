//! Statement and equation rendering.
//!
//! This module handles rendering of:
//! - DAE equations (`render_equation`, `render_flat_equation`)
//! - Algorithm statements (`render_statement`, `render_statements`)
//! - AST expression variants used within statements

use super::render_expr::{
    get_binop_string, get_field, get_unop_string, is_exp_op, is_mul_elem_op, is_variant,
    render_args, render_expression,
};
use super::{ExprConfig, IfStyle, RenderResult};
use crate::errors::render_err;
use minijinja::Value;

// ── Equation rendering ───────────────────────────────────────────────

fn no_statement_render_match<T>() -> Result<Option<T>, minijinja::Error> {
    Ok(Option::None)
}

fn required_attr(
    node: &Value,
    attr: &'static str,
    context: &'static str,
) -> Result<Value, minijinja::Error> {
    let value = node
        .get_attr(attr)
        .map_err(|err| render_err(format!("{context} missing required `{attr}` field: {err}")))?;
    if value.is_none() || value.is_undefined() {
        return Err(render_err(format!(
            "{context} missing required `{attr}` field"
        )));
    }
    Ok(value)
}

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
            .map_err(|err| render_err(format!("explicit equation RHS failed to render: {err}")))?;
        return Ok(format!("{lhs_str} = {rhs_str}"));
    }

    // Residual form: try to decompose top-level Binary Sub into lhs = rhs
    let rhs = required_attr(eq, "rhs", "equation")?;
    if let Ok(binary) = get_field(&rhs, "Binary")
        && is_sub_op(&binary)
    {
        let lhs_expr = get_field(&binary, "lhs")
            .and_then(|v| render_expression(&v, cfg))
            .map_err(|err| render_err(format!("residual equation LHS failed to render: {err}")))?;
        let rhs_expr = get_field(&binary, "rhs")
            .and_then(|v| render_expression(&v, cfg))
            .map_err(|err| render_err(format!("residual equation RHS failed to render: {err}")))?;
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
    let residual = required_attr(eq, "residual", "flat equation")?;
    if let Ok(binary) = get_field(&residual, "Binary")
        && is_sub_op(&binary)
    {
        let lhs_expr = get_field(&binary, "lhs")
            .and_then(|v| render_expression(&v, cfg))
            .map_err(|err| {
                render_err(format!(
                    "flat residual equation LHS failed to render: {err}"
                ))
            })?;
        let rhs_expr = get_field(&binary, "rhs")
            .and_then(|v| render_expression(&v, cfg))
            .map_err(|err| {
                render_err(format!(
                    "flat residual equation RHS failed to render: {err}"
                ))
            })?;
        return Ok(format!("{lhs_expr} = {rhs_expr}"));
    }

    // Fallback: 0 = expression
    let expr_str = render_expression(&residual, cfg)?;
    Ok(format!("0 = {expr_str}"))
}

/// Render the LHS of an explicit equation (VarName).
fn render_explicit_lhs(lhs: &Value, cfg: &ExprConfig) -> String {
    let raw = super::render_expr::render_serialized_name(lhs);
    super::emitted_symbol_or_fallback(&raw, cfg)
}

/// Check if a Binary expression's op is Sub or SubElem.
fn is_sub_op(binary: &Value) -> bool {
    if let Ok(op) = get_field(binary, "op") {
        return is_variant(&op, "Sub") || is_variant(&op, "SubElem");
    }
    false
}

// ── Statement rendering ──────────────────────────────────────────────

/// Render a list of statements to a string.
pub(crate) fn render_statements(stmts: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
    let len = stmts
        .len()
        .ok_or_else(|| render_err("statement list is not a sequence"))?;

    let mut stmt_strs = Vec::new();
    for i in 0..len {
        let stmt = stmts
            .get_item(&Value::from(i))
            .map_err(|err| render_err(format!("statement {i} is inaccessible: {err}")))?;
        if stmt.is_undefined() || stmt.is_none() {
            return Err(render_err(format!("statement {i} is missing")));
        }
        let s = render_statement(&stmt, cfg, indent)?;
        if !s.is_empty() {
            stmt_strs.push(s);
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
    if let Ok(spanned) = get_field(stmt, "Spanned") {
        let inner = get_field(&spanned, "statement")
            .map_err(|_| render_err(format!("Spanned statement missing 'statement': {stmt}")))?;
        return render_statement(&inner, cfg, indent);
    }
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
    let comp = render_component_ref(&comp_val, cfg)?;
    if comp.trim().is_empty() {
        return Err(render_err(format!(
            "Assignment target resolved to empty component reference: {assign}"
        )));
    }

    let value_node = get_field(assign, "value")
        .map_err(|_| render_err(format!("Assignment missing 'value' field: {assign}")))?;

    // For C backends (IfStyle::Ternary), array assignment `x = {a, b, c}` is illegal.
    // Instead, emit element-wise assignments: `x[0] = a; x[1] = b; x[2] = c;`
    if matches!(cfg.if_style, IfStyle::Ternary)
        && let Some(elem_strs) = try_extract_array_elements(&value_node, cfg)?
    {
        let mut lines = Vec::new();
        for (i, elem) in elem_strs.iter().enumerate() {
            lines.push(format!("{indent}{comp}[{i}] = {elem};"));
        }
        return Ok(lines.join("\n"));
    }

    let value = render_expression(&value_node, cfg)?;
    let semi = if matches!(cfg.if_style, IfStyle::Ternary | IfStyle::Modelica) {
        ";"
    } else {
        ""
    };
    Ok(format!("{indent}{comp} = {value}{semi}"))
}

/// Try to extract array elements from a value node.
/// Returns `Some(vec_of_rendered_elements)` if the value is an Array expression,
/// `None` otherwise. Works for both DAE IR and AST Array nodes (both serialize as
/// `{"Array": {"elements": [...]}}`).
fn try_extract_array_elements(
    value: &Value,
    cfg: &ExprConfig,
) -> Result<Option<Vec<String>>, minijinja::Error> {
    if let Ok(array) = get_field(value, "Array")
        && let Ok(elements) = get_field(&array, "elements")
        && let Some(len) = elements.len()
    {
        let mut strs = Vec::with_capacity(len);
        for i in 0..len {
            if let Ok(elem) = elements.get_item(&Value::from(i)) {
                // Try DAE IR renderer first, fall back to AST renderer
                let rendered =
                    render_expression(&elem, cfg).or_else(|_| render_ast_expression(&elem, cfg))?;
                strs.push(rendered);
            }
        }
        return Ok(Some(strs));
    }
    no_statement_render_match()
}

/// Render a for loop statement.
fn render_for_statement(for_stmt: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
    let mut result = String::new();

    // Get indices (loop variables)
    let indices = for_stmt.get_attr("indices").ok();
    let equations = for_stmt.get_attr("equations").ok();

    // Extract first index (simplification: handle one index for now)
    let (loop_var, range_parts) = extract_for_loop_index(&indices, cfg)?;

    // Generate for loop header based on config
    match cfg.if_style {
        IfStyle::Ternary => {
            // C-style for loop: Modelica `for i in start:end` → `for (int i = start; i <= end; i++)`
            // Modelica `for i in start:step:end` → `for (int i = start; i <= end; i += step)`
            match &range_parts {
                ForRange::StartEnd { start, end } => {
                    result.push_str(&format!(
                        "{indent}for (int {loop_var} = {start}; {loop_var} <= {end}; {loop_var}++) {{\n"
                    ));
                }
                ForRange::StartStepEnd { start, step, end } => {
                    result.push_str(&format!(
                        "{indent}for (int {loop_var} = {start}; {loop_var} <= {end}; {loop_var} += {step}) {{\n"
                    ));
                }
                ForRange::Raw(raw) => {
                    result.push_str(&format!(
                        "{indent}for (int {loop_var} = 0; {loop_var} < /* {raw} */; {loop_var}++) {{\n"
                    ));
                }
            }
        }
        IfStyle::Function => {
            // Python-style for loop: Modelica `for i in start:end` → `for i in range(start, end + 1)`
            // Python range() is exclusive on the upper bound, so end+1.
            match &range_parts {
                ForRange::StartEnd { start, end } => {
                    result.push_str(&format!(
                        "{indent}for {loop_var} in range({start}, {end} + 1):\n"
                    ));
                }
                ForRange::StartStepEnd { start, step, end } => {
                    result.push_str(&format!(
                        "{indent}for {loop_var} in range({start}, {end} + 1, {step}):\n"
                    ));
                }
                ForRange::Raw(raw) => {
                    result.push_str(&format!("{indent}for {loop_var} in range({raw}):\n"));
                }
            }
        }
        IfStyle::Modelica => {
            let range_str = match &range_parts {
                ForRange::StartEnd { start, end } => format!("{start}:{end}"),
                ForRange::StartStepEnd { start, step, end } => {
                    format!("{start}:{step}:{end}")
                }
                ForRange::Raw(raw) => raw.clone(),
            };
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

/// Structured representation of a for-loop range.
enum ForRange {
    /// `start:end` — simple range (Modelica `for i in 1:10`)
    StartEnd { start: String, end: String },
    /// `start:step:end` — stepped range (Modelica `for i in 1:2:10`)
    StartStepEnd {
        start: String,
        step: String,
        end: String,
    },
    /// Fallback: raw rendered string (non-range expression)
    Raw(String),
}

/// Extract loop variable and structured range from for loop indices.
fn extract_for_loop_index(
    indices: &Option<Value>,
    cfg: &ExprConfig,
) -> Result<(String, ForRange), minijinja::Error> {
    let default = (
        "i".to_string(),
        ForRange::StartEnd {
            start: "1".to_string(),
            end: "1".to_string(),
        },
    );

    let Some(indices_val) = indices else {
        return Ok(default);
    };
    let Ok(first) = indices_val.get_item(&Value::from(0)) else {
        return Ok(default);
    };

    let ident = first
        .get_attr("ident")
        .ok()
        .map(|i| {
            // Try AST Token form (ident.text) first
            if let Ok(text) = i.get_attr("text") {
                let s = text.to_string();
                if !s.is_empty() {
                    return s;
                }
            }
            // Fall back to DAE string form (plain String value)
            let s = i.to_string();
            let trimmed = s.trim_matches('"').to_string();
            if trimmed.is_empty() {
                "i".to_string()
            } else {
                trimmed
            }
        })
        .unwrap_or_else(|| "i".to_string());

    let range = if let Ok(range_val) = first.get_attr("range") {
        extract_for_range(&range_val, cfg)?
    } else {
        ForRange::StartEnd {
            start: "1".to_string(),
            end: "1".to_string(),
        }
    };

    Ok((ident, range))
}

/// Try to decompose a range expression into structured ForRange parts.
fn extract_for_range(range_val: &Value, cfg: &ExprConfig) -> Result<ForRange, minijinja::Error> {
    // Check if this is an AST Range node (has start/end attributes)
    if let Ok(range_node) = get_field(range_val, "Range") {
        return extract_for_range_from_ast_range(&range_node, cfg);
    }
    // The value itself might be a Range node (not wrapped)
    if let Ok(start_val) = range_val.get_attr("start")
        && !start_val.is_undefined()
        && !start_val.is_none()
    {
        return extract_for_range_from_ast_range(range_val, cfg);
    }
    // Fallback: render as raw expression
    let raw = render_ast_expression(range_val, cfg)?;
    // Try to parse colon-separated range from the rendered string
    if let Some(parts) = parse_colon_range(&raw) {
        return Ok(parts);
    }
    Ok(ForRange::Raw(raw))
}

/// Extract ForRange from an AST Range node with start/end/step attributes.
fn extract_for_range_from_ast_range(
    range_node: &Value,
    cfg: &ExprConfig,
) -> Result<ForRange, minijinja::Error> {
    let start = range_node
        .get_attr("start")
        .and_then(|s| render_ast_expression(&s, cfg))
        .unwrap_or_else(|_| "1".to_string());
    let end = range_node
        .get_attr("end")
        .and_then(|e| render_ast_expression(&e, cfg))
        .unwrap_or_else(|_| "1".to_string());
    let step = range_node.get_attr("step").ok();

    if let Some(ref step_val) = step
        && !step_val.is_none()
        && !step_val.is_undefined()
    {
        let step_str = render_ast_expression(step_val, cfg)?;
        Ok(ForRange::StartStepEnd {
            start,
            step: step_str,
            end,
        })
    } else {
        Ok(ForRange::StartEnd { start, end })
    }
}

/// Try to parse a colon-separated range string like "1:13" or "1:2:13".
fn parse_colon_range(s: &str) -> Option<ForRange> {
    let parts: Vec<&str> = s.split(':').collect();
    match parts.len() {
        2 => Some(ForRange::StartEnd {
            start: parts[0].trim().to_string(),
            end: parts[1].trim().to_string(),
        }),
        3 => Some(ForRange::StartStepEnd {
            start: parts[0].trim().to_string(),
            step: parts[1].trim().to_string(),
            end: parts[2].trim().to_string(),
        }),
        _ => None,
    }
}

/// Render a while loop statement.
fn render_while_statement(while_stmt: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
    let mut result = String::new();

    let cond = while_stmt
        .get_attr("cond")
        .and_then(|c| render_ast_expression(&c, cfg))
        .unwrap_or_else(|_| "true".to_string());
    let stmts = statement_block_statements(while_stmt);

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
    let len = blocks
        .len()
        .ok_or_else(|| render_err("If condition blocks is not a sequence"))?;

    let mut result = String::new();
    for i in 0..len {
        let block = blocks
            .get_item(&Value::from(i))
            .map_err(|err| render_err(format!("If condition block {i} is inaccessible: {err}")))?;
        if block.is_undefined() || block.is_none() {
            return Err(render_err(format!("If condition block {i} is missing")));
        }
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
    let stmts = statement_block_statements(block);

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
        result.push_str(&format!("{next_indent}pass\n"));
    } else {
        result.push_str(&body);
    }

    if matches!(cfg.if_style, IfStyle::Ternary) {
        result.push_str(&format!("\n{indent}}}"));
    }

    Ok(result)
}

fn statement_block_statements(block: &Value) -> Option<Value> {
    block
        .get_attr("stmts")
        .ok()
        .or_else(|| block.get_attr("statements").ok())
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
    let comp_val = func_call.get_attr("comp").map_err(|_| {
        render_err(format!(
            "FunctionCall statement missing 'comp': {func_call}"
        ))
    })?;
    if comp_val.is_undefined() || comp_val.is_none() {
        return Err(render_err(format!(
            "FunctionCall statement missing 'comp': {func_call}"
        )));
    }
    let comp = render_component_ref(&comp_val, cfg)?;
    if comp.trim().is_empty() {
        return Err(render_err(format!(
            "FunctionCall statement target resolved to empty component reference: {func_call}"
        )));
    }

    let args = render_args(func_call, cfg).map_err(|err| {
        render_err(format!(
            "FunctionCall statement args failed to render: {err}"
        ))
    })?;

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
        return no_statement_render_match();
    };
    let Some(len) = outputs.len() else {
        return no_statement_render_match();
    };
    if len == 0 {
        return no_statement_render_match();
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
    let variable = reinit
        .get_attr("variable")
        .map_err(|_| render_err(format!("Reinit statement missing 'variable': {reinit}")))?;
    let var = render_component_ref(&variable, cfg)?;
    if var.trim().is_empty() {
        return Err(render_err(format!(
            "Reinit target resolved to empty component reference: {reinit}"
        )));
    }
    let value = reinit
        .get_attr("value")
        .and_then(|v| render_ast_expression(&v, cfg))
        .map_err(|err| render_err(format!("Reinit value failed to render: {err}")))?;
    Ok(format!("{indent}/* reinit({var}, {value}) */"))
}

/// Render an assert statement.
fn render_assert_statement(assert: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
    let cond = assert
        .get_attr("condition")
        .and_then(|c| render_ast_expression(&c, cfg))
        .map_err(|err| render_err(format!("Assert condition failed to render: {err}")))?;
    let msg = match assert.get_attr("message") {
        Ok(message) if !message.is_undefined() && !message.is_none() => {
            render_ast_expression(&message, cfg)
                .map_err(|err| render_err(format!("Assert message failed to render: {err}")))?
        }
        _ => "\"assertion failed\"".to_string(),
    };

    Ok(match cfg.if_style {
        IfStyle::Ternary => format!("{indent}assert({cond}); /* {msg} */"),
        IfStyle::Function => format!("{indent}assert {cond}, {msg}"),
        IfStyle::Modelica => format!("{indent}assert({cond}, {msg});"),
    })
}

// ── Component reference rendering ────────────────────────────────────

/// Render an AST ComponentReference to a string.
fn render_component_ref(comp: &Value, cfg: &ExprConfig) -> RenderResult {
    if let Some(s) = comp.as_str() {
        return Ok(super::emitted_symbol_or_fallback(s, cfg));
    }

    let parts_val = get_field(comp, "parts")
        .map_err(|_| render_err(format!("ComponentReference missing 'parts': {comp}")))?;
    let len = parts_val.len().ok_or_else(|| {
        render_err(format!(
            "ComponentReference parts is not a sequence: {comp}"
        ))
    })?;

    let mut part_strs = Vec::new();
    for i in 0..len {
        let part = parts_val
            .get_item(&Value::from(i))
            .map_err(|err| render_err(format!("ComponentReference part {i} missing: {err}")))?;
        if part.is_undefined() || part.is_none() {
            return Err(render_err(format!("ComponentReference part {i} missing")));
        }
        part_strs.push(render_component_ref_part(&part, cfg)?);
    }

    let joined = part_strs.join(".");
    let joined = joined.replace(' ', "");
    if joined.trim().is_empty() {
        return Err(render_err(format!(
            "ComponentReference resolved to empty name: {comp}"
        )));
    }
    Ok(super::emitted_symbol_or_fallback(&joined, cfg))
}

/// Render a single component reference part (identifier + optional subscripts).
fn render_component_ref_part(part: &Value, cfg: &ExprConfig) -> RenderResult {
    let ident_value = get_field(part, "ident")
        .map_err(|_| render_err(format!("ComponentReference part missing 'ident': {part}")))?;
    let ident = if let Ok(text) = ident_value.get_attr("text")
        && !text.is_undefined()
        && !text.is_none()
    {
        text.to_string()
    } else {
        ident_value
            .as_str()
            .map(ToOwned::to_owned)
            .unwrap_or_else(|| ident_value.to_string())
    };
    if ident.trim().is_empty() {
        return Err(render_err(format!(
            "ComponentReference part resolved to empty ident: {part}"
        )));
    }

    let sub_str = render_part_subscripts(part, cfg)?;
    if sub_str.is_empty() {
        Ok(ident)
    } else {
        Ok(format!("{}[{}]", ident, sub_str))
    }
}

/// Render subscripts for a component reference part.
fn render_part_subscripts(part: &Value, cfg: &ExprConfig) -> RenderResult {
    let subs_val = get_field(part, "subscripts")
        .ok()
        .or_else(|| get_field(part, "subs").ok());
    let Some(subs_val) = subs_val else {
        return Ok(String::new());
    };
    let sub_len = subs_val.len().ok_or_else(|| {
        render_err(format!(
            "ComponentReference subscripts is not a sequence: {part}"
        ))
    })?;
    if sub_len == 0 {
        return Ok(String::new());
    }

    let mut sub_strs = Vec::new();
    for j in 0..sub_len {
        let sub = subs_val.get_item(&Value::from(j)).map_err(|err| {
            render_err(format!("ComponentReference subscript {j} missing: {err}"))
        })?;
        if sub.is_undefined() || sub.is_none() {
            return Err(render_err(format!(
                "ComponentReference subscript {j} missing"
            )));
        }
        sub_strs.push(render_ast_subscript(&sub, cfg)?);
    }

    Ok(sub_strs.join(","))
}

/// Render an AST subscript.
fn render_ast_subscript(sub: &Value, cfg: &ExprConfig) -> RenderResult {
    if let Ok(index) = get_field(sub, "Index") {
        return Ok(index.to_string());
    }
    if let Ok(expr) = get_field(sub, "Expr") {
        return render_expression(&expr, cfg);
    }
    // Subscripts can be Expression, Range, or Empty (colon)
    if let Ok(expr) = get_field(sub, "Expression") {
        return render_ast_expression(&expr, cfg);
    }
    if sub.as_str() == Some("Empty") || get_field(sub, "Colon").is_ok() {
        return Ok(":".to_string());
    }
    Err(render_err(format!(
        "unhandled AST subscript variant: {sub}"
    )))
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
        return render_ast_terminal(&terminal, cfg);
    }
    if let Ok(comp_ref) = get_field(expr, "ComponentReference") {
        return render_component_ref(&comp_ref, cfg);
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
    // DAE-style expressions like VarRef, Literal, BuiltinCall, etc.).
    render_expression(expr, cfg)
        .map_err(|err| render_err(format!("unhandled AST expression variant {expr}: {err}")))
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
    // Use function-call form for logical operators when the op string
    // looks like a qualified function name (e.g. "ca.logic_and").
    if is_variant(&op, "And") && rumoca_core::has_top_level_dot(&cfg.and_op) {
        return Ok(format!("{}({}, {})", cfg.and_op, lhs, rhs));
    }
    if is_variant(&op, "Or") && rumoca_core::has_top_level_dot(&cfg.or_op) {
        return Ok(format!("{}({}, {})", cfg.or_op, lhs, rhs));
    }
    if is_exp_op(&op) {
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

fn render_ast_terminal(terminal: &Value, cfg: &ExprConfig) -> RenderResult {
    let term_type = get_field(terminal, "terminal_type")
        .map(|t| super::value_to_string(&t))
        .map_err(|err| render_err(format!("AST Terminal missing 'terminal_type': {err}")))?;

    match term_type.as_str() {
        "\"True\"" | "True" => return Ok(cfg.true_val.clone()),
        "\"False\"" | "False" => return Ok(cfg.false_val.clone()),
        _ => {}
    }

    let token = get_field(terminal, "token")
        .map_err(|err| render_err(format!("AST Terminal missing 'token': {err}")))?;
    let text = get_field(&token, "text")
        .map(|t| super::value_to_string(&t))
        .map_err(|err| render_err(format!("AST Terminal missing token text: {err}")))?;
    if text.is_empty() {
        return Err(render_err("AST Terminal token text is empty"));
    }

    match term_type.as_str() {
        "UnsignedInteger" | "UnsignedReal" => Ok(text),
        "String" => Ok(format!("\"{}\"", text)),
        _ => Ok(text.replace('.', "_")), // Identifier - sanitize dots
    }
}

fn render_ast_if_expr(if_expr: &Value, cfg: &ExprConfig) -> RenderResult {
    let cond = if_expr
        .get_attr("cond")
        .and_then(|c| render_ast_expression(&c, cfg))
        .map_err(|err| {
            render_err(format!(
                "AST If expression condition failed to render: {err}"
            ))
        })?;
    let then_expr = if_expr
        .get_attr("then_expr")
        .and_then(|t| render_ast_expression(&t, cfg))
        .map_err(|err| {
            render_err(format!(
                "AST If expression then branch failed to render: {err}"
            ))
        })?;
    let else_expr = if_expr
        .get_attr("else_expr")
        .and_then(|e| render_ast_expression(&e, cfg))
        .map_err(|err| {
            render_err(format!(
                "AST If expression else branch failed to render: {err}"
            ))
        })?;

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
    let name_value = func_call
        .get_attr("name")
        .map_err(|_| render_err(format!("AST FunctionCall missing 'name': {func_call}")))?;
    if name_value.is_undefined() || name_value.is_none() {
        return Err(render_err(format!(
            "AST FunctionCall missing 'name': {func_call}"
        )));
    }
    let name = render_component_ref(&name_value, cfg)?;
    if name.trim().is_empty() {
        return Err(render_err(format!(
            "AST FunctionCall name resolved to empty component reference: {func_call}"
        )));
    }
    let args = match func_call.get_attr("args") {
        Ok(args) if !args.is_undefined() && !args.is_none() => render_ast_args(&args, cfg)?,
        _ => String::new(),
    };
    Ok(format!("{}({})", name, args))
}

fn render_ast_args(args: &Value, cfg: &ExprConfig) -> RenderResult {
    let len = args
        .len()
        .ok_or_else(|| render_err(format!("AST function-call args is not a sequence: {args}")))?;

    let mut arg_strs = Vec::new();
    for i in 0..len {
        let arg = args
            .get_item(&Value::from(i))
            .map_err(|err| render_err(format!("AST function-call arg {i} missing: {err}")))?;
        if arg.is_undefined() || arg.is_none() {
            return Err(render_err(format!("AST function-call arg {i} missing")));
        }
        arg_strs.push(render_ast_expression(&arg, cfg)?);
    }

    Ok(arg_strs.join(", "))
}

fn render_ast_array(array: &Value, cfg: &ExprConfig) -> RenderResult {
    let elems = get_field(array, "elements")
        .map_err(|_| render_err(format!("AST Array missing 'elements': {array}")))?;
    let len = elems
        .len()
        .ok_or_else(|| render_err(format!("AST Array elements is not a sequence: {array}")))?;
    let mut elem_strs = Vec::new();
    for i in 0..len {
        let elem = elems
            .get_item(&Value::from(i))
            .map_err(|err| render_err(format!("AST Array element {i} missing: {err}")))?;
        if elem.is_undefined() || elem.is_none() {
            return Err(render_err(format!("AST Array element {i} missing")));
        }
        elem_strs.push(render_ast_expression(&elem, cfg)?);
    }
    Ok(format!(
        "{}{}{}",
        cfg.array_start,
        elem_strs.join(", "),
        cfg.array_end
    ))
}

fn render_ast_tuple(tuple: &Value, cfg: &ExprConfig) -> RenderResult {
    let elems = get_field(tuple, "elements")
        .map_err(|_| render_err(format!("AST Tuple missing 'elements': {tuple}")))?;
    let len = elems
        .len()
        .ok_or_else(|| render_err(format!("AST Tuple elements is not a sequence: {tuple}")))?;
    let mut elem_strs = Vec::new();
    for i in 0..len {
        let elem = elems
            .get_item(&Value::from(i))
            .map_err(|err| render_err(format!("AST Tuple element {i} missing: {err}")))?;
        if elem.is_undefined() || elem.is_none() {
            return Err(render_err(format!("AST Tuple element {i} missing")));
        }
        elem_strs.push(render_ast_expression(&elem, cfg)?);
    }
    Ok(format!("({})", elem_strs.join(", ")))
}

fn render_ast_range(range: &Value, cfg: &ExprConfig) -> RenderResult {
    let start = range
        .get_attr("start")
        .and_then(|s| render_ast_expression(&s, cfg))
        .map_err(|err| render_err(format!("AST Range start failed to render: {err}")))?;
    let end = range
        .get_attr("end")
        .and_then(|e| render_ast_expression(&e, cfg))
        .map_err(|err| render_err(format!("AST Range end failed to render: {err}")))?;
    let step = range.get_attr("step").ok();

    if cfg.python_range {
        let end_plus1 = python_range_end(&end);
        if let Some(ref step_val) = step
            && !step_val.is_none()
        {
            let step_str = render_ast_expression(step_val, cfg)?;
            return Ok(format!("range({}, {}, {})", start, end_plus1, step_str));
        }
        return Ok(format!("range({}, {})", start, end_plus1));
    }

    if let Some(ref step_val) = step
        && !step_val.is_none()
    {
        let step_str = render_ast_expression(step_val, cfg)?;
        return Ok(format!("{}:{}:{}", start, step_str, end));
    }
    Ok(format!("{}:{}", start, end))
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

fn render_ast_named_arg(named: &Value, cfg: &ExprConfig) -> RenderResult {
    let name = named
        .get_attr("name")
        .map_err(|_| render_err(format!("AST named argument missing 'name': {named}")))?
        .get_attr("text")
        .map_err(|_| {
            render_err(format!(
                "AST named argument name missing token text: {named}"
            ))
        })?
        .to_string();
    if name.trim().is_empty() {
        return Err(render_err(format!(
            "AST named argument resolved to empty name: {named}"
        )));
    }
    let value = named
        .get_attr("value")
        .and_then(|v| render_ast_expression(&v, cfg))
        .map_err(|err| render_err(format!("AST named argument value failed to render: {err}")))?;
    Ok(format!("{name}={value}"))
}
