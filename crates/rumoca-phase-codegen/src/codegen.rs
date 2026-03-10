//! Code generation implementation.
//!
//! This module provides a simple template rendering function. The DAE is
//! serialized and passed directly to minijinja templates, which can then
//! walk the expression tree and generate code as needed.
//!
//! For common cases, templates can use the built-in `render_expr` function
//! which handles the recursive tree walking with configurable operator syntax.

use crate::errors::{CodegenError, render_err};
use minijinja::{Environment, UndefinedBehavior, Value};
use rumoca_ir_ast as ast;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use serde_json::json;
use std::path::Path;

/// Result type for internal render functions.
type RenderResult = Result<String, minijinja::Error>;

/// Supported IR roots for template rendering.
#[derive(Debug, Clone, Copy)]
pub enum CodegenInput<'a> {
    Dae(&'a dae::Dae),
    Flat(&'a flat::Model),
    Ast(&'a ast::ClassTree),
}

pub fn dae_template_json(dae: &dae::Dae) -> serde_json::Value {
    json!({
        // Long-form names used by existing templates.
        "states": &dae.states,
        "algebraics": &dae.algebraics,
        "inputs": &dae.inputs,
        "outputs": &dae.outputs,
        "parameters": &dae.parameters,
        "constants": &dae.constants,
        "discrete_reals": &dae.discrete_reals,
        "discrete_valued": &dae.discrete_valued,
        "derivative_aliases": &dae.derivative_aliases,
        // Canonical short-form aliases (MLS B.1 notation).
        "x": &dae.states,
        "y": &dae.algebraics,
        "u": &dae.inputs,
        "w": &dae.outputs,
        "p": &dae.parameters,
        "z": &dae.discrete_reals,
        "m": &dae.discrete_valued,
        "x_dot_alias": &dae.derivative_aliases,
        // Equations and metadata.
        "f_x": &dae.f_x,
        "f_z": &dae.f_z,
        "f_m": &dae.f_m,
        "f_c": &dae.f_c,
        "relation": &dae.relation,
        "initial_equations": &dae.initial_equations,
        "functions": &dae.functions,
        "enum_literal_ordinals": &dae.enum_literal_ordinals,
        "interface_flow_count": dae.interface_flow_count,
        "overconstrained_interface_count": dae.overconstrained_interface_count,
        "oc_break_edge_scalar_count": dae.oc_break_edge_scalar_count,
        "is_partial": dae.is_partial,
        "class_type": &dae.class_type,
        "model_description": dae.model_description,
    })
}

fn dae_template_value(dae: &dae::Dae) -> Value {
    Value::from_serialize(dae_template_json(dae))
}

fn render_with_input_context(
    tmpl: &minijinja::Template<'_, '_>,
    input: CodegenInput<'_>,
    model_name: Option<&str>,
) -> Result<String, CodegenError> {
    let rendered = match (input, model_name) {
        (CodegenInput::Dae(dae_model), None) => {
            let dae_value = dae_template_value(dae_model);
            tmpl.render(minijinja::context! {
                dae => dae_value.clone(),
                ir => dae_value,
                ir_kind => "dae",
            })?
        }
        (CodegenInput::Dae(dae_model), Some(name)) => {
            let dae_value = dae_template_value(dae_model);
            tmpl.render(minijinja::context! {
                dae => dae_value.clone(),
                ir => dae_value,
                ir_kind => "dae",
                model_name => name,
            })?
        }
        (CodegenInput::Flat(flat_model), None) => {
            let flat_value = Value::from_serialize(flat_model);
            tmpl.render(minijinja::context! {
                flat => flat_value.clone(),
                ir => flat_value,
                ir_kind => "flat",
            })?
        }
        (CodegenInput::Flat(flat_model), Some(name)) => {
            let flat_value = Value::from_serialize(flat_model);
            tmpl.render(minijinja::context! {
                flat => flat_value.clone(),
                ir => flat_value,
                ir_kind => "flat",
                model_name => name,
            })?
        }
        (CodegenInput::Ast(ast_tree), None) => {
            let ast_value = Value::from_serialize(ast_tree);
            tmpl.render(minijinja::context! {
                ast => ast_value.clone(),
                ir => ast_value,
                ir_kind => "ast",
            })?
        }
        (CodegenInput::Ast(ast_tree), Some(name)) => {
            let ast_value = Value::from_serialize(ast_tree);
            tmpl.render(minijinja::context! {
                ast => ast_value.clone(),
                ir => ast_value,
                ir_kind => "ast",
                model_name => name,
            })?
        }
    };
    Ok(rendered)
}

/// Render any supported IR using a template string.
pub fn render_template_for_input(
    input: CodegenInput<'_>,
    template: &str,
) -> Result<String, CodegenError> {
    let mut env = create_environment();
    env.add_template("inline", template)?;
    let tmpl = env.get_template("inline")?;
    render_with_input_context(&tmpl, input, None)
}

/// Render any supported IR using a template string, with model name.
pub fn render_template_with_name_for_input(
    input: CodegenInput<'_>,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    let mut env = create_environment();
    env.add_template("inline", template)?;
    let tmpl = env.get_template("inline")?;
    render_with_input_context(&tmpl, input, Some(model_name))
}

/// Render a DAE using a template string.
///
/// The template receives the full DAE structure as `dae` and can access
/// any field using standard Jinja2 syntax.
///
/// # Example Template
///
/// ```jinja
/// # States: {{ dae.states | length }}
/// {% for name, var in dae.states %}
/// {{ name | sanitize }} = Symbol('{{ name }}')
/// {% endfor %}
/// ```
///
/// # Built-in Functions
///
/// - `render_expr(expr, config)` - Render expression with operator config
///
/// # Available Filters
///
/// - `sanitize` - Replace dots with underscores
/// - Standard minijinja filters (length, upper, lower, etc.)
pub fn render_template(dae: &dae::Dae, template: &str) -> Result<String, CodegenError> {
    render_template_for_input(CodegenInput::Dae(dae), template)
}

/// Render a template using a pre-built `dae` JSON context object.
///
/// This is useful when callers need to augment the canonical DAE context with
/// additional template-only metadata.
pub fn render_template_with_dae_json(
    dae_json: &serde_json::Value,
    template: &str,
) -> Result<String, CodegenError> {
    let mut env = create_environment();
    env.add_template("inline", template)?;

    let dae_value = Value::from_serialize(dae_json);
    let tmpl = env.get_template("inline")?;
    let result = tmpl.render(minijinja::context! { dae => dae_value })?;

    Ok(result)
}

/// Render a DAE using a template string, with an additional model name in context.
///
/// The template receives both `dae` and `model_name` as context variables.
/// This is useful for templates that need the model name (e.g., flat Modelica output).
pub fn render_template_with_name(
    dae: &dae::Dae,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    render_template_with_name_for_input(CodegenInput::Dae(dae), template, model_name)
}

/// Render a DAE using a template file.
///
/// This is the recommended approach for customizable templates.
///
/// # Example
///
/// ```ignore
/// let code = render_template_file(&dae, "templates/casadi.py.jinja")?;
/// ```
pub fn render_template_file(
    dae: &dae::Dae,
    path: impl AsRef<Path>,
) -> Result<String, CodegenError> {
    let path_ref = path.as_ref();
    let template = std::fs::read_to_string(path_ref)
        .map_err(|e| CodegenError::template(format!("Failed to read template: {e}")))?;

    let mut env = create_environment();
    env.add_template("file", &template)?;

    let tmpl = env.get_template("file")?;
    render_with_input_context(&tmpl, CodegenInput::Dae(dae), None)
}

/// Render a Model using a template string, with an additional model name in context.
///
/// The template receives `flat` (the Model) and `model_name` as context variables.
/// This is used for rendering flat Modelica output for OMC comparison.
pub fn render_flat_template_with_name(
    flat: &flat::Model,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    render_template_with_name_for_input(CodegenInput::Flat(flat), template, model_name)
}

/// Render an AST class tree using a template string.
///
/// The template receives the AST structure as `ast`.
pub fn render_ast_template(ast: &ast::ClassTree, template: &str) -> Result<String, CodegenError> {
    render_template_for_input(CodegenInput::Ast(ast), template)
}

/// Render an AST class tree using a template string and model name.
///
/// The template receives both `ast` and `model_name`.
pub fn render_ast_template_with_name(
    ast: &ast::ClassTree,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    render_template_with_name_for_input(CodegenInput::Ast(ast), template, model_name)
}

/// Create a minijinja environment with all custom filters and functions.
fn create_environment() -> Environment<'static> {
    let mut env = Environment::new();
    // Fail fast on missing fields/variables in templates.
    env.set_undefined_behavior(UndefinedBehavior::Strict);

    // Custom filters
    env.add_filter("sanitize", sanitize_filter);
    env.add_filter("product", product_filter);

    // Custom functions for expression rendering
    env.add_function("render_expr", render_expr_function);
    env.add_function("render_equation", render_equation_function);

    // Custom functions for statement rendering (MLS §12: function bodies)
    env.add_function("render_statement", render_statement_function);
    env.add_function("render_statements", render_statements_function);

    // Custom function for flat equation rendering (Model residual equations)
    env.add_function("render_flat_equation", render_flat_equation_function);

    env
}

/// Filter to sanitize variable names (replace dots with underscores).
fn sanitize_filter(value: Value) -> String {
    value.to_string().replace('.', "_")
}

/// Filter to compute the product of all elements in a sequence.
///
/// Used by MX template: `{{ var.dims | product }}` → total scalar size.
fn product_filter(value: Value) -> Value {
    let Some(len) = value.len() else {
        return Value::from(1);
    };
    let mut result: i64 = 1;
    for i in 0..len {
        if let Ok(item) = value.get_item(&Value::from(i)) {
            result *= item.as_i64().unwrap_or(1);
        }
    }
    Value::from(result)
}

/// Built-in expression renderer function.
///
/// Usage in templates:
/// ```jinja
/// {{ render_expr(expr, config) }}
/// ```
///
/// The config object can contain:
/// - `prefix` - Prefix for function calls (e.g., "ca." for CasADi, "np." for numpy)
/// - `power` - Power operator syntax (e.g., "**" for Python, "^" for Julia)
/// - `and_op` - Logical AND (e.g., "and", "&&")
/// - `or_op` - Logical OR (e.g., "or", "||")
/// - `not_op` - Logical NOT (e.g., "not ", "!")
/// - `true_val` - True literal (e.g., "True", "true")
/// - `false_val` - False literal (e.g., "False", "false")
/// - `array_start` - Array literal start (e.g., "[", "{")
/// - `array_end` - Array literal end (e.g., "]", "}")
/// - `if_else` - If-else style: "python" (if_else(c,t,e)), "ternary" (c ? t : e), "julia" (c ? t : e)
/// - `mul_elem_fn` - Optional function for element-wise multiply (e.g., "ca.times")
fn render_expr_function(expr: Value, config: Value) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    render_expression(&expr, &cfg)
}

/// Render an equation in `lhs = rhs` form.
///
/// For explicit equations (lhs is set), renders `lhs = rhs`.
/// For residual equations (lhs is None), decomposes top-level subtraction
/// into `lhs_expr = rhs_expr`. Falls back to `0 = expr` if no subtraction.
///
/// Usage in templates:
/// ```jinja
/// {{ render_equation(eq, config) }}
/// ```
fn render_equation_function(eq: Value, config: Value) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    render_equation(&eq, &cfg)
}

/// Render an equation to `lhs = rhs` form.
fn render_equation(eq: &Value, cfg: &ExprConfig) -> RenderResult {
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
///
/// Usage in templates:
/// ```jinja
/// {{ render_flat_equation(eq, config) }}
/// ```
fn render_flat_equation_function(eq: Value, config: Value) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);

    let residual = eq.get_attr("residual").unwrap_or(Value::UNDEFINED);
    if let Ok(binary) = get_field(&residual, "Binary")
        && is_sub_op(&binary)
    {
        let lhs_expr = get_field(&binary, "lhs")
            .and_then(|v| render_expression(&v, &cfg))
            .unwrap_or_default();
        let rhs_expr = get_field(&binary, "rhs")
            .and_then(|v| render_expression(&v, &cfg))
            .unwrap_or_default();
        return Ok(format!("{lhs_expr} = {rhs_expr}"));
    }

    // Fallback: 0 = expression
    let expr_str = render_expression(&residual, &cfg)?;
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

/// Render a single statement (MLS §12: function body statements).
///
/// Usage in templates:
/// ```jinja
/// {% for stmt in func.body %}
/// {{ render_statement(stmt, cfg, indent) }}
/// {% endfor %}
/// ```
fn render_statement_function(stmt: Value, config: Value, indent: Value) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    let indent_str = indent.as_str().unwrap_or("    ");
    render_statement(&stmt, &cfg, indent_str)
}

/// Render a list of statements (MLS §12: function body).
///
/// Usage in templates:
/// ```jinja
/// {{ render_statements(func.body, cfg, "    ") }}
/// ```
fn render_statements_function(stmts: Value, config: Value, indent: Value) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    let indent_str = indent.as_str().unwrap_or("    ");
    render_statements(&stmts, &cfg, indent_str)
}

/// Render a list of statements to a string.
fn render_statements(stmts: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
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
fn render_statement(stmt: &Value, cfg: &ExprConfig, indent: &str) -> RenderResult {
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

    if let Some(ref stmts_val) = stmts {
        result.push_str(&render_statements(stmts_val, cfg, next_indent)?);
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

/// Render AST Expression (different from Expression).
fn render_ast_expression(expr: &Value, cfg: &ExprConfig) -> RenderResult {
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

    // Fallback: try to convert to string
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
        IfStyle::Function => format!(
            "{}if_else({}, {}, {})",
            cfg.prefix, cond, then_expr, else_expr
        ),
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

/// Configuration for expression rendering.
struct ExprConfig {
    prefix: String,
    power: String,
    and_op: String,
    or_op: String,
    not_op: String,
    true_val: String,
    false_val: String,
    array_start: String,
    array_end: String,
    if_style: IfStyle,
    /// When false, keep dots in variable/function names instead of replacing with underscores.
    sanitize_dots: bool,
    /// When true, use 1-based indexing (Modelica) instead of 0-based (Python).
    one_based_index: bool,
    /// When true, use Modelica builtin names (abs, min, max) instead of Python (fabs, fmin, fmax).
    modelica_builtins: bool,
    /// Optional function for element-wise multiply (e.g., `ca.times` for CasADi).
    mul_elem_fn: Option<String>,
}

#[derive(Clone, Copy)]
enum IfStyle {
    /// Python-style: ca.if_else(cond, then, else)
    Function,
    /// Ternary: cond ? then : else
    Ternary,
    /// Modelica-style: if cond then expr elseif cond2 then expr2 else expr3
    Modelica,
}

impl Default for ExprConfig {
    fn default() -> Self {
        Self {
            prefix: String::new(),
            power: "**".to_string(),
            and_op: "and".to_string(),
            or_op: "or".to_string(),
            not_op: "not ".to_string(),
            true_val: "True".to_string(),
            false_val: "False".to_string(),
            array_start: "[".to_string(),
            array_end: "]".to_string(),
            if_style: IfStyle::Function,
            sanitize_dots: true,
            one_based_index: false,
            modelica_builtins: false,
            mul_elem_fn: None,
        }
    }
}

/// Helper to get a string attribute from a Value.
fn get_str_attr(v: &Value, attr: &str) -> Option<String> {
    v.get_attr(attr)
        .ok()
        .and_then(|val| val.as_str().map(|s| s.to_string()))
}

impl ExprConfig {
    fn from_value(v: &Value) -> Self {
        let mut cfg = Self::default();

        if let Some(s) = get_str_attr(v, "prefix") {
            cfg.prefix = s;
        }
        if let Some(s) = get_str_attr(v, "power") {
            cfg.power = s;
        }
        if let Some(s) = get_str_attr(v, "and_op") {
            cfg.and_op = s;
        }
        if let Some(s) = get_str_attr(v, "or_op") {
            cfg.or_op = s;
        }
        if let Some(s) = get_str_attr(v, "not_op") {
            cfg.not_op = s;
        }
        if let Some(s) = get_str_attr(v, "true_val") {
            cfg.true_val = s;
        }
        if let Some(s) = get_str_attr(v, "false_val") {
            cfg.false_val = s;
        }
        if let Some(s) = get_str_attr(v, "array_start") {
            cfg.array_start = s;
        }
        if let Some(s) = get_str_attr(v, "array_end") {
            cfg.array_end = s;
        }
        if let Some(s) = get_str_attr(v, "if_style") {
            cfg.if_style = match s.as_str() {
                "ternary" => IfStyle::Ternary,
                "modelica" => IfStyle::Modelica,
                _ => IfStyle::Function,
            };
        }
        if let Ok(val) = v.get_attr("sanitize_dots")
            && !val.is_undefined()
            && !val.is_none()
        {
            cfg.sanitize_dots = val.is_true();
        }
        if let Ok(val) = v.get_attr("one_based_index")
            && !val.is_undefined()
            && !val.is_none()
        {
            cfg.one_based_index = val.is_true();
        }
        if let Ok(val) = v.get_attr("modelica_builtins")
            && !val.is_undefined()
            && !val.is_none()
        {
            cfg.modelica_builtins = val.is_true();
        }
        if let Some(s) = get_str_attr(v, "mul_elem_fn")
            && !s.is_empty()
        {
            cfg.mul_elem_fn = Some(s);
        }

        cfg
    }
}

/// Access a named field from a Value, checking that it exists (not undefined/none).
///
/// Serialized Rust enums produce map-like Values where variant names are keys.
/// minijinja maps return `Ok(undefined)` for missing keys instead of `Err`,
/// so we must check that the returned value is not undefined/none.
fn get_field(value: &Value, name: &str) -> Result<Value, minijinja::Error> {
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
fn render_expression(expr: &Value, cfg: &ExprConfig) -> RenderResult {
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

fn is_mul_elem_op(op: &Value) -> bool {
    get_field(op, "MulElem").is_ok()
}

fn get_binop_string(op: &Value, cfg: &ExprConfig) -> RenderResult {
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

fn get_unop_string(op: &Value, cfg: &ExprConfig) -> RenderResult {
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

fn render_subscript(sub: &Value, cfg: &ExprConfig) -> RenderResult {
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

fn render_args(call: &Value, cfg: &ExprConfig) -> RenderResult {
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

#[cfg(test)]
mod codegen_tests;
