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
use std::path::Path;

mod render_c;
mod render_expr;
mod render_stmt;

use render_expr::render_expression;
use render_stmt::{render_equation, render_flat_equation, render_statement, render_statements};

/// Result type for internal render functions.
pub(crate) type RenderResult = Result<String, minijinja::Error>;

/// Supported IR roots for template rendering.
#[derive(Debug, Clone, Copy)]
pub enum CodegenInput<'a> {
    Dae(&'a dae::Dae),
    Flat(&'a flat::Model),
    Ast(&'a ast::ClassTree),
}

/// Extract unique enum type names from enum literal ordinals.
/// E.g., from `Modelica.Blocks.Types.Smoothness.LinearSegments` → `Modelica.Blocks.Types.Smoothness`.
fn enum_type_names_from_ordinals(ordinals: &dae::Dae) -> Vec<String> {
    let mut seen = std::collections::HashSet::new();
    let mut result = Vec::new();
    for name in ordinals.enum_literal_ordinals.keys() {
        // Strip the last `.Component` to get the type path
        if let Some(dot_pos) = name.rfind('.') {
            let type_name = &name[..dot_pos];
            if seen.insert(type_name.to_string()) {
                result.push(type_name.to_string());
            }
        }
    }
    result
}

pub fn dae_template_json(dae: &dae::Dae) -> serde_json::Value {
    let mut value = serde_json::to_value(dae).expect("DAE should serialize");
    let object = value
        .as_object_mut()
        .expect("DAE should serialize to a JSON object");
    object.insert(
        "enum_type_names".to_string(),
        serde_json::to_value(enum_type_names_from_ordinals(dae))
            .expect("enum_type_names should serialize"),
    );

    value
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
/// # States: {{ dae.x | length }}
/// {% for name, var in dae.x %}
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

/// Render a template using a pre-built `dae` JSON context object and model name.
pub fn render_template_with_dae_json_and_name(
    dae_json: &serde_json::Value,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    let mut env = create_environment();
    env.add_template("inline", template)?;

    let dae_value = Value::from_serialize(dae_json);
    let tmpl = env.get_template("inline")?;
    let result = tmpl.render(minijinja::context! {
        dae => dae_value,
        model_name => model_name,
    })?;

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
    env.add_filter("last_segment", last_segment_filter);

    // Custom functions for expression rendering
    env.add_function("render_expr", render_expr_function);
    env.add_function("render_equation", render_equation_function);

    // Custom functions for statement rendering (MLS §12: function bodies)
    env.add_function("render_statement", render_statement_function);
    env.add_function("render_statements", render_statements_function);

    // Custom function for flat equation rendering (Model residual equations)
    env.add_function("render_flat_equation", render_flat_equation_function);

    // Custom function for detecting self-referential (builtin alias) functions
    env.add_function("is_self_call", is_self_call_function);
    env.add_function("fail", fail_function);

    // Extract explicit ODE rhs from residual equation: 0 = der(x) - expr → expr
    env.add_function("ode_rhs", render_c::ode_rhs_function);
    // Find derivative expression for a specific state variable
    env.add_function("ode_rhs_for_state", render_c::ode_rhs_for_state_function);

    // Find explicit RHS for an algebraic variable from residual: 0 = y - expr → expr
    env.add_function("alg_rhs_for_var", render_c::alg_rhs_for_var_function);
    env.add_function(
        "alg_rhs_for_var_or_self",
        render_c::alg_rhs_for_var_or_self_function,
    );
    env.add_function(
        "discrete_rhs_for_var",
        render_c::discrete_rhs_for_var_function,
    );

    // Index into an array expression to render element i (1-based)
    env.add_function(
        "render_expr_at_index",
        render_c::render_expr_at_index_function,
    );

    // Check if an expression is a string literal (for C codegen)
    env.add_function("is_string_literal", render_c::is_string_literal_function);
    env.add_function("expr_has_var_ref", render_c::expr_has_var_ref_function);
    env.add_function(
        "initial_rhs_for_var",
        render_c::initial_rhs_for_var_function,
    );

    // Check if a function has Complex-typed parameters
    env.add_function("has_complex_params", render_c::has_complex_params_function);

    env
}

/// Python keywords that cannot be used as identifiers.
const PYTHON_KEYWORDS: &[&str] = &[
    "False", "None", "True", "and", "as", "assert", "async", "await", "break", "class", "continue",
    "def", "del", "elif", "else", "except", "finally", "for", "from", "global", "if", "import",
    "in", "is", "lambda", "nonlocal", "not", "or", "pass", "raise", "return", "try", "while",
    "with", "yield",
];

/// C/C++ reserved words that cannot be used as identifiers in generated C code.
const C_KEYWORDS: &[&str] = &[
    "auto", "break", "case", "char", "const", "continue", "default", "do", "double", "else",
    "enum", "extern", "float", "for", "goto", "if", "int", "long", "register", "return", "short",
    "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void",
    "volatile", "while", "inline", "restrict",
];

/// Sanitize a name for use as a target-language identifier.
///
/// Replaces all non-alphanumeric/underscore characters with `_`, then
/// appends `_` if the result is a reserved keyword.  This matches the
/// `sanitize` Jinja filter so that equation-side references agree with
/// the variable declarations emitted by the templates.
pub(crate) fn sanitize_name(name: &str) -> String {
    let name = normalize_static_component_subscripts(name);
    let mut result = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_alphanumeric() || ch == '_' {
            result.push(ch);
        } else if ch == ']' {
            // Drop closing brackets to avoid trailing underscores.
            // After for-loop unrolling, VarRef names like "Kp[1]" get sanitized
            // here; replacing ']' with '_' would produce "Kp_1_" instead of "Kp_1".
        } else {
            result.push('_');
        }
    }
    escape_reserved_keyword(&result)
}

/// Escape a name if it collides with a reserved keyword (Python or C).
/// Appends `_` to the name if it matches.
pub(crate) fn escape_reserved_keyword(name: &str) -> String {
    if PYTHON_KEYWORDS.contains(&name) || C_KEYWORDS.contains(&name) {
        format!("{name}_")
    } else {
        name.to_string()
    }
}

/// Filter to sanitize variable names for target language identifiers.
///
/// Replaces dots and other non-identifier characters with underscores,
/// and appends `_` to reserved keywords (Python + C).
fn sanitize_filter(value: Value) -> String {
    let s = normalize_static_component_subscripts(&value.to_string());
    let mut result = String::with_capacity(s.len());
    for ch in s.chars() {
        if ch.is_alphanumeric() || ch == '_' {
            result.push(ch);
        } else if ch == ']' {
            // Drop closing brackets (see sanitize_name for rationale)
        } else {
            result.push('_');
        }
    }
    // Escape reserved keywords by appending underscore
    if PYTHON_KEYWORDS.contains(&result.as_str()) || C_KEYWORDS.contains(&result.as_str()) {
        result.push('_');
    }
    result
}

fn normalize_static_component_subscripts(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    let mut rest = name;
    while let Some(open_idx) = rest.find('[') {
        out.push_str(&rest[..open_idx + 1]);
        let after_open = &rest[open_idx + 1..];
        let Some(close_idx) = after_open.find(']') else {
            out.push_str(after_open);
            return out;
        };
        let inner = &after_open[..close_idx];
        if let Some(normalized) = normalize_static_subscript_list(inner) {
            out.push_str(&normalized);
        } else {
            out.push_str(inner);
        }
        out.push(']');
        rest = &after_open[close_idx + 1..];
    }
    out.push_str(rest);
    out
}

fn normalize_static_subscript_list(inner: &str) -> Option<String> {
    let mut normalized = Vec::new();
    for part in inner.split(',') {
        normalized.push(eval_integer_sum(part.trim())?.to_string());
    }
    Some(normalized.join(","))
}

fn eval_integer_sum(expr: &str) -> Option<i64> {
    let mut chars = expr.chars().peekable();
    let mut total = 0i64;
    let mut sign = 1i64;

    loop {
        while matches!(chars.peek(), Some(ch) if ch.is_whitespace()) {
            chars.next();
        }
        while matches!(chars.peek(), Some('(')) {
            chars.next();
            while matches!(chars.peek(), Some(ch) if ch.is_whitespace()) {
                chars.next();
            }
        }
        match chars.peek().copied() {
            Some('+') => {
                sign = 1;
                chars.next();
                continue;
            }
            Some('-') => {
                sign = -1;
                chars.next();
                continue;
            }
            _ => {}
        }

        while matches!(chars.peek(), Some(ch) if ch.is_whitespace()) {
            chars.next();
        }
        while matches!(chars.peek(), Some('(')) {
            chars.next();
            while matches!(chars.peek(), Some(ch) if ch.is_whitespace()) {
                chars.next();
            }
        }

        let mut value = 0i64;
        let mut digits = 0usize;
        while let Some(ch) = chars.peek().copied() {
            if let Some(digit) = ch.to_digit(10) {
                value = value.checked_mul(10)?.checked_add(digit as i64)?;
                digits += 1;
                chars.next();
            } else {
                break;
            }
        }
        if digits == 0 {
            return None;
        }
        total = total.checked_add(sign.checked_mul(value)?)?;

        while matches!(chars.peek(), Some(ch) if ch.is_whitespace()) {
            chars.next();
        }
        while matches!(chars.peek(), Some(')')) {
            chars.next();
            while matches!(chars.peek(), Some(ch) if ch.is_whitespace()) {
                chars.next();
            }
        }
        match chars.peek().copied() {
            Some('+') => {
                sign = 1;
                chars.next();
            }
            Some('-') => {
                sign = -1;
                chars.next();
            }
            Some(_) => return None,
            None => break,
        }
    }

    Some(total)
}

/// Filter to extract the last dot-separated segment of a name.
///
/// Used in templates: `{{ "Modelica.Math.sin" | last_segment }}` -> `"sin"`
fn last_segment_filter(value: Value) -> String {
    let s = value.to_string().replace('"', "");
    s.rsplit('.').next().unwrap_or(&s).to_string()
}

/// Filter to compute the product of all elements in a sequence.
///
/// Used by MX template: `{{ var.dims | product }}` -> total scalar size.
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

/// Fail template rendering with an explicit message.
///
/// Templates use this to declare target-specific capability constraints
/// without pushing those policies into Rust-side backend branching.
fn fail_function(message: Value) -> RenderResult {
    let msg = message
        .as_str()
        .map(str::to_owned)
        .unwrap_or_else(|| message.to_string());
    Err(render_err(msg))
}

/// Detect whether a function is a trivial self-call (builtin alias).
///
/// Returns true if the function body is a single assignment whose RHS is a
/// direct `FunctionCall` back to the function itself (e.g. `y := sin(x)`).
///
/// Usage in templates:
/// ```jinja
/// {% if is_self_call(func_name, func) %}...{% endif %}
/// ```
fn is_self_call_function(func_name: Value, func: Value) -> Result<bool, minijinja::Error> {
    use render_expr::get_field;
    let name_str = func_name.to_string().replace('"', "");
    let Ok(body) = get_field(&func, "body") else {
        return Ok(false);
    };
    let Some(len) = body.len() else {
        return Ok(false);
    };
    // Only match trivial bodies: exactly one assignment whose RHS is a direct
    // FunctionCall to self (e.g. `result := sin(u)`). This avoids matching
    // complex functions that happen to contain a nested self-reference.
    if len != 1 {
        return Ok(false);
    }
    let Ok(stmt) = body.get_item(&Value::from(0)) else {
        return Ok(false);
    };
    let Ok(assign) = get_field(&stmt, "Assignment") else {
        return Ok(false);
    };
    let Ok(value) = get_field(&assign, "value") else {
        return Ok(false);
    };
    // Check if value is a direct FunctionCall to self
    if let Ok(func_call) = get_field(&value, "FunctionCall")
        && let Ok(name) = get_field(&func_call, "name")
    {
        let call_name = get_field(&name, "0")
            .map(|v| v.to_string().replace('"', ""))
            .unwrap_or_else(|_| name.to_string().replace('"', ""));
        return Ok(call_name == name_str);
    }
    Ok(false)
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
    render_flat_equation(&eq, &cfg)
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
    let mut cfg = ExprConfig::from_value(&config);
    // Function bodies use local arrays / lists, so array subscripts must
    // always use bracket notation — see render_statements_function.
    cfg.subscript_underscore = false;
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
    let mut cfg = ExprConfig::from_value(&config);
    // Function bodies use local C arrays / Python lists, so array subscripts
    // must always use bracket notation (y[i]) — never the underscore style
    // (y_i) which is reserved for top-level DAE named-scalar unpacking.
    cfg.subscript_underscore = false;
    let indent_str = indent.as_str().unwrap_or("    ");
    render_statements(&stmts, &cfg, indent_str)
}

// ── ExprConfig and helpers ───────────────────────────────────────────

/// Configuration for expression rendering.
#[derive(Clone)]
pub(crate) struct ExprConfig {
    pub(crate) prefix: String,
    pub(crate) power: String,
    pub(crate) and_op: String,
    pub(crate) or_op: String,
    pub(crate) not_op: String,
    pub(crate) true_val: String,
    pub(crate) false_val: String,
    pub(crate) array_start: String,
    pub(crate) array_end: String,
    pub(crate) if_style: IfStyle,
    /// When false, keep dots in variable/function names instead of replacing with underscores.
    pub(crate) sanitize_dots: bool,
    /// When true, use 1-based indexing (Modelica) instead of 0-based (Python).
    pub(crate) one_based_index: bool,
    /// When true, use Modelica builtin names (abs, min, max) instead of Python (fabs, fmin, fmax).
    pub(crate) modelica_builtins: bool,
    /// Optional function for element-wise multiply (e.g., `ca.times` for CasADi).
    pub(crate) mul_elem_fn: Option<String>,
    /// Optional function-call form for power (e.g., `ca.power` for CasADi).
    /// When set, `a^b` renders as `power_fn(a, b)` instead of `a ** b`.
    pub(crate) power_fn: Option<String>,
    /// Subscript rendering style: "bracket" (default: `x[0]`) or "underscore" (`x_1`, 1-based).
    /// The "underscore" style matches the C template's unpack_vars naming convention.
    pub(crate) subscript_underscore: bool,
    /// Override function name for `IfStyle::Function` (default: `"if_else"`).
    /// E.g., set to `"IfElse.ifelse"` for Julia ModelingToolkit.
    pub(crate) if_else_fn: Option<String>,
    /// When true, render Modelica range `start:end` as Python `range(start, end + 1)`
    /// and array comprehensions with `[...]` instead of `{...}`.
    pub(crate) python_range: bool,
    /// Override function name for `sum()` calls on non-literal arrays.
    /// Default is `"sum1"` (CasADi convention, rendered as `prefix + sum1`).
    /// C backends set this to their helper name (e.g., `"__rumoca_sum"`).
    pub(crate) sum_fn: String,
    /// When true, render all numeric literals as float constants with `f` suffix.
    /// E.g., `8` → `8.0f`, `3.14` → `3.14f`. Used by embedded C backend.
    pub(crate) float_literals: bool,
}

#[derive(Clone, Copy)]
pub(crate) enum IfStyle {
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
            power_fn: None,
            subscript_underscore: false,
            if_else_fn: None,
            python_range: false,
            sum_fn: "sum1".to_string(),
            float_literals: false,
        }
    }
}

/// Helper to get a string attribute from a Value.
pub(crate) fn get_str_attr(v: &Value, attr: &str) -> Option<String> {
    v.get_attr(attr)
        .ok()
        .and_then(|val| val.as_str().map(|s| s.to_string()))
}

impl ExprConfig {
    pub(crate) fn from_value(v: &Value) -> Self {
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
        if let Some(s) = get_str_attr(v, "power_fn")
            && !s.is_empty()
        {
            cfg.power_fn = Some(s);
        }
        if let Ok(val) = v.get_attr("subscript_underscore")
            && !val.is_undefined()
            && !val.is_none()
        {
            cfg.subscript_underscore = val.is_true();
        }
        if let Some(s) = get_str_attr(v, "if_else_fn")
            && !s.is_empty()
        {
            cfg.if_else_fn = Some(s);
        }
        if let Ok(val) = v.get_attr("python_range")
            && !val.is_undefined()
            && !val.is_none()
        {
            cfg.python_range = val.is_true();
        }
        if let Some(s) = get_str_attr(v, "sum_fn")
            && !s.is_empty()
        {
            cfg.sum_fn = s;
        }
        if let Ok(val) = v.get_attr("float_literals")
            && !val.is_undefined()
            && !val.is_none()
        {
            cfg.float_literals = val.is_true();
        }

        cfg
    }
}

#[cfg(test)]
mod codegen_tests;
