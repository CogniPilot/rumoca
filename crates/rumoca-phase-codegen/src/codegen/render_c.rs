//! C-backend template functions for FMI2 and embedded-C code generation.
//!
//! These functions are registered in the minijinja environment and used by
//! `fmi2/model.c.jinja` and `embedded_c/model.c.jinja` templates to extract explicit
//! ODE/algebraic RHS expressions from residual-form DAE equations.

use super::{ExprConfig, RenderResult};
use crate::errors::render_err;
use minijinja::Value;
use render_expr::{
    get_field, is_variant, render_expression, render_serialized_name, subscript_index_value,
};
use std::collections::BTreeMap;

use super::render_expr;
mod discrete_statespace;
use discrete_statespace::synthesize_discrete_statespace_rhs;

type LinearDerivativeRow = (BTreeMap<usize, String>, String);
type MaybeLinearDerivativeRow = Result<Option<LinearDerivativeRow>, minijinja::Error>;

// ── Functions ───────────────────────────────────────────────────────────

fn no_render_match<T>() -> Result<Option<T>, minijinja::Error> {
    Ok(Option::None)
}

fn equation_residual_or_rhs(eq: &Value) -> Result<Option<Value>, minijinja::Error> {
    if let Ok(rhs) = get_field(eq, "rhs") {
        return Ok(Some(rhs));
    }
    if let Ok(residual) = get_field(eq, "residual") {
        return Ok(Some(residual));
    }
    no_render_match()
}

fn required_equation_residual_or_rhs(
    eq: &Value,
    context: &'static str,
) -> Result<Value, minijinja::Error> {
    equation_residual_or_rhs(eq)?
        .ok_or_else(|| render_err(format!("{context} missing required `rhs`/`residual` field")))
}

/// Render element `index` (1-based) of an expression. If the expression is an
/// `Array { elements }`, flattens nested array constructors in row-major order
/// and renders the requested scalar element. Otherwise renders the whole
/// expression for scalar broadcast semantics.
///
/// Usage in templates:
/// ```jinja
/// {{ render_expr_at_index(var.start, i + 1, config) }}
/// ```
pub(super) fn render_expr_at_index_function(
    expr: Value,
    index: Value,
    config: Value,
) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    let idx = index.as_usize().filter(|idx| *idx > 0).ok_or_else(|| {
        render_err(format!(
            "render_expr_at_index requires a positive index, got {index}"
        ))
    })?;

    if let Some(rendered) = render_array_expr_at_index_checked(&expr, idx, &cfg)? {
        return Ok(rendered);
    }

    // If expression is a whole-array VarRef (e.g. A_d_rp), index into the generated
    // element aliases used by embedded C templates (A_d_rp_1, A_d_rp_2, ...).
    if cfg.subscript_underscore
        && let Ok(var_ref) = get_field(&expr, "VarRef")
        && let Ok(subs) = get_field(&var_ref, "subscripts")
        && subs.len().unwrap_or(0) == 0
    {
        let base = render_expression(&expr, &cfg)?;
        return Ok(format!("{}_{}", base, idx));
    }

    // Support scalar extraction from generator calls like zeros(n)/ones(n)
    // when start values are indexed element-wise in templates.
    if let Ok(builtin) = get_field(&expr, "BuiltinCall")
        && let Ok(function) = get_field(&builtin, "function")
    {
        let f = function.to_string();
        if f == "Zeros" || f == "\"Zeros\"" {
            if cfg.power == "**" {
                return Ok("0.0".to_string());
            }
            return Ok("REAL_C(0.0)".to_string());
        }
        if f == "Ones" || f == "\"Ones\"" {
            if cfg.power == "**" {
                return Ok("1.0".to_string());
            }
            return Ok("REAL_C(1.0)".to_string());
        }
    }

    // Fallback: render whole expression (scalar value broadcast to all indices)
    render_expression(&expr, &cfg)
}

fn collect_array_elements_flat(expr: &Value, out: &mut Vec<Value>) {
    if let Ok(array) = get_field(expr, "Array")
        && let Ok(elements) = get_field(&array, "elements")
        && let Some(len) = elements.len()
    {
        for i in 0..len {
            if let Ok(elem) = elements.get_item(&Value::from(i)) {
                collect_array_elements_flat(&elem, out);
            }
        }
        return;
    }
    out.push(expr.clone());
}

/// Check if an expression is a string literal.
///
/// Returns "yes" if it's a Literal::String, empty string otherwise.
/// Used in C codegen to skip string parameter assignments.
pub(super) fn is_string_literal_function(expr: Value) -> String {
    if let Ok(literal) = get_field(&expr, "Literal")
        && get_field(&literal, "String").is_ok()
    {
        return "yes".to_string();
    }
    String::new()
}

/// Check if a function has record-typed parameters.
///
/// Returns "yes" if any input parameter carries record type metadata.
pub(super) fn has_complex_params_function(func: Value) -> String {
    if let Ok(inputs) = get_field(&func, "inputs")
        && list_any(&inputs, |param| {
            get_field(&param, "type_class")
                .map(|type_class| type_class.to_string().trim_matches('"') == "Record")
                .unwrap_or(false)
        })
    {
        return "yes".to_string();
    }
    String::new()
}

/// Extract the explicit RHS from a residual ODE equation.
///
/// Given an equation in residual form `0 = der(x) - expr` (MLS Appendix B.1a),
/// returns the rendered `expr`. This converts the implicit DAE form used internally
/// to explicit ODE form needed by FMI 2.0 `fmi2GetDerivatives` (FMI 2.0.4 §3.2.2).
///
/// Usage in templates:
/// ```jinja
/// m->xdot[{{ loop.index0 }}] = {{ ode_rhs(eq, cfg) }};
/// ```
pub(super) fn ode_rhs_function(eq: Value, config: Value) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);

    let rhs = required_equation_residual_or_rhs(&eq, "ode_rhs equation")?;

    // Residual form: rhs is Binary { Sub, lhs: der(x), rhs: expr }
    // We want to return just `expr`
    if let Ok(binary) = get_field(&rhs, "Binary")
        && is_sub_op(&binary)
    {
        let rhs_expr = get_field(&binary, "rhs").and_then(|v| render_expression(&v, &cfg))?;
        return Ok(rhs_expr);
    }

    // Fallback: negate the whole expression (0 = expr → xdot = -expr)
    let expr_str = render_expression(&rhs, &cfg)?;
    Ok(format!("-({expr_str})"))
}

/// Find the derivative expression for a named state variable from the f_x equation list.
///
/// Searches through f_x equations (MLS Appendix B.1a residual form) to find the one
/// matching `der(state_name)`, and returns the explicit RHS. This correctly handles
/// cases where state ordering in `dae.x` differs from equation ordering in `dae.f_x`,
/// which is required for FMI 2.0 `fmi2GetDerivatives` (FMI 2.0.4 §3.2.2).
///
/// Usage in templates:
/// ```jinja
/// {% for name, var in dae.x | items %}
/// m->xdot[{{ loop.index0 }}] = {{ ode_rhs_for_state(name, dae.f_x, cfg) }};
/// {% endfor %}
/// ```
pub(super) fn ode_rhs_for_state_function(
    state_name: Value,
    equations: Value,
    config: Value,
) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    let name_str = state_name.to_string().trim_matches('"').to_string();

    // Iterate through equations to find the one whose LHS is der(state_name)
    let Ok(iter) = equations.try_iter() else {
        return Ok("0.0".to_string());
    };
    for eq in iter {
        if let Some(rhs_expr) = find_derivative_rhs(&eq, &name_str, &cfg)? {
            return Ok(rhs_expr);
        }
    }

    if let Some(rhs_expr) =
        find_linear_derivative_system_rhs_in_equations(&equations, &name_str, &cfg)?
    {
        return Ok(rhs_expr);
    }

    // No matching equation found — emit warning so it's visible in generated code
    // Use Python-style comment when power is "**" (Python backends)
    if cfg.power == "**" {
        Ok(format!(
            "0.0  # WARNING: no ODE equation found for der({})",
            name_str
        ))
    } else {
        Ok(format!(
            "0.0 /* WARNING: no ODE equation found for der({}) */",
            name_str
        ))
    }
}

/// Extract the explicit RHS for an algebraic variable from f_x equations.
///
/// Searches f_x for an equation matching `0 = var_name - expr` and returns
/// the rendered `expr`. This is the algebraic analogue of `ode_rhs_for_state`.
///
/// Usage in templates:
/// ```jinja
/// {% for name, var in dae.y | items %}
/// m->y[{{ loop.index0 }}] = {{ alg_rhs_for_var(name, dae.f_x, cfg) }};
/// {% endfor %}
/// ```
pub(super) fn alg_rhs_for_var_function(
    var_name: Value,
    equations: Value,
    config: Value,
) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    let name_str = var_name.to_string().trim_matches('"').to_string();

    let Ok(iter) = equations.try_iter() else {
        return Ok("0.0".to_string());
    };
    for eq in iter {
        if let Some(rhs_expr) = find_algebraic_rhs_direct(&eq, &name_str, &cfg)? {
            return Ok(rhs_expr);
        }
    }

    let Ok(iter) = equations.try_iter() else {
        return Ok("0.0".to_string());
    };
    for eq in iter {
        if let Some(rhs_expr) = find_algebraic_rhs(&eq, &name_str, &cfg)? {
            return Ok(rhs_expr);
        }
    }

    // No matching equation found — emit warning so it's visible in generated code
    // Use Python-style comment when power is "**" (Python backends)
    if cfg.power == "**" {
        Ok(format!(
            "0.0  # WARNING: no equation found for {}",
            name_str
        ))
    } else {
        Ok(format!(
            "0.0 /* WARNING: no equation found for {} */",
            name_str
        ))
    }
}

/// Extract algebraic RHS like `alg_rhs_for_var`, but if no matching equation is
/// found, return the current variable alias (hold-last-value semantics).
///
/// This is used for embedded discrete next-state updates where unmatched
/// variables should remain unchanged.
pub(super) fn alg_rhs_for_var_or_self_function(
    var_name: Value,
    equations: Value,
    config: Value,
) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    let name_str = var_name.to_string().trim_matches('"').to_string();

    let Ok(iter) = equations.try_iter() else {
        return var_name_to_symbol(&name_str, &cfg);
    };
    for eq in iter {
        if let Some(rhs_expr) = find_algebraic_rhs(&eq, &name_str, &cfg)? {
            return Ok(rhs_expr);
        }
    }

    var_name_to_symbol(&name_str, &cfg)
}

/// Extract RHS for embedded discrete updates.
///
/// Resolution order:
/// 1) explicit equations in f_z
/// 2) explicit equations in f_m
/// 3) synthesized component-wise state-space updates using naming conventions
///    (prefix.x, prefix.e, prefix.u_k with prefix.A_d/B_d/C_d/D_d and
///    prefix.setpoint/prefix.measurement)
/// 4) hold current value
pub(super) fn discrete_rhs_for_var_function(
    var_name: Value,
    equations_z: Value,
    equations_m: Value,
    dae: Value,
    config: Value,
) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    let name = var_name.to_string().trim_matches('"').to_string();

    if let Ok(iter) = equations_z.try_iter() {
        for eq in iter {
            if let Some(rhs_expr) = find_algebraic_rhs(&eq, &name, &cfg)? {
                return Ok(rhs_expr);
            }
        }
    }
    if let Ok(iter) = equations_m.try_iter() {
        for eq in iter {
            if let Some(rhs_expr) = find_algebraic_rhs(&eq, &name, &cfg)? {
                return Ok(rhs_expr);
            }
        }
    }

    if let Some(synthesized) = synthesize_discrete_statespace_rhs(&name, &dae, &cfg)? {
        return Ok(synthesized);
    }

    var_name_to_symbol(&name, &cfg)
}

// ── Helpers ─────────────────────────────────────────────────────────────

/// Extract the derivative RHS from a single equation if it contains `der(state_name)`.
/// Helper for `ode_rhs_for_state_function`; decomposes MLS B.1a residual form.
///
/// Handles multiple equation forms:
/// - `0 = der(x) - expr` → `der(x) = expr`
/// - `0 = expr - der(x)` → `der(x) = expr`
/// - `0 = k*der(x) - expr` → `der(x) = expr / k`
/// - `0 = der(x)*k - expr` → `der(x) = expr / k`
/// - `0 = -(any of above)` → unwrap negation, swap lhs/rhs
///
/// Matches both scalar (`der(x)`) and indexed (`der(x[1])`) forms via `is_der_of`,
/// which reconstructs the full VarRef name including subscripts.
fn find_derivative_rhs(
    eq: &Value,
    state_name: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Some(rhs) = equation_residual_or_rhs(eq)? else {
        return Ok(Option::None);
    };

    // Try direct Binary{Sub} first, then try unwrapping Unary{Minus}.
    // 0 = -(A - B) is equivalent to 0 = B - A, so we swap lhs/rhs.
    let (binary, swapped) = if let Ok(b) = get_field(&rhs, "Binary") {
        (b, false)
    } else if let Ok(unary) = get_field(&rhs, "Unary") {
        let Some(op) = get_field(&unary, "op").ok().map(|v| v.to_string()) else {
            return no_render_match();
        };
        if op.contains("Minus") || op.contains("Neg") {
            let Ok(inner) = get_field(&unary, "rhs") else {
                return no_render_match();
            };
            let Ok(b) = get_field(&inner, "Binary") else {
                return no_render_match();
            };
            (b, true)
        } else {
            return no_render_match();
        }
    } else {
        return no_render_match();
    };

    if !is_sub_op(&binary) {
        return no_render_match();
    }
    let (lhs, rhs_val) = if swapped {
        // -(A - B) = B - A: swap the operands
        let Ok(rhs) = get_field(&binary, "rhs") else {
            return no_render_match();
        };
        let Ok(lhs) = get_field(&binary, "lhs") else {
            return no_render_match();
        };
        (rhs, lhs)
    } else {
        let Ok(lhs) = get_field(&binary, "lhs") else {
            return no_render_match();
        };
        let Ok(rhs) = get_field(&binary, "rhs") else {
            return no_render_match();
        };
        (lhs, rhs)
    };

    // Case 1: 0 = der(x) - expr → der(x) = expr
    if is_der_of(&lhs, state_name) {
        let rhs_expr = render_expression(&rhs_val, cfg)?;
        return Ok(Some(rhs_expr));
    }

    // Case 2: 0 = expr - der(x) → der(x) = expr
    if is_der_of(&rhs_val, state_name) {
        let lhs_expr = render_expression(&lhs, cfg)?;
        return Ok(Some(lhs_expr));
    }

    // Case 3: 0 = k*der(x) - expr or 0 = der(x)*k - expr → der(x) = expr / k
    if let Some(coeff) = extract_der_coefficient(&lhs, state_name, cfg)? {
        let rhs_expr = render_expression(&rhs_val, cfg)?;
        return Ok(Some(format!("({rhs_expr}) / ({coeff})")));
    }

    // Case 4: 0 = expr - k*der(x) or 0 = expr - der(x)*k → der(x) = expr / k
    if let Some(coeff) = extract_der_coefficient(&rhs_val, state_name, cfg)? {
        let lhs_expr = render_expression(&lhs, cfg)?;
        return Ok(Some(format!("({lhs_expr}) / ({coeff})")));
    }

    // Case 5: 0 = A * der(x_vec) - b_vec, with the equation preserved as a
    // vector residual. Emit a small dense linear solve for the requested
    // component instead of silently dropping the derivative.
    let scalar_count = eq
        .get_attr("scalar_count")
        .ok()
        .and_then(|v| v.as_usize())
        .unwrap_or(1);
    if let Some(rhs_expr) =
        find_linear_derivative_system_rhs(&lhs, &rhs_val, state_name, scalar_count, cfg)?
    {
        return Ok(Some(rhs_expr));
    }

    no_render_match()
}

fn find_linear_derivative_system_rhs(
    lhs: &Value,
    rhs: &Value,
    state_name: &str,
    scalar_count: usize,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Some((state_base, component)) = parse_indexed_ref(state_name) else {
        return no_render_match();
    };
    let n = scalar_count.max(component);
    if component == 0 || component > n {
        return no_render_match();
    }

    if let Some(product) = extract_matrix_derivative_product(lhs, &state_base)
        && !contains_der(rhs)
    {
        return render_linear_solve_component(&product, rhs, n, component, cfg);
    }

    if let Some(product) = extract_matrix_derivative_product(rhs, &state_base)
        && !contains_der(lhs)
    {
        return render_linear_solve_component(&product, lhs, n, component, cfg);
    }

    no_render_match()
}

fn find_linear_derivative_system_rhs_in_equations(
    equations: &Value,
    state_name: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Some((state_base, component)) = parse_indexed_ref(state_name) else {
        return no_render_match();
    };
    let mut rows = Vec::new();
    let mut n = component;

    let Ok(iter) = equations.try_iter() else {
        return no_render_match();
    };
    for eq in iter {
        if let Some((coefficients, rhs)) = extract_linear_derivative_row(&eq, &state_base, cfg)? {
            if let Some(max_col) = coefficients.keys().next_back().copied() {
                n = n.max(max_col);
            }
            rows.push((coefficients, rhs));
        }
    }

    if rows.len() < n || component == 0 || component > n {
        return no_render_match();
    }

    let mut matrix_entries = Vec::with_capacity(n * n);
    let mut rhs_entries = Vec::with_capacity(n);
    for (coefficients, rhs) in rows.iter().take(n) {
        for col in 1..=n {
            matrix_entries.push(
                coefficients
                    .get(&col)
                    .cloned()
                    .unwrap_or_else(|| "0.0".to_string()),
            );
        }
        rhs_entries.push(rhs.clone());
    }

    Ok(Some(format!(
        "__rumoca_solve_linear_component((double[]){{{}}}, (double[]){{{}}}, {}, {})",
        matrix_entries.join(", "),
        rhs_entries.join(", "),
        n,
        component - 1
    )))
}

fn extract_linear_derivative_row(
    eq: &Value,
    state_base: &str,
    cfg: &ExprConfig,
) -> MaybeLinearDerivativeRow {
    let Some(rhs) = equation_residual_or_rhs(eq)? else {
        return Ok(Option::None);
    };
    let Some((lhs, rhs_val)) = decompose_subtraction(&rhs) else {
        return no_render_match();
    };

    if contains_der_of_base(&lhs, state_base) && !contains_der(&rhs_val) {
        let Some(coefficients) = extract_linear_derivative_coefficients(&lhs, state_base, cfg)?
        else {
            return no_render_match();
        };
        let rhs_rendered = render_expression(&rhs_val, cfg)?;
        return Ok(Some((coefficients, rhs_rendered)));
    }

    if contains_der_of_base(&rhs_val, state_base) && !contains_der(&lhs) {
        let Some(coefficients) = extract_linear_derivative_coefficients(&rhs_val, state_base, cfg)?
        else {
            return no_render_match();
        };
        let lhs_rendered = render_expression(&lhs, cfg)?;
        return Ok(Some((coefficients, lhs_rendered)));
    }

    no_render_match()
}

fn extract_linear_derivative_coefficients(
    expr: &Value,
    state_base: &str,
    cfg: &ExprConfig,
) -> Result<Option<BTreeMap<usize, String>>, minijinja::Error> {
    let mut terms = Vec::new();
    flatten_value_add_terms(expr, true, &mut terms);

    let mut coefficients: BTreeMap<usize, String> = BTreeMap::new();
    for (positive, term) in terms {
        if !contains_der_of_base(&term, state_base) {
            return no_render_match();
        }
        let Some((component, coefficient)) =
            extract_derivative_term_coefficient(&term, state_base, cfg)?
        else {
            return no_render_match();
        };
        let signed = if positive {
            coefficient
        } else {
            format!("(-({coefficient}))")
        };
        coefficients
            .entry(component)
            .and_modify(|existing| *existing = format!("({existing} + {signed})"))
            .or_insert(signed);
    }

    if coefficients.is_empty() {
        no_render_match()
    } else {
        Ok(Some(coefficients))
    }
}

fn extract_derivative_term_coefficient(
    expr: &Value,
    state_base: &str,
    cfg: &ExprConfig,
) -> Result<Option<(usize, String)>, minijinja::Error> {
    if let Some(component) = der_index_of_base(expr, state_base) {
        return Ok(Some((component, "1.0".to_string())));
    }

    let Ok(binary) = get_field(expr, "Binary") else {
        return no_render_match();
    };
    if !is_mul_op(&binary) {
        return no_render_match();
    }
    let Ok(lhs) = get_field(&binary, "lhs") else {
        return no_render_match();
    };
    let Ok(rhs) = get_field(&binary, "rhs") else {
        return no_render_match();
    };

    if let Some(component) = der_index_of_base(&rhs, state_base)
        && !contains_der(&lhs)
    {
        return Ok(Some((component, render_expression(&lhs, cfg)?)));
    }
    if let Some(component) = der_index_of_base(&lhs, state_base)
        && !contains_der(&rhs)
    {
        return Ok(Some((component, render_expression(&rhs, cfg)?)));
    }

    no_render_match()
}

fn decompose_subtraction(expr: &Value) -> Option<(Value, Value)> {
    let (binary, swapped) = if let Ok(b) = get_field(expr, "Binary") {
        (b, false)
    } else if let Ok(unary) = get_field(expr, "Unary") {
        let op = get_field(&unary, "op").ok().map(|v| v.to_string())?;
        if op.contains("Minus") || op.contains("Neg") {
            let inner = get_field(&unary, "rhs").ok()?;
            let b = get_field(&inner, "Binary").ok()?;
            (b, true)
        } else {
            return None;
        }
    } else {
        return None;
    };

    if !is_sub_op(&binary) {
        return None;
    }

    if swapped {
        Some((
            get_field(&binary, "rhs").ok()?,
            get_field(&binary, "lhs").ok()?,
        ))
    } else {
        Some((
            get_field(&binary, "lhs").ok()?,
            get_field(&binary, "rhs").ok()?,
        ))
    }
}

struct MatrixDerivativeProduct {
    matrix: Value,
    transpose: bool,
}

fn extract_matrix_derivative_product(
    expr: &Value,
    state_base: &str,
) -> Option<MatrixDerivativeProduct> {
    let binary = get_field(expr, "Binary").ok()?;
    if !is_mul_op(&binary) {
        return None;
    }
    let lhs = get_field(&binary, "lhs").ok()?;
    let rhs = get_field(&binary, "rhs").ok()?;

    if is_der_of_whole(&rhs, state_base) {
        return Some(MatrixDerivativeProduct {
            matrix: lhs,
            transpose: false,
        });
    }
    if is_der_of_whole(&lhs, state_base) {
        return Some(MatrixDerivativeProduct {
            matrix: rhs,
            transpose: true,
        });
    }

    None
}

fn render_linear_solve_component(
    product: &MatrixDerivativeProduct,
    rhs: &Value,
    n: usize,
    component: usize,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    if n == 0 {
        return no_render_match();
    }

    let mut matrix_entries = Vec::with_capacity(n * n);
    for row in 1..=n {
        for col in 1..=n {
            let (source_row, source_col) = if product.transpose {
                (col, row)
            } else {
                (row, col)
            };
            let Some(entry) =
                render_matrix_expr_at_indices(&product.matrix, source_row, source_col, n, cfg)?
            else {
                return no_render_match();
            };
            matrix_entries.push(entry);
        }
    }

    let mut rhs_entries = Vec::with_capacity(n);
    for idx in 1..=n {
        let Some(entry) = render_array_expr_at_index_or_scalar_checked(rhs, idx, cfg)? else {
            return no_render_match();
        };
        rhs_entries.push(entry);
    }

    Ok(Some(format!(
        "__rumoca_solve_linear_component((double[]){{{}}}, (double[]){{{}}}, {}, {})",
        matrix_entries.join(", "),
        rhs_entries.join(", "),
        n,
        component - 1
    )))
}

fn render_matrix_expr_at_indices(
    expr: &Value,
    row: usize,
    col: usize,
    columns: usize,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    if let Ok(var_ref) = get_field(expr, "VarRef") {
        return render_var_ref_with_indices(&var_ref, &[row, col], cfg);
    }

    if get_field(expr, "Array").is_ok() {
        return render_array_expr_at_index_checked(expr, (row - 1) * columns + col, cfg);
    }

    if let Ok(binary) = get_field(expr, "Binary") {
        let Ok(lhs) = get_field(&binary, "lhs") else {
            return no_render_match();
        };
        let Ok(rhs) = get_field(&binary, "rhs") else {
            return no_render_match();
        };
        let Some(lhs_render) =
            render_matrix_expr_at_indices_or_scalar(&lhs, row, col, columns, cfg)?
        else {
            return no_render_match();
        };
        let Some(rhs_render) =
            render_matrix_expr_at_indices_or_scalar(&rhs, row, col, columns, cfg)?
        else {
            return no_render_match();
        };
        let Ok(op) = get_field(&binary, "op") else {
            return no_render_match();
        };
        let op_str = render_expr::get_binop_string(&op, cfg)?;
        return Ok(Some(format!("({lhs_render} {op_str} {rhs_render})")));
    }

    if let Ok(unary) = get_field(expr, "Unary") {
        let Ok(rhs) = get_field(&unary, "rhs") else {
            return no_render_match();
        };
        let Some(rhs_render) =
            render_matrix_expr_at_indices_or_scalar(&rhs, row, col, columns, cfg)?
        else {
            return no_render_match();
        };
        let Ok(op) = get_field(&unary, "op") else {
            return no_render_match();
        };
        let op_str = render_expr::get_unop_string(&op, cfg)?;
        return Ok(Some(format!("({op_str}{rhs_render})")));
    }

    render_expression(expr, cfg).map(Some)
}

fn render_matrix_expr_at_indices_or_scalar(
    expr: &Value,
    row: usize,
    col: usize,
    columns: usize,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    if let Some(rendered) = render_matrix_expr_at_indices(expr, row, col, columns, cfg)? {
        return Ok(Some(rendered));
    }
    render_expression(expr, cfg).map(Some)
}

fn render_var_ref_with_indices(
    var_ref: &Value,
    indices: &[usize],
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Ok(subscripts) = get_field(var_ref, "subscripts") else {
        return no_render_match();
    };
    if subscripts.len().unwrap_or(0) != 0 {
        return no_render_match();
    }

    let raw_name = var_ref_base_name(var_ref);
    if raw_name.is_empty() {
        return no_render_match();
    }

    let index_text = indices
        .iter()
        .map(|idx| idx.to_string())
        .collect::<Vec<_>>()
        .join(",");
    let indexed_ref = format!("{raw_name}[{index_text}]");
    if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &indexed_ref) {
        return Ok(Some(symbol));
    }

    let name = super::emitted_symbol(&raw_name, cfg)?;
    if cfg.subscript_underscore {
        return Ok(Some(format!(
            "{}_{}",
            name,
            indices
                .iter()
                .map(|idx| idx.to_string())
                .collect::<Vec<_>>()
                .join("_")
        )));
    }
    if cfg.one_based_index {
        return Ok(Some(format!("{name}[{index_text}]")));
    }

    if indices.len() == 1 {
        return Ok(Some(format!("{}[{}]", name, indices[0] - 1)));
    }

    no_render_match()
}

/// Extract the algebraic RHS from a single equation if it matches `0 = var_name - expr`.
/// Helper for `alg_rhs_for_var_function`; decomposes MLS B.1a residual form for algebraics.
///
/// Matches both scalar (`y`) and indexed (`y[1]`) forms via `is_var_ref_of`,
/// which reconstructs the full VarRef name including subscripts.
fn find_algebraic_rhs(
    eq: &Value,
    var_name: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    // Try direct assignment form first: lhs = rhs
    if let Some(result) = find_algebraic_rhs_assignment(eq, var_name, cfg)? {
        return Ok(Some(result));
    }

    let Some(rhs) = equation_residual_or_rhs(eq)? else {
        return Ok(Option::None);
    };

    // Try subtraction form first: 0 = var - expr or 0 = -(var - expr)
    if let Some(result) = find_algebraic_rhs_subtraction(&rhs, var_name, cfg)? {
        return Ok(Some(result));
    }

    // Try additive form: 0 = a + b + c (connection equations)
    if let Some(result) = find_algebraic_rhs_additive(&rhs, var_name, cfg)? {
        return Ok(Some(result));
    }

    // Try array-level binding: searching for "v[i]" but equation has "v" (whole array)
    // with an Array RHS. Extract element i from the array.
    if let Some(result) = find_algebraic_rhs_array_element(&rhs, var_name, cfg)? {
        return Ok(Some(result));
    }

    no_render_match()
}

/// Extract only direct defining equations for `var_name`.
///
/// This pass intentionally ignores equations where the target appears on the
/// RHS of another variable's equation. For example, in
/// `omega_error = omega_cmd - omega`, `omega_cmd` is algebraically solvable, but
/// if a later connection equation directly defines `omega_cmd`, that direct
/// equation is the correct explicit assignment for generated C.
fn find_algebraic_rhs_direct(
    eq: &Value,
    var_name: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    if let Some(result) = find_algebraic_rhs_assignment_direct(eq, var_name, cfg)? {
        return Ok(Some(result));
    }

    let Some(rhs) = equation_residual_or_rhs(eq)? else {
        return Ok(Option::None);
    };
    find_algebraic_rhs_subtraction_direct(&rhs, var_name, cfg)
}

fn find_algebraic_rhs_assignment_direct(
    eq: &Value,
    var_name: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Ok(lhs) = eq.get_attr("lhs") else {
        return no_render_match();
    };
    let Ok(rhs) = eq.get_attr("rhs") else {
        return no_render_match();
    };
    render_direct_rhs_for_lhs(&lhs, &rhs, var_name, cfg)
}

fn find_algebraic_rhs_subtraction_direct(
    rhs: &Value,
    var_name: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Ok(binary) = get_field(rhs, "Binary") else {
        return no_render_match();
    };
    if !is_sub_op(&binary) {
        return no_render_match();
    }
    let Ok(lhs_side) = get_field(&binary, "lhs") else {
        return no_render_match();
    };
    let Ok(rhs_side) = get_field(&binary, "rhs") else {
        return no_render_match();
    };
    if contains_der(&rhs_side) {
        return no_render_match();
    }
    render_direct_rhs_for_lhs(&lhs_side, &rhs_side, var_name, cfg)
}

fn render_direct_rhs_for_lhs(
    lhs: &Value,
    rhs: &Value,
    var_name: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    if is_var_ref_of(lhs, var_name) {
        return render_expression(rhs, cfg).map(Some);
    }
    if let Some(index) = array_lhs_element_index(lhs, var_name) {
        return render_array_expr_at_index_or_scalar_checked(rhs, index, cfg);
    }
    if let Some(index) = whole_array_lhs_index(lhs, var_name) {
        return render_array_expr_at_index_or_scalar_checked(rhs, index, cfg);
    }
    no_render_match()
}

fn array_lhs_element_index(lhs: &Value, var_name: &str) -> Option<usize> {
    if get_field(lhs, "Array").is_err() {
        return None;
    }
    let mut elements = Vec::new();
    collect_array_elements_flat(lhs, &mut elements);
    elements
        .iter()
        .position(|elem| is_var_ref_of(elem, var_name))
        .map(|idx| idx + 1)
}

fn whole_array_lhs_index(lhs: &Value, var_name: &str) -> Option<usize> {
    let (base_name, index) = parse_indexed_ref(var_name)?;
    if is_var_ref_of(lhs, &base_name) {
        Some(index)
    } else {
        None
    }
}

/// Try assignment form: `lhs = rhs` where lhs is the target variable.
/// This is used by prepared discrete partitions that are emitted as direct
/// assignments rather than residual equations.
///
/// For guarded when-sample equations, the RHS is an If-expression with a sample()
/// condition. Extract the state update from the true branch (condition=[sample(...), expr]).
/// The false branch (pre(var)) is implicit in the solver's discrete semantics.
fn find_algebraic_rhs_assignment(
    eq: &Value,
    var_name: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Ok(lhs) = eq.get_attr("lhs") else {
        return no_render_match();
    };

    // Handle two forms:
    // 1. lhs is a VarRef object: { "VarRef": { "name": "x" } }
    // 2. lhs is a plain string: "x"
    let lhs_matches = if let Ok(_var_ref) = get_field(&lhs, "VarRef") {
        is_var_ref_of(&lhs, var_name)
    } else if let Some(lhs_str) = lhs.as_str() {
        let lhs_trimmed = lhs_str.trim_matches('"');
        let var_trimmed = var_name.trim_matches('"');
        lhs_trimmed == var_trimmed
    } else {
        false
    };

    if lhs_matches {
        let Ok(rhs) = eq.get_attr("rhs") else {
            return no_render_match();
        };

        // If the RHS is an If-expression with sample() guard (when-statement),
        // extract the update expression from the true branch, not the full ternary.
        if let Ok(if_expr) = get_field(&rhs, "If")
            && let Ok(branches) = get_field(&if_expr, "branches")
            && let Ok(first_branch) = branches.get_item(&Value::from(0))
            && let Ok(branch_array) = first_branch.try_iter()
        {
            // branches is a list of [condition, expression] pairs.
            let items: Vec<_> = branch_array.take(2).collect();
            if let Some(update_expr) = items.get(1) {
                return render_expression(update_expr, cfg).map(Some);
            }
        }

        // Fall back to rendering the entire RHS (for non-guarded cases)
        return render_expression(&rhs, cfg).map(Some);
    }

    // Array-level assignment support: searching for "v[i]" while equation lhs is
    // the whole array "v" and rhs is Array{...}.
    let Some((base_name, index)) = parse_indexed_ref(var_name) else {
        return no_render_match();
    };
    if !is_var_ref_of(&lhs, &base_name) {
        return no_render_match();
    }

    let Ok(rhs) = eq.get_attr("rhs") else {
        return no_render_match();
    };
    let Ok(array) = get_field(&rhs, "Array") else {
        return no_render_match();
    };
    let Ok(elements) = get_field(&array, "elements") else {
        return no_render_match();
    };
    let Some(len) = elements.len() else {
        return no_render_match();
    };
    if index > len {
        return no_render_match();
    }
    let Ok(elem) = elements.get_item(&Value::from(index - 1)) else {
        return no_render_match();
    };
    render_expression(&elem, cfg).map(Some)
}

/// Try subtraction form: 0 = var - expr, 0 = expr - var, 0 = -(A - B)
fn find_algebraic_rhs_subtraction(
    rhs: &Value,
    var_name: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    // Try direct Binary{Sub} first, then try unwrapping Unary{Minus}.
    let (binary, swapped) = if let Ok(b) = get_field(rhs, "Binary") {
        (b, false)
    } else if let Ok(unary) = get_field(rhs, "Unary") {
        let Some(op) = get_field(&unary, "op").ok().map(|v| v.to_string()) else {
            return no_render_match();
        };
        if op.contains("Minus") || op.contains("Neg") {
            let Ok(inner) = get_field(&unary, "rhs") else {
                return no_render_match();
            };
            let Ok(b) = get_field(&inner, "Binary") else {
                return no_render_match();
            };
            (b, true)
        } else {
            return no_render_match();
        }
    } else {
        return no_render_match();
    };

    if !is_sub_op(&binary) {
        return no_render_match();
    }

    let (lhs_side, rhs_side) = if swapped {
        let Ok(rhs) = get_field(&binary, "rhs") else {
            return no_render_match();
        };
        let Ok(lhs) = get_field(&binary, "lhs") else {
            return no_render_match();
        };
        (rhs, lhs)
    } else {
        let Ok(lhs) = get_field(&binary, "lhs") else {
            return no_render_match();
        };
        let Ok(rhs) = get_field(&binary, "rhs") else {
            return no_render_match();
        };
        (lhs, rhs)
    };

    // Case 1: 0 = var - expr → var = expr
    if is_var_ref_of(&lhs_side, var_name) && !contains_der(&rhs_side) {
        let rhs_expr = render_expression(&rhs_side, cfg)?;
        return Ok(Some(rhs_expr));
    }

    // Case 2: 0 = expr - var → var = expr
    if is_var_ref_of(&rhs_side, var_name) && !contains_der(&lhs_side) {
        let lhs_expr = render_expression(&lhs_side, cfg)?;
        return Ok(Some(lhs_expr));
    }

    // Case 3: 0 = coeff * var - expr → var = expr / coeff
    if !contains_der(&lhs_side) && !contains_der(&rhs_side) {
        if let Some(coeff) = extract_mul_coefficient(&lhs_side, var_name, cfg)? {
            let rhs_expr = render_expression(&rhs_side, cfg)?;
            return Ok(Some(format!("({rhs_expr}) / ({coeff})")));
        }
        // Case 4: 0 = expr - coeff * var → var = expr / coeff
        if let Some(coeff) = extract_mul_coefficient(&rhs_side, var_name, cfg)? {
            let lhs_expr = render_expression(&lhs_side, cfg)?;
            return Ok(Some(format!("({lhs_expr}) / ({coeff})")));
        }
    }

    no_render_match()
}

/// Extract the coefficient from a `coeff * var` or `var * coeff` expression.
/// Returns the rendered coefficient string if the expression is a Mul with one
/// side being a VarRef to the target variable.
fn extract_mul_coefficient(
    expr: &Value,
    var_name: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Ok(binary) = get_field(expr, "Binary") else {
        return no_render_match();
    };
    if !is_mul_op(&binary) {
        return no_render_match();
    }
    let Ok(lhs) = get_field(&binary, "lhs") else {
        return no_render_match();
    };
    let Ok(rhs) = get_field(&binary, "rhs") else {
        return no_render_match();
    };

    // coeff * var
    if is_var_ref_of(&rhs, var_name) && !contains_var_ref(&lhs, var_name) {
        return render_expression(&lhs, cfg).map(Some);
    }
    // var * coeff
    if is_var_ref_of(&lhs, var_name) && !contains_var_ref(&rhs, var_name) {
        return render_expression(&rhs, cfg).map(Some);
    }
    no_render_match()
}

/// Check if an expression tree contains a VarRef matching the given name.
fn contains_var_ref(expr: &Value, var_name: &str) -> bool {
    if is_var_ref_of(expr, var_name) {
        return true;
    }
    if let Ok(binary) = get_field(expr, "Binary")
        && let (Ok(lhs), Ok(rhs)) = (get_field(&binary, "lhs"), get_field(&binary, "rhs"))
    {
        return contains_var_ref(&lhs, var_name) || contains_var_ref(&rhs, var_name);
    }
    if let Ok(unary) = get_field(expr, "Unary")
        && let Ok(inner) = get_field(&unary, "rhs")
    {
        return contains_var_ref(&inner, var_name);
    }
    false
}

/// Try to extract algebraic RHS from an additive equation (connection equation form).
/// Handles `0 = a + b + c` where exactly one term is the target variable.
/// Returns `var = -(other_terms)`.
fn find_algebraic_rhs_additive(
    rhs: &Value,
    var_name: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    // Flatten the Add/Sub tree into signed terms
    let mut terms: Vec<(bool, Value)> = Vec::new();
    flatten_value_add_terms(rhs, true, &mut terms);
    if terms.len() < 2 {
        return no_render_match();
    }

    // Skip if any term contains der()
    if terms.iter().any(|(_, t)| contains_der(t)) {
        return no_render_match();
    }

    // Find which term is the target variable
    let mut var_idx = None;
    for (i, (_, term)) in terms.iter().enumerate() {
        if is_var_ref_of(term, var_name) {
            if var_idx.is_some() {
                return no_render_match(); // multiple occurrences
            }
            var_idx = Some(i);
        }
    }
    let Some(var_idx) = var_idx else {
        return no_render_match();
    };
    let var_positive = terms[var_idx].0;

    // Build negation of other terms: var = -(other_terms) or var = other_terms
    let other_terms: Vec<(bool, &Value)> = terms
        .iter()
        .enumerate()
        .filter(|(i, _)| *i != var_idx)
        .map(|(_, (sign, term))| {
            if var_positive {
                (!*sign, term)
            } else {
                (*sign, term)
            }
        })
        .collect();

    // Render the sum of other terms
    let mut parts: Vec<String> = Vec::new();
    for (i, (positive, term)) in other_terms.iter().enumerate() {
        let rendered = render_expression(term, cfg)?;
        if i == 0 {
            if *positive {
                parts.push(rendered);
            } else {
                parts.push(format!("(-{})", rendered));
            }
        } else if *positive {
            parts.push(format!(" + {}", rendered));
        } else {
            parts.push(format!(" - {}", rendered));
        }
    }
    let expr = if other_terms.len() == 1 {
        parts.join("")
    } else {
        format!("({})", parts.join(""))
    };
    Ok(Some(expr))
}

/// Handle array-level binding equations when searching for a subscripted variable.
///
/// When searching for `v[i]`, if the equation has `0 = v - Array{...}` where `v` is
/// the base name (no subscripts), extract element `i` from the Array RHS.
/// This handles cases where the scalarizer didn't fully split array binding equations.
fn find_algebraic_rhs_array_element(
    rhs: &Value,
    var_name: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    // Only applies when var_name has a subscript, e.g. "error_dot[2]"
    let Some((base_name, index)) = parse_indexed_ref(var_name) else {
        return no_render_match();
    };

    // Try subtraction form: 0 = base_var - Array{...}
    let Ok(binary) = get_field(rhs, "Binary") else {
        return no_render_match();
    };
    if !is_sub_op(&binary) {
        return no_render_match();
    }
    let Ok(lhs_side) = get_field(&binary, "lhs") else {
        return no_render_match();
    };
    let Ok(rhs_side) = get_field(&binary, "rhs") else {
        return no_render_match();
    };

    // Check if one side is a VarRef matching the base name (no subscripts)
    // and the other side is an array-valued expression.
    let array_expr = if is_var_ref_of(&lhs_side, &base_name) {
        &rhs_side
    } else if is_var_ref_of(&rhs_side, &base_name) {
        &lhs_side
    } else {
        return no_render_match();
    };

    render_array_expr_at_index_checked(array_expr, index, cfg)
}

fn render_array_expr_at_index_checked(
    expr: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    if get_field(expr, "Array").is_ok() {
        let mut flat_elems = Vec::new();
        collect_array_elements_flat(expr, &mut flat_elems);
        let Some(elem) = index
            .checked_sub(1)
            .and_then(|flat_index| flat_elems.get(flat_index))
        else {
            return Err(render_err(format!(
                "array expression scalar index {index} is outside flattened size {}",
                flat_elems.len()
            )));
        };
        return render_expression(elem, cfg).map(Some);
    }

    if let Ok(var_ref) = get_field(expr, "VarRef") {
        return try_render_indexed_var_ref(&var_ref, index, cfg);
    }

    if let Ok(binary) = get_field(expr, "Binary") {
        return render_binary_array_expr_at_index_checked(&binary, index, cfg);
    }

    if let Ok(unary) = get_field(expr, "Unary") {
        let Ok(rhs) = get_field(&unary, "rhs") else {
            return no_render_match();
        };
        let Some(rhs_render) = render_array_expr_at_index_or_scalar_checked(&rhs, index, cfg)?
        else {
            return no_render_match();
        };
        let Ok(op) = get_field(&unary, "op") else {
            return no_render_match();
        };
        let op_str = render_expr::get_unop_string(&op, cfg)?;
        return Ok(Some(format!("({op_str}{rhs_render})")));
    }

    if let Ok(if_expr) = get_field(expr, "If") {
        let Ok(branches) = get_field(&if_expr, "branches") else {
            return no_render_match();
        };
        let Ok(else_branch) = get_field(&if_expr, "else_branch") else {
            return no_render_match();
        };
        let Some(else_render) =
            render_array_expr_at_index_or_scalar_checked(&else_branch, index, cfg)?
        else {
            return no_render_match();
        };
        let Some(branch_count) = branches.len() else {
            return Ok(Some(else_render));
        };
        let mut result = else_render;
        for branch_idx in (0..branch_count).rev() {
            let Ok(branch) = branches.get_item(&Value::from(branch_idx)) else {
                return no_render_match();
            };
            let Ok(cond) = branch.get_item(&Value::from(0)) else {
                return no_render_match();
            };
            let Ok(branch_expr) = branch.get_item(&Value::from(1)) else {
                return no_render_match();
            };
            let cond_render = render_expression(&cond, cfg)?;
            let Some(branch_render) =
                render_array_expr_at_index_or_scalar_checked(&branch_expr, index, cfg)?
            else {
                return no_render_match();
            };
            result = format!("(({cond_render}) ? ({branch_render}) : ({result}))");
        }
        return Ok(Some(result));
    }

    if let Ok(builtin) = get_field(expr, "BuiltinCall") {
        return render_builtin_array_expr_at_index_checked(&builtin, index, cfg);
    }

    no_render_match()
}

fn render_array_expr_at_index_or_scalar_checked(
    expr: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    if let Some(rendered) = render_array_expr_at_index_checked(expr, index, cfg)? {
        return Ok(Some(rendered));
    }
    render_expression(expr, cfg).map(Some)
}

fn try_render_indexed_var_ref(
    var_ref: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Ok(subscripts) = get_field(var_ref, "subscripts") else {
        return no_render_match();
    };
    if subscripts.len().unwrap_or(0) != 0 {
        return no_render_match();
    }

    let Some(raw_name) = get_field(var_ref, "name")
        .ok()
        .map(|n| {
            get_field(&n, "0")
                .map(|v| super::value_to_string(&v))
                .unwrap_or_else(|_| super::value_to_string(&n))
        })
        .filter(|name| !name.is_empty())
    else {
        return no_render_match();
    };

    if cfg.subscript_underscore {
        let indexed_ref = format!("{raw_name}[{index}]");
        if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &indexed_ref) {
            return Ok(Some(symbol));
        }
        let name = super::emitted_symbol(&raw_name, cfg)?;
        Ok(Some(format!("{name}_{index}")))
    } else if cfg.one_based_index {
        let name = super::emitted_symbol(&raw_name, cfg)?;
        Ok(Some(format!("{name}[{index}]")))
    } else {
        let name = super::emitted_symbol(&raw_name, cfg)?;
        Ok(Some(format!("{}[{}]", name, index - 1)))
    }
}

fn render_binary_array_expr_at_index_checked(
    binary: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Ok(lhs) = get_field(binary, "lhs") else {
        return no_render_match();
    };
    let Ok(rhs) = get_field(binary, "rhs") else {
        return no_render_match();
    };
    let Some(lhs_render) = render_array_expr_at_index_or_scalar_checked(&lhs, index, cfg)? else {
        return no_render_match();
    };
    let Some(rhs_render) = render_array_expr_at_index_or_scalar_checked(&rhs, index, cfg)? else {
        return no_render_match();
    };
    let Ok(op) = get_field(binary, "op") else {
        return no_render_match();
    };
    if render_expr::is_mul_elem_op(&op)
        && let Some(func) = &cfg.mul_elem_fn
    {
        return Ok(Some(format!("{func}({lhs_render}, {rhs_render})")));
    }
    if render_expr::is_exp_op(&op) {
        if let Some(power_fn) = &cfg.power_fn {
            return Ok(Some(format!("{power_fn}({lhs_render}, {rhs_render})")));
        }
        if cfg
            .power
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphabetic())
        {
            return Ok(Some(format!("{}({lhs_render}, {rhs_render})", cfg.power)));
        }
    }
    let op_str = render_expr::get_binop_string(&op, cfg)?;
    Ok(Some(format!("({lhs_render} {op_str} {rhs_render})")))
}

fn render_builtin_array_expr_at_index_checked(
    builtin: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let func_name = get_field(builtin, "function")
        .map(|f| super::value_to_string(&f))
        .map_err(|err| render_err(format!("BuiltinCall missing 'function' field: {err}")))?;
    let args = get_field(builtin, "args")
        .map_err(|err| render_err(format!("BuiltinCall {func_name} missing 'args': {err}")))?;
    match func_name.as_str() {
        "Smooth" => {
            let inner = required_builtin_arg(&args, 1, "BuiltinCall Smooth")?;
            render_array_expr_at_index_or_scalar_checked(&inner, index, cfg)
        }
        "NoEvent" | "Homotopy" | "Previous" | "Hold" | "NoClock" | "SubSample" | "SuperSample"
        | "ShiftSample" | "BackSample" => {
            let inner = required_builtin_arg(&args, 0, &format!("BuiltinCall {func_name}"))?;
            render_array_expr_at_index_or_scalar_checked(&inner, index, cfg)
        }
        "Pre" => {
            let inner = required_builtin_arg(&args, 0, "BuiltinCall Pre")?;
            let Some(selected) = render_array_expr_at_index_or_scalar_checked(&inner, index, cfg)?
            else {
                return no_render_match();
            };
            Ok(Some(format!("pre({selected})")))
        }
        _ => no_render_match(),
    }
}

fn required_builtin_arg(
    args: &Value,
    index: usize,
    context: &str,
) -> Result<Value, minijinja::Error> {
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

/// Flatten a Value expression tree of Add/Sub into signed terms.
fn flatten_value_add_terms(expr: &Value, positive: bool, terms: &mut Vec<(bool, Value)>) {
    if let Ok(binary) = get_field(expr, "Binary") {
        if is_add_op(&binary)
            && let (Ok(lhs), Ok(rhs)) = (get_field(&binary, "lhs"), get_field(&binary, "rhs"))
        {
            flatten_value_add_terms(&lhs, positive, terms);
            flatten_value_add_terms(&rhs, positive, terms);
            return;
        }
        if is_sub_op(&binary)
            && let (Ok(lhs), Ok(rhs)) = (get_field(&binary, "lhs"), get_field(&binary, "rhs"))
        {
            flatten_value_add_terms(&lhs, positive, terms);
            flatten_value_add_terms(&rhs, !positive, terms);
            return;
        }
    }
    // Check for Unary Minus
    if let Ok(unary) = get_field(expr, "Unary")
        && let Ok(op) = get_field(&unary, "op").map(|v| v.to_string())
        && (op.contains("Minus") || op.contains("Neg"))
        && let Ok(inner) = get_field(&unary, "rhs")
    {
        flatten_value_add_terms(&inner, !positive, terms);
        return;
    }
    terms.push((positive, expr.clone()));
}

/// Check if an expression is `BuiltinCall { function: Der, args: [VarRef { name, subscripts }] }`
/// where the full name (including subscripts) matches the given state name.
///
/// Handles both scalar (`der(x)` matches `"x"`) and indexed
/// (`der(x[1])` matches `"x[1]"`) forms.
fn is_der_of(expr: &Value, state_name: &str) -> bool {
    let state_name = state_name.trim_matches('"');
    der_ref_name(expr).is_some_and(|name| name == state_name)
}

fn der_index_of_base(expr: &Value, state_base: &str) -> Option<usize> {
    let name = der_ref_name(expr)?;
    let (base, index) = parse_indexed_ref(&name)?;
    if base == state_base {
        Some(index)
    } else {
        None
    }
}

fn der_ref_name(expr: &Value) -> Option<String> {
    let Ok(builtin) = get_field(expr, "BuiltinCall") else {
        return None;
    };
    let Ok(func) = get_field(&builtin, "function") else {
        return None;
    };
    let func_str = func.to_string();
    if func_str != "Der" && func_str != "\"Der\"" {
        return None;
    }
    let Ok(args) = get_field(&builtin, "args") else {
        return None;
    };
    let Ok(first_arg) = args.get_item(&Value::from(0)) else {
        return None;
    };
    let Ok(var_ref) = get_field(&first_arg, "VarRef") else {
        return None;
    };
    Some(var_ref_full_name(&var_ref))
}

fn is_der_of_whole(expr: &Value, state_base: &str) -> bool {
    let state_base = state_base.trim_matches('"');
    der_ref_name(expr).is_some_and(|name| name == state_base)
}

/// Check if an expression is `VarRef { name, subscripts }` matching the given variable name.
///
/// Handles both scalar (`y` matches `"y"`) and indexed
/// (`y[1]` matches `"y[1]"`) forms.
fn is_var_ref_of(expr: &Value, target_name: &str) -> bool {
    let target_name = target_name.trim_matches('"');
    let Ok(var_ref) = get_field(expr, "VarRef") else {
        return false;
    };
    var_ref_full_name(&var_ref) == target_name
}

/// Extract the coefficient from a `k*der(x)` or `der(x)*k` expression.
///
/// If `expr` is `Binary { Mul, lhs: k, rhs: der(x) }` or `Binary { Mul, lhs: der(x), rhs: k }`,
/// returns the rendered `k`. Otherwise returns None.
fn extract_der_coefficient(
    expr: &Value,
    state_name: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Ok(binary) = get_field(expr, "Binary") else {
        return no_render_match();
    };
    if !is_mul_op(&binary) {
        return no_render_match();
    }
    let Ok(lhs) = get_field(&binary, "lhs") else {
        return no_render_match();
    };
    let Ok(rhs) = get_field(&binary, "rhs") else {
        return no_render_match();
    };

    if is_der_of(&rhs, state_name) {
        // k * der(x) → coefficient is k
        return Ok(Some(render_expression(&lhs, cfg)?));
    }
    if is_der_of(&lhs, state_name) {
        // der(x) * k → coefficient is k
        return Ok(Some(render_expression(&rhs, cfg)?));
    }
    no_render_match()
}

/// Check if a Binary expression's op is Mul or MulElem.
fn is_mul_op(binary: &Value) -> bool {
    if let Ok(op) = get_field(binary, "op") {
        return is_variant(&op, "Mul") || is_variant(&op, "MulElem");
    }
    false
}

/// Check if a Binary expression's op is Sub or SubElem.
fn is_sub_op(binary: &Value) -> bool {
    if let Ok(op) = get_field(binary, "op") {
        return is_variant(&op, "Sub") || is_variant(&op, "SubElem");
    }
    false
}

/// Check if a Binary expression's op is Add or AddElem.
fn is_add_op(binary: &Value) -> bool {
    if let Ok(op) = get_field(binary, "op") {
        return is_variant(&op, "Add") || is_variant(&op, "AddElem");
    }
    false
}

/// Check if an expression tree contains a `der()` call anywhere.
/// Used to skip algebraic equations that are actually ODE equations.
/// Check whether any element in a Value list satisfies a predicate.
fn any_arg_matches(args: &Value, predicate: fn(&Value) -> bool) -> bool {
    let Some(len) = args.len() else {
        return false;
    };
    for i in 0..len {
        if let Ok(arg) = args.get_item(&Value::from(i))
            && predicate(&arg)
        {
            return true;
        }
    }
    false
}

fn contains_der(expr: &Value) -> bool {
    // Direct BuiltinCall with Der
    if let Ok(builtin) = get_field(expr, "BuiltinCall") {
        if let Ok(func) = get_field(&builtin, "function") {
            let s = func.to_string();
            if s == "Der" || s == "\"Der\"" {
                return true;
            }
        }
        // Check args recursively
        if let Ok(args) = get_field(&builtin, "args") {
            return any_arg_matches(&args, contains_der);
        }
        return false;
    }
    // Binary
    if let Ok(binary) = get_field(expr, "Binary") {
        if let Ok(lhs) = get_field(&binary, "lhs")
            && contains_der(&lhs)
        {
            return true;
        }
        if let Ok(rhs) = get_field(&binary, "rhs")
            && contains_der(&rhs)
        {
            return true;
        }
        return false;
    }
    // Unary
    if let Ok(unary) = get_field(expr, "Unary")
        && let Ok(inner) = get_field(&unary, "rhs")
    {
        return contains_der(&inner);
    }
    false
}

fn contains_der_of_base(expr: &Value, state_base: &str) -> bool {
    if der_index_of_base(expr, state_base).is_some() || is_der_of_whole(expr, state_base) {
        return true;
    }
    if let Ok(builtin) = get_field(expr, "BuiltinCall") {
        if let Ok(args) = get_field(&builtin, "args") {
            return any_arg_matches_with_state_base(&args, state_base, contains_der_of_base);
        }
        return false;
    }
    if let Ok(binary) = get_field(expr, "Binary") {
        if let Ok(lhs) = get_field(&binary, "lhs")
            && contains_der_of_base(&lhs, state_base)
        {
            return true;
        }
        if let Ok(rhs) = get_field(&binary, "rhs")
            && contains_der_of_base(&rhs, state_base)
        {
            return true;
        }
        return false;
    }
    if let Ok(unary) = get_field(expr, "Unary")
        && let Ok(inner) = get_field(&unary, "rhs")
    {
        return contains_der_of_base(&inner, state_base);
    }
    false
}

fn any_arg_matches_with_state_base(
    args: &Value,
    state_base: &str,
    predicate: fn(&Value, &str) -> bool,
) -> bool {
    let Some(len) = args.len() else {
        return false;
    };
    for i in 0..len {
        if let Ok(arg) = args.get_item(&Value::from(i))
            && predicate(&arg, state_base)
        {
            return true;
        }
    }
    false
}

/// Reconstruct the full name of a VarRef including 1-based subscripts.
///
/// `VarRef { name: "x", subscripts: [] }` → `"x"`
/// `VarRef { name: "x", subscripts: [Index(1)] }` → `"x[1]"`
/// `VarRef { name: "x", subscripts: [Index(1), Index(2)] }` → `"x[1,2]"`
fn var_ref_full_name(var_ref: &Value) -> String {
    let base_name = var_ref_base_name(var_ref);
    if base_name.is_empty() {
        return String::new();
    }

    // Check for subscripts
    let Ok(subs) = get_field(var_ref, "subscripts") else {
        return base_name;
    };
    let Some(len) = subs.len() else {
        return base_name;
    };
    if len == 0 {
        return base_name;
    }

    // Build subscript string (1-based Modelica convention)
    let mut sub_parts = Vec::new();
    for i in 0..len {
        if let Ok(sub) = subs.get_item(&Value::from(i))
            && let Ok(idx) = get_field(&sub, "Index")
            && let Ok(val) = subscript_index_value(&idx)
        {
            sub_parts.push(val.to_string());
        }
    }
    if sub_parts.is_empty() {
        return base_name;
    }
    format!("{}[{}]", base_name, sub_parts.join(","))
}

fn var_ref_base_name(var_ref: &Value) -> String {
    let Ok(name) = get_field(var_ref, "name") else {
        return String::new();
    };
    render_serialized_name(&name)
}

fn parse_indexed_ref(name: &str) -> Option<(String, usize)> {
    let trimmed = name.trim_matches('"');
    let (base, subscripts) = rumoca_core::split_trailing_subscript_suffix(trimmed)?;
    let mut parts = subscripts.split(',');
    let first = parts.next()?.trim().parse::<usize>().ok()?;
    if first < 1 {
        return None;
    }
    if parts.next().is_some() {
        return None;
    }
    Some((base.to_string(), first))
}

fn list_any(list: &Value, mut predicate: impl FnMut(Value) -> bool) -> bool {
    let Some(len) = list.len() else {
        return false;
    };
    for i in 0..len {
        let Ok(item) = list.get_item(&Value::from(i)) else {
            continue;
        };
        if predicate(item) {
            return true;
        }
    }
    false
}

/// Convert a source reference into the emitted symbol configured by the template.
pub(super) fn var_name_to_symbol(name: &str, cfg: &ExprConfig) -> RenderResult {
    if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), name) {
        return Ok(symbol);
    }
    if let Some((base, subscripts)) = rumoca_core::split_trailing_subscript_suffix(name) {
        let suffix = subscripts.replace(',', "_").replace(' ', "");
        let base_symbol = super::emitted_symbol(base, cfg)?;
        return Ok(format!("{base_symbol}_{suffix}"));
    }

    super::emitted_symbol(name, cfg)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indexed_ref_parser_uses_balanced_trailing_subscript() {
        assert_eq!(parse_indexed_ref("x[3]"), Some(("x".to_string(), 3)));
        assert_eq!(
            parse_indexed_ref("arr[index.with.dot].x[4]"),
            Some(("arr[index.with.dot].x".to_string(), 4))
        );
        assert_eq!(parse_indexed_ref("x[3,4]"), None);
        assert_eq!(parse_indexed_ref("x[0]"), None);
        assert_eq!(parse_indexed_ref("x[3"), None);
    }

    #[test]
    fn var_name_to_symbol_uses_balanced_trailing_subscript() {
        let cfg = ExprConfig::default();

        assert_eq!(var_name_to_symbol("x[3]", &cfg).unwrap(), "x_3");
        assert_eq!(
            var_name_to_symbol("arr[index.with.dot].x[4]", &cfg).unwrap(),
            "arr_index_with_dot_x_4"
        );
        assert_eq!(var_name_to_symbol("x[3", &cfg).unwrap(), "x_3");
    }
}
