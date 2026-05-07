//! C-backend template functions for FMI2 and embedded-C code generation.
//!
//! These functions are registered in the minijinja environment and used by
//! `fmi2/model.c.jinja` and `embedded_c/model.c.jinja` templates to extract explicit
//! ODE/algebraic RHS expressions from residual-form DAE equations.

use super::{ExprConfig, RenderResult};
use minijinja::Value;
use render_expr::{get_field, render_expression};
use std::collections::BTreeMap;

use super::render_expr;

// ── Functions ───────────────────────────────────────────────────────────

/// Render element `index` (1-based) of an expression. If the expression is an
/// `Array { elements }`, extracts `elements[index-1]` and renders it.
/// Otherwise renders the whole expression (scalar broadcast).
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
    let idx = index.as_usize().unwrap_or(1);

    // If expression is an Array, flatten nested arrays (row-major) and pick element idx.
    if get_field(&expr, "Array").is_ok() {
        let mut flat_elems: Vec<Value> = Vec::new();
        collect_array_elements_flat(&expr, &mut flat_elems);
        if idx >= 1
            && idx <= flat_elems.len()
            && let Some(elem) = flat_elems.get(idx - 1)
        {
            return render_expression(elem, &cfg);
        }
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

/// Check if a function has Complex-typed parameters.
///
/// Returns "yes" if any input parameter has type_name == "Complex".
pub(super) fn has_complex_params_function(func: Value) -> String {
    if let Ok(inputs) = get_field(&func, "inputs")
        && list_any(&inputs, |param| {
            get_field(&param, "type_name")
                .map(|type_name| type_name.to_string().trim_matches('"') == "Complex")
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

    let rhs = eq.get_attr("rhs").unwrap_or(Value::UNDEFINED);

    // Residual form: rhs is Binary { Sub, lhs: der(x), rhs: expr }
    // We want to return just `expr`
    if let Ok(binary) = get_field(&rhs, "Binary")
        && is_sub_op(&binary)
    {
        let rhs_expr = get_field(&binary, "rhs")
            .and_then(|v| render_expression(&v, &cfg))
            .unwrap_or_default();
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
        if let Some(rhs_expr) = find_derivative_rhs(&eq, &name_str, &cfg) {
            return Ok(rhs_expr);
        }
    }

    if let Some(rhs_expr) =
        find_linear_derivative_system_rhs_in_equations(&equations, &name_str, &cfg)
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
        if let Some(rhs_expr) = find_algebraic_rhs_direct(&eq, &name_str, &cfg) {
            return Ok(rhs_expr);
        }
    }

    let Ok(iter) = equations.try_iter() else {
        return Ok("0.0".to_string());
    };
    for eq in iter {
        if let Some(rhs_expr) = find_algebraic_rhs(&eq, &name_str, &cfg) {
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
        return Ok(var_name_to_symbol(&name_str, &cfg));
    };
    for eq in iter {
        if let Some(rhs_expr) = find_algebraic_rhs(&eq, &name_str, &cfg) {
            return Ok(rhs_expr);
        }
    }

    Ok(var_name_to_symbol(&name_str, &cfg))
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
            if let Some(rhs_expr) = find_algebraic_rhs(&eq, &name, &cfg) {
                return Ok(rhs_expr);
            }
        }
    }
    if let Ok(iter) = equations_m.try_iter() {
        for eq in iter {
            if let Some(rhs_expr) = find_algebraic_rhs(&eq, &name, &cfg) {
                return Ok(rhs_expr);
            }
        }
    }

    if let Some(synthesized) = synthesize_discrete_statespace_rhs(&name, &dae, &cfg) {
        return Ok(synthesized);
    }

    Ok(var_name_to_symbol(&name, &cfg))
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
fn find_derivative_rhs(eq: &Value, state_name: &str, cfg: &ExprConfig) -> Option<String> {
    let rhs = eq.get_attr("rhs").unwrap_or(Value::UNDEFINED);

    // Try direct Binary{Sub} first, then try unwrapping Unary{Minus}.
    // 0 = -(A - B) is equivalent to 0 = B - A, so we swap lhs/rhs.
    let (binary, swapped) = if let Ok(b) = get_field(&rhs, "Binary") {
        (b, false)
    } else if let Ok(unary) = get_field(&rhs, "Unary") {
        let op = get_field(&unary, "op")
            .ok()
            .map(|v| v.to_string())
            .unwrap_or_default();
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
    let (lhs, rhs_val) = if swapped {
        // -(A - B) = B - A: swap the operands
        (
            get_field(&binary, "rhs").ok()?,
            get_field(&binary, "lhs").ok()?,
        )
    } else {
        (
            get_field(&binary, "lhs").ok()?,
            get_field(&binary, "rhs").ok()?,
        )
    };

    // Case 1: 0 = der(x) - expr → der(x) = expr
    if is_der_of(&lhs, state_name) {
        let rhs_expr = render_expression(&rhs_val, cfg).unwrap_or_default();
        return Some(rhs_expr);
    }

    // Case 2: 0 = expr - der(x) → der(x) = expr
    if is_der_of(&rhs_val, state_name) {
        let lhs_expr = render_expression(&lhs, cfg).unwrap_or_default();
        return Some(lhs_expr);
    }

    // Case 3: 0 = k*der(x) - expr or 0 = der(x)*k - expr → der(x) = expr / k
    if let Some(coeff) = extract_der_coefficient(&lhs, state_name, cfg) {
        let rhs_expr = render_expression(&rhs_val, cfg).unwrap_or_default();
        return Some(format!("({rhs_expr}) / ({coeff})"));
    }

    // Case 4: 0 = expr - k*der(x) or 0 = expr - der(x)*k → der(x) = expr / k
    if let Some(coeff) = extract_der_coefficient(&rhs_val, state_name, cfg) {
        let lhs_expr = render_expression(&lhs, cfg).unwrap_or_default();
        return Some(format!("({lhs_expr}) / ({coeff})"));
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
        find_linear_derivative_system_rhs(&lhs, &rhs_val, state_name, scalar_count, cfg)
    {
        return Some(rhs_expr);
    }

    None
}

fn find_linear_derivative_system_rhs(
    lhs: &Value,
    rhs: &Value,
    state_name: &str,
    scalar_count: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    let (state_base, component) = parse_indexed_ref(state_name)?;
    let n = scalar_count.max(component);
    if component == 0 || component > n {
        return None;
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

    None
}

fn find_linear_derivative_system_rhs_in_equations(
    equations: &Value,
    state_name: &str,
    cfg: &ExprConfig,
) -> Option<String> {
    let (state_base, component) = parse_indexed_ref(state_name)?;
    let mut rows = Vec::new();
    let mut n = component;

    let iter = equations.try_iter().ok()?;
    for eq in iter {
        if let Some((coefficients, rhs)) = extract_linear_derivative_row(&eq, &state_base, cfg) {
            if let Some(max_col) = coefficients.keys().next_back().copied() {
                n = n.max(max_col);
            }
            rows.push((coefficients, rhs));
        }
    }

    if rows.len() < n || component == 0 || component > n {
        return None;
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

    Some(format!(
        "__rumoca_solve_linear_component((double[]){{{}}}, (double[]){{{}}}, {}, {})",
        matrix_entries.join(", "),
        rhs_entries.join(", "),
        n,
        component - 1
    ))
}

fn extract_linear_derivative_row(
    eq: &Value,
    state_base: &str,
    cfg: &ExprConfig,
) -> Option<(BTreeMap<usize, String>, String)> {
    let rhs = eq.get_attr("rhs").unwrap_or(Value::UNDEFINED);
    let (lhs, rhs_val) = decompose_subtraction(&rhs)?;

    if contains_der_of_base(&lhs, state_base) && !contains_der(&rhs_val) {
        let coefficients = extract_linear_derivative_coefficients(&lhs, state_base, cfg)?;
        let rhs_rendered = render_expression(&rhs_val, cfg).ok()?;
        return Some((coefficients, rhs_rendered));
    }

    if contains_der_of_base(&rhs_val, state_base) && !contains_der(&lhs) {
        let coefficients = extract_linear_derivative_coefficients(&rhs_val, state_base, cfg)?;
        let lhs_rendered = render_expression(&lhs, cfg).ok()?;
        return Some((coefficients, lhs_rendered));
    }

    None
}

fn extract_linear_derivative_coefficients(
    expr: &Value,
    state_base: &str,
    cfg: &ExprConfig,
) -> Option<BTreeMap<usize, String>> {
    let mut terms = Vec::new();
    flatten_value_add_terms(expr, true, &mut terms);

    let mut coefficients: BTreeMap<usize, String> = BTreeMap::new();
    for (positive, term) in terms {
        if !contains_der_of_base(&term, state_base) {
            return None;
        }
        let (component, coefficient) = extract_derivative_term_coefficient(&term, state_base, cfg)?;
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
        None
    } else {
        Some(coefficients)
    }
}

fn extract_derivative_term_coefficient(
    expr: &Value,
    state_base: &str,
    cfg: &ExprConfig,
) -> Option<(usize, String)> {
    if let Some(component) = der_index_of_base(expr, state_base) {
        return Some((component, "1.0".to_string()));
    }

    let binary = get_field(expr, "Binary").ok()?;
    if !is_mul_op(&binary) {
        return None;
    }
    let lhs = get_field(&binary, "lhs").ok()?;
    let rhs = get_field(&binary, "rhs").ok()?;

    if let Some(component) = der_index_of_base(&rhs, state_base)
        && !contains_der(&lhs)
    {
        return Some((component, render_expression(&lhs, cfg).ok()?));
    }
    if let Some(component) = der_index_of_base(&lhs, state_base)
        && !contains_der(&rhs)
    {
        return Some((component, render_expression(&rhs, cfg).ok()?));
    }

    None
}

fn decompose_subtraction(expr: &Value) -> Option<(Value, Value)> {
    let (binary, swapped) = if let Ok(b) = get_field(expr, "Binary") {
        (b, false)
    } else if let Ok(unary) = get_field(expr, "Unary") {
        let op = get_field(&unary, "op")
            .ok()
            .map(|v| v.to_string())
            .unwrap_or_default();
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
) -> Option<String> {
    if n == 0 {
        return None;
    }

    let mut matrix_entries = Vec::with_capacity(n * n);
    for row in 1..=n {
        for col in 1..=n {
            let (source_row, source_col) = if product.transpose {
                (col, row)
            } else {
                (row, col)
            };
            matrix_entries.push(render_matrix_expr_at_indices(
                &product.matrix,
                source_row,
                source_col,
                n,
                cfg,
            )?);
        }
    }

    let mut rhs_entries = Vec::with_capacity(n);
    for idx in 1..=n {
        rhs_entries.push(render_array_expr_at_index_or_scalar(rhs, idx, cfg)?);
    }

    Some(format!(
        "__rumoca_solve_linear_component((double[]){{{}}}, (double[]){{{}}}, {}, {})",
        matrix_entries.join(", "),
        rhs_entries.join(", "),
        n,
        component - 1
    ))
}

fn render_matrix_expr_at_indices(
    expr: &Value,
    row: usize,
    col: usize,
    columns: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    if let Ok(var_ref) = get_field(expr, "VarRef") {
        return render_var_ref_with_indices(&var_ref, &[row, col], cfg);
    }

    if get_field(expr, "Array").is_ok() {
        return render_array_expr_at_index(expr, (row - 1) * columns + col, cfg);
    }

    if let Ok(binary) = get_field(expr, "Binary") {
        let lhs = get_field(&binary, "lhs").ok()?;
        let rhs = get_field(&binary, "rhs").ok()?;
        let lhs_render = render_matrix_expr_at_indices_or_scalar(&lhs, row, col, columns, cfg)?;
        let rhs_render = render_matrix_expr_at_indices_or_scalar(&rhs, row, col, columns, cfg)?;
        let op = get_field(&binary, "op").ok()?;
        let op_str = render_expr::get_binop_string(&op, cfg).ok()?;
        return Some(format!("({lhs_render} {op_str} {rhs_render})"));
    }

    if let Ok(unary) = get_field(expr, "Unary") {
        let rhs = get_field(&unary, "rhs").ok()?;
        let rhs_render = render_matrix_expr_at_indices_or_scalar(&rhs, row, col, columns, cfg)?;
        let op = get_field(&unary, "op").ok()?;
        let op_str = render_expr::get_unop_string(&op, cfg).ok()?;
        return Some(format!("({op_str}{rhs_render})"));
    }

    None
}

fn render_matrix_expr_at_indices_or_scalar(
    expr: &Value,
    row: usize,
    col: usize,
    columns: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    render_matrix_expr_at_indices(expr, row, col, columns, cfg)
        .or_else(|| render_expression(expr, cfg).ok())
}

fn render_var_ref_with_indices(
    var_ref: &Value,
    indices: &[usize],
    cfg: &ExprConfig,
) -> Option<String> {
    let subscripts = get_field(var_ref, "subscripts").ok()?;
    if subscripts.len().unwrap_or(0) != 0 {
        return None;
    }

    let raw_name = var_ref_base_name(var_ref);
    if raw_name.is_empty() {
        return None;
    }

    let index_text = indices
        .iter()
        .map(|idx| idx.to_string())
        .collect::<Vec<_>>()
        .join(",");
    let indexed_ref = format!("{raw_name}[{index_text}]");
    if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &indexed_ref) {
        return Some(symbol);
    }

    let name = super::emitted_symbol_or_fallback(&raw_name, cfg);
    if cfg.subscript_underscore {
        return Some(format!(
            "{}_{}",
            name,
            indices
                .iter()
                .map(|idx| idx.to_string())
                .collect::<Vec<_>>()
                .join("_")
        ));
    }
    if cfg.one_based_index {
        return Some(format!("{name}[{index_text}]"));
    }

    if indices.len() == 1 {
        return Some(format!("{}[{}]", name, indices[0] - 1));
    }

    None
}

/// Extract the algebraic RHS from a single equation if it matches `0 = var_name - expr`.
/// Helper for `alg_rhs_for_var_function`; decomposes MLS B.1a residual form for algebraics.
///
/// Matches both scalar (`y`) and indexed (`y[1]`) forms via `is_var_ref_of`,
/// which reconstructs the full VarRef name including subscripts.
fn find_algebraic_rhs(eq: &Value, var_name: &str, cfg: &ExprConfig) -> Option<String> {
    // Try direct assignment form first: lhs = rhs
    if let Some(result) = find_algebraic_rhs_assignment(eq, var_name, cfg) {
        return Some(result);
    }

    let rhs = eq.get_attr("rhs").unwrap_or(Value::UNDEFINED);

    // Try subtraction form first: 0 = var - expr or 0 = -(var - expr)
    if let Some(result) = find_algebraic_rhs_subtraction(&rhs, var_name, cfg) {
        return Some(result);
    }

    // Try additive form: 0 = a + b + c (connection equations)
    if let Some(result) = find_algebraic_rhs_additive(&rhs, var_name, cfg) {
        return Some(result);
    }

    // Try array-level binding: searching for "v[i]" but equation has "v" (whole array)
    // with an Array RHS. Extract element i from the array.
    if let Some(result) = find_algebraic_rhs_array_element(&rhs, var_name, cfg) {
        return Some(result);
    }

    None
}

/// Extract only direct defining equations for `var_name`.
///
/// This pass intentionally ignores equations where the target appears on the
/// RHS of another variable's equation. For example, in
/// `omega_error = omega_cmd - omega`, `omega_cmd` is algebraically solvable, but
/// if a later connection equation directly defines `omega_cmd`, that direct
/// equation is the correct explicit assignment for generated C.
fn find_algebraic_rhs_direct(eq: &Value, var_name: &str, cfg: &ExprConfig) -> Option<String> {
    if let Some(result) = find_algebraic_rhs_assignment_direct(eq, var_name, cfg) {
        return Some(result);
    }

    let rhs = eq.get_attr("rhs").unwrap_or(Value::UNDEFINED);
    find_algebraic_rhs_subtraction_direct(&rhs, var_name, cfg)
}

fn find_algebraic_rhs_assignment_direct(
    eq: &Value,
    var_name: &str,
    cfg: &ExprConfig,
) -> Option<String> {
    let lhs = eq.get_attr("lhs").ok()?;
    let rhs = eq.get_attr("rhs").ok()?;
    render_direct_rhs_for_lhs(&lhs, &rhs, var_name, cfg)
}

fn find_algebraic_rhs_subtraction_direct(
    rhs: &Value,
    var_name: &str,
    cfg: &ExprConfig,
) -> Option<String> {
    let binary = get_field(rhs, "Binary").ok()?;
    if !is_sub_op(&binary) {
        return None;
    }
    let lhs_side = get_field(&binary, "lhs").ok()?;
    let rhs_side = get_field(&binary, "rhs").ok()?;
    if contains_der(&rhs_side) {
        return None;
    }
    render_direct_rhs_for_lhs(&lhs_side, &rhs_side, var_name, cfg)
}

fn render_direct_rhs_for_lhs(
    lhs: &Value,
    rhs: &Value,
    var_name: &str,
    cfg: &ExprConfig,
) -> Option<String> {
    if is_var_ref_of(lhs, var_name) {
        return render_expression(rhs, cfg).ok();
    }
    if let Some(index) = array_lhs_element_index(lhs, var_name) {
        return render_array_expr_at_index_or_scalar(rhs, index, cfg);
    }
    if let Some(index) = whole_array_lhs_index(lhs, var_name) {
        return render_array_expr_at_index_or_scalar(rhs, index, cfg);
    }
    None
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
fn find_algebraic_rhs_assignment(eq: &Value, var_name: &str, cfg: &ExprConfig) -> Option<String> {
    let lhs = eq.get_attr("lhs").ok()?;

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
        let rhs = eq.get_attr("rhs").ok()?;

        // If the RHS is an If-expression with sample() guard (when-statement),
        // extract the update expression from the true branch, not the full ternary.
        if let Ok(if_expr) = get_field(&rhs, "If")
            && let Ok(branches) = get_field(&if_expr, "branches")
            && let Ok(first_branch) = branches.get_item(&Value::from(0))
            && let Ok(branch_array) = first_branch.try_iter()
        {
            // branches is a list of [condition, expression] pairs.
            let items: Vec<_> = branch_array.take(2).collect();
            if let Some(rendered) = items
                .get(1)
                .and_then(|update_expr| render_expression(update_expr, cfg).ok())
            {
                return Some(rendered);
            }
        }

        // Fall back to rendering the entire RHS (for non-guarded cases)
        return render_expression(&rhs, cfg).ok();
    }

    // Array-level assignment support: searching for "v[i]" while equation lhs is
    // the whole array "v" and rhs is Array{...}.
    let bracket_pos = var_name.find('[')?;
    let base_name = &var_name[..bracket_pos];
    let subscript_str = var_name[bracket_pos + 1..].trim_end_matches(']');
    let index: usize = subscript_str.parse().ok()?;
    if index < 1 {
        return None;
    }
    if !is_var_ref_of(&lhs, base_name) {
        return None;
    }

    let rhs = eq.get_attr("rhs").ok()?;
    let array = get_field(&rhs, "Array").ok()?;
    let elements = get_field(&array, "elements").ok()?;
    let len = elements.len()?;
    if index > len {
        return None;
    }
    let elem = elements.get_item(&Value::from(index - 1)).ok()?;
    render_expression(&elem, cfg).ok()
}

/// Try subtraction form: 0 = var - expr, 0 = expr - var, 0 = -(A - B)
fn find_algebraic_rhs_subtraction(rhs: &Value, var_name: &str, cfg: &ExprConfig) -> Option<String> {
    // Try direct Binary{Sub} first, then try unwrapping Unary{Minus}.
    let (binary, swapped) = if let Ok(b) = get_field(rhs, "Binary") {
        (b, false)
    } else if let Ok(unary) = get_field(rhs, "Unary") {
        let op = get_field(&unary, "op")
            .ok()
            .map(|v| v.to_string())
            .unwrap_or_default();
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

    let (lhs_side, rhs_side) = if swapped {
        (
            get_field(&binary, "rhs").ok()?,
            get_field(&binary, "lhs").ok()?,
        )
    } else {
        (
            get_field(&binary, "lhs").ok()?,
            get_field(&binary, "rhs").ok()?,
        )
    };

    // Case 1: 0 = var - expr → var = expr
    if is_var_ref_of(&lhs_side, var_name) && !contains_der(&rhs_side) {
        let rhs_expr = render_expression(&rhs_side, cfg).unwrap_or_default();
        return Some(rhs_expr);
    }

    // Case 2: 0 = expr - var → var = expr
    if is_var_ref_of(&rhs_side, var_name) && !contains_der(&lhs_side) {
        let lhs_expr = render_expression(&lhs_side, cfg).unwrap_or_default();
        return Some(lhs_expr);
    }

    // Case 3: 0 = coeff * var - expr → var = expr / coeff
    if !contains_der(&lhs_side) && !contains_der(&rhs_side) {
        if let Some(coeff) = extract_mul_coefficient(&lhs_side, var_name, cfg) {
            let rhs_expr = render_expression(&rhs_side, cfg).unwrap_or_default();
            return Some(format!("({rhs_expr}) / ({coeff})"));
        }
        // Case 4: 0 = expr - coeff * var → var = expr / coeff
        if let Some(coeff) = extract_mul_coefficient(&rhs_side, var_name, cfg) {
            let lhs_expr = render_expression(&lhs_side, cfg).unwrap_or_default();
            return Some(format!("({lhs_expr}) / ({coeff})"));
        }
    }

    None
}

/// Extract the coefficient from a `coeff * var` or `var * coeff` expression.
/// Returns the rendered coefficient string if the expression is a Mul with one
/// side being a VarRef to the target variable.
fn extract_mul_coefficient(expr: &Value, var_name: &str, cfg: &ExprConfig) -> Option<String> {
    let binary = get_field(expr, "Binary").ok()?;
    if !is_mul_op(&binary) {
        return None;
    }
    let lhs = get_field(&binary, "lhs").ok()?;
    let rhs = get_field(&binary, "rhs").ok()?;

    // coeff * var
    if is_var_ref_of(&rhs, var_name) && !contains_var_ref(&lhs, var_name) {
        return render_expression(&lhs, cfg).ok();
    }
    // var * coeff
    if is_var_ref_of(&lhs, var_name) && !contains_var_ref(&rhs, var_name) {
        return render_expression(&rhs, cfg).ok();
    }
    None
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
fn find_algebraic_rhs_additive(rhs: &Value, var_name: &str, cfg: &ExprConfig) -> Option<String> {
    // Flatten the Add/Sub tree into signed terms
    let mut terms: Vec<(bool, Value)> = Vec::new();
    flatten_value_add_terms(rhs, true, &mut terms);
    if terms.len() < 2 {
        return None;
    }

    // Skip if any term contains der()
    if terms.iter().any(|(_, t)| contains_der(t)) {
        return None;
    }

    // Find which term is the target variable
    let mut var_idx = None;
    for (i, (_, term)) in terms.iter().enumerate() {
        if is_var_ref_of(term, var_name) {
            if var_idx.is_some() {
                return None; // multiple occurrences
            }
            var_idx = Some(i);
        }
    }
    let var_idx = var_idx?;
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
        let rendered = render_expression(term, cfg).ok()?;
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
    Some(expr)
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
) -> Option<String> {
    // Only applies when var_name has a subscript, e.g. "error_dot[2]"
    let bracket_pos = var_name.find('[')?;
    let base_name = &var_name[..bracket_pos];
    let subscript_str = var_name[bracket_pos + 1..].trim_end_matches(']');
    let index: usize = subscript_str.parse().ok()?;
    if index < 1 {
        return None;
    }

    // Try subtraction form: 0 = base_var - Array{...}
    let binary = get_field(rhs, "Binary").ok()?;
    if !is_sub_op(&binary) {
        return None;
    }
    let lhs_side = get_field(&binary, "lhs").ok()?;
    let rhs_side = get_field(&binary, "rhs").ok()?;

    // Check if one side is a VarRef matching the base name (no subscripts)
    // and the other side is an array-valued expression.
    let array_expr = if is_var_ref_of(&lhs_side, base_name) {
        &rhs_side
    } else if is_var_ref_of(&rhs_side, base_name) {
        &lhs_side
    } else {
        return None;
    };

    render_array_expr_at_index(array_expr, index, cfg)
}

fn render_array_expr_at_index(expr: &Value, index: usize, cfg: &ExprConfig) -> Option<String> {
    if let Ok(array) = get_field(expr, "Array") {
        let elements = get_field(&array, "elements").ok()?;
        let len = elements.len()?;
        if index == 0 || index > len {
            return None;
        }
        let elem = elements.get_item(&minijinja::Value::from(index - 1)).ok()?;
        return render_expression(&elem, cfg).ok();
    }

    if let Ok(var_ref) = get_field(expr, "VarRef") {
        return render_indexed_var_ref(&var_ref, index, cfg);
    }

    if let Ok(binary) = get_field(expr, "Binary") {
        return render_binary_array_expr_at_index(&binary, index, cfg);
    }

    if let Ok(unary) = get_field(expr, "Unary") {
        let rhs = get_field(&unary, "rhs").ok()?;
        let rhs_render = render_array_expr_at_index_or_scalar(&rhs, index, cfg)?;
        let op = get_field(&unary, "op").ok()?;
        let op_str = render_expr::get_unop_string(&op, cfg).ok()?;
        return Some(format!("({op_str}{rhs_render})"));
    }

    if let Ok(if_expr) = get_field(expr, "If") {
        let branches = get_field(&if_expr, "branches").ok()?;
        let else_branch = get_field(&if_expr, "else_branch").ok()?;
        let else_render = render_array_expr_at_index_or_scalar(&else_branch, index, cfg)?;
        let Some(branch_count) = branches.len() else {
            return Some(else_render);
        };
        let mut result = else_render;
        for branch_idx in (0..branch_count).rev() {
            let branch = branches.get_item(&Value::from(branch_idx)).ok()?;
            let cond = branch.get_item(&Value::from(0)).ok()?;
            let branch_expr = branch.get_item(&Value::from(1)).ok()?;
            let cond_render = render_expression(&cond, cfg).ok()?;
            let branch_render = render_array_expr_at_index_or_scalar(&branch_expr, index, cfg)?;
            result = format!("(({cond_render}) ? ({branch_render}) : ({result}))");
        }
        return Some(result);
    }

    if let Ok(builtin) = get_field(expr, "BuiltinCall") {
        return render_builtin_array_expr_at_index(&builtin, index, cfg);
    }

    None
}

fn render_array_expr_at_index_or_scalar(
    expr: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    render_array_expr_at_index(expr, index, cfg).or_else(|| render_expression(expr, cfg).ok())
}

fn render_indexed_var_ref(var_ref: &Value, index: usize, cfg: &ExprConfig) -> Option<String> {
    let subscripts = get_field(var_ref, "subscripts").ok()?;
    if subscripts.len().unwrap_or(0) != 0 {
        return None;
    }

    let raw_name = get_field(var_ref, "name")
        .ok()
        .map(|n| {
            get_field(&n, "0")
                .map(|v| super::value_to_string(&v))
                .unwrap_or_else(|_| super::value_to_string(&n))
        })
        .unwrap_or_default();

    if cfg.subscript_underscore {
        let indexed_ref = format!("{raw_name}[{index}]");
        if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &indexed_ref) {
            return Some(symbol);
        }
        let name = super::emitted_symbol_or_fallback(&raw_name, cfg);
        Some(format!("{name}_{index}"))
    } else if cfg.one_based_index {
        let name = super::emitted_symbol_or_fallback(&raw_name, cfg);
        Some(format!("{name}[{index}]"))
    } else {
        let name = super::emitted_symbol_or_fallback(&raw_name, cfg);
        Some(format!("{}[{}]", name, index - 1))
    }
}

fn render_binary_array_expr_at_index(
    binary: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    let lhs = get_field(binary, "lhs").ok()?;
    let rhs = get_field(binary, "rhs").ok()?;
    let lhs_render = render_array_expr_at_index_or_scalar(&lhs, index, cfg)?;
    let rhs_render = render_array_expr_at_index_or_scalar(&rhs, index, cfg)?;
    let op = get_field(binary, "op").ok()?;
    if render_expr::is_mul_elem_op(&op)
        && let Some(func) = &cfg.mul_elem_fn
    {
        return Some(format!("{func}({lhs_render}, {rhs_render})"));
    }
    if render_expr::is_exp_op(&op) {
        if let Some(power_fn) = &cfg.power_fn {
            return Some(format!("{power_fn}({lhs_render}, {rhs_render})"));
        }
        if cfg
            .power
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphabetic())
        {
            return Some(format!("{}({lhs_render}, {rhs_render})", cfg.power));
        }
    }
    let op_str = render_expr::get_binop_string(&op, cfg).ok()?;
    Some(format!("({lhs_render} {op_str} {rhs_render})"))
}

fn render_builtin_array_expr_at_index(
    builtin: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    let func_name = get_field(builtin, "function").ok()?.to_string();
    let args = get_field(builtin, "args").ok()?;
    match func_name.as_str() {
        "Smooth" => {
            let inner = args.get_item(&Value::from(1)).ok()?;
            render_array_expr_at_index_or_scalar(&inner, index, cfg)
        }
        "NoEvent" | "Homotopy" | "Previous" | "Hold" | "NoClock" | "SubSample" | "SuperSample"
        | "ShiftSample" | "BackSample" => {
            let inner = args.get_item(&Value::from(0)).ok()?;
            render_array_expr_at_index_or_scalar(&inner, index, cfg)
        }
        "Pre" => {
            let inner = args.get_item(&Value::from(0)).ok()?;
            let selected = render_array_expr_at_index_or_scalar(&inner, index, cfg)?;
            Some(format!("pre({selected})"))
        }
        _ => None,
    }
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
    if let Ok(unary) = get_field(expr, "Unary") {
        let op = get_field(&unary, "op")
            .ok()
            .map(|v| v.to_string())
            .unwrap_or_default();
        if (op.contains("Minus") || op.contains("Neg"))
            && let Ok(inner) = get_field(&unary, "rhs")
        {
            flatten_value_add_terms(&inner, !positive, terms);
            return;
        }
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
fn extract_der_coefficient(expr: &Value, state_name: &str, cfg: &ExprConfig) -> Option<String> {
    let binary = get_field(expr, "Binary").ok()?;
    if !is_mul_op(&binary) {
        return None;
    }
    let lhs = get_field(&binary, "lhs").ok()?;
    let rhs = get_field(&binary, "rhs").ok()?;

    if is_der_of(&rhs, state_name) {
        // k * der(x) → coefficient is k
        return render_expression(&lhs, cfg).ok();
    }
    if is_der_of(&lhs, state_name) {
        // der(x) * k → coefficient is k
        return render_expression(&rhs, cfg).ok();
    }
    None
}

/// Check if a Binary expression's op is Mul or MulElem.
fn is_mul_op(binary: &Value) -> bool {
    if let Ok(op) = get_field(binary, "op") {
        return get_field(&op, "Mul").is_ok() || get_field(&op, "MulElem").is_ok();
    }
    false
}

/// Check if a Binary expression's op is Sub or SubElem.
fn is_sub_op(binary: &Value) -> bool {
    if let Ok(op) = get_field(binary, "op") {
        return get_field(&op, "Sub").is_ok() || get_field(&op, "SubElem").is_ok();
    }
    false
}

/// Check if a Binary expression's op is Add or AddElem.
fn is_add_op(binary: &Value) -> bool {
    if let Ok(op) = get_field(binary, "op") {
        return get_field(&op, "Add").is_ok() || get_field(&op, "AddElem").is_ok();
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
            && let Some(val) = idx.as_i64()
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
    let base_name = get_field(&name, "0")
        .map(|v| v.to_string())
        .unwrap_or_else(|_| name.to_string());
    base_name.trim_matches('"').to_string()
}

fn parse_indexed_ref(name: &str) -> Option<(String, usize)> {
    let trimmed = name.trim_matches('"');
    let (base, subscript_part) = trimmed.rsplit_once('[')?;
    let subscripts = subscript_part.strip_suffix(']')?;
    let mut parts = subscripts.split(',');
    let first = parts.next()?.trim().parse::<usize>().ok()?;
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
fn var_name_to_symbol(name: &str, cfg: &ExprConfig) -> String {
    if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), name) {
        return symbol;
    }
    if let Some((base, subscript_part)) = name.rsplit_once('[')
        && let Some(subscripts) = subscript_part.strip_suffix(']')
    {
        let suffix = subscripts.replace(',', "_").replace(' ', "");
        let base_symbol = super::emitted_symbol_or_fallback(base, cfg);
        return format!("{base_symbol}_{suffix}");
    }

    super::emitted_symbol_or_fallback(name, cfg)
}

fn synthesize_discrete_statespace_rhs(
    var_name: &str,
    dae: &Value,
    cfg: &ExprConfig,
) -> Option<String> {
    if let Some(prefix) = var_name.strip_suffix(".e") {
        let setpoint = format!("{prefix}.setpoint");
        let measurement = format!("{prefix}.measurement");
        if has_var_in_dae_map(dae, "y", &setpoint) && has_var_in_dae_map(dae, "y", &measurement) {
            return Some(format!(
                "({}) - ({})",
                var_name_to_symbol(&setpoint, cfg),
                var_name_to_symbol(&measurement, cfg)
            ));
        }
        return None;
    }

    if let Some(prefix) = var_name.strip_suffix(".u_k") {
        let x_name = format!("{prefix}.x");
        let e_name = format!("{prefix}.e");
        let c_name = format!("{prefix}.C_d");
        let d_name = format!("{prefix}.D_d");
        let e_expr = current_error_expr(prefix, dae, cfg);
        let n = get_first_dim_for_var_in_dae(dae, &x_name)?;
        if !has_var_in_dae_map(dae, "z", &x_name)
            || !has_var_in_dae_map(dae, "z", &e_name)
            || !has_var_in_dae_map(dae, "p", &c_name)
            || !has_var_in_dae_map(dae, "p", &d_name)
        {
            return None;
        }

        let mut terms = Vec::new();
        for j in 1..=n {
            terms.push(format!(
                "({} * pre_{})",
                indexed_alias(&c_name, j, cfg),
                indexed_alias(&x_name, j, cfg)
            ));
        }
        terms.push(format!(
            "({} * {})",
            var_name_to_symbol(&d_name, cfg),
            e_expr
        ));
        return Some(terms.join(" + "));
    }

    if let Some((prefix, i)) = parse_indexed_suffix(var_name, ".x") {
        let x_name = format!("{prefix}.x");
        let e_name = format!("{prefix}.e");
        let a_name = format!("{prefix}.A_d");
        let b_name = format!("{prefix}.B_d");
        let e_expr = current_error_expr(prefix, dae, cfg);
        let n = get_first_dim_for_var_in_dae(dae, &x_name)?;
        if i < 1 || i > n {
            return None;
        }
        if !has_var_in_dae_map(dae, "z", &x_name)
            || !has_var_in_dae_map(dae, "z", &e_name)
            || !has_var_in_dae_map(dae, "p", &a_name)
            || !has_var_in_dae_map(dae, "p", &b_name)
        {
            return None;
        }

        let mut terms = Vec::new();
        for j in 1..=n {
            let flat_idx = (i - 1) * n + j;
            terms.push(format!(
                "({} * pre_{})",
                indexed_alias(&a_name, flat_idx, cfg),
                indexed_alias(&x_name, j, cfg)
            ));
        }
        terms.push(format!("({} * {})", indexed_alias(&b_name, i, cfg), e_expr));
        return Some(terms.join(" + "));
    }

    None
}

fn parse_indexed_suffix<'a>(name: &'a str, suffix: &str) -> Option<(&'a str, usize)> {
    let marker = format!("{suffix}[");
    let pos = name.find(&marker)?;
    let prefix = &name[..pos];
    let idx_str = name[pos + marker.len()..].strip_suffix(']')?;
    let idx = idx_str.parse::<usize>().ok()?;
    Some((prefix, idx))
}

fn indexed_alias(base_name: &str, idx: usize, cfg: &ExprConfig) -> String {
    var_name_to_symbol(&format!("{base_name}[{idx}]"), cfg)
}

fn has_var_in_dae_map(dae: &Value, map_name: &str, var_name: &str) -> bool {
    let Ok(map) = dae.get_attr(map_name) else {
        return false;
    };
    map.get_item(&Value::from(var_name)).is_ok()
}

fn get_first_dim_for_var_in_dae(dae: &Value, var_name: &str) -> Option<usize> {
    for map_name in ["z", "m", "x"] {
        let Ok(map) = dae.get_attr(map_name) else {
            continue;
        };
        let Ok(var) = map.get_item(&Value::from(var_name)) else {
            continue;
        };
        let Ok(dims) = get_field(&var, "dims") else {
            return None;
        };
        let len = dims.len()?;
        if len == 0 {
            return None;
        }
        let first = dims.get_item(&Value::from(0)).ok()?;
        if let Some(v) = first.as_usize() {
            return Some(v);
        }
        if let Some(v) = first.as_i64() {
            return usize::try_from(v).ok();
        }
    }
    None
}

fn current_error_expr(prefix: &str, dae: &Value, cfg: &ExprConfig) -> String {
    let setpoint = format!("{prefix}.setpoint");
    let measurement = format!("{prefix}.measurement");
    if has_var_in_dae_map(dae, "y", &setpoint) && has_var_in_dae_map(dae, "y", &measurement) {
        return format!(
            "({} - {})",
            var_name_to_symbol(&setpoint, cfg),
            var_name_to_symbol(&measurement, cfg)
        );
    }
    var_name_to_symbol(&format!("{prefix}.e"), cfg)
}
