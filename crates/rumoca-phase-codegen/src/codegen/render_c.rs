//! C-backend template functions for FMI2 and embedded-C code generation.
//!
//! These functions are registered in the minijinja environment and used by
//! `fmi2/model.c.jinja` and `embedded_c/model.c.jinja` templates to extract explicit
//! ODE/algebraic RHS expressions from residual-form DAE equations.
//!
//! SPEC_0021 file-size exception: the C target renderer still hosts binding,
//! RHS extraction, discrete-state, and runtime-helper rendering while the split plan
//! lands for neutral modules such as `render_c/bindings.rs`,
//! `render_c/rhs.rs`, `render_c/discrete.rs`, and `render_c/helpers.rs`.

// Keep the template surface explicit and test-backed while the renderer split lands.
#![allow(
    clippy::collapsible_if,
    clippy::excessive_nesting,
    clippy::manual_pattern_char_comparison,
    clippy::needless_borrow,
    clippy::too_many_lines
)]

use super::{ExprConfig, RenderResult};
use minijinja::Value;
use render_expr::{
    get_field, is_variant, render_expression, render_serialized_name, subscript_index_value,
};
use rumoca_core::{parent_scope, top_level_last_segment};
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::hash::{Hash, Hasher};

use super::render_expr;

type LinearDerivativeRow = (BTreeMap<usize, String>, String);
type MaybeLinearDerivativeRow = Result<Option<LinearDerivativeRow>, minijinja::Error>;

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
    equation_residual_or_rhs(eq)?.ok_or_else(|| {
        minijinja::Error::new(
            minijinja::ErrorKind::InvalidOperation,
            format!("{context} missing required `rhs`/`residual` field"),
        )
    })
}

thread_local! {
    static ALG_EQUATION_CANDIDATE_INDEX: RefCell<Option<AlgEquationCandidateIndex>> = const { RefCell::new(None) };
    static EQUATION_ALIAS_INDEX: RefCell<Option<EquationAliasIndex>> = const { RefCell::new(None) };
    static DAE_ALIAS_INDEX: RefCell<Option<EquationAliasIndex>> = const { RefCell::new(None) };
}

#[derive(Clone)]
struct AlgEquationCandidateIndex {
    fingerprint: u64,
    by_var_name: HashMap<String, Vec<Value>>,
}

#[derive(Clone)]
struct EquationAliasIndex {
    fingerprint: u64,
    aliases: HashSet<String>,
}

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

/// Render a parameter binding RHS only when the generated C expression is safe
/// for initialization-time propagation. Returns an empty string for string
/// parameters, dynamically indexed expressions, or unsupported C snippets.
pub(super) fn parameter_binding_rhs_function(
    target_name: Value,
    expr: Value,
    index: Value,
    config: Value,
) -> RenderResult {
    let target = target_name
        .as_str()
        .map(str::to_string)
        .unwrap_or_else(|| target_name.to_string().trim_matches('"').to_string());
    let idx = index.as_usize().unwrap_or(0);
    if let Some(rhs) = synthesize_buildings_actuator_filter_parameter_rhs(&target) {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_buildings_limit_slew_rate_parameter_rhs(&target) {
        return Ok(rhs);
    }
    if !is_supported_parameter_binding_target(&target) {
        return Ok(String::new());
    }
    if is_string_literal_function(expr.clone()) == "yes" || expr_has_dynamic_multidim_index(&expr) {
        return Ok(String::new());
    }

    let rendered = if idx > 0 {
        render_expr_at_index_function(expr, Value::from(idx), config)?
    } else {
        let cfg = ExprConfig::from_value(&config);
        render_expression(&expr, &cfg)?
    };
    if c_rhs_is_supported(&rendered) {
        Ok(rendered)
    } else {
        Ok(String::new())
    }
}

fn is_supported_parameter_binding_target(name: &str) -> bool {
    name.contains(".VolFloCur")
        || name.contains(".PreCur")
        || name.contains(".HydEff")
        || name.contains(".MotEff")
        || name.contains(".P_nominal")
        || name.contains(".PFan_nominal")
        || name.contains(".fraPFan_nominal")
        || name.contains(".mCW_flow_nominal")
        || name.contains(".QEva_flow_nominal")
        || name.contains(".COP_nominal")
        || name.contains(".per.pressure.")
        || name.contains(".per.motorEfficiency.")
        || name.contains(".per.hydraulicEfficiency.")
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

fn c_ode_rhs_is_supported(rhs: &str) -> bool {
    c_rhs_is_supported(rhs) || rhs.contains("__rumoca_solve_linear_component(")
}

fn c_rhs_is_supported(rhs: &str) -> bool {
    c_rhs_is_supported_with_options(rhs, false)
}

fn c_switch_rhs_is_supported(rhs: &str) -> bool {
    c_rhs_is_supported_with_options(rhs, true)
}

fn c_rhs_is_supported_with_options(rhs: &str, allow_ternary_colon: bool) -> bool {
    if rhs.trim().is_empty() {
        return false;
    }
    let compact = rhs.replace(char::is_whitespace, "");
    let compact_for_colon_check =
        if allow_ternary_colon && rhs.contains(" ? ") && rhs.contains(" : ") {
            compact.replace(':', "")
        } else {
            compact.clone()
        };
    !(rhs.contains("inStream(")
        || rhs.contains("actualStream(")
        || rhs.contains("cat(")
        || rhs.contains("Buildings_Controls_OBC_CDL_")
        || rhs.contains("Buildings_ThermalZones_EnergyPlus_9_6_0_BaseClasses_SpawnExternalObject")
        || rhs.contains("cha_fan(")
        || rhs.contains("Modelica_Units_SI_Pressure(")
        || rhs.contains("Modelica_Media_Interfaces_PartialSimpleMedium_setState_")
        || rhs.contains("Modelica_Media_Interfaces_PartialMedium_setState_")
        || rhs.contains("Modelica_Media_Interfaces_PartialMedium_temperature_phX")
        || rhs.contains("Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX")
        || rhs.contains("__rumoca_named_arg___X(")
        || rhs.contains("__rumoca_named_arg___u(")
        || rhs.contains("_ramLim_u")
        || rhs.contains("_outPort_0_")
        || rhs.contains("_inPort_0_")
        || rhs.contains("_Xi)")
        || rhs.contains("_Xi;")
        || rhs.contains("_s *")
        || rhs.contains("* floor")
        || rhs.contains("[((floor")
        || rhs.contains("_filter_s[((")
        || rhs.contains("_filter_n")
        || rhs.contains("_gain_")
        || rhs.contains("ele___")
        || rhs.contains("(double[]){")
        || rhs.contains(" for i in ")
        || rhs.contains("_extract_")
        || compact.contains("1:0")
        || compact_for_colon_check.contains(":"))
}

fn is_logical_switch_output_var(var_name: &str) -> bool {
    var_name.ends_with(".swi.y") || var_name.ends_with("_swi_y")
}

fn c_event_or_discrete_rhs_is_supported(rhs: &str) -> bool {
    c_rhs_is_supported(rhs)
}

fn c_rhs_with_equation_context(rhs: String, equations: &Value) -> String {
    if !rhs.contains("__rumoca_sum_d(") {
        return rhs;
    }
    let aliases = c_aliases_for_equations(equations);
    expand_sum_calls_for_scalarized_aliases(&rhs, &aliases)
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
    let text = expr.to_string();
    if text.contains("String") || text.contains("\\\"") {
        return "yes".to_string();
    }
    String::new()
}

/// Check if an expression contains any variable reference.
///
/// Returns "yes" when a runtime start/binding expression depends on another
/// variable and must be re-evaluated after FMI parameter changes.
pub(super) fn expr_has_var_ref_function(expr: Value) -> String {
    if expr_has_var_ref(&expr) {
        return "yes".to_string();
    }
    String::new()
}

/// Check whether a C initializer/binding expression contains an alias access
/// that cannot be rendered as a C scalar expression.
pub(super) fn expr_has_dynamic_multidim_index_function(expr: Value) -> String {
    if expr_has_dynamic_multidim_index(&expr) {
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
            get_field(&param, "type_class")
                .map(|type_class| type_class.to_string().trim_matches('"') == "Record")
                .unwrap_or(false)
        })
    {
        return "yes".to_string();
    }
    String::new()
}

/// Find an explicit RHS for an initialization equation targeting `var_name`.
pub(super) fn initial_rhs_for_var_function(
    dae: Value,
    var_name: Value,
    config: Value,
) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    let name = var_name
        .as_str()
        .map(str::to_string)
        .unwrap_or_else(|| var_name.to_string().trim_matches('"').to_string());
    let Ok(initial_equations) = get_field(&dae, "initial_equations") else {
        return Ok(String::new());
    };
    let Some(len) = initial_equations.len() else {
        return Ok(String::new());
    };
    for i in 0..len {
        let Ok(eq) = initial_equations.get_item(&Value::from(i)) else {
            continue;
        };
        if let Ok(Some(rhs)) = find_algebraic_rhs(&eq, &name, &cfg) {
            let rhs = c_rhs_with_equation_context(rhs, &initial_equations);
            if c_rhs_is_supported(&rhs) {
                return Ok(rhs);
            }
        }
    }
    for suffix in [".localActive", ".newActive", ".oldActive"] {
        if let Some(prefix) = name.strip_suffix(suffix) {
            let active_name = format!("{prefix}.active");
            for i in 0..len {
                let Ok(eq) = initial_equations.get_item(&Value::from(i)) else {
                    continue;
                };
                if let Ok(Some(rhs)) = find_algebraic_rhs(&eq, &active_name, &cfg) {
                    let rhs = c_rhs_with_equation_context(rhs, &initial_equations);
                    if c_rhs_is_supported(&rhs) {
                        return Ok(rhs);
                    }
                }
            }
        }
    }
    Ok(String::new())
}

fn expr_has_var_ref(expr: &Value) -> bool {
    if get_field(expr, "VarRef").is_ok() {
        return true;
    }
    if let Ok(binary) = get_field(expr, "Binary") {
        return get_field(&binary, "lhs").is_ok_and(|lhs| expr_has_var_ref(&lhs))
            || get_field(&binary, "rhs").is_ok_and(|rhs| expr_has_var_ref(&rhs));
    }
    if let Ok(unary) = get_field(expr, "Unary") {
        return get_field(&unary, "rhs").is_ok_and(|rhs| expr_has_var_ref(&rhs));
    }
    if let Ok(call) = get_field(expr, "BuiltinCall").or_else(|_| get_field(expr, "FunctionCall"))
        && let Ok(args) = get_field(&call, "args")
    {
        return list_any(&args, |arg| expr_has_var_ref(&arg));
    }
    if let Ok(if_expr) = get_field(expr, "If") {
        let branch_refs = get_field(&if_expr, "branches")
            .map(|branches| list_any(&branches, |branch| expr_has_var_ref(&branch)))
            .unwrap_or(false);
        let else_refs = get_field(&if_expr, "else_branch")
            .map(|else_branch| expr_has_var_ref(&else_branch))
            .unwrap_or(false);
        return branch_refs || else_refs;
    }
    if let Ok(array) = get_field(expr, "Array").or_else(|_| get_field(expr, "Tuple"))
        && let Ok(elements) = get_field(&array, "elements")
    {
        return list_any(&elements, |element| expr_has_var_ref(&element));
    }
    if let Ok(range) = get_field(expr, "Range") {
        return get_field(&range, "start").is_ok_and(|start| expr_has_var_ref(&start))
            || get_field(&range, "step").is_ok_and(|step| expr_has_var_ref(&step))
            || get_field(&range, "end").is_ok_and(|end| expr_has_var_ref(&end));
    }
    if let Ok(index) = get_field(expr, "Index") {
        let base_refs = get_field(&index, "base")
            .map(|base| expr_has_var_ref(&base))
            .unwrap_or(false);
        let sub_refs = get_field(&index, "subscripts")
            .map(|subs| list_any(&subs, |sub| expr_has_var_ref(&sub)))
            .unwrap_or(false);
        return base_refs || sub_refs;
    }
    if let Ok(field_access) = get_field(expr, "FieldAccess") {
        return get_field(&field_access, "base")
            .map(|base| expr_has_var_ref(&base))
            .unwrap_or(false);
    }
    false
}

fn expr_has_dynamic_multidim_index(expr: &Value) -> bool {
    if let Ok(literal) = get_field(expr, "Literal")
        && get_field(&literal, "String").is_ok()
    {
        return true;
    }
    if let Ok(var_ref) = get_field(expr, "VarRef")
        && let Ok(subs) = get_field(&var_ref, "subscripts")
        && list_any(&subs, |sub| is_colon_subscript(&sub))
    {
        return true;
    }
    if let Ok(var_ref) = get_field(expr, "VarRef")
        && let Ok(subs) = get_field(&var_ref, "subscripts")
        && subs.len().unwrap_or(0) > 1
    {
        return true;
    }
    if let Ok(binary) = get_field(expr, "Binary") {
        return get_field(&binary, "lhs").is_ok_and(|lhs| expr_has_dynamic_multidim_index(&lhs))
            || get_field(&binary, "rhs").is_ok_and(|rhs| expr_has_dynamic_multidim_index(&rhs));
    }
    if let Ok(unary) = get_field(expr, "Unary") {
        return get_field(&unary, "rhs").is_ok_and(|rhs| expr_has_dynamic_multidim_index(&rhs));
    }
    if let Ok(call) = get_field(expr, "BuiltinCall").or_else(|_| get_field(expr, "FunctionCall"))
        && let Ok(args) = get_field(&call, "args")
    {
        return list_any(&args, |arg| expr_has_dynamic_multidim_index(&arg));
    }
    if let Ok(if_expr) = get_field(expr, "If") {
        let branch_refs = get_field(&if_expr, "branches")
            .map(|branches| list_any(&branches, |branch| expr_has_dynamic_multidim_index(&branch)))
            .unwrap_or(false);
        let else_refs = get_field(&if_expr, "else_branch")
            .map(|else_branch| expr_has_dynamic_multidim_index(&else_branch))
            .unwrap_or(false);
        return branch_refs || else_refs;
    }
    if let Ok(array) = get_field(expr, "Array").or_else(|_| get_field(expr, "Tuple"))
        && let Ok(elements) = get_field(&array, "elements")
    {
        return list_any(&elements, |element| {
            expr_has_dynamic_multidim_index(&element)
        });
    }
    if get_field(expr, "ArrayComprehension").is_ok() {
        return true;
    }
    if get_field(expr, "Range").is_ok() {
        return true;
    }
    if let Ok(index) = get_field(expr, "Index") {
        let base_refs = get_field(&index, "base")
            .map(|base| expr_has_dynamic_multidim_index(&base))
            .unwrap_or(false);
        let sub_refs = get_field(&index, "subscripts")
            .map(|subs| list_any(&subs, |sub| expr_has_dynamic_multidim_index(&sub)))
            .unwrap_or(false);
        return base_refs || sub_refs;
    }
    if get_field(expr, "FieldAccess").is_ok() {
        return true;
    }
    false
}

fn is_colon_subscript(sub: &Value) -> bool {
    sub.as_str() == Some("Colon")
        || sub.to_string().trim_matches('"') == "Colon"
        || get_field(sub, "Colon").is_ok()
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
            let rhs_expr = c_rhs_with_equation_context(rhs_expr, &equations);
            if c_ode_rhs_is_supported(&rhs_expr) {
                return Ok(rhs_expr);
            }
        }
    }

    if let Some(rhs_expr) =
        find_linear_derivative_system_rhs_in_equations(&equations, &name_str, &cfg)?
    {
        let rhs_expr = c_rhs_with_equation_context(rhs_expr, &equations);
        if c_ode_rhs_is_supported(&rhs_expr) {
            return Ok(rhs_expr);
        }
    }

    if let Some(rhs_expr) = synthesize_buildings_actuator_filter_ode_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_integrator_ode_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_internal_fluid_volume_ode_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_buildings_expansion_vessel_ode_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_buildings_temperature_two_port_ode_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_buildings_dynamic_flow_sensor_ode_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_buildings_limit_slew_rate_ode_rhs(&name_str) {
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
    let aliases = c_aliases_for_equations(&equations);
    alg_rhs_for_var_with_aliases(var_name, equations, &aliases, config)
}

pub(super) fn alg_rhs_for_var_with_dae_function(
    var_name: Value,
    dae: Value,
    config: Value,
) -> RenderResult {
    let equations = dae
        .get_attr("f_x")
        .unwrap_or(Value::from(Vec::<Value>::new()));
    let Some(fingerprint) = fingerprint_dae_alias_context(&dae) else {
        let aliases = collect_c_aliases_from_dae(&dae);
        return alg_rhs_for_var_with_aliases(var_name, equations, &aliases, config);
    };
    DAE_ALIAS_INDEX.with(|cache| {
        let mut cached = cache.borrow_mut();
        if !matches!(cached.as_ref(), Some(index) if index.fingerprint == fingerprint) {
            let aliases = collect_c_aliases_from_dae(&dae);
            *cached = Some(EquationAliasIndex {
                fingerprint,
                aliases,
            });
        }
        let aliases = &cached.as_ref().expect("DAE alias cache populated").aliases;
        alg_rhs_for_var_with_aliases(var_name, equations, aliases, config)
    })
}

fn alg_rhs_for_var_with_aliases(
    var_name: Value,
    equations: Value,
    aliases: &HashSet<String>,
    config: Value,
) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    let name_str = var_name.to_string().trim_matches('"').to_string();

    if let Some(rhs_expr) = synthesize_modelica_gain_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_replicator_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_buildings_actuator_filter_alg_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_add_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_multiswitch_expr_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_fluid_absolute_pressure_sensor_stream_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_fluid_volume_flow_sensor_density_inflow_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_trace_substances_two_port_cmed_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_stategraph_transition_timer_rhs(&name_str, aliases)
    {
        return Ok(rhs_expr);
    }
    let Some(candidate_equations) = alg_equation_candidates_for_var(&equations, &name_str) else {
        if let Some(rhs_expr) = synthesize_fluid_sensor_mnor_flow_rhs(&name_str) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_fluid_sensor_k_rhs(&name_str) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_fluid_temperature_sensor_inflow_rhs(&name_str) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_fluid_absolute_pressure_sensor_stream_rhs(&name_str) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_fluid_volume_flow_sensor_density_inflow_rhs(&name_str) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_trace_substances_two_port_cmed_rhs(&name_str, aliases) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_fluid_thermodynamic_state_field_rhs(&name_str) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_moist_air_medium_field_rhs(&name_str, aliases) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_internal_fluid_volume_medium_rhs(&name_str, aliases) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_buildings_actuator_filter_alg_rhs(&name_str) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) =
            synthesize_indexed_stream_from_h_outflow_rhs(&name_str, &equations, &cfg, aliases)
        {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_fluid_stream_pass_through_rhs(&name_str, aliases) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) =
            synthesize_internal_fluid_volume_port_enthalpy_flow_rhs(&name_str, aliases)
        {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_internal_fluid_volume_species_rhs(&name_str, aliases) {
            return Ok(rhs_expr);
        }
        return Ok("0.0".to_string());
    };

    for eq in &candidate_equations {
        if let Some(rhs_expr) = find_algebraic_rhs_direct(eq, &name_str, &cfg)? {
            let rhs_expr = c_rhs_with_equation_context(rhs_expr, &equations);
            if c_rhs_is_supported(&rhs_expr) {
                return Ok(rhs_expr);
            }
        }
    }
    for eq in &candidate_equations {
        if let Some(rhs_expr) = find_algebraic_rhs(eq, &name_str, &cfg)? {
            let rhs_expr = c_rhs_with_equation_context(rhs_expr, &equations);
            if c_rhs_is_supported(&rhs_expr) {
                return Ok(rhs_expr);
            }
        }
    }

    let mut first_supported_rhs = None;
    let mut best_supported_rhs: Option<(isize, String)> = None;
    let prefer_conditional_rhs = is_logical_switch_output_var(&name_str);
    for eq in candidate_equations {
        if let Some(rhs_expr) = find_algebraic_rhs(&eq, &name_str, &cfg)? {
            let rhs_expr = c_rhs_with_equation_context(rhs_expr, &equations);
            if is_fluid_connector_flow_var(&name_str)
                && rhs_is_internal_fluid_volume_alias(&rhs_expr, &name_str)
            {
                continue;
            }
            if is_fluid_connector_stream_var(&name_str)
                && rhs_references_same_fluid_component(&rhs_expr, &name_str)
            {
                continue;
            }
            let is_conditional = rhs_expr.contains(" ? ") && rhs_expr.contains(" : ");
            if prefer_conditional_rhs && is_conditional && c_switch_rhs_is_supported(&rhs_expr) {
                return Ok(rhs_expr);
            }
            if c_rhs_is_supported(&rhs_expr) {
                if prefer_conditional_rhs && is_conditional {
                    return Ok(rhs_expr);
                }
                if first_supported_rhs.is_none() {
                    first_supported_rhs = Some(rhs_expr.clone());
                }
                let score = algebraic_rhs_candidate_score(&name_str, &rhs_expr, &eq);
                if best_supported_rhs
                    .as_ref()
                    .is_none_or(|(best_score, _)| score > *best_score)
                {
                    best_supported_rhs = Some((score, rhs_expr));
                }
            }
        }
    }
    if let Some(rhs_expr) = synthesize_pump_stage_output_rhs(&name_str, &equations) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_gain_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_replicator_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_buildings_actuator_filter_alg_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_add_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_stategraph_transition_timer_rhs(&name_str, aliases)
    {
        return Ok(rhs_expr);
    }
    if let Some((score, rhs_expr)) = best_supported_rhs {
        if score > 0
            || is_fluid_connector_pressure_var(&name_str)
            || is_fluid_connector_flow_var(&name_str)
            || is_fluid_connector_stream_var(&name_str)
        {
            return Ok(rhs_expr);
        }
    }
    if let Some(rhs_expr) = synthesize_fluid_sensor_mnor_flow_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_fluid_sensor_k_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_fluid_temperature_sensor_inflow_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_fluid_absolute_pressure_sensor_stream_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_fluid_volume_flow_sensor_density_inflow_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_fluid_thermodynamic_state_field_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_moist_air_medium_field_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_fixed_resistance_velocity_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_internal_fluid_volume_medium_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) =
        synthesize_indexed_stream_from_h_outflow_rhs(&name_str, &equations, &cfg, aliases)
    {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_fluid_stream_pass_through_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) =
        synthesize_internal_fluid_volume_port_enthalpy_flow_rhs(&name_str, aliases)
    {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_internal_fluid_volume_species_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if is_fluid_connector_flow_var(&name_str)
        && let Some(rhs_expr) =
            synthesize_internal_fluid_volume_port_flow_rhs(&name_str, &equations)
    {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_mover_density_signal_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_mover_pressure_signal_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_mover_command_signal_rhs(&name_str, &equations) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_mover_interface_signal_rhs(&name_str, &equations) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_conpi_control_signal_rhs(&name_str, &equations) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_stage_n_vector_output_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_stategraph_active_steps_rhs(&name_str, &aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_stage_n_multiswitch_rhs(&name_str, &aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = first_supported_rhs {
        return Ok(rhs_expr);
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

fn algebraic_rhs_candidate_score(var_name: &str, rhs: &str, eq: &Value) -> isize {
    let origin = equation_origin_text(eq);
    let mut score = 0isize;
    if is_fluid_connector_pressure_var(var_name) && is_connection_origin_text(&origin) {
        score += 100;
    }
    if is_fluid_connector_flow_var(var_name) && is_flow_balance_origin_text(&origin) {
        score += 100;
    }
    if is_fluid_connector_flow_var(var_name) && rhs_is_internal_fluid_volume_alias(rhs, var_name) {
        score -= 175;
    }
    if is_fluid_connector_flow_var(var_name) && rhs_references_same_fluid_component(rhs, var_name) {
        score -= 25;
    }
    if is_fluid_connector_stream_var(var_name) && is_connection_origin_text(&origin) {
        score += 100;
    }
    if is_fluid_connector_stream_var(var_name) && rhs_references_same_fluid_component(rhs, var_name)
    {
        score -= 75;
    }
    if rhs_is_mover_volume_flow_from_mass_flow(rhs, var_name) {
        score += 200;
    }
    if rhs_is_mover_volume_flow_interface_alias(rhs, var_name) {
        score -= 125;
    }
    if rhs_is_simple_self_alias(rhs, var_name) {
        score -= 50;
    }
    score
}

fn equation_origin_text(eq: &Value) -> String {
    eq.get_attr("origin").ok().map_or(String::new(), |value| {
        value.to_string().trim_matches('"').to_string()
    })
}

fn is_connection_origin_text(origin: &str) -> bool {
    origin.starts_with("connect(") || origin.starts_with("connection equation:")
}

fn is_flow_balance_origin_text(origin: &str) -> bool {
    is_connection_origin_text(origin)
        || origin.contains("flow sum")
        || origin.contains("FlowSum")
        || origin.contains("unconnected flow")
        || origin.contains("UnconnectedFlow")
}

fn is_fluid_connector_pressure_var(var_name: &str) -> bool {
    var_name.ends_with(".p") || var_name.ends_with("_p")
}

fn is_fluid_connector_flow_var(var_name: &str) -> bool {
    var_name.ends_with(".m_flow") || var_name.ends_with("_m_flow")
}

fn is_fluid_connector_stream_var(var_name: &str) -> bool {
    var_name.ends_with(".h_outflow")
        || var_name.ends_with("_h_outflow")
        || is_indexed_fluid_connector_stream_var(var_name, "Xi_outflow")
        || is_indexed_fluid_connector_stream_var(var_name, "C_outflow")
}

fn is_indexed_fluid_connector_stream_var(var_name: &str, field: &str) -> bool {
    let Some(stem) = var_name.strip_suffix(']') else {
        return false;
    };
    stem.rsplit_once(&format!(".{field}["))
        .or_else(|| stem.rsplit_once(&format!("_{field}_")))
        .is_some_and(|(_, index)| index.parse::<usize>().is_ok())
}

fn synthesize_fluid_stream_pass_through_rhs(
    var_name: &str,
    _aliases: &HashSet<String>,
) -> Option<String> {
    if !is_fluid_connector_stream_var(var_name) {
        return None;
    }
    let counterpart = fluid_stream_counterpart_var(var_name)?;
    let counterpart_alias = var_name_to_c_alias(&counterpart);
    Some(counterpart_alias)
}

fn fluid_stream_counterpart_var(var_name: &str) -> Option<String> {
    if let Some(counterpart) = indexed_fluid_stream_counterpart_var(var_name, "Xi_outflow")
        .or_else(|| indexed_fluid_stream_counterpart_var(var_name, "C_outflow"))
    {
        return Some(counterpart);
    }
    for (from, to) in [
        (".port_a.h_outflow", ".port_b.h_outflow"),
        (".port_b.h_outflow", ".port_a.h_outflow"),
        (".port_a1.h_outflow", ".port_b1.h_outflow"),
        (".port_b1.h_outflow", ".port_a1.h_outflow"),
        (".port_a2.h_outflow", ".port_b2.h_outflow"),
        (".port_b2.h_outflow", ".port_a2.h_outflow"),
        (".port_a3.h_outflow", ".port_b3.h_outflow"),
        (".port_b3.h_outflow", ".port_a3.h_outflow"),
        ("_port_a_h_outflow", "_port_b_h_outflow"),
        ("_port_b_h_outflow", "_port_a_h_outflow"),
        ("_port_a1_h_outflow", "_port_b1_h_outflow"),
        ("_port_b1_h_outflow", "_port_a1_h_outflow"),
        ("_port_a2_h_outflow", "_port_b2_h_outflow"),
        ("_port_b2_h_outflow", "_port_a2_h_outflow"),
        ("_port_a3_h_outflow", "_port_b3_h_outflow"),
        ("_port_b3_h_outflow", "_port_a3_h_outflow"),
    ] {
        if let Some(prefix) = var_name.strip_suffix(from) {
            return Some(format!("{prefix}{to}"));
        }
    }
    None
}

fn indexed_fluid_stream_counterpart_var(var_name: &str, field: &str) -> Option<String> {
    for (from, to) in [
        (".port_a.", ".port_b."),
        (".port_b.", ".port_a."),
        (".port_a1.", ".port_b1."),
        (".port_b1.", ".port_a1."),
        (".port_a2.", ".port_b2."),
        (".port_b2.", ".port_a2."),
        (".port_a3.", ".port_b3."),
        (".port_b3.", ".port_a3."),
    ] {
        let marker = format!("{from}{field}[");
        if let Some(stem) = var_name.strip_suffix(']')
            && let Some((prefix, index)) = stem.rsplit_once(&marker)
            && index.parse::<usize>().is_ok()
        {
            return Some(format!("{prefix}{to}{field}[{index}]"));
        }
    }

    for (from, to) in [
        ("_port_a_", "_port_b_"),
        ("_port_b_", "_port_a_"),
        ("_port_a1_", "_port_b1_"),
        ("_port_b1_", "_port_a1_"),
        ("_port_a2_", "_port_b2_"),
        ("_port_b2_", "_port_a2_"),
        ("_port_a3_", "_port_b3_"),
        ("_port_b3_", "_port_a3_"),
    ] {
        let marker = format!("{from}{field}_");
        if let Some((prefix, index)) = var_name.rsplit_once(&marker)
            && index.parse::<usize>().is_ok()
        {
            return Some(format!("{prefix}{to}{field}_{index}"));
        }
    }

    None
}

fn synthesize_indexed_stream_from_h_outflow_rhs(
    var_name: &str,
    equations: &Value,
    cfg: &ExprConfig,
    aliases: &HashSet<String>,
) -> Option<String> {
    let (h_var, target_suffix) = indexed_stream_h_sibling(var_name)?;
    let candidate_equations = alg_equation_candidates_for_var(equations, &h_var)?;
    let mut best_supported_rhs: Option<(isize, String)> = None;
    for eq in candidate_equations {
        let Some(rhs_expr) = find_algebraic_rhs(&eq, &h_var, cfg).ok().flatten() else {
            continue;
        };
        let rhs_expr = c_rhs_with_equation_context(rhs_expr, equations);
        if rhs_references_same_fluid_component(&rhs_expr, &h_var) || !c_rhs_is_supported(&rhs_expr)
        {
            continue;
        }
        let score = algebraic_rhs_candidate_score(&h_var, &rhs_expr, &eq);
        if best_supported_rhs
            .as_ref()
            .is_none_or(|(best_score, _)| score > *best_score)
        {
            best_supported_rhs = Some((score, rhs_expr));
        }
    }

    let (_, h_rhs) = best_supported_rhs?;
    let h_alias = rhs_single_alias(&h_rhs)?;
    let target_alias = h_alias.strip_suffix("_h_outflow")?.to_string() + &target_suffix;
    if aliases.contains(&target_alias) {
        Some(target_alias)
    } else {
        None
    }
}

fn indexed_stream_h_sibling(var_name: &str) -> Option<(String, String)> {
    if let Some(stem) = var_name.strip_suffix(']') {
        if let Some((prefix, index)) = stem.rsplit_once(".Xi_outflow[")
            && index.parse::<usize>().is_ok()
        {
            return Some((
                format!("{prefix}.h_outflow"),
                format!("_Xi_outflow_{index}"),
            ));
        }
        if let Some((prefix, index)) = stem.rsplit_once(".C_outflow[")
            && index.parse::<usize>().is_ok()
        {
            return Some((format!("{prefix}.h_outflow"), format!("_C_outflow_{index}")));
        }
    }
    if let Some((prefix, index)) = var_name.rsplit_once("_Xi_outflow_")
        && index.parse::<usize>().is_ok()
    {
        return Some((
            format!("{prefix}_h_outflow"),
            format!("_Xi_outflow_{index}"),
        ));
    }
    if let Some((prefix, index)) = var_name.rsplit_once("_C_outflow_")
        && index.parse::<usize>().is_ok()
    {
        return Some((format!("{prefix}_h_outflow"), format!("_C_outflow_{index}")));
    }
    None
}

fn rhs_is_simple_self_alias(rhs: &str, var_name: &str) -> bool {
    rhs.trim() == var_name_to_c_alias(var_name)
}

fn rhs_is_mover_volume_flow_from_mass_flow(rhs: &str, var_name: &str) -> bool {
    let Some(prefix) = var_name.strip_suffix(".eff.V_flow") else {
        return false;
    };
    let prefix_alias = var_name_to_c_alias(prefix);
    rhs.contains(&format!("{prefix_alias}_eff_m_flow"))
        && rhs.contains(&format!("{prefix_alias}_eff_rho"))
}

fn rhs_is_mover_volume_flow_interface_alias(rhs: &str, var_name: &str) -> bool {
    let Some(prefix) = var_name.strip_suffix(".eff.V_flow") else {
        return false;
    };
    rhs.trim() == format!("{}_VMachine_flow", var_name_to_c_alias(prefix))
}

fn rhs_is_internal_fluid_volume_alias(rhs: &str, var_name: &str) -> bool {
    let Some(rhs_alias) = rhs_single_alias(rhs) else {
        return false;
    };
    let var_alias = var_name_to_c_alias(var_name);
    let Some(var_norm) = normalize_internal_fluid_volume_alias(&var_alias) else {
        return false;
    };
    let Some(rhs_norm) = normalize_internal_fluid_volume_alias(&rhs_alias) else {
        return false;
    };
    var_norm == rhs_norm && var_alias != rhs_alias
}

fn rhs_single_alias(rhs: &str) -> Option<String> {
    let trimmed = rhs.trim();
    if trimmed.is_empty()
        || !trimmed
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
        || trimmed.chars().next().is_some_and(|ch| ch.is_ascii_digit())
    {
        return None;
    }
    Some(trimmed.to_string())
}

fn normalize_internal_fluid_volume_alias(alias: &str) -> Option<String> {
    if !(alias.contains("_vol_ports_") || alias.contains("_vol_dynBal_ports_")) {
        return None;
    }
    Some(alias.replace("_vol_dynBal_ports_", "_vol_ports_"))
}

fn synthesize_internal_fluid_volume_port_flow_rhs(
    var_name: &str,
    equations: &Value,
) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    let (prefix, port_index) = parse_internal_dynbal_port_flow_alias(&alias)?;
    let mb_alias = format!("{prefix}_mb_flow");
    let aliases = c_aliases_for_equations(equations);
    if !aliases.contains(&mb_alias) {
        return None;
    }

    let mut sibling_aliases = aliases
        .iter()
        .filter_map(|candidate| {
            let sibling_index = parse_internal_dynbal_port_flow_alias(candidate)
                .filter(|(candidate_prefix, _)| *candidate_prefix == prefix)
                .map(|(_, index)| index)?;
            if sibling_index == port_index {
                return None;
            }
            Some((sibling_index, candidate.clone()))
        })
        .collect::<Vec<_>>();
    sibling_aliases.sort_by_key(|(index, _)| *index);

    let mut rhs = mb_alias;
    for (_, sibling_alias) in sibling_aliases {
        rhs = format!("({rhs} - {sibling_alias})");
    }
    Some(rhs)
}

fn parse_internal_dynbal_port_flow_alias(alias: &str) -> Option<(&str, usize)> {
    let marker = "_ports_";
    let marker_pos = alias.rfind(marker)?;
    let prefix = &alias[..marker_pos];
    if !prefix.contains("_vol_dynBal") {
        return None;
    }
    let suffix = alias[marker_pos + marker.len()..].strip_suffix("_m_flow")?;
    let port_index = suffix.parse::<usize>().ok()?;
    Some((prefix, port_index))
}

fn synthesize_internal_fluid_volume_port_enthalpy_flow_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    let (prefix, port_index) = parse_internal_dynbal_port_enthalpy_flow_alias(&alias)?;
    let m_flow = format!("{prefix}_ports_{port_index}_m_flow");
    let h_outflow = format!("{prefix}_ports_{port_index}_h_outflow");
    if !aliases.is_empty() && (!aliases.contains(&m_flow) || !aliases.contains(&h_outflow)) {
        return None;
    }
    Some(format!(
        "({prefix}_ports_{port_index}_m_flow * {prefix}_ports_{port_index}_h_outflow)"
    ))
}

fn parse_internal_dynbal_port_enthalpy_flow_alias(alias: &str) -> Option<(&str, usize)> {
    let marker = "_ports_H_flow_";
    let marker_pos = alias.rfind(marker)?;
    let prefix = &alias[..marker_pos];
    if !(prefix.contains("_dynBal") || prefix.contains("_vol")) {
        return None;
    }
    let port_index = alias[marker_pos + marker.len()..].parse::<usize>().ok()?;
    Some((prefix, port_index))
}

fn synthesize_internal_fluid_volume_species_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    if let Some((prefix, port_index, xi_index)) = parse_port_xi_outflow_alias(&alias) {
        return synthesize_port_xi_outflow_alias(prefix, port_index, xi_index, aliases);
    }
    if let Some((prefix, port_index, xi_index)) = parse_ports_mxi_flow_alias(&alias) {
        let m_flow = format!("{prefix}_ports_{port_index}_m_flow");
        let xi_outflow = format!("{prefix}_ports_{port_index}_Xi_outflow_{xi_index}");
        if fluid_alias_exists(aliases, &m_flow) && fluid_alias_exists(aliases, &xi_outflow) {
            return Some(format!("({m_flow} * {xi_outflow})"));
        }
    }
    if let Some((prefix, port_index, c_index)) = parse_port_c_outflow_alias(&alias) {
        return synthesize_port_c_outflow_alias(prefix, port_index, c_index, aliases);
    }
    None
}

fn synthesize_port_xi_outflow_alias(
    prefix: &str,
    _port_index: usize,
    xi_index: usize,
    aliases: &HashSet<String>,
) -> Option<String> {
    if let Some(parent_prefix) = prefix.strip_suffix("_dynBal") {
        let dyn_medium = format!("{prefix}_medium_Xi_{xi_index}");
        if fluid_alias_exists(aliases, &dyn_medium) {
            return Some(dyn_medium);
        }
        let parent_xi = format!("{parent_prefix}_Xi_{xi_index}");
        if fluid_alias_exists(aliases, &parent_xi) {
            return Some(parent_xi);
        }
        let parent_medium = format!("{parent_prefix}_medium_Xi_{xi_index}");
        if fluid_alias_exists(aliases, &parent_medium) {
            return Some(parent_medium);
        }
    }
    let medium = format!("{prefix}_medium_Xi_{xi_index}");
    if fluid_alias_exists(aliases, &medium) {
        Some(medium)
    } else {
        let dyn_medium = format!("{prefix}_dynBal_medium_Xi_{xi_index}");
        fluid_alias_exists(aliases, &dyn_medium).then_some(dyn_medium)
    }
}

fn parse_port_xi_outflow_alias(alias: &str) -> Option<(&str, usize, usize)> {
    let (prefix, port_index, tail) = parse_alias_index_tail(alias, "_ports_")?;
    let tail = tail.trim_start_matches('_');
    let xi_index = tail.strip_prefix("Xi_outflow_")?.parse::<usize>().ok()?;
    Some((prefix, port_index, xi_index))
}

fn parse_ports_mxi_flow_alias(alias: &str) -> Option<(&str, usize, usize)> {
    let (prefix, port_index, tail) = parse_alias_index_tail(alias, "_ports_mXi_flow_")?;
    let xi_index = tail.parse::<usize>().ok()?;
    Some((prefix, port_index, xi_index))
}

fn synthesize_port_c_outflow_alias(
    prefix: &str,
    _port_index: usize,
    c_index: usize,
    aliases: &HashSet<String>,
) -> Option<String> {
    if let Some(parent_prefix) = prefix.strip_suffix("_dynBal") {
        let dyn_c = format!("{prefix}_C_{c_index}");
        if fluid_alias_exists(aliases, &dyn_c) {
            return Some(dyn_c);
        }
        let parent_c = format!("{parent_prefix}_C_{c_index}");
        if fluid_alias_exists(aliases, &parent_c) {
            return Some(parent_c);
        }
    }
    let c = format!("{prefix}_C_{c_index}");
    fluid_alias_exists(aliases, &c).then_some(c)
}

fn parse_port_c_outflow_alias(alias: &str) -> Option<(&str, usize, usize)> {
    let (prefix, port_index, tail) = parse_alias_index_tail(alias, "_ports_")?;
    let tail = tail.trim_start_matches('_');
    let c_index = tail.strip_prefix("C_outflow_")?.parse::<usize>().ok()?;
    Some((prefix, port_index, c_index))
}

fn parse_alias_index_tail<'a>(alias: &'a str, marker: &str) -> Option<(&'a str, usize, &'a str)> {
    let marker_pos = alias.rfind(marker)?;
    let prefix = &alias[..marker_pos];
    let tail = &alias[marker_pos + marker.len()..];
    let (index, rest) = tail.split_once('_')?;
    let index = index.parse::<usize>().ok()?;
    Some((prefix, index, rest))
}

fn synthesize_fluid_sensor_mnor_flow_rhs(var_name: &str) -> Option<String> {
    let prefix = var_name
        .strip_suffix(".mNor_flow")
        .or_else(|| var_name.strip_suffix("_mNor_flow"))?;
    let alias = var_name_to_c_alias(prefix);
    Some(format!(
        "({alias}_dynamic ? ((fabs({alias}_m_flow_nominal) > 1e-12) ? ({alias}_port_a_m_flow / {alias}_m_flow_nominal) : 1.0) : 1.0)"
    ))
}

fn synthesize_fluid_sensor_k_rhs(var_name: &str) -> Option<String> {
    let prefix = var_name
        .strip_suffix(".k")
        .or_else(|| var_name.strip_suffix("_k"))?;
    let alias = var_name_to_c_alias(prefix);
    if !fluid_sensor_prefix_has_sensor_name(prefix) {
        return None;
    }
    Some(format!(
        "({alias}_dynamic ? Modelica_Fluid_Utilities_regStep(__rumoca_named_arg___x({alias}_port_a_m_flow), __rumoca_named_arg___y1({alias}_mNor_flow), __rumoca_named_arg___y2(-{alias}_mNor_flow), __rumoca_named_arg___x_small({alias}_m_flow_small)) : 1.0)"
    ))
}

fn fluid_sensor_prefix_has_sensor_name(prefix: &str) -> bool {
    prefix
        .rsplit(|ch| ch == '.' || ch == '_')
        .any(|part| part.starts_with("sen") || fluid_temperature_sensor_component_name(part))
}

fn fluid_temperature_sensor_component_name(part: &str) -> bool {
    matches!(part, "TMix" | "TRet" | "TOutSen")
        || part.starts_with("TEnt")
        || part.starts_with("TLea")
        || part.starts_with("temEnt")
        || part.starts_with("temLea")
        || part.starts_with("temSen")
}

fn synthesize_fluid_absolute_pressure_sensor_stream_rhs(var_name: &str) -> Option<String> {
    let prefix = var_name
        .strip_suffix(".port.h_outflow")
        .or_else(|| var_name.strip_suffix("_port_h_outflow"))
        .or_else(|| indexed_sensor_port_stream_prefix(var_name, "Xi_outflow"))
        .or_else(|| indexed_sensor_port_stream_prefix(var_name, "C_outflow"))?;
    let component = prefix.rsplit(|ch| ch == '.' || ch == '_').next()?;
    if !fluid_absolute_pressure_sensor_component_name(component) {
        return None;
    }
    Some("0.0".to_string())
}

fn indexed_sensor_port_stream_prefix<'a>(var_name: &'a str, field: &str) -> Option<&'a str> {
    let stem = var_name.strip_suffix(']')?;
    stem.rsplit_once(&format!(".port.{field}["))
        .or_else(|| stem.rsplit_once(&format!("_port_{field}_")))
        .and_then(|(prefix, index)| index.parse::<usize>().ok().map(|_| prefix))
}

fn fluid_absolute_pressure_sensor_component_name(part: &str) -> bool {
    matches!(
        part,
        "pEnt" | "pLea" | "pWatEnt" | "pWatLea" | "pAirEnt" | "pAirLea"
    ) || part.starts_with("preWat")
        || part.starts_with("preAir")
}

fn synthesize_fluid_temperature_sensor_inflow_rhs(var_name: &str) -> Option<String> {
    let (prefix, source_port) = var_name
        .strip_suffix(".T_a_inflow")
        .map(|prefix| (prefix, "port_b"))
        .or_else(|| {
            var_name
                .strip_suffix(".T_b_inflow")
                .map(|prefix| (prefix, "port_a"))
        })?;
    if !fluid_sensor_prefix_has_sensor_name(prefix) {
        return None;
    }
    let alias = var_name_to_c_alias(prefix);
    Some(format!(
        "Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX({alias}_{source_port}_p, {alias}_{source_port}_h_outflow, {alias}_{source_port}_Xi_outflow, {alias}_{source_port}_Xi_outflow__len)"
    ))
}

fn synthesize_fluid_volume_flow_sensor_density_inflow_rhs(var_name: &str) -> Option<String> {
    let (prefix, source_port) = var_name
        .strip_suffix(".d_a_inflow")
        .map(|prefix| (prefix, "port_b"))
        .or_else(|| {
            var_name
                .strip_suffix(".d_b_inflow")
                .map(|prefix| (prefix, "port_a"))
        })
        .or_else(|| {
            var_name
                .strip_suffix(".rho_a_inflow")
                .map(|prefix| (prefix, "port_b"))
        })
        .or_else(|| {
            var_name
                .strip_suffix(".rho_b_inflow")
                .map(|prefix| (prefix, "port_a"))
        })?;
    if !fluid_volume_flow_sensor_prefix_has_name(prefix) {
        return None;
    }
    let alias = var_name_to_c_alias(prefix);
    let temperature = format!(
        "Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX({alias}_{source_port}_p, {alias}_{source_port}_h_outflow, {alias}_{source_port}_Xi_outflow, {alias}_{source_port}_Xi_outflow__len)"
    );
    Some(format!(
        "__rumoca_media_density_pTX({alias}_{source_port}_p, {temperature}, {alias}_{source_port}_Xi_outflow, {alias}_{source_port}_Xi_outflow__len)"
    ))
}

fn fluid_volume_flow_sensor_prefix_has_name(prefix: &str) -> bool {
    prefix
        .rsplit(|ch| ch == '.' || ch == '_')
        .any(|part| part.starts_with("senVolFlo"))
        || prefix.ends_with(".V_flowLea")
        || prefix.ends_with("_V_flowLea")
}

fn synthesize_trace_substances_two_port_cmed_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let sensor_prefix = var_name.strip_suffix(".CMed")?;
    let sensor_name = top_level_last_segment(sensor_prefix);
    if !trace_substances_two_port_sensor_component_name(sensor_name) {
        return None;
    }
    let alias = var_name_to_c_alias(sensor_prefix);
    let required = [
        format!("{alias}_port_a_m_flow"),
        format!("{alias}_port_a_C_outflow_1"),
        format!("{alias}_port_b_C_outflow_1"),
        format!("{alias}_s_1"),
        format!("{alias}_m_flow_small"),
    ];
    if required
        .iter()
        .any(|candidate| !fluid_alias_exists(aliases, candidate))
    {
        return None;
    }
    Some(format!(
        "Modelica_Fluid_Utilities_regStep(__rumoca_named_arg___x({alias}_port_a_m_flow), __rumoca_named_arg___y1(({alias}_s_1 * {alias}_port_b_C_outflow_1)), __rumoca_named_arg___y2(({alias}_s_1 * {alias}_port_a_C_outflow_1)), __rumoca_named_arg___x_small({alias}_m_flow_small))"
    ))
}

fn trace_substances_two_port_sensor_component_name(part: &str) -> bool {
    part.starts_with("senCO2") || part.starts_with("senTraSub")
}

fn synthesize_fluid_thermodynamic_state_field_rhs(var_name: &str) -> Option<String> {
    let (state_prefix, field) = var_name
        .strip_suffix(".p")
        .map(|prefix| (prefix, "p"))
        .or_else(|| var_name.strip_suffix(".T").map(|prefix| (prefix, "T")))
        .or_else(|| {
            let (prefix, index) = parse_indexed_suffix(var_name, ".X")?;
            Some((
                prefix,
                match index {
                    1 => "X1",
                    2 => "X2",
                    _ => return None,
                },
            ))
        })?;
    let port_name = thermodynamic_state_source_port_name(state_prefix)?;
    let component_prefix = thermodynamic_state_component_prefix(state_prefix)?;
    let alias = var_name_to_c_alias(component_prefix);

    match field {
        "p" => Some(format!("{alias}_{port_name}_p")),
        "T" => Some(format!(
            "Modelica_Media_Interfaces_PartialSimpleMedium_temperature_phX({alias}_{port_name}_p, {alias}_{port_name}_h_outflow, {alias}_{port_name}_Xi_outflow, {alias}_{port_name}_Xi_outflow__len)"
        )),
        "X1" => Some(format!("{alias}_{port_name}_Xi_outflow_1")),
        "X2" => Some(format!("(1.0 - {alias}_{port_name}_Xi_outflow_1)")),
        _ => None,
    }
}

fn synthesize_internal_fluid_volume_medium_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    if let Some(prefix) = var_name.strip_suffix(".p")
        && internal_fluid_volume_prefix(prefix)
    {
        let alias = var_name_to_c_alias(prefix);
        return Some(format!("{alias}_ports_1_p"));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".C")
        && internal_fluid_volume_prefix(prefix)
    {
        return synthesize_internal_fluid_volume_c_rhs(prefix, index, aliases);
    }
    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".Xi")
        && internal_fluid_volume_prefix(prefix)
    {
        return synthesize_internal_fluid_volume_xi_output_rhs(prefix, index, aliases);
    }
    if let Some((dynbal_prefix, index)) = parse_indexed_suffix(var_name, ".C")
        && internal_fluid_dynbal_prefix(dynbal_prefix)
    {
        return synthesize_internal_fluid_dynbal_c_rhs(dynbal_prefix, index, aliases);
    }
    if let Some((dynbal_prefix, index)) = parse_indexed_suffix(var_name, ".mbXi_flow")
        && internal_fluid_dynbal_prefix(dynbal_prefix)
    {
        return synthesize_internal_fluid_dynbal_flow_sum_rhs(
            dynbal_prefix,
            index,
            aliases,
            "_ports_mXi_flow_",
        );
    }
    if let Some((dynbal_prefix, index)) = parse_indexed_suffix(var_name, ".mbC_flow")
        && internal_fluid_dynbal_prefix(dynbal_prefix)
    {
        return synthesize_internal_fluid_dynbal_flow_sum_rhs(
            dynbal_prefix,
            index,
            aliases,
            "_ports_mC_flow_",
        );
    }
    if let Some((dynbal_prefix, index)) = parse_indexed_suffix(var_name, ".C_flow_internal")
        && internal_fluid_dynbal_prefix(dynbal_prefix)
    {
        return synthesize_internal_fluid_dynbal_c_flow_internal_rhs(dynbal_prefix, index, aliases);
    }
    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".mbXi_flow")
        && internal_fluid_volume_prefix(prefix)
    {
        return synthesize_internal_fluid_dynbal_flow_sum_rhs(
            prefix,
            index,
            aliases,
            "_ports_mXi_flow_",
        );
    }
    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".mbC_flow")
        && internal_fluid_volume_prefix(prefix)
    {
        return synthesize_internal_fluid_dynbal_flow_sum_rhs(
            prefix,
            index,
            aliases,
            "_ports_mC_flow_",
        );
    }
    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".C_flow_internal")
        && internal_fluid_volume_prefix(prefix)
    {
        return synthesize_internal_fluid_dynbal_c_flow_internal_rhs(prefix, index, aliases);
    }
    if let Some((dynbal_prefix, index)) = parse_indexed_suffix(var_name, ".mXi")
        && internal_fluid_dynbal_prefix(dynbal_prefix)
    {
        let alias = var_name_to_c_alias(dynbal_prefix);
        let m = format!("{alias}_m");
        let xi = format!("{alias}_medium_Xi_{index}");
        if fluid_alias_exists(aliases, &m) && fluid_alias_exists(aliases, &xi) {
            return Some(format!("({m} * {xi})"));
        }
    }
    if let Some((dynbal_prefix, index)) = parse_indexed_suffix(var_name, ".mC")
        && internal_fluid_dynbal_prefix(dynbal_prefix)
    {
        let alias = var_name_to_c_alias(dynbal_prefix);
        let m = format!("{alias}_m");
        let c = format!("{alias}_C_{index}");
        if fluid_alias_exists(aliases, &m) && fluid_alias_exists(aliases, &c) {
            return Some(format!("({m} * {c})"));
        }
    }
    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".mXi")
        && internal_fluid_volume_prefix(prefix)
    {
        let alias = var_name_to_c_alias(prefix);
        let dyn_mxi = format!("{alias}_dynBal_mXiOut_{index}");
        let dyn_mxi_direct = format!("{alias}_dynBal_mXi_{index}");
        if fluid_alias_exists(aliases, &dyn_mxi) {
            return Some(dyn_mxi);
        }
        if fluid_alias_exists(aliases, &dyn_mxi_direct) {
            return Some(dyn_mxi_direct);
        }
        let m = format!("{alias}_m");
        let xi = format!("{alias}_medium_Xi_{index}");
        if fluid_alias_exists(aliases, &m) && fluid_alias_exists(aliases, &xi) {
            return Some(format!("({m} * {xi})"));
        }
    }
    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".mC")
        && internal_fluid_volume_prefix(prefix)
    {
        let alias = var_name_to_c_alias(prefix);
        let dyn_mc = format!("{alias}_dynBal_mCOut_{index}");
        let dyn_mc_direct = format!("{alias}_dynBal_mC_{index}");
        if fluid_alias_exists(aliases, &dyn_mc) {
            return Some(dyn_mc);
        }
        if fluid_alias_exists(aliases, &dyn_mc_direct) {
            return Some(dyn_mc_direct);
        }
    }
    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".COut")
        && internal_fluid_volume_prefix(prefix)
    {
        return synthesize_internal_fluid_volume_c_output_rhs(prefix, index, aliases);
    }
    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".COut_internal")
        && internal_fluid_volume_prefix(prefix)
    {
        return synthesize_internal_fluid_volume_c_output_rhs(prefix, index, aliases);
    }
    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".mCOut")
        && internal_fluid_volume_prefix(prefix)
    {
        let alias = var_name_to_c_alias(prefix);
        return Some(format!("{alias}_mC_{index}"));
    }
    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".XiOut")
        && internal_fluid_volume_prefix(prefix)
    {
        return synthesize_internal_fluid_volume_xi_output_rhs(prefix, index, aliases);
    }
    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".XiOut_internal")
        && internal_fluid_volume_prefix(prefix)
    {
        return synthesize_internal_fluid_volume_xi_output_rhs(prefix, index, aliases);
    }
    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".mXiOut")
        && internal_fluid_volume_prefix(prefix)
    {
        let alias = var_name_to_c_alias(prefix);
        return Some(format!("{alias}_mXi_{index}"));
    }
    if let Some((dynbal_prefix, index)) = parse_indexed_suffix(var_name, ".COut")
        && let Some(parent_prefix) = dynbal_prefix.strip_suffix(".dynBal")
        && internal_fluid_volume_prefix(parent_prefix)
    {
        return synthesize_internal_fluid_volume_c_output_rhs(dynbal_prefix, index, aliases);
    }
    if let Some((dynbal_prefix, index)) = parse_indexed_suffix(var_name, ".mCOut")
        && let Some(parent_prefix) = dynbal_prefix.strip_suffix(".dynBal")
        && internal_fluid_volume_prefix(parent_prefix)
    {
        let parent_alias = var_name_to_c_alias(parent_prefix);
        return Some(format!("{parent_alias}_mC_{index}"));
    }
    if let Some((dynbal_prefix, index)) = parse_indexed_suffix(var_name, ".XiOut")
        && let Some(parent_prefix) = dynbal_prefix.strip_suffix(".dynBal")
        && internal_fluid_volume_prefix(parent_prefix)
    {
        return synthesize_internal_fluid_volume_xi_output_rhs(dynbal_prefix, index, aliases);
    }
    if let Some((dynbal_prefix, index)) = parse_indexed_suffix(var_name, ".mXiOut")
        && let Some(parent_prefix) = dynbal_prefix.strip_suffix(".dynBal")
        && internal_fluid_volume_prefix(parent_prefix)
    {
        let parent_alias = var_name_to_c_alias(parent_prefix);
        return Some(format!("{parent_alias}_mXi_{index}"));
    }
    None
}

fn synthesize_moist_air_medium_field_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let (prefix, field) = var_name
        .strip_suffix(".R_s")
        .map(|prefix| (prefix, "R_s"))
        .or_else(|| var_name.strip_suffix(".d").map(|prefix| (prefix, "d")))?;
    let alias = var_name_to_c_alias(prefix);
    if !aliases.contains(&format!("{alias}_p"))
        || !aliases.contains(&format!("{alias}_X_1"))
        || !aliases.contains(&format!("{alias}_X_2"))
        || !(aliases.contains(&format!("{alias}_T")) || aliases.contains(&format!("{alias}_dT")))
    {
        return None;
    }

    let gas_constant = format!("(287.05 * {alias}_X_2 + 461.52 * {alias}_X_1)");
    match field {
        "R_s" => Some(gas_constant),
        "d" => {
            let temperature = if aliases.contains(&format!("{alias}_dT")) {
                format!("({alias}_dT + 273.15)")
            } else {
                format!("{alias}_T")
            };
            Some(format!(
                "(({gas_constant} > 1e-9 && {temperature} > 1e-9) ? ({alias}_p / ({gas_constant} * {temperature})) : 1.2)"
            ))
        }
        _ => None,
    }
}

fn synthesize_fixed_resistance_velocity_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let prefix = var_name.strip_suffix(".v")?;
    let alias = var_name_to_c_alias(prefix);
    let m_flow = format!("{alias}_m_flow");
    let rho_default = format!("{alias}_rho_default");
    let area = format!("{alias}_ARound");
    if !aliases.contains(&m_flow) || !aliases.contains(&rho_default) || !aliases.contains(&area) {
        return None;
    }
    Some(format!(
        "((fabs({rho_default} * {area}) > 1e-12) ? ({m_flow} / ({rho_default} * {area})) : 0.0)"
    ))
}

fn synthesize_internal_fluid_volume_c_rhs(
    prefix: &str,
    index: usize,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(prefix);
    let dyn_c = format!("{alias}_dynBal_C_{index}");
    if fluid_alias_exists(aliases, &dyn_c) {
        return Some(dyn_c);
    }
    let m = format!("{alias}_m");
    let m_c = format!("{alias}_mC_{index}");
    Some(format!("((fabs({m}) > 1e-12) ? ({m_c} / {m}) : 0.0)"))
}

fn synthesize_internal_fluid_dynbal_c_rhs(
    prefix: &str,
    index: usize,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(prefix);
    let m = format!("{alias}_m");
    let m_c = format!("{alias}_mC_{index}");
    if !fluid_alias_exists(aliases, &m) || !fluid_alias_exists(aliases, &m_c) {
        return None;
    }
    Some(format!("((fabs({m}) > 1e-12) ? ({m_c} / {m}) : 0.0)"))
}

fn synthesize_internal_fluid_dynbal_flow_sum_rhs(
    prefix: &str,
    index: usize,
    aliases: &HashSet<String>,
    marker: &str,
) -> Option<String> {
    let alias = var_name_to_c_alias(prefix);
    let mut terms = Vec::new();
    for port_index in 1..=32 {
        let indexed = format!("{alias}{marker}{port_index}_{index}");
        if aliases.contains(&indexed) {
            terms.push(indexed);
            continue;
        }
        let scalar = format!("{alias}{marker}{port_index}");
        if index == 1 && aliases.contains(&scalar) {
            terms.push(scalar);
        }
    }
    if terms.is_empty() {
        None
    } else {
        Some(format!("({})", terms.join(" + ")))
    }
}

fn synthesize_internal_fluid_dynbal_c_flow_internal_rhs(
    prefix: &str,
    index: usize,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(prefix);
    let c_flow = format!("{alias}_C_flow_{index}");
    if fluid_alias_exists(aliases, &c_flow) {
        return Some(c_flow);
    }
    let c_flow_scalar = format!("{alias}_C_flow");
    if index == 1 && fluid_alias_exists(aliases, &c_flow_scalar) {
        return Some(c_flow_scalar);
    }
    Some("0.0".to_string())
}

fn synthesize_internal_fluid_volume_c_output_rhs(
    prefix: &str,
    index: usize,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(prefix);
    let c = format!("{alias}_C_{index}");
    if !fluid_alias_exists(aliases, &c) {
        let dyn_c = format!("{alias}_dynBal_C_{index}");
        if fluid_alias_exists(aliases, &dyn_c) {
            return Some(dyn_c);
        }
        if let Some(parent_prefix) = prefix.strip_suffix(".dynBal") {
            let parent_alias = var_name_to_c_alias(parent_prefix);
            let parent_c = format!("{parent_alias}_C_{index}");
            return fluid_alias_exists(aliases, &parent_c).then_some(parent_c);
        }
        return None;
    }
    Some(c)
}

fn synthesize_internal_fluid_volume_xi_output_rhs(
    prefix: &str,
    index: usize,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(prefix);
    let medium_xi = format!("{alias}_medium_Xi_{index}");
    if !fluid_alias_exists(aliases, &medium_xi) {
        if let Some(parent_prefix) = prefix.strip_suffix(".dynBal") {
            let parent_alias = var_name_to_c_alias(parent_prefix);
            let parent_medium_xi = format!("{parent_alias}_medium_Xi_{index}");
            if fluid_alias_exists(aliases, &parent_medium_xi) {
                return Some(parent_medium_xi);
            }
        }
        let dyn_medium_xi = format!("{alias}_dynBal_medium_Xi_{index}");
        return fluid_alias_exists(aliases, &dyn_medium_xi).then_some(dyn_medium_xi);
    }
    Some(medium_xi)
}

fn fluid_alias_exists(aliases: &HashSet<String>, alias: &str) -> bool {
    if aliases.contains(alias) {
        return true;
    }
    let Some((base, suffix)) = alias.rsplit_once('_') else {
        return false;
    };
    !suffix.is_empty() && suffix.chars().all(|ch| ch.is_ascii_digit()) && aliases.contains(base)
}

fn internal_fluid_volume_prefix(prefix: &str) -> bool {
    let name = top_level_last_segment(prefix);
    name == "vol"
        || name.strip_prefix("vol").is_some_and(|suffix| {
            !suffix.is_empty() && suffix.chars().all(|ch| ch.is_ascii_digit())
        })
}

fn internal_fluid_dynbal_prefix(prefix: &str) -> bool {
    prefix
        .strip_suffix(".dynBal")
        .is_some_and(internal_fluid_volume_prefix)
}

fn thermodynamic_state_source_port_name(state_prefix: &str) -> Option<&'static str> {
    for (suffix, port) in [
        (".state_a1_inflow", "port_a1"),
        (".state_b1_inflow", "port_b1"),
        (".state_a2_inflow", "port_a2"),
        (".state_b2_inflow", "port_b2"),
        (".sta_a1", "port_a1"),
        (".sta_b1", "port_b1"),
        (".sta_a2", "port_a2"),
        (".sta_b2", "port_b2"),
        (".sta_a", "port_a"),
        (".sta_b", "port_b"),
    ] {
        if state_prefix.ends_with(suffix) {
            return Some(port);
        }
    }
    None
}

fn thermodynamic_state_component_prefix(state_prefix: &str) -> Option<&str> {
    for suffix in [
        ".state_a1_inflow",
        ".state_b1_inflow",
        ".state_a2_inflow",
        ".state_b2_inflow",
        ".sta_a1",
        ".sta_b1",
        ".sta_a2",
        ".sta_b2",
        ".sta_a",
        ".sta_b",
    ] {
        if let Some(prefix) = state_prefix.strip_suffix(suffix) {
            return Some(prefix);
        }
    }
    None
}

fn synthesize_mover_command_signal_rhs(var_name: &str, equations: &Value) -> Option<String> {
    let prefix = var_name.strip_suffix(".varSpeFloMov.y")?;
    let aliases = c_aliases_for_equations(equations);
    let parent_input = format!("{prefix}.u");
    let alias = var_name_to_c_alias(&parent_input);
    if aliases.contains(&alias) {
        return Some(alias);
    }
    None
}

fn synthesize_mover_density_signal_rhs(var_name: &str) -> Option<String> {
    let prefix = var_name
        .strip_suffix(".eff.rho")
        .or_else(|| var_name.strip_suffix(".rho_inlet.y"))?;
    Some(format!("{}_rho_default", var_name_to_c_alias(prefix)))
}

fn synthesize_mover_pressure_signal_rhs(var_name: &str) -> Option<String> {
    let prefix = var_name
        .strip_suffix(".eff.dp")
        .or_else(|| var_name.strip_suffix(".eff.dp_internal"))?;
    let alias = var_name_to_c_alias(prefix);
    let speed_alias = if let Some(parent) = prefix.strip_suffix(".varSpeFloMov") {
        format!("{}_u", var_name_to_c_alias(parent))
    } else {
        format!("{alias}_eff_r_N")
    };
    Some(format!(
        "__rumoca_mover_pressure({alias}_eff_V_flow, {speed_alias}, {alias}_eff_dpMax, {alias}_eff_V_flow_max)"
    ))
}

fn synthesize_modelica_gain_rhs(var_name: &str, aliases: &HashSet<String>) -> Option<String> {
    let prefix = var_name.strip_suffix(".y")?;
    let prefix_alias = var_name_to_c_alias(prefix);
    let u = format!("{prefix_alias}_u");
    let scalar_k = format!("{prefix_alias}_k");
    let indexed_k = format!("{prefix_alias}_k_1");
    let named_gain = prefix.contains(".gain[") || prefix.ends_with(".gain");
    if !named_gain
        && (!fluid_alias_exists(aliases, &u)
            || (!fluid_alias_exists(aliases, &scalar_k)
                && !fluid_alias_exists(aliases, &indexed_k)))
    {
        return None;
    }
    let k = if aliases.contains(&scalar_k) {
        scalar_k
    } else if aliases.contains(&indexed_k) {
        indexed_k
    } else {
        scalar_k
    };
    Some(format!("({k} * {u})"))
}

fn synthesize_modelica_replicator_rhs(var_name: &str, aliases: &HashSet<String>) -> Option<String> {
    let (prefix, _index) = parse_indexed_suffix(var_name, ".y")?;
    if !(prefix.contains("replicator") || prefix.ends_with(".QPeaRep")) {
        return None;
    }
    let prefix_alias = var_name_to_c_alias(prefix);
    let u = format!("{prefix_alias}_u");
    if !fluid_alias_exists(aliases, &u) {
        return None;
    }
    Some(u)
}

fn synthesize_modelica_add_rhs(var_name: &str, aliases: &HashSet<String>) -> Option<String> {
    let (prefix, index) = parse_indexed_suffix(var_name, ".y")?;
    if !prefix.ends_with(".CTot_flow") {
        return None;
    }
    let prefix_alias = var_name_to_c_alias(prefix);
    let k1 = format!("{prefix_alias}_k1_{index}");
    let k2 = format!("{prefix_alias}_k2_{index}");
    let u1 = format!("{prefix_alias}_u1_{index}");
    let u2 = format!("{prefix_alias}_u2_{index}");
    if ![&k1, &k2, &u1, &u2]
        .iter()
        .all(|alias| fluid_alias_exists(aliases, alias))
    {
        return None;
    }
    Some(format!("(({k1} * {u1}) + ({k2} * {u2}))"))
}

fn synthesize_modelica_multiswitch_expr_rhs(var_name: &str) -> Option<String> {
    let (base, index) = parse_indexed_ref(var_name)?;
    if !base.ends_with(".expr") {
        return None;
    }
    Some((index.saturating_sub(1)).to_string())
}

fn synthesize_buildings_actuator_filter_parameter_rhs(var_name: &str) -> Option<String> {
    if let Some(parent_prefix) = var_name.strip_suffix(".fCut") {
        return Some(format!(
            "(5.0 / (2.0 * 3.14159265358979323846 * {}))",
            var_name_to_c_alias(&format!("{parent_prefix}.riseTime"))
        ));
    }

    let filter_prefix = var_name.split(".filter.").next()?;
    if var_name == format!("{filter_prefix}.filter.n") {
        return Some("2.0".to_string());
    }
    if var_name == format!("{filter_prefix}.filter.f") {
        return Some(var_name_to_c_alias(&format!("{filter_prefix}.fCut")));
    }
    if var_name == format!("{filter_prefix}.filter.u_nom") {
        let u_nominal = var_name_to_c_alias(&format!("{filter_prefix}.filter.u_nominal"));
        return Some(format!(
            "((fabs({u_nominal} - 1.0) < 1e-12) ? (1.0 - 1e-12) : {u_nominal})"
        ));
    }
    if var_name == format!("{filter_prefix}.filter.w_u") {
        return Some(format!(
            "(2.0 * 3.14159265358979323846 * {} / {} / {})",
            var_name_to_c_alias(&format!("{filter_prefix}.filter.f")),
            var_name_to_c_alias(&format!("{filter_prefix}.filter.alpha")),
            var_name_to_c_alias(&format!("{filter_prefix}.filter.u_nom"))
        ));
    }

    None
}

fn synthesize_buildings_actuator_filter_alg_rhs(var_name: &str) -> Option<String> {
    if let Some((filter_prefix, index)) = parse_indexed_suffix(var_name, ".x") {
        if !filter_prefix.ends_with(".filter") || !(1..=2).contains(&index) {
            return None;
        }
        return Some(format!(
            "({} * {})",
            var_name_to_c_alias(&format!("{filter_prefix}.u_nom")),
            indexed_alias(&format!("{filter_prefix}.s"), index)
        ));
    }

    if let Some(filter_prefix) = var_name.strip_suffix(".y") {
        if !filter_prefix.ends_with(".filter") {
            return None;
        }
        return Some(format!(
            "({} * {})",
            var_name_to_c_alias(&format!("{filter_prefix}.u_nom")),
            indexed_alias(&format!("{filter_prefix}.s"), 2)
        ));
    }

    None
}

fn synthesize_buildings_limit_slew_rate_parameter_rhs(var_name: &str) -> Option<String> {
    let component_prefix = var_name
        .strip_suffix(".fallingSlewRate")
        .or_else(|| var_name.strip_suffix(".Td"))?;
    if !limit_slew_rate_component_prefix(component_prefix) {
        return None;
    }
    let raising_slew_rate = var_name_to_c_alias(&format!("{component_prefix}.raisingSlewRate"));
    if var_name.ends_with(".fallingSlewRate") {
        Some(format!("(-{raising_slew_rate})"))
    } else {
        Some(format!("({raising_slew_rate} * 10.0)"))
    }
}

fn limit_slew_rate_component_prefix(prefix: &str) -> bool {
    prefix
        .rsplit(|ch| ch == '.' || ch == '_')
        .next()
        .is_some_and(|component| component == "ramLim" || component.starts_with("ramLim"))
}

fn synthesize_buildings_actuator_filter_ode_rhs(state_name: &str) -> Option<String> {
    let (filter_prefix, index) = parse_indexed_suffix(state_name, ".s")?;
    if !filter_prefix.ends_with(".filter") || !(1..=2).contains(&index) {
        return None;
    }

    let u_nom = var_name_to_c_alias(&format!("{filter_prefix}.u_nom"));
    let w_u = var_name_to_c_alias(&format!("{filter_prefix}.w_u"));
    match index {
        1 => Some(format!(
            "(({} - ({} * {})) * {})",
            var_name_to_c_alias(&format!("{filter_prefix}.u")),
            u_nom,
            indexed_alias(&format!("{filter_prefix}.s"), 1),
            w_u
        )),
        2 => Some(format!(
            "(({} * ({} - {})) * {})",
            u_nom,
            indexed_alias(&format!("{filter_prefix}.s"), 1),
            indexed_alias(&format!("{filter_prefix}.s"), 2),
            w_u
        )),
        _ => None,
    }
}

fn synthesize_modelica_integrator_ode_rhs(state_name: &str) -> Option<String> {
    let integrator_prefix = state_name.strip_suffix(".I.y")?;
    let i_prefix = format!("{integrator_prefix}.I");
    let k = var_name_to_c_alias(&format!("{i_prefix}.k"));
    let u = var_name_to_c_alias(&format!("{i_prefix}.u"));
    Some(format!("({k} * {u})"))
}

fn synthesize_internal_fluid_volume_ode_rhs(state_name: &str) -> Option<String> {
    if let Some(volume_prefix) = state_name.strip_suffix(".dynBal.m") {
        return Some(var_name_to_c_alias(&format!(
            "{volume_prefix}.dynBal.mb_flow"
        )));
    }
    if let Some(volume_prefix) = state_name.strip_suffix(".dynBal.U") {
        let hb_flow = var_name_to_c_alias(&format!("{volume_prefix}.dynBal.Hb_flow"));
        let q_flow = var_name_to_c_alias(&format!("{volume_prefix}.dynBal.Q_flow"));
        return Some(format!("({hb_flow} + {q_flow})"));
    }
    if let Some((volume_prefix, index)) = parse_indexed_suffix(state_name, ".dynBal.medium.Xi")
        && internal_fluid_volume_prefix(volume_prefix)
    {
        let m = var_name_to_c_alias(&format!("{volume_prefix}.dynBal.m"));
        let mb_xi_flow = indexed_alias(&format!("{volume_prefix}.dynBal.mbXi_flow"), index);
        let m_wat_flow_internal =
            var_name_to_c_alias(&format!("{volume_prefix}.dynBal.mWat_flow_internal"));
        let s = indexed_alias(&format!("{volume_prefix}.dynBal.s"), index);
        return Some(format!(
            "(({m} > 1e-9) ? (({mb_xi_flow} + ({m_wat_flow_internal} * {s})) / {m}) : 0.0)"
        ));
    }
    if let Some((volume_prefix, index)) = parse_indexed_suffix(state_name, ".dynBal.mC")
        && internal_fluid_volume_prefix(volume_prefix)
    {
        let mb_c_flow = indexed_alias(&format!("{volume_prefix}.dynBal.mbC_flow"), index);
        let c_flow_internal =
            indexed_alias(&format!("{volume_prefix}.dynBal.C_flow_internal"), index);
        return Some(format!("({mb_c_flow} + {c_flow_internal})"));
    }
    None
}

fn synthesize_buildings_temperature_two_port_ode_rhs(state_name: &str) -> Option<String> {
    let sensor_prefix = state_name.strip_suffix(".T")?;
    let sensor_name = top_level_last_segment(sensor_prefix);
    if !sensor_name.starts_with("senT") && !fluid_temperature_sensor_component_name(sensor_name) {
        return None;
    }

    let t = var_name_to_c_alias(state_name);
    let t_med = var_name_to_c_alias(&format!("{sensor_prefix}.TMed"));
    let k = var_name_to_c_alias(&format!("{sensor_prefix}.k"));
    let tau_inv = var_name_to_c_alias(&format!("{sensor_prefix}.tauInv"));
    let transfer_heat = var_name_to_c_alias(&format!("{sensor_prefix}.transferHeat"));
    let t_amb = var_name_to_c_alias(&format!("{sensor_prefix}.TAmb"));
    let tau_hea_tra_inv = var_name_to_c_alias(&format!("{sensor_prefix}.tauHeaTraInv"));
    let rat_tau = var_name_to_c_alias(&format!("{sensor_prefix}.ratTau"));
    Some(format!(
        "((({t_med} - {t}) * {k} * {tau_inv}) + ({transfer_heat} ? ((({t_amb} - {t}) * {tau_hea_tra_inv}) / (({rat_tau} * {k}) + 1.0)) : 0.0))"
    ))
}

fn synthesize_buildings_dynamic_flow_sensor_ode_rhs(state_name: &str) -> Option<String> {
    let (sensor_prefix, state_field, medium_field) = state_name
        .strip_suffix(".d")
        .map(|prefix| (prefix, "d", "dMed"))
        .or_else(|| {
            state_name
                .strip_suffix(".C")
                .map(|prefix| (prefix, "C", "CMed"))
        })
        .or_else(|| {
            state_name
                .strip_suffix(".phi")
                .map(|prefix| (prefix, "phi", "phiMed"))
        })?;
    let sensor_name = top_level_last_segment(sensor_prefix);
    match state_field {
        "d" if !fluid_volume_flow_sensor_prefix_has_name(sensor_prefix) => return None,
        "C" if !trace_substances_two_port_sensor_component_name(sensor_name) => return None,
        "phi" if !sensor_name.starts_with("senRelHum") => return None,
        _ => {}
    }

    let state = var_name_to_c_alias(state_name);
    let medium = var_name_to_c_alias(&format!("{sensor_prefix}.{medium_field}"));
    let k = var_name_to_c_alias(&format!("{sensor_prefix}.k"));
    let tau_inv = var_name_to_c_alias(&format!("{sensor_prefix}.tauInv"));
    Some(format!("(({medium} - {state}) * {k} * {tau_inv})"))
}

fn synthesize_buildings_limit_slew_rate_ode_rhs(state_name: &str) -> Option<String> {
    let component_prefix = state_name.strip_suffix(".y")?;
    if !limit_slew_rate_component_prefix(component_prefix) {
        return None;
    }
    let alias = var_name_to_c_alias(component_prefix);
    let y = var_name_to_c_alias(state_name);
    let thr = format!("(({alias}_u - {y}) / {alias}_Td)");
    Some(format!(
        "({alias}_enable ? (({thr} < {alias}_fallingSlewRate) ? {alias}_fallingSlewRate : (({thr} > {alias}_raisingSlewRate) ? {alias}_raisingSlewRate : {thr})) : 0.0)"
    ))
}

fn synthesize_buildings_expansion_vessel_ode_rhs(state_name: &str) -> Option<String> {
    let vessel_prefix = state_name.strip_suffix(".H")?;

    let m_flow = var_name_to_c_alias(&format!("{vessel_prefix}.port_a.m_flow"));
    let connected_h = var_name_to_c_alias(&format!("{vessel_prefix}.port_a.h_outflow"));
    let h = var_name_to_c_alias(state_name);
    let m = var_name_to_c_alias(&format!("{vessel_prefix}.m"));
    Some(format!(
        "({m_flow} * (({m_flow} > 0.0) ? {connected_h} : (({m} > 1e-9) ? ({h} / {m}) : {connected_h})))"
    ))
}

fn synthesize_mover_interface_signal_rhs(var_name: &str, equations: &Value) -> Option<String> {
    let prefix = var_name
        .strip_suffix(".eff.y_in")
        .or_else(|| var_name.strip_suffix(".eff.y_out"))
        .or_else(|| var_name.strip_suffix(".eff.r_N"))?;
    let aliases = c_aliases_for_equations(equations);
    for candidate in mover_interface_signal_candidates(prefix) {
        let alias = var_name_to_c_alias(&candidate);
        if candidate != var_name && aliases.contains(&alias) {
            return Some(alias);
        }
    }
    None
}

fn mover_interface_signal_candidates(prefix: &str) -> Vec<String> {
    let mut candidates = vec![
        format!("{prefix}.y"),
        format!("{prefix}.inputSwitch.y"),
        format!("{prefix}.filter.y"),
        format!("{prefix}.eff.y_in"),
    ];
    if let Some(parent) = parent_scope(prefix) {
        candidates.push(format!("{parent}.u"));
        candidates.push(format!("{parent}.y"));
    }
    candidates
}

fn synthesize_conpi_control_signal_rhs(var_name: &str, equations: &Value) -> Option<String> {
    let aliases = c_aliases_for_equations(equations);

    if let Some(prefix) = var_name.strip_suffix(".booToRea.y")
        && prefix.ends_with(".conPI")
    {
        let input = format!("{prefix}.booToRea.u");
        let input_alias = var_name_to_c_alias(&input);
        if aliases.contains(&input_alias) {
            return Some(format!("({input_alias} ? 1.0 : 0.0)"));
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".mul.u1")
        && prefix.ends_with(".conPI")
    {
        let boolean_output = format!("{prefix}.booToRea.y");
        let alias = var_name_to_c_alias(&boolean_output);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".mul.u2")
        && prefix.ends_with(".conPI")
    {
        let pid_output = format!("{prefix}.conPID.y");
        let alias = var_name_to_c_alias(&pid_output);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".conPI.set") {
        let parent_setpoint = format!("{prefix}.dpSet");
        let alias = var_name_to_c_alias(&parent_setpoint);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".conPI.mea") {
        for measurement in [format!("{prefix}.dpMea"), format!("{prefix}.dp")] {
            let alias = var_name_to_c_alias(&measurement);
            if aliases.contains(&alias) {
                return Some(alias);
            }
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".secPumCon.dpSet") {
        let plant_setpoint = format!("{prefix}.dpSet");
        let alias = var_name_to_c_alias(&plant_setpoint);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".secPumCon.dpMea") {
        for measurement in [format!("{prefix}.dpMea"), format!("{prefix}.dp")] {
            let alias = var_name_to_c_alias(&measurement);
            if aliases.contains(&alias) {
                return Some(alias);
            }
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".conPID.u_s")
        && prefix.ends_with(".conPI")
    {
        let setpoint = format!("{prefix}.set");
        let alias = var_name_to_c_alias(&setpoint);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".conPID.u_m")
        && prefix.ends_with(".conPI")
    {
        let measurement = format!("{prefix}.mea");
        let alias = var_name_to_c_alias(&measurement);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".conPID.controlError.u1")
        && prefix.ends_with(".conPI")
    {
        let setpoint = format!("{prefix}.conPID.u_s");
        let alias = var_name_to_c_alias(&setpoint);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".conPID.controlError.u2")
        && prefix.ends_with(".conPI")
    {
        let measurement = format!("{prefix}.conPID.u_m");
        let alias = var_name_to_c_alias(&measurement);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".conPID.P.u")
        && prefix.ends_with(".conPI")
    {
        let control_error = format!("{prefix}.conPID.controlError.y");
        let alias = var_name_to_c_alias(&control_error);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".conPID.addPID.u1")
        && prefix.ends_with(".conPI")
    {
        let proportional_output = format!("{prefix}.conPID.P.y");
        let alias = var_name_to_c_alias(&proportional_output);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    if let Some((prefix, index)) =
        parse_indexed_component_signal(var_name, ".secPumCon.product[", "].u1")
    {
        let pump_stage = format!("{prefix}.secPumCon.pumSta.y[{index}]");
        let alias = var_name_to_c_alias(&pump_stage);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    if let Some((prefix, index)) =
        parse_indexed_component_signal(var_name, ".secPumCon.product[", "].u2")
    {
        let replicated_command = format!("{prefix}.secPumCon.replicator.y[{index}]");
        let alias = var_name_to_c_alias(&replicated_command);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    if let Some((prefix, index)) = parse_indexed_component_signal(var_name, ".secPumCon.y[", "]") {
        let product_output = format!("{prefix}.secPumCon.product[{index}].y");
        let alias = var_name_to_c_alias(&product_output);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".lim.y") {
        let input = format!("{prefix}.lim.u");
        let alias = var_name_to_c_alias(&input);
        if aliases.contains(&alias) {
            return Some(alias);
        }
    }

    None
}

fn synthesize_pump_stage_output_rhs(var_name: &str, _equations: &Value) -> Option<String> {
    let (prefix, index) = parse_indexed_component_signal(var_name, ".secPumCon.pumSta.y[", "]")?;
    if index == 0 {
        return None;
    }
    let pump_stage_on = format!("{prefix}.secPumCon.pumSta.On");
    let alias = var_name_to_c_alias(&pump_stage_on);
    Some(format!("({alias} ? 1.0 : 0.0)"))
}

fn synthesize_stategraph_active_steps_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let prefix = var_name
        .strip_suffix(".stateGraphRoot.subgraphStatePort.activeSteps")
        .or_else(|| var_name.strip_suffix(".stateGraphRoot.activeSteps"))?;
    let prefix_alias = var_name_to_c_alias(prefix);
    let mut active_aliases = aliases
        .iter()
        .filter(|alias| alias.starts_with(&format!("{prefix_alias}_")))
        .filter(|alias| alias.ends_with("_active"))
        .filter(|alias| !alias.contains("_stateGraphRoot_"))
        .filter(|alias| !alias.contains("_outerStatePort_"))
        .filter(|alias| !alias.contains("_subgraphStatePort_"))
        .filter(|alias| !alias.ends_with("_localActive"))
        .filter(|alias| !alias.ends_with("_newActive"))
        .filter(|alias| !alias.ends_with("_oldActive"))
        .cloned()
        .collect::<Vec<_>>();
    active_aliases.sort();
    active_aliases.dedup();
    if active_aliases.is_empty() {
        None
    } else {
        Some(active_aliases.join(" + "))
    }
}

fn synthesize_stage_n_multiswitch_rhs(var_name: &str, aliases: &HashSet<String>) -> Option<String> {
    let prefix = var_name
        .strip_suffix(".multiSwitch.y")
        .or_else(|| var_name.strip_suffix(".y"))?;
    if !(prefix.ends_with(".nSta") || prefix.ends_with(".pumNSta")) {
        return None;
    }
    let terms = stage_n_active_terms(prefix, aliases)?;
    Some(
        terms
            .into_iter()
            .map(|(step, alias)| format!("({alias} ? {step}.0 : 0.0)"))
            .collect::<Vec<_>>()
            .join(" + "),
    )
}

fn synthesize_stage_n_vector_output_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let (stage_prefix, index) = if let Some((prefix, index)) =
        parse_indexed_component_signal(var_name, ".chiSta.y[", "]")
    {
        (format!("{prefix}.chiSta.nSta"), index)
    } else if let Some((prefix, index)) =
        parse_indexed_component_signal(var_name, ".boiSta.y[", "]")
    {
        (format!("{prefix}.boiSta.nSta"), index)
    } else if let Some(index) =
        parse_read_surface_stage_index(var_name, "reaChiWatSys_ChiSta_", "_y")
    {
        (
            "chilledWaterPlant.chillerPlant.chiSta.nSta".to_string(),
            index,
        )
    } else if let Some(index) =
        parse_read_surface_stage_index(var_name, "reaHotWatSys_BoiSta_", "_y")
    {
        ("hotWaterPlant.boilerPlant.boiSta.nSta".to_string(), index)
    } else if let Some(index) =
        parse_read_surface_stage_index(var_name, "chilledWaterPlant.chillerEnable[", "]")
    {
        (
            "chilledWaterPlant.chillerPlant.chiSta.nSta".to_string(),
            index,
        )
    } else if let Some(index) =
        parse_read_surface_stage_index(var_name, "hotWaterPlant.boilerEnable[", "]")
    {
        ("hotWaterPlant.boilerPlant.boiSta.nSta".to_string(), index)
    } else {
        return None;
    };
    if index == 0 {
        return None;
    }
    let terms = stage_n_active_terms(&stage_prefix, aliases)?;
    let level = terms
        .into_iter()
        .map(|(step, alias)| format!("({alias} ? {step}.0 : 0.0)"))
        .collect::<Vec<_>>()
        .join(" + ");
    Some(format!("(({level}) >= {index}.0 ? 1.0 : 0.0)"))
}

fn stage_n_active_terms(prefix: &str, aliases: &HashSet<String>) -> Option<Vec<(usize, String)>> {
    let prefix_alias = var_name_to_c_alias(prefix);
    let mut terms = aliases
        .iter()
        .filter_map(|alias| {
            let rest = alias.strip_prefix(&format!("{prefix_alias}_"))?;
            if rest.starts_with("iOn_") && rest.ends_with("_active") {
                let step = rest
                    .trim_start_matches("iOn_")
                    .trim_end_matches("_active")
                    .parse::<usize>()
                    .ok()?;
                Some((step, alias.clone()))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let terminal_step = terms
        .iter()
        .map(|(step, _)| *step)
        .max()
        .map(|step| step + 1);
    if let Some(step) = terminal_step {
        let terminal_alias = format!("{prefix_alias}_nOn_active");
        if aliases.contains(&terminal_alias) {
            terms.push((step, terminal_alias));
        }
    }
    terms.sort_by_key(|(step, _)| *step);
    terms.dedup_by(|a, b| a.0 == b.0 && a.1 == b.1);
    if terms.is_empty() { None } else { Some(terms) }
}

fn parse_read_surface_stage_index(var_name: &str, prefix: &str, suffix: &str) -> Option<usize> {
    var_name
        .strip_prefix(prefix)?
        .strip_suffix(suffix)?
        .parse::<usize>()
        .ok()
}

fn synthesize_modelica_stategraph_transition_timer_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let prefix = var_name.strip_suffix(".t")?;
    let prefix_alias = var_name_to_c_alias(prefix);
    let enable_fire = format!("{prefix_alias}_enableFire");
    let elapsed = format!("{prefix_alias}_t_dummy");
    if aliases.contains(&enable_fire) && aliases.contains(&elapsed) {
        Some(format!("({enable_fire} ? {elapsed} : 0.0)"))
    } else {
        None
    }
}

fn parse_indexed_component_signal<'a>(
    var_name: &'a str,
    marker: &str,
    suffix: &str,
) -> Option<(&'a str, usize)> {
    let marker_pos = var_name.rfind(marker)?;
    let prefix = &var_name[..marker_pos];
    let rest = &var_name[marker_pos + marker.len()..];
    let index_text = rest.strip_suffix(suffix)?;
    let index = index_text.parse::<usize>().ok()?;
    Some((prefix, index))
}

fn rhs_references_same_fluid_component(rhs: &str, var_name: &str) -> bool {
    let alias = var_name_to_c_alias(var_name);
    let Some(stem) = alias
        .strip_suffix("_m_flow")
        .or_else(|| alias.strip_suffix("_p"))
        .or_else(|| alias.strip_suffix("_h_outflow"))
    else {
        return false;
    };
    rhs.contains(stem)
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

    let Some(candidate_equations) = alg_equation_candidates_for_var(&equations, &name_str) else {
        return Ok(var_name_to_c_alias(&name_str));
    };
    for eq in candidate_equations {
        if let Ok(Some(rhs_expr)) = find_algebraic_rhs(&eq, &name_str, &cfg) {
            let rhs_expr = c_rhs_with_equation_context(rhs_expr, &equations);
            if c_rhs_is_supported(&rhs_expr) {
                return Ok(rhs_expr);
            }
        }
    }

    Ok(var_name_to_c_alias(&name_str))
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
    let mut self_alias_rhs = None;

    if let Ok(iter) = equations_z.try_iter() {
        for eq in iter {
            if let Some(rhs_expr) = find_algebraic_rhs(&eq, &name, &cfg)? {
                let rhs_expr = c_rhs_with_equation_context(rhs_expr, &equations_z);
                if c_event_or_discrete_rhs_is_supported(&rhs_expr) {
                    if rhs_is_simple_self_alias(&rhs_expr, &name) {
                        self_alias_rhs = Some(rhs_expr);
                        continue;
                    }
                    return Ok(rhs_expr);
                }
            }
        }
    }
    if let Ok(iter) = equations_m.try_iter() {
        for eq in iter {
            if let Some(rhs_expr) = find_algebraic_rhs(&eq, &name, &cfg)? {
                let rhs_expr = c_rhs_with_equation_context(rhs_expr, &equations_m);
                if c_event_or_discrete_rhs_is_supported(&rhs_expr) {
                    if rhs_is_simple_self_alias(&rhs_expr, &name) {
                        self_alias_rhs = Some(rhs_expr);
                        continue;
                    }
                    return Ok(rhs_expr);
                }
            }
        }
    }

    if let Some(synthesized) = synthesize_discrete_statespace_rhs(&name, &dae) {
        return Ok(synthesized);
    }

    if let Some(synthesized) = synthesize_parent_discrete_enable_rhs(&name, &dae) {
        return Ok(synthesized);
    }

    if let Some(synthesized) = synthesize_modelica_stategraph_new_active_rhs(&name, &equations_m) {
        return Ok(synthesized);
    }

    if let Some(synthesized) = synthesize_modelica_stategraph_step_port_rhs(&name, &equations_m) {
        return Ok(synthesized);
    }

    if let Some(rhs_expr) = self_alias_rhs {
        return Ok(rhs_expr);
    }

    Ok(var_name_to_c_alias(&name))
}

fn synthesize_modelica_stategraph_new_active_rhs(
    var_name: &str,
    equations_m: &Value,
) -> Option<String> {
    let prefix = var_name.strip_suffix(".newActive")?;
    let aliases = c_aliases_for_equations(equations_m);
    let prefix_alias = var_name_to_c_alias(prefix);
    let local_active = format!("{prefix_alias}_localActive");
    if !aliases.contains(&local_active) {
        return None;
    }

    let any_in_set = joined_indexed_alias_or(&aliases, &format!("{prefix_alias}_inPort_"), "_set")
        .unwrap_or_else(|| "0".to_string());
    let any_out_reset =
        joined_indexed_alias_or(&aliases, &format!("{prefix_alias}_outPort_"), "_reset")
            .unwrap_or_else(|| "0".to_string());
    let resume = alias_or_zero(
        &aliases,
        &format!("{prefix_alias}_outerStatePort_subgraphStatePort_resume"),
    );
    let suspend = alias_or_zero(
        &aliases,
        &format!("{prefix_alias}_outerStatePort_subgraphStatePort_suspend"),
    );
    let old_active = alias_or_fallback(
        &aliases,
        &format!("{prefix_alias}_oldActive"),
        &local_active,
    );

    Some(format!(
        "({resume} ? {old_active} : ((({any_in_set}) || ({local_active} && !({any_out_reset}))) && !({suspend})))"
    ))
}

fn synthesize_modelica_stategraph_step_port_rhs(
    var_name: &str,
    equations_m: &Value,
) -> Option<String> {
    let aliases = c_aliases_for_equations(equations_m);
    let (prefix, index, port_kind) = parse_stategraph_step_port(var_name)?;
    let prefix_alias = var_name_to_c_alias(prefix);
    let local_active = format!("{prefix_alias}_localActive");
    if !aliases.contains(&local_active) {
        return None;
    }

    match port_kind {
        StateGraphStepPortKind::InOccupied => {
            if index == 1 {
                Some(local_active)
            } else {
                let prev = index - 1;
                let occupied = format!("{prefix_alias}_inPort_{prev}_occupied");
                let set = format!("{prefix_alias}_inPort_{prev}_set");
                Some(format!(
                    "({} || {})",
                    alias_or_zero(&aliases, &occupied),
                    alias_or_zero(&aliases, &set)
                ))
            }
        }
        StateGraphStepPortKind::OutAvailable => {
            if index == 1 {
                Some(local_active)
            } else {
                let prev = index - 1;
                let available = format!("{prefix_alias}_outPort_{prev}_available");
                let reset = format!("{prefix_alias}_outPort_{prev}_reset");
                Some(format!(
                    "({} && !({}))",
                    alias_or_zero(&aliases, &available),
                    alias_or_zero(&aliases, &reset)
                ))
            }
        }
    }
}

#[derive(Clone, Copy)]
enum StateGraphStepPortKind {
    InOccupied,
    OutAvailable,
}

fn parse_stategraph_step_port(var_name: &str) -> Option<(&str, usize, StateGraphStepPortKind)> {
    if let Some(stem) = var_name.strip_suffix(".occupied") {
        let (prefix, index) = parse_indexed_suffix(stem, ".inPort")?;
        if index == 0 {
            return None;
        }
        return Some((prefix, index, StateGraphStepPortKind::InOccupied));
    }
    if let Some(stem) = var_name.strip_suffix(".available") {
        let (prefix, index) = parse_indexed_suffix(stem, ".outPort")?;
        if index == 0 {
            return None;
        }
        return Some((prefix, index, StateGraphStepPortKind::OutAvailable));
    }
    None
}

fn alias_or_zero(aliases: &HashSet<String>, alias: &str) -> String {
    if aliases.contains(alias) {
        alias.to_string()
    } else {
        "0".to_string()
    }
}

fn alias_or_fallback(aliases: &HashSet<String>, alias: &str, fallback: &str) -> String {
    if aliases.contains(alias) {
        alias.to_string()
    } else {
        fallback.to_string()
    }
}

fn joined_indexed_alias_or(aliases: &HashSet<String>, start: &str, end: &str) -> Option<String> {
    let mut matches = aliases
        .iter()
        .filter(|alias| alias.starts_with(start) && alias.ends_with(end))
        .filter(|alias| indexed_alias_middle_is_positive(alias, start, end))
        .cloned()
        .collect::<Vec<_>>();
    matches.sort_by(|a, b| {
        indexed_or_scalar_alias_sort_key(a, start, end)
            .cmp(&indexed_or_scalar_alias_sort_key(b, start, end))
    });
    if matches.is_empty() {
        None
    } else {
        Some(matches.join(" || "))
    }
}

fn indexed_or_scalar_alias_sort_key(alias: &str, start: &str, end: &str) -> Vec<usize> {
    if alias.len() <= start.len() + end.len() {
        Vec::new()
    } else {
        scalarized_alias_sort_key(alias, start, end)
    }
}

fn indexed_alias_middle_is_positive(alias: &str, start: &str, end: &str) -> bool {
    if alias.len() <= start.len() + end.len() {
        return false;
    }
    alias[start.len()..alias.len() - end.len()]
        .split('_')
        .all(|part| part.parse::<usize>().is_ok_and(|value| value > 0))
}

pub(super) fn event_indicator_expr_function(expr: Value, config: Value) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    let rendered = render_expression(&expr, &cfg).unwrap_or_else(|_| String::new());
    if c_event_or_discrete_rhs_is_supported(&rendered) {
        Ok(rendered)
    } else {
        Ok("0.0".to_string())
    }
}

fn collect_c_aliases_from_equations(equations: &Value) -> HashSet<String> {
    let mut aliases = HashSet::new();
    let Ok(iter) = equations.try_iter() else {
        return aliases;
    };
    let mut names = HashSet::new();
    for eq in iter {
        collect_equation_var_refs(&eq, &mut names);
    }
    for name in names {
        aliases.insert(var_name_to_c_alias(&name));
        if let Some((base_name, _)) = name.split_once('[') {
            aliases.insert(var_name_to_c_alias(base_name));
        }
    }
    aliases
}

fn c_aliases_for_equations(equations: &Value) -> HashSet<String> {
    let Some(fingerprint) = fingerprint_equations(equations) else {
        return HashSet::new();
    };
    EQUATION_ALIAS_INDEX.with(|cache| {
        let mut cached = cache.borrow_mut();
        if let Some(index) = cached.as_ref()
            && index.fingerprint == fingerprint
        {
            return index.aliases.clone();
        }
        let aliases = collect_c_aliases_from_equations(equations);
        *cached = Some(EquationAliasIndex {
            fingerprint,
            aliases: aliases.clone(),
        });
        aliases
    })
}

fn collect_c_aliases_from_dae(dae: &Value) -> HashSet<String> {
    let mut aliases = c_aliases_for_equations(
        &dae.get_attr("f_x")
            .unwrap_or(Value::from(Vec::<Value>::new())),
    );
    for field in ["x", "y", "w", "z", "m", "u", "p", "constants"] {
        let Ok(vars) = dae.get_attr(field) else {
            continue;
        };
        collect_c_aliases_from_variable_map(&vars, &mut aliases);
    }
    aliases
}

fn fingerprint_dae_alias_context(dae: &Value) -> Option<u64> {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    fingerprint_equations(
        &dae.get_attr("f_x")
            .unwrap_or(Value::from(Vec::<Value>::new())),
    )?
    .hash(&mut hasher);
    for field in ["x", "y", "w", "z", "m", "u", "p", "constants"] {
        field.hash(&mut hasher);
        let Ok(vars) = dae.get_attr(field) else {
            0usize.hash(&mut hasher);
            continue;
        };
        vars.len()?.hash(&mut hasher);
        if let Some(object) = vars.as_object()
            && let Some(iter) = object.try_iter_pairs()
        {
            for (key, _) in iter.take(8) {
                key.to_string().hash(&mut hasher);
            }
        }
    }
    Some(hasher.finish())
}

fn collect_c_aliases_from_variable_map(vars: &Value, aliases: &mut HashSet<String>) {
    let Some(object) = vars.as_object() else {
        return;
    };
    let Some(iter) = object.try_iter_pairs() else {
        return;
    };
    for (key, _) in iter {
        let name = key.to_string().trim_matches('"').to_string();
        aliases.insert(var_name_to_c_alias(&name));
        if let Some((base_name, _)) = name.split_once('[') {
            aliases.insert(var_name_to_c_alias(base_name));
        }
    }
}

fn expand_sum_calls_for_scalarized_aliases(rhs: &str, aliases: &HashSet<String>) -> String {
    let mut out = String::with_capacity(rhs.len());
    let mut rest = rhs;
    const SUM_CALL: &str = "__rumoca_sum_d(";

    while let Some(call_start) = rest.find(SUM_CALL) {
        out.push_str(&rest[..call_start]);
        let after_call = &rest[call_start + SUM_CALL.len()..];
        let Some(call_end) = after_call.find(')') else {
            out.push_str(&rest[call_start..]);
            return out;
        };
        let args = &after_call[..call_end];
        let replacement = expand_single_sum_call(args, aliases);
        out.push_str(&replacement);
        rest = &after_call[call_end + 1..];
    }
    out.push_str(rest);
    out
}

fn expand_single_sum_call(args: &str, aliases: &HashSet<String>) -> String {
    let mut parts = args.split(',').map(str::trim);
    let Some(base) = parts.next() else {
        return "0.0".to_string();
    };
    let Some(len_arg) = parts.next() else {
        return "0.0".to_string();
    };
    if parts.next().is_some() {
        return "0.0".to_string();
    }
    if let Some(expanded) = scalarized_alias_sum(base, aliases) {
        return expanded;
    }
    if aliases.contains(base) {
        return format!("__rumoca_sum_d({base}, {len_arg})");
    }
    "0.0".to_string()
}

fn scalarized_alias_sum(base: &str, aliases: &HashSet<String>) -> Option<String> {
    let mut best_matches: Vec<String> = Vec::new();
    let mut split_positions = base
        .match_indices('_')
        .map(|(idx, _)| idx)
        .collect::<Vec<_>>();
    split_positions.reverse();
    for split in split_positions {
        let prefix = &base[..split];
        let suffix = &base[split + 1..];
        if prefix.is_empty() || suffix.is_empty() {
            continue;
        }
        let start = format!("{prefix}_");
        let end = format!("_{suffix}");
        let mut matches = aliases
            .iter()
            .filter(|alias| alias.starts_with(&start) && alias.ends_with(&end))
            .filter(|alias| alias.len() > start.len() + end.len())
            .filter(|alias| {
                let middle = &alias[start.len()..alias.len() - end.len()];
                middle
                    .split('_')
                    .all(|part| !part.is_empty() && part.chars().all(|ch| ch.is_ascii_digit()))
            })
            .cloned()
            .collect::<Vec<_>>();
        if matches.len() > best_matches.len() {
            matches.sort_by(|a, b| {
                scalarized_alias_sort_key(a, &start, &end)
                    .cmp(&scalarized_alias_sort_key(b, &start, &end))
            });
            best_matches = matches;
        }
    }
    if best_matches.is_empty() {
        None
    } else {
        Some(format!("({})", best_matches.join(" + ")))
    }
}

fn scalarized_alias_sort_key(alias: &str, start: &str, end: &str) -> Vec<usize> {
    alias[start.len()..alias.len() - end.len()]
        .split('_')
        .filter_map(|part| part.parse::<usize>().ok())
        .collect()
}

fn alg_equation_candidates_for_var(equations: &Value, var_name: &str) -> Option<Vec<Value>> {
    let fingerprint = fingerprint_equations(equations)?;
    ALG_EQUATION_CANDIDATE_INDEX.with(|cache| {
        let mut cached = cache.borrow_mut();
        let index = match cached.as_ref() {
            Some(index) if index.fingerprint == fingerprint => index,
            _ => {
                *cached = Some(build_alg_equation_candidate_index(equations, fingerprint)?);
                cached.as_ref()?
            }
        };

        let mut candidates = Vec::new();
        append_equation_candidates(index, var_name, &mut candidates);
        if let Some(base_name) = var_name.split_once('[').map(|(base, _)| base) {
            append_equation_candidates(index, base_name, &mut candidates);
        }
        Some(candidates)
    })
}

fn append_equation_candidates(
    index: &AlgEquationCandidateIndex,
    var_name: &str,
    candidates: &mut Vec<Value>,
) {
    let Some(equations) = index.by_var_name.get(var_name) else {
        return;
    };
    for eq in equations {
        candidates.push(eq.clone());
    }
}

fn build_alg_equation_candidate_index(
    equations: &Value,
    fingerprint: u64,
) -> Option<AlgEquationCandidateIndex> {
    let Ok(iter) = equations.try_iter() else {
        return None;
    };
    let mut by_var_name: HashMap<String, Vec<Value>> = HashMap::new();
    for eq in iter {
        let mut var_names = HashSet::new();
        collect_equation_var_refs(&eq, &mut var_names);
        for var_name in var_names {
            by_var_name.entry(var_name).or_default().push(eq.clone());
        }
    }
    Some(AlgEquationCandidateIndex {
        fingerprint,
        by_var_name,
    })
}

fn fingerprint_equations(equations: &Value) -> Option<u64> {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    equations.len()?.hash(&mut hasher);
    if let Ok(iter) = equations.try_iter() {
        for eq in iter.take(16) {
            eq.to_string().hash(&mut hasher);
        }
    }
    Some(hasher.finish())
}

fn collect_equation_var_refs(eq: &Value, out: &mut HashSet<String>) {
    if let Ok(lhs) = eq.get_attr("lhs") {
        collect_lhs_var_refs(&lhs, out);
    }
    if let Ok(rhs) = eq.get_attr("rhs") {
        collect_expr_var_refs(&rhs, out);
    }
}

fn collect_lhs_var_refs(lhs: &Value, out: &mut HashSet<String>) {
    if let Some(lhs_str) = lhs.as_str() {
        out.insert(lhs_str.trim_matches('"').to_string());
        return;
    }
    collect_expr_var_refs(lhs, out);
}

fn insert_var_name(out: &mut HashSet<String>, name: &str) {
    if name.is_empty() {
        return;
    }
    out.insert(name.to_string());
    if let Some((base_name, _)) = name.split_once('[') {
        out.insert(base_name.to_string());
    }
}

fn collect_expr_var_refs(expr: &Value, out: &mut HashSet<String>) {
    if let Ok(var_ref) = get_field(expr, "VarRef") {
        let full_name = var_ref_full_name(&var_ref);
        insert_var_name(out, &full_name);
        return;
    }
    if let Ok(binary) = get_field(expr, "Binary") {
        if let Ok(lhs) = get_field(&binary, "lhs") {
            collect_expr_var_refs(&lhs, out);
        }
        if let Ok(rhs) = get_field(&binary, "rhs") {
            collect_expr_var_refs(&rhs, out);
        }
        return;
    }
    if let Ok(unary) = get_field(expr, "Unary") {
        if let Ok(rhs) = get_field(&unary, "rhs") {
            collect_expr_var_refs(&rhs, out);
        }
        return;
    }
    if let Ok(call) = get_field(expr, "BuiltinCall").or_else(|_| get_field(expr, "FunctionCall")) {
        if let Ok(args) = get_field(&call, "args") {
            collect_list_var_refs(&args, out);
        }
        return;
    }
    if let Ok(if_expr) = get_field(expr, "If") {
        if let Ok(branches) = get_field(&if_expr, "branches")
            && let Some(len) = branches.len()
        {
            for i in 0..len {
                if let Ok(branch) = branches.get_item(&Value::from(i)) {
                    collect_list_var_refs(&branch, out);
                }
            }
        }
        if let Ok(else_branch) = get_field(&if_expr, "else_branch") {
            collect_expr_var_refs(&else_branch, out);
        }
        return;
    }
    if let Ok(array) = get_field(expr, "Array").or_else(|_| get_field(expr, "Tuple")) {
        if let Ok(elements) = get_field(&array, "elements") {
            collect_list_var_refs(&elements, out);
        }
        return;
    }
    if let Ok(range) = get_field(expr, "Range") {
        if let Ok(start) = get_field(&range, "start") {
            collect_expr_var_refs(&start, out);
        }
        if let Ok(step) = get_field(&range, "step") {
            collect_expr_var_refs(&step, out);
        }
        if let Ok(end) = get_field(&range, "end") {
            collect_expr_var_refs(&end, out);
        }
        return;
    }
    if let Ok(index) = get_field(expr, "Index") {
        if let Ok(base) = get_field(&index, "base") {
            collect_expr_var_refs(&base, out);
        }
        if let Ok(subscripts) = get_field(&index, "subscripts") {
            collect_list_var_refs(&subscripts, out);
        }
        return;
    }
    if let Ok(field_access) = get_field(expr, "FieldAccess")
        && let Ok(base) = get_field(&field_access, "base")
    {
        collect_expr_var_refs(&base, out);
    }
}

fn collect_list_var_refs(list: &Value, out: &mut HashSet<String>) {
    let Some(len) = list.len() else {
        return;
    };
    for i in 0..len {
        if let Ok(item) = list.get_item(&Value::from(i)) {
            collect_expr_var_refs(&item, out);
        }
    }
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
        return Ok(render_var_ref_with_indices(&var_ref, &[row, col], cfg));
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
        if let Some((_base_name, index)) = parse_indexed_ref(var_name)
            && let Some(projected) = render_array_expr_at_index(rhs, index, cfg)
        {
            return Ok(Some(projected));
        }
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

        if let Some((_base_name, index)) = parse_indexed_ref(var_name)
            && let Some(projected) = render_array_expr_at_index(&rhs, index, cfg)
        {
            return Ok(Some(projected));
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
// ── Upstream-aligned derivative/algebraic helper support ──

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

fn der_index_of_base(expr: &Value, state_base: &str) -> Option<usize> {
    let name = der_ref_name(expr)?;
    let (base, index) = parse_indexed_ref(&name)?;
    if base == state_base {
        Some(index)
    } else {
        None
    }
}

fn is_der_of_whole(expr: &Value, state_base: &str) -> bool {
    let state_base = state_base.trim_matches('"');
    der_ref_name(expr).is_some_and(|name| name == state_base)
}

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

    if let Ok(call) = get_field(expr, "FunctionCall") {
        return render_function_array_expr_at_index(&call, index, cfg);
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

fn render_array_expr_at_index_checked(
    expr: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    match render_array_expr_at_index(expr, index, cfg) {
        Some(rendered) => Ok(Some(rendered)),
        None => no_render_match(),
    }
}

fn render_array_expr_at_index_or_scalar_checked(
    expr: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    if let Some(rendered) = render_array_expr_at_index(expr, index, cfg) {
        return Ok(Some(rendered));
    }
    render_expression(expr, cfg).map(Some)
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
                .map(|v| v.to_string())
                .unwrap_or_else(|_| n.to_string())
        })
        .map_or(String::new(), |raw| raw);
    let name = if cfg.sanitize_dots {
        super::sanitize_name(&raw_name)
    } else {
        super::escape_reserved_keyword(&raw_name)
    };
    if cfg.subscript_underscore {
        Some(format!("{name}_{index}"))
    } else if cfg.one_based_index {
        Some(format!("{name}[{index}]"))
    } else {
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
        "Linspace" => render_linspace_array_expr_at_index(&args, index, cfg),
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

fn render_function_array_expr_at_index(
    call: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    let name = get_field(call, "name").ok()?;
    let raw_name = render_serialized_name(&name);
    if top_level_last_segment(&raw_name) != "linspace" {
        return None;
    }
    let args = get_field(call, "args").ok()?;
    render_linspace_array_expr_at_index(&args, index, cfg)
}

fn render_linspace_array_expr_at_index(
    args: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    if args.len()? != 3 || index == 0 {
        return None;
    }
    let start = args.get_item(&Value::from(0)).ok()?;
    let stop = args.get_item(&Value::from(1)).ok()?;
    let count = args.get_item(&Value::from(2)).ok()?;
    let start = render_expression(&start, cfg).ok()?;
    let stop = render_expression(&stop, cfg).ok()?;
    let count = render_expression(&count, cfg).ok()?;
    let offset = index - 1;
    Some(format!(
        "((({count}) <= 1.0) ? ({start}) : (({start}) + (({stop}) - ({start})) * ({offset}.0 / (({count}) - 1.0))))"
    ))
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
            .map_or(String::new(), |v| v.to_string());
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

/// Convert a Modelica variable reference name into the local C alias format
/// used by templates when `subscript_underscore = true`.
/// Examples: `x` -> `x`, `x[1]` -> `x_1`, `a.b[1,2]` -> `a_b_1_2`.
fn var_name_to_c_alias(name: &str) -> String {
    super::sanitize_name(name)
}

fn synthesize_discrete_statespace_rhs(var_name: &str, dae: &Value) -> Option<String> {
    if let Some(prefix) = var_name.strip_suffix(".e") {
        let setpoint = format!("{prefix}.setpoint");
        let measurement = format!("{prefix}.measurement");
        if has_var_in_dae_map(dae, "y", &setpoint) && has_var_in_dae_map(dae, "y", &measurement) {
            return Some(format!(
                "({}) - ({})",
                var_name_to_c_alias(&setpoint),
                var_name_to_c_alias(&measurement)
            ));
        }
        return None;
    }

    if let Some(prefix) = var_name.strip_suffix(".u_k") {
        let x_name = format!("{prefix}.x");
        let e_name = format!("{prefix}.e");
        let c_name = format!("{prefix}.C_d");
        let d_name = format!("{prefix}.D_d");
        let e_expr = current_error_expr(prefix, dae);
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
                indexed_alias(&c_name, j),
                indexed_alias(&x_name, j)
            ));
        }
        terms.push(format!("({} * {})", var_name_to_c_alias(&d_name), e_expr));
        return Some(terms.join(" + "));
    }

    if let Some((prefix, i)) = parse_indexed_suffix(var_name, ".x") {
        let x_name = format!("{prefix}.x");
        let e_name = format!("{prefix}.e");
        let a_name = format!("{prefix}.A_d");
        let b_name = format!("{prefix}.B_d");
        let e_expr = current_error_expr(prefix, dae);
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
                indexed_alias(&a_name, flat_idx),
                indexed_alias(&x_name, j)
            ));
        }
        terms.push(format!("({} * {})", indexed_alias(&b_name, i), e_expr));
        return Some(terms.join(" + "));
    }

    None
}

fn synthesize_parent_discrete_enable_rhs(var_name: &str, dae: &Value) -> Option<String> {
    if var_name.ends_with(".onFanOcc") {
        return synthesize_parent_signal_rhs(var_name, ".onFanOcc", dae);
    }
    if let Some(prefix) = var_name.strip_suffix(".mixingBox.On") {
        let candidate = format!("{prefix}.onFanOcc");
        if dae_contains_var(dae, &candidate) {
            return Some(var_name_to_c_alias(&candidate));
        }
    }
    if let Some(prefix) = var_name.strip_suffix(".booToRea.u")
        && prefix.ends_with(".conPI")
    {
        let candidate = format!("{prefix}.On");
        if dae_contains_var(dae, &candidate) {
            return Some(var_name_to_c_alias(&candidate));
        }
    }
    if let Some(prefix) = var_name.strip_suffix(".conPID.trigger")
        && prefix.ends_with(".conPI")
    {
        let candidate = format!("{prefix}.On");
        if dae_contains_var(dae, &candidate) {
            return Some(var_name_to_c_alias(&candidate));
        }
    }
    if let Some(prefix) = var_name.strip_suffix(".conPID.I.trigger")
        && prefix.ends_with(".conPI")
    {
        let pid_trigger = format!("{prefix}.conPID.trigger");
        if dae_contains_var(dae, &pid_trigger) {
            return Some(var_name_to_c_alias(&pid_trigger));
        }
        let on_signal = format!("{prefix}.On");
        if dae_contains_var(dae, &on_signal) {
            return Some(var_name_to_c_alias(&on_signal));
        }
    }
    if var_name.ends_with(".conPI.On") {
        return synthesize_parent_signal_rhs(var_name, ".On", dae);
    }
    None
}

fn synthesize_parent_signal_rhs(var_name: &str, suffix: &str, dae: &Value) -> Option<String> {
    let mut prefix = var_name.strip_suffix(suffix)?;
    while let Some(parent) = parent_scope(prefix) {
        let candidate = format!("{parent}{suffix}");
        if candidate != var_name && dae_contains_var(dae, &candidate) {
            return Some(var_name_to_c_alias(&candidate));
        }
        prefix = parent;
    }
    None
}

fn dae_contains_var(dae: &Value, var_name: &str) -> bool {
    ["m", "z", "y", "u", "x", "p", "c"]
        .into_iter()
        .any(|map_name| has_var_in_dae_map(dae, map_name, var_name))
}

fn parse_indexed_suffix<'a>(name: &'a str, suffix: &str) -> Option<(&'a str, usize)> {
    let marker = format!("{suffix}[");
    let pos = name.find(&marker)?;
    let prefix = &name[..pos];
    let idx_str = name[pos + marker.len()..].strip_suffix(']')?;
    let idx = idx_str.parse::<usize>().ok()?;
    Some((prefix, idx))
}

fn indexed_alias(base_name: &str, idx: usize) -> String {
    format!("{}_{}", var_name_to_c_alias(base_name), idx)
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

fn current_error_expr(prefix: &str, dae: &Value) -> String {
    let setpoint = format!("{prefix}.setpoint");
    let measurement = format!("{prefix}.measurement");
    if has_var_in_dae_map(dae, "y", &setpoint) && has_var_in_dae_map(dae, "y", &measurement) {
        return format!(
            "({} - {})",
            var_name_to_c_alias(&setpoint),
            var_name_to_c_alias(&measurement)
        );
    }
    var_name_to_c_alias(&format!("{prefix}.e"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn internal_fluid_volume_species_synthesis_uses_dae_aliases() {
        let dae = Value::from_serialize(serde_json::json!({
            "f_x": [],
            "y": {
                "room.vol.medium.Xi[1]": {},
                "room.vol.ports[2].m_flow": {},
                "room.vol.ports[2].Xi_outflow[1]": {},
                "room.vol.ports_mXi_flow[2,1]": {},
                "room.vol.dynBal.ports[2].m_flow": {},
                "room.vol.dynBal.ports[2].Xi_outflow[1]": {},
                "room.vol.dynBal.ports_mXi_flow[2,1]": {}
            }
        }));
        let aliases = collect_c_aliases_from_dae(&dae);

        assert!(aliases.contains("room_vol_medium_Xi_1"));
        assert_eq!(
            parse_port_xi_outflow_alias("room_vol_ports_2_Xi_outflow_1"),
            Some(("room_vol", 2, 1))
        );
        assert_eq!(
            parse_ports_mxi_flow_alias("room_vol_ports_mXi_flow_2_1"),
            Some(("room_vol", 2, 1))
        );
        assert_eq!(
            synthesize_internal_fluid_volume_species_rhs(
                "room.vol.ports[2].Xi_outflow[1]",
                &aliases,
            ),
            Some("room_vol_medium_Xi_1".to_string())
        );
        assert_eq!(
            synthesize_internal_fluid_volume_species_rhs("room.vol.ports_mXi_flow[2,1]", &aliases,),
            Some("(room_vol_ports_2_m_flow * room_vol_ports_2_Xi_outflow_1)".to_string())
        );
    }
}
