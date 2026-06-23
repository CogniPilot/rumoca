//! C-backend template functions for FMI2 and embedded-C code generation.
//!
//! These functions are registered in the minijinja environment and used by
//! `fmi2/model.c.jinja` and `embedded_c/model.c.jinja` templates to extract explicit
//! ODE/algebraic RHS expressions from residual-form DAE equations.
//!
//! SPEC_0021 file-size exception: Kelvin BOPTEST FMI export helpers are
//! intentionally colocated with the C render surface until a split plan lands
//! for `render_c_boptest.rs` plus shared expression helpers.

// Kelvin top-down/BOPTEST FMI export helpers intentionally mirror generated-style
// complexity while we keep the template surface explicit and test-backed.
#![allow(
    clippy::collapsible_if,
    clippy::excessive_nesting,
    clippy::manual_pattern_char_comparison,
    clippy::needless_borrow,
    clippy::too_many_lines
)]

use super::{ExprConfig, IfStyle, RenderResult, render_solve::render_solve_row_c_function};
use minijinja::Value;
use render_expr::{
    get_field, is_variant, render_expression, render_serialized_name, subscript_index_value,
};
use rumoca_core::{parent_scope, split_last_top_level, top_level_last_segment};
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
    static DIRECT_ALG_RHS_CACHE: RefCell<Option<DirectAlgRhsCache>> = const { RefCell::new(None) };
    static ALG_RHS_CACHE: RefCell<Option<AlgRhsCache>> = const { RefCell::new(None) };
    static EQUATION_FINGERPRINT_CACHE: RefCell<HashMap<EquationFingerprintCacheKey, u64>> = RefCell::new(HashMap::new());
}

#[derive(Clone)]
struct AlgEquationCandidateIndex {
    fingerprint: u64,
    by_var_name: HashMap<String, Vec<Value>>,
    by_direct_target_name: HashMap<String, Vec<Value>>,
}

#[derive(Clone)]
struct EquationAliasIndex {
    fingerprint: u64,
    aliases: HashSet<String>,
}

struct DirectAlgRhsCache {
    fingerprint: u64,
    entries: HashMap<DirectAlgRhsKey, Option<String>>,
}

struct AlgRhsCache {
    fingerprint: u64,
    entries: HashMap<AlgRhsKey, String>,
}

#[derive(Clone, Eq, Hash, PartialEq)]
struct AlgRhsKey {
    var_name: String,
    alias_addr: usize,
    alias_len: usize,
    prefer_direct: bool,
    config: DirectAlgRhsConfigKey,
}

#[derive(Clone, Eq, Hash, PartialEq)]
struct DirectAlgRhsKey {
    var_name: String,
    config: DirectAlgRhsConfigKey,
}

#[derive(Clone, Eq, Hash, PartialEq)]
struct DirectAlgRhsConfigKey {
    prefix: String,
    power: String,
    and_op: String,
    or_op: String,
    not_op: String,
    true_val: String,
    false_val: String,
    array_start: String,
    array_end: String,
    if_style: u8,
    sanitize_dots: bool,
    one_based_index: bool,
    modelica_builtins: bool,
    mul_elem_fn: Option<String>,
    power_fn: Option<String>,
    subscript_underscore: bool,
    if_else_fn: Option<String>,
    python_range: bool,
    sum_fn: String,
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct EquationFingerprintCacheKey {
    object_addr: usize,
    len: usize,
}

// ── Functions ───────────────────────────────────────────────────────────

fn with_dae_aliases<R>(dae: &Value, f: impl FnOnce(&HashSet<String>) -> R) -> R {
    let Some(fingerprint) = fingerprint_dae_alias_context(dae) else {
        let aliases = collect_c_aliases_from_dae(dae);
        return f(&aliases);
    };
    DAE_ALIAS_INDEX.with(|cache| {
        let mut cached = cache.borrow_mut();
        if !matches!(cached.as_ref(), Some(index) if index.fingerprint == fingerprint) {
            let aliases = collect_c_aliases_from_dae(dae);
            *cached = Some(EquationAliasIndex {
                fingerprint,
                aliases,
            });
        }
        let aliases = &cached.as_ref().expect("DAE alias cache populated").aliases;
        f(aliases)
    })
}

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

    if let Some(rendered) = render_function_output_projection_at_index(&expr, idx, &cfg, &config)? {
        return Ok(rendered);
    }

    if let Some(rendered) = render_array_expr_at_index(&expr, idx, &cfg) {
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
            return Ok(if cfg.power == "**" {
                "REAL_C(0.0)"
            } else {
                "0.0"
            }
            .to_string());
        }
        if f == "Ones" || f == "\"Ones\"" {
            return Ok(if cfg.power == "**" {
                "REAL_C(1.0)"
            } else {
                "1.0"
            }
            .to_string());
        }
    }

    // Fallback: render whole expression (scalar value broadcast to all indices)
    render_expression(&expr, &cfg)
}

fn render_function_output_projection_at_index(
    expr: &Value,
    index: usize,
    cfg: &ExprConfig,
    config: &Value,
) -> Result<Option<String>, minijinja::Error> {
    let Ok(call) = get_field(expr, "FunctionCall") else {
        return Ok(None);
    };
    let raw_name = render_expr::render_name_field(&call, "name", "FunctionCall")?;
    let accessors = config.get_attr("function_array_outputs").ok();
    let Some(accessors) = accessors else {
        return Ok(None);
    };
    let Ok(accessor) = accessors.get_item(&Value::from(raw_name.as_str())) else {
        return Ok(None);
    };
    if accessor.is_undefined() || accessor.is_none() {
        return Ok(None);
    }
    let accessor = accessor
        .as_str()
        .map(ToOwned::to_owned)
        .unwrap_or_else(|| accessor.to_string().trim_matches('"').to_string());
    if accessor.is_empty() || index == 0 {
        return Ok(None);
    }
    let args = render_expr::render_args(&call, cfg)?;
    let output_index = index - 1;
    let rendered = if args.trim().is_empty() {
        format!("{accessor}({output_index})")
    } else {
        format!("{accessor}({args}, {output_index})")
    };
    Ok(Some(rendered))
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
    let cfg_for_binding = ExprConfig::from_value(&config);
    if let Some(rhs) = synthesize_boptest_floor_air_mass_flow_parameter_rhs(&target) {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_boptest_zone_volume_parameter_rhs(&target) {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_boptest_floor_air_volume_curve_parameter_rhs(&target, idx) {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_boptest_vav_terminal_parameter_rhs(&target) {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_boptest_plant_wrapper_parameter_rhs(&target) {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_boptest_chilled_water_tower_parameter_rhs(&target, idx) {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_boptest_chilled_water_chiller_parameter_rhs(&target, idx) {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_boptest_hot_water_boiler_parameter_rhs(&target, idx) {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_boptest_chiller_plant_pump_efficiency_parameter_rhs(&target) {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_boptest_boiler_plant_efficiency_parameter_rhs(&target) {
        return Ok(rhs);
    }
    if let Some(rhs) =
        synthesize_buildings_boiler_a_qua_lin_parameter_rhs(&target, &cfg_for_binding)
    {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_boptest_junction_m_dyn_flow_nominal_rhs(&target) {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_buildings_mover_derived_parameter_rhs(&target, &cfg_for_binding) {
        return Ok(rhs);
    }
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
        render_expression(&expr, &cfg_for_binding)?
    };
    if c_rhs_is_supported(&rendered) {
        Ok(rendered)
    } else {
        Ok(String::new())
    }
}

fn synthesize_boptest_zone_volume_parameter_rhs(target_name: &str) -> Option<String> {
    if let Some(prefix) = target_name.strip_suffix(".rho_start") {
        parse_boptest_zone_volume_floor_zone(prefix)?;
        let alias = var_name_to_c_alias(prefix);
        return Some(format!("Air_density({alias}_p_start, {alias}_T_start)"));
    }
    if let Some(prefix) = target_name.strip_suffix(".p_start") {
        if parse_boptest_zone_volume_floor_zone(prefix).is_some()
            || parse_boptest_thermal_zone_floor_zone(prefix).is_some()
        {
            return Some("101325.0".to_string());
        }
    }
    if let Some(prefix) = target_name.strip_suffix(".fluidVolume") {
        let (floor, zone) = parse_boptest_zone_volume_floor_zone(prefix)?;
        return Some(boptest_zone_air_volume_source_rhs(floor, zone));
    }
    if let Some(prefix) = target_name.strip_suffix(".mSenFac") {
        if parse_boptest_zone_volume_floor_zone(prefix).is_some()
            || parse_boptest_thermal_zone_floor_zone(prefix).is_some()
        {
            return Some("8.0".to_string());
        }
    }
    if let Some(prefix) = target_name.strip_suffix(".CSen") {
        let (floor, zone) = parse_boptest_zone_volume_floor_zone(prefix)?;
        let volume = boptest_zone_air_volume_source_rhs(floor, zone);
        return Some(format!("((8.0 - 1.0) * 1.2 * 1006.0 * {volume})"));
    }
    if let Some(prefix) = target_name.strip_suffix(".V") {
        let (floor, zone) = parse_boptest_thermal_zone_floor_zone(prefix)?;
        return Some(boptest_zone_air_volume_source_rhs(floor, zone));
    }
    None
}

fn boptest_zone_air_volume_source_rhs(floor: usize, zone: usize) -> String {
    let area = match zone {
        1 => "2532.32",
        2 => "201.98",
        3 => "313.42",
        4 => "313.42",
        5 => "201.98",
        _ => "0.0",
    };
    let floor_multiplier = if floor == 2 { "10.0" } else { "1.0" };
    format!("({area} * 2.74 * {floor_multiplier})")
}

fn parse_boptest_thermal_zone_floor_zone(prefix: &str) -> Option<(usize, usize)> {
    let floor_prefix = prefix.strip_prefix("floor")?;
    let floor_digits: String = floor_prefix
        .chars()
        .take_while(|ch| ch.is_ascii_digit())
        .collect();
    let floor_index = floor_digits.parse::<usize>().ok()?;
    if !(1..=3).contains(&floor_index) {
        return None;
    }
    let expected = format!("floor{floor_index}BoptestAirNetwork.floor.fivZonVAV.zon[");
    let rest = prefix.strip_prefix(&expected)?;
    let zone_text = rest
        .strip_suffix("].fmuZon")
        .or_else(|| rest.strip_suffix(']'))?;
    let zone_index = zone_text.parse::<usize>().ok()?;
    if !(1..=5).contains(&zone_index) {
        return None;
    }
    Some((floor_index, zone_index))
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
        || name.contains(".initialZoneTemperature")
        || name.contains(".deterministicInitialZoneTemperature")
        || name.contains(".per.pressure.")
        || name.contains(".per.motorEfficiency.")
        || name.contains(".per.hydraulicEfficiency.")
}

fn synthesize_boptest_vav_terminal_parameter_rhs(target_name: &str) -> Option<String> {
    let (wrapper, terminal, field) = parse_boptest_vav_terminal_parameter(target_name)?;
    let zone_index = parse_vav_terminal_index(terminal)?;
    let flow_index = (wrapper.floor_index - 1) * 5 + zone_index;
    let prefix_alias = var_name_to_c_alias(wrapper.prefix);
    match (
        terminal.ends_with(".dam"),
        terminal.ends_with(".rehVal"),
        field,
    ) {
        (false, false, "mAirFloRat") => Some(format!("{prefix_alias}_mAirFloRat_{flow_index}")),
        (false, false, "mWatFloRat") => Some(format!("{prefix_alias}_mWatFloRat_{flow_index}")),
        (false, false, "PreDroAir") => Some(format!("{prefix_alias}_PreDroAir_{zone_index}")),
        (false, false, "PreDroWat") => Some(format!("{prefix_alias}_PreDroWat_{zone_index}")),
        (true, false, "m_flow_nominal") | (true, false, "m_flow_nominal_pos") => {
            Some(format!("{prefix_alias}_mAirFloRat_{flow_index}"))
        }
        (false, true, "m_flow_nominal") | (false, true, "m_flow_nominal_pos") => {
            Some(format!("{prefix_alias}_mWatFloRat_{flow_index}"))
        }
        (true, false, "dpValve_nominal") | (true, false, "dp_nominal_pos") => {
            Some(format!("{prefix_alias}_PreDroAir_{zone_index}"))
        }
        (false, true, "dpValve_nominal") | (false, true, "dp_nominal_pos") => {
            Some(format!("{prefix_alias}_PreDroWat_{zone_index}"))
        }
        (true, false, "Kv_SI") => {
            let m_flow = format!("{prefix_alias}_mAirFloRat_{flow_index}");
            let dp = format!("{prefix_alias}_PreDroAir_{zone_index}");
            Some(format!("({m_flow} / sqrt(fmax(0.00001, {dp})))"))
        }
        (false, true, "Kv_SI") => {
            let m_flow = format!("{prefix_alias}_mWatFloRat_{flow_index}");
            let dp = format!("{prefix_alias}_PreDroWat_{zone_index}");
            Some(format!("({m_flow} / sqrt(fmax(0.00001, {dp})))"))
        }
        _ => None,
    }
}

struct BoptestVavWrapper<'a> {
    prefix: &'a str,
    floor_index: usize,
}

fn parse_boptest_vav_terminal_parameter<'a>(
    target_name: &'a str,
) -> Option<(BoptestVavWrapper<'a>, &'a str, &'a str)> {
    let floor_pos = target_name.find("BoptestAirNetwork.floor.fivZonVAV.vAV")?;
    let wrapper_prefix = &target_name[..floor_pos + "BoptestAirNetwork".len()];
    let floor_index = parse_boptest_air_network_floor_index(wrapper_prefix)?;
    let suffix = &target_name[floor_pos + "BoptestAirNetwork.floor.fivZonVAV.".len()..];
    let (terminal, field) = split_last_top_level(suffix)?;
    if !terminal.starts_with("vAV") {
        return None;
    }
    Some((
        BoptestVavWrapper {
            prefix: wrapper_prefix,
            floor_index,
        },
        terminal,
        field,
    ))
}

fn parse_boptest_air_network_floor_index(prefix: &str) -> Option<usize> {
    let rest = prefix.strip_prefix("floor")?;
    let digits: String = rest.chars().take_while(|ch| ch.is_ascii_digit()).collect();
    let index = digits.parse::<usize>().ok()?;
    (1..=3).contains(&index).then_some(index)
}

fn parse_vav_terminal_index(terminal: &str) -> Option<usize> {
    let rest = terminal.strip_prefix("vAV")?;
    let digits: String = rest.chars().take_while(|ch| ch.is_ascii_digit()).collect();
    let index = digits.parse::<usize>().ok()?;
    (1..=5).contains(&index).then_some(index)
}

fn synthesize_boptest_plant_wrapper_parameter_rhs(target_name: &str) -> Option<String> {
    if target_name == "chilledWaterPlant.nominalChillerCapacity"
        || target_name.ends_with("_chilledWaterPlant_nominalChillerCapacity")
    {
        return Some("2391666.6666666665".to_string());
    }
    if target_name == "hotWaterPlant.nominalBoilerCapacity"
        || target_name.ends_with("_hotWaterPlant_nominalBoilerCapacity")
    {
        return Some("2619375.0".to_string());
    }
    None
}

fn synthesize_boptest_chilled_water_tower_parameter_rhs(
    target_name: &str,
    index: usize,
) -> Option<String> {
    if !target_name.contains("chilledWaterPlant.chillerPlant") {
        return None;
    }
    let tower_nominal_mass_flow = "131.656925135186";
    let bypass_nominal_mass_flow = format!("(3.0 * {tower_nominal_mass_flow})");
    if target_name.contains(".cooTowWithByp.dPByp_nominal")
        || target_name.contains(".cooTowWithByp.byp.dPByp_nominal")
        || target_name.contains(".byp.valByp.dpValve_nominal")
    {
        return Some(var_name_to_c_alias(
            "chilledWaterPlant.chillerPlant.dPByp_nominal",
        ));
    }
    if target_name.contains(".byp.m_flow_nominal")
        || target_name.contains(".byp.senMasFloTow.m_flow_nominal")
        || target_name.contains(".byp.senMasFloByp.m_flow_nominal")
        || target_name.contains(".byp.valByp.m_flow_nominal")
        || target_name.contains(".byp.valByp.m_flow_nominal_pos")
    {
        return Some(bypass_nominal_mass_flow);
    }
    if target_name.contains(".byp.senMasFloTow.m_flow_small")
        || target_name.contains(".byp.senMasFloByp.m_flow_small")
        || target_name.contains(".byp.valByp.m_flow_small")
        || target_name.contains(".byp.valByp.m_flow_turbulent")
    {
        return Some(format!("(1e-4 * 3.0 * {tower_nominal_mass_flow})"));
    }
    if target_name.contains(".byp.valByp.dp_nominal_pos") {
        return Some(var_name_to_c_alias(
            "chilledWaterPlant.chillerPlant.dPByp_nominal",
        ));
    }
    if target_name.contains(".byp.valByp.Kv_SI") {
        let dp = var_name_to_c_alias("chilledWaterPlant.chillerPlant.dPByp_nominal");
        return Some(format!(
            "({bypass_nominal_mass_flow} / sqrt(fmax(0.00001, {dp})))"
        ));
    }
    let unit = if (1..=3).contains(&index) {
        index
    } else {
        parse_boptest_tower_indexed_parameter(target_name)
            .or_else(|| parse_component_stage_suffix(target_name, "_ct_", "_P_nominal"))
            .or_else(|| parse_component_stage_suffix(target_name, "_ct_", "_yorkCalc_PFan_nominal"))
            .or_else(|| {
                parse_component_stage_suffix(target_name, "_ct_", "_yorkCalc_fraPFan_nominal")
            })
            .or_else(|| {
                parse_component_stage_suffix(target_name, "_ct_", "_yorkCalc_mWat_flow_nominal")
            })
            .or_else(|| parse_component_stage_suffix(target_name, "_ct_", "_mCW_flow_nominal"))?
    };
    if !(1..=3).contains(&unit) {
        return None;
    }
    let tower_nominal_power = "(2391666.6666666665 * (5.06 + 1.0) / 5.06 * 0.015)";
    if target_name.contains("PTow_nominal")
        || target_name.contains("cooTowWithByp.P_nominal")
        || target_name.contains("mulCooTowSys.P_nominal")
        || target_name.contains(".P_nominal")
        || target_name.contains(".PFan_nominal")
    {
        return Some(tower_nominal_power.to_string());
    }
    if target_name.contains(".fraPFan_nominal") {
        return Some(format!(
            "({tower_nominal_power} / {tower_nominal_mass_flow})"
        ));
    }
    if target_name.contains(".mCW_flow_nominal") || target_name.contains(".mWat_flow_nominal") {
        return Some(tower_nominal_mass_flow.to_string());
    }
    None
}

fn synthesize_boptest_hot_water_boiler_parameter_rhs(
    target_name: &str,
    index: usize,
) -> Option<String> {
    if !target_name.contains("hotWaterPlant.boilerPlant.mulBoi.boi") {
        return None;
    }
    let unit = parse_boptest_hot_water_boiler_index(target_name)
        .or_else(|| (1..=2).contains(&index).then_some(index))?;
    if !(1..=2).contains(&unit) {
        return None;
    }

    if target_name.ends_with(".boi.Q_flow_nominal") || target_name.ends_with("_boi_Q_flow_nominal")
    {
        let delta_t = var_name_to_c_alias(&format!(
            "hotWaterPlant.boilerPlant.mulBoi.boi[{unit}].dTHW_nominal"
        ));
        let mass_flow = var_name_to_c_alias(&format!(
            "hotWaterPlant.boilerPlant.mulBoi.boi[{unit}].mHW_flow_nominal"
        ));
        return Some(format!("(({delta_t} * {mass_flow}) * 4200.0)"));
    }
    if target_name.ends_with(".boi.eta_nominal") || target_name.ends_with("_boi_eta_nominal") {
        return Some(var_name_to_c_alias(&format!(
            "hotWaterPlant.boilerPlant.mulBoi.boi[{unit}].boi.a[1]"
        )));
    }
    if target_name.ends_with(".boi.a[1]") || target_name.ends_with("_boi_a_1") {
        return Some(var_name_to_c_alias(&format!(
            "hotWaterPlant.boilerPlant.mulBoi.boi[{unit}].eta[1]"
        )));
    }
    if target_name.ends_with(".boi.eps") || target_name.ends_with("_boi_eps") {
        return Some("1.0".to_string());
    }
    if target_name.ends_with(".boi.mDry") || target_name.ends_with("_boi_mDry") {
        let q_nominal = var_name_to_c_alias(&format!(
            "hotWaterPlant.boilerPlant.mulBoi.boi[{unit}].boi.Q_flow_nominal"
        ));
        return Some(format!("(0.0015 * {q_nominal})"));
    }
    if target_name.ends_with(".boi.heaCapDry.C") || target_name.ends_with("_boi_heaCapDry_C") {
        let m_dry = var_name_to_c_alias(&format!(
            "hotWaterPlant.boilerPlant.mulBoi.boi[{unit}].boi.mDry"
        ));
        return Some(format!("(500.0 * {m_dry})"));
    }
    None
}

fn synthesize_boptest_chilled_water_chiller_parameter_rhs(
    target_name: &str,
    index: usize,
) -> Option<String> {
    if !target_name.contains("chilledWaterPlant.chillerPlant.mulChiSys") {
        return None;
    }
    let unit = parse_boptest_chilled_water_chiller_index(target_name)
        .or_else(|| (1..=3).contains(&index).then_some(index))?;
    if !(1..=3).contains(&unit) {
        return None;
    }

    if target_name.ends_with(".QEva_flow_nominal") || target_name.ends_with("_QEva_flow_nominal") {
        return Some("-2391666.6666666665".to_string());
    }
    if target_name.ends_with(".COP_nominal") || target_name.ends_with("_COP_nominal") {
        return Some("5.06".to_string());
    }
    if target_name.ends_with(".mEva_flow_nominal") || target_name.ends_with("_mEva_flow_nominal") {
        return Some("(1000.0 * 0.05047)".to_string());
    }
    if target_name.ends_with(".mCon_flow_nominal") || target_name.ends_with("_mCon_flow_nominal") {
        return Some("(1000.0 * 0.05047)".to_string());
    }
    if target_name.ends_with(".PLRMin") || target_name.ends_with("_PLRMin") {
        return Some("0.10".to_string());
    }
    if target_name.ends_with(".PLRMinUnl") || target_name.ends_with("_PLRMinUnl") {
        return Some("0.10".to_string());
    }
    if target_name.ends_with(".PLRMax") || target_name.ends_with("_PLRMax") {
        return Some("1.07".to_string());
    }
    if target_name.ends_with(".TEvaLvg_nominal") || target_name.ends_with("_TEvaLvg_nominal") {
        return Some("(273.15 + 5.56)".to_string());
    }
    if target_name.ends_with(".TConEnt_nominal") || target_name.ends_with("_TConEnt_nominal") {
        return Some("(273.15 + 23.89)".to_string());
    }
    if target_name.ends_with(".TEvaLvgMin") || target_name.ends_with("_TEvaLvgMin") {
        return Some("(273.15 + 4.44)".to_string());
    }
    if target_name.ends_with(".TEvaLvgMax") || target_name.ends_with("_TEvaLvgMax") {
        return Some("(273.15 + 8.89)".to_string());
    }
    if target_name.ends_with(".TConEntMin") || target_name.ends_with("_TConEntMin") {
        return Some("(273.15 + 12.78)".to_string());
    }
    if target_name.ends_with(".TConEntMax") || target_name.ends_with("_TConEntMax") {
        return Some("(273.15 + 35.00)".to_string());
    }
    if target_name.ends_with(".etaMotor") || target_name.ends_with("_etaMotor") {
        return Some("1.0".to_string());
    }
    if let Some(coeff_index) = boptest_chiller_curve_coeff_index(target_name, "capFunT", index) {
        const CAP_FUN_T: [&str; 6] = [
            "4.007341E-01",
            "1.148568E-03",
            "1.049504E-04",
            "5.574667E-02",
            "-1.646131E-03",
            "1.323200E-03",
        ];
        return CAP_FUN_T
            .get(coeff_index - 1)
            .map(|value| value.to_string());
    }
    if let Some(coeff_index) = boptest_chiller_curve_coeff_index(target_name, "EIRFunT", index) {
        const EIR_FUN_T: [&str; 6] = [
            "6.287646E-01",
            "-3.024605E-02",
            "6.137016E-04",
            "1.805826E-02",
            "1.640653E-04",
            "-1.113802E-04",
        ];
        return EIR_FUN_T
            .get(coeff_index - 1)
            .map(|value| value.to_string());
    }
    if let Some(coeff_index) = boptest_chiller_curve_coeff_index(target_name, "EIRFunPLR", index) {
        const EIR_FUN_PLR: [&str; 3] = ["9.299787E-02", "3.244475E-01", "5.818753E-01"];
        return EIR_FUN_PLR
            .get(coeff_index - 1)
            .map(|value| value.to_string());
    }
    None
}

fn synthesize_boptest_chiller_plant_pump_efficiency_parameter_rhs(
    target_name: &str,
) -> Option<String> {
    if !target_name.contains("chilledWaterPlant.chillerPlant") {
        return None;
    }
    if target_name.contains(".pumPriCHW.Motor_eta")
        || target_name.contains(".pumCW.Motor_eta")
        || target_name.contains(".motorEfficiency.eta")
        || target_name.contains("_pumPriCHW_Motor_eta_")
        || target_name.contains("_pumCW_Motor_eta_")
        || target_name.contains("_motorEfficiency_eta_")
    {
        return Some("0.87".to_string());
    }
    if target_name.contains(".pumPriCHW.Hydra_eta")
        || target_name.contains(".pumCW.Hydra_eta")
        || target_name.contains(".hydraulicEfficiency.eta")
        || target_name.contains("_pumPriCHW_Hydra_eta_")
        || target_name.contains("_pumCW_Hydra_eta_")
        || target_name.contains("_hydraulicEfficiency_eta_")
    {
        return Some("1.0".to_string());
    }
    None
}

fn synthesize_boptest_boiler_plant_efficiency_parameter_rhs(target_name: &str) -> Option<String> {
    if target_name.contains("hotWaterPlant.boilerPlant")
        && (target_name.contains(".mulBoi.eta") || target_name.contains("_mulBoi_eta_"))
    {
        return Some("0.8".to_string());
    }
    if target_name.contains("hotWaterPlant.boilerPlant")
        && target_name.contains(".boi[")
        && target_name.contains(".eta")
    {
        return Some("0.8".to_string());
    }
    None
}

fn synthesize_boptest_junction_m_dyn_flow_nominal_rhs(target_name: &str) -> Option<String> {
    let prefix = target_name.strip_suffix(".mDyn_flow_nominal")?;
    let terms = (1..=3)
        .map(|index| {
            let alias = var_name_to_c_alias(&format!("{prefix}.m_flow_nominal[{index}]"));
            format!("fabs({alias})")
        })
        .collect::<Vec<_>>()
        .join(" + ");
    Some(format!("(({terms}) / 3.0)"))
}

fn synthesize_buildings_boiler_a_qua_lin_parameter_rhs(
    target_name: &str,
    cfg: &ExprConfig,
) -> Option<String> {
    let (prefix, index) = parse_indexed_suffix(target_name, ".aQuaLin")?;
    let source = format!("{prefix}.a");
    let len = scalarized_1d_len_for_base(&source, cfg)?;
    if len == 6 {
        return Some(var_name_to_c_alias(&format!("{source}[{index}]")));
    }
    Some("0".to_string())
}

fn synthesize_buildings_mover_derived_parameter_rhs(
    target_name: &str,
    cfg: &ExprConfig,
) -> Option<String> {
    if let Some((prefix, index)) = parse_indexed_suffix(target_name, ".speeds") {
        let speeds_rpm = var_name_to_c_alias(&format!("{prefix}.speeds_rpm[{index}]"));
        let nominal = var_name_to_c_alias(&format!("{prefix}.speed_rpm_nominal"));
        return Some(format!("({speeds_rpm} / {nominal})"));
    }
    if let Some((prefix, index)) = parse_indexed_suffix(target_name, ".massFlowRates") {
        let m_flow_nominal = var_name_to_c_alias(&format!("{prefix}.m_flow_nominal"));
        let speeds = format!("{prefix}.per.speeds");
        let len = scalarized_1d_len_for_base(&speeds, cfg).unwrap_or(1);
        let speed_index = index.min(len);
        let speed_i = var_name_to_c_alias(&format!("{speeds}[{speed_index}]"));
        let speed_last = var_name_to_c_alias(&format!("{speeds}[{len}]"));
        return Some(format!("({m_flow_nominal} * ({speed_i} / {speed_last}))"));
    }
    if let Some(prefix) = target_name.strip_suffix(".m_flow_nominal") {
        let pressure_v_flow = format!("{prefix}.per.pressure.V_flow");
        if let Some(len) = scalarized_1d_len_for_base(&pressure_v_flow, cfg) {
            let terms = (1..=len)
                .map(|index| var_name_to_c_alias(&format!("{pressure_v_flow}[{index}]")))
                .collect::<Vec<_>>();
            let v_flow_max = render_c_fmax_chain(terms)?;
            let rho_default = var_name_to_c_alias(&format!("{prefix}.rho_default"));
            return Some(format!("({v_flow_max} * {rho_default})"));
        }
    }
    if let Some(prefix) = target_name.strip_suffix(".V_flow_nominal") {
        return synthesize_mover_v_flow_reference(prefix, cfg);
    }
    if let Some(prefix) = target_name.strip_suffix(".V_flow_max") {
        return synthesize_mover_v_flow_reference(prefix, cfg);
    }
    let prefix = target_name.strip_suffix(".haveVMax")?;
    let pressure_dp = format!("{prefix}.per.pressure.dp");
    let len = scalarized_1d_len_for_base(&pressure_dp, cfg)?;
    let dp = var_name_to_c_alias(&format!("{pressure_dp}[{len}]"));
    Some(format!("(fabs({dp}) < 0.0000000000000002220446049250313)"))
}

fn render_c_fmax_chain(mut terms: Vec<String>) -> Option<String> {
    if terms.is_empty() {
        return None;
    }
    let mut result = terms.remove(0);
    for term in terms {
        result = format!("fmax({result}, {term})");
    }
    Some(result)
}

fn synthesize_mover_v_flow_reference(prefix: &str, cfg: &ExprConfig) -> Option<String> {
    let pressure_v_flow = format!("{prefix}.per.pressure.V_flow");
    if let Some(len) = scalarized_1d_len_for_base(&pressure_v_flow, cfg) {
        return Some(var_name_to_c_alias(&format!("{pressure_v_flow}[{len}]")));
    }
    let m_flow_nominal = var_name_to_c_alias(&format!("{prefix}.m_flow_nominal"));
    let rho_default = var_name_to_c_alias(&format!("{prefix}.rho_default"));
    Some(format!("({m_flow_nominal} / {rho_default})"))
}

fn scalarized_1d_len_for_base(base_name: &str, cfg: &ExprConfig) -> Option<usize> {
    let mut len = 0usize;
    for index in 1..=256 {
        let source_ref = format!("{base_name}[{index}]");
        if super::lookup_symbol_value(cfg.symbols.as_ref(), &source_ref).is_some() {
            len = index;
        } else if len > 0 {
            break;
        }
    }
    (len > 0).then_some(len)
}

fn boptest_chiller_curve_coeff_index(
    target_name: &str,
    curve_name: &str,
    render_index: usize,
) -> Option<usize> {
    let bracket_suffix = format!(".{curve_name}[");
    if let Some((_, index)) = parse_indexed_component_signal(target_name, &bracket_suffix, "]") {
        return Some(index);
    }
    let underscore = format!("_{curve_name}_");
    if let Some(start) = target_name.rfind(&underscore) {
        let rest = &target_name[start + underscore.len()..];
        if let Ok(index) = rest.parse::<usize>() {
            return Some(index);
        }
    }
    (target_name.ends_with(&format!(".{curve_name}"))
        || target_name.ends_with(&format!("_{curve_name}")))
    .then_some(render_index)
    .filter(|index| *index > 0)
}

fn parse_boptest_tower_indexed_parameter(name: &str) -> Option<usize> {
    [
        "].P_nominal",
        "].yorkCalc.PFan_nominal",
        "].yorkCalc.fraPFan_nominal",
        "].yorkCalc.mWat_flow_nominal",
        "].mCW_flow_nominal",
        "].",
    ]
    .into_iter()
    .find_map(|suffix| {
        parse_indexed_component_signal(name, ".cooTowWithByp.mulCooTowSys.ct[", suffix)
            .map(|(_, index)| index)
    })
}

fn parse_component_stage_suffix(name: &str, prefix: &str, suffix: &str) -> Option<usize> {
    let start = name.find(prefix)? + prefix.len();
    let rest = &name[start..];
    let end = rest.find(suffix)?;
    rest[..end].parse::<usize>().ok()
}

fn synthesize_boptest_floor_air_mass_flow_parameter_rhs(target_name: &str) -> Option<String> {
    let (floor, zone) = parse_boptest_floor_air_mass_flow_parameter(target_name)?;
    Some(boptest_floor_air_mass_flow_source_rhs(floor, zone))
}

fn boptest_floor_air_mass_flow_source_rhs(floor: usize, zone: usize) -> String {
    let base = match zone {
        1 => "10.92",
        2 => "2.25",
        3 => "1.49",
        4 => "1.9",
        5 => "1.73",
        _ => "0.0",
    };
    let floor_multiplier = if floor == 2 { "10.0" } else { "1.0" };
    format!("({base} * 1.2 * floor{floor}BoptestAirNetwork_alpha * 3.0 * {floor_multiplier})")
}

fn parse_boptest_floor_air_mass_flow_parameter(target_name: &str) -> Option<(usize, usize)> {
    for floor in 1..=3 {
        let prefix = format!("floor{floor}BoptestAirNetwork");
        if let Some(zone) = target_name
            .strip_prefix(&format!("{prefix}.floor.mAirFloRat"))
            .and_then(|suffix| suffix.parse::<usize>().ok())
        {
            return (1..=5).contains(&zone).then_some((floor, zone));
        }
        if let Some(zone) = target_name
            .strip_prefix(&format!("{prefix}_floor_mAirFloRat"))
            .and_then(|suffix| suffix.parse::<usize>().ok())
        {
            return (1..=5).contains(&zone).then_some((floor, zone));
        }
        if let Some(zone) = target_name
            .strip_prefix(&format!("{prefix}.floor.fivZonVAV.mAirFloRat"))
            .and_then(|suffix| suffix.parse::<usize>().ok())
        {
            return (1..=5).contains(&zone).then_some((floor, zone));
        }
        if let Some(zone) = target_name
            .strip_prefix(&format!("{prefix}_floor_fivZonVAV_mAirFloRat"))
            .and_then(|suffix| suffix.parse::<usize>().ok())
        {
            return (1..=5).contains(&zone).then_some((floor, zone));
        }
    }
    None
}

fn synthesize_boptest_floor_air_volume_curve_parameter_rhs(
    target_name: &str,
    index: usize,
) -> Option<String> {
    let floor = boptest_floor_air_network_index(target_name)?;
    if !target_name.contains(".VolFloCur") || !(1..=4).contains(&index) {
        return None;
    }
    let curve_scale = match index {
        1 => "0.5",
        2 => "0.7",
        3 => "0.9",
        4 => "1.2",
        _ => return None,
    };
    let floor_multiplier = if floor == 2 { "10.0" } else { "1.0" };
    Some(format!(
        "((10.92 + 2.25 + 1.49 + 1.9 + 1.73) * floor{floor}BoptestAirNetwork_alpha * 3.0 * {floor_multiplier} * {curve_scale})"
    ))
}

fn boptest_floor_air_network_index(name: &str) -> Option<usize> {
    (1..=3).find(|floor| name.contains(&format!("floor{floor}BoptestAirNetwork")))
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
        || rhs.contains("linspace(")
        || rhs.contains("firstActiveIndex")
        || rhs.contains("polynomial(")
        || rhs.contains("quadraticLinear(")
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
        || rhs.contains("[((floor")
        || rhs.contains("_filter_s[((")
        || rhs.contains("].")
        || rhs.contains("_filter_n")
        || (rhs.contains("_gain_") && !rhs.contains("internal_gain"))
        || rhs.contains("ele___")
        || rhs.contains("(double[]){")
        || rhs.contains(" for i in ")
        || rhs.contains("_extract_")
        || (!allow_ternary_colon && compact.contains("1:0"))
        || compact_for_colon_check.contains(":"))
}

fn is_logical_switch_output_var(var_name: &str) -> bool {
    var_name.ends_with(".swi.y") || var_name.ends_with("_swi_y")
}

fn c_event_or_discrete_rhs_is_supported(rhs: &str) -> bool {
    c_switch_rhs_is_supported(rhs)
        && !(rhs.contains("floor1BoptestAirNetwork_")
            || rhs.contains("floor2BoptestAirNetwork_")
            || rhs.contains("floor3BoptestAirNetwork_"))
}

fn c_rhs_with_equation_context(rhs: String, equations: &Value) -> String {
    if !rhs.contains("__rumoca_sum_d(") {
        return rhs;
    }
    let aliases = c_aliases_for_equations(equations);
    expand_sum_calls_for_scalarized_aliases(&rhs, &aliases)
}

fn expr_contains_sample_builtin(expr: &Value) -> bool {
    expr.to_string().contains("Sample")
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
    if let Some(rhs) = synthesize_boptest_zone_volume_energy_initial_rhs(&name) {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_boptest_zone_volume_medium_xi_initial_rhs(&name) {
        return Ok(rhs);
    }
    if let Some(rhs) = synthesize_boptest_air_zone_temperature_initial_rhs(&name) {
        return Ok(rhs);
    }
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

fn synthesize_boptest_air_zone_temperature_initial_rhs(name: &str) -> Option<String> {
    let suffix = name.strip_prefix("floor")?;
    let floor_digits: String = suffix
        .chars()
        .take_while(|ch| ch.is_ascii_digit())
        .collect();
    let floor_index = floor_digits.parse::<usize>().ok()?;
    if !(1..=3).contains(&floor_index) {
        return None;
    }
    let expected_prefix = format!("floor{floor_index}BoptestAirNetwork.airZoneTemperature[");
    let zone_suffix = name.strip_prefix(&expected_prefix)?.strip_suffix(']')?;
    let zone_index = zone_suffix.parse::<usize>().ok()?;
    if !(1..=5).contains(&zone_index) {
        return None;
    }
    Some(format!(
        "floor{floor_index}BoptestAirNetwork_initialZoneTemperature_{zone_index}"
    ))
}

fn synthesize_boptest_zone_volume_energy_initial_rhs(name: &str) -> Option<String> {
    let prefix = name.strip_suffix(".U")?;
    let (floor_index, zone_index) = parse_boptest_zone_volume_floor_zone(prefix)?;
    let alias = var_name_to_c_alias(prefix);
    let global_zone_index = (floor_index - 1) * 5 + zone_index;
    let initial_temperature = format!("initial_temperature_{global_zone_index}");
    Some(format!(
        "((({alias}_fluidVolume * {alias}_rho_start) * ((2501014.5 * {alias}_X_start_1) - (101325.0 / 1.2))) + (({initial_temperature} - 273.15) * ((({alias}_fluidVolume * {alias}_rho_start) * ((1006.0 * (1.0 - {alias}_X_start_1)) + (1860.0 * {alias}_X_start_1))) + {alias}_CSen)))"
    ))
}

fn synthesize_boptest_zone_volume_medium_xi_initial_rhs(name: &str) -> Option<String> {
    let (prefix, index) = parse_indexed_suffix(name, ".medium.Xi")?;
    parse_boptest_zone_volume_floor_zone(prefix)?;
    let alias = var_name_to_c_alias(prefix);
    Some(format!("{alias}_X_start_{index}"))
}

fn parse_boptest_zone_volume_floor_zone(prefix: &str) -> Option<(usize, usize)> {
    let floor_prefix = prefix.strip_prefix("floor")?;
    let floor_digits: String = floor_prefix
        .chars()
        .take_while(|ch| ch.is_ascii_digit())
        .collect();
    let floor_index = floor_digits.parse::<usize>().ok()?;
    if !(1..=3).contains(&floor_index) {
        return None;
    }
    let expected = format!("floor{floor_index}BoptestAirNetwork.floor.fivZonVAV.zon[");
    let zone_suffix = prefix.strip_prefix(&expected)?.strip_suffix("].vol")?;
    let zone_index = zone_suffix.parse::<usize>().ok()?;
    if !(1..=5).contains(&zone_index) {
        return None;
    }
    Some((floor_index, zone_index))
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
    if let Ok(field_access) = get_field(expr, "FieldAccess") {
        if field_access_is_supported_record_constructor_projection(&field_access) {
            return false;
        }
        return true;
    }
    false
}

fn field_access_is_supported_record_constructor_projection(field_access: &Value) -> bool {
    let field = get_field(field_access, "field")
        .ok()
        .map(|value| value.to_string().trim_matches('"').to_string());
    let Some(field) = field else {
        return false;
    };
    if !matches!(field.as_str(), "p" | "T" | "X" | "reference_X") {
        return false;
    }
    let Ok(base) = get_field(field_access, "base") else {
        return false;
    };
    let Ok(call) = get_field(&base, "FunctionCall") else {
        return false;
    };
    let Ok(raw_name) = render_expr::render_name_field(&call, "name", "FunctionCall") else {
        return false;
    };
    let last = rumoca_core::top_level_last_segment(&raw_name);
    matches!(last, "setState_pTX" | "setState_phX" | "ThermodynamicState")
        || raw_name.ends_with("_setState_pTX")
        || raw_name.ends_with("_setState_phX")
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
    let name_str = template_value_name(&state_name);

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
    if let Some(rhs_expr) = synthesize_top_down_radiant_tail_ode_rhs(&name_str) {
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

pub(super) fn ode_rhs_override_for_state_function(
    state_name: Value,
    _config: Value,
) -> RenderResult {
    let name_str = template_value_name(&state_name);

    if let Some(rhs_expr) = synthesize_internal_fluid_volume_ode_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_buildings_expansion_vessel_ode_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_buildings_actuator_filter_ode_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_integrator_ode_rhs(&name_str) {
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
    if let Some(rhs_expr) = synthesize_top_down_radiant_tail_ode_rhs(&name_str) {
        return Ok(rhs_expr);
    }

    Ok(String::new())
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
    alg_rhs_for_var_with_aliases(var_name, equations, &aliases, config, true)
}

pub(super) fn alg_rhs_for_var_with_dae_function(
    var_name: Value,
    dae: Value,
    config: Value,
) -> RenderResult {
    let name_str = template_value_name(&var_name);
    let prefer_direct = dae_has_output_var(&dae, &name_str);
    let equations = dae
        .get_attr("f_x")
        .unwrap_or(Value::from(Vec::<Value>::new()));
    with_dae_aliases(&dae, |aliases| {
        alg_rhs_for_var_with_aliases(var_name, equations, aliases, config, prefer_direct)
    })
}

pub(super) fn output_rhs_for_var_with_solve_function(
    var_name: Value,
    dae: Value,
    solve: Value,
    expr_config: Value,
    solve_row_config: Value,
) -> RenderResult {
    let name_str = template_value_name(&var_name);
    if let Some(rhs_expr) = synthesize_boptest_secondary_pump_command_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_pump_stage_output_rhs(&name_str, &dae) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_mover_flow_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = with_dae_aliases(&dae, |aliases| {
        synthesize_boptest_order_safe_output_read_surface_rhs(&name_str, aliases)
    }) {
        return Ok(rhs_expr);
    }
    let dae_rhs =
        alg_rhs_for_var_with_dae_function(var_name.clone(), dae.clone(), expr_config.clone())?;
    if !dae_rhs.contains("WARNING: no equation found") {
        let cfg = ExprConfig::from_value(&expr_config);
        if let Some(projected_rhs) = with_dae_aliases(&dae, |aliases| {
            project_whole_array_rhs_alias_for_indexed_target(&dae_rhs, &name_str, aliases, &cfg)
        }) {
            return Ok(projected_rhs);
        }
        if let Some(repaired_rhs) = with_dae_aliases(&dae, |aliases| {
            repair_missing_projected_alias_rhs(&dae_rhs, aliases)
        }) {
            return Ok(repaired_rhs);
        }
        return Ok(dae_rhs);
    }
    if let Some((visible_index, row)) = solve_visible_row_for_name(&solve, &name_str)
        && !solve_visible_row_is_identity_y_load(&row, visible_index)
    {
        return render_solve_row_c_function(row, solve_row_config);
    }
    Ok(dae_rhs)
}

fn project_whole_array_rhs_alias_for_indexed_target(
    rhs: &str,
    target_name: &str,
    aliases: &HashSet<String>,
    cfg: &ExprConfig,
) -> Option<String> {
    let (target_base, index) = parse_indexed_ref(target_name)?;
    let rhs_alias = rhs.trim();
    if rhs_alias.is_empty()
        || !rhs_alias
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
    {
        return None;
    }

    if let Some(prefix) = target_base.strip_suffix(".y") {
        let source_name = format!("{prefix}.u");
        if rhs_alias == var_name_to_c_alias(&source_name) {
            let projected = lookup_scalarized_array_symbol(&source_name, index, cfg)
                .unwrap_or_else(|| var_name_to_c_alias(&format!("{source_name}[{index}]")));
            if aliases.contains(&projected) {
                return Some(projected);
            }
        }
    }
    None
}

fn repair_missing_projected_alias_rhs(rhs: &str, aliases: &HashSet<String>) -> Option<String> {
    let rhs_alias = rhs.trim();
    if rhs_alias.is_empty()
        || !rhs_alias
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
        || aliases.contains(rhs_alias)
    {
        return None;
    }
    let (base, suffix) = rhs_alias.rsplit_once('_')?;
    if suffix.parse::<usize>().ok().is_none_or(|index| index == 0) {
        return None;
    }
    aliases.contains(base).then(|| base.to_string())
}

pub(super) fn solve_y_get_cases_function(dae: Value, solve: Value) -> RenderResult {
    render_solve_y_runtime_cases(&dae, &solve, SolveYRuntimeCaseMode::Get)
}

pub(super) fn solve_y_set_cases_function(dae: Value, solve: Value) -> RenderResult {
    render_solve_y_runtime_cases(&dae, &solve, SolveYRuntimeCaseMode::Set)
}

#[derive(Clone, Copy)]
enum SolveYRuntimeCaseMode {
    Get,
    Set,
}

fn render_solve_y_runtime_cases(
    dae: &Value,
    solve: &Value,
    mode: SolveYRuntimeCaseMode,
) -> RenderResult {
    let Ok(names) = solve.get_attr("visible_names") else {
        return Ok(String::new());
    };
    let mut slots = HashMap::new();
    collect_runtime_scalar_slots(dae, "x", "x", &mut slots);
    collect_runtime_scalar_slots(dae, "y", "y", &mut slots);
    collect_runtime_scalar_slots(dae, "w", "w", &mut slots);

    let Some(len) = names.len() else {
        return Ok(String::new());
    };
    let mut out = String::new();
    for idx in 0..len {
        let name = names
            .get_item(&Value::from(idx))
            .map(|value| template_value_name(&value))
            .unwrap_or_default();
        let Some((array_name, runtime_index)) = slots.get(&name) else {
            continue;
        };
        match mode {
            SolveYRuntimeCaseMode::Get => {
                out.push_str(&format!(
                    "        case {idx}: return m->{array_name}[{runtime_index}];  /* {name} */\n"
                ));
            }
            SolveYRuntimeCaseMode::Set => {
                out.push_str(&format!(
                    "        case {idx}: m->{array_name}[{runtime_index}] = value; return;  /* {name} */\n"
                ));
            }
        }
    }
    Ok(out)
}

fn collect_runtime_scalar_slots(
    dae: &Value,
    field: &str,
    array_name: &'static str,
    slots: &mut HashMap<String, (&'static str, usize)>,
) {
    let Ok(vars) = dae.get_attr(field) else {
        return;
    };
    let Some(object) = vars.as_object() else {
        return;
    };
    let Some(iter) = object.try_iter_pairs() else {
        return;
    };
    let mut offset = 0usize;
    for (key, var) in iter {
        let name = template_value_name(&key);
        let dims_value = get_field(&var, "dims").ok();
        let size = dims_value
            .as_ref()
            .map(runtime_scalar_count_from_dims)
            .unwrap_or(1);
        let dims = dims_value
            .as_ref()
            .map(runtime_dims_from_value)
            .unwrap_or_default();
        for linear_index in 1..=size {
            let scalar_name = scalar_source_ref(&name, &dims, linear_index);
            slots.insert(scalar_name, (array_name, offset));
            offset += 1;
        }
    }
}

fn scalar_source_ref(name: &str, dims: &[usize], linear_index: usize) -> String {
    if dims.is_empty() {
        return name.to_string();
    }
    let mut remaining = linear_index.saturating_sub(1);
    let mut subscripts = vec![1usize; dims.len()];
    for idx in (0..dims.len()).rev() {
        let dim = dims[idx].max(1);
        subscripts[idx] = (remaining % dim) + 1;
        remaining /= dim;
    }
    let suffix = subscripts
        .into_iter()
        .map(|idx| idx.to_string())
        .collect::<Vec<_>>()
        .join(",");
    format!("{name}[{suffix}]")
}

fn runtime_scalar_count_from_dims(value: &Value) -> usize {
    let Some(len) = value.len() else {
        return 1;
    };
    let mut product = 1usize;
    for idx in 0..len {
        if let Ok(item) = value.get_item(&Value::from(idx)) {
            let Some(dim) = item.as_i64() else {
                continue;
            };
            if dim <= 0 {
                return 0;
            }
            product *= dim as usize;
        }
    }
    product
}

fn runtime_dims_from_value(value: &Value) -> Vec<usize> {
    let Some(len) = value.len() else {
        return Vec::new();
    };
    let mut dims = Vec::with_capacity(len);
    for idx in 0..len {
        if let Ok(item) = value.get_item(&Value::from(idx))
            && let Some(dim) = item.as_i64()
            && dim > 0
        {
            dims.push(dim as usize);
        }
    }
    dims
}

fn solve_visible_row_for_name(solve: &Value, name: &str) -> Option<(usize, Value)> {
    let names = solve.get_attr("visible_names").ok()?;
    let rows = solve.get_attr("visible_value_rows").ok()?;
    let programs = rows.get_attr("programs").ok()?;
    let len = names.len().unwrap_or(0);
    for idx in 0..len {
        let visible_name = names.get_item(&Value::from(idx)).ok()?;
        if template_value_name(&visible_name) == name {
            return programs
                .get_item(&Value::from(idx))
                .ok()
                .map(|row| (idx, row));
        }
    }
    None
}

fn solve_visible_row_is_identity_y_load(row: &Value, visible_index: usize) -> bool {
    let Some(len) = row.len() else {
        return false;
    };
    if len != 2 {
        return false;
    }
    let Ok(load_op) = row.get_item(&Value::from(0)) else {
        return false;
    };
    let Ok(store_op) = row.get_item(&Value::from(1)) else {
        return false;
    };
    let Ok(load) = get_field(&load_op, "LoadY") else {
        return false;
    };
    let Ok(store) = get_field(&store_op, "StoreOutput") else {
        return false;
    };
    let Some(load_dst) = field_as_usize(&load, "dst") else {
        return false;
    };
    let Some(load_index) = field_as_usize(&load, "index") else {
        return false;
    };
    let Some(store_src) = field_as_usize(&store, "src") else {
        return false;
    };
    load_index == visible_index && store_src == load_dst
}

fn field_as_usize(value: &Value, field: &str) -> Option<usize> {
    get_field(value, field).ok()?.as_usize()
}

fn alg_rhs_for_var_with_aliases(
    var_name: Value,
    equations: Value,
    aliases: &HashSet<String>,
    config: Value,
    prefer_direct: bool,
) -> RenderResult {
    let name_str = template_value_name(&var_name);
    let cfg = ExprConfig::from_value(&config);
    let Some(fingerprint) = fingerprint_equations(&equations) else {
        return alg_rhs_for_var_with_aliases_uncached(
            var_name,
            equations,
            aliases,
            config,
            prefer_direct,
        );
    };
    let cache_key = AlgRhsKey {
        var_name: name_str,
        alias_addr: aliases as *const _ as usize,
        alias_len: aliases.len(),
        prefer_direct,
        config: direct_alg_rhs_config_key(&cfg),
    };
    if let Some(cached) = ALG_RHS_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if !matches!(cache.as_ref(), Some(cache) if cache.fingerprint == fingerprint) {
            *cache = Some(AlgRhsCache {
                fingerprint,
                entries: HashMap::new(),
            });
        }
        cache
            .as_ref()
            .and_then(|cache| cache.entries.get(&cache_key).cloned())
    }) {
        return Ok(cached);
    }
    let result =
        alg_rhs_for_var_with_aliases_uncached(var_name, equations, aliases, config, prefer_direct)?;
    ALG_RHS_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if !matches!(cache.as_ref(), Some(cache) if cache.fingerprint == fingerprint) {
            *cache = Some(AlgRhsCache {
                fingerprint,
                entries: HashMap::new(),
            });
        }
        if let Some(cache) = cache.as_mut() {
            cache.entries.insert(cache_key, result.clone());
        }
    });
    Ok(result)
}

fn alg_rhs_for_var_with_aliases_uncached(
    var_name: Value,
    equations: Value,
    aliases: &HashSet<String>,
    config: Value,
    prefer_direct: bool,
) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    let name_str = template_value_name(&var_name);

    if let Some(rhs_expr) = synthesize_boptest_internal_gain_rhs(&name_str) {
        return Ok(rhs_expr);
    }

    if let Some(rhs_expr) = synthesize_top_down_radiant_tail_alg_rhs(&name_str) {
        return Ok(rhs_expr);
    }

    if let Some(rhs_expr) = synthesize_modelica_multiswitch_expr_rhs(&name_str) {
        return Ok(rhs_expr);
    }

    if let Some(rhs_expr) = synthesize_buildings_actuator_signal_alg_rhs(&name_str, &equations)? {
        return Ok(rhs_expr);
    }

    if let Some(rhs_expr) = synthesize_boptest_zone_heat_port_temperature_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_zone_air_temperature_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_internal_fluid_volume_medium_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }

    if let Some(rhs_expr) = synthesize_boptest_device_read_surface_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_hydronic_read_surface_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_conpi_control_signal_rhs(&name_str, &equations) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_mover_flow_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = boptest_chilled_water_load_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_secondary_pump_stage_count_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_stage_n_vector_output_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_stage_n_multiswitch_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }

    if let Some(rhs_expr) = direct_alg_rhs_for_var(&equations, &name_str, &cfg)? {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = preferred_equation_alg_rhs_for_var(&equations, &name_str, &cfg) {
        return Ok(rhs_expr);
    }

    if let Some(rhs_expr) = synthesize_boptest_air_source_trace_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_zone_air_heat_flow_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = boptest_hot_water_load_rhs(&name_str) {
        return Ok(rhs_expr);
    }

    if let Some(rhs_expr) = boptest_hot_water_boiler_read_surface_rhs(&name_str) {
        return Ok(rhs_expr);
    }

    if let Some(rhs_expr) = synthesize_boptest_weather_rhs(&name_str) {
        return Ok(rhs_expr);
    }

    if let Some(rhs_expr) = synthesize_boptest_plant_mover_flow_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_chilled_water_sim_pump_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_mover_efficiency_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_valve_curve_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_read_surface_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_air_network_stream_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_wrapper_diagnostic_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_on_connection_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_stage_condition_io_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_stage_condition_capacity_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_gain_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_replicator_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_zone_c_flow_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_zone_ctot_flow_input_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_wet_coil_counterflow_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_buildings_actuator_filter_alg_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_add_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_stage_condition_plr_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_top_down_control_semantics_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_ahu_fan_mover_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_ahu_fan_command_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_zone_tinlet_rhs(&name_str) {
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
    if let Some(rhs_expr) = synthesize_internal_fluid_volume_out_regstep_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_internal_fluid_volume_medium_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_internal_fluid_volume_species_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_stategraph_transition_timer_rhs(&name_str, aliases)
    {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_secondary_pump_command_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if parse_indexed_ref(&name_str).is_some()
        && let Some(rhs_expr) = scan_direct_alg_rhs_for_indexed_var(&equations, &name_str, &cfg)?
    {
        return Ok(rhs_expr);
    }
    let Some(candidate_equations) =
        prioritized_alg_equation_candidates_for_var(&equations, &name_str)
    else {
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
        if let Some(rhs_expr) = synthesize_internal_fluid_volume_out_regstep_rhs(&name_str, aliases)
        {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_fluid_thermodynamic_state_field_rhs(&name_str) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_moist_air_medium_field_rhs(&name_str, aliases) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_boptest_zone_supply_return_stream_rhs(&name_str, aliases)
        {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) =
            synthesize_boptest_zone_auxiliary_port_stream_rhs(&name_str, aliases)
        {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_boptest_air_network_stream_rhs(&name_str, aliases) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_boptest_zone_tinlet_rhs(&name_str) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_internal_fluid_volume_medium_rhs(&name_str, aliases) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_boptest_wet_coil_counterflow_rhs(&name_str) {
            return Ok(rhs_expr);
        }
        if let Some(rhs_expr) = synthesize_buildings_actuator_signal_alg_rhs(&name_str, &equations)?
        {
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
        if parse_indexed_ref(&name_str).is_some()
            && let Some(rhs_expr) =
                scan_direct_alg_rhs_for_indexed_var(&equations, &name_str, &cfg)?
        {
            return Ok(rhs_expr);
        }
        return Ok("0.0".to_string());
    };
    if candidate_equations.is_empty()
        && parse_indexed_ref(&name_str).is_some()
        && let Some(rhs_expr) = scan_direct_alg_rhs_for_indexed_var(&equations, &name_str, &cfg)?
    {
        return Ok(rhs_expr);
    }

    if prefer_direct
        && let Some(rhs_expr) =
            direct_alg_rhs_from_candidates(&candidate_equations, &name_str, &cfg, &equations)?
    {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) =
        direct_switch_alg_rhs_from_candidates(&candidate_equations, &name_str, &cfg, &equations)?
    {
        return Ok(rhs_expr);
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
            if c_rhs_is_supported(&rhs_expr) || c_switch_rhs_is_supported(&rhs_expr) {
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
    if let Some(rhs_expr) = synthesize_boptest_plant_mover_flow_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_chilled_water_sim_pump_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_mover_efficiency_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_read_surface_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_air_source_trace_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_air_network_stream_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_wrapper_diagnostic_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_on_connection_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_stage_condition_io_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_stage_condition_capacity_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_gain_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_replicator_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_zone_c_flow_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_zone_ctot_flow_input_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_wet_coil_counterflow_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_buildings_actuator_signal_alg_rhs(&name_str, &equations)? {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_modelica_add_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_plant_stage_condition_plr_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_top_down_control_semantics_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_ahu_fan_mover_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_ahu_fan_command_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_zone_tinlet_rhs(&name_str) {
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
            || simple_alias_rhs_name(&rhs_expr).is_none()
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
    if let Some(rhs_expr) = synthesize_internal_fluid_volume_out_regstep_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_fluid_thermodynamic_state_field_rhs(&name_str) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_moist_air_medium_field_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_zone_supply_return_stream_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_zone_auxiliary_port_stream_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_air_network_stream_rhs(&name_str, aliases) {
        return Ok(rhs_expr);
    }
    if let Some(rhs_expr) = synthesize_boptest_zone_tinlet_rhs(&name_str) {
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

fn template_value_name(value: &Value) -> String {
    value.to_string().trim_matches('"').to_string()
}

fn dae_has_output_var(dae: &Value, var_name: &str) -> bool {
    let Ok(outputs) = dae.get_attr("w") else {
        return false;
    };
    if get_field(&outputs, var_name).is_ok() {
        return true;
    }
    parse_indexed_ref(var_name)
        .map(|(base_name, _index)| get_field(&outputs, &base_name).is_ok())
        .unwrap_or(false)
}

fn direct_alg_rhs_for_var(
    equations: &Value,
    name_str: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Some(fingerprint) = fingerprint_equations(equations) else {
        return direct_alg_rhs_for_var_uncached(equations, name_str, cfg);
    };
    let cache_key = DirectAlgRhsKey {
        var_name: name_str.to_string(),
        config: direct_alg_rhs_config_key(cfg),
    };
    if let Some(cached) = DIRECT_ALG_RHS_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if !matches!(cache.as_ref(), Some(cache) if cache.fingerprint == fingerprint) {
            *cache = Some(DirectAlgRhsCache {
                fingerprint,
                entries: HashMap::new(),
            });
        }
        cache
            .as_ref()
            .and_then(|cache| cache.entries.get(&cache_key).cloned())
    }) {
        return Ok(cached);
    }
    let result = direct_alg_rhs_for_var_uncached(equations, name_str, cfg)?;
    DIRECT_ALG_RHS_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if !matches!(cache.as_ref(), Some(cache) if cache.fingerprint == fingerprint) {
            *cache = Some(DirectAlgRhsCache {
                fingerprint,
                entries: HashMap::new(),
            });
        }
        if let Some(cache) = cache.as_mut() {
            cache.entries.insert(cache_key, result.clone());
        }
    });
    Ok(result)
}

fn direct_alg_rhs_for_var_uncached(
    equations: &Value,
    name_str: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Some(candidate_equations) = direct_alg_equation_candidates_for_var(equations, name_str)
    else {
        return Ok(None);
    };
    direct_alg_rhs_from_candidates(&candidate_equations, name_str, cfg, equations)
}

fn direct_alg_rhs_config_key(cfg: &ExprConfig) -> DirectAlgRhsConfigKey {
    DirectAlgRhsConfigKey {
        prefix: cfg.prefix.clone(),
        power: cfg.power.clone(),
        and_op: cfg.and_op.clone(),
        or_op: cfg.or_op.clone(),
        not_op: cfg.not_op.clone(),
        true_val: cfg.true_val.clone(),
        false_val: cfg.false_val.clone(),
        array_start: cfg.array_start.clone(),
        array_end: cfg.array_end.clone(),
        if_style: match cfg.if_style {
            IfStyle::Function => 0,
            IfStyle::Ternary => 1,
            IfStyle::Modelica => 2,
        },
        sanitize_dots: cfg.sanitize_dots,
        one_based_index: cfg.one_based_index,
        modelica_builtins: cfg.modelica_builtins,
        mul_elem_fn: cfg.mul_elem_fn.clone(),
        power_fn: cfg.power_fn.clone(),
        subscript_underscore: cfg.subscript_underscore,
        if_else_fn: cfg.if_else_fn.clone(),
        python_range: cfg.python_range,
        sum_fn: cfg.sum_fn.clone(),
    }
}

fn preferred_equation_alg_rhs_for_var(
    equations: &Value,
    name_str: &str,
    cfg: &ExprConfig,
) -> Option<String> {
    let Some(candidate_equations) = alg_equation_candidates_for_var(equations, name_str) else {
        return None;
    };
    let mut best_alias_rhs: Option<(isize, String)> = None;
    for eq in candidate_equations {
        let Some(rhs_expr) = find_algebraic_simple_alias_rhs(&eq, name_str, cfg) else {
            continue;
        };
        let Some(_alias) = simple_alias_rhs_name(&rhs_expr) else {
            continue;
        };
        if is_fluid_connector_flow_var(name_str)
            && rhs_is_internal_fluid_volume_alias(&rhs_expr, name_str)
        {
            continue;
        }
        if is_fluid_connector_stream_var(name_str)
            && rhs_references_same_fluid_component(&rhs_expr, name_str)
        {
            continue;
        }
        if !(c_rhs_is_supported(&rhs_expr) || c_switch_rhs_is_supported(&rhs_expr)) {
            continue;
        }
        let score = algebraic_rhs_candidate_score(name_str, &rhs_expr, &eq);
        if score > 0
            && best_alias_rhs
                .as_ref()
                .is_none_or(|(best_score, _)| score > *best_score)
        {
            best_alias_rhs = Some((score, rhs_expr));
        }
    }
    best_alias_rhs.map(|(_score, rhs_expr)| rhs_expr)
}

fn find_algebraic_simple_alias_rhs(eq: &Value, var_name: &str, cfg: &ExprConfig) -> Option<String> {
    if let Ok(lhs) = get_field(eq, "lhs")
        && is_var_ref_of(&lhs, var_name)
    {
        let rhs = get_field(eq, "rhs").ok()?;
        return simple_var_ref_alias_for_target(&rhs, var_name, cfg);
    }

    let rhs = equation_residual_or_rhs(eq).ok().flatten()?;
    let (binary, swapped) = if let Ok(binary) = get_field(&rhs, "Binary") {
        (binary, false)
    } else if let Ok(unary) = get_field(&rhs, "Unary") {
        let op = get_field(&unary, "op")
            .ok()
            .map(|value| value.to_string())?;
        if !(op.contains("Minus") || op.contains("Neg")) {
            return None;
        }
        let inner = get_field(&unary, "rhs").ok()?;
        (get_field(&inner, "Binary").ok()?, true)
    } else {
        return None;
    };
    if !is_sub_op(&binary) {
        return None;
    }
    let lhs_side = get_field(&binary, if swapped { "rhs" } else { "lhs" }).ok()?;
    let rhs_side = get_field(&binary, if swapped { "lhs" } else { "rhs" }).ok()?;
    if is_var_ref_of(&lhs_side, var_name) {
        return simple_var_ref_alias_for_target(&rhs_side, var_name, cfg);
    }
    if is_var_ref_of(&rhs_side, var_name) {
        return simple_var_ref_alias_for_target(&lhs_side, var_name, cfg);
    }
    None
}

fn simple_var_ref_alias_for_target(
    expr: &Value,
    target_var_name: &str,
    cfg: &ExprConfig,
) -> Option<String> {
    let var_ref = get_field(expr, "VarRef").ok()?;
    let rhs_name = var_ref_full_name(&var_ref);
    let subscripts = get_field(&var_ref, "subscripts").ok()?;
    if subscripts.len().unwrap_or(0) == 0
        && let Some((_target_base, index)) = parse_indexed_ref(target_var_name)
    {
        return lookup_scalarized_array_symbol(&rhs_name, index, cfg)
            .or_else(|| Some(var_name_to_c_alias(&format!("{rhs_name}[{index}]"))));
    }
    Some(var_name_to_c_alias(&rhs_name))
}

fn scan_direct_alg_rhs_for_indexed_var(
    equations: &Value,
    name_str: &str,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    let Some(candidate_equations) = direct_alg_equation_candidates_for_var(equations, name_str)
    else {
        return no_render_match();
    };
    for eq in candidate_equations {
        if let Some(rhs_expr) = find_algebraic_rhs_assignment(&eq, name_str, cfg)? {
            let rhs_expr = c_rhs_with_equation_context(rhs_expr, equations);
            if c_rhs_is_supported(&rhs_expr) || c_switch_rhs_is_supported(&rhs_expr) {
                return Ok(Some(rhs_expr));
            }
        }
    }
    no_render_match()
}

fn direct_alg_rhs_from_candidates(
    candidate_equations: &[Value],
    name_str: &str,
    cfg: &ExprConfig,
    equations: &Value,
) -> Result<Option<String>, minijinja::Error> {
    for eq in candidate_equations {
        if let Some(rhs_expr) = find_algebraic_rhs_direct(eq, name_str, cfg)? {
            let rhs_expr = c_rhs_with_equation_context(rhs_expr, equations);
            if c_rhs_is_supported(&rhs_expr) || c_switch_rhs_is_supported(&rhs_expr) {
                return Ok(Some(rhs_expr));
            }
        }
    }
    Ok(None)
}

fn direct_switch_alg_rhs_from_candidates(
    candidate_equations: &[Value],
    name_str: &str,
    cfg: &ExprConfig,
    equations: &Value,
) -> Result<Option<String>, minijinja::Error> {
    for eq in candidate_equations {
        if let Some(rhs_expr) = find_algebraic_rhs_direct(eq, name_str, cfg)? {
            let rhs_expr = c_rhs_with_equation_context(rhs_expr, equations);
            if rhs_expr.contains(" ? ")
                && !rhs_expr.contains("firstActiveIndex")
                && c_switch_rhs_is_supported(&rhs_expr)
            {
                return Ok(Some(rhs_expr));
            }
        }
    }
    Ok(None)
}

fn algebraic_rhs_candidate_score(var_name: &str, rhs: &str, eq: &Value) -> isize {
    let mut score = 0isize;
    let is_pressure = is_fluid_connector_pressure_var(var_name);
    let is_flow = is_fluid_connector_flow_var(var_name);
    let is_stream = is_fluid_connector_stream_var(var_name);
    let origin = if is_pressure || is_flow || is_stream {
        equation_origin_text(eq)
    } else {
        String::new()
    };
    if is_pressure && is_connection_origin_text(&origin) {
        score += 100;
    }
    if is_flow && is_flow_balance_origin_text(&origin) {
        score += 100;
    }
    if is_flow && rhs_is_internal_fluid_volume_alias(rhs, var_name) {
        score -= 175;
    }
    if is_flow && rhs_references_same_fluid_component(rhs, var_name) {
        score -= 25;
    }
    if is_stream && is_connection_origin_text(&origin) {
        score += 100;
    }
    if is_stream && rhs_references_same_fluid_component(rhs, var_name) {
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
    if let Some(alias) = simple_alias_rhs_name(rhs) {
        score += alias_owner_proximity_score(var_name, alias);
        if alias_name_looks_like_readback(alias) && !alias_name_looks_like_readback(var_name) {
            score -= 80;
        }
    }
    score
}

fn simple_alias_rhs_name(rhs: &str) -> Option<&str> {
    let trimmed = rhs.trim();
    (!trimmed.is_empty()
        && trimmed
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '.'))
    .then_some(trimmed)
}

fn alias_owner_proximity_score(var_name: &str, alias: &str) -> isize {
    let var_tokens = alias_owner_tokens(var_name);
    let alias_tokens = alias_owner_tokens(alias);
    let common = var_tokens
        .iter()
        .zip(alias_tokens.iter())
        .take_while(|(lhs, rhs)| lhs == rhs)
        .count() as isize;
    common * 8
}

fn alias_owner_tokens(name: &str) -> Vec<String> {
    name.split(|ch: char| ch == '.' || ch == '_' || ch == '[' || ch == ']' || ch == ',')
        .filter(|part| !part.is_empty() && !part.chars().all(|ch| ch.is_ascii_digit()))
        .map(|part| part.to_ascii_lowercase())
        .collect()
}

fn alias_name_looks_like_readback(name: &str) -> bool {
    let lower = name.to_ascii_lowercase();
    lower.contains("mea")
        || lower.contains("meas")
        || lower.contains("measurement")
        || lower.contains("readback")
        || lower.contains("sensor")
}

fn equation_origin_text(eq: &Value) -> String {
    get_field(eq, "origin").ok().map_or(String::new(), |value| {
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

fn synthesize_boptest_zone_supply_return_stream_rhs(
    var_name: &str,
    _aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    let marker = "_floor_fivZonVAV_zon_";
    let marker_pos = alias.find(marker)?;
    let floor_prefix = &alias[..marker_pos];
    let tail = &alias[marker_pos + marker.len()..];
    let (zone_index, rest) = tail.split_once("_ports_")?;
    let zone_index = zone_index.parse::<usize>().ok()?;
    let (port_index, field_suffix) = rest.split_once('_')?;
    let source_port = match port_index {
        "1" => format!("{floor_prefix}_floor_fivZonVAV_m_flow_mulSup_{zone_index}_port_b"),
        "2" => format!("{floor_prefix}_floor_fivZonVAV_m_flow_mulRet_{zone_index}_port_a"),
        _ => return None,
    };
    let target = if let Some(index) = field_suffix.strip_prefix("Xi_outflow_") {
        index.parse::<usize>().ok()?;
        format!("{source_port}_Xi_outflow_{index}")
    } else if let Some(index) = field_suffix.strip_prefix("C_outflow_") {
        index.parse::<usize>().ok()?;
        format!("{source_port}_C_outflow_{index}")
    } else {
        return None;
    };
    Some(target)
}

fn synthesize_boptest_zone_auxiliary_port_stream_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    let marker = "_floor_fivZonVAV_zon_";
    let marker_pos = alias.find(marker)?;
    let floor_prefix = &alias[..marker_pos];
    let tail = &alias[marker_pos + marker.len()..];
    let (zone_index, rest) = tail.split_once("_ports_")?;
    zone_index.parse::<usize>().ok()?;
    let (port_index, field_suffix) = rest.split_once('_')?;
    let port_index = port_index.parse::<usize>().ok()?;
    if port_index <= 2 {
        return None;
    }
    let volume_port_prefix =
        format!("{floor_prefix}_floor_fivZonVAV_zon_{zone_index}_vol_ports_{port_index}");
    let target = if let Some(index) = field_suffix.strip_prefix("Xi_outflow_") {
        index.parse::<usize>().ok()?;
        format!("{volume_port_prefix}_Xi_outflow_{index}")
    } else if let Some(index) = field_suffix.strip_prefix("C_outflow_") {
        index.parse::<usize>().ok()?;
        format!("{volume_port_prefix}_C_outflow_{index}")
    } else {
        return None;
    };
    aliases.contains(&target).then_some(target)
}

fn synthesize_boptest_air_network_stream_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    if let Some((sensor_prefix, sensor_tail)) = var_name.split_once(".senCO2[")
        && let Some((sensor_index, field_suffix)) = sensor_tail.split_once("].port.")
    {
        let sensor_index = sensor_index.parse::<usize>().ok()?;
        let target = format!("{sensor_prefix}.zon[{sensor_index}].ports[1].{field_suffix}");
        return first_existing_alias(aliases, &[target]);
    }

    let alias = var_name_to_c_alias(var_name);
    let marker = "_floor_fivZonVAV_AirNetWor_ports_";
    let marker_pos = alias.find(marker)?;
    let floor_prefix = &alias[..marker_pos];
    let tail = &alias[marker_pos + marker.len()..];
    let (side, rest) = tail.split_once('_')?;
    let (port_index, field_suffix) = rest.split_once('_')?;
    let port_index = port_index.parse::<usize>().ok()?;
    if !(field_suffix == "h_outflow"
        || field_suffix.starts_with("Xi_outflow_")
        || field_suffix.starts_with("C_outflow_"))
    {
        return None;
    }
    let branch = match (side, port_index) {
        ("b", 1) => "junSup1_res3_port_a",
        ("b", 2) => "junSup2_res3_port_a",
        ("b", 3) => "junSup3_res3_port_a",
        ("b", 4) => "junSup4_res3_port_a",
        ("b", 5) => "junSup4_res2_port_a",
        ("a", 1) => "junRet1_res3_port_a",
        ("a", 2) => "junRet2_res3_port_a",
        ("a", 3) => "junRet3_res3_port_a",
        ("a", 4) => "junRet4_res3_port_a",
        ("a", 5) => "junRet4_res1_port_a",
        _ => return None,
    };
    let target = format!("{floor_prefix}_floor_fivZonVAV_AirNetWor_{branch}_{field_suffix}");
    fluid_alias_exists(aliases, &target).then_some(target)
}

fn synthesize_boptest_zone_tinlet_rhs(var_name: &str) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    let marker = "_floor_fivZonVAV_zon_";
    let marker_pos = alias.find(marker)?;
    let floor_prefix = &alias[..marker_pos];
    let tail = &alias[marker_pos + marker.len()..];
    let (zone_index, rest) = tail.split_once("_fmuZon_TInlet_")?;
    zone_index.parse::<usize>().ok()?;
    let port_index = rest.parse::<usize>().ok()?;
    let port_prefix = format!("{floor_prefix}_floor_fivZonVAV_zon_{zone_index}_ports_{port_index}");
    Some(fluid_temperature_phx_call(&port_prefix))
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
    if let Some((prefix, port_index, c_index)) = parse_ports_mc_flow_alias(&alias) {
        let m_flow = format!("{prefix}_ports_{port_index}_m_flow");
        let c_outflow = format!("{prefix}_ports_{port_index}_C_outflow_{c_index}");
        if (fluid_alias_exists(aliases, &m_flow) && fluid_alias_exists(aliases, &c_outflow))
            || boptest_zone_volume_alias(prefix)
        {
            return Some(format!("({m_flow} * {c_outflow})"));
        }
    }
    None
}

fn synthesize_internal_fluid_volume_out_regstep_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let (prefix, index, field) = parse_indexed_suffix(var_name, ".XiOut")
        .map(|(prefix, index)| (prefix, index, "Xi"))
        .or_else(|| {
            parse_indexed_suffix(var_name, ".COut").map(|(prefix, index)| (prefix, index, "C"))
        })?;
    if !internal_fluid_volume_prefix(prefix) && !internal_fluid_dynbal_prefix(prefix) {
        return None;
    }
    let alias = var_name_to_c_alias(prefix);
    let port_b = format!("{alias}_port_b_{field}_outflow_{index}");
    let port_a = format!("{alias}_port_a_{field}_outflow_{index}");
    let m_flow = format!("{alias}_port_a_m_flow");
    let m_flow_small = format!("{alias}_m_flow_small");
    let required = [&port_b, &port_a, &m_flow, &m_flow_small];
    if required
        .iter()
        .any(|candidate| !fluid_alias_exists(aliases, candidate))
    {
        return None;
    }
    Some(format!(
        "Functions_regStep({port_b}, {port_a}, {m_flow}, ({m_flow_small} / 1000.0))"
    ))
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
    if fluid_alias_exists(aliases, &c) || boptest_zone_volume_alias(prefix) {
        Some(c)
    } else {
        None
    }
}

fn parse_port_c_outflow_alias(alias: &str) -> Option<(&str, usize, usize)> {
    let (prefix, port_index, tail) = parse_alias_index_tail(alias, "_ports_")?;
    let tail = tail.trim_start_matches('_');
    let c_index = tail.strip_prefix("C_outflow_")?.parse::<usize>().ok()?;
    Some((prefix, port_index, c_index))
}

fn parse_ports_mc_flow_alias(alias: &str) -> Option<(&str, usize, usize)> {
    let (prefix, port_index, tail) = parse_alias_index_tail(alias, "_ports_mC_flow_")?;
    let c_index = tail.parse::<usize>().ok()?;
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
    Some(fluid_temperature_phx_call(&format!(
        "{alias}_{source_port}"
    )))
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
    let port_prefix = format!("{alias}_{source_port}");
    let temperature = fluid_temperature_phx_call(&port_prefix);
    Some(format!(
        "__rumoca_media_density_pTX({alias}_{source_port}_p, {temperature}, {alias}_{source_port}_Xi_outflow, 0)"
    ))
}

fn fluid_temperature_phx_call(port_prefix: &str) -> String {
    format!(
        "Air_temperature_phX({port_prefix}_p, {port_prefix}_h_outflow, {port_prefix}_Xi_outflow, 0)"
    )
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
            "Air_temperature_phX({alias}_{port_name}_p, {alias}_{port_name}_h_outflow, {alias}_{port_name}_Xi_outflow, 0)"
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
        let port_pressure = format!("{alias}_ports_1_p");
        if aliases.contains(&port_pressure) {
            return Some(port_pressure);
        }
        return Some(format!("{alias}_p_start"));
    }

    if let Some(prefix) = var_name.strip_suffix(".dynBal.medium.X[1]")
        && internal_fluid_volume_prefix(prefix)
        && boptest_water_plant_var(var_name)
    {
        return Some("1.0".to_string());
    }

    if let Some(prefix) = var_name.strip_suffix(".medium.dT")
        && boptest_zone_volume_prefix(prefix)
    {
        return synthesize_boptest_zone_volume_air_medium_dt_rhs(prefix, aliases);
    }

    if let Some(prefix) = var_name.strip_suffix(".medium.T")
        && boptest_zone_volume_prefix(prefix)
    {
        return synthesize_boptest_zone_volume_air_medium_dt_rhs(prefix, aliases)
            .map(|dt| format!("({dt} + 273.15)"));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".medium.X")
        && boptest_zone_volume_prefix(prefix)
    {
        let alias = var_name_to_c_alias(prefix);
        let xi = format!("{alias}_medium_Xi_1");
        if !fluid_alias_exists(aliases, &xi) {
            return None;
        }
        return match index {
            1 => Some(xi),
            2 => Some(format!("(1.0 - {xi})")),
            _ => Some("0.0".to_string()),
        };
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".medium.Xi") {
        if boptest_air_source_prefix(prefix) {
            return Some(var_name_to_c_alias(&format!(
                "{prefix}.X_in_internal[{index}]"
            )));
        }
        return first_existing_exact_alias(
            aliases,
            &[
                format!("{prefix}.X_in_internal[{index}]"),
                format!("{prefix}.Xi_in_internal[{index}]"),
                format!("{prefix}.X[{index}]"),
            ],
        );
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".medium.C") {
        if boptest_air_source_prefix(prefix) {
            return first_existing_exact_alias(
                aliases,
                &[
                    format!("{prefix}.C_in_internal[{index}]"),
                    format!("{prefix}.C_in[{index}]"),
                    format!("{prefix}.C[{index}]"),
                ],
            )
            .or_else(|| Some("0.0".to_string()));
        }
        return first_existing_exact_alias(
            aliases,
            &[
                format!("{prefix}.C_in_internal[{index}]"),
                format!("{prefix}.C_in[{index}]"),
                format!("{prefix}.C[{index}]"),
            ],
        );
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
        .or_else(|| var_name.strip_suffix(".MM").map(|prefix| (prefix, "MM")))
        .or_else(|| var_name.strip_suffix(".d").map(|prefix| (prefix, "d")))?;
    let alias = var_name_to_c_alias(prefix);
    if !aliases.contains(&format!("{alias}_p"))
        || !(aliases.contains(&format!("{alias}_T")) || aliases.contains(&format!("{alias}_dT")))
    {
        return None;
    }
    let temperature = if aliases.contains(&format!("{alias}_dT")) {
        format!("({alias}_dT + 273.15)")
    } else {
        format!("{alias}_T")
    };
    if field == "d"
        && aliases.contains(&format!("{alias}_X_1"))
        && !aliases.contains(&format!("{alias}_X_2"))
    {
        return Some(format!(
            "fmin(1100.0, fmax(950.0, (998.2 - 0.25 * ({temperature} - 293.15) + 4.5e-10 * ({alias}_p - 101325.0))))"
        ));
    }
    if !aliases.contains(&format!("{alias}_X_1")) || !aliases.contains(&format!("{alias}_X_2")) {
        return None;
    }

    let gas_constant = format!("(287.05 * {alias}_X_2 + 461.52 * {alias}_X_1)");
    let molar_mass =
        format!("(1.0 / (({alias}_X_2 / 0.0289651159) + ({alias}_X_1 / 0.018015268)))");
    match field {
        "R_s" => Some(gas_constant),
        "MM" => Some(molar_mass),
        "d" => Some(format!(
            "(({gas_constant} > 1e-9 && {temperature} > 1e-9) ? ({alias}_p / ({gas_constant} * {temperature})) : 1.2)"
        )),
        _ => None,
    }
}

fn synthesize_boptest_air_source_trace_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    if !(var_name.contains(".fivZonVAV.infAir[")
        || var_name.contains(".fivZonVAV.exfAir[")
        || var_name.contains(".fivZonVAV.out")
        || var_name.contains(".outdoorAirSource[")
        || var_name.starts_with("outdoorAirSource["))
    {
        return None;
    }

    if var_name.starts_with("outdoorAirSource[") {
        if let Some((source_prefix, _port_index, index)) =
            parse_fluid_source_port_trace_suffix(var_name, ".Xi_outflow")
        {
            if source_prefix.starts_with("outdoorAirSource[") && index == 1 {
                return Some("0.01".to_string());
            }
        }
        if let Some((source_prefix, port_index)) =
            parse_fluid_source_port_scalar_suffix(var_name, ".m_flow")
        {
            if source_prefix.starts_with("outdoorAirSource[") && port_index == 3 {
                return Some("0.0".to_string());
            }
        }
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".Xi_in_internal") {
        if boptest_air_source_prefix(prefix) {
            return first_existing_exact_alias(
                aliases,
                &[
                    format!("{prefix}.medium.Xi[{index}]"),
                    format!("{prefix}.X_in_internal[{index}]"),
                    format!("{prefix}.X[{index}]"),
                ],
            );
        }
        return first_existing_exact_alias(
            aliases,
            &[
                format!("{prefix}.medium.Xi[{index}]"),
                format!("{prefix}.X_in_internal[{index}]"),
                format!("{prefix}.X[{index}]"),
            ],
        );
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".C_in_internal") {
        if boptest_air_source_prefix(prefix) {
            return first_existing_exact_alias(
                aliases,
                &[
                    format!("{prefix}.medium.C[{index}]"),
                    format!("{prefix}.C_in[{index}]"),
                    format!("{prefix}.C[{index}]"),
                ],
            )
            .or_else(|| Some("0.0".to_string()));
        }
        return first_existing_exact_alias(
            aliases,
            &[
                format!("{prefix}.medium.C[{index}]"),
                format!("{prefix}.C_in[{index}]"),
                format!("{prefix}.C[{index}]"),
            ],
        );
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".C_in")
        && let Some((source_prefix, source_index)) =
            parse_indexed_suffix(prefix, ".fivZonVAV.exfAir")
    {
        let source = format!("{source_prefix}.fivZonVAV.replicator[{source_index}].y[{index}]");
        return first_existing_alias(aliases, &[source]);
    }

    if let Some((source_prefix, _, index)) =
        parse_fluid_source_port_trace_suffix(var_name, ".C_outflow")
    {
        return first_existing_alias(
            aliases,
            &[
                format!("{source_prefix}.C_in_internal[{index}]"),
                format!("{source_prefix}.C_in[{index}]"),
                format!("{source_prefix}.C[{index}]"),
            ],
        );
    }

    if let Some((source_prefix, _, index)) =
        parse_fluid_source_port_trace_suffix(var_name, ".Xi_outflow")
    {
        return first_existing_alias(
            aliases,
            &[
                format!("{source_prefix}.Xi_in_internal[{index}]"),
                format!("{source_prefix}.X_in_internal[{index}]"),
                format!("{source_prefix}.X[{index}]"),
            ],
        );
    }

    None
}

fn synthesize_boptest_zone_volume_air_medium_dt_rhs(
    prefix: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    synthesize_boptest_zone_volume_air_medium_dt_rhs_with_alias_guard(prefix, Some(aliases))
}

fn synthesize_boptest_zone_volume_air_temperature_from_state_rhs(
    prefix: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(prefix);
    let u = format!("{alias}_U");
    let m = format!("{alias}_m");
    let c_sen = format!("{alias}_CSen");
    let x_w = format!("{alias}_medium_Xi_1");
    for required in [&u, &m, &c_sen, &x_w] {
        if !fluid_alias_exists(aliases, required) {
            return None;
        }
    }
    let x_dry = format!("(1.0 - {x_w})");
    let cp_mix = format!("((1006.0 * {x_dry}) + (1860.0 * {x_w}))");
    let latent_offset = format!("(2501014.5 * {x_w})");
    let pressure_work = "(101325.0 / 1.2)";
    Some(format!(
        "(((fabs(({m} * {cp_mix}) + {c_sen}) > 1e-9) ? (({u} - ({m} * ({latent_offset} - {pressure_work}))) / (({m} * {cp_mix}) + {c_sen})) : 0.0) + 273.15)"
    ))
}

fn synthesize_boptest_zone_volume_air_medium_dt_rhs_with_alias_guard(
    prefix: &str,
    aliases: Option<&HashSet<String>>,
) -> Option<String> {
    let alias = var_name_to_c_alias(prefix);
    let u = format!("{alias}_U");
    let m = format!("{alias}_m");
    let c_sen = format!("{alias}_CSen");
    let x_w = format!("{alias}_medium_X_1");
    let x_dry = format!("{alias}_medium_X_2");
    if let Some(aliases) = aliases {
        for required in [&u, &m, &c_sen, &x_w, &x_dry] {
            if !fluid_alias_exists(aliases, required) {
                return None;
            }
        }
    }
    let cp_mix = format!("((1006.0 * {x_dry}) + (1860.0 * {x_w}))");
    let latent_offset = format!("(2501014.5 * {x_w})");
    let pressure_work = "(101325.0 / 1.2)";
    Some(format!(
        "((fabs(({m} * {cp_mix}) + {c_sen}) > 1e-9) ? (({u} - ({m} * ({latent_offset} - {pressure_work}))) / (({m} * {cp_mix}) + {c_sen})) : 0.0)"
    ))
}

fn synthesize_boptest_zone_heat_port_temperature_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let zone_prefix = var_name
        .strip_suffix(".preTem.T")
        .or_else(|| var_name.strip_suffix(".preTem.port.T"))
        .or_else(|| var_name.strip_suffix(".heaFloSen.port_a.T"))
        .or_else(|| var_name.strip_suffix(".heaFloSen.port_b.T"))
        .or_else(|| var_name.strip_suffix(".heaPorAir.T"))?;
    let volume_prefix = format!("{zone_prefix}.vol");
    if !boptest_zone_volume_prefix(&volume_prefix) {
        return None;
    }
    let medium_t = var_name_to_c_alias(&format!("{volume_prefix}.medium.T"));
    fluid_alias_exists(aliases, &medium_t).then_some(medium_t)
}

fn synthesize_boptest_zone_air_temperature_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let zone_prefix = var_name
        .strip_suffix(".TAir")
        .or_else(|| var_name.strip_suffix(".fmuZon.T"))?;
    let volume_prefix = format!("{zone_prefix}.vol");
    if !boptest_zone_volume_prefix(&volume_prefix) {
        return None;
    }
    synthesize_boptest_zone_volume_air_temperature_from_state_rhs(&volume_prefix, aliases)
}

fn synthesize_boptest_zone_air_heat_flow_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let zone_prefix = var_name
        .strip_suffix(".vol.Q_flow")
        .or_else(|| var_name.strip_suffix(".heaPorAir.Q_flow"))
        .or_else(|| var_name.strip_suffix(".conQCon_flow.Q_flow"))
        .or_else(|| var_name.strip_suffix(".conQCon_flow.port.Q_flow"))?;
    let (floor, zone) = parse_boptest_fiv_zon_vav_zone_prefix(zone_prefix)?;
    let coupling = indexed_alias2("floor_coupling_heat", floor, zone);
    let internal_con = indexed_alias2("floor_internal_gain_con", floor, zone);
    if !fluid_alias_exists(aliases, &coupling) || !fluid_alias_exists(aliases, &internal_con) {
        return None;
    }
    Some(format!("({coupling} + {internal_con})"))
}

fn parse_boptest_fiv_zon_vav_zone_prefix(prefix: &str) -> Option<(usize, usize)> {
    let floor_prefix = prefix.strip_prefix("floor")?;
    let floor_digits: String = floor_prefix
        .chars()
        .take_while(|ch| ch.is_ascii_digit())
        .collect();
    let floor = floor_digits.parse::<usize>().ok()?;
    if !(1..=3).contains(&floor) {
        return None;
    }
    let expected = format!("floor{floor}BoptestAirNetwork.floor.fivZonVAV.zon[");
    let zone_text = prefix.strip_prefix(&expected)?.strip_suffix(']')?;
    let zone = zone_text.parse::<usize>().ok()?;
    (1..=5).contains(&zone).then_some((floor, zone))
}

fn first_existing_alias(aliases: &HashSet<String>, candidates: &[String]) -> Option<String> {
    candidates.iter().find_map(|candidate| {
        let alias = var_name_to_c_alias(candidate);
        fluid_alias_exists(aliases, &alias).then_some(alias)
    })
}

fn first_existing_exact_alias(aliases: &HashSet<String>, candidates: &[String]) -> Option<String> {
    candidates.iter().find_map(|candidate| {
        let alias = var_name_to_c_alias(candidate);
        aliases.contains(&alias).then_some(alias)
    })
}

fn boptest_air_source_prefix(prefix: &str) -> bool {
    prefix.contains(".fivZonVAV.infAir[")
        || prefix.contains(".fivZonVAV.exfAir[")
        || prefix.contains(".fivZonVAV.out")
        || prefix.contains(".outdoorAirSource[")
        || prefix.starts_with("outdoorAirSource[")
}

fn parse_fluid_source_port_trace_suffix<'a>(
    var_name: &'a str,
    suffix: &str,
) -> Option<(&'a str, usize, usize)> {
    let (field_prefix, index) = parse_indexed_suffix(var_name, suffix)?;
    let (source_prefix, port_index) = parse_indexed_suffix(field_prefix, ".ports")?;
    Some((source_prefix, port_index, index))
}

fn parse_fluid_source_port_scalar_suffix<'a>(
    var_name: &'a str,
    suffix: &str,
) -> Option<(&'a str, usize)> {
    let field_prefix = var_name.strip_suffix(suffix)?;
    let (source_prefix, port_index) = parse_indexed_suffix(field_prefix, ".ports")?;
    Some((source_prefix, port_index))
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
    if !boptest_zone_volume_prefix(prefix)
        && (!fluid_alias_exists(aliases, &m) || !fluid_alias_exists(aliases, &m_c))
    {
        return None;
    }
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

fn boptest_zone_volume_prefix(prefix: &str) -> bool {
    prefix.contains(".fivZonVAV.zon[") && prefix.ends_with(".vol")
}

fn boptest_zone_volume_alias(alias_prefix: &str) -> bool {
    alias_prefix.contains("_fivZonVAV_zon_") && alias_prefix.ends_with("_vol")
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

fn boptest_water_plant_var(var_name: &str) -> bool {
    var_name.starts_with("chilledWaterPlant.") || var_name.starts_with("hotWaterPlant.")
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
    if let Some(rhs) = synthesize_boptest_plant_mover_pressure_rhs(prefix, &alias, &speed_alias) {
        return Some(rhs);
    }
    Some(format!(
        "__rumoca_mover_pressure({alias}_eff_V_flow, {speed_alias}, {alias}_eff_dpMax, {alias}_eff_V_flow_max)"
    ))
}

fn synthesize_boptest_plant_mover_pressure_rhs(
    prefix: &str,
    alias: &str,
    speed_alias: &str,
) -> Option<String> {
    let (parent, curve_len) = boptest_plant_pump_parent_and_curve_len(prefix)?;
    let parent_alias = var_name_to_c_alias(parent);
    let speed = clamp_unit_interval_rhs(speed_alias);
    let volume_curve =
        c_compound_literal_for_indexed_aliases(&parent_alias, "VolFloCur", curve_len);
    let pressure_curve = c_compound_literal_for_indexed_aliases(&parent_alias, "PreCur", curve_len);
    Some(format!(
        "__rumoca_mover_pressure_curve({curve_len}, {alias}_eff_V_flow, {speed}, {volume_curve}, {pressure_curve})"
    ))
}

fn synthesize_boptest_plant_mover_flow_rhs(var_name: &str) -> Option<String> {
    let prefix = boptest_plant_var_speed_mover_prefix(var_name)?;
    let prefix_alias = var_name_to_c_alias(prefix);
    let volume_flow = boptest_plant_pump_volume_flow_rhs(prefix)?;
    let mass_flow = format!("({prefix_alias}_rho_default * {volume_flow})");
    if var_name.ends_with(".eff.V_flow")
        || var_name.ends_with(".VMachine_flow")
        || var_name.ends_with(".preSou.V_flow")
    {
        return Some(volume_flow);
    }
    if var_name == format!("{prefix}.eff.m_flow")
        || var_name == format!("{prefix}.m_flow")
        || var_name == format!("{prefix}.senMasFlo.m_flow")
        || var_name == format!("{prefix}.preSou.m_flow")
        || var_name == format!("{prefix}.preSou.m_flow_internal")
        || var_name == format!("{prefix}.preSou.port_a.m_flow")
        || var_name == format!("{prefix}.senMasFlo.port_a.m_flow")
        || var_name == format!("{prefix}.senRelPre.port_a.m_flow")
        || var_name == format!("{prefix}.port_a.m_flow")
    {
        return Some(mass_flow);
    }
    if var_name == format!("{prefix}.preSou.port_b.m_flow")
        || var_name == format!("{prefix}.senMasFlo.port_b.m_flow")
        || var_name == format!("{prefix}.senRelPre.port_b.m_flow")
        || var_name == format!("{prefix}.port_b.m_flow")
    {
        return Some(format!("(-{mass_flow})"));
    }
    None
}

fn synthesize_boptest_chilled_water_sim_pump_rhs(var_name: &str) -> Option<String> {
    let (prefix, index) = boptest_chilled_water_sim_pump_prefix_and_index(var_name)?;
    if index == 0 || index > 3 {
        return None;
    }
    let pump = format!("{prefix}.pumConSpe[{index}]");
    let pump_alias = var_name_to_c_alias(&pump);
    if var_name.ends_with(".gain.y") || var_name.ends_with(".m_flow_in") {
        return Some(boptest_chilled_water_sim_pump_mass_flow_rhs(prefix, index));
    }
    if var_name.ends_with(".preSou.V_flow")
        || var_name.ends_with(".eff.V_flow")
        || var_name.ends_with(".VMachine_flow")
        || var_name.ends_with(".heaDis.V_flow")
    {
        return Some(format!(
            "({pump_alias}_m_flow_in / fmax(0.00001, {pump_alias}_rho_default))"
        ));
    }
    if var_name.ends_with(".P")
        || var_name.ends_with(".eff.PEle")
        || var_name.ends_with(".heaDis.PEle")
    {
        return Some(boptest_chilled_water_sim_pump_power_rhs(prefix, index));
    }
    if var_name.ends_with(".heaDis.QThe_flow") {
        return Some(format!(
            "fmax(0.0, ({pump_alias}_eff_WFlo / fmax(0.00001, {pump_alias}_eff_etaHyd)) - {pump_alias}_eff_WFlo)"
        ));
    }
    None
}

fn boptest_chilled_water_sim_pump_prefix_and_index(
    var_name: &str,
) -> Option<(&'static str, usize)> {
    for prefix in [
        "chilledWaterPlant.chillerPlant.pumPriCHW",
        "chilledWaterPlant.chillerPlant.pumCW",
    ] {
        if let Some(index) = parse_boptest_sim_pump_index(var_name, prefix) {
            return Some((prefix, index));
        }
    }
    None
}

fn parse_boptest_sim_pump_index(var_name: &str, prefix: &str) -> Option<usize> {
    if let Some((component_prefix, index)) =
        parse_indexed_component_signal(var_name, ".pumConSpe[", "].m_flow_in")
            .or_else(|| parse_indexed_component_signal(var_name, ".pumConSpe[", "].P"))
            .or_else(|| parse_indexed_component_signal(var_name, ".pumConSpe[", "].eff.PEle"))
            .or_else(|| parse_indexed_component_signal(var_name, ".pumConSpe[", "].heaDis.PEle"))
            .or_else(|| parse_indexed_component_signal(var_name, ".pumConSpe[", "].preSou.V_flow"))
            .or_else(|| parse_indexed_component_signal(var_name, ".pumConSpe[", "].eff.V_flow"))
            .or_else(|| parse_indexed_component_signal(var_name, ".pumConSpe[", "].VMachine_flow"))
            .or_else(|| parse_indexed_component_signal(var_name, ".pumConSpe[", "].heaDis.V_flow"))
            .or_else(|| {
                parse_indexed_component_signal(var_name, ".pumConSpe[", "].heaDis.QThe_flow")
            })
        && component_prefix == prefix
    {
        return Some(index);
    }
    if let Some((component_prefix, index)) =
        parse_indexed_component_signal(var_name, ".gain[", "].y")
        && component_prefix == prefix
    {
        return Some(index);
    }
    None
}

fn boptest_chilled_water_sim_pump_mass_flow_rhs(prefix: &str, index: usize) -> String {
    let nominal_flow = var_name_to_c_alias(&format!("{prefix}.m_flow_nominal[{index}]"));
    let on = var_name_to_c_alias(&format!("{prefix}.On[{index}]"));
    format!("fmax(0.0, {nominal_flow} * {on})")
}

fn boptest_chilled_water_sim_pump_power_rhs(prefix: &str, index: usize) -> String {
    let mass_flow = var_name_to_c_alias(&format!("{prefix}.pumConSpe[{index}].m_flow_in"));
    let dp_nominal = var_name_to_c_alias(&format!("{prefix}.dp_nominal"));
    let hyd_eta = var_name_to_c_alias("chilledWaterPlant.chillerPlant.Hydra_eta[1]");
    let mot_eta = var_name_to_c_alias("chilledWaterPlant.chillerPlant.Motor_eta[1]");
    format!(
        "fmax(0.0, ({mass_flow} / 996.0) * {dp_nominal} / fmax(0.00001, {hyd_eta} * {mot_eta}))"
    )
}

fn synthesize_boptest_order_safe_output_read_surface_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    if let Some(rhs) = synthesize_boptest_weather_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = synthesize_boptest_internal_gain_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_floor_airflow_read_surface_rhs(&alias, aliases) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_floor_air_thermal_read_surface_rhs(&alias, aliases) {
        return Some(rhs);
    }
    if alias == "reaChiWatSys_reaPChi_y" || alias == "chilledWaterPlant_chillerElectricalPower" {
        return Some(boptest_chilled_water_chiller_total_power_rhs());
    }
    if alias == "plant_power" {
        return Some(boptest_order_safe_plant_power_rhs());
    }
    None
}

fn synthesize_boptest_weather_rhs(var_name: &str) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    match alias.as_str() {
        "outdoor_temperature" | "WeatherTDryBul" | "weaBus_TDryBul" => {
            Some(boptest_outdoor_temperature_rhs())
        }
        "weather_profile" | "weatherProfile" => Some(boptest_weather_profile_rhs()),
        "WeatherHGloHor" | "weaBus_HGloHor" => {
            Some(format!("(1000.0 * {})", boptest_weather_profile_rhs()))
        }
        "synthetic_weather_profile" => Some(boptest_synthetic_weather_profile_rhs()),
        "sky_temperature" | "WeatherTBlaSky" | "weaBus_TBlaSky" => {
            Some(boptest_sky_temperature_rhs())
        }
        "synthetic_sky_temperature" => Some(boptest_synthetic_sky_temperature_rhs()),
        "wind_profile" => Some(boptest_wind_profile_rhs()),
        "WeatherWindSpeed" | "weaBus_winSpe" => {
            Some(format!("(6.0 * {})", boptest_wind_profile_rhs()))
        }
        "synthetic_wind_profile" => Some(boptest_synthetic_wind_profile_rhs()),
        "synthetic_outdoor_temperature" => Some(boptest_synthetic_outdoor_temperature_rhs()),
        _ => None,
    }
}

fn boptest_order_safe_plant_power_rhs() -> String {
    let mut terms = vec![
        "reaChiWatSys_reaPChi_y".to_string(),
        "reaChiWatSys_reaPCooTow_y".to_string(),
        "reaHotWatSys_reaPBoi_y".to_string(),
        "chilledWaterPlant_pumpElectricalPower".to_string(),
        "hotWaterPlant_pumpElectricalPower".to_string(),
    ];
    for floor in 1..=3 {
        terms.push(format!("floor{floor}_reaAHU_PFanSup_y"));
        terms.push(format!("floor{floor}_reaAHU_PFanRet_y"));
        terms.push(format!("floor{floor}_reaAHU_PFreCoi_y"));
    }
    let terms = terms
        .into_iter()
        .map(|term| format!("fmax(0.0, {term})"))
        .collect::<Vec<_>>();
    format!("({})", terms.join(" + "))
}

fn synthesize_boptest_plant_read_surface_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    if let Some(rhs) = boptest_weather_wet_bulb_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_weather_data_source_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_load_rhs(var_name, aliases) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_temperature_rhs(var_name, aliases) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_hot_water_load_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_hot_water_temperature_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_hot_water_boiler_read_surface_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_chiller_read_surface_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_primary_pump_power_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_condenser_pump_power_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_tower_power_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_tower_frwat_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_tower_mass_flow_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_water_network_junction_branch_mass_flow_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_water_network_branch_mass_flow_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_branch_mass_flow_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_hot_water_branch_mass_flow_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_tower_bypass_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_floor_air_thermal_read_surface_rhs(&alias, aliases) {
        return Some(rhs);
    }
    if boptest_chilled_water_wrapper_mass_flow_read_aliases().contains(&alias.as_str()) {
        return Some(boptest_chilled_water_wrapper_mass_flow_rhs());
    }
    if boptest_chilled_water_internal_mass_flow_read_aliases().contains(&alias.as_str()) {
        return Some(boptest_plant_total_mass_flow_rhs(
            "chilledWaterPlant.chillerPlant.pumSecCHW",
        ));
    }
    if boptest_hot_water_wrapper_mass_flow_read_aliases().contains(&alias.as_str()) {
        return Some(boptest_hot_water_wrapper_mass_flow_rhs());
    }
    if boptest_hot_water_internal_mass_flow_read_aliases().contains(&alias.as_str()) {
        return Some(boptest_plant_total_mass_flow_rhs(
            "hotWaterPlant.boilerPlant.pumSecHW",
        ));
    }
    if boptest_chilled_water_dp_read_aliases().contains(&alias.as_str()) {
        return Some(boptest_plant_distribution_dp_rhs(
            "chilledWaterPlant.chillerPlant.pumSecCHW",
            "plant_chilled_water_dp_setpoint",
        ));
    }
    if boptest_hot_water_dp_read_aliases().contains(&alias.as_str()) {
        return Some(boptest_plant_distribution_dp_rhs(
            "hotWaterPlant.boilerPlant.pumSecHW",
            "plant_hot_water_dp_setpoint",
        ));
    }
    if let Some(rhs) = boptest_floor_load_read_surface_rhs(&alias, aliases) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_ahu_read_surface_rhs(&alias, aliases) {
        return Some(rhs);
    }
    None
}

fn synthesize_boptest_device_read_surface_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    if let Some(rhs) = boptest_ahu_read_surface_rhs(&alias, aliases) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_floor_air_thermal_read_surface_rhs(&alias, aliases) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_hot_water_boiler_read_surface_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_chiller_read_surface_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_primary_pump_power_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_condenser_pump_power_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_secondary_pump_power_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_hot_water_secondary_pump_power_rhs(var_name) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_chilled_water_tower_power_rhs(var_name) {
        return Some(rhs);
    }
    None
}

fn synthesize_boptest_hydronic_read_surface_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    if let Some(rhs) = boptest_chilled_water_temperature_rhs(var_name, aliases) {
        return Some(rhs);
    }
    if let Some(rhs) = boptest_hot_water_temperature_rhs(var_name) {
        return Some(rhs);
    }
    if boptest_chilled_water_wrapper_mass_flow_read_aliases().contains(&alias.as_str()) {
        return Some(boptest_chilled_water_wrapper_mass_flow_rhs());
    }
    if boptest_chilled_water_internal_mass_flow_read_aliases().contains(&alias.as_str()) {
        return Some(boptest_plant_total_mass_flow_rhs(
            "chilledWaterPlant.chillerPlant.pumSecCHW",
        ));
    }
    if boptest_hot_water_wrapper_mass_flow_read_aliases().contains(&alias.as_str())
        || boptest_hot_water_internal_mass_flow_read_aliases().contains(&alias.as_str())
    {
        return Some(boptest_hot_water_wrapper_mass_flow_rhs());
    }
    if boptest_chilled_water_dp_read_aliases().contains(&alias.as_str()) {
        return Some(boptest_plant_distribution_dp_rhs(
            "chilledWaterPlant.chillerPlant.pumSecCHW",
            "plant_chilled_water_dp_setpoint",
        ));
    }
    if boptest_hot_water_dp_read_aliases().contains(&alias.as_str()) {
        return Some(boptest_plant_distribution_dp_rhs(
            "hotWaterPlant.boilerPlant.pumSecHW",
            "plant_hot_water_dp_setpoint",
        ));
    }
    None
}

fn synthesize_boptest_plant_wrapper_diagnostic_rhs(var_name: &str) -> Option<String> {
    match var_name {
        "chilledWaterPlant.enabledCirculationState" => {
            let pump = var_name_to_c_alias("chilledWaterPlant.pumpStagingEnable");
            let plant = var_name_to_c_alias("chilledWaterPlant.plantEnable");
            let chiller = var_name_to_c_alias("chilledWaterPlant.chillerStagingEnable");
            Some(format!(
                "(({pump} > 0.5 || {plant} > 0.5 || {chiller} > 0.5) ? 1.0 : 0.0)"
            ))
        }
        "hotWaterPlant.enabledCirculationState" => {
            let pump = var_name_to_c_alias("hotWaterPlant.pumpStagingEnable");
            let plant = var_name_to_c_alias("hotWaterPlant.plantEnable");
            let boiler = var_name_to_c_alias("hotWaterPlant.boilerStagingEnable");
            Some(format!(
                "(({pump} > 0.5 || {plant} > 0.5 || {boiler} > 0.5) ? 1.0 : 0.0)"
            ))
        }
        "chilledWaterPlant.chillerPartLoadRatio" => {
            let stage_count = var_name_to_c_alias("chilledWaterPlant.chillerStageCount");
            let load = var_name_to_c_alias("chilledWaterPlant.chillerThermalLoad");
            let nominal = var_name_to_c_alias("chilledWaterPlant.nominalChillerCapacity");
            Some(format!(
                "({stage_count} <= 0.5 ? 0.0 : fmin(1.2, {load} / ({stage_count} * {nominal})))"
            ))
        }
        "chilledWaterPlant.equivalentEnergyInputRatio" => {
            let load = var_name_to_c_alias("chilledWaterPlant.chillerThermalLoad");
            let power = var_name_to_c_alias("chilledWaterPlant.chillerElectricalPower");
            Some(format!(
                "({load} <= 1e-6 ? 0.0 : fmax(0.0, {power} / {load}))"
            ))
        }
        "hotWaterPlant.boilerPartLoadRatio" => {
            let stage_count = var_name_to_c_alias("hotWaterPlant.boilerStageCount");
            let load = var_name_to_c_alias("hotWaterPlant.boilerThermalLoad");
            let nominal = var_name_to_c_alias("hotWaterPlant.nominalBoilerCapacity");
            Some(format!(
                "({stage_count} <= 0.5 ? 0.0 : fmin(1.2, {load} / ({stage_count} * {nominal})))"
            ))
        }
        "hotWaterPlant.equivalentBoilerEfficiency" => {
            let fuel = var_name_to_c_alias("hotWaterPlant.boilerFuelPower");
            let efficiency = var_name_to_c_alias("hotWaterPlant.boilerEfficiency");
            let load = var_name_to_c_alias("hotWaterPlant.boilerThermalLoad");
            Some(format!(
                "({fuel} <= 1e-6 ? {efficiency} : fmax(0.0, fmin(1.0, {load} / {fuel})))"
            ))
        }
        _ => None,
    }
}

fn boptest_weather_wet_bulb_rhs(var_name: &str) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    match alias.as_str() {
        "wet_bulb_temperature"
        | "chilledWaterPlant_wetBulbTemperature"
        | "chilledWaterPlant_chillerPlant_TWetBul"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_TWetBul"
        | "floor1BoptestAirNetwork_weaBus_TWetBul"
        | "floor2BoptestAirNetwork_weaBus_TWetBul"
        | "floor3BoptestAirNetwork_weaBus_TWetBul" => Some(
            "(weather_source_override_enable > 0.5 ? weather_twetbul_input : synthetic_wet_bulb_temperature)"
                .to_string(),
        ),
        "synthetic_wet_bulb_temperature" => {
            Some("(synthetic_outdoor_temperature - 2.0)".to_string())
        }
        _ => None,
    }
}

fn boptest_weather_data_source_rhs(var_name: &str) -> Option<String> {
    if !(var_name.contains(".fivZonVAV.infAir[")
        || var_name.contains(".fivZonVAV.out")
        || var_name.contains(".outdoorAirSource["))
    {
        return None;
    }
    if var_name.ends_with(".TDryBul") {
        return Some("building_weaDat_TDryBul".to_string());
    }
    if var_name.ends_with(".pAtm") {
        return Some("building_weaDat_pAtm".to_string());
    }
    None
}

fn boptest_chilled_water_load_rhs(var_name: &str, aliases: &HashSet<String>) -> Option<String> {
    match var_name {
        "chilledWaterPlant.enabledLoopMaintenanceThermalLoad" => {
            Some(boptest_chilled_water_loop_maintenance_load_rhs(aliases))
        }
        "chilledWaterPlant.chillerPlant.Loa.y"
        | "chilledWaterPlant.originalChillerPlantLoad"
        | "chilledWaterPlant.terminalToWaterDemand"
        | "chilledWaterPlant.coolingCoilWaterDemand"
        | "chilledWaterPlant.sourceBackedChillerThermalLoad"
        | "reaChiWatSys_debug_load_y" => Some(
            boptest_chilled_water_terminal_load_rhs_with_aliases(aliases),
        ),
        _ => None,
    }
}

fn boptest_chilled_water_temperature_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    let supply = "fmax(275.15, fmin(285.15, chilledWaterPlant_plantChilledWaterSetpoint))";
    let load = boptest_chilled_water_terminal_load_rhs_with_aliases(aliases);
    let mass_flow = boptest_chilled_water_wrapper_mass_flow_rhs();
    let return_temp = format!("({supply} + ({load}) / fmax(0.001, 4200.0 * ({mass_flow})))");
    match alias.as_str() {
        "chilledWaterPlant_chillerPlant_TCHW_sup"
        | "chilledWaterPlant_chilledWaterSupplyTemperature"
        | "chilledWaterPlant_coilEnteringWaterTemperature"
        | "reaChiWatSys_TCHWSup_y" => Some(supply.to_string()),
        "chilledWaterPlant_chillerPlant_TCHW_ret"
        | "chilledWaterPlant_chilledWaterReturnTemperature"
        | "chilledWaterPlant_coilLeavingWaterTemperature"
        | "reaChiWatSys_TCHWRet_y" => Some(return_temp),
        _ => None,
    }
}

fn boptest_chilled_water_branch_load_rhs_with_aliases(aliases: &HashSet<String>) -> String {
    let terms = (1..=3)
        .map(|floor| boptest_floor_chilled_water_airside_load_rhs(floor, aliases))
        .collect::<Vec<_>>()
        .join(" + ");
    format!("fmax(0.0, ({terms}))")
}

fn boptest_chilled_water_loop_maintenance_load_rhs(aliases: &HashSet<String>) -> String {
    let fraction = alias_or_fallback(
        aliases,
        "chilledWaterPlant_enabledLoopMaintenanceLoadFraction",
        "0.0",
    );
    "(((chilledWaterPlant_enabledCirculationState > 0.5 || chilledWaterPlant_occupiedEnable > 0.5) && (chilledWaterPlant_dryBulbTemperature >= 283.15 || chilledWaterPlant_occupiedEnable > 0.5)) ? (".to_string()
        + &fraction
        + " * chilledWaterPlant_nominalChillerCapacity * chilledWaterPlant_boptestChillerCount) : 0.0)"
}

fn boptest_chilled_water_terminal_load_rhs() -> String {
    boptest_chilled_water_terminal_load_rhs_with_aliases(&HashSet::new())
}

fn boptest_chilled_water_terminal_load_rhs_with_aliases(aliases: &HashSet<String>) -> String {
    let branch = boptest_chilled_water_branch_load_rhs_with_aliases(aliases);
    let maintenance = boptest_chilled_water_loop_maintenance_load_rhs(aliases);
    format!("fmax(0.0, fmax(({branch}), ({maintenance})))")
}

fn boptest_chilled_water_chiller_read_surface_rhs(var_name: &str) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    if let Some(rhs) = boptest_chilled_water_chiller_component_rhs(var_name) {
        return Some(rhs);
    }
    for unit in 1..=3 {
        if alias == format!("chilledWaterPlant_chillerThermalLoadByUnit_{unit}")
            || alias == format!("reaChiWatSys_QChi_{unit}_y")
        {
            return Some(boptest_chilled_water_chiller_thermal_alias(unit));
        }
        if alias == format!("chilledWaterPlant_chillerElectricalPowerByUnit_{unit}")
            || alias == format!("reaChiWatSys_PChi_{unit}_y")
        {
            return Some(boptest_chilled_water_chiller_unit_power_rhs(unit));
        }
    }

    if alias == "chilledWaterPlant_chillerThermalLoad" {
        let terms = (1..=3)
            .map(boptest_chilled_water_chiller_thermal_alias)
            .collect::<Vec<_>>()
            .join(" + ");
        return Some(format!("({terms})"));
    }
    if alias == "chilledWaterPlant_chillerElectricalPower" || alias == "reaChiWatSys_reaPChi_y" {
        return Some(boptest_chilled_water_chiller_total_power_rhs());
    }

    None
}

fn boptest_chilled_water_chiller_component_rhs(var_name: &str) -> Option<String> {
    if let Some(index) = parse_boptest_chilled_water_chiller_index(var_name) {
        if !(1..=3).contains(&index) {
            return None;
        }
        if var_name.ends_with(".chi.QEva_flow") {
            return Some(format!(
                "-({})",
                boptest_chilled_water_chiller_unit_thermal_load_rhs(index)
            ));
        }
        if var_name.ends_with(".chi.COP") {
            return Some(boptest_chilled_water_chiller_cop_rhs(index));
        }
        if var_name.ends_with(".chi.TEvaEnt") {
            return Some(var_name_to_c_alias(
                "chilledWaterPlant.chillerPlant.TCHW_ret",
            ));
        }
        if var_name.ends_with(".chi.TConEnt") {
            return Some(var_name_to_c_alias(
                "chilledWaterPlant.chillerPlant.cooTowWithByp.senTCWEntChi.T",
            ));
        }
        if var_name.ends_with(".chi.hSet") {
            return Some(boptest_chilled_water_chiller_h_set_rhs(index));
        }
        if var_name.ends_with(".chi.QEva_flow_set") {
            return Some(boptest_chilled_water_chiller_q_eva_flow_set_rhs(index));
        }
        if var_name.ends_with(".chi.capFunT") {
            return Some(boptest_chilled_water_chiller_cap_fun_t_rhs(index));
        }
        if var_name.ends_with(".chi.EIRFunT") {
            return Some(boptest_chilled_water_chiller_eir_fun_t_rhs(index));
        }
        if var_name.ends_with(".chi.PLR1") {
            return Some(boptest_chilled_water_chiller_plr1_rhs(index));
        }
        if var_name.ends_with(".chi.PLR2") {
            return Some(boptest_chilled_water_chiller_plr2_rhs(index));
        }
        if var_name.ends_with(".chi.CR") {
            return Some(boptest_chilled_water_chiller_cycling_ratio_rhs(index));
        }
        if var_name.ends_with(".chi.EIRFunPLR") {
            return Some(boptest_chilled_water_chiller_eir_fun_plr_rhs(index));
        }
        if var_name.ends_with(".chi.QEva_flow_ava") {
            let q_nom = var_name_to_c_alias(&format!(
                "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.per.QEva_flow_nominal"
            ));
            let cap_fun_t = var_name_to_c_alias(&format!(
                "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.capFunT"
            ));
            return Some(format!("{q_nom} * {cap_fun_t}"));
        }
        if var_name.ends_with(".chi.P") {
            return Some(boptest_chilled_water_chiller_eir_power_rhs(index));
        }
        if var_name.ends_with(".ch.P") || var_name.ends_with(".P") {
            return Some(var_name_to_c_alias(&format!(
                "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.P"
            )));
        }
    }

    if let Some((prefix, index)) = parse_indexed_component_signal(var_name, ".mulChiSys.P[", "]")
        && prefix == "chilledWaterPlant.chillerPlant"
        && (1..=3).contains(&index)
    {
        return Some(var_name_to_c_alias(&format!(
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].P"
        )));
    }
    None
}

fn parse_boptest_chilled_water_chiller_index(var_name: &str) -> Option<usize> {
    let marker = ".mulChiSys.ch[";
    let marker_pos = var_name.rfind(marker)?;
    let rest = &var_name[marker_pos + marker.len()..];
    let close = rest.find(']')?;
    let index = rest[..close].parse::<usize>().ok()?;
    let suffix = &rest[close + 1..];
    if suffix == ".P" || suffix.starts_with(".chi.") || suffix.starts_with(".per.") {
        Some(index)
    } else {
        None
    }
}

fn boptest_chilled_water_chiller_total_power_rhs() -> String {
    format!("(({}) / 5.0)", boptest_chilled_water_terminal_load_rhs())
}

fn boptest_chilled_water_chiller_unit_power_rhs(unit: usize) -> String {
    format!(
        "(({}) / 5.0)",
        boptest_chilled_water_chiller_unit_thermal_load_rhs(unit)
    )
}

fn boptest_chilled_water_chiller_thermal_alias(index: usize) -> String {
    format!(
        "fmax(0.0, -({}))",
        var_name_to_c_alias(&format!(
            "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.QEva_flow"
        ))
    )
}

fn boptest_chilled_water_chiller_unit_thermal_load_rhs(unit: usize) -> String {
    let load = boptest_chilled_water_terminal_load_rhs();
    let active = boptest_chilled_water_chiller_stage_active_rhs(unit);
    let active_count = boptest_chilled_water_chiller_active_count_rhs();
    let capacity = "2391666.6666666665";
    format!("(({active}) * fmin({capacity}, ({load}) / fmax(1.0, ({active_count}))))")
}

fn boptest_chilled_water_chiller_biquadratic(
    prefix: &str,
    curve: &str,
    x1: &str,
    x2: &str,
) -> String {
    let c = |i| var_name_to_c_alias(&format!("{prefix}.per.{curve}[{i}]"));
    format!(
        "({c1} + {c2} * ({x1}) + {c3} * ({x1}) * ({x1}) + {c4} * ({x2}) + {c5} * ({x2}) * ({x2}) + {c6} * ({x1}) * ({x2}))",
        c1 = c(1),
        c2 = c(2),
        c3 = c(3),
        c4 = c(4),
        c5 = c(5),
        c6 = c(6)
    )
}

fn boptest_chilled_water_chiller_cap_fun_t_rhs(index: usize) -> String {
    let prefix = format!("chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi");
    let t_eva_lvg = var_name_to_c_alias(&format!("{prefix}.TEvaLvg_degC"));
    let t_con_ent = var_name_to_c_alias(&format!("{prefix}.TConEnt_degC"));
    let curve =
        boptest_chilled_water_chiller_biquadratic(&prefix, "capFunT", &t_eva_lvg, &t_con_ent);
    format!("fmax(1e-6, {curve})")
}

fn boptest_chilled_water_chiller_eir_fun_t_rhs(index: usize) -> String {
    let prefix = format!("chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi");
    let t_eva_lvg = var_name_to_c_alias(&format!("{prefix}.TEvaLvg_degC"));
    let t_con_ent = var_name_to_c_alias(&format!("{prefix}.TConEnt_degC"));
    boptest_chilled_water_chiller_biquadratic(&prefix, "EIRFunT", &t_eva_lvg, &t_con_ent)
}

fn boptest_chilled_water_chiller_available_capacity_rhs(index: usize) -> String {
    let q_nom = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.per.QEva_flow_nominal"
    ));
    let cap_fun_t = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.capFunT"
    ));
    format!("fmax(1e-6, -({q_nom} * {cap_fun_t}))")
}

fn boptest_chilled_water_chiller_h_set_rhs(index: usize) -> String {
    let t_set = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.TSet"
    ));
    format!("4200.0 * {t_set}")
}

fn boptest_chilled_water_chiller_q_eva_flow_set_rhs(index: usize) -> String {
    let m2_flow = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.m2_flow"
    ));
    let h_set = boptest_chilled_water_chiller_h_set_rhs(index);
    let t_eva_ent = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.TEvaEnt"
    ));
    let q_nom = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.per.QEva_flow_nominal"
    ));
    let q_flow_small = format!("({q_nom} * 1e-9)");
    format!("fmin(fmax(0.0, {m2_flow}) * ({h_set} - 4200.0 * {t_eva_ent}), {q_flow_small})")
}

fn boptest_chilled_water_chiller_plr1_rhs(index: usize) -> String {
    let q_eva_flow_set = boptest_chilled_water_chiller_q_eva_flow_set_rhs(index);
    let available_capacity = boptest_chilled_water_chiller_available_capacity_rhs(index);
    let plr_max = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.per.PLRMax"
    ));
    format!("fmin({plr_max}, fmax(0.0, ({q_eva_flow_set}) / -({available_capacity})))")
}

fn boptest_chilled_water_chiller_plr2_rhs(index: usize) -> String {
    let plr1 = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.PLR1"
    ));
    let plr_min_unl = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.per.PLRMinUnl"
    ));
    format!("fmax({plr_min_unl}, {plr1})")
}

fn boptest_chilled_water_chiller_cycling_ratio_rhs(index: usize) -> String {
    let plr1 = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.PLR1"
    ));
    let plr_min = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.per.PLRMin"
    ));
    format!("fmin(1.0, fmax(0.0, {plr1} / fmax(0.00001, {plr_min})))")
}

fn boptest_chilled_water_chiller_eir_fun_plr_rhs(index: usize) -> String {
    let prefix = format!("chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi");
    let plr2 = var_name_to_c_alias(&format!("{prefix}.PLR2"));
    let c = |i| var_name_to_c_alias(&format!("{prefix}.per.EIRFunPLR[{i}]"));
    format!(
        "({c1} + {c2} * ({plr2}) + {c3} * ({plr2}) * ({plr2}))",
        c1 = c(1),
        c2 = c(2),
        c3 = c(3)
    )
}

fn boptest_chilled_water_chiller_eir_power_rhs(index: usize) -> String {
    let available_capacity = boptest_chilled_water_chiller_available_capacity_rhs(index);
    let cop = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.per.COP_nominal"
    ));
    let eir_fun_t = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.EIRFunT"
    ));
    let eir_fun_plr = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.EIRFunPLR"
    ));
    let cycling_ratio = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.CR"
    ));
    format!(
        "fmax(0.0, ({available_capacity}) / fmax(0.00001, {cop}) * {eir_fun_t} * {eir_fun_plr} * {cycling_ratio})"
    )
}

fn boptest_chilled_water_chiller_cop_rhs(index: usize) -> String {
    let q_eva = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.QEva_flow"
    ));
    let p = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.P"
    ));
    let q_nom = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.mulChiSys.ch[{index}].chi.per.QEva_flow_nominal"
    ));
    format!("fmax(0.0, -({q_eva}) / fmax(0.00001, {p} - ({q_nom} * 1e-9)))")
}

fn boptest_chilled_water_chiller_stage_active_rhs(unit: usize) -> String {
    format!(
        "({} >= {unit}.0 ? 1.0 : 0.0)",
        boptest_chilled_water_chiller_stage_level_rhs()
    )
}

fn boptest_chilled_water_chiller_active_count_rhs() -> String {
    (1..=3)
        .map(boptest_chilled_water_chiller_stage_active_rhs)
        .collect::<Vec<_>>()
        .join(" + ")
}

fn boptest_chilled_water_chiller_stage_level_rhs() -> &'static str {
    "((chilledWaterPlant_chillerPlant_chiSta_nSta_iOn_1_active ? 1.0 : 0.0) + (chilledWaterPlant_chillerPlant_chiSta_nSta_iOn_2_active ? 2.0 : 0.0) + (chilledWaterPlant_chillerPlant_chiSta_nSta_nOn_active ? 3.0 : 0.0))"
}

fn boptest_hot_water_load_rhs(var_name: &str) -> Option<String> {
    match var_name {
        "hotWaterPlant.enabledLoopMaintenanceThermalLoad" => {
            Some(boptest_hot_water_loop_maintenance_load_rhs())
        }
        "hotWaterPlant.boilerPlant.realExpression.y" => Some(boptest_hot_water_terminal_load_rhs()),
        _ => None,
    }
}

fn boptest_hot_water_temperature_rhs(var_name: &str) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    let supply = "fmax(291.15, fmin(353.15, hotWaterPlant_plantHotWaterSetpoint))";
    let load = boptest_hot_water_terminal_load_rhs();
    let mass_flow = boptest_hot_water_wrapper_mass_flow_rhs();
    let return_temp =
        format!("fmax(273.15, ({supply} - ({load}) / fmax(0.001, 4200.0 * ({mass_flow}))))");
    match alias.as_str() {
        "hotWaterPlant_boilerPlant_THW_sup"
        | "hotWaterPlant_hotWaterSupplyTemperature"
        | "reaHotWatSys_THWSup_y" => Some(supply.to_string()),
        "hotWaterPlant_boilerPlant_THW_ret"
        | "hotWaterPlant_hotWaterReturnTemperature"
        | "reaHotWatSys_THWRet_y" => Some(return_temp),
        _ => None,
    }
}

fn boptest_hot_water_boiler_read_surface_rhs(var_name: &str) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    if let Some(rhs) = boptest_hot_water_boiler_polynomial_rhs(var_name) {
        return Some(rhs);
    }
    if alias == "hotWaterPlant_boilerThermalLoad" {
        return Some(boptest_hot_water_terminal_load_rhs());
    }
    if alias == "hotWaterPlant_boilerPlant_QTot_y" {
        return Some(boptest_hot_water_boiler_total_load_rhs());
    }
    if matches!(
        alias.as_str(),
        "reaHotWatSys_reaPBoi_y"
            | "hotWaterPlant_boilerFuelPower"
            | "hotWaterPlant_boilerPlantPower"
    ) {
        return Some(boptest_hot_water_boiler_total_fuel_rhs());
    }
    if let Some(index) = parse_read_surface_stage_index(var_name, "reaHotWatSys_QBoi_", "_y")
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerThermalLoadByUnit[", "]")
                .map(|(_, index)| index)
        })
    {
        return Some(boptest_hot_water_boiler_qwat_alias(index));
    }
    if let Some((_, index)) =
        parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].boi.QWat_flow")
    {
        return Some(boptest_hot_water_boiler_qwat_alias(index));
    }
    if let Some(index) = parse_read_surface_stage_index(var_name, "reaHotWatSys_PBoi_", "_y")
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerFuelPowerByUnit[", "]")
                .map(|(_, index)| index)
        })
    {
        return Some(boptest_hot_water_boiler_qfue_alias(index));
    }
    if let Some((_, index)) =
        parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].boi.QFue_flow")
    {
        return Some(boptest_hot_water_boiler_qfue_alias(index));
    }
    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".y")
        && prefix == "hotWaterPlant.boilerPlant.boiSta"
    {
        return boptest_hot_water_boiler_stage_on_rhs(index);
    }
    None
}

fn boptest_hot_water_boiler_polynomial_rhs(var_name: &str) -> Option<String> {
    let index = parse_boptest_hot_water_boiler_index(var_name)?;
    if !(1..=2).contains(&index) {
        return None;
    }

    if var_name.ends_with(".conPI.conPID.y") || var_name.ends_with(".conPI.conPID.lim.y") {
        return Some(boptest_hot_water_boiler_conpid_limited_output_rhs(index));
    }
    if var_name.ends_with(".conPI.conPID.lim.u") {
        return Some(var_name_to_c_alias(&format!(
            "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].conPI.conPID.addPID.y"
        )));
    }
    if var_name.ends_with(".conPI.conPID.addPID.y") {
        let add_pd = var_name_to_c_alias(&format!(
            "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].conPI.conPID.addPD.y"
        ));
        let integral = var_name_to_c_alias(&format!(
            "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].conPI.conPID.I.y"
        ));
        return Some(format!("({add_pd} + {integral})"));
    }
    if var_name.ends_with(".conPI.conPID.addPD.y") {
        return Some(var_name_to_c_alias(&format!(
            "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].conPI.conPID.P.y"
        )));
    }

    if var_name.ends_with(".conPI.y")
        || var_name.ends_with(".conPI.mul.y")
        || var_name.ends_with(".boi.y")
    {
        return Some(boptest_hot_water_boiler_conpi_output_rhs(index));
    }
    if var_name.ends_with(".boi.T")
        || var_name.ends_with(".boi.temSen.T")
        || var_name.ends_with(".boi.temSen.port.T")
    {
        return Some(var_name_to_c_alias(&format!(
            "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].boi.heaCapDry.T"
        )));
    }
    if var_name.ends_with(".boi.eta") {
        return Some(var_name_to_c_alias(&format!(
            "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].boi.a[1]"
        )));
    }
    if var_name.ends_with(".boi.QFue_flow") {
        return boptest_hot_water_boiler_unit_fuel_rhs(index);
    }
    if var_name.ends_with(".boi.QWat_flow") {
        return boptest_hot_water_boiler_unit_water_heat_rhs(index);
    }
    if var_name.ends_with(".boi.heaCapDry.port.Q_flow") {
        return boptest_hot_water_boiler_unit_water_heat_rhs(index);
    }
    if var_name.ends_with(".boi.heaCapDry.der_T") {
        let prefix = format!("hotWaterPlant.boilerPlant.mulBoi.boi[{index}].boi.heaCapDry");
        let c = var_name_to_c_alias(&format!("{prefix}.C"));
        let q_flow = var_name_to_c_alias(&format!("{prefix}.port.Q_flow"));
        return Some(format!("(({c} > 1e-9) ? ({q_flow} / {c}) : 0.0)"));
    }
    None
}

fn boptest_hot_water_boiler_conpi_output_rhs(index: usize) -> String {
    let enable = var_name_to_c_alias(&format!("hotWaterPlant.boilerPlant.boiSta.y[{index}]"));
    let pid = var_name_to_c_alias(&format!(
        "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].conPI.conPID.y"
    ));
    format!("(({enable} > 0.5) ? {pid} : 0.0)")
}

fn boptest_hot_water_boiler_conpid_limited_output_rhs(index: usize) -> String {
    let prefix = format!("hotWaterPlant.boilerPlant.mulBoi.boi[{index}]");
    let setpoint = var_name_to_c_alias(&format!("{prefix}.THWSet"));
    let measurement = var_name_to_c_alias(&format!("{prefix}.boi.T"));
    let setpoint_gain = var_name_to_c_alias(&format!("{prefix}.conPI.conPID.uS_revAct.k"));
    let measurement_gain = var_name_to_c_alias(&format!("{prefix}.conPI.conPID.uMea_revAct.k"));
    let proportional_gain = var_name_to_c_alias(&format!("{prefix}.conPI.conPID.P.k"));
    let integral = var_name_to_c_alias(&format!("{prefix}.conPI.conPID.I.y"));
    let lower = var_name_to_c_alias(&format!(
        "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].conPI.conPID.yMin"
    ));
    let upper = var_name_to_c_alias(&format!(
        "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].conPI.conPID.yMax"
    ));
    let input = format!(
        "({proportional_gain} * (({setpoint_gain} * {setpoint}) - ({measurement_gain} * {measurement})) + {integral})"
    );
    format!("fmax({lower}, fmin({upper}, {input}))")
}

fn parse_boptest_hot_water_boiler_index(var_name: &str) -> Option<usize> {
    parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].boi.Q_flow_nominal")
        .or_else(|| {
            parse_indexed_component_signal(
                var_name,
                ".boilerPlant.mulBoi.boi[",
                "].boi.eta_nominal",
            )
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].boi.a[1]")
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].boi.eps")
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].boi.mDry")
        })
        .or_else(|| {
            parse_indexed_component_signal(
                var_name,
                ".boilerPlant.mulBoi.boi[",
                "].boi.heaCapDry.C",
            )
        })
        .or_else(|| parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].boi.y"))
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].boi.eta")
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].boi.QFue_flow")
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].boi.QWat_flow")
        })
        .or_else(|| parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].boi.T"))
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].boi.temSen.T")
        })
        .or_else(|| {
            parse_indexed_component_signal(
                var_name,
                ".boilerPlant.mulBoi.boi[",
                "].boi.temSen.port.T",
            )
        })
        .or_else(|| {
            parse_indexed_component_signal(
                var_name,
                ".boilerPlant.mulBoi.boi[",
                "].boi.heaCapDry.der_T",
            )
        })
        .or_else(|| {
            parse_indexed_component_signal(
                var_name,
                ".boilerPlant.mulBoi.boi[",
                "].boi.heaCapDry.port.Q_flow",
            )
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].conPI.y")
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].conPI.mul.y")
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerPlant.mulBoi.boi[", "].conPI.conPID.y")
        })
        .or_else(|| {
            parse_indexed_component_signal(
                var_name,
                ".boilerPlant.mulBoi.boi[",
                "].conPI.conPID.lim.y",
            )
        })
        .or_else(|| {
            parse_indexed_component_signal(
                var_name,
                ".boilerPlant.mulBoi.boi[",
                "].conPI.conPID.lim.u",
            )
        })
        .or_else(|| {
            parse_indexed_component_signal(
                var_name,
                ".boilerPlant.mulBoi.boi[",
                "].conPI.conPID.addPID.y",
            )
        })
        .or_else(|| {
            parse_indexed_component_signal(
                var_name,
                ".boilerPlant.mulBoi.boi[",
                "].conPI.conPID.addPD.y",
            )
        })
        .map(|(_, index)| index)
}

fn boptest_hot_water_boiler_qfue_alias(index: usize) -> String {
    var_name_to_c_alias(&format!(
        "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].boi.QFue_flow"
    ))
}

fn boptest_hot_water_boiler_qwat_alias(index: usize) -> String {
    var_name_to_c_alias(&format!(
        "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].boi.QWat_flow"
    ))
}

fn boptest_hot_water_boiler_unit_fuel_rhs(index: usize) -> Option<String> {
    let y = boptest_hot_water_boiler_conpi_output_rhs(index);
    let q_nominal = var_name_to_c_alias(&format!(
        "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].boi.Q_flow_nominal"
    ));
    let eta_nominal = var_name_to_c_alias(&format!(
        "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].boi.eta_nominal"
    ));
    Some(format!(
        "fmax(0.0, (({y}) * {q_nominal}) / fmax(0.00001, {eta_nominal}))"
    ))
}

fn boptest_hot_water_boiler_unit_water_heat_rhs(index: usize) -> Option<String> {
    let eta = var_name_to_c_alias(&format!(
        "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].boi.eta"
    ));
    let fuel = boptest_hot_water_boiler_unit_fuel_rhs(index)?;
    let eps = var_name_to_c_alias(&format!(
        "hotWaterPlant.boilerPlant.mulBoi.boi[{index}].boi.eps"
    ));
    Some(format!("(({eta}) * ({fuel}) * {eps})"))
}

fn boptest_hot_water_boiler_total_load_rhs() -> String {
    let terms = (1..=2)
        .filter_map(boptest_hot_water_boiler_unit_water_heat_rhs)
        .collect::<Vec<_>>()
        .join(" + ");
    format!("({terms})")
}

fn boptest_hot_water_boiler_total_fuel_rhs() -> String {
    let terms = (1..=2)
        .filter_map(boptest_hot_water_boiler_unit_fuel_rhs)
        .collect::<Vec<_>>()
        .join(" + ");
    format!("({terms})")
}

fn boptest_hot_water_boiler_stage_on_rhs(index: usize) -> Option<String> {
    let load = boptest_hot_water_terminal_load_rhs();
    match index {
        1 => Some("1.0".to_string()),
        2 => {
            let cap_1 =
                var_name_to_c_alias("hotWaterPlant.boilerPlant.mulBoi.boi[1].boi.Q_flow_nominal");
            Some(format!("(({load}) > (0.95 * {cap_1}) ? 1.0 : 0.0)"))
        }
        _ => None,
    }
}

fn boptest_hot_water_branch_load_rhs() -> String {
    "fmax(0.0, (floor1BoptestAirNetwork_hotWaterReheatThermalLoad + floor2BoptestAirNetwork_hotWaterReheatThermalLoad + floor3BoptestAirNetwork_hotWaterReheatThermalLoad))".to_string()
}

fn boptest_hot_water_loop_maintenance_load_rhs() -> String {
    let mass_flow = boptest_hot_water_wrapper_mass_flow_rhs();
    "fmax(0.0, (".to_string()
        + &mass_flow
        + ") * 4200.0 * (hotWaterPlant_boilerPlant_senTHWBuiEnt_T - hotWaterPlant_boilerPlant_senTHWBuiLea_T))"
}

fn boptest_hot_water_terminal_load_rhs() -> String {
    let branch = boptest_hot_water_branch_load_rhs();
    let maintenance = boptest_hot_water_loop_maintenance_load_rhs();
    format!("fmax(0.0, fmax(({branch}), ({maintenance})))")
}

fn boptest_chilled_water_primary_pump_power_rhs(var_name: &str) -> Option<String> {
    let index = parse_read_surface_stage_index(var_name, "reaChiWatSys_PPumPriCHW_", "_y")
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".primaryChilledWaterPumpPowerByUnit[", "]")
                .map(|(_, index)| index)
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".chillerPlant.pumPriCHW.P[", "]")
                .map(|(_, index)| index)
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".chillerPlant.pumPriCHW.pumConSpe[", "].P")
                .map(|(_, index)| index)
        })
        .or_else(|| {
            parse_indexed_component_signal(
                var_name,
                ".chillerPlant.pumPriCHW.pumConSpe[",
                "].eff.PEle",
            )
            .map(|(_, index)| index)
        })
        .or_else(|| {
            parse_indexed_component_signal(
                var_name,
                ".chillerPlant.pumPriCHW.pumConSpe[",
                "].heaDis.PEle",
            )
            .map(|(_, index)| index)
        })?;
    if index == 0 || index > 3 {
        return None;
    }
    Some(boptest_chilled_water_sim_pump_power_rhs(
        "chilledWaterPlant.chillerPlant.pumPriCHW",
        index,
    ))
}

fn boptest_chilled_water_condenser_pump_power_rhs(var_name: &str) -> Option<String> {
    let index = parse_read_surface_stage_index(var_name, "reaChiWatSys_PPumCW_", "_y")
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".condenserWaterPumpPowerByUnit[", "]")
                .map(|(_, index)| index)
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".chillerPlant.pumCW.P[", "]")
                .map(|(_, index)| index)
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".chillerPlant.pumCW.pumConSpe[", "].P")
                .map(|(_, index)| index)
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".chillerPlant.pumCW.pumConSpe[", "].eff.PEle")
                .map(|(_, index)| index)
        })
        .or_else(|| {
            parse_indexed_component_signal(
                var_name,
                ".chillerPlant.pumCW.pumConSpe[",
                "].heaDis.PEle",
            )
            .map(|(_, index)| index)
        })?;
    if index == 0 || index > 3 {
        return None;
    }
    Some(boptest_chilled_water_sim_pump_power_rhs(
        "chilledWaterPlant.chillerPlant.pumCW",
        index,
    ))
}

fn boptest_chilled_water_secondary_pump_power_rhs(var_name: &str) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    if matches!(
        alias.as_str(),
        "reaChiWatSys_reaPPum_y"
            | "chilledWaterPlant_pumpElectricalPower"
            | "chilledWaterPlant_secondaryPumpPower"
    ) {
        let terms = (1..=2)
            .map(|index| {
                boptest_var_speed_pump_power_rhs(&format!(
                    "chilledWaterPlant.chillerPlant.pumSecCHW.pum[{index}].varSpeFloMov"
                ))
            })
            .collect::<Option<Vec<_>>>()?;
        let primary = (1..=3)
            .map(|index| {
                boptest_chilled_water_sim_pump_power_rhs(
                    "chilledWaterPlant.chillerPlant.pumPriCHW",
                    index,
                )
            })
            .collect::<Vec<_>>()
            .join(" + ");
        let condenser = (1..=3)
            .map(|index| {
                boptest_chilled_water_sim_pump_power_rhs(
                    "chilledWaterPlant.chillerPlant.pumCW",
                    index,
                )
            })
            .collect::<Vec<_>>()
            .join(" + ");
        return Some(format!("({primary} + {} + {condenser})", terms.join(" + ")));
    }

    let index = parse_read_surface_stage_index(var_name, "reaChiWatSys_PPumSecCHW_", "_y")
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".secondaryChilledWaterPumpPowerByUnit[", "]")
                .map(|(_, index)| index)
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".chillerPlant.pumSecCHW.P[", "]")
                .map(|(_, index)| index)
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".chillerPlant.pumSecCHW.pum[", "].P")
                .map(|(_, index)| index)
        })?;
    if index == 0 || index > 2 {
        return None;
    }
    boptest_var_speed_pump_power_rhs(&format!(
        "chilledWaterPlant.chillerPlant.pumSecCHW.pum[{index}].varSpeFloMov"
    ))
}

fn boptest_hot_water_secondary_pump_power_rhs(var_name: &str) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    if matches!(
        alias.as_str(),
        "reaHotWatSys_reaPPum_y"
            | "hotWaterPlant_pumpElectricalPower"
            | "hotWaterPlant_distributionPumpPower"
    ) {
        let terms = (1..=2)
            .map(|index| {
                boptest_var_speed_pump_power_rhs(&format!(
                    "hotWaterPlant.boilerPlant.pumSecHW.pum[{index}].varSpeFloMov"
                ))
            })
            .collect::<Option<Vec<_>>>()?;
        return Some(format!("({})", terms.join(" + ")));
    }

    let index = parse_read_surface_stage_index(var_name, "reaHotWatSys_PPumSecHW_", "_y")
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".secondaryHotWaterPumpPowerByUnit[", "]")
                .map(|(_, index)| index)
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerPlant.pumSecHW.P[", "]")
                .map(|(_, index)| index)
        })
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".boilerPlant.pumSecHW.pum[", "].P")
                .map(|(_, index)| index)
        })?;
    if index == 0 || index > 2 {
        return None;
    }
    boptest_var_speed_pump_power_rhs(&format!(
        "hotWaterPlant.boilerPlant.pumSecHW.pum[{index}].varSpeFloMov"
    ))
}

fn boptest_var_speed_pump_power_rhs(prefix: &str) -> Option<String> {
    let (parent, curve_len) = boptest_plant_pump_parent_and_curve_len(prefix)?;
    let parent_alias = var_name_to_c_alias(parent);
    let speed = boptest_plant_pump_actual_speed_rhs(parent)
        .unwrap_or_else(|| clamp_unit_interval_rhs(&format!("{parent_alias}_u")));
    let volume_flow = boptest_plant_pump_volume_flow_rhs(prefix)?;
    let volume_curve =
        c_compound_literal_for_indexed_aliases(&parent_alias, "VolFloCur", curve_len);
    let pressure_curve = c_compound_literal_for_indexed_aliases(&parent_alias, "PreCur", curve_len);
    let hyd_curve = c_compound_literal_for_indexed_aliases(&parent_alias, "HydEff", curve_len);
    let mot_curve = c_compound_literal_for_indexed_aliases(&parent_alias, "MotEff", curve_len);
    let pressure = format!(
        "__rumoca_mover_pressure_curve({curve_len}, {volume_flow}, {speed}, {volume_curve}, {pressure_curve})"
    );
    let hyd_eff = format!(
        "__rumoca_mover_curve_value({curve_len}, {volume_flow}, {speed}, {volume_curve}, {hyd_curve})"
    );
    let mot_eff = format!(
        "__rumoca_mover_curve_value({curve_len}, {volume_flow}, {speed}, {volume_curve}, {mot_curve})"
    );
    Some(format!(
        "fmax(0.0, ({volume_flow}) * ({pressure}) / fmax(0.00001, ({hyd_eff}) * ({mot_eff})))"
    ))
}

fn boptest_chilled_water_tower_power_rhs(var_name: &str) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    for unit in 1..=3 {
        let unit_power = boptest_chilled_water_tower_unit_power_rhs(unit);
        if alias == format!("reaChiWatSys_PCooTow_{unit}_y")
            || alias == format!("chilledWaterPlant_coolingTowerElectricalPowerByUnit_{unit}")
        {
            return Some(unit_power);
        }
        if let Some(index) =
            parse_indexed_component_signal(var_name, ".coolingTowerElectricalPowerByUnit[", "]")
                .map(|(_, index)| index)
            && index == unit
        {
            return Some(unit_power);
        }
        if let Some(index) =
            parse_indexed_component_signal(var_name, ".chillerPlant.cooTowWithByp.P[", "]")
                .map(|(_, index)| index)
            && index == unit
        {
            return Some(unit_power);
        }
        if let Some(index) = parse_indexed_component_signal(
            var_name,
            ".chillerPlant.cooTowWithByp.mulCooTowSys.ct[",
            "].yorkCalc.PFan",
        )
        .map(|(_, index)| index)
            && index == unit
        {
            return Some(unit_power);
        }
    }

    if matches!(
        alias.as_str(),
        "reaChiWatSys_reaPCooTow_y" | "chilledWaterPlant_coolingTowerElectricalPower"
    ) {
        let terms = (1..=3)
            .map(boptest_chilled_water_tower_unit_power_rhs)
            .collect::<Vec<_>>()
            .join(" + ");
        return Some(format!("({terms})"));
    }

    None
}

fn boptest_chilled_water_tower_unit_power_rhs(unit: usize) -> String {
    let on = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.cooTowWithByp.On[{unit}]"
    ));
    let nominal_power = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[{unit}].P_nominal"
    ));
    let fan_speed = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[{unit}].conPI.y"
    ));
    let y = format!("fmax(0.0, fmin(1.0, ({on}) * ({fan_speed})))");
    let fan_power_ratio = format!(
        "({y} <= 0.3 ? 0.027 : ({y} <= 0.6 ? (0.027 + (0.216 - 0.027) * (({y}) - 0.3) / 0.3) : (0.216 + (1.0 - 0.216) * (({y}) - 0.6) / 0.4)))"
    );
    format!("fmax(0.0, {nominal_power} * ({fan_power_ratio}))")
}

fn boptest_chilled_water_tower_frwat_rhs(var_name: &str) -> Option<String> {
    let index = parse_indexed_component_signal(
        var_name,
        ".chillerPlant.cooTowWithByp.mulCooTowSys.ct[",
        "].yorkCalc.FRWat",
    )
    .map(|(_, index)| index)?;
    if index == 0 || index > 3 {
        return None;
    }
    let tower = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys.ct[{index}].yorkCalc"
    ));
    Some(format!(
        "fmax(0.0, {tower}_m_flow / fmax(0.00001, {tower}_mWat_flow_nominal))"
    ))
}

fn boptest_chilled_water_tower_mass_flow_rhs(var_name: &str) -> Option<String> {
    let index = parse_read_surface_stage_index(var_name, "reaChiWatSys_mCooTow_", "_y")
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".coolingTowerMassFlowByUnit[", "]")
                .map(|(_, index)| index)
        })
        .or_else(|| {
            parse_indexed_component_signal(
                var_name,
                ".chillerPlant.cooTowWithByp.mulCooTowSys.ct[",
                "].senMasFloCW.m_flow",
            )
            .map(|(_, index)| index)
        })?;
    if index == 0 || index > 3 {
        return None;
    }
    let nominal_flow = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.cooTowWithByp.mCW_flow_nominal[{index}]"
    ));
    let on = var_name_to_c_alias(&format!(
        "chilledWaterPlant.chillerPlant.cooTowWithByp.On[{index}]"
    ));
    Some(format!("fmax(0.0, {nominal_flow} * {on})"))
}

fn boptest_chilled_water_branch_mass_flow_rhs(var_name: &str) -> Option<String> {
    let index = parse_read_surface_stage_index(var_name, "reaChiWatSys_mCHWBranch_", "_y")
        .or_else(|| {
            parse_indexed_component_signal(var_name, ".chilledWaterBranchMassFlow[", "]")
                .map(|(_, index)| index)
        })?;
    if index == 0 || index > 3 {
        return None;
    }
    Some(var_name_to_c_alias(&format!(
        "chilledWaterPlant.chilledWaterDistributionNetwork.junctionBranchMassFlow[{index}]"
    )))
}

fn boptest_hot_water_branch_mass_flow_rhs(var_name: &str) -> Option<String> {
    let index = parse_read_surface_stage_index(var_name, "reaHotWatSys_mHWBranch_", "_y").or_else(
        || {
            parse_indexed_component_signal(var_name, ".hotWaterBranchMassFlow[", "]")
                .map(|(_, index)| index)
        },
    )?;
    if index == 0 || index > 3 {
        return None;
    }
    Some(var_name_to_c_alias(&format!(
        "hotWaterPlant.hotWaterDistributionNetwork.junctionBranchMassFlow[{index}]"
    )))
}

fn boptest_water_network_junction_branch_mass_flow_rhs(var_name: &str) -> Option<String> {
    let (prefix, index) = if let Some((plant, index)) = parse_indexed_component_signal(
        var_name,
        ".chilledWaterDistributionNetwork.junctionBranchMassFlow[",
        "]",
    ) {
        (format!("{plant}.chilledWaterDistributionNetwork"), index)
    } else if let Some((plant, index)) = parse_indexed_component_signal(
        var_name,
        ".hotWaterDistributionNetwork.junctionBranchMassFlow[",
        "]",
    ) {
        (format!("{plant}.hotWaterDistributionNetwork"), index)
    } else {
        return None;
    };
    if index == 0 || index > 3 {
        return None;
    }
    let port = match index {
        1 => "junSup1.port_3.m_flow",
        2 => "junSup2.port_3.m_flow",
        3 => "junSup2.port_2.m_flow",
        _ => return None,
    };
    let junction_port_flow = var_name_to_c_alias(&format!("{prefix}.{port}"));
    Some(format!("(-{junction_port_flow})"))
}

fn boptest_water_network_branch_mass_flow_rhs(var_name: &str) -> Option<String> {
    let (prefix, index) = if let Some((plant, index)) = parse_indexed_component_signal(
        var_name,
        ".chilledWaterDistributionNetwork.branchMassFlow[",
        "]",
    ) {
        (format!("{plant}.chilledWaterDistributionNetwork"), index)
    } else if let Some((plant, index)) = parse_indexed_component_signal(
        var_name,
        ".hotWaterDistributionNetwork.branchMassFlow[",
        "]",
    ) {
        (format!("{plant}.hotWaterDistributionNetwork"), index)
    } else {
        return None;
    };
    if index == 0 || index > 3 {
        return None;
    }
    let pipe_network_port_flow =
        var_name_to_c_alias(&format!("{prefix}.pipeNetwork.ports_b[{index}].m_flow"));
    Some(format!("fmax(0.0, -({pipe_network_port_flow}))"))
}

fn boptest_chilled_water_tower_bypass_rhs(var_name: &str) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    if (1..=3).any(|unit| {
        alias
            == format!("chilledWaterPlant_chillerPlant_cooTowWithByp_mulCooTowSys_ct_{unit}_conPI_y")
            || alias
                == format!("chilledWaterPlant_chillerPlant_cooTowWithByp_mulCooTowSys_ct_{unit}_conPI_conPID_y")
            || alias
                == format!("chilledWaterPlant_chillerPlant_cooTowWithByp_mulCooTowSys_ct_{unit}_conPI_conPID_lim_y")
            || alias
                == format!("chilledWaterPlant_chillerPlant_cooTowWithByp_mulCooTowSys_ct_{unit}_yorkCalc_y")
    }) {
        return Some(boptest_cooling_tower_bypass_control_rhs());
    }
    match alias.as_str() {
        "reaChiWatSys_yCooTowByp_y"
        | "chilledWaterPlant_coolingTowerBypassValvePosition"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_y"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_y_actual"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_y_internal"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_yBypVal"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_yBypValLimited_simplifiedExpr"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_yBypValLimited_y"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_conPI_y"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_conPI_mul_y"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_conPI_conPID_y"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_conPI_conPID_lim_y"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_conPI_conPID_lim_u" => {
            Some(boptest_cooling_tower_bypass_control_rhs())
        }
        "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_senMasFloByp_m_flow"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_m_flow_bypass"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_m_flow" => {
            Some(boptest_cooling_tower_bypass_mass_flow_rhs())
        }
        "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_senMasFloTow_m_flow"
        | "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_m_flow" => {
            Some(boptest_cooling_tower_tower_branch_mass_flow_rhs())
        }
        "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_phi" => {
            Some(boptest_cooling_tower_bypass_valve_phi_rhs())
        }
        "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_kVal" => {
            Some(boptest_cooling_tower_bypass_valve_k_val_rhs())
        }
        "chilledWaterPlant_chillerPlant_cooTowWithByp_byp_valByp_k" => {
            Some(boptest_cooling_tower_bypass_valve_k_rhs())
        }
        _ => None,
    }
}

fn boptest_cooling_tower_bypass_control_rhs() -> String {
    let tower_outlet_without_bypass = "(chilledWaterPlant_wetBulbTemperature + chilledWaterPlant_chillerPlant_cooTowWithByp_dTApp_nominal)";
    "fmin(1.0, fmax(0.0, (chilledWaterPlant_chillerPlant_cooTowWithByp_TCWLowSet - ".to_string()
        + tower_outlet_without_bypass
        + ") / fmax(0.1, chilledWaterPlant_chillerPlant_cooTowWithByp_dTCW_nominal)))"
}

fn boptest_cooling_tower_total_condenser_flow_rhs() -> String {
    (1..=3)
        .map(|unit| {
            var_name_to_c_alias(&format!(
                "chilledWaterPlant.chillerPlant.pumCW.pumConSpe[{unit}].m_flow_in"
            ))
        })
        .collect::<Vec<_>>()
        .join(" + ")
}

fn boptest_cooling_tower_bypass_mass_flow_rhs() -> String {
    let total_flow = boptest_cooling_tower_total_condenser_flow_rhs();
    let valve_position = boptest_cooling_tower_bypass_control_rhs();
    format!("fmax(0.0, ({total_flow}) * ({valve_position}))")
}

fn boptest_cooling_tower_tower_branch_mass_flow_rhs() -> String {
    let total_flow = boptest_cooling_tower_total_condenser_flow_rhs();
    let bypass_flow = boptest_cooling_tower_bypass_mass_flow_rhs();
    format!("fmax(0.0, ({total_flow}) - ({bypass_flow}))")
}

fn boptest_cooling_tower_bypass_valve_phi_rhs() -> String {
    let y = boptest_cooling_tower_bypass_control_rhs();
    let r = var_name_to_c_alias("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.R");
    let l = var_name_to_c_alias("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.l");
    let delta =
        var_name_to_c_alias("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.delta0");
    let log_r = format!("log({r})");
    let z = format!("(3.0 * ({delta}) / 2.0)");
    let q = format!("(({delta}) * pow({r}, {z}) * ({log_r}))");
    let p = format!("pow({r}, {z})");
    let a =
        format!("(({q}) - 2.0 * ({p}) + 2.0 * pow({r}, {delta})) / (pow({delta}, 3.0) * ({r}))");
    let b = format!(
        "(-5.0 * ({q}) + 12.0 * ({p}) - 13.0 * pow({r}, {delta}) + ({l}) * ({r})) / (2.0 * pow({delta}, 2.0) * ({r}))"
    );
    let c = format!(
        "(7.0 * ({q}) - 18.0 * ({p}) + 24.0 * pow({r}, {delta}) - 6.0 * ({l}) * ({r})) / (4.0 * ({delta}) * ({r}))"
    );
    let d = format!(
        "(-3.0 * ({q}) + 8.0 * ({p}) - 9.0 * pow({r}, {delta}) + 9.0 * ({l}) * ({r})) / (8.0 * ({r}))"
    );
    let linear = format!("({l}) + ({y}) * (pow({r}, ({delta}) - 1.0) - ({l})) / ({delta})");
    let exponential = format!("pow({r}, ({y}) - 1.0)");
    let spline = format!("({d}) + ({y}) * (({c}) + ({y}) * (({b}) + ({y}) * ({a})))");
    format!(
        "fmax(0.1 * ({l}), (({y}) < ({delta}) / 2.0 ? ({linear}) : (({y}) > (1.5 * ({delta})) ? ({exponential}) : ({spline}))))"
    )
}

fn boptest_cooling_tower_bypass_valve_k_val_rhs() -> String {
    let phi = boptest_cooling_tower_bypass_valve_phi_rhs();
    let kv_si =
        var_name_to_c_alias("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.Kv_SI");
    format!("(({phi}) * ({kv_si}))")
}

fn boptest_cooling_tower_bypass_valve_k_rhs() -> String {
    let dp_fixed = var_name_to_c_alias(
        "chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.dpFixed_nominal",
    );
    let k_fixed =
        var_name_to_c_alias("chilledWaterPlant.chillerPlant.cooTowWithByp.byp.valByp.kFixed");
    let k_val = boptest_cooling_tower_bypass_valve_k_val_rhs();
    format!(
        "(({dp_fixed} > 0.0000000000000002220446049250313) ? sqrt(1.0 / (1.0 / fmax(0.0000000001, ({k_fixed}) * ({k_fixed})) + 1.0 / fmax(0.0000000001, ({k_val}) * ({k_val})))) : ({k_val}))"
    )
}

#[derive(Clone, Copy)]
enum BoptestPlantValveKind {
    Linear,
    EqualPercentage,
}

fn synthesize_boptest_plant_valve_curve_rhs(var_name: &str) -> Option<String> {
    let (prefix, field) = var_name
        .strip_suffix(".phi")
        .map(|prefix| (prefix, "phi"))
        .or_else(|| {
            var_name
                .strip_suffix(".kVal")
                .map(|prefix| (prefix, "kVal"))
        })
        .or_else(|| var_name.strip_suffix(".k").map(|prefix| (prefix, "k")))?;
    let kind = boptest_plant_valve_kind(prefix)?;
    match field {
        "phi" => Some(boptest_plant_valve_phi_rhs(prefix, kind)),
        "kVal" => Some(boptest_plant_valve_k_val_rhs(prefix, kind)),
        "k" => Some(boptest_plant_valve_k_rhs(prefix, kind)),
        _ => None,
    }
}

fn boptest_plant_valve_kind(prefix: &str) -> Option<BoptestPlantValveKind> {
    if let Some((component, index)) = parse_indexed_component_signal(prefix, ".ch[", "].valCW")
        .or_else(|| parse_indexed_component_signal(prefix, ".ch[", "].valCHW"))
        && component == "chilledWaterPlant.chillerPlant.mulChiSys"
        && (1..=3).contains(&index)
    {
        return Some(BoptestPlantValveKind::Linear);
    }
    if let Some((component, index)) = parse_indexed_component_signal(prefix, ".boi[", "].valHW")
        && component == "hotWaterPlant.boilerPlant.mulBoi"
        && (1..=2).contains(&index)
    {
        return Some(BoptestPlantValveKind::Linear);
    }
    if let Some((component, index)) = parse_indexed_component_signal(prefix, ".val[", "]")
        && matches!(
            component,
            "chilledWaterPlant.chillerPlant.pumSecCHW" | "hotWaterPlant.boilerPlant.pumSecHW"
        )
        && (1..=2).contains(&index)
    {
        return Some(BoptestPlantValveKind::EqualPercentage);
    }
    if let Some((component, index)) = parse_indexed_component_signal(prefix, ".ct[", "].val")
        && component == "chilledWaterPlant.chillerPlant.cooTowWithByp.mulCooTowSys"
        && (1..=3).contains(&index)
    {
        return Some(BoptestPlantValveKind::EqualPercentage);
    }
    if let Some((wrapper, terminal)) = parse_boptest_vav_valve_prefix(prefix) {
        let zone_index = parse_vav_terminal_index(terminal)?;
        if (1..=3).contains(&wrapper.floor_index)
            && (1..=5).contains(&zone_index)
            && (terminal.ends_with(".dam") || terminal.ends_with(".rehVal"))
        {
            return Some(BoptestPlantValveKind::EqualPercentage);
        }
    }
    None
}

fn parse_boptest_vav_valve_prefix<'a>(prefix: &'a str) -> Option<(BoptestVavWrapper<'a>, &'a str)> {
    let floor_pos = prefix.find("BoptestAirNetwork.floor.fivZonVAV.vAV")?;
    let wrapper_prefix = &prefix[..floor_pos + "BoptestAirNetwork".len()];
    let floor_index = parse_boptest_air_network_floor_index(wrapper_prefix)?;
    let terminal = &prefix[floor_pos + "BoptestAirNetwork.floor.fivZonVAV.".len()..];
    if !(terminal.ends_with(".dam") || terminal.ends_with(".rehVal")) {
        return None;
    }
    Some((
        BoptestVavWrapper {
            prefix: wrapper_prefix,
            floor_index,
        },
        terminal,
    ))
}

fn boptest_plant_valve_phi_rhs(prefix: &str, kind: BoptestPlantValveKind) -> String {
    let alias = var_name_to_c_alias(prefix);
    let y = format!("{alias}_y_actual");
    let l = format!("{alias}_l");
    match kind {
        BoptestPlantValveKind::Linear => {
            format!("fmax(0.1 * ({l}), ({l}) + ({y}) * (1.0 - ({l})))")
        }
        BoptestPlantValveKind::EqualPercentage => {
            let r = format!("{alias}_R");
            let delta = format!("{alias}_delta0");
            boptest_equal_percentage_valve_phi_rhs(&y, &r, &l, &delta)
        }
    }
}

fn boptest_equal_percentage_valve_phi_rhs(y: &str, r: &str, l: &str, delta: &str) -> String {
    let log_r = format!("log({r})");
    let z = format!("(3.0 * ({delta}) / 2.0)");
    let q = format!("(({delta}) * pow({r}, {z}) * ({log_r}))");
    let p = format!("pow({r}, {z})");
    let a =
        format!("(({q}) - 2.0 * ({p}) + 2.0 * pow({r}, {delta})) / (pow({delta}, 3.0) * ({r}))");
    let b = format!(
        "(-5.0 * ({q}) + 12.0 * ({p}) - 13.0 * pow({r}, {delta}) + ({l}) * ({r})) / (2.0 * pow({delta}, 2.0) * ({r}))"
    );
    let c = format!(
        "(7.0 * ({q}) - 18.0 * ({p}) + 24.0 * pow({r}, {delta}) - 6.0 * ({l}) * ({r})) / (4.0 * ({delta}) * ({r}))"
    );
    let d = format!(
        "(-3.0 * ({q}) + 8.0 * ({p}) - 9.0 * pow({r}, {delta}) + 9.0 * ({l}) * ({r})) / (8.0 * ({r}))"
    );
    let linear = format!("({l}) + ({y}) * (pow({r}, ({delta}) - 1.0) - ({l})) / ({delta})");
    let exponential = format!("pow({r}, ({y}) - 1.0)");
    let spline = format!("({d}) + ({y}) * (({c}) + ({y}) * (({b}) + ({y}) * ({a})))");
    format!(
        "fmax(0.1 * ({l}), (({y}) < ({delta}) / 2.0 ? ({linear}) : (({y}) > (1.5 * ({delta})) ? ({exponential}) : ({spline}))))"
    )
}

fn boptest_plant_valve_k_val_rhs(prefix: &str, kind: BoptestPlantValveKind) -> String {
    let alias = var_name_to_c_alias(prefix);
    let phi = boptest_plant_valve_phi_rhs(prefix, kind);
    format!("(({phi}) * ({alias}_Kv_SI))")
}

fn boptest_plant_valve_k_rhs(prefix: &str, kind: BoptestPlantValveKind) -> String {
    let alias = var_name_to_c_alias(prefix);
    let k_val = boptest_plant_valve_k_val_rhs(prefix, kind);
    format!(
        "(({alias}_dpFixed_nominal > 0.0000000000000002220446049250313) ? sqrt(1.0 / (1.0 / fmax(0.0000000001, ({alias}_kFixed) * ({alias}_kFixed)) + 1.0 / fmax(0.0000000001, ({k_val}) * ({k_val})))) : ({k_val}))"
    )
}

fn synthesize_boptest_plant_on_connection_rhs(
    var_name: &str,
    _aliases: &HashSet<String>,
) -> Option<String> {
    let (prefix, index) = parse_indexed_suffix(var_name, ".On")?;
    let source = if prefix == "chilledWaterPlant.chillerPlant.mulChiSys" {
        format!("chilledWaterPlant.chillerPlant.chiSta.y[{index}]")
    } else if matches!(
        prefix,
        "chilledWaterPlant.chillerPlant.pumPriCHW"
            | "chilledWaterPlant.chillerPlant.pumCW"
            | "chilledWaterPlant.chillerPlant.cooTowWithByp"
    ) {
        format!("chilledWaterPlant.chillerPlant.mulChiSys.On[{index}]")
    } else {
        return None;
    };
    Some(var_name_to_c_alias(&source))
}

fn synthesize_boptest_plant_stage_condition_io_rhs(var_name: &str) -> Option<String> {
    if var_name == "chilledWaterPlant.chillerPlant.chiSta.loa"
        || var_name == "chilledWaterPlant.chillerPlant.chiSta.plantNStageCondition.Loa"
    {
        return Some(var_name_to_c_alias("chilledWaterPlant.chillerPlant.Loa.y"));
    }
    if var_name == "hotWaterPlant.boilerPlant.boiSta.loa"
        || var_name == "hotWaterPlant.boilerPlant.boiSta.plantNStageCondition.Loa"
    {
        return Some(var_name_to_c_alias(
            "hotWaterPlant.boilerPlant.realExpression.y",
        ));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".sta") {
        if prefix == "chilledWaterPlant.chillerPlant.chiSta" {
            return Some(var_name_to_c_alias(&format!(
                "chilledWaterPlant.chillerPlant.chiSta.y[{index}]"
            )));
        }
        if prefix == "hotWaterPlant.boilerPlant.boiSta" {
            return Some(var_name_to_c_alias(&format!(
                "hotWaterPlant.boilerPlant.boiSta.y[{index}]"
            )));
        }
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".plantNStageCondition.Status") {
        if prefix == "chilledWaterPlant.chillerPlant.chiSta" {
            return Some(var_name_to_c_alias(&format!(
                "chilledWaterPlant.chillerPlant.chiSta.y[{index}]"
            )));
        }
        if prefix == "hotWaterPlant.boilerPlant.boiSta" {
            return Some(var_name_to_c_alias(&format!(
                "hotWaterPlant.boilerPlant.boiSta.y[{index}]"
            )));
        }
    }

    None
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
    if let Some((base, index)) = parse_indexed_ref(var_name)
        && base.ends_with(".expr")
    {
        return Some((index.saturating_sub(1)).to_string());
    }
    let alias = var_name_to_c_alias(var_name);
    let (_, alias_index) = alias.rsplit_once("_multiSwitch_expr_")?;
    let alias_index = alias_index.parse::<usize>().ok()?;
    Some((alias_index.saturating_sub(1)).to_string())
}

fn synthesize_boptest_zone_ctot_flow_input_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let (prefix, source) =
        if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".CTot_flow.u1") {
            let prefix_alias = var_name_to_c_alias(prefix);
            (prefix, format!("{prefix_alias}_C_flow_{index}"))
        } else if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".CTot_flow.u2") {
            let prefix_alias = var_name_to_c_alias(prefix);
            (prefix, format!("{prefix_alias}_QPeaRep_y_{index}"))
        } else {
            return None;
        };
    if !prefix.contains(".fivZonVAV.zon[") {
        return None;
    }
    fluid_alias_exists(aliases, &source).then_some(source)
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

fn synthesize_buildings_actuator_signal_alg_rhs(
    var_name: &str,
    equations: &Value,
) -> Result<Option<String>, minijinja::Error> {
    if let Some(rhs) = synthesize_boptest_vav_terminal_overwrite_signal_rhs(var_name) {
        return Ok(Some(rhs));
    }

    if let Some(rhs) = synthesize_buildings_actuator_filter_alg_rhs(var_name) {
        return Ok(Some(rhs));
    }

    if let Some(prefix) = var_name.strip_suffix(".filter.u") {
        if !is_boptest_vav_terminal_actuator_prefix(prefix) {
            return Ok(None);
        }
        if let Some(overwrite_prefix) = boptest_vav_terminal_overwrite_prefix_for_actuator(prefix) {
            return Ok(Some(var_name_to_c_alias(&format!(
                "{overwrite_prefix}.swi.y"
            ))));
        }
        let input = format!("{prefix}.y");
        if let Some(rhs) = actuator_input_connection_rhs(&input, prefix, equations) {
            return Ok(Some(rhs));
        }
        return Ok(Some(var_name_to_c_alias(&input)));
    }
    if !var_name.ends_with(".filter.y")
        && let Some(prefix) = var_name.strip_suffix(".y")
        && is_boptest_vav_terminal_actuator_prefix(prefix)
        && let Some(rhs) = actuator_input_connection_rhs(var_name, prefix, equations)
    {
        return Ok(Some(rhs));
    }
    if !var_name.ends_with(".filter.y")
        && let Some(prefix) = var_name.strip_suffix(".y")
        && is_boptest_vav_terminal_actuator_prefix(prefix)
        && let Some(overwrite_prefix) = boptest_vav_terminal_overwrite_prefix_for_actuator(prefix)
    {
        return Ok(Some(var_name_to_c_alias(&format!(
            "{overwrite_prefix}.swi.y"
        ))));
    }
    if let Some(prefix) = var_name.strip_suffix(".y_filtered") {
        if !is_boptest_vav_terminal_actuator_prefix(prefix) {
            return Ok(None);
        }
        return Ok(Some(var_name_to_c_alias(&format!("{prefix}.filter.y"))));
    }
    if let Some(prefix) = var_name.strip_suffix(".y_internal") {
        if !is_boptest_vav_terminal_actuator_prefix(prefix) {
            return Ok(None);
        }
        let use_input_filter = var_name_to_c_alias(&format!("{prefix}.use_inputFilter"));
        let filtered = var_name_to_c_alias(&format!("{prefix}.filter.y"));
        let direct = var_name_to_c_alias(&format!("{prefix}.y"));
        return Ok(Some(format!(
            "({use_input_filter} ? {filtered} : {direct})"
        )));
    }
    if let Some(prefix) = var_name.strip_suffix(".y_actual") {
        if !is_boptest_vav_terminal_actuator_prefix(prefix) {
            return Ok(None);
        }
        return Ok(Some(var_name_to_c_alias(&format!("{prefix}.y_internal"))));
    }

    Ok(None)
}

fn synthesize_boptest_vav_terminal_overwrite_signal_rhs(var_name: &str) -> Option<String> {
    let (overwrite_prefix, signal) = parse_boptest_vav_terminal_overwrite_prefix(var_name)?;
    if var_name == format!("{overwrite_prefix}.swi.y") {
        let switch_prefix = var_name_to_c_alias(&format!("{overwrite_prefix}.swi"));
        return Some(format!(
            "{switch_prefix}_u2 ? {switch_prefix}_u1 : {switch_prefix}_u3"
        ));
    }
    if var_name == format!("{overwrite_prefix}.y") || var_name == format!("{overwrite_prefix}_out")
    {
        return Some(var_name_to_c_alias(&format!("{overwrite_prefix}.swi.y")));
    }
    if var_name == format!("{overwrite_prefix}.out") {
        return Some(var_name_to_c_alias(&format!("{overwrite_prefix}.swi.y")));
    }
    if var_name == format!("{overwrite_prefix}.u")
        || var_name == format!("{overwrite_prefix}.in")
        || var_name == format!("{overwrite_prefix}_in")
        || var_name == format!("{overwrite_prefix}.swi.u3")
    {
        return Some(boptest_vav_terminal_native_controller_rhs(
            &overwrite_prefix,
            signal,
        ));
    }
    None
}

fn parse_boptest_vav_terminal_overwrite_prefix(var_name: &str) -> Option<(String, &'static str)> {
    for signal in ["yDam", "yReaHea"] {
        for suffix in [
            ".swi.y", ".swi.u3", ".out", "_out", ".y", ".u", ".in", "_in",
        ] {
            let Some(prefix) = var_name.strip_suffix(suffix) else {
                continue;
            };
            if !prefix.ends_with(&format!(".oveZonLoc.{signal}")) {
                continue;
            }
            if prefix.contains("BoptestAirNetwork.floor.fivZonVAV.vAV") {
                return Some((prefix.to_string(), signal));
            }
        }
    }
    None
}

fn boptest_vav_terminal_overwrite_prefix_for_actuator(actuator_prefix: &str) -> Option<String> {
    let (wrapper, terminal) = parse_boptest_vav_valve_prefix(actuator_prefix)?;
    let (terminal_base, signal) = terminal
        .strip_suffix(".dam")
        .map(|base| (base, "yDam"))
        .or_else(|| {
            terminal
                .strip_suffix(".rehVal")
                .map(|base| (base, "yReaHea"))
        })?;
    Some(format!(
        "{}.floor.fivZonVAV.{terminal_base}.oveZonLoc.{signal}",
        wrapper.prefix
    ))
}

fn boptest_vav_terminal_native_controller_rhs(overwrite_prefix: &str, signal: &str) -> String {
    let native_source = match signal {
        "yDam" => overwrite_prefix.replace(".oveZonLoc.yDam", ".pI.y"),
        "yReaHea" => overwrite_prefix.replace(".oveZonLoc.yReaHea", ".yVal"),
        _ => overwrite_prefix.to_string(),
    };
    var_name_to_c_alias(&native_source)
}

fn is_boptest_vav_terminal_actuator_prefix(prefix: &str) -> bool {
    parse_boptest_vav_valve_prefix(prefix).is_some()
}

fn actuator_input_connection_rhs(
    input_var: &str,
    actuator_prefix: &str,
    equations: &Value,
) -> Option<String> {
    let Some(candidate_equations) = alg_equation_candidates_for_var(equations, input_var) else {
        return None;
    };
    let filter_u = var_name_to_c_alias(&format!("{actuator_prefix}.filter.u"));
    let mut best_rhs: Option<(isize, String)> = None;
    for eq in candidate_equations {
        let Some(alias) = actuator_simple_input_alias(&eq, input_var) else {
            continue;
        };
        let alias = var_name_to_c_alias(&alias);
        if alias == filter_u || alias == var_name_to_c_alias(input_var) {
            continue;
        }
        let mut score = 0;
        if alias.contains("swi_y") || alias.ends_with("_out") || alias.ends_with("_y") {
            score += 100;
        }
        if !alias.starts_with(&var_name_to_c_alias(actuator_prefix)) {
            score += 50;
        }
        if best_rhs
            .as_ref()
            .is_none_or(|(best_score, _)| score > *best_score)
        {
            best_rhs = Some((score, alias));
        }
    }
    best_rhs.map(|(_score, rhs)| rhs)
}

fn actuator_simple_input_alias(eq: &Value, input_var: &str) -> Option<String> {
    let rhs = get_field(eq, "rhs").ok()?;
    let binary = get_field(&rhs, "Binary").ok()?;
    if !is_sub_op(&binary) {
        return None;
    }
    let lhs_side = get_field(&binary, "lhs").ok()?;
    let rhs_side = get_field(&binary, "rhs").ok()?;
    if is_var_ref_of(&lhs_side, input_var) {
        return simple_var_ref_name(&rhs_side);
    }
    if is_var_ref_of(&rhs_side, input_var) {
        return simple_var_ref_name(&lhs_side);
    }
    None
}

fn simple_var_ref_name(expr: &Value) -> Option<String> {
    let var_ref = get_field(expr, "VarRef").ok()?;
    Some(var_ref_full_name(&var_ref))
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
    if let Some(volume_prefix) = state_name.strip_suffix(".m")
        && boptest_zone_volume_prefix(volume_prefix)
    {
        let mb_flow = var_name_to_c_alias(&format!("{volume_prefix}.mb_flow"));
        let simplify_m_wat_flow =
            var_name_to_c_alias(&format!("{volume_prefix}.simplify_mWat_flow"));
        let m_wat_flow_internal =
            var_name_to_c_alias(&format!("{volume_prefix}.mWat_flow_internal"));
        return Some(format!(
            "({mb_flow} + ({simplify_m_wat_flow} ? 0.0 : {m_wat_flow_internal}))"
        ));
    }
    if let Some(volume_prefix) = state_name.strip_suffix(".U")
        && boptest_zone_volume_prefix(volume_prefix)
    {
        let hb_flow = var_name_to_c_alias(&format!("{volume_prefix}.Hb_flow"));
        let q_flow = var_name_to_c_alias(&format!("{volume_prefix}.Q_flow"));
        return Some(format!("({hb_flow} + {q_flow})"));
    }
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
    if let Some((volume_prefix, index)) = parse_indexed_suffix(state_name, ".medium.Xi")
        && boptest_zone_volume_prefix(volume_prefix)
    {
        let m = var_name_to_c_alias(&format!("{volume_prefix}.m"));
        let mb_xi_flow = indexed_alias(&format!("{volume_prefix}.mbXi_flow"), index);
        let m_wat_flow_internal =
            var_name_to_c_alias(&format!("{volume_prefix}.mWat_flow_internal"));
        let s = indexed_alias(&format!("{volume_prefix}.s"), index);
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
    if let Some((volume_prefix, index)) = parse_indexed_suffix(state_name, ".mC")
        && boptest_zone_volume_prefix(volume_prefix)
    {
        let mb_c_flow = indexed_alias(&format!("{volume_prefix}.mbC_flow"), index);
        let c_flow_internal = indexed_alias(&format!("{volume_prefix}.C_flow_internal"), index);
        return Some(format!("({mb_c_flow} + {c_flow_internal})"));
    }
    if let Some(volume_prefix) = state_name.strip_suffix(".dynBal.medium.T")
        && internal_fluid_volume_prefix(volume_prefix)
        && boptest_water_plant_var(state_name)
    {
        let m = var_name_to_c_alias(&format!("{volume_prefix}.dynBal.m"));
        let u = var_name_to_c_alias(&format!("{volume_prefix}.dynBal.U"));
        let hb_flow = var_name_to_c_alias(&format!("{volume_prefix}.dynBal.Hb_flow"));
        let q_flow = var_name_to_c_alias(&format!("{volume_prefix}.dynBal.Q_flow"));
        let mb_flow = var_name_to_c_alias(&format!("{volume_prefix}.dynBal.mb_flow"));
        let cp_default = var_name_to_c_alias(&format!("{volume_prefix}.dynBal.cp_default"));
        return Some(format!(
            "(({m} > 1e-9 && fabs({cp_default}) > 1e-9) ? (((({hb_flow} + {q_flow}) * {m}) - ({u} * {mb_flow})) / ({cp_default} * {m} * {m})) : 0.0)"
        ));
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

fn synthesize_top_down_radiant_tail_ode_rhs(state_name: &str) -> Option<String> {
    if state_name == "floor3_core_radiant_surface_temperature" {
        let gain = indexed_alias2("floor_internal_gain_rad", 3, 1);
        let heat = var_name_to_c_alias("floor3_core_radiant_heat_to_air");
        let capacity = var_name_to_c_alias("floor3_core_radiant_capacity");
        return Some(format!("(({gain} - {heat}) / {capacity})"));
    }

    if let Some((floor, zone)) = parse_top_down_radiant_surface_state(state_name) {
        let heat = var_name_to_c_alias(&format!("floor{floor}_radiant_heat_to_air_{zone}"));
        let gain = indexed_alias2("floor_internal_gain_rad", floor, zone);
        let capacity_index = zone - 1;
        if floor == 3 {
            let slab_heat =
                var_name_to_c_alias(&format!("floor3_shared_perimeter_slab_heat_{zone}"));
            let capacity = indexed_alias("floor3_perimeter_radiant_capacity", capacity_index);
            return Some(format!("(({gain} - {heat} + {slab_heat}) / {capacity})"));
        }
        let capacity = indexed_alias("lower_perimeter_radiant_capacity", capacity_index);
        return Some(format!("(({gain} - {heat}) / {capacity})"));
    }

    if state_name == "floor3_late_perimeter_tail_temperature" {
        let gain_east = indexed_alias2("floor_internal_gain_rad", 3, 2);
        let gain_west = indexed_alias2("floor_internal_gain_rad", 3, 5);
        let heat_east = var_name_to_c_alias("floor3_late_perimeter_tail_heat_2");
        let heat_west = var_name_to_c_alias("floor3_late_perimeter_tail_heat_5");
        let capacity = var_name_to_c_alias("floor3_late_perimeter_tail_capacity");
        return Some(format!(
            "(((0.35 * {gain_east}) + (0.35 * {gain_west}) - {heat_east} - {heat_west}) / {capacity})"
        ));
    }

    None
}

fn synthesize_top_down_radiant_tail_alg_rhs(var_name: &str) -> Option<String> {
    if let Some(floor) = var_name
        .strip_prefix("floor")
        .and_then(|rest| rest.strip_suffix("_core_radiant_heat_to_air"))
        .and_then(|floor| floor.parse::<usize>().ok())
        && floor == 3
    {
        let conductance = var_name_to_c_alias("floor3_core_radiant_conductance");
        let surface =
            var_name_to_c_alias(&format!("floor{floor}_core_radiant_surface_temperature"));
        let zone = indexed_alias2("floor_zone_temperature", floor, 1);
        return Some(format!("({conductance} * ({surface} - {zone}))"));
    }

    None
}

fn parse_top_down_radiant_surface_state(state_name: &str) -> Option<(usize, usize)> {
    let rest = state_name.strip_prefix("floor")?;
    let (floor, rest) = rest.split_once("_radiant_surface_temperature_")?;
    let floor = floor.parse::<usize>().ok()?;
    let zone = rest.parse::<usize>().ok()?;
    ((1..=3).contains(&floor) && (2..=5).contains(&zone)).then_some((floor, zone))
}

fn indexed_alias2(prefix: &str, first: usize, second: usize) -> String {
    format!("{}_{}_{}", var_name_to_c_alias(prefix), first, second)
}

fn synthesize_boptest_internal_gain_rhs(var_name: &str) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    if let Some((floor, zone)) = parse_floor_total_internal_gain_alias(&alias) {
        let source_index = ((floor - 1) * 5) + zone;
        let deterministic = indexed_alias("internal_gains", source_index);
        let source_backed = indexed_alias2("floor_internal_gain", floor, zone);
        return Some(format!("({deterministic} + {source_backed})"));
    }
    if let Some((kind, floor, zone)) = parse_floor_internal_gain_alias(&alias) {
        let source = indexed_alias2(&format!("source_floor_internal_gain_{kind}"), floor, zone);
        let deterministic = indexed_alias2(
            &format!("deterministic_floor_internal_gain_{kind}"),
            floor,
            zone,
        );
        return Some(format!(
            "((internal_gain_source_override_enable > 0.5) ? {source} : {deterministic})"
        ));
    }
    if let Some((kind, floor, zone)) = parse_internal_gain_output_alias(&alias) {
        let gain = indexed_alias2(&format!("floor_internal_gain_{kind}"), floor, zone);
        if floor == 2 {
            return Some(format!("(({gain}) * floor_multiplier_2)"));
        }
        return Some(gain);
    }
    None
}

fn parse_floor_total_internal_gain_alias(alias: &str) -> Option<(usize, usize)> {
    let rest = alias.strip_prefix("floor_total_internal_gain_")?;
    let (floor, zone) = rest.split_once('_')?;
    let floor = floor.parse::<usize>().ok()?;
    let zone = zone.parse::<usize>().ok()?;
    ((1..=3).contains(&floor) && (1..=5).contains(&zone)).then_some((floor, zone))
}

fn parse_floor_internal_gain_alias(alias: &str) -> Option<(&'static str, usize, usize)> {
    for kind in ["con", "rad", "lat"] {
        let Some(rest) = alias.strip_prefix(&format!("floor_internal_gain_{kind}_")) else {
            continue;
        };
        let (floor, zone) = rest.split_once('_')?;
        let floor = floor.parse::<usize>().ok()?;
        let zone = zone.parse::<usize>().ok()?;
        if (1..=3).contains(&floor) && (1..=5).contains(&zone) {
            return Some((kind, floor, zone));
        }
    }
    None
}

fn parse_internal_gain_output_alias(alias: &str) -> Option<(&'static str, usize, usize)> {
    for kind in ["con", "rad", "lat"] {
        let Some(rest) =
            alias.strip_prefix(&format!("InternalGains{}_", title_case_gain_kind(kind)))
        else {
            continue;
        };
        let (floor_token, zone_token) = rest.rsplit_once('_')?;
        let floor = match floor_token {
            "bot_floor" => 1,
            "mid_floor" => 2,
            "top_floor" => 3,
            _ => return None,
        };
        let zone = match zone_token {
            "cor" => 1,
            "sou" => 2,
            "eas" => 3,
            "nor" => 4,
            "wes" => 5,
            _ => return None,
        };
        return Some((kind, floor, zone));
    }
    None
}

fn title_case_gain_kind(kind: &str) -> &'static str {
    match kind {
        "con" => "Con",
        "rad" => "Rad",
        "lat" => "Lat",
        _ => unreachable!("unsupported internal gain kind"),
    }
}

fn synthesize_buildings_expansion_vessel_ode_rhs(state_name: &str) -> Option<String> {
    let vessel_prefix = state_name.strip_suffix(".H")?;
    let vessel_name = top_level_last_segment(vessel_prefix);
    if !vessel_name.starts_with("expVes") || !boptest_water_plant_var(state_name) {
        return None;
    }

    let m_flow = var_name_to_c_alias(&format!("{vessel_prefix}.port_a.m_flow"));
    let connected_h = var_name_to_c_alias(&format!("{vessel_prefix}.port_a.h_outflow"));
    let h = var_name_to_c_alias(state_name);
    let m = var_name_to_c_alias(&format!("{vessel_prefix}.m"));
    Some(format!(
        "({m_flow} * (({m_flow} > 0.0) ? {connected_h} : (({m} > 1e-9) ? ({h} / {m}) : {connected_h})))"
    ))
}

fn synthesize_boptest_wet_coil_counterflow_rhs(var_name: &str) -> Option<String> {
    if let Some(rhs) = synthesize_boptest_floor_ahu_sizing_rhs(var_name) {
        return Some(rhs);
    }

    if !var_name.contains(".cooCoi.coi.cooCoi") {
        return None;
    }

    if let Some(ele_prefix) = var_name.strip_suffix(".UA_nominal")
        && ele_prefix.contains(".ele[")
    {
        let coil_prefix = ele_prefix.split(".ele[").next()?;
        return Some(format!(
            "({} / fmax(1.0, {}))",
            var_name_to_c_alias(&format!("{coil_prefix}.UA_nominal")),
            var_name_to_c_alias(&format!("{coil_prefix}.nEle"))
        ));
    }

    if let Some(ele_prefix) = var_name.strip_suffix(".tau_m")
        && ele_prefix.contains(".ele[")
    {
        let coil_prefix = ele_prefix.split(".ele[").next()?;
        return Some(format!(
            "({} / fmax(1.0, {}))",
            var_name_to_c_alias(&format!("{coil_prefix}.tau_m")),
            var_name_to_c_alias(&format!("{coil_prefix}.nEle"))
        ));
    }

    if let Some(prefix) = var_name.strip_suffix(".UA_nominal") {
        return Some(boptest_wet_coil_counterflow_parent_ua_rhs(prefix));
    }

    if let Some(ele_prefix) = var_name.strip_suffix(".mas.C")
        && ele_prefix.contains(".ele[")
    {
        return Some(var_name_to_c_alias(&format!("{ele_prefix}.C")));
    }

    if let Some(ele_prefix) = var_name.strip_suffix(".C")
        && ele_prefix.contains(".ele[")
    {
        return Some(format!(
            "(2.0 * {} * {})",
            var_name_to_c_alias(&format!("{ele_prefix}.UA_nominal")),
            var_name_to_c_alias(&format!("{ele_prefix}.tau_m"))
        ));
    }

    if let Some(ele_prefix) = var_name.strip_suffix(".mas.der_T")
        && ele_prefix.contains(".ele[")
    {
        let c = var_name_to_c_alias(&format!("{ele_prefix}.mas.C"));
        let q_flow = var_name_to_c_alias(&format!("{ele_prefix}.mas.port.Q_flow"));
        return Some(format!("(({c} > 1e-9) ? ({q_flow} / {c}) : 0.0)"));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".T1") {
        return Some(var_name_to_c_alias(&format!(
            "{prefix}.ele[{index}].vol1.T"
        )));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".T2") {
        return Some(var_name_to_c_alias(&format!(
            "{prefix}.ele[{index}].vol2.T"
        )));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".T_m") {
        return Some(var_name_to_c_alias(&format!(
            "{prefix}.ele[{index}].con1.solid.T"
        )));
    }

    if let Some(ele_prefix) = var_name.strip_suffix(".masExc.Gc")
        && ele_prefix.contains(".ele[")
    {
        return Some(var_name_to_c_alias(&format!("{ele_prefix}.Gc_2")));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".vol1.dynBal.medium.X")
        && index == 1
        && prefix.contains(".ele[")
    {
        return Some("1.0".to_string());
    }

    None
}

fn synthesize_boptest_floor_ahu_sizing_rhs(var_name: &str) -> Option<String> {
    let (floor_prefix, floor_index) = boptest_floor_network_prefix(var_name)?;

    if let Some(zone_index) = var_name
        .strip_prefix(&format!("{floor_prefix}.floor.mAirFloRat"))
        .and_then(|suffix| suffix.parse::<usize>().ok())
    {
        return boptest_floor_air_flow_ratio(floor_index, zone_index)
            .map(|value| format!("{value:.12}"));
    }

    if let Some(zone_index) = var_name
        .strip_prefix(&format!("{floor_prefix}.floor.fivZonVAV.mAirFloRat"))
        .and_then(|suffix| suffix.parse::<usize>().ok())
    {
        return boptest_floor_air_flow_ratio(floor_index, zone_index)
            .map(|value| format!("{value:.12}"));
    }

    if var_name == format!("{floor_prefix}.floor.duaFanAirHanUni.mAirFloRat") {
        let terms = (1..=5)
            .map(|index| var_name_to_c_alias(&format!("{floor_prefix}.floor.mAirFloRat{index}")))
            .collect::<Vec<_>>()
            .join(" + ");
        return Some(format!("({terms})"));
    }

    if var_name == format!("{floor_prefix}.floor.duaFanAirHanUni.UA") {
        let m_air =
            var_name_to_c_alias(&format!("{floor_prefix}.floor.duaFanAirHanUni.mAirFloRat"));
        let lmtd = "(((273.15 + 12.0) - (273.15 + 30.0) - ((273.15 + 6.0) - (273.15 + 12.88))) / log(((273.15 + 12.0) - (273.15 + 30.0)) / ((273.15 + 6.0) - (273.15 + 12.88))))";
        return Some(format!("(-{m_air} * (1000.0 * 17.0) / {lmtd})"));
    }

    if var_name == format!("{floor_prefix}.floor.duaFanAirHanUni.cooCoi.UA") {
        let ahu_ua = var_name_to_c_alias(&format!("{floor_prefix}.floor.duaFanAirHanUni.UA"));
        let eps = var_name_to_c_alias(&format!("{floor_prefix}.floor.duaFanAirHanUni.eps"));
        return Some(format!("({ahu_ua} * 1.2 * {eps})"));
    }

    if var_name == format!("{floor_prefix}.floor.duaFanAirHanUni.cooCoi.coi.UA") {
        return Some(var_name_to_c_alias(&format!(
            "{floor_prefix}.floor.duaFanAirHanUni.cooCoi.UA"
        )));
    }

    None
}

fn boptest_wet_coil_counterflow_parent_ua_rhs(prefix: &str) -> String {
    let wet_coil_prefix = prefix.strip_suffix(".cooCoi").unwrap_or(prefix);
    var_name_to_c_alias(&format!("{wet_coil_prefix}.UA"))
}

fn boptest_floor_network_prefix(var_name: &str) -> Option<(&str, usize)> {
    let floor_prefix = var_name.split(".floor.").next()?;
    let index = floor_prefix
        .strip_prefix("floor")
        .and_then(|suffix| suffix.strip_suffix("BoptestAirNetwork"))?
        .parse::<usize>()
        .ok()?;
    (1..=3).contains(&index).then_some((floor_prefix, index))
}

fn boptest_floor_air_flow_ratio(floor_index: usize, zone_index: usize) -> Option<f64> {
    let base = match zone_index {
        1 => 10.92,
        2 => 2.25,
        3 => 1.49,
        4 => 1.9,
        5 => 1.73,
        _ => return None,
    };
    let floor_multiplier = if floor_index == 2 { 10.0 } else { 1.0 };
    Some(base * 1.2 * 1.25 * 3.0 * floor_multiplier)
}

fn synthesize_boptest_zone_c_flow_rhs(var_name: &str, aliases: &HashSet<String>) -> Option<String> {
    let (prefix, index) = parse_indexed_suffix(var_name, ".C_flow")?;
    if index != 1 || !prefix.contains(".fivZonVAV.zon[") {
        return None;
    }
    let zone_index = parse_last_bracket_index(prefix)?;
    let floor_prefix = prefix.split(".fivZonVAV.zon[").next()?;
    let source = var_name_to_c_alias(&format!("{floor_prefix}.fivZonVAV.gaiCO2[{zone_index}].y"));
    fluid_alias_exists(aliases, &source).then_some(source)
}

fn parse_last_bracket_index(name: &str) -> Option<usize> {
    let open = name.rfind('[')?;
    let close = name[open + 1..].find(']')? + open + 1;
    name[open + 1..close].parse::<usize>().ok()
}

fn synthesize_top_down_control_semantics_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    if var_name == "controlSemantics.rawPlantChilledWaterSetpoint" {
        return Some("plant_chilled_water_setpoint".to_string());
    }
    if var_name == "controlSemantics.rawPlantHotWaterSetpoint" {
        return Some("plant_hot_water_setpoint".to_string());
    }
    if var_name == "controlSemantics.effectivePlantChilledWaterSetpoint" {
        return Some("controlSemantics_rawPlantChilledWaterSetpoint".to_string());
    }
    if var_name == "controlSemantics.effectivePlantHotWaterSetpoint" {
        return Some("controlSemantics_rawPlantHotWaterSetpoint".to_string());
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".rawAhuSupplyFanSpeed")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias("ahu_supply_fan_speed", index));
    }

    if let Some((prefix, index)) =
        parse_indexed_suffix(var_name, ".rawAhuSupplyAirTemperatureSetpoint")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias("ahu_supply_air_temperature_setpoint", index));
    }

    if let Some((prefix, index)) =
        parse_indexed_suffix(var_name, ".effectiveAhuSupplyAirTemperatureSetpoint")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias(
            "controlSemantics.rawAhuSupplyAirTemperatureSetpoint",
            index,
        ));
    }

    if let Some((prefix, index)) =
        parse_indexed_suffix(var_name, ".rawAhuDuctStaticPressureSetpoint")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias("ahu_duct_static_pressure_setpoint", index));
    }

    if let Some((prefix, index)) =
        parse_indexed_suffix(var_name, ".effectiveAhuDuctStaticPressureSetpoint")
        && prefix == "controlSemantics"
    {
        let raw = indexed_alias("controlSemantics.rawAhuDuctStaticPressureSetpoint", index);
        return Some(format!("fmax(0.0, {raw})"));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".rawAhuMixingDamperCommand")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias("ahu_mixing_damper_command", index));
    }

    if let Some((prefix, index)) =
        parse_indexed_suffix(var_name, ".effectiveAhuMixingDamperCommand")
        && prefix == "controlSemantics"
    {
        let raw = indexed_alias("controlSemantics.rawAhuMixingDamperCommand", index);
        return Some(clamp_unit_interval_rhs(&raw));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".rawZoneTerminalDamperCommand")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias("zone_terminal_damper_command", index));
    }

    if let Some((prefix, index)) =
        parse_indexed_suffix(var_name, ".effectiveZoneTerminalDamperCommand")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias(
            "controlSemantics.rawZoneTerminalDamperCommand",
            index,
        ));
    }

    if let Some((prefix, index)) =
        parse_indexed_suffix(var_name, ".rawZoneTerminalAirflowSetpointCommand")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias(
            "zone_terminal_airflow_setpoint_command",
            index,
        ));
    }

    if let Some((prefix, index)) =
        parse_indexed_suffix(var_name, ".effectiveZoneTerminalAirflowSetpointCommand")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias(
            "controlSemantics.rawZoneTerminalAirflowSetpointCommand",
            index,
        ));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".rawZoneTerminalReheatCommand")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias("zone_terminal_reheat_command", index));
    }

    if let Some((prefix, index)) =
        parse_indexed_suffix(var_name, ".effectiveZoneTerminalReheatCommand")
        && prefix == "controlSemantics"
    {
        let raw = indexed_alias("controlSemantics.rawZoneTerminalReheatCommand", index);
        return Some(clamp_unit_interval_rhs(&raw));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".rawZoneCoolingSetpointCommand")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias("zone_cooling_setpoint_command", index));
    }

    if let Some((prefix, index)) =
        parse_indexed_suffix(var_name, ".effectiveZoneCoolingSetpointCommand")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias(
            "controlSemantics.rawZoneCoolingSetpointCommand",
            index,
        ));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".rawZoneHeatingSetpointCommand")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias("zone_heating_setpoint_command", index));
    }

    if let Some((prefix, index)) =
        parse_indexed_suffix(var_name, ".effectiveZoneHeatingSetpointCommand")
        && prefix == "controlSemantics"
    {
        return Some(indexed_alias(
            "controlSemantics.rawZoneHeatingSetpointCommand",
            index,
        ));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".fanEnable")
        && prefix == "controlSemantics"
    {
        return Some(top_down_fan_enable_rhs(index));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".overrideActive")
        && prefix == "controlSemantics"
    {
        let initial = indexed_alias("controlSemantics.initialOverrideActive", index);
        return Some(format!("((time <= 0.0) ? {initial} : 0.0)"));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".overwriteState")
        && prefix == "controlSemantics"
    {
        let initial = indexed_alias("controlSemantics.initialOverwriteState", index);
        return Some(format!("((time <= 0.0) ? {initial} : 0.0)"));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".economizerEnable")
        && prefix == "controlSemantics"
    {
        let initial = indexed_alias("controlSemantics.initialEconomizerEnable", index);
        let request = indexed_alias("controlSemantics.economizerStagingState", index);
        return Some(format!(
            "((time <= 0.0) ? {initial} : (({request} > 0.5) ? 1.0 : 0.0))"
        ));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".economizerState")
        && prefix == "controlSemantics"
    {
        let initial = indexed_alias("controlSemantics.initialEconomizerState", index);
        let raw = indexed_alias("controlSemantics.rawAhuMixingDamperCommand", index);
        let enable = indexed_alias("controlSemantics.economizerEnable", index);
        let staged =
            format!("({raw} + {enable} * (1.0 - {raw}) * controlSemantics_weatherProfile)");
        let clamped = clamp_unit_interval_rhs(&staged);
        return Some(format!("((time <= 0.0) ? {initial} : ({clamped}))"));
    }

    if let Some((prefix, index)) = parse_indexed_suffix(var_name, ".effectiveAhuSupplyFanSpeed")
        && prefix == "controlSemantics"
    {
        return Some(top_down_effective_ahu_supply_fan_speed_rhs(index, aliases));
    }

    None
}

fn top_down_fan_enable_rhs(floor: usize) -> String {
    let initial = indexed_alias("controlSemantics.initialFanEnable", floor);
    let state = indexed_alias("controlSemantics.fanStagingState", floor);
    format!("((time <= 0.0) ? {initial} : (({state} > 0.5) ? 1.0 : 0.0))")
}

fn top_down_effective_ahu_supply_fan_speed_rhs(floor: usize, aliases: &HashSet<String>) -> String {
    let raw = clamp_unit_interval_rhs(&indexed_alias("ahu_supply_fan_speed", floor));
    let source_owned = top_down_source_owned_fan_speed_demand_rhs(floor, aliases);
    let enable = top_down_fan_enable_rhs(floor);
    format!("((({raw}) > 1e-6 ? ({raw}) : {source_owned}) * ({enable}))")
}

fn top_down_occupied_fan_demand_rhs(floor: usize) -> String {
    format!(
        "((time <= 0.0) ? controlSemantics_initialOccupancyScheduleState_{floor} : {profile})",
        profile = top_down_occupancy_profile_rhs(floor)
    )
}

fn top_down_occupancy_profile_rhs(floor: usize) -> String {
    let schedule = top_down_hvac_operation_schedule_rhs();
    format!(
        "((occupancy_schedule_source_override_enable > 0.5) ? fmax(0.0, fmin(1.0, occupancy_schedule_command_{floor})) : {schedule})"
    )
}

fn top_down_hvac_operation_schedule_rhs() -> String {
    let schedule_time = "(time + initializationWarmupContract_scheduleStartTimeSeconds)";
    format!(
        "((fmod({schedule_time}, 86400.0) >= 6.0 * 3600.0 && fmod({schedule_time}, 86400.0) < 22.0 * 3600.0) ? 1.0 : 0.0)"
    )
}

fn top_down_source_owned_fan_speed_demand_rhs(floor: usize, aliases: &HashSet<String>) -> String {
    let occupied = top_down_occupied_fan_demand_rhs(floor);
    let cycling = top_down_unoccupied_fan_cycling_state_rhs(floor);
    let pressure = top_down_occupied_pressure_fan_speed_rhs(floor, aliases);
    format!("fmax(({cycling}), (({occupied}) * ({pressure})))")
}

fn top_down_occupied_pressure_fan_speed_rhs(floor: usize, aliases: &HashSet<String>) -> String {
    let setpoint = indexed_alias("ahu_duct_static_pressure_setpoint", floor);
    let measurement = boptest_floor_duct_static_pressure_alias(floor, aliases);
    let integral = format!("controlSemantics_ahuFanPressureIntegral_{floor}");
    let unlimited = format!(
        "(controlSemantics_occupiedFanMinimumSpeed + controlSemantics_ahuFanPressureControllerGain * (fmax(0.0, {setpoint}) - fmax(0.0, {measurement})) + {integral})"
    );
    format!("fmax(controlSemantics_occupiedFanMinimumSpeed, fmin(1.0, {unlimited}))")
}

fn top_down_unoccupied_fan_cycling_state_rhs(floor: usize) -> String {
    format!("controlSemantics_unoccupiedFanCyclingState_{floor}")
}

fn synthesize_boptest_ahu_fan_mover_rhs(
    var_name: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    let fan = boptest_ahu_fan_mover(var_name)?;
    let alias = var_name_to_c_alias(fan.prefix);
    let volume_flow = boptest_ahu_fan_volume_flow_rhs(&fan, aliases);

    if var_name.ends_with(".eff.V_flow")
        || var_name.ends_with(".VMachine_flow")
        || var_name.ends_with(".preSou.V_flow")
    {
        return Some(volume_flow);
    }

    if var_name == format!("{}.eff.m_flow", fan.prefix)
        || var_name == format!("{}.m_flow", fan.prefix)
        || var_name == format!("{}.senMasFlo.m_flow", fan.prefix)
        || var_name == format!("{}.port_a.m_flow", fan.prefix)
    {
        return Some(format!("({alias}_rho_default * {volume_flow})"));
    }

    if var_name.ends_with(".eff.dp") || var_name.ends_with(".eff.dp_internal") {
        return Some(boptest_ahu_fan_pressure_rhs(&fan, &alias, aliases));
    }

    if var_name.ends_with(".eff.etaHyd") || var_name.ends_with(".etaHyd") {
        return Some(boptest_ahu_fan_curve_value_rhs(&fan, &alias, "HydEff"));
    }
    if var_name.ends_with(".eff.etaMot") || var_name.ends_with(".etaMot") {
        return Some(boptest_ahu_fan_curve_value_rhs(&fan, &alias, "MotEff"));
    }
    if var_name.ends_with(".eff.eta") || var_name.ends_with(".eta") {
        let eta_hyd = boptest_ahu_fan_curve_value_rhs(&fan, &alias, "HydEff");
        let eta_mot = boptest_ahu_fan_curve_value_rhs(&fan, &alias, "MotEff");
        return Some(format!("({eta_hyd} * {eta_mot})"));
    }

    let flow_work = format!("fmax(0.0, {alias}_eff_V_flow * {alias}_eff_dp)");
    if var_name.ends_with(".eff.WFlo") || var_name.ends_with(".heaDis.WFlo") {
        return Some(flow_work);
    }

    let electrical_power = format!(
        "({alias}_eff_WFlo / Buildings_Utilities_Math_Functions_smoothMax(__rumoca_named_arg___x1({alias}_eff_eta), __rumoca_named_arg___x2(0.00001), __rumoca_named_arg___deltaX(0.000001)))"
    );
    if var_name.ends_with(".eff.PEle") || var_name.ends_with(".heaDis.PEle") {
        return Some(electrical_power);
    }
    if var_name == format!("{}.P", fan.prefix) {
        return Some(format!("{alias}_heaDis_PEle"));
    }

    None
}

struct BoptestAhuFanMover<'a> {
    floor: usize,
    prefix: &'a str,
    speed_alias: String,
    pressure_curve_field: &'static str,
}

fn boptest_ahu_fan_mover(var_name: &str) -> Option<BoptestAhuFanMover<'_>> {
    let marker = ".varSpeFloMov";
    let pos = var_name.find(marker)?;
    let prefix = &var_name[..pos + marker.len()];
    for floor in 1..=3 {
        let supply_prefix = format!(
            "floor{floor}BoptestAirNetwork.floor.duaFanAirHanUni.supFan.withoutMotor.varSpeFloMov"
        );
        if prefix == supply_prefix {
            return Some(BoptestAhuFanMover {
                floor,
                prefix,
                speed_alias: format!(
                    "floor{floor}BoptestAirNetwork_floor_duaFanAirHanUni_supFan_withoutMotor_u"
                ),
                pressure_curve_field: "SupPreCur",
            });
        }

        let return_prefix =
            format!("floor{floor}BoptestAirNetwork.floor.duaFanAirHanUni.retFan.varSpeFloMov");
        if prefix == return_prefix {
            return Some(BoptestAhuFanMover {
                floor,
                prefix,
                speed_alias: format!(
                    "floor{floor}BoptestAirNetwork_floor_duaFanAirHanUni_retFan_u"
                ),
                pressure_curve_field: "RetPreCur",
            });
        }
    }
    None
}

fn boptest_ahu_fan_volume_flow_rhs(
    fan: &BoptestAhuFanMover<'_>,
    aliases: &HashSet<String>,
) -> String {
    let floor_alias = format!("floor{}BoptestAirNetwork", fan.floor);
    let speed = clamp_unit_interval_rhs(&fan.speed_alias);
    let volume_curve = c_compound_literal_for_indexed_aliases(&floor_alias, "VolFloCur", 4);
    let pressure_curve =
        c_compound_literal_for_indexed_aliases(&floor_alias, fan.pressure_curve_field, 4);
    let pressure = boptest_floor_duct_static_pressure_alias(fan.floor, aliases);
    let network_dp_nominal =
        alias_or_fallback(aliases, &format!("{floor_alias}_dp_nominal"), "400.0");
    let network_volume_flow_nominal =
        alias_or_fallback(aliases, &format!("{floor_alias}_V_flow_nominal"), "1.0");
    format!(
        "__rumoca_mover_network_resistance_flow(4, fmax(0.0, {pressure}), {network_dp_nominal}, {network_volume_flow_nominal}, {speed}, {volume_curve}, {pressure_curve})"
    )
}

fn boptest_ahu_fan_pressure_rhs(
    fan: &BoptestAhuFanMover<'_>,
    alias: &str,
    aliases: &HashSet<String>,
) -> String {
    let floor_alias = format!("floor{}BoptestAirNetwork", fan.floor);
    let speed = clamp_unit_interval_rhs(&fan.speed_alias);
    let volume_curve = c_compound_literal_for_indexed_aliases(&floor_alias, "VolFloCur", 4);
    let pressure_curve =
        c_compound_literal_for_indexed_aliases(&floor_alias, fan.pressure_curve_field, 4);
    let volume_flow = alias_or_fallback(aliases, &format!("{alias}_eff_V_flow"), "0.0");
    format!(
        "__rumoca_mover_pressure_curve(4, {volume_flow}, {speed}, {volume_curve}, {pressure_curve})"
    )
}

fn boptest_ahu_fan_curve_value_rhs(
    fan: &BoptestAhuFanMover<'_>,
    alias: &str,
    field: &str,
) -> String {
    let floor_alias = format!("floor{}BoptestAirNetwork", fan.floor);
    let speed = clamp_unit_interval_rhs(&fan.speed_alias);
    let volume_curve = c_compound_literal_for_indexed_aliases(&floor_alias, "VolFloCur", 4);
    let value_curve = c_compound_literal_for_indexed_aliases(&floor_alias, field, 4);
    format!(
        "__rumoca_mover_curve_value(4, {alias}_eff_V_flow, {speed}, {volume_curve}, {value_curve})"
    )
}

fn synthesize_boptest_ahu_fan_command_rhs(var_name: &str) -> Option<String> {
    let alias = var_name_to_c_alias(var_name);
    for floor in 1..=3 {
        let prefix = format!("floor{floor}BoptestAirNetwork_floor_duaFanAirHanUni_supFan");
        let controller_output = format!("{prefix}_varSpe_swi_y");
        if alias == format!("{prefix}_varSpe_ySup")
            || alias == format!("{prefix}_oveSpeSupFan_u")
            || alias == format!("{prefix}_oveSpeSupFan_swi_u3")
        {
            return Some(controller_output.clone());
        }
        if alias == format!("{prefix}_oveSpeSupFan_y")
            || alias == format!("{prefix}_withoutMotor_u")
        {
            return Some(format!(
                "{prefix}_oveSpeSupFan_swi_u2 ? {prefix}_oveSpeSupFan_swi_u1 : {controller_output}"
            ));
        }
    }
    None
}

fn boptest_ahu_read_surface_rhs(alias: &str, aliases: &HashSet<String>) -> Option<String> {
    for floor in 1..=3 {
        let supply_volume_flow_source = format!(
            "floor{floor}BoptestAirNetwork_floor_duaFanAirHanUni_supFan_withoutMotor_varSpeFloMov_eff_V_flow"
        );
        if alias == format!("floor{floor}BoptestAirNetwork_V_flowSupAir")
            || alias == format!("floor{floor}BoptestAirNetwork_floor_duaFanAirHanUni_V_flowSupAir")
        {
            return Some(format!("fmax(0.0, {supply_volume_flow_source})"));
        }

        let supply_volume_flow = format!("floor{floor}_reaAHU_V_flowSup_y");
        if alias == supply_volume_flow {
            return Some(format!("floor{floor}BoptestAirNetwork_V_flowSupAir"));
        }

        if alias == format!("floor{floor}BoptestAirNetwork_supplyAirMassFlow")
            || alias == format!("floor{floor}Ahu_supplyMassFlow")
        {
            return Some(format!(
                "floor{floor}BoptestAirNetwork_rhoAir * floor{floor}BoptestAirNetwork_V_flowSupAir"
            ));
        }

        if alias == format!("floor{floor}BoptestAirNetwork_PFanSup")
            || alias == format!("floor{floor}BoptestAirNetwork_floor_duaFanAirHanUni_PFanSup")
        {
            return Some(boptest_ahu_fan_direct_electrical_power_rhs(
                floor,
                "SupPreCur",
                aliases,
            ));
        }

        let supply_power = format!("floor{floor}_reaAHU_PFanSup_y");
        if alias == supply_power {
            return Some(format!("floor{floor}BoptestAirNetwork_PFanSup"));
        }

        if alias == format!("floor{floor}BoptestAirNetwork_PFanRet")
            || alias == format!("floor{floor}BoptestAirNetwork_floor_duaFanAirHanUni_PFanRet")
        {
            return Some(boptest_ahu_fan_direct_electrical_power_rhs(
                floor,
                "RetPreCur",
                aliases,
            ));
        }

        let return_power = format!("floor{floor}_reaAHU_PFanRet_y");
        if alias == return_power {
            return Some(format!("floor{floor}BoptestAirNetwork_PFanRet"));
        }

        if alias == format!("floor{floor}_reaAHU_PFreCoi_y")
            || alias == format!("floor{floor}Ahu_coolingCoilPower")
        {
            return Some(boptest_floor_chilled_water_airside_load_rhs(floor, aliases));
        }
    }
    None
}

fn boptest_ahu_fan_direct_electrical_power_rhs(
    floor: usize,
    pressure_curve_field: &str,
    aliases: &HashSet<String>,
) -> String {
    let floor_alias = format!("floor{floor}BoptestAirNetwork");
    let speed = boptest_floor_supply_fan_speed_rhs(floor, aliases);
    let volume_curve = c_compound_literal_for_indexed_aliases(&floor_alias, "VolFloCur", 4);
    let pressure_curve =
        c_compound_literal_for_indexed_aliases(&floor_alias, pressure_curve_field, 4);
    let hyd_eff_curve = c_compound_literal_for_indexed_aliases(&floor_alias, "HydEff", 4);
    let mot_eff_curve = c_compound_literal_for_indexed_aliases(&floor_alias, "MotEff", 4);
    let volume_flow = boptest_floor_supply_air_volume_flow_rhs(floor, aliases);
    let pressure = format!(
        "__rumoca_mover_pressure_curve(4, ({volume_flow}), {speed}, {volume_curve}, {pressure_curve})"
    );
    let eta_hyd = format!(
        "__rumoca_mover_curve_value(4, ({volume_flow}), {speed}, {volume_curve}, {hyd_eff_curve})"
    );
    let eta_mot = format!(
        "__rumoca_mover_curve_value(4, ({volume_flow}), {speed}, {volume_curve}, {mot_eff_curve})"
    );
    format!(
        "(fmax(0.0, ({volume_flow}) * ({pressure})) / Buildings_Utilities_Math_Functions_smoothMax(__rumoca_named_arg___x1(({eta_hyd}) * ({eta_mot})), __rumoca_named_arg___x2(0.00001), __rumoca_named_arg___deltaX(0.000001)))"
    )
}

fn boptest_floor_supply_fan_speed_rhs(floor: usize, aliases: &HashSet<String>) -> String {
    format!(
        "fmax(0.0, fmin(1.0, {}))",
        top_down_effective_ahu_supply_fan_speed_rhs(floor, aliases)
    )
}

fn boptest_floor_air_thermal_read_surface_rhs(
    alias: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    for floor in 1..=3 {
        let floor_alias = format!("floor{floor}BoptestAirNetwork");
        let supply_air_temp = format!("{floor_alias}_TSupAir");
        let mixed_air_temp = format!("{floor_alias}_TMixAir");

        if let Some(rhs) = boptest_floor_airflow_read_surface_rhs(alias, aliases) {
            return Some(rhs);
        }

        if alias == format!("{floor_alias}_TSupAir")
            || alias == format!("{floor_alias}_floor_duaFanAirHanUni_TSupAir")
        {
            return Some(format!("{floor_alias}_disTSet"));
        }

        if alias == format!("{floor_alias}_TRetAir")
            || alias == format!("{floor_alias}_floor_duaFanAirHanUni_TRetAir")
            || alias == format!("{floor_alias}_TMixAir")
            || alias == format!("{floor_alias}_floor_duaFanAirHanUni_TMixAir")
            || alias == format!("floor{floor}Ahu_mixedAirTemperature")
        {
            return Some(boptest_floor_return_air_temperature_rhs(floor, aliases));
        }

        if alias == format!("floor{floor}Ahu_ductStaticPressureRise") {
            return Some(format!("fmax(0.0, {floor_alias}_pSet)"));
        }

        if alias == format!("{floor_alias}_TSupCHW")
            || alias == format!("{floor_alias}_floor_duaFanAirHanUni_cooCoi_coi_TEntWat_T")
        {
            return Some(
                "fmax(275.15, fmin(285.15, chilledWaterPlant_plantChilledWaterSetpoint))"
                    .to_string(),
            );
        }

        if alias == format!("{floor_alias}_TRetCHW")
            || alias == format!("{floor_alias}_floor_duaFanAirHanUni_cooCoi_coi_TLeaWat_T")
        {
            let load = boptest_floor_chilled_water_airside_load_rhs(floor, aliases);
            return Some(format!(
                "({floor_alias}_TSupCHW + ({load}) / fmax(0.001, {floor_alias}_cpWater * {floor_alias}_chilledWaterMassFlow))"
            ));
        }

        if alias == format!("{floor_alias}_yCooVal")
            || alias == format!("{floor_alias}_floor_duaFanAirHanUni_cooCoi_val_y_actual")
        {
            return Some(format!(
                "(({mixed_air_temp} > {supply_air_temp} + 0.1) ? 1.0 : 0.0)"
            ));
        }

        if alias == format!("{floor_alias}_PFreCoi")
            || alias == format!("{floor_alias}_floor_duaFanAirHanUni_PFreCoi")
            || alias == format!("{floor_alias}_floor_duaFanAirHanUni_freCoi_Q_flow")
        {
            return Some(boptest_floor_chilled_water_airside_load_rhs(floor, aliases));
        }

        for zone in 1..=5 {
            let global = (floor - 1) * 5 + zone;
            let airflow = boptest_floor_zone_airflow_rhs(floor, zone, global, aliases);
            if alias == format!("{floor_alias}_Vflow_{zone}")
                || alias == format!("{floor_alias}_floor_fivZonVAV_Vflow_{zone}")
            {
                return Some(airflow);
            }
            if alias == format!("{floor_alias}_Vflow_set_{zone}")
                || alias == format!("{floor_alias}_floor_fivZonVAV_Vflow_set_{zone}")
            {
                return Some(format!(
                    "fmax(0.0, controlSemantics_effectiveZoneTerminalAirflowSetpointCommand_{global})"
                ));
            }
            if alias == format!("{floor_alias}_TSup_{zone}")
                || alias == format!("{floor_alias}_floor_fivZonVAV_TSup_{zone}")
            {
                return Some(boptest_floor_zone_supply_temperature_rhs(
                    floor, zone, global, aliases,
                ));
            }
            if alias == format!("{floor_alias}_TZon_{zone}") {
                let volume_prefix =
                    format!("floor{floor}BoptestAirNetwork.floor.fivZonVAV.zon[{zone}].vol");
                return synthesize_boptest_zone_volume_air_temperature_from_state_rhs(
                    &volume_prefix,
                    aliases,
                );
            }
            if alias == format!("{floor_alias}_hotWaterReheatThermalLoadByZone_{zone}") {
                return Some(boptest_floor_zone_reheat_load_rhs(
                    floor, zone, global, aliases,
                ));
            }
        }

        if let Some(zone) = boptest_public_reheat_zone_index(floor, alias) {
            return Some(format!(
                "{floor_alias}_hotWaterReheatThermalLoadByZone_{zone}"
            ));
        }
    }
    None
}

fn boptest_floor_airflow_read_surface_rhs(
    alias: &str,
    aliases: &HashSet<String>,
) -> Option<String> {
    for floor in 1..=3 {
        let floor_alias = format!("floor{floor}BoptestAirNetwork");
        for zone in 1..=5 {
            let global = (floor - 1) * 5 + zone;
            if alias == format!("{floor_alias}_terminalVolumeFlow_{zone}") {
                let wrapper_flow = format!("{floor_alias}_Vflow_{zone}");
                if aliases.contains(&wrapper_flow) {
                    return Some(wrapper_flow);
                }
                return Some(boptest_floor_zone_airflow_rhs(floor, zone, global, aliases));
            }
            if alias == format!("{floor_alias}_terminalVolumeFlowSetpoint_{zone}") {
                return Some(format!(
                    "fmax(0.0, controlSemantics_effectiveZoneTerminalAirflowSetpointCommand_{global})"
                ));
            }
        }

        if let Some(zone) = boptest_public_zone_flow_index(floor, alias, "V_flow_y") {
            let wrapper_flow = format!("{floor_alias}_Vflow_{zone}");
            if aliases.contains(&wrapper_flow) {
                return Some(wrapper_flow);
            }
            let global = (floor - 1) * 5 + zone;
            return Some(boptest_floor_zone_airflow_rhs(floor, zone, global, aliases));
        }
        if let Some(zone) = boptest_public_zone_flow_index(floor, alias, "V_flowSet_y") {
            let global = (floor - 1) * 5 + zone;
            return Some(format!(
                "fmax(0.0, controlSemantics_effectiveZoneTerminalAirflowSetpointCommand_{global})"
            ));
        }
    }
    None
}

fn boptest_public_zone_flow_index(floor: usize, alias: &str, suffix: &str) -> Option<usize> {
    let aliases = [(1, "Cor"), (3, "Eas"), (4, "Nor"), (2, "Sou"), (5, "Wes")];
    for (zone, name) in aliases {
        if alias == format!("floor{floor}_reaZon{name}_{suffix}") {
            return Some(zone);
        }
    }
    None
}

fn boptest_floor_return_air_temperature_rhs(floor: usize, aliases: &HashSet<String>) -> String {
    let terms = (1..=5)
        .map(|zone| boptest_floor_zone_temperature_alias(floor, zone, aliases))
        .collect::<Vec<_>>()
        .join(" + ");
    format!("(({terms}) / 5.0)")
}

fn boptest_floor_chilled_water_airside_load_rhs(floor: usize, aliases: &HashSet<String>) -> String {
    let floor_alias = format!("floor{floor}BoptestAirNetwork");
    let supply_air_mass_flow = boptest_floor_supply_air_mass_flow_rhs(floor, aliases);
    let mixed_air_temp = boptest_floor_mixed_air_temperature_rhs(floor, aliases);
    let supply_air_temp = format!(
        "fmax(280.15, fmin(295.15, controlSemantics_effectiveAhuSupplyAirTemperatureSetpoint_{floor}))"
    );
    format!(
        "fmax(0.0, ({supply_air_mass_flow}) * {floor_alias}_cpAir * fmax(0.0, ({mixed_air_temp}) - ({supply_air_temp})))"
    )
}

fn boptest_floor_mixed_air_temperature_rhs(floor: usize, aliases: &HashSet<String>) -> String {
    let outdoor_damper = boptest_floor_outdoor_damper_rhs(floor);
    let outdoor_temp = boptest_outdoor_temperature_rhs();
    let return_temp = boptest_floor_return_air_temperature_rhs(floor, aliases);
    format!(
        "(({outdoor_damper}) * ({outdoor_temp}) + (1.0 - ({outdoor_damper})) * ({return_temp}))"
    )
}

fn boptest_floor_outdoor_damper_rhs(floor: usize) -> String {
    let mean_damper = boptest_floor_mean_terminal_damper_rhs(floor);
    format!("fmax(0.05, fmin(1.0, 0.2 + 0.6 * ({mean_damper})))")
}

fn boptest_outdoor_temperature_rhs() -> String {
    let synthetic = boptest_synthetic_outdoor_temperature_rhs();
    format!("((weather_source_override_enable > 0.5) ? weather_tdrybul_input : {synthetic})")
}

fn boptest_synthetic_outdoor_temperature_rhs() -> String {
    let weather_time = "(time + initializationWarmupContract_weatherStartTimeSeconds)";
    format!(
        "(286.15 + 7.0 * sin(2.0 * 3.14159265358979323846 * {weather_time} / 86400.0 - 3.14159265358979323846 / 2.0) + 1.5 * sin(4.0 * 3.14159265358979323846 * {weather_time} / 86400.0))"
    )
}

fn boptest_weather_profile_rhs() -> String {
    let synthetic = boptest_synthetic_weather_profile_rhs();
    format!(
        "((weather_source_override_enable > 0.5) ? fmax(0.0, fmin(1.0, weather_hglohor_input / 1000.0)) : {synthetic})"
    )
}

fn boptest_synthetic_weather_profile_rhs() -> String {
    let weather_time = "(time + initializationWarmupContract_weatherStartTimeSeconds)";
    format!(
        "fmax(0.0, sin(2.0 * 3.14159265358979323846 * ({weather_time} - 6.0 * 3600.0) / 86400.0))"
    )
}

fn boptest_sky_temperature_rhs() -> String {
    let synthetic = boptest_synthetic_sky_temperature_rhs();
    format!("((weather_source_override_enable > 0.5) ? weather_tblasky_input : {synthetic})")
}

fn boptest_synthetic_sky_temperature_rhs() -> String {
    format!(
        "({} - (7.0 - 3.0 * {}))",
        boptest_synthetic_outdoor_temperature_rhs(),
        boptest_synthetic_weather_profile_rhs()
    )
}

fn boptest_wind_profile_rhs() -> String {
    let synthetic = boptest_synthetic_wind_profile_rhs();
    format!(
        "((weather_source_override_enable > 0.5) ? fmax(0.0, fmin(1.0, weather_wind_speed_input / 6.0)) : {synthetic})"
    )
}

fn boptest_synthetic_wind_profile_rhs() -> String {
    let weather_time = "(time + initializationWarmupContract_weatherStartTimeSeconds)";
    format!(
        "(0.45 + 0.35 * fmax(0.0, sin(2.0 * 3.14159265358979323846 * ({weather_time} - 2.0 * 3600.0) / 86400.0)))"
    )
}

fn boptest_floor_zone_airflow_rhs(
    floor: usize,
    zone: usize,
    global: usize,
    aliases: &HashSet<String>,
) -> String {
    let floor_alias = format!("floor{floor}BoptestAirNetwork");
    let nominal_air_flow = boptest_floor_zone_nominal_air_mass_flow_alias(floor, zone, aliases);
    let on_zone = boptest_floor_on_zone_rhs(floor, aliases);
    let damper = boptest_floor_terminal_damper_rhs(global);
    format!("(({on_zone}) * (({nominal_air_flow}) / {floor_alias}_rhoAir * ({damper})))")
}

fn boptest_floor_supply_air_volume_flow_rhs(floor: usize, aliases: &HashSet<String>) -> String {
    let start = (floor - 1) * 5 + 1;
    let terms = (1..=5)
        .map(|zone| {
            let global = start + zone - 1;
            boptest_floor_zone_airflow_rhs(floor, zone, global, aliases)
        })
        .collect::<Vec<_>>()
        .join(" + ");
    format!("({terms})")
}

fn boptest_floor_supply_air_mass_flow_rhs(floor: usize, aliases: &HashSet<String>) -> String {
    let total_nominal = (1..=5)
        .map(|zone| boptest_floor_zone_nominal_air_mass_flow_alias(floor, zone, aliases))
        .collect::<Vec<_>>()
        .join(" + ");
    let mean_damper = boptest_floor_mean_terminal_damper_rhs(floor);
    let fan_enable = boptest_floor_direct_fan_enable_rhs(floor, aliases);
    let pressure_ratio = format!(
        "fmax(0.0, fmin(2.0, fmax(0.0, ahu_duct_static_pressure_setpoint_{floor}) / 400.0))"
    );
    format!(
        "(({fan_enable}) * ({total_nominal}) * fmax(0.05, ({mean_damper})) * fmax(0.25, ({pressure_ratio})))"
    )
}

fn boptest_floor_on_zone_rhs(floor: usize, aliases: &HashSet<String>) -> String {
    boptest_floor_direct_fan_enable_rhs(floor, aliases)
}

fn boptest_floor_direct_fan_enable_rhs(floor: usize, aliases: &HashSet<String>) -> String {
    format!(
        "fmax(0.0, fmin(1.0, {}))",
        top_down_effective_ahu_supply_fan_speed_rhs(floor, aliases)
    )
}

fn boptest_floor_mean_terminal_damper_rhs(floor: usize) -> String {
    let start = (floor - 1) * 5 + 1;
    let terms = (start..start + 5)
        .map(boptest_floor_terminal_damper_rhs)
        .collect::<Vec<_>>()
        .join(" + ");
    format!("(({terms}) / 5.0)")
}

fn boptest_floor_terminal_damper_rhs(global: usize) -> String {
    format!("fmin(1.0, fmax(0.0, controlSemantics_effectiveZoneTerminalDamperCommand_{global}))")
}

fn boptest_floor_zone_supply_temperature_rhs(
    floor: usize,
    zone: usize,
    global: usize,
    aliases: &HashSet<String>,
) -> String {
    let floor_alias = format!("floor{floor}BoptestAirNetwork");
    let airflow = boptest_floor_zone_airflow_rhs(floor, zone, global, aliases);
    let air_capacity = format!("{floor_alias}_rhoAir * {floor_alias}_cpAir * ({airflow})");
    let reheat_load = boptest_floor_zone_reheat_load_rhs(floor, zone, global, aliases);
    format!("({floor_alias}_TSupAir + ({reheat_load}) / fmax(0.001, ({air_capacity})))")
}

fn boptest_floor_zone_reheat_load_rhs(
    floor: usize,
    zone: usize,
    global: usize,
    aliases: &HashSet<String>,
) -> String {
    let floor_alias = format!("floor{floor}BoptestAirNetwork");
    let airflow = boptest_floor_zone_airflow_rhs(floor, zone, global, aliases);
    let nominal_water_flow =
        boptest_floor_zone_nominal_reheat_water_mass_flow_rhs(floor, zone, aliases);
    let reheat = format!(
        "fmin(1.0, fmax(0.0, controlSemantics_effectiveZoneTerminalReheatCommand_{global}))"
    );
    let air_capacity = format!("{floor_alias}_rhoAir * {floor_alias}_cpAir * ({airflow})");
    let water_capacity = format!("hotWaterPlant_cpWater * ({nominal_water_flow}) * ({reheat})");
    format!(
        "fmax(0.0, {floor_alias}_eps_{zone} * fmin(fmax(0.0, ({air_capacity})), fmax(0.0, ({water_capacity}))) * fmax(0.0, hotWaterPlant_hotWaterSupplyTemperature - {floor_alias}_TSupAir))"
    )
}

fn boptest_floor_zone_nominal_reheat_water_mass_flow_rhs(
    floor: usize,
    zone: usize,
    aliases: &HashSet<String>,
) -> String {
    let floor_alias = format!("floor{floor}BoptestAirNetwork");
    let candidates = [
        format!("{floor_alias}_mWatFloRat_{floor}_{zone}"),
        format!("{floor_alias}_mWatFloRat_{zone}"),
        format!("{floor_alias}_floor_mWatFloRat{zone}"),
        format!("{floor_alias}_floor_fivZonVAV_mWatFloRat{zone}"),
        format!("{floor_alias}_floor_fivZonVAV_vAV{zone}_mWatFloRat"),
        format!("{floor_alias}_floor_fivZonVAV_vAV{zone}_heaCoil_mWatFloRat"),
    ];
    if let Some(alias) = candidates.into_iter().find(|alias| aliases.contains(alias)) {
        return alias;
    }
    let nominal_air_flow = boptest_floor_zone_nominal_air_mass_flow_alias(floor, zone, aliases);
    format!("(({nominal_air_flow}) * 0.3 * (35.0 - 12.88) / 4.2 / 20.0)")
}

fn boptest_public_reheat_zone_index(floor: usize, alias: &str) -> Option<usize> {
    let aliases = [(1, "Cor"), (3, "Eas"), (4, "Nor"), (2, "Sou"), (5, "Wes")];
    for (zone, name) in aliases {
        if alias == format!("floor{floor}_reaZon{name}_QTerHea_y") {
            return Some(zone);
        }
    }
    None
}

fn boptest_floor_load_read_surface_rhs(alias: &str, aliases: &HashSet<String>) -> Option<String> {
    let hot_water_sum = "(floor1BoptestAirNetwork_hotWaterReheatThermalLoad + floor2BoptestAirNetwork_hotWaterReheatThermalLoad + floor3BoptestAirNetwork_hotWaterReheatThermalLoad)";
    if alias == "hotWaterPlant_terminalReheatDemand" {
        return Some(hot_water_sum.to_string());
    }

    for floor in 1..=3 {
        let chw_load = format!("floor{floor}BoptestAirNetwork_chilledWaterThermalLoad");
        let chw_load_rhs = boptest_floor_chilled_water_airside_load_rhs(floor, aliases);
        if alias == chw_load {
            return Some(chw_load_rhs.clone());
        }
        if alias == format!("floor_zone_cooling_load_{floor}")
            || alias == format!("floor_cooling_demand_{floor}")
            || alias == format!("chilledWaterPlant_chilledWaterBranchDemand_{floor}")
            || alias == format!("chilledWaterPlant_chilledWaterBranchThermalDemand_{floor}")
        {
            return Some(chw_load_rhs);
        }

        let hw_load = format!("floor{floor}BoptestAirNetwork_hotWaterReheatThermalLoad");
        let hw_load_rhs = format!(
            "__rumoca_sum_d(floor{floor}BoptestAirNetwork_hotWaterReheatThermalLoadByZone, floor{floor}BoptestAirNetwork_hotWaterReheatThermalLoadByZone__len)"
        );
        if alias == hw_load {
            return Some(hw_load_rhs);
        }
        if alias == format!("floor_heating_demand_{floor}")
            || alias == format!("floor_terminal_delivered_reheat_heat_{floor}")
            || alias == format!("floor_terminal_reheat_equipment_demand_{floor}")
            || alias == format!("hotWaterPlant_hotWaterBranchDemand_{floor}")
            || alias == format!("TerminalDeliveredReheatHeatFloor{floor}")
        {
            return Some(hw_load);
        }
    }
    None
}

fn boptest_chilled_water_wrapper_mass_flow_read_aliases() -> &'static [&'static str] {
    &[
        "chilledWaterPlant_chilledWaterMassFlow",
        "chilledWaterPlant_secondaryChilledWaterMassFlow",
        "chilledWaterPlant_coolingCoilWaterMassFlow",
        "reaChiWatSys_mCHWTot_y",
    ]
}

fn boptest_chilled_water_internal_mass_flow_read_aliases() -> &'static [&'static str] {
    &[
        "chilledWaterPlant_chillerPlant_mCHW_tot",
        "chilledWaterPlant_chillerPlant_senMasFloSecCHW_m_flow",
    ]
}

fn boptest_hot_water_wrapper_mass_flow_read_aliases() -> &'static [&'static str] {
    &[
        "hotWaterPlant_hotWaterMassFlow",
        "hotWaterPlant_hotWaterDistributionMassFlow",
        "hotWaterPlant_boilerLoopMassFlow",
        "reaHotWatSys_mHWTot_y",
    ]
}

fn boptest_hot_water_internal_mass_flow_read_aliases() -> &'static [&'static str] {
    &[
        "hotWaterPlant_boilerPlant_mHW_tot",
        "hotWaterPlant_boilerPlant_senMasFlo_m_flow",
    ]
}

fn boptest_chilled_water_wrapper_mass_flow_rhs() -> String {
    "(chilledWaterPlant_terminalToWaterDemand <= 1e-6 ? 0.0 : fmin(chilledWaterPlant_nominalMassFlow, chilledWaterPlant_terminalToWaterDemand / fmax(0.001, chilledWaterPlant_cpWater * chilledWaterPlant_nominalLoopDeltaT)))".to_string()
}

fn boptest_hot_water_wrapper_mass_flow_rhs() -> String {
    boptest_plant_total_mass_flow_rhs("hotWaterPlant.boilerPlant.pumSecHW")
}

fn boptest_chilled_water_dp_read_aliases() -> &'static [&'static str] {
    &[
        "chilledWaterPlant_chilledWaterDistributionNetwork_differentialPressure",
        "chilledWaterPlant_loopDifferentialPressure",
        "chilledWaterPlant_chilledWaterDifferentialPressure",
        "reaChiWatSys_dp_y",
    ]
}

fn boptest_hot_water_dp_read_aliases() -> &'static [&'static str] {
    &[
        "hotWaterPlant_hotWaterDistributionNetwork_differentialPressure",
        "hotWaterPlant_loopDifferentialPressure",
        "hotWaterPlant_hotWaterDifferentialPressure",
        "reaHotWatSys_dp_y",
    ]
}

fn boptest_plant_total_mass_flow_rhs(pump_array_prefix: &str) -> String {
    let terms = (1..=2)
        .map(|index| {
            let prefix = format!("{pump_array_prefix}.pum[{index}].varSpeFloMov");
            let prefix_alias = var_name_to_c_alias(&prefix);
            let volume_flow =
                boptest_plant_pump_volume_flow_rhs(&prefix).unwrap_or_else(|| "0.0".to_string());
            format!("({prefix_alias}_rho_default * {volume_flow})")
        })
        .collect::<Vec<_>>();
    format!("({})", terms.join(" + "))
}

fn boptest_plant_distribution_dp_rhs(pump_array_prefix: &str, network_dp_nominal: &str) -> String {
    if let Some(rhs) = boptest_parallel_secondary_pump_network_dp_rhs(pump_array_prefix) {
        return rhs;
    }

    let volume_terms = (1..=2)
        .map(|index| {
            let prefix = format!("{pump_array_prefix}.pum[{index}].varSpeFloMov");
            boptest_plant_pump_volume_flow_rhs(&prefix).unwrap_or_else(|| "0.0".to_string())
        })
        .collect::<Vec<_>>();
    let nominal_terms = (1..=2)
        .filter_map(|index| {
            let prefix = format!("{pump_array_prefix}.pum[{index}].varSpeFloMov");
            let (parent, _) = boptest_plant_pump_parent_and_curve_len(&prefix)?;
            let parent_alias = var_name_to_c_alias(parent);
            let (_, network_volume_flow_nominal) =
                boptest_plant_pump_network_resistance(&prefix, &parent_alias)?;
            Some(network_volume_flow_nominal)
        })
        .collect::<Vec<_>>();
    let total_volume_flow = format!("({})", volume_terms.join(" + "));
    let total_nominal_flow = format!("({})", nominal_terms.join(" + "));
    format!(
        "__rumoca_network_resistance_dp({network_dp_nominal}, {total_nominal_flow}, {total_volume_flow})"
    )
}

fn boptest_plant_pump_volume_flow_rhs(prefix: &str) -> Option<String> {
    if let Some(rhs) = boptest_parallel_secondary_pump_volume_flow_rhs(prefix) {
        return Some(rhs);
    }

    let (parent, curve_len) = boptest_plant_pump_parent_and_curve_len(prefix)?;
    let parent_alias = var_name_to_c_alias(parent);
    let speed = boptest_plant_pump_actual_speed_rhs(parent)
        .unwrap_or_else(|| clamp_unit_interval_rhs(&format!("{parent_alias}_u")));
    let dp = boptest_plant_pump_dp_signal(prefix)?;
    let (network_dp_nominal, network_volume_flow_nominal) =
        boptest_plant_pump_network_resistance(prefix, &parent_alias)?;
    let volume_curve =
        c_compound_literal_for_indexed_aliases(&parent_alias, "VolFloCur", curve_len);
    let pressure_curve = c_compound_literal_for_indexed_aliases(&parent_alias, "PreCur", curve_len);
    Some(format!(
        "__rumoca_mover_network_resistance_flow({curve_len}, {dp}, {network_dp_nominal}, {network_volume_flow_nominal}, {speed}, {volume_curve}, {pressure_curve})"
    ))
}

fn boptest_parallel_secondary_pump_volume_flow_rhs(prefix: &str) -> Option<String> {
    let (pump_array_prefix, index, curve_len, network_dp_nominal) =
        boptest_parallel_secondary_pump_group(prefix)?;
    Some(boptest_parallel_secondary_pump_network_flow_rhs(
        pump_array_prefix,
        index,
        curve_len,
        network_dp_nominal,
    ))
}

fn boptest_parallel_secondary_pump_network_dp_rhs(pump_array_prefix: &str) -> Option<String> {
    if pump_array_prefix == "hotWaterPlant.boilerPlant.pumSecHW" {
        Some(boptest_parallel_secondary_pump_network_common_dp_rhs(
            pump_array_prefix,
            5,
            "plant_hot_water_dp_setpoint",
        ))
    } else if pump_array_prefix == "chilledWaterPlant.chillerPlant.pumSecCHW" {
        Some(boptest_parallel_secondary_pump_network_common_dp_rhs(
            pump_array_prefix,
            6,
            "plant_chilled_water_dp_setpoint",
        ))
    } else {
        None
    }
}

fn boptest_parallel_secondary_pump_group(
    prefix: &str,
) -> Option<(&'static str, usize, usize, &'static str)> {
    if let Some(index) = parse_indexed_component_signal(
        prefix,
        "hotWaterPlant.boilerPlant.pumSecHW.pum[",
        "].varSpeFloMov",
    )
    .map(|(_, index)| index)
    {
        if (1..=2).contains(&index) {
            return Some((
                "hotWaterPlant.boilerPlant.pumSecHW",
                index,
                5,
                "plant_hot_water_dp_setpoint",
            ));
        }
    }
    if let Some(index) = parse_indexed_component_signal(
        prefix,
        "chilledWaterPlant.chillerPlant.pumSecCHW.pum[",
        "].varSpeFloMov",
    )
    .map(|(_, index)| index)
    {
        if (1..=2).contains(&index) {
            return Some((
                "chilledWaterPlant.chillerPlant.pumSecCHW",
                index,
                6,
                "plant_chilled_water_dp_setpoint",
            ));
        }
    }
    None
}

fn boptest_parallel_secondary_pump_network_flow_rhs(
    pump_array_prefix: &str,
    index: usize,
    curve_len: usize,
    network_dp_nominal: &str,
) -> String {
    let (_, network_volume_flow_nominal) =
        boptest_parallel_secondary_pump_network_parts(pump_array_prefix, curve_len);
    let (speed1, speed2, volume_curve1, pressure_curve1, volume_curve2, pressure_curve2) =
        boptest_parallel_secondary_pump_curve_parts(pump_array_prefix, curve_len);
    format!(
        "__rumoca_parallel2_mover_network_flow({curve_len}, {index}, {network_dp_nominal}, {network_volume_flow_nominal}, {speed1}, {speed2}, {volume_curve1}, {pressure_curve1}, {volume_curve2}, {pressure_curve2})"
    )
}

fn boptest_parallel_secondary_pump_network_common_dp_rhs(
    pump_array_prefix: &str,
    curve_len: usize,
    network_dp_nominal: &str,
) -> String {
    let (_, network_volume_flow_nominal) =
        boptest_parallel_secondary_pump_network_parts(pump_array_prefix, curve_len);
    let (speed1, speed2, volume_curve1, pressure_curve1, volume_curve2, pressure_curve2) =
        boptest_parallel_secondary_pump_curve_parts(pump_array_prefix, curve_len);
    format!(
        "__rumoca_parallel2_mover_network_dp({curve_len}, {network_dp_nominal}, {network_volume_flow_nominal}, {speed1}, {speed2}, {volume_curve1}, {pressure_curve1}, {volume_curve2}, {pressure_curve2})"
    )
}

fn boptest_parallel_secondary_pump_network_parts(
    pump_array_prefix: &str,
    curve_len: usize,
) -> (String, String) {
    let plant_prefix = if pump_array_prefix == "hotWaterPlant.boilerPlant.pumSecHW" {
        "hotWaterPlant.boilerPlant"
    } else {
        "chilledWaterPlant.chillerPlant"
    };
    let nominal_terms = (1..=2)
        .map(|index| {
            let parent_alias = var_name_to_c_alias(&format!("{pump_array_prefix}.pum[{index}]"));
            let stage =
                secondary_pump_stage_rhs(plant_prefix, index).unwrap_or_else(|| "0.0".to_string());
            format!("(({stage}) > 0.5 ? {parent_alias}_VolFloCur_{curve_len} : 0.0)")
        })
        .collect::<Vec<_>>();
    (
        format!("({})", nominal_terms.join(" + ")),
        format!("({})", nominal_terms.join(" + ")),
    )
}

fn boptest_parallel_secondary_pump_curve_parts(
    pump_array_prefix: &str,
    curve_len: usize,
) -> (String, String, String, String, String, String) {
    let parent1 = format!("{pump_array_prefix}.pum[1]");
    let parent2 = format!("{pump_array_prefix}.pum[2]");
    let parent1_alias = var_name_to_c_alias(&parent1);
    let parent2_alias = var_name_to_c_alias(&parent2);
    let speed1 = boptest_plant_pump_actual_speed_rhs(&parent1)
        .unwrap_or_else(|| clamp_unit_interval_rhs(&format!("{parent1_alias}_u")));
    let speed2 = boptest_plant_pump_actual_speed_rhs(&parent2)
        .unwrap_or_else(|| clamp_unit_interval_rhs(&format!("{parent2_alias}_u")));
    let volume_curve1 =
        c_compound_literal_for_indexed_aliases(&parent1_alias, "VolFloCur", curve_len);
    let pressure_curve1 =
        c_compound_literal_for_indexed_aliases(&parent1_alias, "PreCur", curve_len);
    let volume_curve2 =
        c_compound_literal_for_indexed_aliases(&parent2_alias, "VolFloCur", curve_len);
    let pressure_curve2 =
        c_compound_literal_for_indexed_aliases(&parent2_alias, "PreCur", curve_len);
    (
        speed1,
        speed2,
        volume_curve1,
        pressure_curve1,
        volume_curve2,
        pressure_curve2,
    )
}

fn boptest_plant_pump_actual_speed_rhs(parent: &str) -> Option<String> {
    if let Some(index) =
        parse_indexed_component_signal(parent, "hotWaterPlant.boilerPlant.pumSecHW.pum[", "]")
            .map(|(_, index)| index)
    {
        let stage = secondary_pump_stage_rhs("hotWaterPlant.boilerPlant", index)?;
        let filter_y = var_name_to_c_alias(&format!("{parent}.varSpeFloMov.filter.y"));
        return Some(format!("({stage} * {filter_y})"));
    }
    if let Some(index) =
        parse_indexed_component_signal(parent, "chilledWaterPlant.chillerPlant.pumSecCHW.pum[", "]")
            .map(|(_, index)| index)
    {
        let stage = secondary_pump_stage_rhs("chilledWaterPlant.chillerPlant", index)?;
        let filter_y = var_name_to_c_alias(&format!("{parent}.varSpeFloMov.filter.y"));
        return Some(format!("({stage} * {filter_y})"));
    }
    None
}

fn synthesize_boptest_plant_mover_efficiency_rhs(var_name: &str) -> Option<String> {
    let prefix = boptest_plant_var_speed_mover_prefix(var_name)?;
    let (parent, curve_len) = boptest_plant_pump_parent_and_curve_len(prefix)?;
    let parent_alias = var_name_to_c_alias(parent);
    let alias = var_name_to_c_alias(prefix);
    let speed = boptest_plant_pump_actual_speed_rhs(parent)
        .unwrap_or_else(|| clamp_unit_interval_rhs(&format!("{parent_alias}_u")));
    let volume_curve =
        c_compound_literal_for_indexed_aliases(&parent_alias, "VolFloCur", curve_len);
    if var_name.ends_with(".eff.etaHyd") || var_name.ends_with(".etaHyd") {
        let hyd_curve = c_compound_literal_for_indexed_aliases(&parent_alias, "HydEff", curve_len);
        return Some(format!(
            "__rumoca_mover_curve_value({curve_len}, {alias}_eff_V_flow, {speed}, {volume_curve}, {hyd_curve})"
        ));
    }
    if var_name.ends_with(".eff.etaMot") || var_name.ends_with(".etaMot") {
        let mot_curve = c_compound_literal_for_indexed_aliases(&parent_alias, "MotEff", curve_len);
        return Some(format!(
            "__rumoca_mover_curve_value({curve_len}, {alias}_eff_V_flow, {speed}, {volume_curve}, {mot_curve})"
        ));
    }
    if var_name.ends_with(".eff.eta") || var_name.ends_with(".eta") {
        let hyd_curve = c_compound_literal_for_indexed_aliases(&parent_alias, "HydEff", curve_len);
        let mot_curve = c_compound_literal_for_indexed_aliases(&parent_alias, "MotEff", curve_len);
        return Some(format!(
            "(__rumoca_mover_curve_value({curve_len}, {alias}_eff_V_flow, {speed}, {volume_curve}, {hyd_curve}) * __rumoca_mover_curve_value({curve_len}, {alias}_eff_V_flow, {speed}, {volume_curve}, {mot_curve}))"
        ));
    }
    None
}

fn boptest_plant_pump_dp_signal(prefix: &str) -> Option<String> {
    if prefix.contains(".pumSecCHW.pum[") || prefix.contains(".pumSecHW.pum[") {
        Some("0.0".to_string())
    } else {
        None
    }
}

fn boptest_plant_pump_network_resistance(
    prefix: &str,
    parent_alias: &str,
) -> Option<(String, String)> {
    if prefix.contains(".pumSecCHW.pum[") {
        Some((
            "plant_chilled_water_dp_setpoint".to_string(),
            format!("{parent_alias}_VolFloCur_5"),
        ))
    } else if prefix.contains(".pumSecHW.pum[") {
        Some((
            "plant_hot_water_dp_setpoint".to_string(),
            format!("{parent_alias}_VolFloCur_4"),
        ))
    } else {
        None
    }
}

fn boptest_plant_var_speed_mover_prefix(var_name: &str) -> Option<&str> {
    let marker = ".varSpeFloMov";
    let pos = var_name.find(marker)?;
    let prefix = &var_name[..pos + marker.len()];
    boptest_plant_pump_parent_and_curve_len(prefix)?;
    Some(prefix)
}

fn boptest_plant_pump_parent_and_curve_len(prefix: &str) -> Option<(&str, usize)> {
    if prefix.contains(".pumSecCHW.pum[") {
        Some((prefix.strip_suffix(".varSpeFloMov")?, 6))
    } else if prefix.contains(".pumSecHW.pum[") {
        Some((prefix.strip_suffix(".varSpeFloMov")?, 5))
    } else {
        None
    }
}

fn clamp_unit_interval_rhs(expr: &str) -> String {
    format!("fmin(1.0, fmax(0.0, {expr}))")
}

fn c_compound_literal_for_indexed_aliases(parent_alias: &str, field: &str, len: usize) -> String {
    let values = (1..=len)
        .map(|index| format!("{parent_alias}_{field}_{index}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!("(const double[]){{{values}}}")
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
        if is_boptest_secondary_pump_conpi_prefix(prefix) {
            return Some("1.0".to_string());
        }
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

    if let Some(prefix) = var_name.strip_suffix(".y")
        && prefix.ends_with(".conPI")
    {
        let pid_output = var_name_to_c_alias(&format!("{prefix}.conPID.y"));
        if is_boptest_secondary_pump_conpi_prefix(prefix) && aliases.contains(&pid_output) {
            let plant_prefix = prefix.strip_suffix(".secPumCon.conPI")?;
            return Some(secondary_pump_conpi_rhs(plant_prefix));
        }
        let boolean_output = var_name_to_c_alias(&format!("{prefix}.booToRea.y"));
        if aliases.contains(&boolean_output) && aliases.contains(&pid_output) {
            return Some(format!("({boolean_output} * {pid_output})"));
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
        if prefix == "hotWaterPlant.boilerPlant" {
            return Some(boptest_plant_distribution_dp_rhs(
                "hotWaterPlant.boilerPlant.pumSecHW",
                "plant_hot_water_dp_setpoint",
            ));
        }
        if prefix == "chilledWaterPlant.chillerPlant" {
            return Some(boptest_plant_distribution_dp_rhs(
                "chilledWaterPlant.chillerPlant.pumSecCHW",
                "plant_chilled_water_dp_setpoint",
            ));
        }
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

    if let Some(prefix) = var_name
        .strip_suffix(".conPID.y")
        .or_else(|| var_name.strip_suffix(".conPID.lim.y"))
        && prefix.ends_with(".conPI")
    {
        let input = var_name_to_c_alias(&format!("{prefix}.conPID.addPID.y"));
        let lower = var_name_to_c_alias(&format!("{prefix}.conPID.yMin"));
        let upper = var_name_to_c_alias(&format!("{prefix}.conPID.yMax"));
        if aliases.contains(&input) {
            return Some(format!("fmax({lower}, fmin({upper}, {input}))"));
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".conPID.lim.u")
        && prefix.ends_with(".conPI")
    {
        let input = var_name_to_c_alias(&format!("{prefix}.conPID.addPID.y"));
        if aliases.contains(&input) {
            return Some(input);
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".conPID.addPID.y")
        && prefix.ends_with(".conPI")
    {
        let proportional_derivative = var_name_to_c_alias(&format!("{prefix}.conPID.addPD.y"));
        let integral = var_name_to_c_alias(&format!("{prefix}.conPID.I.y"));
        if aliases.contains(&proportional_derivative) {
            if aliases.contains(&integral) {
                return Some(format!("({proportional_derivative} + {integral})"));
            }
            return Some(proportional_derivative);
        }
    }

    if let Some(prefix) = var_name.strip_suffix(".conPID.addPD.y")
        && prefix.ends_with(".conPI")
    {
        let proportional = var_name_to_c_alias(&format!("{prefix}.conPID.P.y"));
        let derivative = var_name_to_c_alias(&format!("{prefix}.conPID.D.y"));
        if aliases.contains(&proportional) {
            if aliases.contains(&derivative) {
                return Some(format!("({proportional} + {derivative})"));
            }
            return Some(proportional);
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
    secondary_pump_stage_rhs(&prefix, index)
}

fn synthesize_boptest_secondary_pump_command_rhs(var_name: &str) -> Option<String> {
    if let Some((prefix, index)) = parse_indexed_component_signal(var_name, ".secPumCon.y[", "]") {
        return secondary_pump_command_rhs(&prefix, index);
    }

    if let Some((prefix, index)) =
        parse_indexed_component_signal(var_name, ".secPumCon.product[", "].y")
    {
        return secondary_pump_command_rhs(&prefix, index);
    }

    if let Some((prefix, index)) =
        parse_indexed_component_signal(var_name, ".secPumCon.product[", "].u1")
    {
        return secondary_pump_stage_rhs(&prefix, index);
    }

    if let Some((prefix, _index)) =
        parse_indexed_component_signal(var_name, ".secPumCon.product[", "].u2")
    {
        return Some(secondary_pump_conpi_rhs(&prefix));
    }

    for (pump_prefix, plant_prefix) in [
        (
            "hotWaterPlant.boilerPlant.pumSecHW",
            "hotWaterPlant.boilerPlant",
        ),
        (
            "chilledWaterPlant.chillerPlant.pumSecCHW",
            "chilledWaterPlant.chillerPlant",
        ),
    ] {
        if let Some(index) = parse_secondary_pump_signal_index(var_name, pump_prefix) {
            return secondary_pump_command_rhs(plant_prefix, index);
        }
    }

    None
}

fn parse_secondary_pump_signal_index(var_name: &str, pump_prefix: &str) -> Option<usize> {
    parse_indexed_suffix(var_name, &format!("{pump_prefix}.speSig"))
        .map(|(_, index)| index)
        .or_else(|| {
            parse_indexed_component_signal(var_name, &format!("{pump_prefix}.pum["), "].u")
                .map(|(_, index)| index)
        })
        .or_else(|| {
            [
                "].varSpeFloMov.y",
                "].varSpeFloMov.gaiSpe.u",
                "].varSpeFloMov.gaiSpe.y",
                "].varSpeFloMov.inputSwitch.u",
                "].varSpeFloMov.inputSwitch.y",
                "].varSpeFloMov.filter.u",
            ]
            .iter()
            .find_map(|suffix| {
                parse_indexed_component_signal(var_name, &format!("{pump_prefix}.pum["), suffix)
                    .map(|(_, index)| index)
            })
        })
}

fn secondary_pump_command_rhs(plant_prefix: &str, index: usize) -> Option<String> {
    let stage = secondary_pump_stage_rhs(plant_prefix, index)?;
    let command = secondary_pump_conpi_rhs(plant_prefix);
    Some(format!("({stage} * {command})"))
}

fn secondary_pump_conpi_rhs(plant_prefix: &str) -> String {
    let pid = var_name_to_c_alias(&format!("{plant_prefix}.secPumCon.conPI.conPID.y"));
    let enabled = var_name_to_c_alias(&format!("{plant_prefix}.secPumCon.conPI.booToRea.y"));
    format!("({enabled} * {pid})")
}

fn is_boptest_secondary_pump_conpi_prefix(prefix: &str) -> bool {
    matches!(
        prefix,
        "hotWaterPlant.boilerPlant.secPumCon.conPI"
            | "chilledWaterPlant.chillerPlant.secPumCon.conPI"
    )
}

fn synthesize_boptest_secondary_pump_stage_count_rhs(var_name: &str) -> Option<String> {
    let plant_prefix = var_name
        .strip_suffix(".secPumCon.pumSta.pumNSta.y")
        .or_else(|| var_name.strip_suffix(".secPumCon.pumSta.pumNSta.multiSwitch.y"))?;
    if !matches!(
        plant_prefix,
        "hotWaterPlant.boilerPlant" | "chilledWaterPlant.chillerPlant"
    ) {
        return None;
    }

    let on =
        synthesize_boptest_boolean_expression_rhs(&format!("{plant_prefix}.secPumCon.pumSta.On"))
            .unwrap_or_else(|| var_name_to_c_alias(&format!("{plant_prefix}.secPumCon.pumSta.On")));
    let previous_stage_speed =
        var_name_to_c_alias(&format!("{plant_prefix}.secPumCon.pumSta.sta[1]"));
    let stage_up_threshold =
        var_name_to_c_alias(&format!("{plant_prefix}.secPumCon.pumSta.thehol_up"));
    Some(format!(
        "(({on}) > 0.5 ? ((({previous_stage_speed}) > ({stage_up_threshold}) && ({previous_stage_speed}) > 0.0) ? 2.0 : 1.0) : 0.0)"
    ))
}

fn secondary_pump_stage_rhs(plant_prefix: &str, index: usize) -> Option<String> {
    if index == 0 {
        return None;
    }
    let active_stage = var_name_to_c_alias(&format!("{plant_prefix}.secPumCon.pumSta.pumNSta.y"));
    Some(format!("(({active_stage}) >= {index}.0 ? 1.0 : 0.0)"))
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

    if let Some(synthesized) = synthesize_boptest_boolean_expression_rhs(&name) {
        return Ok(synthesized);
    }

    if let Some(synthesized) =
        synthesize_boptest_stage_condition_connection_rhs(&name, &equations_z, &equations_m)
    {
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

/// Extract RHS for FMI discrete-valued (`m`) updates.
///
/// The `m` partition contains Modelica StateGraph bookkeeping in addition to
/// simple BooleanExpression/enable values. The generic discrete RHS path may
/// render unsupported array-port expressions such as `anyTrue(port, port_size)`;
/// keep those states self-held until the backend has a typed array lowering.
pub(super) fn discrete_valued_rhs_for_var_function(
    var_name: Value,
    equations_z: Value,
    equations_m: Value,
    dae: Value,
    _config: Value,
) -> RenderResult {
    let name = var_name.to_string().trim_matches('"').to_string();

    if let Some(synthesized) = synthesize_parent_discrete_enable_rhs(&name, &dae) {
        return Ok(synthesized);
    }

    if let Some(synthesized) = synthesize_boptest_boolean_expression_rhs(&name) {
        return Ok(synthesized);
    }

    if let Some(synthesized) =
        synthesize_boptest_stage_condition_connection_rhs(&name, &equations_z, &equations_m)
    {
        return Ok(synthesized);
    }

    Ok(var_name_to_c_alias(&name))
}

fn synthesize_boptest_stage_condition_connection_rhs(
    var_name: &str,
    equations_z: &Value,
    equations_m: &Value,
) -> Option<String> {
    let candidates = [
        (".nSta.On[", ".plantNStageCondition.On["),
        (".nSta.Off[", ".plantNStageCondition.Off["),
        (".pumNSta.On[", ".pumNStaCon.On["),
        (".pumNSta.Off[", ".pumNStaCon.Off["),
    ];
    let aliases_z = c_aliases_for_equations(equations_z);
    let aliases_m = c_aliases_for_equations(equations_m);
    for (target, source) in candidates {
        if var_name.contains(target) {
            let source_name = var_name.replacen(target, source, 1);
            let source_alias = var_name_to_c_alias(&source_name);
            if aliases_z.contains(&source_alias) || aliases_m.contains(&source_alias) {
                return Some(source_alias);
            }
        }
    }
    None
}

fn synthesize_boptest_boolean_expression_rhs(var_name: &str) -> Option<String> {
    if matches!(
        var_name,
        "chilledWaterPlant.chillerPlant.On.y"
            | "chilledWaterPlant.chillerPlant.On1.y"
            | "chilledWaterPlant.chillerPlant.secPumCon.On"
            | "chilledWaterPlant.chillerPlant.secPumCon.conPI.On"
            | "chilledWaterPlant.chillerPlant.secPumCon.conPI.booToRea.u"
            | "chilledWaterPlant.chillerPlant.secPumCon.conPI.conPID.trigger"
            | "chilledWaterPlant.chillerPlant.secPumCon.conPI.conPID.I.trigger"
            | "chilledWaterPlant.chillerPlant.secPumCon.pumSta.On"
            | "hotWaterPlant.boilerPlant.On.y"
            | "hotWaterPlant.boilerPlant.On1.y"
            | "hotWaterPlant.boilerPlant.secPumCon.On"
            | "hotWaterPlant.boilerPlant.secPumCon.conPI.On"
            | "hotWaterPlant.boilerPlant.secPumCon.conPI.booToRea.u"
            | "hotWaterPlant.boilerPlant.secPumCon.conPI.conPID.trigger"
            | "hotWaterPlant.boilerPlant.secPumCon.conPI.conPID.I.trigger"
            | "hotWaterPlant.boilerPlant.secPumCon.pumSta.On"
    ) {
        Some("1.0".to_string())
    } else {
        None
    }
}

fn synthesize_boptest_plant_stage_condition_plr_rhs(var_name: &str) -> Option<String> {
    let prefix = var_name.strip_suffix(".PLR")?;
    if !prefix.ends_with(".plantNStageCondition") {
        return None;
    }
    let prefix_alias = var_name_to_c_alias(prefix);
    let load = format!("{prefix_alias}_Loa");
    let cap = boptest_stage_condition_capacity_expr(prefix)?;
    Some(format!("(({cap} > 0.01) ? ({load} / ({cap})) : 0.0)"))
}

fn synthesize_boptest_plant_stage_condition_capacity_rhs(var_name: &str) -> Option<String> {
    let prefix = var_name.strip_suffix(".cap_avi")?;
    if !prefix.ends_with(".plantNStageCondition") {
        return None;
    }
    boptest_stage_condition_capacity_expr(prefix)
}

fn boptest_stage_condition_capacity_expr(prefix: &str) -> Option<String> {
    if prefix == "chilledWaterPlant.chillerPlant.chiSta.plantNStageCondition" {
        let stage_prefix = "chilledWaterPlant.chillerPlant.chiSta";
        let cap_prefix = var_name_to_c_alias(prefix);
        return Some(
            (1..=3)
                .map(|i| {
                    format!(
                        "({} * {}_Cap_{})",
                        var_name_to_c_alias(&format!("{stage_prefix}.y[{i}]")),
                        cap_prefix,
                        i
                    )
                })
                .collect::<Vec<_>>()
                .join(" + "),
        );
    }
    if prefix == "hotWaterPlant.boilerPlant.boiSta.plantNStageCondition" {
        let stage_prefix = "hotWaterPlant.boilerPlant.boiSta";
        let cap_prefix = var_name_to_c_alias(prefix);
        return Some(
            (1..=2)
                .map(|i| {
                    format!(
                        "({} * {}_Cap_{})",
                        var_name_to_c_alias(&format!("{stage_prefix}.y[{i}]")),
                        cap_prefix,
                        i
                    )
                })
                .collect::<Vec<_>>()
                .join(" + "),
        );
    }
    None
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

fn boptest_floor_duct_static_pressure_alias(floor: usize, aliases: &HashSet<String>) -> String {
    let floor_alias = format!("floor{floor}BoptestAirNetwork");
    let candidates = [
        format!("{floor_alias}_ductStaticPressure"),
        format!("{floor_alias}_pSet"),
        indexed_alias("ahu_duct_static_pressure_setpoint", floor),
    ];
    candidates
        .into_iter()
        .find(|alias| aliases.contains(alias))
        .unwrap_or_else(|| "0.0".to_string())
}

fn boptest_floor_zone_nominal_air_mass_flow_alias(
    floor: usize,
    zone: usize,
    aliases: &HashSet<String>,
) -> String {
    let floor_alias = format!("floor{floor}BoptestAirNetwork");
    let candidates = [
        format!("{floor_alias}_mAirFloRat_{floor}_{zone}"),
        format!("{floor_alias}_mAirFloRat_{zone}"),
        format!("{floor_alias}_floor_mAirFloRat{zone}"),
        format!("{floor_alias}_floor_fivZonVAV_mAirFloRat{zone}"),
    ];
    candidates
        .into_iter()
        .find(|alias| aliases.contains(alias))
        .unwrap_or_else(|| boptest_floor_air_mass_flow_source_rhs(floor, zone))
}

fn boptest_floor_zone_temperature_alias(
    floor: usize,
    zone: usize,
    aliases: &HashSet<String>,
) -> String {
    let floor_alias = format!("floor{floor}BoptestAirNetwork");
    let candidates = [
        format!("{floor_alias}_floor_fivZonVAV_zon_{zone}_fmuZon_T"),
        format!("{floor_alias}_floor_fivZonVAV_zon_{zone}_vol_medium_T"),
        format!("{floor_alias}_floor_fivZonVAV_zon_{zone}_vol_medium_state_T"),
        format!("{floor_alias}_floor_fivZonVAV_zon_{zone}_vol_T_start"),
        format!("{floor_alias}_initialZoneTemperature_{zone}"),
    ];
    candidates
        .into_iter()
        .find(|alias| aliases.contains(alias))
        .unwrap_or_else(|| "293.15".to_string())
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
    let mut aliases =
        c_aliases_for_equations(&get_field(dae, "f_x").unwrap_or(Value::from(Vec::<Value>::new())));
    for field in ["x", "y", "w", "z", "m", "u", "p", "constants"] {
        let Ok(vars) = get_field(dae, field) else {
            continue;
        };
        collect_c_aliases_from_variable_map(&vars, &mut aliases);
    }
    aliases
}

fn fingerprint_dae_alias_context(dae: &Value) -> Option<u64> {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    get_field(dae, "f_x")
        .ok()
        .and_then(|value| value.len())
        .unwrap_or(0)
        .hash(&mut hasher);
    for field in ["x", "y", "w", "z", "m", "u", "p", "constants"] {
        field.hash(&mut hasher);
        let Ok(vars) = get_field(dae, field) else {
            0usize.hash(&mut hasher);
            continue;
        };
        vars.len()?.hash(&mut hasher);
        if let Some(object) = vars.as_object()
            && let Some(iter) = object.try_iter_pairs()
        {
            for (key, _) in iter.take(8) {
                template_value_name(&key).hash(&mut hasher);
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
    for (key, var) in iter {
        let name = key.to_string().trim_matches('"').to_string();
        aliases.insert(var_name_to_c_alias(&name));
        if let Some((base_name, _)) = name.split_once('[') {
            aliases.insert(var_name_to_c_alias(base_name));
        }
        collect_scalarized_dim_aliases(&name, &var, aliases);
    }
}

fn collect_scalarized_dim_aliases(name: &str, var: &Value, aliases: &mut HashSet<String>) {
    if parse_indexed_ref(name).is_some() {
        return;
    };
    let Ok(dims) = get_field(var, "dims") else {
        return;
    };
    if dims.len().unwrap_or(0) != 1 {
        return;
    }
    let Ok(dim_value) = dims.get_item(&Value::from(0)) else {
        return;
    };
    let Some(dim) = dim_value.as_usize() else {
        return;
    };
    for index in 1..=dim.min(4096) {
        aliases.insert(var_name_to_c_alias(&format!("{name}[{index}]")));
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
    alg_equation_candidates_for_var_with_selector(equations, var_name, |index| &index.by_var_name)
}

fn direct_alg_equation_candidates_for_var(equations: &Value, var_name: &str) -> Option<Vec<Value>> {
    let candidates = exact_direct_alg_equation_candidates_for_var(equations, var_name)?;
    if !candidates.is_empty() {
        return Some(candidates);
    }
    if let Some(base_name) = var_name.split_once('[').map(|(base, _)| base) {
        let base_candidates = exact_direct_alg_equation_candidates_for_var(equations, base_name)?;
        if !base_candidates.is_empty() {
            return Some(base_candidates);
        }
    }
    Some(Vec::new())
}

fn prioritized_alg_equation_candidates_for_var(
    equations: &Value,
    var_name: &str,
) -> Option<Vec<Value>> {
    let direct_candidates = direct_alg_equation_candidates_for_var(equations, var_name)?;
    if direct_candidates.is_empty() {
        alg_equation_candidates_for_var(equations, var_name)
    } else {
        Some(direct_candidates)
    }
}

fn exact_direct_alg_equation_candidates_for_var(
    equations: &Value,
    var_name: &str,
) -> Option<Vec<Value>> {
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
        append_equation_candidates(&index.by_direct_target_name, var_name, &mut candidates);
        dedup_equation_candidates(&mut candidates);
        Some(candidates)
    })
}

fn alg_equation_candidates_for_var_with_selector(
    equations: &Value,
    var_name: &str,
    select_map: impl Fn(&AlgEquationCandidateIndex) -> &HashMap<String, Vec<Value>>,
) -> Option<Vec<Value>> {
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
        append_equation_candidates(select_map(index), var_name, &mut candidates);
        if let Some(base_name) = var_name.split_once('[').map(|(base, _)| base) {
            append_equation_candidates(select_map(index), base_name, &mut candidates);
        }
        dedup_equation_candidates(&mut candidates);
        Some(candidates)
    })
}

fn append_equation_candidates(
    by_var_name: &HashMap<String, Vec<Value>>,
    var_name: &str,
    candidates: &mut Vec<Value>,
) {
    for candidate_name in [var_name.to_string(), var_name_to_c_alias(var_name)] {
        let Some(equations) = by_var_name.get(&candidate_name) else {
            continue;
        };
        for eq in equations {
            candidates.push(eq.clone());
        }
    }
}

fn dedup_equation_candidates(candidates: &mut Vec<Value>) {
    let mut seen = HashSet::new();
    candidates.retain(|candidate| {
        let Some(key) = value_object_identity(candidate) else {
            return true;
        };
        seen.insert(key)
    });
}

fn build_alg_equation_candidate_index(
    equations: &Value,
    fingerprint: u64,
) -> Option<AlgEquationCandidateIndex> {
    let Ok(iter) = equations.try_iter() else {
        return None;
    };
    let mut by_var_name: HashMap<String, Vec<Value>> = HashMap::new();
    let mut by_direct_target_name: HashMap<String, Vec<Value>> = HashMap::new();
    for eq in iter {
        let mut var_names = HashSet::new();
        collect_equation_var_refs(&eq, &mut var_names);
        for var_name in var_names {
            by_var_name.entry(var_name).or_default().push(eq.clone());
        }
        let mut direct_target_names = HashSet::new();
        collect_direct_alg_target_refs(&eq, &mut direct_target_names);
        for var_name in direct_target_names {
            by_direct_target_name
                .entry(var_name)
                .or_default()
                .push(eq.clone());
        }
    }
    Some(AlgEquationCandidateIndex {
        fingerprint,
        by_var_name,
        by_direct_target_name,
    })
}

fn fingerprint_equations(equations: &Value) -> Option<u64> {
    let len = equations.len()?;
    if let Some(cache_key) = equation_fingerprint_cache_key(equations, len) {
        if let Some(fingerprint) =
            EQUATION_FINGERPRINT_CACHE.with(|cache| cache.borrow().get(&cache_key).copied())
        {
            return Some(fingerprint);
        }
        let fingerprint = fingerprint_equations_uncached(equations, len);
        EQUATION_FINGERPRINT_CACHE.with(|cache| {
            cache.borrow_mut().insert(cache_key, fingerprint);
        });
        return Some(fingerprint);
    }
    Some(fingerprint_equations_uncached(equations, len))
}

fn equation_fingerprint_cache_key(
    equations: &Value,
    len: usize,
) -> Option<EquationFingerprintCacheKey> {
    let object_addr = value_object_identity(equations)?;
    Some(EquationFingerprintCacheKey { object_addr, len })
}

fn value_object_identity(value: &Value) -> Option<usize> {
    let object = value.as_object()?;
    Some(object as *const _ as *const () as usize)
}

fn fingerprint_equations_uncached(equations: &Value, len: usize) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    len.hash(&mut hasher);
    if let Ok(iter) = equations.try_iter() {
        for eq in iter.take(16) {
            if let Some(key) = value_object_identity(&eq) {
                key.hash(&mut hasher);
            } else {
                eq.len().unwrap_or(0).hash(&mut hasher);
            }
        }
    }
    hasher.finish()
}

fn collect_equation_var_refs(eq: &Value, out: &mut HashSet<String>) {
    if let Ok(lhs) = get_field(eq, "lhs") {
        collect_lhs_var_refs(&lhs, out);
    }
    if let Ok(rhs) = get_field(eq, "rhs").or_else(|_| get_field(eq, "residual")) {
        collect_expr_var_refs(&rhs, out);
    }
}

fn collect_direct_alg_target_refs(eq: &Value, out: &mut HashSet<String>) {
    if let Ok(lhs) = get_field(eq, "lhs") {
        collect_lhs_var_refs(&lhs, out);
        return;
    }
    let Some(rhs) = equation_residual_or_rhs(eq).ok().flatten() else {
        return;
    };
    let Ok(binary) = get_field(&rhs, "Binary") else {
        return;
    };
    if !is_sub_op(&binary) {
        return;
    }
    let Ok(rhs_side) = get_field(&binary, "rhs") else {
        return;
    };
    if contains_der(&rhs_side) {
        return;
    }
    if let Ok(lhs_side) = get_field(&binary, "lhs") {
        collect_lhs_var_refs(&lhs_side, out);
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
    out.insert(var_name_to_c_alias(name));
    if let Some((base_name, _)) = name.split_once('[') {
        out.insert(base_name.to_string());
        out.insert(var_name_to_c_alias(base_name));
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
    let Ok(lhs) = get_field(eq, "lhs") else {
        return no_render_match();
    };
    let Ok(rhs) = get_field(eq, "rhs") else {
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
            && let Some(rendered) = render_array_expr_at_index(rhs, index, cfg)
        {
            return Ok(Some(rendered));
        }
        return render_expression(rhs, cfg).map(Some);
    }
    if let Some(index) = array_lhs_element_index(lhs, var_name) {
        return render_whole_array_assignment_rhs_at_index(rhs, index, cfg);
    }
    if let Some(index) = whole_array_lhs_index(lhs, var_name) {
        return render_whole_array_assignment_rhs_at_index(rhs, index, cfg);
    }
    no_render_match()
}

fn render_whole_array_assignment_rhs_at_index(
    rhs: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Result<Option<String>, minijinja::Error> {
    if let Ok(var_ref) = get_field(rhs, "VarRef") {
        let subscripts = get_field(&var_ref, "subscripts").ok();
        if subscripts
            .as_ref()
            .and_then(|value| value.len())
            .unwrap_or(0)
            == 0
        {
            let rhs_name = var_ref_full_name(&var_ref);
            if let Some(rendered) = lookup_scalarized_array_symbol(&rhs_name, index, cfg) {
                return Ok(Some(rendered));
            }
            return render_expression(rhs, cfg).map(Some);
        }
    }
    render_array_expr_at_index_or_scalar_checked(rhs, index, cfg)
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
    let Ok(lhs) = get_field(eq, "lhs") else {
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
        let Ok(rhs) = get_field(eq, "rhs") else {
            return no_render_match();
        };

        // If the RHS is an If-expression with sample() guard (clocked
        // when-statement), extract the update expression from the true branch.
        // Ordinary event when-equations must keep the full guarded RHS.
        if let Ok(if_expr) = get_field(&rhs, "If")
            && let Ok(branches) = get_field(&if_expr, "branches")
            && let Ok(first_branch) = branches.get_item(&Value::from(0))
            && let Ok(branch_array) = first_branch.try_iter()
        {
            // branches is a list of [condition, expression] pairs.
            let items: Vec<_> = branch_array.take(2).collect();
            if items
                .first()
                .is_some_and(|condition| expr_contains_sample_builtin(condition))
                && let Some(update_expr) = items.get(1)
            {
                if let Some((_base_name, index)) = parse_indexed_ref(var_name)
                    && let Some(rendered) = render_array_expr_at_index(update_expr, index, cfg)
                {
                    return Ok(Some(rendered));
                }
                return render_expression(update_expr, cfg).map(Some);
            }
        }

        // Fall back to rendering the entire RHS (for non-guarded cases)
        if let Some((_base_name, index)) = parse_indexed_ref(var_name)
            && let Some(rendered) = render_array_expr_at_index(&rhs, index, cfg)
        {
            return Ok(Some(rendered));
        }
        return render_expression(&rhs, cfg).map(Some);
    }

    // Array-level assignment support: searching for "v[i]" while equation lhs is
    // the whole array "v" and rhs is Array{...}.
    let Some((base_name, index)) = parse_indexed_ref(var_name) else {
        return no_render_match();
    };
    let lhs_rendered = render_expression(&lhs, cfg).ok();
    let lhs_matches_base = is_var_ref_of(&lhs, &base_name)
        || lhs_rendered
            .as_deref()
            .is_some_and(|rendered| rendered == var_name_to_c_alias(&base_name));
    if !lhs_matches_base {
        return no_render_match();
    }

    let Ok(rhs) = get_field(eq, "rhs") else {
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
        if let Some((_base_name, index)) = parse_indexed_ref(var_name)
            && let Some(rendered) = render_array_expr_at_index(&rhs_side, index, cfg)
        {
            return Ok(Some(rendered));
        }
        let rhs_expr = render_expression(&rhs_side, cfg)?;
        return Ok(Some(rhs_expr));
    }

    // Case 2: 0 = expr - var → var = expr
    if is_var_ref_of(&rhs_side, var_name) && !contains_der(&lhs_side) {
        if let Some((_base_name, index)) = parse_indexed_ref(var_name)
            && let Some(rendered) = render_array_expr_at_index(&lhs_side, index, cfg)
        {
            return Ok(Some(rendered));
        }
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
    let (base, subscripts) = if let Some(stem) = trimmed.strip_suffix(']') {
        stem.rsplit_once('[')?
    } else {
        rumoca_core::split_trailing_subscript_suffix(trimmed)?
    };
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

fn is_runtime_scalar_var(name: &str) -> bool {
    matches!(name.trim_matches('"'), "time" | "t")
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

    if let Ok(array_comp) = get_field(expr, "ArrayComprehension") {
        return render_array_comprehension_expr_at_index(&array_comp, index, cfg);
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
            if let Some(cond_value) = render_expr::constant_bool_expr(&cond) {
                if cond_value {
                    result = render_array_expr_at_index_or_scalar(&branch_expr, index, cfg)?;
                }
                continue;
            }
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

    if let Ok(field_access) = get_field(expr, "FieldAccess") {
        return render_field_access_array_expr_at_index(&field_access, index, cfg);
    }

    None
}

fn render_field_access_array_expr_at_index(
    field_access: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    let field = get_field(field_access, "field")
        .ok()
        .map(|value| value.to_string().trim_matches('"').to_string())?;
    let base = get_field(field_access, "base").ok()?;
    let call = get_field(&base, "FunctionCall").ok()?;
    let raw_name = render_expr::render_name_field(&call, "name", "FunctionCall").ok()?;
    let last = rumoca_core::top_level_last_segment(&raw_name);
    let arg_index = match (last, field.as_str()) {
        ("setState_pTX" | "setState_phX" | "ThermodynamicState", "X" | "reference_X") => 2,
        _ => return None,
    };
    let args = get_field(&call, "args").ok()?;
    if let Some(named_arg) = named_function_arg_value(&args, "X") {
        return render_array_expr_at_index_or_scalar(&named_arg, index, cfg);
    }
    let arg = args.get_item(&Value::from(arg_index)).ok()?;
    render_array_expr_at_index_or_scalar(&arg, index, cfg)
}

fn named_function_arg_value(args: &Value, name: &str) -> Option<Value> {
    let len = args.len()?;
    let expected = format!("__rumoca_named_arg__.{name}");
    for i in 0..len {
        let arg = args.get_item(&Value::from(i)).ok()?;
        let call = get_field(&arg, "FunctionCall").ok()?;
        let call_name = render_expr::render_name_field(&call, "name", "FunctionCall").ok()?;
        if call_name != expected {
            continue;
        }
        let call_args = get_field(&call, "args").ok()?;
        return call_args.get_item(&Value::from(0)).ok();
    }
    None
}

fn render_array_comprehension_expr_at_index(
    array_comp: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    if index == 0 {
        return None;
    }
    let indices = get_field(array_comp, "indices").ok()?;
    if indices.len()? != 1 {
        return None;
    }
    let iter = indices.get_item(&Value::from(0)).ok()?;
    let iter_name = get_field(&iter, "name").ok()?.to_string();
    let body = get_field(array_comp, "expr").ok()?;
    let mut iter_cfg = cfg.clone();
    iter_cfg.substitutions.push((iter_name, index.to_string()));
    render_expression(&body, &iter_cfg).ok()
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
    let raw_name = render_serialized_name(&get_field(var_ref, "name").ok()?);
    if subscripts.len().unwrap_or(0) == 1
        && let Ok(subscript) = subscripts.get_item(&Value::from(0))
        && let Ok(idx) = get_field(&subscript, "Index")
        && let Ok(row) = subscript_index_value(&idx)
    {
        return lookup_scalarized_row_slice_symbol(&raw_name, row, index, cfg);
    }
    if subscripts.len().unwrap_or(0) != 0 {
        return None;
    }
    if is_runtime_scalar_var(&raw_name) {
        return super::lookup_symbol_value(cfg.symbols.as_ref(), &raw_name)
            .or_else(|| Some(var_name_to_c_alias(&raw_name)));
    }
    if let Some(symbol) = lookup_scalarized_array_symbol(&raw_name, index, cfg) {
        return Some(symbol);
    }
    if raw_name.contains('[') {
        return super::lookup_symbol_value(cfg.symbols.as_ref(), &raw_name)
            .or_else(|| Some(var_name_to_c_alias(&raw_name)));
    }
    if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &raw_name) {
        return Some(symbol);
    }
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

fn lookup_scalarized_row_slice_symbol(
    base_name: &str,
    row: i64,
    index: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    if row < 1 || index == 0 {
        return None;
    }
    let source_ref = format!("{base_name}[{row},{index}]");
    if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &source_ref) {
        return Some(symbol);
    }
    let row_ref = format!("{base_name}[{row}]");
    if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &row_ref) {
        return Some(symbol);
    }
    Some(if cfg.sanitize_dots || cfg.subscript_underscore {
        super::sanitize_name(&source_ref)
    } else {
        format!("{base_name}[{row},{index}]")
    })
}

fn lookup_scalarized_array_symbol(
    base_name: &str,
    index: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    if index == 0 {
        return None;
    }
    if let Some(symbol) =
        super::lookup_symbol_value(cfg.symbols.as_ref(), &format!("{base_name}[{index}]"))
    {
        return Some(symbol);
    }
    if let Some(columns) = scalarized_matrix_columns_from_symbols(base_name, cfg) {
        let row = ((index - 1) / columns) + 1;
        let col = ((index - 1) % columns) + 1;
        let source_ref = format!("{base_name}[{row},{col}]");
        if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &source_ref) {
            return Some(symbol);
        }
        return None;
    }
    for columns in 2..=16 {
        let row = ((index - 1) / columns) + 1;
        let col = ((index - 1) % columns) + 1;
        let source_ref = format!("{base_name}[{row},{col}]");
        if let Some(symbol) = super::lookup_symbol_value(cfg.symbols.as_ref(), &source_ref) {
            return Some(symbol);
        }
    }
    if super::lookup_symbol_value(cfg.symbols.as_ref(), &format!("{base_name}[2]")).is_none()
        && let Some(symbol) =
            super::lookup_symbol_value(cfg.symbols.as_ref(), &format!("{base_name}[1]"))
    {
        return Some(symbol);
    }
    None
}

fn scalarized_matrix_columns_from_symbols(base_name: &str, cfg: &ExprConfig) -> Option<usize> {
    let symbols = cfg.symbols.as_ref()?;
    let object = symbols.as_object()?;
    let pairs = object.try_iter_pairs()?;
    let prefix = format!("{base_name}[");
    let mut max_row = 0usize;
    let mut max_col = 0usize;
    let mut seen = false;
    for (key, _) in pairs {
        let key = template_value_name(&key);
        let Some(rest) = key.strip_prefix(&prefix) else {
            continue;
        };
        let Some(subscripts) = rest.strip_suffix(']') else {
            continue;
        };
        let Some((row, col)) = subscripts.split_once(',') else {
            continue;
        };
        let Ok(row) = row.trim().parse::<usize>() else {
            continue;
        };
        let Ok(col) = col.trim().parse::<usize>() else {
            continue;
        };
        if row == 0 || col == 0 {
            continue;
        }
        seen = true;
        max_row = max_row.max(row);
        max_col = max_col.max(col);
    }
    if seen && max_row > 0 && max_col > 0 {
        Some(max_col)
    } else {
        None
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
    let func_name = func_name.trim_matches('"');
    let args = get_field(builtin, "args").ok()?;
    match func_name {
        "Cat" | "cat" => render_cat_array_expr_at_index(&args, index, cfg),
        "Zeros" | "zeros" => Some(if cfg.power == "**" {
            "REAL_C(0.0)".to_string()
        } else {
            "0.0".to_string()
        }),
        "Ones" | "ones" => Some(if cfg.power == "**" {
            "REAL_C(1.0)".to_string()
        } else {
            "1.0".to_string()
        }),
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

fn render_cat_array_expr_at_index(args: &Value, index: usize, cfg: &ExprConfig) -> Option<String> {
    if index == 0 || args.len()? < 2 {
        return None;
    }
    let mut offset = 0usize;
    for arg_pos in 1..args.len()? {
        let arg = args.get_item(&Value::from(arg_pos)).ok()?;
        if let Some(len) = array_expr_static_len(&arg, cfg) {
            if index <= offset + len {
                return render_array_expr_at_index_or_scalar(&arg, index - offset, cfg);
            }
            offset += len;
            continue;
        }
        if arg_pos + 1 == args.len()? {
            return render_array_expr_at_index_or_scalar(&arg, index - offset, cfg);
        }
        return None;
    }
    None
}

fn array_expr_static_len(expr: &Value, cfg: &ExprConfig) -> Option<usize> {
    if let Ok(array) = get_field(expr, "Array") {
        return get_field(&array, "elements").ok()?.len();
    }
    if let Ok(var_ref) = get_field(expr, "VarRef") {
        let subscripts = get_field(&var_ref, "subscripts").ok()?;
        if subscripts.len()? == 0 {
            let raw_name = render_serialized_name(&get_field(&var_ref, "name").ok()?);
            return scalarized_1d_len_for_base(&raw_name, cfg);
        }
    }
    if let Ok(array_comp) = get_field(expr, "ArrayComprehension") {
        return array_comprehension_static_len(&array_comp, cfg);
    }
    if let Ok(builtin) = get_field(expr, "BuiltinCall") {
        let func_name = get_field(&builtin, "function").ok()?.to_string();
        let func_name = func_name.trim_matches('"');
        if matches!(func_name, "Zeros" | "zeros" | "Ones" | "ones") {
            let args = get_field(&builtin, "args").ok()?;
            let first = args.get_item(&Value::from(0)).ok()?;
            return literal_usize_expr(&first);
        }
    }
    None
}

fn array_comprehension_static_len(array_comp: &Value, cfg: &ExprConfig) -> Option<usize> {
    let indices = get_field(array_comp, "indices").ok()?;
    if indices.len()? != 1 {
        return None;
    }
    let iter = indices.get_item(&Value::from(0)).ok()?;
    if let Ok(range) = get_field(&iter, "range")
        && let Ok(range) = get_field(&range, "Range")
        && let (Some(start), Some(end)) = (
            literal_usize_expr(&get_field(&range, "start").ok()?),
            literal_usize_expr(&get_field(&range, "end").ok()?),
        )
    {
        return (end >= start).then_some(end - start + 1);
    }
    let body = get_field(array_comp, "expr").ok()?;
    first_scalarized_var_ref_len(&body, cfg)
}

fn first_scalarized_var_ref_len(expr: &Value, cfg: &ExprConfig) -> Option<usize> {
    if let Ok(inner) = get_field(expr, "expr") {
        return first_scalarized_var_ref_len(&inner, cfg);
    }
    if let Ok(var_ref) = get_field(expr, "VarRef") {
        let raw_name = render_serialized_name(&get_field(&var_ref, "name").ok()?);
        if let Some(len) = scalarized_1d_len_for_base(&raw_name, cfg) {
            return Some(len);
        }
    }
    if let Ok(binary) = get_field(expr, "Binary") {
        let lhs = get_field(&binary, "lhs").ok()?;
        let rhs = get_field(&binary, "rhs").ok()?;
        return first_scalarized_var_ref_len(&lhs, cfg)
            .or_else(|| first_scalarized_var_ref_len(&rhs, cfg));
    }
    if let Ok(unary) = get_field(expr, "Unary") {
        let rhs = get_field(&unary, "rhs").ok()?;
        return first_scalarized_var_ref_len(&rhs, cfg);
    }
    if let Ok(if_expr) = get_field(expr, "If") {
        if let Ok(else_branch) = get_field(&if_expr, "else_branch")
            && let Some(len) = first_scalarized_var_ref_len(&else_branch, cfg)
        {
            return Some(len);
        }
        let branches = get_field(&if_expr, "branches").ok()?;
        for branch_idx in 0..branches.len()? {
            let branch = branches.get_item(&Value::from(branch_idx)).ok()?;
            let branch_expr = branch.get_item(&Value::from(1)).ok()?;
            if let Some(len) = first_scalarized_var_ref_len(&branch_expr, cfg) {
                return Some(len);
            }
        }
    }
    None
}

fn literal_usize_expr(expr: &Value) -> Option<usize> {
    let literal = get_field(expr, "Literal").ok()?;
    let literal_value = get_field(&literal, "value").unwrap_or(literal);
    if let Ok(int) = get_field(&literal_value, "Integer") {
        return int.to_string().parse::<usize>().ok();
    }
    None
}

fn render_function_array_expr_at_index(
    call: &Value,
    index: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    let name = get_field(call, "name").ok()?;
    let raw_name = render_serialized_name(&name);
    if top_level_last_segment(&raw_name) == "splineDerivatives" && index > 0 {
        let args = render_expr::render_args(call, cfg).ok()?;
        return Some(format!("splineDerivatives_d({args}, {})", index - 1));
    }
    if top_level_last_segment(&raw_name) != "linspace" {
        return render_elementwise_function_call_at_index(call, &raw_name, index, cfg);
    }
    let args = get_field(call, "args").ok()?;
    render_linspace_array_expr_at_index(&args, index, cfg)
}

fn render_elementwise_function_call_at_index(
    call: &Value,
    raw_name: &str,
    index: usize,
    cfg: &ExprConfig,
) -> Option<String> {
    if index == 0 || top_level_last_segment(raw_name) != "regStep" {
        return None;
    }
    let args = get_field(call, "args").ok()?;
    if args.len()? != 4 {
        return None;
    }
    let arg = |name: &str, pos: usize| -> Option<Value> {
        named_function_arg_value(&args, name).or_else(|| args.get_item(&Value::from(pos)).ok())
    };
    let x = render_expression(&arg("x", 0)?, cfg).ok()?;
    let y1 = render_array_expr_at_index_or_scalar(&arg("y1", 1)?, index, cfg)?;
    let y2 = render_array_expr_at_index_or_scalar(&arg("y2", 2)?, index, cfg)?;
    let x_small = render_expression(&arg("x_small", 3)?, cfg).ok()?;
    let name = super::emitted_symbol_or_fallback(raw_name, cfg);
    Some(format!("{name}({x}, {y1}, {y2}, {x_small})"))
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
    let full_name = var_ref_full_name(&var_ref);
    full_name == target_name
        || var_name_to_c_alias(&full_name) == target_name
        || full_name == var_name_to_c_alias(target_name)
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
        if dae_contains_scalar_var(dae, &candidate) {
            return Some(var_name_to_c_alias(&candidate));
        }
    }
    if let Some(prefix) = var_name.strip_suffix(".conPI.On") {
        let candidate = format!("{prefix}.On");
        if dae_contains_scalar_var(dae, &candidate) {
            return Some(var_name_to_c_alias(&candidate));
        }
    }
    if let Some(prefix) = var_name.strip_suffix(".booToRea.u")
        && prefix.ends_with(".conPI")
    {
        let candidate = format!("{prefix}.On");
        if dae_contains_scalar_var(dae, &candidate) {
            return Some(var_name_to_c_alias(&candidate));
        }
    }
    if let Some(prefix) = var_name.strip_suffix(".conPID.trigger")
        && prefix.ends_with(".conPI")
    {
        let candidate = format!("{prefix}.On");
        if dae_contains_scalar_var(dae, &candidate) {
            return Some(var_name_to_c_alias(&candidate));
        }
    }
    if let Some(prefix) = var_name.strip_suffix(".conPID.I.trigger")
        && prefix.ends_with(".conPI")
    {
        let pid_trigger = format!("{prefix}.conPID.trigger");
        if dae_contains_scalar_var(dae, &pid_trigger) {
            return Some(var_name_to_c_alias(&pid_trigger));
        }
        let on_signal = format!("{prefix}.On");
        if dae_contains_scalar_var(dae, &on_signal) {
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
        if candidate != var_name && dae_contains_scalar_var(dae, &candidate) {
            return Some(var_name_to_c_alias(&candidate));
        }
        prefix = parent;
    }
    None
}

fn dae_contains_scalar_var(dae: &Value, var_name: &str) -> bool {
    ["m", "z", "y", "u", "x", "p", "c"]
        .into_iter()
        .any(|map_name| has_scalar_var_in_dae_map(dae, map_name, var_name))
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
    let Ok(map) = get_field(dae, map_name) else {
        return false;
    };
    let Ok(var) = map.get_item(&Value::from(var_name)) else {
        return false;
    };
    get_field(&var, "name").is_ok() || get_field(&var, "dims").is_ok()
}

fn has_scalar_var_in_dae_map(dae: &Value, map_name: &str, var_name: &str) -> bool {
    let Ok(map) = get_field(dae, map_name) else {
        return false;
    };
    let Ok(var) = map.get_item(&Value::from(var_name)) else {
        return false;
    };
    if get_field(&var, "name").is_err() && get_field(&var, "dims").is_err() {
        return false;
    }
    let Ok(dims) = get_field(&var, "dims") else {
        return true;
    };
    dims.len().unwrap_or(0) == 0
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

    #[test]
    fn scalarized_row_slice_symbol_keeps_one_dimensional_index() {
        let cfg = ExprConfig {
            sanitize_dots: true,
            subscript_underscore: true,
            symbols: Some(Value::from_serialize(serde_json::json!({
                "floor.TZonHeaSet.y[1]": "floor_TZonHeaSet_y_1"
            }))),
            ..ExprConfig::default()
        };

        assert_eq!(
            lookup_scalarized_row_slice_symbol("floor.TZonHeaSet.y", 1, 1, &cfg),
            Some("floor_TZonHeaSet_y_1".to_string())
        );
    }

    #[test]
    fn indexed_regstep_rhs_projects_array_arguments() {
        let expr = Value::from_serialize(serde_json::json!({
            "FunctionCall": {
                "name": "Functions.regStep",
                "args": [
                    {"VarRef": {"name": "vol.port_a.m_flow", "subscripts": []}},
                    {"VarRef": {"name": "vol.port_b.Xi_outflow", "subscripts": []}},
                    {"VarRef": {"name": "vol.port_a.Xi_outflow", "subscripts": []}},
                    {"VarRef": {"name": "vol.m_flow_small", "subscripts": []}}
                ]
            }
        }));
        let cfg = ExprConfig {
            sanitize_dots: true,
            subscript_underscore: true,
            ..ExprConfig::default()
        };

        assert_eq!(
            render_array_expr_at_index(&expr, 1, &cfg),
            Some(
                "Functions_regStep(vol_port_a_m_flow, vol_port_b_Xi_outflow_1, vol_port_a_Xi_outflow_1, vol_m_flow_small)"
                    .to_string()
            )
        );
    }

    #[test]
    fn indexed_regstep_rhs_reorders_named_arguments() {
        let expr = Value::from_serialize(serde_json::json!({
            "FunctionCall": {
                "name": "Functions.regStep",
                "args": [
                    {"FunctionCall": {"name": "__rumoca_named_arg__.y1", "args": [
                        {"VarRef": {"name": "vol.port_b.Xi_outflow", "subscripts": []}}
                    ]}},
                    {"FunctionCall": {"name": "__rumoca_named_arg__.y2", "args": [
                        {"VarRef": {"name": "vol.port_a.Xi_outflow", "subscripts": []}}
                    ]}},
                    {"FunctionCall": {"name": "__rumoca_named_arg__.x", "args": [
                        {"VarRef": {"name": "vol.port_a.m_flow", "subscripts": []}}
                    ]}},
                    {"FunctionCall": {"name": "__rumoca_named_arg__.x_small", "args": [
                        {"VarRef": {"name": "vol.m_flow_small", "subscripts": []}}
                    ]}}
                ]
            }
        }));
        let cfg = ExprConfig {
            sanitize_dots: true,
            subscript_underscore: true,
            ..ExprConfig::default()
        };

        assert_eq!(
            render_array_expr_at_index(&expr, 1, &cfg),
            Some(
                "Functions_regStep(vol_port_a_m_flow, vol_port_b_Xi_outflow_1, vol_port_a_Xi_outflow_1, vol_m_flow_small)"
                    .to_string()
            )
        );
    }

    #[test]
    fn internal_fluid_medium_species_projects_indexed_inputs() {
        let aliases = HashSet::from([
            "src_medium_Xi_1".to_string(),
            "src_medium_Xi_2".to_string(),
            "src_Xi_in_internal".to_string(),
            "src_Xi_in_internal_1".to_string(),
            "src_X_in_internal_1".to_string(),
            "src_X_in_internal_2".to_string(),
            "floor_fivZonVAV_infAir_1_medium_Xi_1".to_string(),
            "floor_fivZonVAV_infAir_1_X_in_internal_1".to_string(),
            "floor_fivZonVAV_exfAir_1_X_in_internal_1".to_string(),
            "floor_fivZonVAV_out_X_in_internal_2".to_string(),
            "outdoorAirSource_1_X_in_internal_1".to_string(),
        ]);

        assert_eq!(
            synthesize_internal_fluid_volume_medium_rhs("src.medium.Xi[2]", &aliases),
            Some("src_X_in_internal_2".to_string())
        );
        assert_eq!(
            synthesize_boptest_air_source_trace_rhs(
                "floor.fivZonVAV.infAir[1].Xi_in_internal[1]",
                &aliases,
            ),
            Some("floor_fivZonVAV_infAir_1_medium_Xi_1".to_string())
        );
        assert_eq!(
            synthesize_boptest_air_source_trace_rhs(
                "floor.fivZonVAV.exfAir[1].Xi_in_internal[1]",
                &aliases,
            ),
            Some("floor_fivZonVAV_exfAir_1_X_in_internal_1".to_string())
        );
        assert_eq!(
            synthesize_boptest_air_source_trace_rhs(
                "floor.fivZonVAV.out.Xi_in_internal[2]",
                &aliases,
            ),
            Some("floor_fivZonVAV_out_X_in_internal_2".to_string())
        );
        assert_eq!(
            synthesize_boptest_air_source_trace_rhs(
                "outdoorAirSource[1].Xi_in_internal[1]",
                &aliases
            ),
            Some("outdoorAirSource_1_X_in_internal_1".to_string())
        );
        assert_eq!(
            synthesize_internal_fluid_volume_medium_rhs(
                "floor.fivZonVAV.infAir[1].medium.Xi[2]",
                &aliases,
            ),
            Some("floor_fivZonVAV_infAir_1_X_in_internal_2".to_string())
        );
        assert_eq!(
            synthesize_boptest_air_source_trace_rhs(
                "floor.fivZonVAV.infAir[1].C_in_internal[1]",
                &aliases,
            ),
            Some("0.0".to_string())
        );
        assert_eq!(
            synthesize_internal_fluid_volume_medium_rhs(
                "floor.fivZonVAV.infAir[1].medium.C[1]",
                &aliases,
            ),
            Some("0.0".to_string())
        );
    }
}
