use std::collections::{HashMap, HashSet};

use rumoca_eval_flat::eval::{VarEnv, eval_array_values, eval_expr, set_array_entries};
use rumoca_ir_dae as dae;

use crate::runtime::assignment::{evaluate_direct_assignment_values, extract_direct_assignment};
use crate::runtime::timeout::{TimeoutBudget, TimeoutExceeded};

#[derive(Debug, Clone, thiserror::Error)]
/// Errors returned while evaluating parameter/default values for runtime setup.
pub enum ParameterValueError {
    #[error(transparent)]
    Timeout(#[from] TimeoutExceeded),

    #[error("{0}")]
    Invalid(String),
}

fn expand_values_to_size(raw: Vec<f64>, size: usize) -> Vec<f64> {
    if size == 0 {
        return Vec::new();
    }
    if raw.len() == size {
        return raw;
    }
    if raw.is_empty() {
        return vec![0.0; size];
    }
    if raw.len() == 1 {
        return vec![raw[0]; size];
    }
    let last = *raw.last().unwrap_or(&0.0);
    let mut out = Vec::with_capacity(size);
    for idx in 0..size {
        out.push(raw.get(idx).copied().unwrap_or(last));
    }
    out
}

fn eval_var_start_values(var: &dae::Variable, env: &VarEnv<f64>) -> Vec<f64> {
    let size = var.size();
    if size <= 1 {
        return vec![
            var.start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, env))
                .unwrap_or(0.0),
        ];
    }

    let Some(start) = var.start.as_ref() else {
        return vec![0.0; size];
    };

    let raw = eval_array_values::<f64>(start, env);
    expand_values_to_size(raw, size)
}

fn ensure_finite_value(
    name: &str,
    kind: &str,
    value: f64,
    strict_non_finite: bool,
) -> Result<(), ParameterValueError> {
    if !strict_non_finite {
        return Ok(());
    }
    if value.is_nan() {
        return Err(ParameterValueError::Invalid(format!(
            "non-finite {kind} value for '{name}': {value}"
        )));
    }
    Ok(())
}

fn ensure_finite_array_values(
    name: &str,
    kind: &str,
    values: &[f64],
    strict_non_finite: bool,
) -> Result<(), ParameterValueError> {
    if !strict_non_finite {
        return Ok(());
    }
    if let Some((idx, value)) = values.iter().enumerate().find(|(_, value)| value.is_nan()) {
        return Err(ParameterValueError::Invalid(format!(
            "non-finite {kind} value for '{name}[{}]': {}",
            idx + 1,
            value
        )));
    }
    Ok(())
}

#[derive(Debug, Clone)]
struct ParameterLayout {
    offset: usize,
    size: usize,
    dims: Vec<i64>,
}

fn build_parameter_layout(dae_model: &dae::Dae) -> HashMap<String, ParameterLayout> {
    let mut layout = HashMap::new();
    let mut offset = 0usize;
    for (name, var) in &dae_model.parameters {
        let size = var.size().max(1);
        layout.insert(
            name.as_str().to_string(),
            ParameterLayout {
                offset,
                size,
                dims: var.dims.clone(),
            },
        );
        offset += size;
    }
    layout
}

fn parse_parameter_subscript_target(target: &str) -> Option<(&str, Vec<&str>)> {
    let (base, raw) = target.split_once('[')?;
    let inside = raw.strip_suffix(']')?;
    let tokens: Vec<&str> = inside.split(',').map(str::trim).collect();
    if tokens.is_empty() {
        return None;
    }
    Some((base, tokens))
}

fn resolve_parameter_target_indices(
    target: &str,
    layout: &HashMap<String, ParameterLayout>,
) -> Option<Vec<usize>> {
    if let Some(info) = layout.get(target) {
        return Some((info.offset..info.offset + info.size).collect());
    }

    let (base, tokens) = parse_parameter_subscript_target(target)?;
    let info = layout.get(base)?;
    let all_one = tokens.iter().all(|token| token.is_empty() || *token == "1");

    if info.size == 1 && all_one {
        return Some(vec![info.offset]);
    }

    if info.dims.len() == 1 && tokens.len() == 1 {
        let scalar_index = tokens[0].parse::<usize>().ok()?;
        if (1..=info.size).contains(&scalar_index) {
            return Some(vec![info.offset + scalar_index - 1]);
        }
    }

    None
}

fn populate_parameter_values_into_env(dae_model: &dae::Dae, env: &mut VarEnv<f64>, params: &[f64]) {
    let mut pidx = 0usize;
    for (name, var) in &dae_model.parameters {
        let size = var.size().max(1);
        if size == 1 {
            env.set(name.as_str(), params.get(pidx).copied().unwrap_or(0.0));
            pidx += 1;
            continue;
        }

        let mut values = vec![0.0; size];
        for (slot, value) in values.iter_mut().enumerate() {
            *value = params.get(pidx + slot).copied().unwrap_or(0.0);
        }
        set_array_entries(env, name.as_str(), &var.dims, &values);
        pidx += size;
    }
}

fn expression_refs_available_in_env(expr: &dae::Expression, env: &VarEnv<f64>) -> bool {
    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.into_iter()
        .all(|name| env.vars.contains_key(name.as_str()))
}

fn assignment_from_initial_equation(eq: &dae::Equation) -> Option<(String, &dae::Expression)> {
    if let Some(lhs) = &eq.lhs {
        return Some((lhs.as_str().to_string(), &eq.rhs));
    }
    extract_direct_assignment(&eq.rhs)
}

fn write_direct_assignment_param(
    params: &mut [f64],
    values: &[f64],
    slot: usize,
    idx: usize,
    changed: &mut bool,
) {
    if idx >= params.len() {
        return;
    }
    let value = values.get(slot).copied().unwrap_or(0.0);
    if params[idx].to_bits() == value.to_bits() {
        return;
    }
    params[idx] = value;
    *changed = true;
}

fn apply_initial_parameter_initialization<F>(
    dae_model: &dae::Dae,
    env: &mut VarEnv<f64>,
    params: &mut [f64],
    mut check: F,
) -> Result<bool, ParameterValueError>
where
    F: FnMut() -> Result<(), ParameterValueError>,
{
    if dae_model.parameters.is_empty() || dae_model.initial_equations.is_empty() {
        return Ok(false);
    }

    let layout = build_parameter_layout(dae_model);
    if layout.is_empty() {
        return Ok(false);
    }
    populate_parameter_values_into_env(dae_model, env, params);

    let max_passes = dae_model.initial_equations.len().clamp(1, 32);
    let mut changed_any = false;

    for _ in 0..max_passes {
        check()?;
        let mut pass_changed = false;

        for eq in &dae_model.initial_equations {
            check()?;
            let Some((target, solution)) = assignment_from_initial_equation(eq) else {
                continue;
            };
            let Some(indices) = resolve_parameter_target_indices(&target, &layout) else {
                continue;
            };
            if indices.is_empty() || !expression_refs_available_in_env(solution, env) {
                continue;
            }

            let values = evaluate_direct_assignment_values(solution, env, indices.len());
            for (slot, idx) in indices.iter().copied().enumerate() {
                write_direct_assignment_param(params, &values, slot, idx, &mut pass_changed);
            }
        }

        if !pass_changed {
            break;
        }
        changed_any = true;
        populate_parameter_values_into_env(dae_model, env, params);
    }

    check()?;
    Ok(changed_any)
}

fn write_param_value(params: &mut [f64], idx: usize, value: f64, changed: &mut bool) {
    *changed |= params[idx].to_bits() != value.to_bits();
    params[idx] = value;
}

fn reevaluate_parameters_once(
    dae_model: &dae::Dae,
    env: &mut VarEnv<f64>,
    params: &mut [f64],
    check: &mut dyn FnMut() -> Result<(), ParameterValueError>,
) -> Result<bool, ParameterValueError> {
    let mut changed = false;
    let mut pidx = 0usize;
    for (name, var) in &dae_model.parameters {
        check()?;
        let size = var.size();
        if size <= 1 {
            let value = var
                .start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, env))
                .unwrap_or(0.0);
            env.set(name.as_str(), value);
            write_param_value(params, pidx, value, &mut changed);
            pidx += 1;
            continue;
        }

        let element_vals = eval_var_start_values(var, env);
        set_array_entries(env, name.as_str(), &var.dims, &element_vals);
        for value in element_vals {
            write_param_value(params, pidx, value, &mut changed);
            pidx += 1;
        }
    }
    Ok(changed)
}

fn ensure_finite_constant_pass(
    name: &str,
    value: f64,
    pass_idx: usize,
    strict_non_finite: bool,
) -> Result<(), ParameterValueError> {
    if pass_idx != 1 {
        return Ok(());
    }
    ensure_finite_value(name, "constant", value, strict_non_finite)
}

fn default_parameter_values_with_checker<F>(
    dae_model: &dae::Dae,
    mut check: F,
    strict_non_finite: bool,
) -> Result<Vec<f64>, ParameterValueError>
where
    F: FnMut() -> Result<(), ParameterValueError>,
{
    check()?;

    let mut env = VarEnv::<f64>::new();

    if !dae_model.functions.is_empty() {
        let func_map = dae_model
            .functions
            .iter()
            .map(|(name, func)| (name.as_str().to_string(), func.clone()))
            .collect();
        env.functions = std::sync::Arc::new(func_map);
    }

    env.dims = std::sync::Arc::new(rumoca_eval_flat::eval::collect_var_dims(dae_model));
    env.start_exprs = std::sync::Arc::new(rumoca_eval_flat::eval::collect_var_starts(dae_model));
    env.enum_literal_ordinals = std::sync::Arc::new(dae_model.enum_literal_ordinals.clone());

    for &(fqn, value) in rumoca_eval_flat::eval::MODELICA_CONSTANTS {
        env.set(fqn, value);
    }

    for pass_idx in 0..2 {
        check()?;
        for (name, var) in &dae_model.constants {
            check()?;
            let Some(start) = var.start.as_ref() else {
                continue;
            };
            let size = var.size();
            if size <= 1 {
                let value = eval_expr::<f64>(start, &env);
                ensure_finite_constant_pass(name.as_str(), value, pass_idx, strict_non_finite)?;
                env.set(name.as_str(), value);
                continue;
            }

            let raw_values = eval_array_values::<f64>(start, &env);
            let values = expand_values_to_size(raw_values, size);
            if pass_idx == 1 {
                ensure_finite_array_values(name.as_str(), "constant", &values, strict_non_finite)?;
            }
            set_array_entries(&mut env, name.as_str(), &var.dims, &values);
        }
    }

    let mut params = Vec::new();
    for (name, var) in &dae_model.parameters {
        check()?;
        let size = var.size();
        if size <= 1 {
            let value = var
                .start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, &env))
                .unwrap_or(0.0);
            env.set(name.as_str(), value);
            params.push(value);
        } else {
            let element_vals = eval_var_start_values(var, &env);
            set_array_entries(&mut env, name.as_str(), &var.dims, &element_vals);
            params.extend(element_vals);
        }
    }

    let max_recheck_passes = dae_model.parameters.len().clamp(1, 32);
    for _ in 0..max_recheck_passes {
        check()?;
        let changed = reevaluate_parameters_once(dae_model, &mut env, &mut params, &mut check)?;
        if !changed {
            break;
        }
    }

    let _ = apply_initial_parameter_initialization(dae_model, &mut env, &mut params, &mut check)?;

    let mut pidx = 0usize;
    for (name, var) in &dae_model.parameters {
        check()?;
        let size = var.size();
        if size <= 1 {
            ensure_finite_value(name.as_str(), "parameter", params[pidx], strict_non_finite)?;
            pidx += 1;
        } else {
            let next = pidx + size;
            ensure_finite_array_values(
                name.as_str(),
                "parameter",
                &params[pidx..next],
                strict_non_finite,
            )?;
            pidx = next;
        }
    }

    check()?;
    Ok(params)
}

/// Evaluate default parameter values without timeout checks.
///
/// This is intended for deterministic setup code paths that should not time out.
pub fn default_parameter_values(dae_model: &dae::Dae) -> Vec<f64> {
    default_parameter_values_with_checker(dae_model, || Ok(()), false)
        .expect("default parameter evaluation without timeout checks should be infallible")
}

/// Evaluate default parameter values with timeout enforcement and strict NaN checks.
pub fn default_parameter_values_with_budget(
    dae_model: &dae::Dae,
    budget: &TimeoutBudget,
) -> Result<Vec<f64>, ParameterValueError> {
    default_parameter_values_with_checker(
        dae_model,
        || budget.check().map_err(ParameterValueError::from),
        true,
    )
}
