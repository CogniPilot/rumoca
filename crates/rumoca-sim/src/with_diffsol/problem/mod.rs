use crate::SimError;
use diffsol::{
    FaerSparseMat, MatrixCommon, OdeBuilder, OdeEquationsImplicit, OdeSolverProblem, Vector,
    VectorHost,
};
use rumoca_ir_dae as dae;
use std::collections::{HashMap, HashSet};

type BuiltinFunction = dae::BuiltinFunction;
type Dae = dae::Dae;
type Equation = dae::Equation;
type Expression = dae::Expression;
#[cfg(test)]
type OpBinary = rumoca_ir_core::OpBinary;
type VarName = dae::VarName;
#[cfg(test)]
type Variable = dae::Variable;

use crate::runtime::assignment::evaluate_direct_assignment_values;
use crate::sparsity::SparsityValidation;
use crate::sparsity::{
    greedy_column_coloring, structural_column_sparsity, validate_solver_sparsity,
};
use rumoca_eval_dae::runtime::dual::Dual;
use rumoca_eval_dae::runtime::sim_float::SimFloat;
use rumoca_eval_dae::runtime::{
    VarEnv, build_env, eval_array_values, eval_expr, lift_env, map_var_to_env, set_array_entries,
};
type M = FaerSparseMat<f64>;
type V = <M as MatrixCommon>::V;
type T = <M as MatrixCommon>::T;
type C = <M as MatrixCommon>::C;

fn component_base_name(name: &str) -> Option<String> {
    dae::component_base_name(name)
}

fn sim_trace_enabled() -> bool {
    std::env::var("RUMOCA_SIM_TRACE").is_ok() || std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()
}

fn sim_introspect_enabled() -> bool {
    std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()
}

fn apply_dae_sign<S: SimFloat>(val: S, i: usize, n_x: usize) -> S {
    if i < n_x { -val } else { val }
}

pub(crate) fn count_states(dae: &dae::Dae) -> usize {
    dae.states.values().map(|v| v.size()).sum()
}

#[cfg(test)]
fn count_algebraics(dae: &dae::Dae) -> usize {
    dae.algebraics.values().map(|v| v.size()).sum()
}

#[cfg(test)]
fn count_parameters(dae: &dae::Dae) -> usize {
    dae.parameters.values().map(|v| v.size()).sum()
}

fn expand_values_to_size(raw: Vec<f64>, sz: usize) -> Vec<f64> {
    if sz == 0 {
        return Vec::new();
    }
    if raw.len() == sz {
        return raw;
    }
    if raw.is_empty() {
        return vec![0.0; sz];
    }
    if raw.len() == 1 {
        return vec![raw[0]; sz];
    }
    let last = *raw.last().unwrap_or(&0.0);
    let mut out = Vec::with_capacity(sz);
    for i in 0..sz {
        out.push(raw.get(i).copied().unwrap_or(last));
    }
    out
}

fn eval_var_start_values(var: &dae::Variable, env: &VarEnv<f64>) -> Vec<f64> {
    let sz = var.size();
    if sz <= 1 {
        return vec![
            var.start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, env))
                .unwrap_or(0.0),
        ];
    }

    let Some(start) = var.start.as_ref() else {
        return vec![0.0; sz];
    };

    let raw = eval_array_values::<f64>(start, env);
    expand_values_to_size(raw, sz)
}

fn scalar_subscript_string(sub: &dae::Subscript) -> Option<String> {
    match sub {
        dae::Subscript::Index(i) => Some(i.to_string()),
        dae::Subscript::Expr(expr) => match expr.as_ref() {
            dae::Expression::Literal(dae::Literal::Integer(i)) => Some(i.to_string()),
            dae::Expression::Literal(dae::Literal::Real(v))
                if v.is_finite() && v.fract() == 0.0 =>
            {
                Some((*v as i64).to_string())
            }
            _ => None,
        },
        _ => None,
    }
}

fn append_subscripts(base: String, subscripts: &[dae::Subscript]) -> Option<String> {
    if subscripts.is_empty() {
        return Some(base);
    }
    let mut idx = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        idx.push(scalar_subscript_string(sub)?);
    }
    Some(format!("{base}[{}]", idx.join(",")))
}

fn expr_exact_name(expr: &dae::Expression) -> Option<String> {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            append_subscripts(name.as_str().to_string(), subscripts)
        }
        dae::Expression::Index { base, subscripts } => {
            let base_name = expr_exact_name(base)?;
            append_subscripts(base_name, subscripts)
        }
        dae::Expression::FieldAccess { base, field } => {
            let base_name = expr_exact_name(base)?;
            Some(format!("{base_name}.{field}"))
        }
        _ => None,
    }
}

fn expr_base_name(expr: &dae::Expression) -> Option<String> {
    match expr {
        dae::Expression::VarRef { name, .. } => dae::component_base_name(name.as_str()),
        dae::Expression::Index { base, .. } => expr_base_name(base),
        dae::Expression::FieldAccess { base, field } => {
            let base_name = expr_base_name(base)?;
            Some(format!("{base_name}.{field}"))
        }
        _ => None,
    }
}

pub(crate) fn expr_refers_to_var(expr: &dae::Expression, var_name: &dae::VarName) -> bool {
    if let Some(expr_exact) = expr_exact_name(expr)
        && expr_exact == var_name.as_str()
    {
        return true;
    }

    if var_name.as_str().contains('[') {
        return false;
    }

    let Some(expr_base) = expr_base_name(expr) else {
        return false;
    };
    let Some(var_base) = dae::component_base_name(var_name.as_str()) else {
        return false;
    };
    expr_base == var_base
}

pub(crate) fn expr_contains_der_of(expr: &dae::Expression, var_name: &dae::VarName) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            if *function == dae::BuiltinFunction::Der
                && args
                    .first()
                    .is_some_and(|a| expr_refers_to_var(a, var_name))
            {
                return true;
            }
            args.iter().any(|a| expr_contains_der_of(a, var_name))
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_der_of(lhs, var_name) || expr_contains_der_of(rhs, var_name)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_der_of(rhs, var_name),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(c, e)| {
                expr_contains_der_of(c, var_name) || expr_contains_der_of(e, var_name)
            }) || expr_contains_der_of(else_branch, var_name)
        }
        dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(|a| expr_contains_der_of(a, var_name))
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(|e| expr_contains_der_of(e, var_name))
        }
        dae::Expression::FieldAccess { base, .. } => expr_contains_der_of(base, var_name),
        dae::Expression::Index { base, .. } => expr_contains_der_of(base, var_name),
        _ => false,
    }
}

fn try_match_state_to_row(
    state_idx: usize,
    state_to_rows: &[Vec<usize>],
    row_to_state: &mut [Option<usize>],
    seen_rows: &mut [bool],
) -> bool {
    for &row_idx in &state_to_rows[state_idx] {
        if seen_rows[row_idx] {
            continue;
        }
        seen_rows[row_idx] = true;
        if let Some(other_state_idx) = row_to_state[row_idx] {
            if try_match_state_to_row(other_state_idx, state_to_rows, row_to_state, seen_rows) {
                row_to_state[row_idx] = Some(state_idx);
                return true;
            }
            continue;
        }
        row_to_state[row_idx] = Some(state_idx);
        return true;
    }
    false
}

fn match_primary_derivative_rows(
    state_to_rows: &[Vec<usize>],
    n_rows: usize,
) -> Vec<Option<usize>> {
    let mut state_order: Vec<usize> = (0..state_to_rows.len()).collect();
    state_order.sort_by_key(|idx| state_to_rows[*idx].len());

    let mut row_to_state: Vec<Option<usize>> = vec![None; n_rows];
    for state_idx in state_order {
        if state_to_rows[state_idx].is_empty() {
            continue;
        }
        let mut seen_rows = vec![false; n_rows];
        let _ = try_match_state_to_row(state_idx, state_to_rows, &mut row_to_state, &mut seen_rows);
    }

    let mut state_to_row = vec![None; state_to_rows.len()];
    for (row_idx, state_idx) in row_to_state.into_iter().enumerate() {
        if let Some(state_idx) = state_idx {
            state_to_row[state_idx] = Some(row_idx);
        }
    }
    state_to_row
}

fn trim_state_sizes_to_available_derivative_rows(dae: &mut dae::Dae) -> usize {
    let state_names: Vec<dae::VarName> = dae.states.keys().cloned().collect();
    let mut trimmed_scalars = 0usize;

    for state_name in state_names {
        let Some(current_var) = dae.states.get(&state_name) else {
            continue;
        };
        let current_size = current_var.size();
        if current_size <= 1 {
            continue;
        }

        let derivative_rows = dae
            .f_x
            .iter()
            .filter(|eq| expr_contains_der_of(&eq.rhs, &state_name))
            .count()
            .min(current_size);
        if derivative_rows == current_size {
            continue;
        }

        if derivative_rows == 0 {
            if let Some(var) = dae.states.shift_remove(&state_name) {
                trimmed_scalars += current_size;
                dae.algebraics.insert(state_name, var);
            }
            continue;
        }

        let Some(var) = dae.states.get_mut(&state_name) else {
            continue;
        };
        if var.dims.len() == 1 {
            var.dims[0] = derivative_rows as i64;
            trimmed_scalars += current_size - derivative_rows;
        }
    }

    trimmed_scalars
}

pub fn reorder_equations_for_solver(dae: &mut dae::Dae) -> Result<(), crate::SimError> {
    let mut n_x = count_states(dae);
    let n_eq = dae.f_x.len();

    if n_eq < n_x && trim_state_sizes_to_available_derivative_rows(dae) > 0 {
        n_x = count_states(dae);
    }
    if n_eq < n_x {
        return Err(crate::SimError::EquationMismatch {
            n_equations: n_eq,
            n_states: n_x,
            n_algebraics: n_eq.saturating_sub(n_x),
        });
    }

    let state_entries: Vec<(dae::VarName, usize)> = dae
        .states
        .iter()
        .map(|(state_name, state_var)| (state_name.clone(), state_var.size()))
        .collect();
    let state_to_rows: Vec<Vec<usize>> = state_entries
        .iter()
        .map(|(state_name, _)| {
            dae.f_x
                .iter()
                .enumerate()
                .filter_map(|(idx, eq)| expr_contains_der_of(&eq.rhs, state_name).then_some(idx))
                .collect()
        })
        .collect();
    let primary_rows = match_primary_derivative_rows(&state_to_rows, n_eq);

    let mut used = vec![false; n_eq];
    let mut ordered: Vec<dae::Equation> = Vec::with_capacity(n_eq);

    for (state_idx, (state_name, n_scalars)) in state_entries.iter().enumerate() {
        let n_scalars = *n_scalars;
        let candidates = &state_to_rows[state_idx];
        let mut selected = Vec::new();

        if let Some(primary) = primary_rows[state_idx] {
            selected.push(primary);
            used[primary] = true;
        }

        if selected.is_empty() {
            if sim_introspect_enabled() {
                eprintln!(
                    "[sim-introspect] reorder state={} size={} der_rows_all={:?} selected=[]",
                    state_name, n_scalars, candidates
                );
            }
            return Err(crate::SimError::MissingStateEquation(
                state_name.as_str().to_string(),
            ));
        }

        for &row_idx in candidates {
            if selected.len() >= n_scalars {
                break;
            }
            if used[row_idx] {
                continue;
            }
            selected.push(row_idx);
            used[row_idx] = true;
        }

        if sim_introspect_enabled() {
            eprintln!(
                "[sim-introspect] reorder state={} size={} der_rows_all={:?} selected={:?}",
                state_name, n_scalars, candidates, selected
            );
        }

        ordered.extend(selected.iter().map(|idx| dae.f_x[*idx].clone()));
    }

    for (i, eq) in dae.f_x.iter().enumerate() {
        if !used[i] {
            ordered.push(eq.clone());
        }
    }

    dae.f_x = ordered;
    Ok(())
}

fn ensure_finite_value(
    name: &str,
    kind: &str,
    value: f64,
    strict_non_finite: bool,
) -> Result<(), crate::SimError> {
    if !strict_non_finite {
        return Ok(());
    }
    if value.is_nan() {
        return Err(crate::SimError::SolverError(format!(
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
) -> Result<(), crate::SimError> {
    if !strict_non_finite {
        return Ok(());
    }
    if let Some((idx, value)) = values.iter().enumerate().find(|(_, value)| value.is_nan()) {
        return Err(crate::SimError::SolverError(format!(
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

fn build_parameter_layout(dae: &dae::Dae) -> HashMap<String, ParameterLayout> {
    let mut layout = HashMap::new();
    let mut offset = 0usize;
    for (name, var) in &dae.parameters {
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

fn populate_parameter_values_into_env(dae: &dae::Dae, env: &mut VarEnv<f64>, params: &[f64]) {
    let mut pidx = 0usize;
    for (name, var) in &dae.parameters {
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

fn expression_refs_available_in_env<T: SimFloat>(expr: &dae::Expression, env: &VarEnv<T>) -> bool {
    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.into_iter()
        .all(|name| env.vars.contains_key(name.as_str()))
}

fn should_skip_missing_direct_assignment_target(
    dae: &dae::Dae,
    target: &str,
    solution: &dae::Expression,
    stats: &HashMap<String, crate::runtime::assignment::DirectAssignmentTargetStats>,
    env: &VarEnv<impl SimFloat>,
) -> bool {
    if env.vars.contains_key(target) {
        return true;
    }

    let target_stats = stats.get(target).copied().unwrap_or_default();
    if target_stats.total > 1 && target_stats.non_alias != 1 {
        return true;
    }
    target_stats.total > 1
        && target_stats.non_alias == 1
        && crate::runtime::assignment::assignment_solution_is_alias_varref(dae, solution)
}

pub(crate) fn populate_missing_direct_assignment_targets_in_env<T: SimFloat>(
    dae: &dae::Dae,
    env: &mut VarEnv<T>,
    n_x: usize,
) {
    if dae.f_x.len() <= n_x {
        return;
    }

    let stats = crate::runtime::assignment::collect_direct_assignment_target_stats(dae, n_x, false);
    let max_passes = dae.f_x.len().max(4);
    for _ in 0..max_passes {
        let mut changed = false;
        for eq in dae.f_x.iter().skip(n_x) {
            if eq.origin == "orphaned_variable_pin" {
                continue;
            }
            let Some((target, solution)) =
                crate::runtime::assignment::direct_assignment_from_equation(eq)
            else {
                continue;
            };
            if should_skip_missing_direct_assignment_target(
                dae,
                target.as_str(),
                solution,
                &stats,
                env,
            ) || !expression_refs_available_in_env(solution, env)
            {
                continue;
            }

            let value = eval_expr::<T>(solution, env);
            env.set(target.as_str(), value);
            changed = true;
        }
        if !changed {
            break;
        }
    }
}

fn assignment_from_initial_equation(eq: &dae::Equation) -> Option<(String, &dae::Expression)> {
    if let Some(lhs) = &eq.lhs {
        return Some((lhs.as_str().to_string(), &eq.rhs));
    }
    extract_direct_assignment(&eq.rhs)
}

fn apply_initial_parameter_initialization<F>(
    dae: &dae::Dae,
    env: &mut VarEnv<f64>,
    params: &mut [f64],
    mut check: F,
) -> Result<bool, crate::SimError>
where
    F: FnMut() -> Result<(), crate::SimError>,
{
    if dae.parameters.is_empty() || dae.initial_equations.is_empty() {
        return Ok(false);
    }

    let layout = build_parameter_layout(dae);
    if layout.is_empty() {
        return Ok(false);
    }
    populate_parameter_values_into_env(dae, env, params);

    let max_passes = dae.initial_equations.len().clamp(1, 32);
    let mut changed_any = false;

    for _ in 0..max_passes {
        check()?;
        let mut pass_changed = false;

        for eq in &dae.initial_equations {
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
        populate_parameter_values_into_env(dae, env, params);
    }

    check()?;
    Ok(changed_any)
}

fn write_param_value(params: &mut [f64], idx: usize, value: f64, changed: &mut bool) {
    *changed |= params[idx].to_bits() != value.to_bits();
    params[idx] = value;
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
    let value = clamp_finite(values.get(slot).copied().unwrap_or(0.0));
    if params[idx].to_bits() == value.to_bits() {
        return;
    }
    params[idx] = value;
    *changed = true;
}

fn reevaluate_parameters_once(
    dae: &dae::Dae,
    env: &mut VarEnv<f64>,
    params: &mut [f64],
    check: &mut dyn FnMut() -> Result<(), crate::SimError>,
) -> Result<bool, crate::SimError> {
    let mut changed = false;
    let mut pidx = 0usize;
    for (name, var) in &dae.parameters {
        check()?;
        let sz = var.size();
        if sz <= 1 {
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

fn default_params_with_checker<F>(
    dae: &dae::Dae,
    mut check: F,
    strict_non_finite: bool,
) -> Result<Vec<f64>, crate::SimError>
where
    F: FnMut() -> Result<(), crate::SimError>,
{
    check()?;

    let mut env = VarEnv::<f64>::new();

    if !dae.functions.is_empty() {
        env.functions = std::sync::Arc::new(rumoca_eval_dae::runtime::collect_user_functions(dae));
    }

    env.dims = std::sync::Arc::new(rumoca_eval_dae::runtime::collect_var_dims(dae));
    env.start_exprs = std::sync::Arc::new(rumoca_eval_dae::runtime::collect_var_starts(dae));
    env.enum_literal_ordinals = std::sync::Arc::new(dae.enum_literal_ordinals.clone());

    for &(fqn, value) in rumoca_eval_dae::runtime::MODELICA_CONSTANTS {
        env.set(fqn, value);
    }

    for pass_idx in 0..2 {
        check()?;
        for (name, var) in &dae.constants {
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
    for (name, var) in &dae.parameters {
        check()?;
        let sz = var.size();
        if sz <= 1 {
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

    let max_recheck_passes = dae.parameters.len().clamp(1, 32);
    for _ in 0..max_recheck_passes {
        check()?;
        let changed = reevaluate_parameters_once(dae, &mut env, &mut params, &mut check)?;
        if !changed {
            break;
        }
    }

    let _ = apply_initial_parameter_initialization(dae, &mut env, &mut params, &mut check)?;

    let mut pidx = 0usize;
    for (name, var) in &dae.parameters {
        check()?;
        let sz = var.size();
        if sz <= 1 {
            ensure_finite_value(name.as_str(), "parameter", params[pidx], strict_non_finite)?;
            pidx += 1;
        } else {
            let next = pidx + sz;
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

fn ensure_finite_constant_pass(
    name: &str,
    value: f64,
    pass_idx: usize,
    strict_non_finite: bool,
) -> Result<(), crate::SimError> {
    if pass_idx != 1 {
        return Ok(());
    }
    ensure_finite_value(name, "constant", value, strict_non_finite)
}

pub fn default_params(dae: &dae::Dae) -> Vec<f64> {
    default_params_with_checker(dae, || Ok(()), false)
        .expect("default parameter evaluation without timeout checks should be infallible")
}

pub(crate) fn default_params_with_budget(
    dae: &dae::Dae,
    budget: &crate::TimeoutBudget,
) -> Result<Vec<f64>, crate::SimError> {
    default_params_with_checker(dae, || budget.check().map_err(crate::SimError::from), true)
}

use std::sync::atomic::{AtomicUsize, Ordering};

static RHS_SIGNAL_DEBUG_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn eval_rhs_generic<S: SimFloat>(dae: &dae::Dae, env: &VarEnv<S>, n_x: usize, out: &mut [S]) {
    let mut env = env.clone();
    populate_missing_direct_assignment_targets_in_env(dae, &mut env, n_x);
    if std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()
        && env.vars.contains_key("vIn.signalSource.T_start")
        && env.vars.contains_key("vIn.signalSource.count")
    {
        let hit = RHS_SIGNAL_DEBUG_COUNTER.fetch_add(1, Ordering::Relaxed);
        if hit < 24 {
            eprintln!(
                "[sim-introspect] rhs-env hit={} t={} T_start={} count={} T_rising={} T_width={} T_falling={} startTime={} offset={} amplitude={} vIn.p.v={} vIn.n.v={}",
                hit,
                env.get("time").real(),
                env.get("vIn.signalSource.T_start").real(),
                env.get("vIn.signalSource.count").real(),
                env.get("vIn.signalSource.T_rising").real(),
                env.get("vIn.signalSource.T_width").real(),
                env.get("vIn.signalSource.T_falling").real(),
                env.get("vIn.signalSource.startTime").real(),
                env.get("vIn.signalSource.offset").real(),
                env.get("vIn.signalSource.amplitude").real(),
                env.get("vIn.p.v").real(),
                env.get("vIn.n.v").real(),
            );
        }
    }
    for (i, eq) in dae.f_x.iter().enumerate() {
        if i < out.len() {
            let val = eval_expr::<S>(&eq.rhs, &env);
            out[i] = apply_dae_sign(val, i, n_x);
        }
    }
}

pub(crate) fn eval_rhs_equations(
    dae: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    out: &mut [f64],
    n_x: usize,
) {
    let env = build_env(dae, y, p, t);
    eval_rhs_generic(dae, &env, n_x, out);
}

pub(crate) fn eval_rhs_equations_initial(
    dae: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    out: &mut [f64],
    n_x: usize,
) {
    let mut env = build_env(dae, y, p, t);
    env.is_initial = true;
    eval_rhs_generic(dae, &env, n_x, out);
}

fn seed_state_duals(env: &mut VarEnv<Dual>, dae: &dae::Dae, v: &[f64]) {
    let mut seed_env = VarEnv::<f64>::new();
    let mut idx = 0usize;
    for (name, var) in dae
        .states
        .iter()
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
    {
        map_var_to_env(&mut seed_env, name.as_str(), var, v, &mut idx);
    }

    for (key, du) in seed_env.vars {
        if let Some(entry) = env.vars.get_mut(&key) {
            entry.du = du;
        }
    }
}

pub(crate) fn eval_jacobian_vector_ad(
    dae: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    v: &[f64],
    out: &mut [f64],
    n_x: usize,
) {
    let env_f64 = build_env(dae, y, p, t);
    let mut env_dual: VarEnv<Dual> = lift_env(&env_f64);
    seed_state_duals(&mut env_dual, dae, v);
    let mut dual_out = vec![Dual::default(); out.len()];
    eval_rhs_generic(dae, &env_dual, n_x, &mut dual_out);
    for (i, d) in dual_out.iter().enumerate() {
        out[i] = d.du;
    }
}

pub(crate) fn eval_jacobian_vector_ad_initial(
    dae: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    v: &[f64],
    out: &mut [f64],
    n_x: usize,
) {
    let mut env_f64 = build_env(dae, y, p, t);
    env_f64.is_initial = true;
    let mut env_dual: VarEnv<Dual> = lift_env(&env_f64);
    seed_state_duals(&mut env_dual, dae, v);
    let mut dual_out = vec![Dual::default(); out.len()];
    eval_rhs_generic(dae, &env_dual, n_x, &mut dual_out);
    for (i, d) in dual_out.iter().enumerate() {
        out[i] = d.du;
    }
}

mod core;
pub(crate) use core::*;

mod init;
pub(crate) use init::{
    initial_free_residual_inf, project_algebraics_with_fixed_states_at_time,
    runtime_projection_required, seed_runtime_direct_assignments, solve_initial_algebraic,
};

#[cfg(test)]
mod tests;
