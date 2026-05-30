use super::*;
use crate::dual::Dual;

const NO_NEXT_TIME_EVENT: f64 = f64::MAX;

pub(super) fn eval_builtin_sum<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> T {
    if args.len() == 1 {
        return eval_array_like_values(&args[0], env)
            .into_iter()
            .fold(T::zero(), |acc, v| acc + v);
    }
    args.iter().fold(T::zero(), |acc, expr| {
        acc + eval_expr_or_default::<T>(expr, env)
    })
}

pub(super) fn eval_builtin_product<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> T {
    if args.len() == 1 {
        return eval_array_like_values(&args[0], env)
            .into_iter()
            .fold(T::one(), |acc, v| acc * v);
    }
    args.iter().fold(T::one(), |acc, expr| {
        acc * eval_expr_or_default::<T>(expr, env)
    })
}

/// Resolve a user-defined function by exact qualified name.
///
/// Function names must be resolved before runtime evaluation.
pub(super) fn resolve_user_function<'a, T: SimFloat>(
    name: &str,
    env: &'a VarEnv<T>,
) -> Option<&'a Function> {
    env.functions.get(name)
}

pub(super) fn resolve_function_closure<'a, T: SimFloat>(
    name: &str,
    env: &'a VarEnv<T>,
) -> Option<&'a FunctionClosure> {
    env.function_closures.get(name)
}

fn eval_table_id_arg<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> f64 {
    match args.first() {
        Some(expr) => eval_expr_or_default::<T>(expr, env).real(),
        None => 0.0,
    }
}

fn eval_table_col_arg<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> f64 {
    match args.get(1) {
        Some(expr) => eval_expr_or_default::<T>(expr, env).real(),
        None => 1.0,
    }
}

fn table_col_index_from_real(col_arg: f64) -> usize {
    (col_arg.round() as i64).max(1) as usize - 1
}

fn checked_table_col_index(spec: &ExternalTableSpec, col_arg: f64) -> Option<usize> {
    let rounded = col_arg.round();
    let output_count = table_output_count(spec);
    if !rounded.is_finite()
        || (rounded - col_arg).abs() > 1.0e-9
        || rounded < 1.0
        || rounded > output_count as f64
    {
        return None;
    }
    Some(rounded as usize - 1)
}

fn table_output_count(spec: &ExternalTableSpec) -> usize {
    if spec.columns.is_empty() {
        spec.data
            .first()
            .map(|row| row.len().saturating_sub(1))
            .unwrap_or(0)
    } else {
        spec.columns.len()
    }
}

pub fn eval_table_bound_value_in(
    table_id: f64,
    max: bool,
    tables: &[rumoca_core::ExternalTableData],
) -> f64 {
    let Some(spec) = lookup_external_table_in(table_id, tables) else {
        return 0.0;
    };
    eval_table_bound_spec(&spec, max)
}

pub fn try_eval_table_bound_value_in(
    table_id: f64,
    max: bool,
    tables: &[rumoca_core::ExternalTableData],
) -> Option<f64> {
    let spec = lookup_external_table_in(table_id, tables)?;
    try_eval_table_bound_spec(&spec, max)
}

fn eval_table_bound_spec(spec: &ExternalTableSpec, max: bool) -> f64 {
    try_eval_table_bound_spec(spec, max).unwrap_or_else(default_missing_table_bound)
}

fn try_eval_table_bound_spec(spec: &ExternalTableSpec, max: bool) -> Option<f64> {
    table_x_bounds(spec).map(|(min, upper)| if max { upper } else { min })
}

fn default_missing_table_bound() -> f64 {
    0.0
}

pub fn eval_table_lookup_value_in(
    table_id: f64,
    col_arg: f64,
    x: f64,
    tables: &[rumoca_core::ExternalTableData],
) -> f64 {
    let Some(spec) = lookup_external_table_in(table_id, tables) else {
        return 0.0;
    };
    eval_table_lookup_spec(&spec, col_arg, x)
}

pub fn try_eval_table_lookup_value_in(
    table_id: f64,
    col_arg: f64,
    x: f64,
    tables: &[rumoca_core::ExternalTableData],
) -> Option<f64> {
    let spec = lookup_external_table_in(table_id, tables)?;
    try_eval_table_lookup_spec(&spec, col_arg, x)
}

fn eval_table_lookup_spec<T: SimFloat>(spec: &ExternalTableSpec, col_arg: f64, x: T) -> T {
    let col_idx = table_col_index_from_real(col_arg);
    eval_table_1d_lookup(spec, col_idx, x)
}

fn try_eval_table_lookup_spec<T: SimFloat>(
    spec: &ExternalTableSpec,
    col_arg: f64,
    x: T,
) -> Option<T> {
    let col_idx = checked_table_col_index(spec, col_arg)?;
    Some(eval_table_1d_lookup(spec, col_idx, x))
}

pub fn eval_table_lookup_slope_value_in(
    table_id: f64,
    col_arg: f64,
    x: f64,
    tables: &[rumoca_core::ExternalTableData],
) -> f64 {
    let Some(spec) = lookup_external_table_in(table_id, tables) else {
        return 0.0;
    };
    eval_table_lookup_slope_spec(&spec, col_arg, x)
}

pub fn try_eval_table_lookup_slope_value_in(
    table_id: f64,
    col_arg: f64,
    x: f64,
    tables: &[rumoca_core::ExternalTableData],
) -> Option<f64> {
    let spec = lookup_external_table_in(table_id, tables)?;
    try_eval_table_lookup_slope_spec(&spec, col_arg, x)
}

fn eval_table_lookup_slope_spec(spec: &ExternalTableSpec, col_arg: f64, x: f64) -> f64 {
    let col_idx = table_col_index_from_real(col_arg);
    eval_table_1d_lookup(spec, col_idx, Dual::new(x, 1.0)).du
}

fn try_eval_table_lookup_slope_spec(spec: &ExternalTableSpec, col_arg: f64, x: f64) -> Option<f64> {
    let col_idx = checked_table_col_index(spec, col_arg)?;
    Some(eval_table_1d_lookup(spec, col_idx, Dual::new(x, 1.0)).du)
}

pub fn eval_time_table_next_event_value_in(
    table_id: f64,
    time_in: f64,
    tables: &[rumoca_core::ExternalTableData],
) -> f64 {
    let Some(spec) = lookup_external_table_in(table_id, tables) else {
        return NO_NEXT_TIME_EVENT;
    };
    eval_time_table_next_event_spec(&spec, time_in)
}

pub fn try_eval_time_table_next_event_value_in(
    table_id: f64,
    time_in: f64,
    tables: &[rumoca_core::ExternalTableData],
) -> Option<f64> {
    let spec = lookup_external_table_in(table_id, tables)?;
    Some(eval_time_table_next_event_spec(&spec, time_in))
}

fn eval_time_table_next_event_spec(spec: &ExternalTableSpec, time_in: f64) -> f64 {
    if spec.data.is_empty() {
        return NO_NEXT_TIME_EVENT;
    }

    let knots: Vec<f64> = spec
        .data
        .iter()
        .filter_map(|row| row.first().copied())
        .filter(|x| x.is_finite())
        .collect();
    if knots.is_empty() {
        return NO_NEXT_TIME_EVENT;
    }

    // Modelica.Blocks.Tables.Internal.getNextTimeEvent follows the next strict
    // future knot. For periodic extrapolation, the search wraps across cycles.
    let eps = 1e-12_f64;
    if spec.extrapolation == 3
        && let Some((x_min, x_max)) = table_x_bounds(spec)
    {
        let span = x_max - x_min;
        if span > eps {
            let cycle = ((time_in - x_min) / span).floor() as i64;
            let best = ((cycle - 1)..=(cycle + 2))
                .flat_map(|n| {
                    let shift = (n as f64) * span;
                    knots.iter().copied().map(move |x| x + shift)
                })
                .filter(|candidate| *candidate > time_in + eps)
                .min_by(|a, b| a.total_cmp(b));
            if let Some(best) = best {
                return best;
            }
        }
    }

    knots
        .into_iter()
        .find(|x| *x > time_in + eps)
        .unwrap_or(NO_NEXT_TIME_EVENT)
}

fn eval_table_bounds_call<T: SimFloat>(args: &[Expression], env: &VarEnv<T>, max: bool) -> T {
    let table_id = eval_table_id_arg(args, env);
    let Some(spec) = lookup_external_table_in_registry(&env.runtime.external_tables, table_id)
    else {
        return T::zero();
    };
    T::from_f64(eval_table_bound_spec(&spec, max))
}

fn eval_table_lookup_call<T: SimFloat>(
    args: &[Expression],
    env: &VarEnv<T>,
    input_arg_idx: usize,
) -> T {
    let table_id = eval_table_id_arg(args, env);
    let Some(spec) = lookup_external_table_in_registry(&env.runtime.external_tables, table_id)
    else {
        return T::zero();
    };
    let col_arg = eval_table_col_arg(args, env);
    let col_idx = table_col_index_from_real(col_arg);
    let x = args
        .get(input_arg_idx)
        .map(|e| eval_expr_or_default::<T>(e, env))
        .unwrap_or_else(T::zero);
    eval_table_1d_lookup_with_runtime(&spec, col_idx, x, Some(&env.runtime))
}

fn eval_time_table_next_event_call<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> T {
    let table_id = eval_table_id_arg(args, env);
    let time_in = args
        .get(1)
        .map(|e| eval_expr_or_default::<T>(e, env).real())
        .unwrap_or_else(default_missing_time_table_input);
    let Some(spec) = lookup_external_table_in_registry(&env.runtime.external_tables, table_id)
    else {
        return T::from_f64(NO_NEXT_TIME_EVENT);
    };
    T::from_f64(eval_time_table_next_event_spec(&spec, time_in))
}

fn default_missing_time_table_input() -> f64 {
    0.0
}

pub(super) fn eval_external_table_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    match short_name {
        "ExternalCombiTimeTable" => eval_table_constructor(args, env, true),
        "ExternalCombiTable1D" => eval_table_constructor(args, env, false),
        "getTimeTableTmax" => Some(eval_table_bounds_call(args, env, true)),
        "getTimeTableTmin" => Some(eval_table_bounds_call(args, env, false)),
        "getTimeTableValueNoDer" | "getTimeTableValueNoDer2" | "getTimeTableValue" => {
            Some(eval_table_lookup_call(args, env, 2))
        }
        "getNextTimeEvent" => Some(eval_time_table_next_event_call(args, env)),
        "getTable1DAbscissaUmax" => Some(eval_table_bounds_call(args, env, true)),
        "getTable1DAbscissaUmin" => Some(eval_table_bounds_call(args, env, false)),
        "getTable1DValueNoDer" | "getTable1DValueNoDer2" | "getTable1DValue" => {
            Some(eval_table_lookup_call(args, env, 2))
        }
        _ => None,
    }
}
