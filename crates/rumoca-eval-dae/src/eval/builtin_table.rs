use super::*;
use crate::dual::Dual;

const NO_NEXT_TIME_EVENT: f64 = f64::MAX;

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

fn eval_table_id_arg<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> Result<f64, EvalError> {
    let expr = args.first().ok_or(EvalError::UnsupportedExpression {
        kind: "external table id",
    })?;
    Ok(eval_expr::<T>(expr, env)?.real())
}

fn eval_table_col_arg<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> Result<f64, EvalError> {
    let expr = args.get(1).ok_or(EvalError::UnsupportedExpression {
        kind: "external table column",
    })?;
    Ok(eval_expr::<T>(expr, env)?.real())
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
) -> Result<f64, EvalError> {
    let spec =
        lookup_external_table_in(table_id, tables).ok_or_else(|| EvalError::MissingBinding {
            name: format!("external table {table_id}"),
        })?;
    eval_table_bound_spec(&spec, max)
}

pub fn eval_table_bound_value_opt_in(
    table_id: f64,
    max: bool,
    tables: &[rumoca_core::ExternalTableData],
) -> Option<f64> {
    let spec = lookup_external_table_in(table_id, tables)?;
    eval_table_bound_spec_opt(&spec, max)
}

fn eval_table_bound_spec(spec: &ExternalTableSpec, max: bool) -> Result<f64, EvalError> {
    eval_table_bound_spec_opt(spec, max).ok_or(EvalError::UnsupportedExpression {
        kind: "external table bounds",
    })
}

fn eval_table_bound_spec_opt(spec: &ExternalTableSpec, max: bool) -> Option<f64> {
    table_x_bounds(spec).map(|(min, upper)| if max { upper } else { min })
}

pub fn eval_table_lookup_value_in(
    table_id: f64,
    col_arg: f64,
    x: f64,
    tables: &[rumoca_core::ExternalTableData],
) -> Result<f64, EvalError> {
    let spec =
        lookup_external_table_in(table_id, tables).ok_or_else(|| EvalError::MissingBinding {
            name: format!("external table {table_id}"),
        })?;
    eval_table_lookup_spec(&spec, col_arg, x)
}

pub fn eval_table_lookup_value_opt_in(
    table_id: f64,
    col_arg: f64,
    x: f64,
    tables: &[rumoca_core::ExternalTableData],
) -> Option<f64> {
    let spec = lookup_external_table_in(table_id, tables)?;
    eval_table_lookup_spec_opt(&spec, col_arg, x)
}

fn eval_table_lookup_spec<T: SimFloat>(
    spec: &ExternalTableSpec,
    col_arg: f64,
    x: T,
) -> Result<T, EvalError> {
    let col_idx = table_col_index_from_real(col_arg);
    eval_table_1d_lookup(spec, col_idx, x)
}

fn eval_table_lookup_spec_opt<T: SimFloat>(
    spec: &ExternalTableSpec,
    col_arg: f64,
    x: T,
) -> Option<T> {
    let col_idx = checked_table_col_index(spec, col_arg)?;
    eval_table_1d_lookup(spec, col_idx, x).ok()
}

pub fn eval_table_lookup_slope_value_in(
    table_id: f64,
    col_arg: f64,
    x: f64,
    tables: &[rumoca_core::ExternalTableData],
) -> Result<f64, EvalError> {
    let spec =
        lookup_external_table_in(table_id, tables).ok_or_else(|| EvalError::MissingBinding {
            name: format!("external table {table_id}"),
        })?;
    eval_table_lookup_slope_spec(&spec, col_arg, x)
}

pub fn eval_table_lookup_slope_value_opt_in(
    table_id: f64,
    col_arg: f64,
    x: f64,
    tables: &[rumoca_core::ExternalTableData],
) -> Option<f64> {
    let spec = lookup_external_table_in(table_id, tables)?;
    eval_table_lookup_slope_spec_opt(&spec, col_arg, x)
}

fn eval_table_lookup_slope_spec(
    spec: &ExternalTableSpec,
    col_arg: f64,
    x: f64,
) -> Result<f64, EvalError> {
    let col_idx = table_col_index_from_real(col_arg);
    Ok(eval_table_1d_lookup(spec, col_idx, Dual::new(x, 1.0))?.du)
}

fn eval_table_lookup_slope_spec_opt(spec: &ExternalTableSpec, col_arg: f64, x: f64) -> Option<f64> {
    let col_idx = checked_table_col_index(spec, col_arg)?;
    Some(
        eval_table_1d_lookup(spec, col_idx, Dual::new(x, 1.0))
            .ok()?
            .du,
    )
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

pub fn eval_time_table_next_event_value_opt_in(
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

fn eval_table_bounds_call<T: SimFloat>(
    args: &[Expression],
    env: &VarEnv<T>,
    max: bool,
) -> Result<T, EvalError> {
    let table_id = eval_table_id_arg(args, env)?;
    let spec = lookup_external_table_in_registry(&env.runtime.external_tables, table_id)
        .ok_or_else(|| EvalError::MissingBinding {
            name: format!("external table {table_id}"),
        })?;
    let value = eval_table_bound_spec_opt(&spec, max).ok_or(EvalError::UnsupportedExpression {
        kind: "external table bounds",
    })?;
    Ok(T::from_f64(value))
}

fn eval_table_lookup_call<T: SimFloat>(
    args: &[Expression],
    env: &VarEnv<T>,
    input_arg_idx: usize,
) -> Result<T, EvalError> {
    let table_id = eval_table_id_arg(args, env)?;
    let spec = lookup_external_table_in_registry(&env.runtime.external_tables, table_id)
        .ok_or_else(|| EvalError::MissingBinding {
            name: format!("external table {table_id}"),
        })?;
    let col_arg = eval_table_col_arg(args, env)?;
    let col_idx =
        checked_table_col_index(&spec, col_arg).ok_or(EvalError::UnsupportedExpression {
            kind: "external table column",
        })?;
    let x = eval_expr::<T>(
        args.get(input_arg_idx)
            .ok_or(EvalError::UnsupportedExpression {
                kind: "external table input",
            })?,
        env,
    )?;
    eval_table_1d_lookup_with_runtime(&spec, col_idx, x, Some(&env.runtime))
}

fn eval_time_table_next_event_call<T: SimFloat>(
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let table_id = eval_table_id_arg(args, env)?;
    let time_in = eval_expr::<T>(
        args.get(1).ok_or(EvalError::UnsupportedExpression {
            kind: "time table input",
        })?,
        env,
    )?
    .real();
    let spec = lookup_external_table_in_registry(&env.runtime.external_tables, table_id)
        .ok_or_else(|| EvalError::MissingBinding {
            name: format!("external table {table_id}"),
        })?;
    Ok(T::from_f64(eval_time_table_next_event_spec(&spec, time_in)))
}

pub(super) fn eval_external_table_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    match short_name {
        "ExternalCombiTimeTable" => eval_table_constructor(args, env, true),
        "ExternalCombiTable1D" => eval_table_constructor(args, env, false),
        "getTimeTableTmax" => eval_table_bounds_call(args, env, true).map(Some),
        "getTimeTableTmin" => eval_table_bounds_call(args, env, false).map(Some),
        "getTimeTableValueNoDer" | "getTimeTableValueNoDer2" | "getTimeTableValue" => {
            eval_table_lookup_call(args, env, 2).map(Some)
        }
        "getNextTimeEvent" => eval_time_table_next_event_call(args, env).map(Some),
        "getTable1DAbscissaUmax" => eval_table_bounds_call(args, env, true).map(Some),
        "getTable1DAbscissaUmin" => eval_table_bounds_call(args, env, false).map(Some),
        "getTable1DValueNoDer" | "getTable1DValueNoDer2" | "getTable1DValue" => {
            eval_table_lookup_call(args, env, 2).map(Some)
        }
        _ => Ok(None),
    }
}
