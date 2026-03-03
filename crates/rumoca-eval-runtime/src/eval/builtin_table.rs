use super::*;

pub(super) fn eval_builtin_sum<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> T {
    if args.len() == 1 {
        return eval_array_like_values(&args[0], env)
            .into_iter()
            .fold(T::zero(), |acc, v| acc + v);
    }
    args.iter()
        .fold(T::zero(), |acc, expr| acc + eval_expr::<T>(expr, env))
}

pub(super) fn eval_builtin_product<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> T {
    if args.len() == 1 {
        return eval_array_like_values(&args[0], env)
            .into_iter()
            .fold(T::one(), |acc, v| acc * v);
    }
    args.iter()
        .fold(T::one(), |acc, expr| acc * eval_expr::<T>(expr, env))
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
    args.first()
        .map(|e| eval_expr::<T>(e, env).real())
        .unwrap_or(0.0)
}

fn eval_table_col_arg<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> usize {
    args.get(1)
        .map(|e| eval_expr::<T>(e, env).real().round() as i64)
        .unwrap_or(1)
        .max(1) as usize
        - 1
}

fn eval_table_bounds_call<T: SimFloat>(args: &[Expression], env: &VarEnv<T>, max: bool) -> T {
    let table_id = eval_table_id_arg(args, env);
    let Some(spec) = lookup_external_table(table_id) else {
        return T::zero();
    };
    table_x_bounds(&spec)
        .map(|(min, upper)| {
            if max {
                T::from_f64(upper)
            } else {
                T::from_f64(min)
            }
        })
        .unwrap_or_else(T::zero)
}

fn eval_table_lookup_call<T: SimFloat>(
    args: &[Expression],
    env: &VarEnv<T>,
    input_arg_idx: usize,
) -> T {
    let table_id = eval_table_id_arg(args, env);
    let col_idx = eval_table_col_arg(args, env);
    let x = args
        .get(input_arg_idx)
        .map(|e| eval_expr::<T>(e, env))
        .unwrap_or_else(T::zero);
    let Some(spec) = lookup_external_table(table_id) else {
        return T::zero();
    };
    eval_table_1d_lookup(&spec, col_idx, x)
}

fn eval_time_table_next_event_call<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> T {
    let table_id = eval_table_id_arg(args, env);
    let time_in = args
        .get(1)
        .map(|e| eval_expr::<T>(e, env).real())
        .unwrap_or(0.0);
    let Some(spec) = lookup_external_table(table_id) else {
        return T::from_f64(f64::INFINITY);
    };
    if spec.data.is_empty() {
        return T::from_f64(f64::INFINITY);
    }

    let knots: Vec<f64> = spec
        .data
        .iter()
        .filter_map(|row| row.first().copied())
        .filter(|x| x.is_finite())
        .collect();
    if knots.is_empty() {
        return T::from_f64(f64::INFINITY);
    }

    // Return the first strict future knot; if none exists, there are no more
    // scheduled table time events in this horizon. For periodic extrapolation,
    // search across neighboring cycles.
    let eps = 1e-12_f64;
    if spec.extrapolation == 3
        && let Some((x_min, x_max)) = table_x_bounds(&spec)
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
                return T::from_f64(best);
            }
        }
    }

    for &x in &knots {
        if x > time_in + eps {
            return T::from_f64(x);
        }
    }
    T::from_f64(f64::INFINITY)
}

pub(super) fn eval_external_table_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    match short_name {
        "ExternalCombiTimeTable" => {
            Some(eval_table_constructor(args, env, true).unwrap_or_else(T::zero))
        }
        "ExternalCombiTable1D" => {
            Some(eval_table_constructor(args, env, false).unwrap_or_else(T::zero))
        }
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
