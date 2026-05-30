use super::*;

pub(super) fn eval_table_matrix_arg<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<Vec<Vec<f64>>> {
    match expr {
        rumoca_core::Expression::Array { elements, .. } => {
            if elements.is_empty() {
                return Some(Vec::new());
            }

            if elements
                .iter()
                .all(|e| matches!(e, rumoca_core::Expression::Array { .. }))
            {
                return Some(
                    eval_matrix_literal_rows(elements, env)
                        .into_iter()
                        .map(|row| row.into_iter().map(|value| value.real()).collect())
                        .collect(),
                );
            }

            let values = eval_array_values::<T>(expr, env);
            if values.is_empty() {
                return Some(Vec::new());
            }
            Some(vec![values.iter().map(|v| v.real()).collect()])
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            let Some(flat_values) = array_values_from_env_name(name.as_str(), env) else {
                return env
                    .start_exprs
                    .get(name.as_str())
                    .and_then(|start_expr| eval_table_matrix_arg(start_expr, env));
            };
            Some(table_matrix_from_flat_values(
                name.as_str(),
                flat_values,
                env,
            ))
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let name = flattened_field_access_name(base, field)?;
            let flat_values = eval_field_access_array_values(base, field, env)?;
            Some(table_matrix_from_flat_values(
                name.as_str(),
                flat_values.into_iter().map(|value| value.real()).collect(),
                env,
            ))
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => eval_table_if_matrix_arg(branches, else_branch, env),
        _ => None,
    }
}

fn eval_table_if_matrix_arg<T: SimFloat>(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<Vec<Vec<f64>>> {
    for (condition, value) in branches {
        if eval_expr_or_default::<T>(condition, env).to_bool() {
            return eval_table_matrix_arg(value, env);
        }
    }
    eval_table_matrix_arg(else_branch, env)
}

fn table_matrix_from_flat_values<T: SimFloat>(
    name: &str,
    flat_values: Vec<f64>,
    env: &VarEnv<T>,
) -> Vec<Vec<f64>> {
    if let Some(start_matrix) = richer_table_start_matrix(name, flat_values.len(), env) {
        return start_matrix;
    }
    if flat_values.is_empty() {
        if let Some(start_expr) = env.start_exprs.get(name)
            && let Some(start_matrix) = eval_table_matrix_arg(start_expr, env)
        {
            return start_matrix;
        }
        return Vec::new();
    }
    let raw_dims = declared_dims_or_scalar(name, env);
    let inferred = infer_dims_from_values(&raw_dims, flat_values.len());
    if inferred.len() >= 2 {
        let rows = inferred[0].max(1);
        let cols = inferred[1].max(1);
        return reshape_flat_matrix(&flat_values, rows, cols);
    }
    vec![flat_values]
}

fn richer_table_start_matrix<T: SimFloat>(
    name: &str,
    flat_len: usize,
    env: &VarEnv<T>,
) -> Option<Vec<Vec<f64>>> {
    let start_expr = env.start_exprs.get(name)?;
    if matches!(start_expr, rumoca_core::Expression::VarRef { name: start_name, .. } if start_name.as_str() == name)
    {
        return None;
    }
    let start_matrix = eval_table_matrix_arg(start_expr, env)?;
    let start_len = start_matrix.iter().map(Vec::len).sum::<usize>();
    (start_len > flat_len).then_some(start_matrix)
}

fn map_selected_table_column(
    columns: &[usize],
    requested_output_col: usize,
    data_col_count: usize,
) -> usize {
    if data_col_count == 0 {
        return 0;
    }
    if columns.is_empty() {
        return requested_output_col
            .saturating_add(1)
            .min(data_col_count.saturating_sub(1));
    }
    let mapped = columns
        .get(requested_output_col)
        .copied()
        .or_else(|| columns.last().copied())
        .unwrap_or(1);
    mapped
        .saturating_sub(1)
        .min(data_col_count.saturating_sub(1))
}

pub(super) fn table_x_bounds(spec: &ExternalTableSpec) -> Option<(f64, f64)> {
    let first = spec.data.first()?.first().copied()?;
    let last = spec.data.last()?.first().copied()?;
    Some((first, last))
}

fn apply_extrapolation_policy(
    runtime: Option<&EvalRuntimeState>,
    mut x: f64,
    x_min: f64,
    x_max: f64,
    extrapolation: i64,
) -> (f64, bool, bool) {
    if !x_min.is_finite() || !x_max.is_finite() || x_min > x_max {
        if let Some(runtime) = runtime {
            warn_once!(
                runtime.warned_table_invalid_bounds,
                "Invalid table bounds [{x_min}, {x_max}] during lookup; keeping input value."
            );
        }
        return (x, false, true);
    }
    if x_min == x_max {
        return (x_min, false, false);
    }
    if x >= x_min && x <= x_max {
        return (x, false, true);
    }
    match extrapolation {
        1 => {
            x = x.clamp(x_min, x_max);
            (x, true, false)
        }
        2 => (x, true, true),
        3 => {
            let span = x_max - x_min;
            if span > 0.0 {
                let mut wrapped = (x - x_min) % span;
                if wrapped < 0.0 {
                    wrapped += span;
                }
                x = x_min + wrapped;
            } else {
                x = x_min;
            }
            (x, false, true)
        }
        4 => {
            if let Some(runtime) = runtime {
                warn_once!(
                    runtime.warned_table_extrapolation,
                    "NoExtrapolation requested for table lookup; clamping to table bounds."
                );
            }
            x = x.clamp(x_min, x_max);
            (x, true, false)
        }
        _ => (x.clamp(x_min, x_max), true, false),
    }
}

pub(super) fn eval_table_1d_lookup<T: SimFloat>(
    spec: &ExternalTableSpec,
    requested_output_col: usize,
    x: T,
) -> T {
    eval_table_1d_lookup_with_runtime(spec, requested_output_col, x, None)
}

pub(super) fn eval_table_1d_lookup_with_runtime<T: SimFloat>(
    spec: &ExternalTableSpec,
    requested_output_col: usize,
    x: T,
    runtime: Option<&EvalRuntimeState>,
) -> T {
    if spec.data.is_empty() {
        return T::zero();
    }
    let data_col_count = spec.data.first().map(|r| r.len()).unwrap_or(0);
    if data_col_count < 2 {
        return T::zero();
    }

    let output_col = map_selected_table_column(&spec.columns, requested_output_col, data_col_count);
    let (x_min, x_max) = match table_x_bounds(spec) {
        Some(bounds) => bounds,
        None => return T::zero(),
    };

    let (x_real, out_of_range, preserve_dual_x) =
        apply_extrapolation_policy(runtime, x.real(), x_min, x_max, spec.extrapolation);
    let x_eval = if preserve_dual_x {
        x + T::from_f64(x_real - x.real())
    } else {
        T::from_f64(x_real)
    };
    if spec.data.len() == 1 {
        return T::from_f64(spec.data[0][output_col]);
    }

    let last_idx = spec.data.len() - 1;
    let k = if x_real <= spec.data[0][0] {
        0usize
    } else if x_real >= spec.data[last_idx][0] {
        last_idx.saturating_sub(1)
    } else {
        let mut idx = 0usize;
        while idx + 1 < spec.data.len() && x_real >= spec.data[idx + 1][0] {
            idx += 1;
        }
        idx.min(last_idx.saturating_sub(1))
    };

    let x0 = spec.data[k][0];
    let x1 = spec.data[k + 1][0];
    let y0 = spec.data[k][output_col];
    let y1 = spec.data[k + 1][output_col];

    if spec.smoothness == 3 && !out_of_range {
        if x_real >= x_max {
            return T::from_f64(spec.data[last_idx][output_col]);
        }
        return T::from_f64(y0);
    }

    if (x1 - x0).abs() <= f64::EPSILON {
        return T::from_f64(y0);
    }

    let alpha = (x_eval - T::from_f64(x0)) / T::from_f64(x1 - x0);
    T::from_f64(y0) + alpha * T::from_f64(y1 - y0)
}

pub(super) fn eval_table_constructor<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
    is_time_table: bool,
) -> Option<T> {
    let table_arg_idx = 2usize;
    let columns_arg_idx = if is_time_table { 4 } else { 3 };
    let smoothness_idx = if is_time_table { 5 } else { 4 };
    let extrapolation_idx = if is_time_table { 6 } else { 5 };

    let table_matrix = eval_table_matrix_arg(
        external_table_constructor_arg(args, "table", table_arg_idx)?,
        env,
    )?;
    if table_matrix.is_empty() {
        return None;
    }

    let columns = eval_columns_arg(
        external_table_constructor_arg(args, "columns", columns_arg_idx),
        env,
    );
    let smoothness = external_table_constructor_arg(args, "smoothness", smoothness_idx)
        .map(|e| eval_expr_or_default::<T>(e, env).real().round() as i64)
        .unwrap_or(1);
    let extrapolation = external_table_constructor_arg(args, "extrapolation", extrapolation_idx)
        .map(|e| eval_expr_or_default::<T>(e, env).real().round() as i64)
        .unwrap_or(1);

    let spec = ExternalTableSpec {
        data: table_matrix,
        columns,
        smoothness,
        extrapolation,
    };
    let id = register_external_table_in(&env.runtime.external_tables, spec);
    Some(T::from_f64(id as f64))
}
