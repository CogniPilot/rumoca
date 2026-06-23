use super::*;
use std::path::{Path, PathBuf};

pub(super) fn eval_table_matrix_arg<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<Vec<f64>>>, EvalError> {
    match expr {
        rumoca_core::Expression::Array { elements, .. } => {
            if elements.is_empty() {
                return Ok(Some(Vec::new()));
            }

            if elements
                .iter()
                .all(|e| matches!(e, rumoca_core::Expression::Array { .. }))
            {
                return Ok(Some(
                    eval_matrix_literal_rows(elements, env)?
                        .into_iter()
                        .map(|row| row.into_iter().map(|value| value.real()).collect())
                        .collect(),
                ));
            }

            let values = eval_array_values::<T>(expr, env)?;
            if values.is_empty() {
                return Ok(Some(Vec::new()));
            }
            Ok(Some(vec![values.iter().map(|v| v.real()).collect()]))
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            let Some(flat_values) = array_values_from_env_name(name.as_str(), env)? else {
                return Ok(env
                    .start_exprs
                    .get(name.as_str())
                    .map(|start_expr| eval_table_matrix_arg(start_expr, env))
                    .transpose()?
                    .flatten());
            };
            Ok(Some(table_matrix_from_flat_values(
                name.as_str(),
                flat_values,
                env,
            )?))
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let Some(name) = flattened_field_access_name(base, field) else {
                return Ok(None);
            };
            let flat_values = try_eval_field_access_array_values(base, field, env)?;
            Ok(Some(table_matrix_from_flat_values(
                name.as_str(),
                flat_values.into_iter().map(|value| value.real()).collect(),
                env,
            )?))
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => eval_table_if_matrix_arg(branches, else_branch, env),
        _ => Ok(None),
    }
}

fn eval_table_if_matrix_arg<T: SimFloat>(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<Vec<f64>>>, EvalError> {
    for (condition, value) in branches {
        if eval_expr::<T>(condition, env)?.to_bool() {
            return eval_table_matrix_arg(value, env);
        }
    }
    eval_table_matrix_arg(else_branch, env)
}

fn table_matrix_from_flat_values<T: SimFloat>(
    name: &str,
    flat_values: Vec<f64>,
    env: &VarEnv<T>,
) -> Result<Vec<Vec<f64>>, EvalError> {
    if let Some(start_matrix) = richer_table_start_matrix(name, flat_values.len(), env)? {
        return Ok(start_matrix);
    }
    if flat_values.is_empty() {
        if let Some(start_expr) = env.start_exprs.get(name)
            && let Some(start_matrix) = eval_table_matrix_arg(start_expr, env)?
        {
            return Ok(start_matrix);
        }
        return Ok(Vec::new());
    }
    let raw_dims = declared_dims(name, env)?;
    let inferred = infer_dims_from_values(&raw_dims, flat_values.len())?;
    if inferred.len() >= 2 {
        return reshape_flat_matrix(&flat_values, inferred[0], inferred[1]);
    }
    Ok(vec![flat_values])
}

fn richer_table_start_matrix<T: SimFloat>(
    name: &str,
    flat_len: usize,
    env: &VarEnv<T>,
) -> Result<Option<Vec<Vec<f64>>>, EvalError> {
    let Some(start_expr) = env.start_exprs.get(name) else {
        return Ok(None);
    };
    if matches!(start_expr, rumoca_core::Expression::VarRef { name: start_name, .. } if start_name.as_str() == name)
    {
        return Ok(None);
    }
    let Some(start_matrix) = eval_table_matrix_arg(start_expr, env)? else {
        return Ok(None);
    };
    let start_len = start_matrix.iter().map(Vec::len).sum::<usize>();
    Ok((start_len > flat_len).then_some(start_matrix))
}

pub(super) fn eval_external_table_data_matrix<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
    is_time_table: bool,
) -> Result<Option<Vec<Vec<f64>>>, EvalError> {
    let table_arg_idx = 2usize;
    let Some(table_arg) = external_table_constructor_arg(args, "table", table_arg_idx) else {
        return Ok(None);
    };
    let table_matrix = eval_table_matrix_arg(table_arg, env)?;
    if table_matrix
        .as_ref()
        .is_some_and(|matrix| !matrix.is_empty())
    {
        return Ok(table_matrix);
    }

    let file_name = external_table_constructor_arg(args, "fileName", 1)
        .and_then(|expr| eval_string_expr(expr, env))
        .filter(|name| name != "NoName" && !name.trim().is_empty());
    let Some(file_name) = file_name else {
        return Ok(table_matrix);
    };

    let table_name = external_table_constructor_arg(args, "tableName", 0)
        .and_then(|expr| eval_string_expr(expr, env))
        .filter(|name| name != "NoName" && !name.trim().is_empty())
        .unwrap_or_else(|| {
            if is_time_table {
                "tab".to_string()
            } else {
                "tab1".to_string()
            }
        });

    Ok(read_modelica_text_table(&file_name, &table_name)?)
}

fn eval_string_expr<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<String> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(value),
            ..
        } => Some(value.clone()),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => env
            .start_exprs
            .get(name.as_str())
            .and_then(|start| eval_string_expr(start, env)),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                if eval_expr::<T>(condition, env).ok()?.to_bool() {
                    return eval_string_expr(value, env);
                }
            }
            eval_string_expr(else_branch, env)
        }
        rumoca_core::Expression::FunctionCall { name, args, .. }
            if rumoca_core::top_level_last_segment(name.as_str()) == "loadResource" =>
        {
            let raw = args.first().and_then(|arg| eval_string_expr(arg, env))?;
            resolve_modelica_resource_path(&raw)
                .map(|path| path.to_string_lossy().into_owned())
                .or(Some(raw))
        }
        _ => None,
    }
}

fn read_modelica_text_table(
    file_name: &str,
    table_name: &str,
) -> Result<Option<Vec<Vec<f64>>>, EvalError> {
    let Some(path) = resolve_modelica_resource_path(file_name) else {
        return Ok(None);
    };
    let text = std::fs::read_to_string(path).map_err(|_| EvalError::UnsupportedExpression {
        kind: "external table file",
    })?;
    parse_modelica_text_table(&text, table_name)
}

fn parse_modelica_text_table(
    text: &str,
    table_name: &str,
) -> Result<Option<Vec<Vec<f64>>>, EvalError> {
    let needle = format!("double {table_name}(");
    let mut lines = text.lines();
    let Some(header) = lines.find(|line| line.trim_start().starts_with(&needle)) else {
        return Ok(None);
    };
    let dims = header
        .split_once('(')
        .and_then(|(_, tail)| tail.split_once(')'))
        .map(|(dims, _)| dims)
        .ok_or(EvalError::UnsupportedExpression {
            kind: "external table header",
        })?
        .split(',')
        .map(|part| part.trim().parse::<usize>().ok())
        .collect::<Option<Vec<_>>>()
        .ok_or(EvalError::UnsupportedExpression {
            kind: "external table header",
        })?;
    if dims.len() != 2 || dims[0] == 0 || dims[1] < 2 {
        return Err(EvalError::UnsupportedExpression {
            kind: "external table header",
        });
    }

    let mut rows = Vec::with_capacity(dims[0]);
    for line in lines {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        let row = trimmed
            .split(|ch: char| ch == ',' || ch == ';' || ch.is_whitespace())
            .filter(|part| !part.is_empty())
            .map(|part| part.parse::<f64>().ok())
            .collect::<Option<Vec<_>>>()
            .ok_or(EvalError::UnsupportedExpression {
                kind: "external table row",
            })?;
        if row.len() != dims[1] {
            return Err(EvalError::UnsupportedExpression {
                kind: "external table row",
            });
        }
        rows.push(row);
        if rows.len() == dims[0] {
            return Ok(Some(rows));
        }
    }
    Err(EvalError::UnsupportedExpression {
        kind: "external table data",
    })
}

pub(super) fn resolve_modelica_resource_path(raw: &str) -> Option<PathBuf> {
    let raw_path = Path::new(raw);
    if raw_path.is_file() {
        return Some(raw_path.to_path_buf());
    }
    let (package, rest) = raw.strip_prefix("modelica://")?.split_once('/')?;
    for root in modelica_resource_source_roots() {
        if let Some(path) = resolve_modelica_uri_against_root(&root, package, rest) {
            return Some(path);
        }
    }
    None
}

fn modelica_resource_source_roots() -> Vec<PathBuf> {
    std::env::var_os("RUMOCA_MODELICA_SOURCE_ROOTS")
        .map(|paths| std::env::split_paths(&paths).collect())
        .unwrap_or_default()
}

fn resolve_modelica_uri_against_root(root: &Path, package: &str, rest: &str) -> Option<PathBuf> {
    let direct = root.join(rest);
    if root.file_name().and_then(|name| name.to_str()) == Some(package) && direct.is_file() {
        return Some(direct);
    }
    let nested = root.join(package).join(rest);
    nested.is_file().then_some(nested)
}

fn map_selected_table_column(
    columns: &[usize],
    requested_output_col: usize,
    data_col_count: usize,
) -> Result<usize, EvalError> {
    if data_col_count == 0 {
        return Err(EvalError::UnsupportedExpression {
            kind: "external table data columns",
        });
    }
    if columns.is_empty() {
        return Ok(requested_output_col
            .saturating_add(1)
            .min(data_col_count.saturating_sub(1)));
    }
    let mapped = columns
        .get(requested_output_col)
        .copied()
        .or_else(|| columns.last().copied())
        .ok_or(EvalError::UnsupportedExpression {
            kind: "external table columns",
        })?;
    Ok(mapped
        .saturating_sub(1)
        .min(data_col_count.saturating_sub(1)))
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
) -> Result<T, EvalError> {
    eval_table_1d_lookup_with_runtime(spec, requested_output_col, x, None)
}

pub(super) fn eval_table_1d_lookup_with_runtime<T: SimFloat>(
    spec: &ExternalTableSpec,
    requested_output_col: usize,
    x: T,
    runtime: Option<&EvalRuntimeState>,
) -> Result<T, EvalError> {
    if spec.data.is_empty() {
        return Err(EvalError::UnsupportedExpression {
            kind: "external table data",
        });
    }
    let data_col_count = spec
        .data
        .first()
        .ok_or(EvalError::UnsupportedExpression {
            kind: "external table data",
        })?
        .len();
    if data_col_count < 2 {
        return Err(EvalError::UnsupportedExpression {
            kind: "external table data columns",
        });
    }

    let output_col =
        map_selected_table_column(&spec.columns, requested_output_col, data_col_count)?;
    let (x_min, x_max) = table_x_bounds(spec).ok_or(EvalError::UnsupportedExpression {
        kind: "external table bounds",
    })?;

    let (x_real, out_of_range, preserve_dual_x) =
        apply_extrapolation_policy(runtime, x.real(), x_min, x_max, spec.extrapolation);
    let x_eval = if preserve_dual_x {
        x + T::from_f64(x_real - x.real())
    } else {
        T::from_f64(x_real)
    };
    if spec.data.len() == 1 {
        return Ok(T::from_f64(spec.data[0][output_col]));
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
            return Ok(T::from_f64(spec.data[last_idx][output_col]));
        }
        return Ok(T::from_f64(y0));
    }

    if (x1 - x0).abs() <= f64::EPSILON {
        return Ok(T::from_f64(y0));
    }

    let alpha = (x_eval - T::from_f64(x0)) / T::from_f64(x1 - x0);
    Ok(T::from_f64(y0) + alpha * T::from_f64(y1 - y0))
}

pub(super) fn eval_table_constructor<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
    is_time_table: bool,
) -> Result<Option<T>, EvalError> {
    let columns_arg_idx = if is_time_table { 4 } else { 3 };
    let smoothness_idx = if is_time_table { 5 } else { 4 };
    let extrapolation_idx = if is_time_table { 6 } else { 5 };

    let Some(table_matrix) = eval_external_table_data_matrix(args, env, is_time_table)? else {
        return Ok(None);
    };

    let columns = eval_columns_arg(
        external_table_constructor_arg(args, "columns", columns_arg_idx),
        env,
    )?;
    let smoothness = optional_integer_arg(
        external_table_constructor_arg(args, "smoothness", smoothness_idx),
        env,
        1,
    )?;
    let extrapolation = optional_integer_arg(
        external_table_constructor_arg(args, "extrapolation", extrapolation_idx),
        env,
        1,
    )?;

    let spec = ExternalTableSpec {
        data: table_matrix,
        columns,
        smoothness,
        extrapolation,
    };
    let id = register_external_table_in(&env.runtime.external_tables, spec);
    Ok(Some(T::from_f64(id as f64)))
}

fn optional_integer_arg<T: SimFloat>(
    expr: Option<&rumoca_core::Expression>,
    env: &VarEnv<T>,
    default: i64,
) -> Result<i64, EvalError> {
    let Some(expr) = expr else {
        return Ok(default);
    };
    Ok(eval_expr::<T>(expr, env)?.real().round() as i64)
}
