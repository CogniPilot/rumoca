use rumoca_eval_flat::eval::{self, build_env, eval_expr};
use rumoca_ir_dae as dae;

use crate::equation_scalarize::build_output_names;
use crate::runtime::solver_runtime::panic_payload_message;
use crate::simulation::diagnostics::truncate_debug;

#[derive(Debug)]
pub struct ResidualDiagnostic {
    pub eq_idx: usize,
    pub abs: f64,
    pub non_finite: bool,
    pub origin: String,
    pub rhs: String,
}

pub fn collect_residual_diagnostics(
    dae_model: &dae::Dae,
    env: &eval::VarEnv<f64>,
) -> Vec<ResidualDiagnostic> {
    let mut residuals = Vec::with_capacity(dae_model.f_x.len());
    for (idx, eq) in dae_model.f_x.iter().enumerate() {
        let residual = eval_expr::<f64>(&eq.rhs, env);
        residuals.push(ResidualDiagnostic {
            eq_idx: idx,
            abs: if residual.is_finite() {
                residual.abs()
            } else {
                f64::INFINITY
            },
            non_finite: !residual.is_finite(),
            origin: eq.origin.clone(),
            rhs: truncate_debug(&format!("{:?}", eq.rhs), 180),
        });
    }
    residuals.sort_by(|a, b| {
        b.abs
            .partial_cmp(&a.abs)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then_with(|| a.eq_idx.cmp(&b.eq_idx))
    });
    residuals
}

pub fn trace_residual_diagnostics(dae_model: &dae::Dae, t: f64, residuals: &[ResidualDiagnostic]) {
    let worst = residuals.first().map(|entry| entry.abs).unwrap_or(0.0);
    let non_finite_rows = residuals.iter().filter(|entry| entry.non_finite).count();
    eprintln!(
        "[sim-trace] step-fail diagnostics: t={} n_eq={} non_finite_rows={} worst_abs_residual={}",
        t,
        dae_model.f_x.len(),
        non_finite_rows,
        worst
    );
    for (rank, row) in residuals.iter().take(8).enumerate() {
        eprintln!(
            "[sim-trace]   residual_rank={} eq=f_x[{}] abs={} non_finite={} origin='{}' rhs={}",
            rank, row.eq_idx, row.abs, row.non_finite, row.origin, row.rhs
        );
    }
}

pub fn sorted_value_rows(y: &[f64]) -> Vec<(usize, f64, bool)> {
    let mut rows: Vec<(usize, f64, bool)> = y
        .iter()
        .copied()
        .enumerate()
        .map(|(idx, value)| {
            let abs = if value.is_finite() {
                value.abs()
            } else {
                f64::INFINITY
            };
            (idx, abs, !value.is_finite())
        })
        .collect();
    rows.sort_by(|a, b| {
        b.1.partial_cmp(&a.1)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then_with(|| a.0.cmp(&b.0))
    });
    rows
}

pub fn trace_state_value_diagnostics(dae_model: &dae::Dae, y: &[f64]) -> Vec<String> {
    let mut names = build_output_names(dae_model);
    names.truncate(y.len());
    for (rank, (idx, abs, non_finite)) in sorted_value_rows(y).iter().take(8).enumerate() {
        let name = names.get(*idx).map(String::as_str).unwrap_or("<unnamed>");
        let value = y.get(*idx).copied().unwrap_or(0.0);
        eprintln!(
            "[sim-trace]   value_rank={} y[{}] name={} value={} abs={} non_finite={}",
            rank, idx, name, value, abs, non_finite
        );
    }
    names
}

pub fn jacobian_failure_introspection_enabled() -> bool {
    std::env::var("RUMOCA_SIM_INTROSPECT_JAC_FAILURE")
        .map(|v| {
            let s = v.trim().to_ascii_lowercase();
            !s.is_empty() && s != "0" && s != "false" && s != "no"
        })
        .unwrap_or(false)
}

pub fn safe_eval_expr_for_failure(expr: &dae::Expression, env: &eval::VarEnv<f64>) -> String {
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| eval_expr::<f64>(expr, env))) {
        Ok(value) => format!("{value}"),
        Err(payload) => format!("panic({})", panic_payload_message(payload)),
    }
}

pub fn trace_function_calls_in_expr(
    expr: &dae::Expression,
    env: &eval::VarEnv<f64>,
    remaining: &mut usize,
) -> usize {
    if *remaining == 0 {
        return 0;
    }
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            let value = safe_eval_expr_for_failure(expr, env);
            let arg_values = args
                .iter()
                .take(4)
                .map(|arg| safe_eval_expr_for_failure(arg, env))
                .collect::<Vec<_>>()
                .join(", ");
            eprintln!(
                "[sim-introspect]   function-eval builtin={:?} value={} args=[{}] expr={}",
                function,
                value,
                arg_values,
                truncate_debug(&format!("{:?}", expr), 180)
            );
            *remaining -= 1;
            let mut found = 1usize;
            for arg in args {
                found += trace_function_calls_in_expr(arg, env, remaining);
                if *remaining == 0 {
                    break;
                }
            }
            found
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            let value = safe_eval_expr_for_failure(expr, env);
            let arg_values = args
                .iter()
                .take(4)
                .map(|arg| safe_eval_expr_for_failure(arg, env))
                .collect::<Vec<_>>()
                .join(", ");
            eprintln!(
                "[sim-introspect]   function-eval call={} value={} args=[{}] expr={}",
                name,
                value,
                arg_values,
                truncate_debug(&format!("{:?}", expr), 180)
            );
            *remaining -= 1;
            let mut found = 1usize;
            for arg in args {
                found += trace_function_calls_in_expr(arg, env, remaining);
                if *remaining == 0 {
                    break;
                }
            }
            found
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            trace_function_calls_in_expr(lhs, env, remaining)
                + trace_function_calls_in_expr(rhs, env, remaining)
        }
        dae::Expression::Unary { rhs, .. } => trace_function_calls_in_expr(rhs, env, remaining),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            let mut found = 0usize;
            for (cond, value) in branches {
                found += trace_function_calls_in_expr(cond, env, remaining);
                if *remaining == 0 {
                    return found;
                }
                found += trace_function_calls_in_expr(value, env, remaining);
                if *remaining == 0 {
                    return found;
                }
            }
            found + trace_function_calls_in_expr(else_branch, env, remaining)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            let mut found = 0usize;
            for elem in elements {
                found += trace_function_calls_in_expr(elem, env, remaining);
                if *remaining == 0 {
                    break;
                }
            }
            found
        }
        dae::Expression::Index { base, .. } => trace_function_calls_in_expr(base, env, remaining),
        _ => 0,
    }
}

pub fn trace_function_eval_diagnostics(
    dae_model: &dae::Dae,
    env: &eval::VarEnv<f64>,
    residuals: &[ResidualDiagnostic],
) {
    if !jacobian_failure_introspection_enabled() {
        return;
    }
    let mut remaining = 12usize;
    for row in residuals.iter().take(4) {
        if remaining == 0 {
            break;
        }
        let Some(eq) = dae_model.f_x.get(row.eq_idx) else {
            continue;
        };
        eprintln!(
            "[sim-introspect] function-eval equation f_x[{}] origin='{}' abs_residual={}",
            row.eq_idx, row.origin, row.abs
        );
        let found = trace_function_calls_in_expr(&eq.rhs, env, &mut remaining);
        if found == 0 {
            eprintln!(
                "[sim-introspect]   no function calls found in f_x[{}]",
                row.eq_idx
            );
        }
    }
}

pub fn trace_jacobian_failure_diagnostics<F>(
    dae_model: &dae::Dae,
    y: &[f64],
    names: &[String],
    mut eval_jacobian_vector: F,
) where
    F: FnMut(&[f64], &mut [f64]),
{
    if !jacobian_failure_introspection_enabled() || y.is_empty() {
        return;
    }

    let n_total = y.len();
    let mut row_norms = vec![0.0_f64; n_total];
    let mut col_norms = vec![0.0_f64; n_total];
    let preview_n = n_total.min(8);
    let mut jac_preview = vec![vec![0.0_f64; preview_n]; preview_n];
    let mut v = vec![0.0_f64; n_total];
    let mut jv = vec![0.0_f64; n_total];

    for col in 0..n_total {
        v[col] = 1.0;
        eval_jacobian_vector(&v, &mut jv);
        v[col] = 0.0;
        col_norms[col] = jv.iter().fold(0.0_f64, |acc, value| acc.max(value.abs()));
        if col < preview_n {
            for row in 0..preview_n {
                jac_preview[row][col] = jv[row];
            }
        }
        for (row, val) in jv.iter().copied().enumerate() {
            row_norms[row] = row_norms[row].max(val.abs());
        }
    }

    let near_zero_rows: Vec<usize> = row_norms
        .iter()
        .enumerate()
        .filter_map(|(idx, norm)| (*norm <= 1.0e-12).then_some(idx))
        .collect();
    let near_zero_cols: Vec<usize> = col_norms
        .iter()
        .enumerate()
        .filter_map(|(idx, norm)| (*norm <= 1.0e-12).then_some(idx))
        .collect();

    eprintln!(
        "[sim-introspect] step-fail Jacobian norms: n={} near_zero_rows={} near_zero_cols={}",
        n_total,
        near_zero_rows.len(),
        near_zero_cols.len()
    );
    for idx in near_zero_rows.iter().copied().take(8) {
        let origin = dae_model
            .f_x
            .get(idx)
            .map(|eq| eq.origin.as_str())
            .unwrap_or("<missing-eq>");
        eprintln!(
            "[sim-introspect]   near-zero row f_x[{}] norm={} origin={}",
            idx, row_norms[idx], origin
        );
    }
    for idx in near_zero_cols.iter().copied().take(8) {
        let name = names.get(idx).map(String::as_str).unwrap_or("<unnamed>");
        eprintln!(
            "[sim-introspect]   near-zero col y[{}] {} norm={}",
            idx, name, col_norms[idx]
        );
    }

    if preview_n > 0 {
        eprintln!(
            "[sim-introspect] Jacobian preview (rows 0..{}, cols 0..{})",
            preview_n - 1,
            preview_n - 1
        );
        for (row_idx, row) in jac_preview.iter().enumerate() {
            let values = row
                .iter()
                .map(|value| format!("{value:.3e}"))
                .collect::<Vec<_>>()
                .join(", ");
            eprintln!(
                "[sim-introspect]   J[{}][0..{}] = [{}]",
                row_idx,
                preview_n - 1,
                values
            );
        }
    }
}

pub fn trace_step_failure_diagnostics<F>(
    dae_model: &dae::Dae,
    y: &[f64],
    t: f64,
    param_values: &[f64],
    eval_jacobian_vector: F,
) where
    F: FnMut(&[f64], &mut [f64]),
{
    let env = build_env(dae_model, y, param_values, t);
    let residuals = collect_residual_diagnostics(dae_model, &env);
    trace_residual_diagnostics(dae_model, t, &residuals);
    trace_function_eval_diagnostics(dae_model, &env, &residuals);
    let names = trace_state_value_diagnostics(dae_model, y);
    trace_jacobian_failure_diagnostics(dae_model, y, &names, eval_jacobian_vector);
}
