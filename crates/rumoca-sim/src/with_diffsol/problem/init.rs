use super::core::{
    InitJacobianEvalContext, build_init_jacobian_colored, build_init_jacobian_dense, clamp_finite,
    extract_direct_assignment, find_fixed_state_indices, initialize_state_vector_with_params,
    log_init_linear_system_diagnostics, seed_direct_assignment_initial_values,
    seed_runtime_direct_assignment_values,
    seed_runtime_direct_assignment_values_with_context_and_env_and_blocked_solver_cols,
    solver_idx_for_target, solver_vector_names,
};
use super::*;
mod initial_solve;
pub(crate) use initial_solve::*;

mod projection_apply;
pub(crate) use projection_apply::*;

mod projection_masks;
pub(crate) use projection_masks::*;

fn build_init_jacobian(
    ctx: &InitJacobianEvalContext<'_>,
    fixed_cols: &[bool],
    runtime_fd_jac_cols: &[bool],
    runtime_seed_env: Option<&VarEnv<f64>>,
    ignored_rows: &[bool],
    homotopy_lambda: f64,
    timeout: &crate::TimeoutBudget,
) -> Result<nalgebra::DMatrix<f64>, crate::SimError> {
    if homotopy_lambda < 1.0 - f64::EPSILON
        || (ctx.use_initial && ctx.compiled_initial.is_none())
        || (!ctx.use_initial && ctx.compiled_runtime.is_none())
    {
        return build_runtime_initial_jacobian_dense(
            ctx,
            fixed_cols,
            runtime_seed_env,
            homotopy_lambda,
            timeout,
        );
    }

    let mut jac = if !ctx.use_initial {
        build_init_jacobian_dense(ctx, fixed_cols, timeout)?
    } else if let Some(jac) = build_init_jacobian_colored(ctx, fixed_cols, timeout)? {
        jac
    } else {
        if sim_trace_enabled() {
            eprintln!("[sim-trace] IC Jacobian coloring fallback -> dense assembly");
        }
        build_init_jacobian_dense(ctx, fixed_cols, timeout)?
    };
    overwrite_runtime_fd_jacobian_cols(
        &mut jac,
        ctx,
        fixed_cols,
        runtime_fd_jac_cols,
        runtime_seed_env,
        ignored_rows,
        timeout,
    )?;
    Ok(jac)
}

fn build_runtime_initial_jacobian_dense(
    ctx: &InitJacobianEvalContext<'_>,
    fixed_cols: &[bool],
    runtime_seed_env: Option<&VarEnv<f64>>,
    homotopy_lambda: f64,
    timeout: &crate::TimeoutBudget,
) -> Result<nalgebra::DMatrix<f64>, crate::SimError> {
    let n_total = ctx.y.len();
    let eval_mode = IcEvalMode {
        compiled_initial: None,
        compiled_runtime: None,
        runtime_seed_env: runtime_seed_env.cloned(),
        use_initial: ctx.use_initial,
        is_initial_phase: true,
        homotopy_lambda,
        t_eval: ctx.t_eval,
        n_x: ctx.n_x,
        ignored_rows: None,
    };
    let mut base_rhs = vec![0.0; n_total];
    eval_ic_rhs_at_time(ctx.dae, ctx.y, ctx.p, &eval_mode, &mut base_rhs);

    let mut jac = nalgebra::DMatrix::<f64>::zeros(n_total, n_total);
    let mut y_perturbed = ctx.y.to_vec();
    let mut rhs_perturbed = vec![0.0; n_total];
    for j in 0..n_total {
        timeout.check()?;
        if fixed_cols.get(j).copied().unwrap_or(false) {
            continue;
        }
        let step = runtime_fd_jacobian_step(ctx.y[j]);
        y_perturbed.copy_from_slice(ctx.y);
        y_perturbed[j] += step;
        eval_ic_rhs_at_time(ctx.dae, &y_perturbed, ctx.p, &eval_mode, &mut rhs_perturbed);
        for i in 0..n_total {
            jac[(i, j)] = clamp_finite((rhs_perturbed[i] - base_rhs[i]) / step);
        }
    }
    Ok(jac)
}

struct NewtonInitConfig<'a> {
    n_x: usize,
    fixed_cols: &'a [bool],
    ignored_rows: &'a [bool],
    runtime_fd_jac_cols: &'a [bool],
    use_initial: bool,
    is_initial_phase: bool,
    homotopy_lambda: f64,
    compiled_initial: Option<&'a CompiledInitialNewtonContext>,
    compiled_runtime: Option<&'a CompiledRuntimeNewtonContext>,
    runtime_seed_env: Option<VarEnv<f64>>,
    t_eval: f64,
    timeout: &'a crate::TimeoutBudget,
}

type CachedNewtonJacobian = Option<nalgebra::DMatrix<f64>>;
type NewtonStepOutcome = Result<Option<(f64, CachedNewtonJacobian)>, crate::SimError>;

fn runtime_fd_jacobian_step(value: f64) -> f64 {
    1.0e-8 * value.abs().max(1.0)
}

fn overwrite_runtime_fd_jacobian_cols(
    jac: &mut nalgebra::DMatrix<f64>,
    ctx: &InitJacobianEvalContext<'_>,
    fixed_cols: &[bool],
    runtime_fd_jac_cols: &[bool],
    runtime_seed_env: Option<&VarEnv<f64>>,
    ignored_rows: &[bool],
    timeout: &crate::TimeoutBudget,
) -> Result<(), crate::SimError> {
    if ctx.use_initial || !runtime_fd_jac_cols.iter().any(|&flag| flag) {
        return Ok(());
    }

    // MLS equations.tex "Events and Synchronization": relations inside
    // noEvent/smooth are taken literally during continuous integration, so the
    // runtime Jacobian columns for those branch-local analog unknowns must be
    // built from the same literal residual evaluator as the Newton residual.
    let eval_mode = IcEvalMode {
        compiled_initial: ctx.compiled_initial,
        compiled_runtime: ctx.compiled_runtime,
        runtime_seed_env: runtime_seed_env.cloned(),
        use_initial: false,
        is_initial_phase: false,
        homotopy_lambda: 1.0,
        t_eval: ctx.t_eval,
        n_x: ctx.n_x,
        ignored_rows: Some(ignored_rows),
    };
    let n_total = ctx.y.len();
    let mut base_rhs = vec![0.0; n_total];
    eval_ic_rhs_at_time(ctx.dae, ctx.y, ctx.p, &eval_mode, &mut base_rhs);
    zero_ignored_residual_rows(&mut base_rhs, ignored_rows);

    let mut y_perturbed = ctx.y.to_vec();
    let mut rhs_perturbed = vec![0.0; n_total];
    let mut overwritten = 0usize;
    for j in 0..n_total {
        timeout.check()?;
        if fixed_cols.get(j).copied().unwrap_or(false)
            || !runtime_fd_jac_cols.get(j).copied().unwrap_or(false)
        {
            continue;
        }
        let step = runtime_fd_jacobian_step(ctx.y[j]);
        y_perturbed.copy_from_slice(ctx.y);
        y_perturbed[j] += step;
        eval_ic_rhs_at_time(ctx.dae, &y_perturbed, ctx.p, &eval_mode, &mut rhs_perturbed);
        zero_ignored_residual_rows(&mut rhs_perturbed, ignored_rows);
        for i in 0..n_total {
            jac[(i, j)] = clamp_finite((rhs_perturbed[i] - base_rhs[i]) / step);
        }
        overwritten += 1;
    }
    if sim_trace_enabled() && overwritten > 0 {
        eprintln!(
            "[sim-trace] runtime projection finite-difference Jacobian columns={}",
            overwritten
        );
    }
    Ok(())
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NewtonLinearSolveMethod {
    Lu,
    PseudoInverse,
    LeastSquaresPseudoInverse,
}

fn solve_newton_linear_system_square(
    jac: &nalgebra::DMatrix<f64>,
    rhs: &nalgebra::DVector<f64>,
) -> Option<(nalgebra::DVector<f64>, NewtonLinearSolveMethod)> {
    if let Some(delta) = jac.clone().lu().solve(rhs)
        && delta.iter().all(|value| value.is_finite())
    {
        return Some((delta, NewtonLinearSolveMethod::Lu));
    }

    let scale = jac
        .iter()
        .fold(0.0_f64, |max_abs, value| max_abs.max(value.abs()));
    let eps = (scale.max(1.0)) * 1.0e-12;
    let pinv = jac.clone().svd(true, true).pseudo_inverse(eps).ok()?;
    let delta = pinv * rhs;
    if delta.iter().all(|value| value.is_finite()) {
        Some((delta, NewtonLinearSolveMethod::PseudoInverse))
    } else {
        None
    }
}

fn solve_newton_linear_system_reduced(
    jac: &nalgebra::DMatrix<f64>,
    rhs: &nalgebra::DVector<f64>,
    ignored_rows: &[bool],
    fixed_cols: &[bool],
) -> Option<(nalgebra::DVector<f64>, NewtonLinearSolveMethod)> {
    let n_total = jac.ncols();
    let active_cols: Vec<usize> = (0..n_total)
        .filter(|&j| j >= fixed_cols.len() || !fixed_cols[j])
        .collect();
    let active_rows: Vec<usize> = (0..jac.nrows())
        .filter(|&i| i >= ignored_rows.len() || !ignored_rows[i])
        .collect();

    if active_cols.is_empty() || active_rows.is_empty() {
        return Some((
            nalgebra::DVector::zeros(n_total),
            NewtonLinearSolveMethod::LeastSquaresPseudoInverse,
        ));
    }
    if active_cols.len() == n_total && active_rows.len() == jac.nrows() {
        return solve_newton_linear_system_square(jac, rhs);
    }

    // Runtime projection can ignore residual rows independently from fixed
    // solver columns because equation ordering may diverge from solver-vector
    // ordering after DAE lowering/reordering.
    let mut jac_reduced = nalgebra::DMatrix::<f64>::zeros(active_rows.len(), active_cols.len());
    for (reduced_row, &full_row) in active_rows.iter().enumerate() {
        for (reduced_col, &full_col) in active_cols.iter().enumerate() {
            jac_reduced[(reduced_row, reduced_col)] = jac[(full_row, full_col)];
        }
    }
    let rhs_reduced = nalgebra::DVector::from_iterator(
        active_rows.len(),
        active_rows.iter().map(|&full_row| rhs[full_row]),
    );

    if active_rows.len() == active_cols.len() {
        let (delta_reduced, method) =
            solve_newton_linear_system_square(&jac_reduced, &rhs_reduced)?;
        let mut delta_full = nalgebra::DVector::<f64>::zeros(n_total);
        for (reduced_col, &full_col) in active_cols.iter().enumerate() {
            delta_full[full_col] = delta_reduced[reduced_col];
        }
        return Some((delta_full, method));
    }

    let scale = jac_reduced
        .iter()
        .fold(0.0_f64, |max_abs, value| max_abs.max(value.abs()));
    let eps = (scale.max(1.0)) * 1.0e-12;
    let pinv = jac_reduced.svd(true, true).pseudo_inverse(eps).ok()?;
    let delta_reduced = pinv * rhs_reduced;
    if !delta_reduced.iter().all(|value| value.is_finite()) {
        return None;
    }

    let mut delta_full = nalgebra::DVector::<f64>::zeros(n_total);
    for (reduced_col, &full_col) in active_cols.iter().enumerate() {
        delta_full[full_col] = delta_reduced[reduced_col];
    }
    Some((
        delta_full,
        NewtonLinearSolveMethod::LeastSquaresPseudoInverse,
    ))
}

#[cfg(test)]
mod reduced_linear_solve_tests;

fn free_residual_inf(rhs: &[f64], _n_x: usize, ignored_rows: &[bool]) -> f64 {
    rhs.iter()
        .enumerate()
        .filter(|(idx, _)| !ignored_rows.get(*idx).copied().unwrap_or(false))
        .map(|(_, value)| value.abs())
        .fold(
            0.0_f64,
            |a, b| if b.is_nan() { f64::INFINITY } else { a.max(b) },
        )
}

fn zero_ignored_residual_rows(rhs: &mut [f64], ignored_rows: &[bool]) {
    for (idx, value) in rhs.iter_mut().enumerate() {
        if ignored_rows.get(idx).copied().unwrap_or(false) {
            // Runtime projection can ignore residual rows independently from
            // fixed solver columns because equation ordering may diverge from
            // solver-vector ordering after DAE lowering/reordering.
            *value = 0.0;
        }
    }
}

fn newton_init_step(
    iter: usize,
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    config: &NewtonInitConfig<'_>,
) -> Result<Option<f64>, crate::SimError> {
    newton_init_step_with_cached_jacobian(iter, dae, y, p, config, None, false)
        .map(|result| result.map(|(free_norm_after, _)| free_norm_after))
}

fn build_newton_rhs_vector(rhs: &[f64]) -> nalgebra::DVector<f64> {
    let mut out = nalgebra::DVector::zeros(rhs.len());
    fill_newton_rhs_vector(rhs, &mut out);
    out
}

fn fill_newton_rhs_vector(rhs: &[f64], out: &mut nalgebra::DVector<f64>) {
    if out.len() != rhs.len() {
        *out = nalgebra::DVector::zeros(rhs.len());
    }
    for (dst, src) in out.iter_mut().zip(rhs.iter()) {
        *dst = clamp_finite(-*src);
    }
}

#[derive(Default)]
pub(crate) struct RuntimeProjectionScratch {
    rhs: Vec<f64>,
    rhs_after: Vec<f64>,
    rhs_vec: nalgebra::DVector<f64>,
}

impl RuntimeProjectionScratch {
    fn rhs(&mut self, len: usize) -> &mut [f64] {
        ensure_runtime_projection_vec_len(&mut self.rhs, len);
        &mut self.rhs
    }

    fn rhs_after(&mut self, len: usize) -> &mut [f64] {
        ensure_runtime_projection_vec_len(&mut self.rhs_after, len);
        &mut self.rhs_after
    }

    fn refill_rhs_vector(&mut self) -> &nalgebra::DVector<f64> {
        fill_newton_rhs_vector(&self.rhs, &mut self.rhs_vec);
        &self.rhs_vec
    }
}

fn ensure_runtime_projection_vec_len(buf: &mut Vec<f64>, len: usize) {
    if buf.len() != len {
        buf.resize(len, 0.0);
    }
}

fn solve_newton_delta_with_cached_jacobian(
    iter: usize,
    dae: &Dae,
    rhs: &[f64],
    jac_ctx: &InitJacobianEvalContext<'_>,
    config: &NewtonInitConfig<'_>,
    cached_jac: Option<&nalgebra::DMatrix<f64>>,
    cache_built_jac: bool,
) -> Result<
    Option<(
        nalgebra::DVector<f64>,
        NewtonLinearSolveMethod,
        CachedNewtonJacobian,
    )>,
    crate::SimError,
> {
    let r_vec = build_newton_rhs_vector(rhs);
    solve_newton_delta_with_cached_jacobian_from_r_vec(
        iter,
        NewtonLinearSolveContext {
            dae,
            rhs_for_diagnostics: Some(rhs),
            jac_ctx,
            config,
        },
        &r_vec,
        cached_jac,
        cache_built_jac,
    )
}

struct NewtonLinearSolveContext<'a> {
    dae: &'a Dae,
    rhs_for_diagnostics: Option<&'a [f64]>,
    jac_ctx: &'a InitJacobianEvalContext<'a>,
    config: &'a NewtonInitConfig<'a>,
}

fn solve_newton_delta_with_cached_jacobian_from_r_vec(
    iter: usize,
    solve_ctx: NewtonLinearSolveContext<'_>,
    r_vec: &nalgebra::DVector<f64>,
    cached_jac: Option<&nalgebra::DMatrix<f64>>,
    cache_built_jac: bool,
) -> Result<
    Option<(
        nalgebra::DVector<f64>,
        NewtonLinearSolveMethod,
        CachedNewtonJacobian,
    )>,
    crate::SimError,
> {
    let NewtonLinearSolveContext {
        dae,
        rhs_for_diagnostics,
        jac_ctx,
        config,
    } = solve_ctx;
    let mut built_jac = None;
    let mut delta = if let Some(cached) = cached_jac {
        solve_newton_linear_system_reduced(cached, r_vec, config.ignored_rows, config.fixed_cols)
    } else {
        built_jac = Some(build_init_jacobian(
            jac_ctx,
            config.fixed_cols,
            config.runtime_fd_jac_cols,
            config.runtime_seed_env.as_ref(),
            config.ignored_rows,
            config.homotopy_lambda,
            config.timeout,
        )?);
        solve_newton_linear_system_reduced(
            built_jac
                .as_ref()
                .expect("fresh Newton Jacobian must exist before solve"),
            r_vec,
            config.ignored_rows,
            config.fixed_cols,
        )
    };
    if delta.is_none() {
        built_jac = Some(build_init_jacobian(
            jac_ctx,
            config.fixed_cols,
            config.runtime_fd_jac_cols,
            config.runtime_seed_env.as_ref(),
            config.ignored_rows,
            config.homotopy_lambda,
            config.timeout,
        )?);
        delta = solve_newton_linear_system_reduced(
            built_jac
                .as_ref()
                .expect("fresh Newton Jacobian must exist after cached fallback"),
            r_vec,
            config.ignored_rows,
            config.fixed_cols,
        )
    }
    let jac_for_diagnostics = built_jac
        .as_ref()
        .or(cached_jac)
        .expect("Newton Jacobian must be available before solve");
    if sim_introspect_enabled() && !config.use_initial && iter == 0 {
        if let Some(rhs) = rhs_for_diagnostics {
            log_init_linear_system_diagnostics(dae, jac_for_diagnostics, rhs, config.n_x);
        } else {
            let fallback_rhs = r_vec.iter().map(|v| clamp_finite(-*v)).collect::<Vec<_>>();
            log_init_linear_system_diagnostics(dae, jac_for_diagnostics, &fallback_rhs, config.n_x);
        }
    }
    let Some((delta, solve_method)) = delta else {
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] IC Newton iter={} singular Jacobian (failed linear solve)",
                iter
            );
        }
        if let Some(rhs) = rhs_for_diagnostics {
            log_init_linear_system_diagnostics(dae, jac_for_diagnostics, rhs, config.n_x);
        } else {
            let fallback_rhs = r_vec.iter().map(|v| clamp_finite(-*v)).collect::<Vec<_>>();
            log_init_linear_system_diagnostics(dae, jac_for_diagnostics, &fallback_rhs, config.n_x);
        }
        return Ok(None);
    };
    let jac_to_cache = if cache_built_jac { built_jac } else { None };
    Ok(Some((delta, solve_method, jac_to_cache)))
}

struct NewtonDeltaApplyContext<'a> {
    dae: &'a Dae,
    p: &'a [f64],
    eval_mode: IcEvalMode<'a>,
    ignored_rows: &'a [bool],
    free_norm_before: f64,
}

fn apply_newton_delta_and_measure_residual(
    iter: usize,
    ctx: &NewtonDeltaApplyContext<'_>,
    y: &mut [f64],
    delta: nalgebra::DVector<f64>,
    solve_method: NewtonLinearSolveMethod,
) -> Result<f64, crate::SimError> {
    let mut rhs_after = vec![0.0; y.len()];
    apply_newton_delta_and_measure_residual_into(iter, ctx, y, delta, solve_method, &mut rhs_after)
}

fn apply_newton_delta_and_measure_residual_into(
    iter: usize,
    ctx: &NewtonDeltaApplyContext<'_>,
    y: &mut [f64],
    mut delta: nalgebra::DVector<f64>,
    solve_method: NewtonLinearSolveMethod,
    rhs_after: &mut [f64],
) -> Result<f64, crate::SimError> {
    if sim_trace_enabled() && solve_method != NewtonLinearSolveMethod::Lu {
        eprintln!(
            "[sim-trace] IC Newton iter={} non-LU linear solve method={:?}",
            iter, solve_method
        );
    }

    if sim_trace_enabled() {
        let delta_inf = delta.iter().map(|v| v.abs()).fold(0.0_f64, f64::max);
        eprintln!(
            "[sim-trace] IC Newton iter={} delta_inf={}",
            iter, delta_inf
        );
    }

    if !ctx.eval_mode.use_initial {
        let delta_inf = delta.iter().map(|v| v.abs()).fold(0.0_f64, f64::max);
        let y_inf = y.iter().map(|v| v.abs()).fold(0.0_f64, f64::max);
        let max_step = 10.0 * (1.0 + y_inf);
        if delta_inf.is_finite() && delta_inf > max_step && max_step > 0.0 {
            let scale = max_step / delta_inf;
            for value in delta.iter_mut() {
                *value *= scale;
            }
            if sim_trace_enabled() {
                eprintln!(
                    "[sim-trace] IC Newton iter={} runtime trust-region scale={} delta_inf={} max_step={}",
                    iter, scale, delta_inf, max_step
                );
            }
        }
    }

    for i in 0..y.len() {
        if delta[i].is_finite() {
            y[i] += delta[i];
        }
    }

    eval_ic_rhs_at_time(ctx.dae, y, ctx.p, &ctx.eval_mode, rhs_after);
    let free_norm_after = free_residual_inf(rhs_after, ctx.eval_mode.n_x, ctx.ignored_rows);
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC Newton iter={} residual_inf(before)={} residual_inf(after)={}",
            iter, ctx.free_norm_before, free_norm_after
        );
    }
    Ok(free_norm_after)
}

fn newton_init_step_with_cached_jacobian(
    iter: usize,
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    config: &NewtonInitConfig<'_>,
    cached_jac: Option<&nalgebra::DMatrix<f64>>,
    cache_built_jac: bool,
) -> NewtonStepOutcome {
    config.timeout.check()?;
    let n_total = y.len();
    let eval_mode = IcEvalMode {
        compiled_initial: config.compiled_initial,
        compiled_runtime: config.compiled_runtime,
        runtime_seed_env: config.runtime_seed_env.clone(),
        use_initial: config.use_initial,
        is_initial_phase: config.is_initial_phase,
        homotopy_lambda: config.homotopy_lambda,
        t_eval: config.t_eval,
        n_x: config.n_x,
        ignored_rows: Some(config.ignored_rows),
    };
    let mut rhs = vec![0.0; n_total];
    eval_ic_rhs_at_time(dae, y, p, &eval_mode, &mut rhs);
    config.timeout.check()?;

    let free_norm_before = free_residual_inf(&rhs, config.n_x, config.ignored_rows);
    zero_ignored_residual_rows(&mut rhs, config.ignored_rows);

    let jac_ctx = InitJacobianEvalContext {
        dae,
        y,
        p,
        t_eval: config.t_eval,
        n_x: config.n_x,
        use_initial: config.use_initial,
        compiled_initial: config.compiled_initial,
        compiled_runtime: config.compiled_runtime,
    };
    let Some((delta, solve_method, jac_to_cache)) = solve_newton_delta_with_cached_jacobian(
        iter,
        dae,
        &rhs,
        &jac_ctx,
        config,
        cached_jac,
        cache_built_jac,
    )?
    else {
        return Ok(None);
    };
    config.timeout.check()?;
    let delta_apply_ctx = NewtonDeltaApplyContext {
        dae,
        p,
        eval_mode,
        ignored_rows: config.ignored_rows,
        free_norm_before,
    };
    let free_norm_after =
        apply_newton_delta_and_measure_residual(iter, &delta_apply_ctx, y, delta, solve_method)?;
    Ok(Some((free_norm_after, jac_to_cache)))
}

struct CachedNewtonStep<'a> {
    iter: usize,
    cached_jac: Option<&'a nalgebra::DMatrix<f64>>,
    cache_built_jac: bool,
}

fn newton_init_step_with_cached_jacobian_in_place(
    step: CachedNewtonStep<'_>,
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    config: &NewtonInitConfig<'_>,
    scratch: &mut RuntimeProjectionScratch,
) -> NewtonStepOutcome {
    config.timeout.check()?;
    let n_total = y.len();
    let eval_mode = IcEvalMode {
        compiled_initial: config.compiled_initial,
        compiled_runtime: config.compiled_runtime,
        runtime_seed_env: config.runtime_seed_env.clone(),
        use_initial: config.use_initial,
        is_initial_phase: config.is_initial_phase,
        homotopy_lambda: config.homotopy_lambda,
        t_eval: config.t_eval,
        n_x: config.n_x,
        ignored_rows: Some(config.ignored_rows),
    };
    {
        let rhs = scratch.rhs(n_total);
        eval_ic_rhs_at_time(dae, y, p, &eval_mode, rhs);
    }
    config.timeout.check()?;

    let free_norm_before = free_residual_inf(&scratch.rhs, config.n_x, config.ignored_rows);
    zero_ignored_residual_rows(&mut scratch.rhs, config.ignored_rows);

    let jac_ctx = InitJacobianEvalContext {
        dae,
        y,
        p,
        t_eval: config.t_eval,
        n_x: config.n_x,
        use_initial: config.use_initial,
        compiled_initial: config.compiled_initial,
        compiled_runtime: config.compiled_runtime,
    };
    let Some((delta, solve_method, jac_to_cache)) =
        solve_newton_delta_with_cached_jacobian_from_r_vec(
            step.iter,
            NewtonLinearSolveContext {
                dae,
                rhs_for_diagnostics: None,
                jac_ctx: &jac_ctx,
                config,
            },
            scratch.refill_rhs_vector(),
            step.cached_jac,
            step.cache_built_jac,
        )?
    else {
        return Ok(None);
    };
    config.timeout.check()?;
    let delta_apply_ctx = NewtonDeltaApplyContext {
        dae,
        p,
        eval_mode,
        ignored_rows: config.ignored_rows,
        free_norm_before,
    };
    let free_norm_after = apply_newton_delta_and_measure_residual_into(
        step.iter,
        &delta_apply_ctx,
        y,
        delta,
        solve_method,
        scratch.rhs_after(n_total),
    )?;
    Ok(Some((free_norm_after, jac_to_cache)))
}

fn newton_init_step_from_current_rhs_in_place(
    step: CachedNewtonStep<'_>,
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    config: &NewtonInitConfig<'_>,
    free_norm_before: f64,
    scratch: &mut RuntimeProjectionScratch,
) -> NewtonStepOutcome {
    zero_ignored_residual_rows(&mut scratch.rhs, config.ignored_rows);
    let eval_mode = IcEvalMode {
        compiled_initial: config.compiled_initial,
        compiled_runtime: config.compiled_runtime,
        runtime_seed_env: config.runtime_seed_env.clone(),
        use_initial: config.use_initial,
        is_initial_phase: config.is_initial_phase,
        homotopy_lambda: config.homotopy_lambda,
        t_eval: config.t_eval,
        n_x: config.n_x,
        ignored_rows: Some(config.ignored_rows),
    };
    let jac_ctx = InitJacobianEvalContext {
        dae,
        y,
        p,
        t_eval: config.t_eval,
        n_x: config.n_x,
        use_initial: config.use_initial,
        compiled_initial: config.compiled_initial,
        compiled_runtime: config.compiled_runtime,
    };
    let Some((delta, solve_method, jac_to_cache)) =
        solve_newton_delta_with_cached_jacobian_from_r_vec(
            step.iter,
            NewtonLinearSolveContext {
                dae,
                rhs_for_diagnostics: None,
                jac_ctx: &jac_ctx,
                config,
            },
            scratch.refill_rhs_vector(),
            step.cached_jac,
            step.cache_built_jac,
        )?
    else {
        return Ok(None);
    };
    config.timeout.check()?;
    let delta_apply_ctx = NewtonDeltaApplyContext {
        dae,
        p,
        eval_mode,
        ignored_rows: config.ignored_rows,
        free_norm_before,
    };
    let free_norm_after = apply_newton_delta_and_measure_residual_into(
        step.iter,
        &delta_apply_ctx,
        y,
        delta,
        solve_method,
        scratch.rhs_after(y.len()),
    )?;
    Ok(Some((free_norm_after, jac_to_cache)))
}

fn equations_use_initial(dae: &Dae) -> bool {
    !dae.initial_equations.is_empty() || dae.f_x.iter().any(|eq| expr_contains_initial(&eq.rhs))
}

fn equations_use_homotopy(dae: &Dae) -> bool {
    dae.f_x.iter().any(|eq| expr_contains_homotopy(&eq.rhs))
        || dae
            .initial_equations
            .iter()
            .any(|eq| expr_contains_homotopy(&eq.rhs))
}

fn should_write_partial_ic_solution(
    best_r_inf: f64,
    first_r_inf: Option<f64>,
    tol: f64,
    best_y: &[f64],
) -> bool {
    if !best_r_inf.is_finite() {
        return false;
    }
    if best_y.iter().any(|v| !v.is_finite() || v.abs() > 1e12) {
        return false;
    }
    if best_r_inf <= tol * 100.0 {
        return true;
    }
    if let Some(first) = first_r_inf
        && first.is_finite()
        && best_r_inf < first * 0.5
        && best_r_inf < 10.0
    {
        return true;
    }
    false
}

fn should_write_seeded_ic_solution(seeded_updates: usize, seeded_y: &[f64]) -> bool {
    seeded_updates > 0 && seeded_y.iter().all(|v| v.is_finite() && v.abs() <= 1.0e12)
}

fn trace_ic_newton_stagnation(iter: usize, r_inf: f64, prev_r_inf: f64) {
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC Newton stopping early due to stagnation: iter={} residual_inf={} prev_residual_inf={}",
            iter, r_inf, prev_r_inf
        );
    }
}

fn non_finite_initial_residual_error(before: f64, after: f64) -> crate::SimError {
    crate::SimError::SolverError(format!(
        "initial-condition residual is non-finite at t=0 (before perturbation={before}, \
         after perturbation={after}); aborting startup"
    ))
}

fn ic_non_finite_row_diagnostics_enabled() -> bool {
    sim_trace_enabled() || sim_introspect_enabled()
}

fn collect_non_finite_params(dae: &Dae, eval_env: &VarEnv<f64>) -> Vec<(String, f64)> {
    let mut params = Vec::new();
    for name in dae.parameters.keys() {
        if let Some(value) = eval_env.vars.get(name.as_str()).copied()
            && !value.is_finite()
        {
            params.push((name.as_str().to_string(), value));
        }
    }
    params.sort_by(|a, b| a.0.cmp(&b.0));
    params
}

fn log_non_finite_params(params: &[(String, f64)]) {
    if params.is_empty() {
        return;
    }
    eprintln!(
        "[sim-trace] IC parameter non-finite values count={}",
        params.len()
    );
    for (name, value) in params.iter().take(16) {
        eprintln!("[sim-trace]   non-finite parameter {} = {}", name, value);
    }
    if params.len() > 16 {
        eprintln!(
            "[sim-trace]   ... omitted {} additional non-finite parameters",
            params.len() - 16
        );
    }
}

fn residual_row_details(
    dae: &Dae,
    eval_env: &VarEnv<f64>,
    idx: usize,
) -> (String, String, String, String) {
    let Some(eq) = dae.f_x.get(idx) else {
        return (
            "<missing-eq>".to_string(),
            "<missing-rhs>".to_string(),
            "<none>".to_string(),
            "<none>".to_string(),
        );
    };

    let mut refs = std::collections::HashSet::new();
    eq.rhs.collect_var_refs(&mut refs);
    let mut refs: Vec<String> = refs
        .into_iter()
        .map(|name| name.as_str().to_string())
        .collect();
    refs.sort();

    let ref_values_preview = if refs.is_empty() {
        "<none>".to_string()
    } else {
        refs.iter()
            .take(8)
            .map(|name| format_ref_value_preview(eval_env, name))
            .collect::<Vec<_>>()
            .join(", ")
    };
    refs.truncate(8);
    let refs_preview = if refs.is_empty() {
        "<none>".to_string()
    } else {
        refs.join(", ")
    };

    (
        eq.origin.clone(),
        format!("{:?}", eq.rhs),
        refs_preview,
        ref_values_preview,
    )
}

fn build_initial_eval_env_preserving_pre_values(
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    t_eval: f64,
) -> Result<VarEnv<f64>, crate::SimError> {
    // MLS §8.6: initialization diagnostics/start persistence must see the
    // same initial-section fixed point as `initial()`/`pre(...)` startup
    // seeding, without leaving diagnostic-only mutations in the global pre
    // cache.
    let pre_snapshot = rumoca_eval_dae::runtime::snapshot_pre_values();
    let mut startup_y = y.to_vec();
    let env = crate::runtime::startup::build_initial_section_env_strict(
        dae,
        startup_y.as_mut_slice(),
        p,
        t_eval,
    )
    .map_err(crate::SimError::SolverError);
    rumoca_eval_dae::runtime::restore_pre_values(pre_snapshot);
    env
}

fn build_runtime_eval_env_preserving_pre_values(
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    n_x: usize,
    t_eval: f64,
    ignored_rows: Option<&[bool]>,
    runtime_seed_env: Option<&VarEnv<f64>>,
) -> Result<VarEnv<f64>, crate::SimError> {
    let pre_snapshot = rumoca_eval_dae::runtime::snapshot_pre_values();
    let mut y_work = y.to_vec();
    let mut env = runtime_seed_env
        .cloned()
        .unwrap_or_else(|| crate::runtime::event::build_runtime_state_env(dae, y, p, t_eval));
    rumoca_eval_dae::runtime::refresh_env_solver_and_parameter_values(&mut env, dae, y, p, t_eval);
    if !runtime_projection_needs_settled_discrete_env(dae, ignored_rows) {
        return Ok(env);
    }

    let use_frozen_pre = crate::runtime::no_state::no_state_requires_frozen_event_pre_values(dae);
    let mut guard_env: Option<VarEnv<f64>> = None;
    let settled = crate::runtime::event::settle_runtime_event_updates_with_base_env(
        crate::EventSettleInput {
            dae,
            y: &mut y_work,
            p,
            n_x,
            t_eval,
            is_initial: false,
        },
        env,
        crate::runtime::assignment::propagate_runtime_direct_assignments_from_env,
        crate::runtime::alias::propagate_runtime_alias_components_from_env,
        |dae, env| {
            let guard_env = guard_env.get_or_insert_with(|| env.clone());
            crate::runtime::discrete::apply_discrete_partition_updates_with_guard_env_and_scalar_override(
                dae,
                env,
                guard_env,
                |_eq, _target, _solution, _env, _implicit_clock_active| None,
            )
        },
        crate::runtime::layout::sync_solver_values_from_env,
        !use_frozen_pre,
    );
    rumoca_eval_dae::runtime::restore_pre_values(pre_snapshot);
    Ok(settled)
}

fn runtime_projection_needs_settled_discrete_env(dae: &Dae, ignored_rows: Option<&[bool]>) -> bool {
    !(dae.f_z.is_empty() && dae.f_m.is_empty())
        && dae.f_x.iter().enumerate().any(|(idx, eq)| {
            !ignored_rows
                .and_then(|rows| rows.get(idx))
                .copied()
                .unwrap_or(false)
                && crate::runtime::no_state::expr_reads_event_updated_discrete_var(dae, &eq.rhs)
        })
}

#[cfg(test)]
mod runtime_projection_gate_tests;

fn build_ic_eval_env(
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    mode: &IcEvalMode<'_>,
) -> Result<VarEnv<f64>, crate::SimError> {
    let mut env = if mode.use_initial {
        build_initial_eval_env_preserving_pre_values(dae, y, p, mode.t_eval)
    } else {
        // MLS §8.6: after initialization solve, ordinary event iteration must
        // repeatedly re-evaluate normal equations with v -> pre(v) updates
        // until discrete variables reach a fixed point. Runtime projection
        // residuals therefore need a locally settled discrete environment.
        build_runtime_eval_env_preserving_pre_values(
            dae,
            y,
            p,
            mode.n_x,
            mode.t_eval,
            mode.ignored_rows,
            mode.runtime_seed_env.as_ref(),
        )
    }?;
    env.is_initial = mode.is_initial_phase;
    if mode.is_initial_phase {
        env.set(
            rumoca_eval_dae::runtime::INIT_HOMOTOPY_LAMBDA_KEY,
            mode.homotopy_lambda,
        );
    }
    Ok(env)
}

fn log_non_finite_ic_residual_rows(
    dae: &Dae,
    rhs: &[f64],
    y: &[f64],
    p: &[f64],
    n_x: usize,
    use_initial: bool,
    phase: &str,
) {
    if !ic_non_finite_row_diagnostics_enabled() {
        return;
    }
    let eval_mode = IcEvalMode {
        compiled_initial: None,
        compiled_runtime: None,
        runtime_seed_env: None,
        use_initial,
        is_initial_phase: true,
        homotopy_lambda: 1.0,
        t_eval: 0.0,
        n_x,
        ignored_rows: None,
    };
    let Ok(eval_env) = build_ic_eval_env(dae, y, p, &eval_mode) else {
        eprintln!(
            "[sim-trace] IC non-finite residual diagnostics skipped: failed to build {} eval env",
            if use_initial { "initial" } else { "runtime" }
        );
        return;
    };
    let non_finite_params = collect_non_finite_params(dae, &eval_env);
    log_non_finite_params(&non_finite_params);
    let mut rows: Vec<(usize, f64, bool)> = rhs
        .iter()
        .copied()
        .enumerate()
        .map(|(idx, value)| (idx, value.abs(), !value.is_finite()))
        .collect();
    rows.sort_by(|a, b| {
        b.1.partial_cmp(&a.1)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then_with(|| a.0.cmp(&b.0))
    });
    let non_finite_count = rows.iter().filter(|entry| entry.2).count();
    eprintln!(
        "[sim-trace] IC non-finite residual diagnostics phase={} n_eq={} non_finite_rows={}",
        phase,
        rhs.len(),
        non_finite_count
    );
    for (rank, (idx, abs_value, non_finite)) in rows.into_iter().take(10).enumerate() {
        let value = rhs.get(idx).copied().unwrap_or(0.0);
        let (origin, rhs_expr, refs_preview, ref_values_preview) =
            residual_row_details(dae, &eval_env, idx);
        eprintln!(
            "[sim-trace]   IC residual rank={} eq=f_x[{}] value={} abs={} non_finite={} origin='{}' refs={} ref_values={} rhs={}",
            rank,
            idx,
            value,
            abs_value,
            non_finite,
            origin,
            refs_preview,
            ref_values_preview,
            rhs_expr
        );
    }
}

fn format_ref_value_preview(eval_env: &VarEnv<f64>, name: &str) -> String {
    eval_env
        .vars
        .get(name)
        .map(|value| format!("{name}={value}"))
        .unwrap_or_else(|| format!("{name}=<missing>"))
}

fn expr_contains_initial(expr: &Expression) -> bool {
    match expr {
        Expression::BuiltinCall { function, args } => {
            if *function == BuiltinFunction::Initial {
                return true;
            }
            args.iter().any(expr_contains_initial)
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_initial(lhs) || expr_contains_initial(rhs)
        }
        Expression::Unary { rhs, .. } => expr_contains_initial(rhs),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches
                .iter()
                .any(|(c, e)| expr_contains_initial(c) || expr_contains_initial(e))
                || expr_contains_initial(else_branch)
        }
        Expression::FunctionCall { args, .. } => args.iter().any(expr_contains_initial),
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().any(expr_contains_initial)
        }
        Expression::Index { base, .. } => expr_contains_initial(base),
        _ => false,
    }
}

fn expr_contains_homotopy(expr: &Expression) -> bool {
    match expr {
        Expression::BuiltinCall { function, args } => {
            if *function == BuiltinFunction::Homotopy {
                return true;
            }
            args.iter().any(expr_contains_homotopy)
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_homotopy(lhs) || expr_contains_homotopy(rhs)
        }
        Expression::Unary { rhs, .. } => expr_contains_homotopy(rhs),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches
                .iter()
                .any(|(c, e)| expr_contains_homotopy(c) || expr_contains_homotopy(e))
                || expr_contains_homotopy(else_branch)
        }
        Expression::FunctionCall { args, .. } => args.iter().any(expr_contains_homotopy),
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().any(expr_contains_homotopy)
        }
        Expression::Index { base, .. } => expr_contains_homotopy(base),
        _ => false,
    }
}

#[derive(Clone)]
struct IcEvalMode<'a> {
    compiled_initial: Option<&'a CompiledInitialNewtonContext>,
    compiled_runtime: Option<&'a CompiledRuntimeNewtonContext>,
    runtime_seed_env: Option<VarEnv<f64>>,
    use_initial: bool,
    is_initial_phase: bool,
    homotopy_lambda: f64,
    t_eval: f64,
    n_x: usize,
    ignored_rows: Option<&'a [bool]>,
}

fn eval_runtime_ic_residual(
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    mode: &IcEvalMode<'_>,
    out: &mut [f64],
) {
    let eval_env = build_ic_eval_env(dae, y, p, mode)
        .unwrap_or_else(|err| panic!("runtime eval env required for residual evaluation: {err}"));
    for (slot, eq) in out.iter_mut().zip(dae.f_x.iter()) {
        *slot = rumoca_eval_dae::runtime::eval_expr::<f64>(&eq.rhs, &eval_env);
    }
}

fn expr_needs_runtime_residual_eval(expr: &Expression) -> bool {
    match expr {
        // MLS §3.3 / §3.7.5: noEvent/smooth preserve value semantics while
        // suppressing event generation. Keep runtime projection on the scalar
        // evaluator for those branch-sensitive rows until the compiled runtime
        // residual path proves equivalent on the op-amp limiter family.
        Expression::BuiltinCall { function, args } => {
            matches!(function, BuiltinFunction::NoEvent | BuiltinFunction::Smooth)
                || args.iter().any(expr_needs_runtime_residual_eval)
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_needs_runtime_residual_eval(lhs) || expr_needs_runtime_residual_eval(rhs)
        }
        Expression::Unary { rhs, .. } | Expression::FieldAccess { base: rhs, .. } => {
            expr_needs_runtime_residual_eval(rhs)
        }
        Expression::FunctionCall { args, .. } => args.iter().any(expr_needs_runtime_residual_eval),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_needs_runtime_residual_eval(cond) || expr_needs_runtime_residual_eval(value)
            }) || expr_needs_runtime_residual_eval(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().any(expr_needs_runtime_residual_eval)
        }
        Expression::Range { start, step, end } => {
            expr_needs_runtime_residual_eval(start)
                || step
                    .as_deref()
                    .is_some_and(expr_needs_runtime_residual_eval)
                || expr_needs_runtime_residual_eval(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_needs_runtime_residual_eval(expr)
                || indices
                    .iter()
                    .any(|index| expr_needs_runtime_residual_eval(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_needs_runtime_residual_eval)
        }
        Expression::Index { base, subscripts } => {
            expr_needs_runtime_residual_eval(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_needs_runtime_residual_eval(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => false,
    }
}

fn dae_needs_runtime_residual_eval(dae: &Dae) -> bool {
    dae.f_x
        .iter()
        .any(|eq| expr_needs_runtime_residual_eval(&eq.rhs))
}

fn eval_ic_rhs_at_time(dae: &Dae, y: &[f64], p: &[f64], mode: &IcEvalMode<'_>, out: &mut [f64]) {
    if mode.use_initial
        && (mode.homotopy_lambda - 1.0).abs() <= f64::EPSILON
        && let Some(compiled_initial) = mode.compiled_initial
    {
        // MLS §8.6: initial() is true during initialization, so the IC
        // residual stays on the initial-mode compiled kernel whenever the
        // residual is evaluating the final actual system.
        eval_compiled_initial_residual(compiled_initial, y, p, mode.t_eval, out);
    } else {
        if let Some(compiled_runtime) = mode.compiled_runtime
            && !mode.is_initial_phase
            && mode.runtime_seed_env.is_none()
            && !dae_needs_runtime_residual_eval(dae)
        {
            eval_compiled_runtime_residual(compiled_runtime, y, p, mode.t_eval, out);
        } else {
            eval_runtime_ic_residual(dae, y, p, mode, out);
        }
    }
}

fn build_initial_newton_context_if_needed(
    dae: &Dae,
    n_total: usize,
    use_initial: bool,
) -> Result<Option<CompiledInitialNewtonContext>, crate::SimError> {
    use_initial
        .then(|| build_compiled_initial_newton_context(dae, n_total))
        .transpose()
}

fn build_runtime_newton_context_if_needed(
    dae: &Dae,
    n_total: usize,
    use_initial: bool,
) -> Result<Option<CompiledRuntimeNewtonContext>, crate::SimError> {
    (!use_initial)
        .then(|| build_compiled_runtime_newton_context(dae, n_total))
        .transpose()
}

struct IcResidualContext<'a> {
    eval_mode: IcEvalMode<'a>,
    ignored_rows: &'a [bool],
    timeout: &'a crate::TimeoutBudget,
}

fn ensure_perturbed_residual_is_finite(
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    ctx: &IcResidualContext<'_>,
    initial_free_norm: f64,
) -> Result<(), crate::SimError> {
    if initial_free_norm.is_finite() {
        return Ok(());
    }

    ctx.timeout.check()?;
    let mut rhs_perturbed = vec![0.0; dae.f_x.len()];
    eval_ic_rhs_at_time(dae, y, p, &ctx.eval_mode, &mut rhs_perturbed);
    let perturbed_free_norm =
        free_residual_inf(&rhs_perturbed, ctx.eval_mode.n_x, ctx.ignored_rows);
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC residual_inf(after-perturb)={}",
            perturbed_free_norm
        );
    }
    if !perturbed_free_norm.is_finite() {
        log_non_finite_ic_residual_rows(
            dae,
            &rhs_perturbed,
            y,
            p,
            ctx.eval_mode.n_x,
            ctx.eval_mode.use_initial,
            "after_perturb",
        );
        return Err(non_finite_initial_residual_error(
            initial_free_norm,
            perturbed_free_norm,
        ));
    }
    Ok(())
}

struct IcFinalizeState<'a> {
    p: &'a [f64],
    n_x: usize,
    fixed: &'a [bool],
    tol: f64,
    best_r_inf: f64,
    first_r_inf: Option<f64>,
    best_y: &'a [f64],
    seeded_updates: usize,
    seeded_y: &'a [f64],
}

fn finalize_best_or_seeded_solution(
    dae: &mut Dae,
    state: &IcFinalizeState<'_>,
) -> Result<(), crate::SimError> {
    let mut wrote_solution = false;
    if should_write_partial_ic_solution(
        state.best_r_inf,
        state.first_r_inf,
        state.tol,
        state.best_y,
    ) {
        finalize_initial_solution(dae, state.best_y, state.p, state.n_x, state.fixed, 0.0)?;
        wrote_solution = true;
    }
    if !wrote_solution && should_write_seeded_ic_solution(state.seeded_updates, state.seeded_y) {
        finalize_initial_solution(dae, state.seeded_y, state.p, state.n_x, state.fixed, 0.0)?;
    }
    Ok(())
}

struct IcNewtonContext<'a> {
    p: &'a [f64],
    newton_config: &'a NewtonInitConfig<'a>,
    tol: f64,
    timeout: &'a crate::TimeoutBudget,
    n_x: usize,
    fixed: &'a [bool],
    seeded_updates: usize,
    seeded_y: &'a [f64],
    initial_free_norm: f64,
}

fn write_var_start(var: &mut rumoca_ir_dae::Variable, y: &[f64], idx: &mut usize) {
    let sz = var.size();
    if sz <= 1 {
        if *idx < y.len() {
            var.start = Some(Expression::Literal(rumoca_ir_dae::Literal::Real(y[*idx])));
        }
        *idx += 1;
    } else {
        let elements: Vec<Expression> = (0..sz)
            .map(|i| {
                let val = y.get(*idx + i).copied().unwrap_or(0.0);
                Expression::Literal(rumoca_ir_dae::Literal::Real(val))
            })
            .collect();
        var.start = Some(Expression::Array {
            elements,
            is_matrix: false,
        });
        *idx += sz;
    }
}

fn write_solved_ics(dae: &mut Dae, y: &[f64], n_x: usize, fixed: &[bool]) {
    let mut idx = 0;
    for (_name, var) in dae.states.iter_mut() {
        let sz = var.size();
        let is_fixed = idx < fixed.len() && fixed[idx];
        if !is_fixed {
            write_var_start(var, y, &mut idx);
        } else {
            idx += sz;
        }
    }

    debug_assert_eq!(idx, n_x);
    for (_name, var) in dae.algebraics.iter_mut() {
        write_var_start(var, y, &mut idx);
    }
    for (_name, var) in dae.outputs.iter_mut() {
        write_var_start(var, y, &mut idx);
    }
}

fn write_discrete_start_from_env(
    env: &VarEnv<f64>,
    name: &rumoca_ir_dae::VarName,
    var: &mut rumoca_ir_dae::Variable,
) -> bool {
    let key = name.as_str();
    let sz = var.size();
    if sz <= 1 {
        let Some(value) = env.vars.get(key).copied() else {
            return false;
        };
        var.start = Some(Expression::Literal(rumoca_ir_dae::Literal::Real(value)));
        return true;
    }

    let mut values = Vec::with_capacity(sz);
    for i in 0..sz {
        let indexed = format!("{key}[{}]", i + 1);
        let value = env
            .vars
            .get(indexed.as_str())
            .copied()
            .or_else(|| env.vars.get(key).copied())
            .unwrap_or(0.0);
        values.push(Expression::Literal(rumoca_ir_dae::Literal::Real(value)));
    }
    var.start = Some(Expression::Array {
        elements: values,
        is_matrix: false,
    });
    true
}

pub(super) fn persist_initial_section_discrete_starts(
    dae: &mut Dae,
    y: &[f64],
    p: &[f64],
    t_eval: f64,
) -> Result<usize, crate::SimError> {
    if dae.discrete_reals.is_empty() && dae.discrete_valued.is_empty() {
        return Ok(0);
    }

    let env = build_initial_eval_env_preserving_pre_values(dae, y, p, t_eval)?;

    let mut updates = 0usize;
    for (name, var) in &mut dae.discrete_reals {
        if write_discrete_start_from_env(&env, name, var) {
            updates += 1;
        }
    }
    for (name, var) in &mut dae.discrete_valued {
        if write_discrete_start_from_env(&env, name, var) {
            updates += 1;
        }
    }
    Ok(updates)
}

fn finalize_initial_solution(
    dae: &mut Dae,
    y: &[f64],
    p: &[f64],
    n_x: usize,
    fixed: &[bool],
    t_eval: f64,
) -> Result<(), crate::SimError> {
    write_solved_ics(dae, y, n_x, fixed);
    let discrete_updates = persist_initial_section_discrete_starts(dae, y, p, t_eval)?;
    if sim_trace_enabled() && discrete_updates > 0 {
        eprintln!(
            "[sim-trace] persisted initial-section discrete starts updates={}",
            discrete_updates
        );
    }
    Ok(())
}
