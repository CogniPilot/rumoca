use super::*;
fn build_init_jacobian(
    ctx: &InitJacobianEvalContext<'_>,
    fixed: &[bool],
    timeout: &crate::TimeoutBudget,
) -> Result<nalgebra::DMatrix<f64>, crate::SimError> {
    if !ctx.use_initial {
        return build_init_jacobian_dense(ctx, fixed, timeout);
    }
    if let Some(jac) = build_init_jacobian_colored(ctx, fixed, timeout)? {
        return Ok(jac);
    }
    if sim_trace_enabled() {
        eprintln!("[sim-trace] IC Jacobian coloring fallback -> dense assembly");
    }
    build_init_jacobian_dense(ctx, fixed, timeout)
}

struct NewtonInitConfig<'a> {
    n_x: usize,
    fixed: &'a [bool],
    use_initial: bool,
    t_eval: f64,
    timeout: &'a crate::TimeoutBudget,
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
    fixed: &[bool],
) -> Option<(nalgebra::DVector<f64>, NewtonLinearSolveMethod)> {
    let n_total = jac.ncols();
    let active_cols: Vec<usize> = (0..n_total)
        .filter(|&j| j >= fixed.len() || !fixed[j])
        .collect();

    if active_cols.is_empty() {
        return Some((
            nalgebra::DVector::zeros(n_total),
            NewtonLinearSolveMethod::LeastSquaresPseudoInverse,
        ));
    }
    if active_cols.len() == n_total {
        return solve_newton_linear_system_square(jac, rhs);
    }

    let mut jac_reduced = nalgebra::DMatrix::<f64>::zeros(jac.nrows(), active_cols.len());
    for (reduced_col, &full_col) in active_cols.iter().enumerate() {
        jac_reduced
            .column_mut(reduced_col)
            .copy_from(&jac.column(full_col));
    }

    let scale = jac_reduced
        .iter()
        .fold(0.0_f64, |max_abs, value| max_abs.max(value.abs()));
    let eps = (scale.max(1.0)) * 1.0e-12;
    let pinv = jac_reduced.svd(true, true).pseudo_inverse(eps).ok()?;
    let delta_reduced = pinv * rhs;
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

fn free_residual_inf(rhs: &[f64], _n_x: usize, _fixed: &[bool]) -> f64 {
    rhs.iter().map(|v| v.abs()).fold(
        0.0_f64,
        |a, b| if b.is_nan() { f64::INFINITY } else { a.max(b) },
    )
}

fn newton_init_step(
    iter: usize,
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    config: &NewtonInitConfig<'_>,
) -> Result<Option<f64>, crate::SimError> {
    config.timeout.check()?;
    let n_total = y.len();
    let mut rhs = vec![0.0; n_total];
    if config.use_initial {
        eval_rhs_equations_initial(dae, y, p, config.t_eval, &mut rhs, config.n_x);
    } else {
        eval_rhs_equations(dae, y, p, config.t_eval, &mut rhs, config.n_x);
    }
    config.timeout.check()?;

    let free_norm_before = free_residual_inf(&rhs, config.n_x, config.fixed);

    let jac_ctx = InitJacobianEvalContext {
        dae,
        y,
        p,
        t_eval: config.t_eval,
        n_x: config.n_x,
        use_initial: config.use_initial,
    };
    let jac = build_init_jacobian(&jac_ctx, config.fixed, config.timeout)?;
    if sim_introspect_enabled() && !config.use_initial && iter == 0 {
        log_init_linear_system_diagnostics(dae, &jac, &rhs, config.n_x);
    }
    let neg_r: Vec<f64> = rhs.iter().map(|v| clamp_finite(-v)).collect();
    let r_vec = nalgebra::DVector::from_vec(neg_r);
    let delta = solve_newton_linear_system_reduced(&jac, &r_vec, config.fixed);
    config.timeout.check()?;
    let Some((mut delta, solve_method)) = delta else {
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] IC Newton iter={} singular Jacobian (failed linear solve)",
                iter
            );
        }
        log_init_linear_system_diagnostics(dae, &jac, &rhs, config.n_x);
        return Ok(None);
    };
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

    if !config.use_initial {
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

    for i in 0..n_total {
        if delta[i].is_finite() {
            y[i] += delta[i];
        }
    }

    config.timeout.check()?;
    let mut rhs_after = vec![0.0; n_total];
    if config.use_initial {
        eval_rhs_equations_initial(dae, y, p, config.t_eval, &mut rhs_after, config.n_x);
    } else {
        eval_rhs_equations(dae, y, p, config.t_eval, &mut rhs_after, config.n_x);
    }
    let free_norm_after = free_residual_inf(&rhs_after, config.n_x, config.fixed);
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC Newton iter={} residual_inf(before)={} residual_inf(after)={}",
            iter, free_norm_before, free_norm_after
        );
    }

    Ok(Some(free_norm_after))
}

fn equations_use_initial(dae: &Dae) -> bool {
    !dae.initial_equations.is_empty() || dae.f_x.iter().any(|eq| expr_contains_initial(&eq.rhs))
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

fn log_non_finite_ic_residual_rows(
    dae: &Dae,
    rhs: &[f64],
    y: &[f64],
    p: &[f64],
    use_initial: bool,
    phase: &str,
) {
    if !ic_non_finite_row_diagnostics_enabled() {
        return;
    }
    let mut eval_env = build_env(dae, y, p, 0.0);
    if use_initial {
        eval_env.is_initial = true;
    }
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

fn eval_ic_rhs(dae: &Dae, y: &[f64], p: &[f64], use_initial: bool, n_x: usize, out: &mut [f64]) {
    if use_initial {
        eval_rhs_equations_initial(dae, y, p, 0.0, out, n_x);
    } else {
        eval_rhs_equations(dae, y, p, 0.0, out, n_x);
    }
}

struct IcResidualContext<'a> {
    use_initial: bool,
    n_x: usize,
    fixed: &'a [bool],
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
    eval_ic_rhs(dae, y, p, ctx.use_initial, ctx.n_x, &mut rhs_perturbed);
    let perturbed_free_norm = free_residual_inf(&rhs_perturbed, ctx.n_x, ctx.fixed);
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
            ctx.use_initial,
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

fn finalize_best_or_seeded_solution(dae: &mut Dae, state: &IcFinalizeState<'_>) {
    let mut wrote_solution = false;
    if should_write_partial_ic_solution(
        state.best_r_inf,
        state.first_r_inf,
        state.tol,
        state.best_y,
    ) {
        finalize_initial_solution(dae, state.best_y, state.p, state.n_x, state.fixed, 0.0);
        wrote_solution = true;
    }
    if !wrote_solution && should_write_seeded_ic_solution(state.seeded_updates, state.seeded_y) {
        finalize_initial_solution(dae, state.seeded_y, state.p, state.n_x, state.fixed, 0.0);
    }
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

fn run_initial_newton_iterations(
    dae: &mut Dae,
    y: &mut Vec<f64>,
    ctx: &IcNewtonContext<'_>,
) -> Result<bool, crate::SimError> {
    let first_r_inf = ctx
        .initial_free_norm
        .is_finite()
        .then_some(ctx.initial_free_norm);
    let mut prev_r_inf = f64::INFINITY;
    let mut stagnant_iters = 0usize;
    let mut best_r_inf = f64::INFINITY;
    let mut best_y = y.clone();

    for iter in 0..50 {
        ctx.timeout.check()?;
        let Some(r_inf) = newton_init_step(iter, dae, y, ctx.p, ctx.newton_config)? else {
            let finalize_state = IcFinalizeState {
                p: ctx.p,
                n_x: ctx.n_x,
                fixed: ctx.fixed,
                tol: ctx.tol,
                best_r_inf,
                first_r_inf,
                best_y: &best_y,
                seeded_updates: ctx.seeded_updates,
                seeded_y: ctx.seeded_y,
            };
            finalize_best_or_seeded_solution(dae, &finalize_state);
            return Ok(false);
        };
        if sim_trace_enabled() {
            eprintln!("[sim-trace] IC Newton iter={} residual_inf={}", iter, r_inf);
        }
        if r_inf.is_finite() && r_inf < best_r_inf {
            best_r_inf = r_inf;
            best_y.clone_from(y);
        }
        if r_inf < ctx.tol {
            finalize_initial_solution(dae, y, ctx.p, ctx.n_x, ctx.fixed, 0.0);
            return Ok(true);
        }

        if prev_r_inf.is_finite() && r_inf.is_finite() {
            let ratio = r_inf / prev_r_inf.max(f64::MIN_POSITIVE);
            stagnant_iters = if ratio > 0.95 { stagnant_iters + 1 } else { 0 };
            if iter >= 6 && stagnant_iters >= 4 {
                trace_ic_newton_stagnation(iter, r_inf, prev_r_inf);
                break;
            }
        } else {
            stagnant_iters = 0;
        }
        prev_r_inf = r_inf;
    }

    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC Newton finished converged=false best_residual_inf={} first_residual_inf={}",
            best_r_inf,
            first_r_inf.unwrap_or(f64::NAN)
        );
    }
    let finalize_state = IcFinalizeState {
        p: ctx.p,
        n_x: ctx.n_x,
        fixed: ctx.fixed,
        tol: ctx.tol,
        best_r_inf,
        first_r_inf,
        best_y: &best_y,
        seeded_updates: ctx.seeded_updates,
        seeded_y: ctx.seeded_y,
    };
    finalize_best_or_seeded_solution(dae, &finalize_state);
    Ok(false)
}

pub(crate) fn solve_initial_algebraic(
    dae: &mut Dae,
    n_x: usize,
    tol: f64,
    timeout: &crate::TimeoutBudget,
) -> Result<bool, crate::SimError> {
    let n_eq = dae.f_x.len();
    let n_z = n_eq - n_x;
    if n_z == 0 {
        return Ok(true);
    }

    let use_initial = equations_use_initial(dae);

    let mut y = vec![0.0; n_eq];
    initialize_state_vector(dae, &mut y);
    let p = default_params(dae);
    let initial_updates = apply_initial_section_assignments(dae, &mut y, &p, 0.0);
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC initial-section seeding updates={}",
            initial_updates
        );
        let env_check = build_env(dae, &y, &p, 0.0);
        eprintln!(
            "[sim-trace] IC initial-section env check: count={:?} T_start={:?}",
            env_check.vars.get("vIn.signalSource.count"),
            env_check.vars.get("vIn.signalSource.T_start")
        );
    }
    let fixed = find_fixed_state_indices(dae);
    let newton_config = NewtonInitConfig {
        n_x,
        fixed: &fixed,
        use_initial,
        t_eval: 0.0,
        timeout,
    };

    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC Newton start n_eq={} n_x={} n_z={} fixed_states={} tol={} use_initial={}",
            n_eq,
            n_x,
            n_z,
            fixed.iter().filter(|&&f| f).count(),
            tol,
            use_initial
        );
    }

    let seeded_updates =
        seed_direct_assignment_initial_values(dae, &mut y, &p, n_x, use_initial, 0.0);
    if sim_trace_enabled() && seeded_updates > 0 {
        eprintln!(
            "[sim-trace] IC direct-assignment seeding updates={}",
            seeded_updates
        );
    }
    let seeded_y = y.clone();

    timeout.check()?;
    let mut rhs_initial = vec![0.0; n_eq];
    eval_ic_rhs(dae, &y, &p, use_initial, n_x, &mut rhs_initial);
    let initial_free_norm = free_residual_inf(&rhs_initial, n_x, &fixed);
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC initial residual_inf(before-perturb)={}",
            initial_free_norm
        );
    }
    if !initial_free_norm.is_finite() {
        log_non_finite_ic_residual_rows(dae, &rhs_initial, &y, &p, use_initial, "before_perturb");
    }
    if initial_free_norm <= tol {
        finalize_initial_solution(dae, &y, &p, n_x, &fixed, 0.0);
        return Ok(true);
    }

    for val in &mut y[n_x..n_eq] {
        if *val == 0.0 {
            *val = 1e-6;
        }
    }

    let residual_ctx = IcResidualContext {
        use_initial,
        n_x,
        fixed: &fixed,
        timeout,
    };
    ensure_perturbed_residual_is_finite(dae, &y, &p, &residual_ctx, initial_free_norm)?;

    let newton_ctx = IcNewtonContext {
        p: &p,
        newton_config: &newton_config,
        tol,
        timeout,
        n_x,
        fixed: &fixed,
        seeded_updates,
        seeded_y: &seeded_y,
        initial_free_norm,
    };
    run_initial_newton_iterations(dae, &mut y, &newton_ctx)
}

pub(crate) fn initial_free_residual_inf(
    dae: &Dae,
    n_x: usize,
    timeout: &crate::TimeoutBudget,
) -> Result<f64, crate::SimError> {
    let n_eq = dae.f_x.len();
    if n_eq == 0 {
        return Ok(0.0);
    }
    let use_initial = equations_use_initial(dae);
    let mut y = vec![0.0; n_eq];
    initialize_state_vector(dae, &mut y);
    let p = default_params(dae);
    let _ = apply_initial_section_assignments(dae, &mut y, &p, 0.0);
    let fixed = find_fixed_state_indices(dae);

    timeout.check()?;
    let mut rhs = vec![0.0; n_eq];
    if use_initial {
        eval_rhs_equations_initial(dae, &y, &p, 0.0, &mut rhs, n_x);
    } else {
        eval_rhs_equations(dae, &y, &p, 0.0, &mut rhs, n_x);
    }
    Ok(free_residual_inf(&rhs, n_x, &fixed))
}

pub(crate) fn project_algebraics_with_fixed_states_at_time(
    dae: &Dae,
    y_seed: &[f64],
    n_x: usize,
    t_eval: f64,
    tol: f64,
    timeout: &crate::TimeoutBudget,
) -> Result<Option<Vec<f64>>, crate::SimError> {
    let n_eq = dae.f_x.len();
    if n_eq == 0 || n_x >= n_eq || y_seed.len() < n_eq {
        return Ok(Some(y_seed.get(..n_eq).unwrap_or(y_seed).to_vec()));
    }

    let mut y = y_seed[..n_eq].to_vec();
    let p = default_params(dae);
    let direct_seed_updates = seed_runtime_direct_assignment_values(dae, &mut y, &p, n_x, t_eval);
    if sim_trace_enabled() && direct_seed_updates > 0 {
        eprintln!(
            "[sim-trace] runtime projection direct-seed updates={} t={}",
            direct_seed_updates, t_eval
        );
    }
    let fixed = build_runtime_projection_fixed_mask(dae, n_x, n_eq);
    let newton_config = NewtonInitConfig {
        n_x,
        fixed: &fixed,
        use_initial: false,
        t_eval,
        timeout,
    };

    timeout.check()?;
    let mut rhs_initial = vec![0.0; n_eq];
    eval_rhs_equations(dae, &y, &p, t_eval, &mut rhs_initial, n_x);
    for rhs_i in rhs_initial.iter_mut().take(n_x) {
        *rhs_i = 0.0;
    }
    let initial_free_norm = free_residual_inf(&rhs_initial, n_x, &fixed);
    if initial_free_norm <= tol {
        return Ok(Some(y));
    }

    let mut prev_r_inf = f64::INFINITY;
    let mut stagnant_iters = 0usize;
    for iter in 0..12 {
        timeout.check()?;
        let Some(r_inf) = newton_init_step(iter, dae, &mut y, &p, &newton_config)? else {
            return Ok(None);
        };
        if r_inf < tol {
            return Ok(Some(y));
        }

        if prev_r_inf.is_finite() && r_inf.is_finite() {
            let ratio = r_inf / prev_r_inf.max(f64::MIN_POSITIVE);
            if ratio > 0.98 {
                stagnant_iters += 1;
            } else {
                stagnant_iters = 0;
            }
            if iter >= 4 && stagnant_iters >= 3 {
                return Ok(None);
            }
        } else {
            stagnant_iters = 0;
        }
        prev_r_inf = r_inf;
    }
    Ok(None)
}

fn build_runtime_projection_fixed_mask(dae: &Dae, n_x: usize, n_eq: usize) -> Vec<bool> {
    let mut fixed = vec![false; n_eq];
    for f in fixed.iter_mut().take(n_x.min(n_eq)) {
        *f = true;
    }

    let names = solver_vector_names(dae, n_eq);
    let name_to_idx: std::collections::HashMap<String, usize> = names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect();

    for eq in dae.f_x.iter().skip(n_x) {
        let Some((target, _solution)) = extract_direct_assignment(&eq.rhs) else {
            continue;
        };
        let Some(idx) = solver_idx_for_target(target.as_str(), &name_to_idx) else {
            continue;
        };
        if idx < n_x {
            fixed[idx] = false;
        }
    }

    propagate_runtime_projection_state_dependencies(dae, n_x, &name_to_idx, &mut fixed);

    for (idx, name) in names.iter().enumerate().skip(n_x) {
        if dae
            .f_x
            .get(idx)
            .is_some_and(|eq| eq.origin == "orphaned_variable_pin")
        {
            fixed[idx] = true;
            continue;
        }
        let base = component_base_name(name).unwrap_or_else(|| name.to_string());
        let key = VarName::new(base);
        if dae.discrete_reals.contains_key(&key) || dae.discrete_valued.contains_key(&key) {
            fixed[idx] = true;
        }
    }
    fixed
}

fn propagate_runtime_projection_state_dependencies(
    dae: &Dae,
    n_x: usize,
    name_to_idx: &std::collections::HashMap<String, usize>,
    fixed: &mut [bool],
) {
    let max_rows = n_x.min(dae.f_x.len()).min(fixed.len());
    let mut changed = true;
    while changed {
        changed = false;
        for row in 0..max_rows {
            if fixed[row] {
                continue;
            }
            let mut refs = std::collections::HashSet::new();
            dae.f_x[row].rhs.collect_var_refs(&mut refs);
            for ref_name in refs {
                changed |= clear_fixed_dependency(ref_name.as_str(), name_to_idx, max_rows, fixed);
            }
        }
    }
}

fn clear_fixed_dependency(
    ref_name: &str,
    name_to_idx: &std::collections::HashMap<String, usize>,
    max_rows: usize,
    fixed: &mut [bool],
) -> bool {
    let Some(idx) = dependency_to_unfix(ref_name, name_to_idx, max_rows, fixed) else {
        return false;
    };
    fixed[idx] = false;
    true
}

fn dependency_to_unfix(
    ref_name: &str,
    name_to_idx: &std::collections::HashMap<String, usize>,
    max_rows: usize,
    fixed: &[bool],
) -> Option<usize> {
    solver_idx_for_target(ref_name, name_to_idx).filter(|&idx| idx < max_rows && fixed[idx])
}

fn variable_size_for_target(dae: &Dae, target: &str) -> Option<usize> {
    let lookup = |name: &str| {
        dae.states
            .get(&VarName::new(name))
            .or_else(|| dae.algebraics.get(&VarName::new(name)))
            .or_else(|| dae.outputs.get(&VarName::new(name)))
            .map(|var| var.size())
    };
    lookup(target).or_else(|| component_base_name(target).and_then(|base| lookup(&base)))
}

fn runtime_direct_assignment<'a>(dae: &Dae, eq: &'a Equation) -> Option<(String, &'a Expression)> {
    if eq.origin == "orphaned_variable_pin" {
        return None;
    }

    if let Some((target, solution)) = extract_direct_assignment(&eq.rhs) {
        let target_size = variable_size_for_target(dae, target.as_str())?;
        if !target.contains('[') && target_size > 1 {
            return None;
        }
        return Some((target, solution));
    }

    if let Some(lhs) = eq.lhs.as_ref() {
        let lhs_size = variable_size_for_target(dae, lhs.as_str())?;
        if lhs_size > 1 {
            return None;
        }
        return Some((lhs.as_str().to_string(), &eq.rhs));
    }

    None
}

fn direct_assignment_graph_has_cycle(edges: &std::collections::HashMap<usize, Vec<usize>>) -> bool {
    let mut indegree: std::collections::HashMap<usize, usize> = std::collections::HashMap::new();
    for (&src, deps) in edges {
        indegree.entry(src).or_insert(0);
        for &dst in deps {
            *indegree.entry(dst).or_insert(0) += 1;
        }
    }

    let mut queue = std::collections::VecDeque::new();
    for (&node, &deg) in &indegree {
        if deg == 0 {
            queue.push_back(node);
        }
    }

    let mut visited = 0usize;
    while let Some(node) = queue.pop_front() {
        visited += 1;
        if let Some(deps) = edges.get(&node) {
            for &dst in deps {
                decrement_indegree_and_enqueue(&mut indegree, &mut queue, dst);
            }
        }
    }

    visited != indegree.len()
}

fn decrement_indegree_and_enqueue(
    indegree: &mut std::collections::HashMap<usize, usize>,
    queue: &mut std::collections::VecDeque<usize>,
    dst: usize,
) {
    let Some(deg) = indegree.get_mut(&dst) else {
        return;
    };
    *deg -= 1;
    if *deg == 0 {
        queue.push_back(dst);
    }
}

pub(crate) fn runtime_projection_required(dae: &Dae, n_x: usize) -> bool {
    let n_total = dae.f_x.len();
    if n_x >= n_total {
        return false;
    }

    let names = solver_vector_names(dae, n_total);
    let name_to_idx: std::collections::HashMap<String, usize> = names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect();
    let mut assigned_targets: std::collections::HashSet<usize> = std::collections::HashSet::new();
    let mut edges: std::collections::HashMap<usize, Vec<usize>> = std::collections::HashMap::new();

    for eq in dae.f_x.iter().skip(n_x) {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let Some((target, solution)) = runtime_direct_assignment(dae, eq) else {
            return true;
        };
        let Some(target_idx) = solver_idx_for_target(target.as_str(), &name_to_idx) else {
            return true;
        };
        if target_idx < n_x || target_idx >= n_total {
            return true;
        }
        if !assigned_targets.insert(target_idx) {
            return true;
        }

        let mut refs = std::collections::HashSet::new();
        solution.collect_var_refs(&mut refs);
        for ref_name in refs {
            let Some(dep_idx) = solver_idx_for_target(ref_name.as_str(), &name_to_idx) else {
                continue;
            };
            if dep_idx < n_x || dep_idx >= n_total {
                continue;
            }
            if dep_idx == target_idx {
                return true;
            }
            edges.entry(target_idx).or_default().push(dep_idx);
        }
    }

    for idx in n_x..n_total {
        let is_pinned = dae
            .f_x
            .get(idx)
            .is_some_and(|eq| eq.origin == "orphaned_variable_pin");
        if !is_pinned && !assigned_targets.contains(&idx) {
            return true;
        }
    }

    direct_assignment_graph_has_cycle(&edges)
}

pub(crate) fn seed_runtime_direct_assignments(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    t_eval: f64,
) -> usize {
    seed_runtime_direct_assignment_values(dae, y, p, n_x, t_eval)
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

fn persist_initial_section_discrete_starts(
    dae: &mut Dae,
    y: &[f64],
    p: &[f64],
    t_eval: f64,
) -> usize {
    if dae.discrete_reals.is_empty() && dae.discrete_valued.is_empty() {
        return 0;
    }

    let mut env = build_env(dae, y, p, t_eval);
    env.is_initial = true;
    let max_passes = dae.initial_equations.len().clamp(1, 32);
    for _ in 0..max_passes {
        let mut changed = false;
        for eq in &dae.initial_equations {
            let assignment = eq
                .lhs
                .as_ref()
                .map(|lhs| (lhs.as_str().to_string(), &eq.rhs))
                .or_else(|| extract_direct_assignment(&eq.rhs));
            let Some((target, solution)) = assignment else {
                continue;
            };
            let value = clamp_finite(eval_expr::<f64>(solution, &env));
            let prev = env.vars.get(target.as_str()).copied().unwrap_or(0.0);
            if (prev - value).abs() > 1e-12 {
                env.set(target.as_str(), value);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

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
    updates
}

fn finalize_initial_solution(
    dae: &mut Dae,
    y: &[f64],
    p: &[f64],
    n_x: usize,
    fixed: &[bool],
    t_eval: f64,
) {
    write_solved_ics(dae, y, n_x, fixed);
    let discrete_updates = persist_initial_section_discrete_starts(dae, y, p, t_eval);
    if sim_trace_enabled() && discrete_updates > 0 {
        eprintln!(
            "[sim-trace] persisted initial-section discrete starts updates={}",
            discrete_updates
        );
    }
}
