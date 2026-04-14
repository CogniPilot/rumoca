use super::*;

pub(crate) struct PreparedInitialAlgebraic {
    use_initial: bool,
    y: Vec<f64>,
    p: Vec<f64>,
    compiled_initial: Option<CompiledInitialNewtonContext>,
    compiled_runtime: Option<CompiledRuntimeNewtonContext>,
    fixed: Vec<bool>,
    seeded_updates: usize,
    seeded_y: Vec<f64>,
    initial_free_norm: f64,
}

const INITIAL_HOMOTOPY_PRECONDITION_LAMBDAS: [f64; 5] = [0.0, 0.25, 0.5, 0.75, 1.0];

pub(crate) fn prepare_initial_algebraic(
    dae: &Dae,
    n_x: usize,
    timeout: &crate::TimeoutBudget,
    param_values: &[f64],
) -> Result<PreparedInitialAlgebraic, crate::SimError> {
    let n_eq = dae.f_x.len();
    let use_initial = equations_use_initial(dae);
    let mut y = vec![0.0; n_eq];
    initialize_state_vector_with_params(dae, &mut y, param_values);
    let p = param_values.to_vec();
    let compiled_initial = build_initial_newton_context_if_needed(dae, n_eq, use_initial)?;
    let compiled_runtime = build_runtime_newton_context_if_needed(dae, n_eq, use_initial)?;
    let initial_updates = apply_initial_section_assignments_strict(dae, &mut y, &p, 0.0)?;
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC initial-section seeding updates={}",
            initial_updates
        );
        match build_initial_eval_env_preserving_pre_values(dae, &y, &p, 0.0) {
            Ok(env_check) => {
                eprintln!(
                    "[sim-trace] IC initial-section env check: count={:?} T_start={:?}",
                    env_check.vars.get("vIn.signalSource.count"),
                    env_check.vars.get("vIn.signalSource.T_start")
                );
            }
            Err(err) => {
                eprintln!("[sim-trace] IC initial-section env check failed: {err}");
            }
        }
    }

    let fixed = find_fixed_state_indices(dae);
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
    let eval_mode = IcEvalMode {
        compiled_initial: compiled_initial.as_ref(),
        compiled_runtime: compiled_runtime.as_ref(),
        runtime_seed_env: None,
        use_initial,
        is_initial_phase: true,
        homotopy_lambda: 1.0,
        t_eval: 0.0,
        n_x,
        ignored_rows: Some(&fixed),
    };
    eval_ic_rhs_at_time(dae, &y, &p, &eval_mode, &mut rhs_initial);
    let initial_free_norm = free_residual_inf(&rhs_initial, n_x, &fixed);
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC initial residual_inf(before-perturb)={}",
            initial_free_norm
        );
    }
    if !initial_free_norm.is_finite() {
        log_non_finite_ic_residual_rows(
            dae,
            &rhs_initial,
            &y,
            &p,
            n_x,
            use_initial,
            "before_perturb",
        );
    }

    Ok(PreparedInitialAlgebraic {
        use_initial,
        y,
        p,
        compiled_initial,
        compiled_runtime,
        fixed,
        seeded_updates,
        seeded_y,
        initial_free_norm,
    })
}

pub(crate) struct InitialHomotopyRun<'a> {
    dae: &'a Dae,
    y: &'a mut [f64],
    p: &'a [f64],
    n_x: usize,
    use_initial: bool,
    fixed: &'a [bool],
    tol: f64,
    timeout: &'a crate::TimeoutBudget,
}

pub(crate) fn run_initial_homotopy_preconditioning(
    run: &mut InitialHomotopyRun<'_>,
) -> Result<(), crate::SimError> {
    for &lambda in &INITIAL_HOMOTOPY_PRECONDITION_LAMBDAS {
        run.timeout.check()?;
        let eval_mode = IcEvalMode {
            compiled_initial: None,
            compiled_runtime: None,
            runtime_seed_env: None,
            use_initial: run.use_initial,
            is_initial_phase: true,
            homotopy_lambda: lambda,
            t_eval: 0.0,
            n_x: run.n_x,
            ignored_rows: Some(run.fixed),
        };
        let mut rhs = vec![0.0; run.y.len()];
        eval_ic_rhs_at_time(run.dae, run.y, run.p, &eval_mode, &mut rhs);
        let initial_free_norm = free_residual_inf(&rhs, run.n_x, run.fixed);
        if initial_free_norm <= run.tol {
            continue;
        }
        let residual_ctx = IcResidualContext {
            eval_mode: eval_mode.clone(),
            ignored_rows: run.fixed,
            timeout: run.timeout,
        };
        ensure_perturbed_residual_is_finite(
            run.dae,
            run.y,
            run.p,
            &residual_ctx,
            initial_free_norm,
        )?;

        let config = NewtonInitConfig {
            n_x: run.n_x,
            fixed_cols: run.fixed,
            ignored_rows: run.fixed,
            runtime_fd_jac_cols: &[],
            use_initial: run.use_initial,
            is_initial_phase: true,
            homotopy_lambda: lambda,
            compiled_initial: None,
            compiled_runtime: None,
            runtime_seed_env: None,
            t_eval: 0.0,
            timeout: run.timeout,
        };
        let mut prev_r_inf = f64::INFINITY;
        let mut stagnant_iters = 0usize;
        let mut best_r_inf = initial_free_norm;
        let mut best_y = run.y.to_vec();
        for iter in 0..12 {
            run.timeout.check()?;
            let Some(r_inf) = newton_init_step(iter, run.dae, run.y, run.p, &config)? else {
                break;
            };
            if r_inf.is_finite() && r_inf < best_r_inf {
                best_r_inf = r_inf;
                best_y.copy_from_slice(run.y);
            }
            if r_inf <= run.tol {
                break;
            }
            if prev_r_inf.is_finite() && r_inf.is_finite() {
                let ratio = r_inf / prev_r_inf.max(f64::MIN_POSITIVE);
                match update_stagnant_homotopy_iters(stagnant_iters, ratio) {
                    Some(next_stagnant_iters) => stagnant_iters = next_stagnant_iters,
                    None => break,
                }
            } else {
                stagnant_iters = 0;
            }
            prev_r_inf = r_inf;
        }
        run.y.copy_from_slice(&best_y);
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] IC homotopy precondition lambda={} best_residual_inf={}",
                lambda, best_r_inf
            );
        }
    }
    Ok(())
}

pub(crate) fn update_stagnant_homotopy_iters(stagnant_iters: usize, ratio: f64) -> Option<usize> {
    let stagnant_iters = if ratio > 0.95 { stagnant_iters + 1 } else { 0 };
    (stagnant_iters < 3).then_some(stagnant_iters)
}

pub(crate) fn try_mark_branch_local_analog_col(
    branch_local_analog_cols: &mut [bool],
    member_idx: usize,
    member_group: usize,
    target_group: usize,
) {
    if member_group == target_group && member_idx < branch_local_analog_cols.len() {
        branch_local_analog_cols[member_idx] = true;
    }
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
            finalize_best_or_seeded_solution(dae, &finalize_state)?;
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
            finalize_initial_solution(dae, y, ctx.p, ctx.n_x, ctx.fixed, 0.0)?;
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
    finalize_best_or_seeded_solution(dae, &finalize_state)?;
    Ok(false)
}

#[cfg(test)]
pub(crate) fn solve_initial_algebraic(
    dae: &mut Dae,
    n_x: usize,
    tol: f64,
    timeout: &crate::TimeoutBudget,
) -> Result<bool, crate::SimError> {
    let p = default_params(dae);
    solve_initial_algebraic_with_params(dae, n_x, tol, timeout, &p)
}

pub(crate) fn solve_initial_algebraic_with_params(
    dae: &mut Dae,
    n_x: usize,
    tol: f64,
    timeout: &crate::TimeoutBudget,
    param_values: &[f64],
) -> Result<bool, crate::SimError> {
    let n_eq = dae.f_x.len();
    let n_z = n_eq - n_x;
    if n_z == 0 {
        return Ok(true);
    }

    let PreparedInitialAlgebraic {
        use_initial,
        mut y,
        p,
        compiled_initial,
        compiled_runtime,
        fixed,
        seeded_updates,
        seeded_y,
        mut initial_free_norm,
    } = prepare_initial_algebraic(dae, n_x, timeout, param_values)?;
    let newton_config = build_initial_newton_config(
        n_x,
        use_initial,
        timeout,
        &fixed,
        compiled_initial.as_ref(),
        compiled_runtime.as_ref(),
    );
    trace_initial_newton_start(n_eq, n_x, n_z, &fixed, tol, use_initial);
    if initial_free_norm <= tol {
        finalize_initial_solution(dae, &y, &p, n_x, &fixed, 0.0)?;
        return Ok(true);
    }
    seed_initial_algebraic_unknowns(&mut y[n_x..n_eq]);
    initial_free_norm =
        maybe_run_initial_homotopy_preconditioning(dae, &mut y, &p, &newton_config, tol)?
            .unwrap_or(initial_free_norm);
    if initial_free_norm <= tol {
        finalize_initial_solution(dae, &y, &p, n_x, &fixed, 0.0)?;
        return Ok(true);
    }

    let residual_ctx = IcResidualContext {
        eval_mode: IcEvalMode {
            compiled_initial: compiled_initial.as_ref(),
            compiled_runtime: compiled_runtime.as_ref(),
            runtime_seed_env: None,
            use_initial,
            is_initial_phase: true,
            homotopy_lambda: 1.0,
            t_eval: 0.0,
            n_x,
            ignored_rows: Some(&fixed),
        },
        ignored_rows: &fixed,
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

fn build_initial_newton_config<'a>(
    n_x: usize,
    use_initial: bool,
    timeout: &'a crate::TimeoutBudget,
    fixed: &'a [bool],
    compiled_initial: Option<&'a CompiledInitialNewtonContext>,
    compiled_runtime: Option<&'a CompiledRuntimeNewtonContext>,
) -> NewtonInitConfig<'a> {
    NewtonInitConfig {
        n_x,
        fixed_cols: fixed,
        ignored_rows: fixed,
        runtime_fd_jac_cols: &[],
        use_initial,
        is_initial_phase: true,
        homotopy_lambda: 1.0,
        compiled_initial,
        compiled_runtime,
        runtime_seed_env: None,
        t_eval: 0.0,
        timeout,
    }
}

pub(crate) fn trace_initial_newton_start(
    n_eq: usize,
    n_x: usize,
    n_z: usize,
    fixed: &[bool],
    tol: f64,
    use_initial: bool,
) {
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC Newton start n_eq={} n_x={} n_z={} fixed_states={} tol={} use_initial={}",
            n_eq,
            n_x,
            n_z,
            fixed.iter().filter(|&&flag| flag).count(),
            tol,
            use_initial
        );
    }
}

pub(crate) fn seed_initial_algebraic_unknowns(y: &mut [f64]) {
    for value in y {
        if *value == 0.0 {
            *value = 1e-6;
        }
    }
}

fn maybe_run_initial_homotopy_preconditioning(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    config: &NewtonInitConfig<'_>,
    tol: f64,
) -> Result<Option<f64>, crate::SimError> {
    if !equations_use_homotopy(dae) {
        return Ok(None);
    }
    let mut run = InitialHomotopyRun {
        dae,
        y,
        p,
        n_x: config.n_x,
        use_initial: config.use_initial,
        fixed: config.fixed_cols,
        tol,
        timeout: config.timeout,
    };
    run_initial_homotopy_preconditioning(&mut run)?;
    let mut rhs = vec![0.0; dae.f_x.len()];
    eval_ic_rhs_at_time(
        dae,
        run.y,
        p,
        &IcEvalMode {
            compiled_initial: config.compiled_initial,
            compiled_runtime: config.compiled_runtime,
            runtime_seed_env: None,
            use_initial: config.use_initial,
            is_initial_phase: true,
            homotopy_lambda: 1.0,
            t_eval: 0.0,
            n_x: config.n_x,
            ignored_rows: Some(config.fixed_cols),
        },
        &mut rhs,
    );
    let residual = free_residual_inf(&rhs, config.n_x, config.fixed_cols);
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC residual_inf(after-homotopy-precondition)={}",
            residual
        );
    }
    Ok(Some(residual))
}
