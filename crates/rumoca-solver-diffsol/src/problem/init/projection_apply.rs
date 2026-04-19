use super::*;

pub(crate) struct RuntimeProjectionContext<'a> {
    pub(crate) p: &'a [f64],
    pub(crate) compiled_runtime: Option<&'a CompiledRuntimeNewtonContext>,
    pub(crate) fixed_cols: &'a [bool],
    pub(crate) ignored_rows: &'a [bool],
    pub(crate) branch_local_analog_cols: &'a [bool],
    pub(crate) direct_seed_ctx: Option<&'a RuntimeDirectSeedContext>,
    pub(crate) direct_seed_env_cache: Option<&'a mut Option<VarEnv<f64>>>,
}

pub(crate) struct RuntimeProjectionStep<'a> {
    pub(crate) y_seed: &'a [f64],
    pub(crate) n_x: usize,
    pub(crate) t_eval: f64,
    pub(crate) tol: f64,
    pub(crate) timeout: &'a rumoca_sim::TimeoutBudget,
}

pub(crate) struct PreparedRuntimeProjectionInfo<'a> {
    newton_config: NewtonInitConfig<'a>,
    initial_free_norm: f64,
}

pub(crate) fn seed_runtime_projection_values(
    dae: &Dae,
    y: &mut [f64],
    ctx: &mut RuntimeProjectionContext<'_>,
    step: &RuntimeProjectionStep<'_>,
) -> usize {
    let direct_seed_updates = if let Some(direct_seed_ctx) = ctx.direct_seed_ctx {
        seed_runtime_direct_assignment_values_with_context_and_env_and_blocked_solver_cols(
            direct_seed_ctx,
            dae,
            y,
            ctx.p,
            step.t_eval,
            ctx.direct_seed_env_cache.as_deref_mut(),
            ctx.fixed_cols,
        )
    } else {
        seed_runtime_direct_assignment_values(dae, y, ctx.p, step.n_x, step.t_eval)
    };
    if sim_trace_enabled() && direct_seed_updates > 0 {
        eprintln!(
            "[sim-trace] runtime projection direct-seed updates={} t={}",
            direct_seed_updates, step.t_eval
        );
    }
    direct_seed_updates
}

pub(crate) fn prepare_runtime_projection_in_place<'a>(
    dae: &Dae,
    y: &mut [f64],
    ctx: &mut RuntimeProjectionContext<'a>,
    step: &RuntimeProjectionStep<'a>,
    scratch: &mut RuntimeProjectionScratch,
) -> Result<PreparedRuntimeProjectionInfo<'a>, crate::SimError> {
    seed_runtime_projection_values(dae, y, ctx, step);
    let newton_config = NewtonInitConfig {
        n_x: step.n_x,
        fixed_cols: ctx.fixed_cols,
        ignored_rows: ctx.ignored_rows,
        runtime_fd_jac_cols: ctx.branch_local_analog_cols,
        use_initial: false,
        is_initial_phase: false,
        homotopy_lambda: 1.0,
        compiled_initial: None,
        compiled_runtime: ctx.compiled_runtime,
        runtime_seed_env: ctx
            .direct_seed_env_cache
            .as_deref()
            .and_then(|slot| slot.as_ref().cloned()),
        t_eval: step.t_eval,
        timeout: step.timeout,
    };

    step.timeout.check()?;
    let rhs_initial = scratch.rhs(y.len());
    eval_ic_rhs_at_time(
        dae,
        y,
        ctx.p,
        &IcEvalMode {
            compiled_initial: None,
            compiled_runtime: ctx.compiled_runtime,
            runtime_seed_env: ctx
                .direct_seed_env_cache
                .as_deref()
                .and_then(|slot| slot.as_ref().cloned()),
            use_initial: false,
            is_initial_phase: false,
            homotopy_lambda: 1.0,
            t_eval: step.t_eval,
            n_x: step.n_x,
            ignored_rows: Some(ctx.ignored_rows),
        },
        rhs_initial,
    );
    for rhs_i in rhs_initial.iter_mut().take(step.n_x) {
        *rhs_i = 0.0;
    }
    let initial_free_norm = free_residual_inf(rhs_initial, step.n_x, ctx.ignored_rows);
    Ok(PreparedRuntimeProjectionInfo {
        newton_config,
        initial_free_norm,
    })
}

pub(crate) fn prepare_runtime_projection<'a>(
    dae: &Dae,
    ctx: &mut RuntimeProjectionContext<'a>,
    step: &RuntimeProjectionStep<'a>,
) -> Result<(Vec<f64>, PreparedRuntimeProjectionInfo<'a>), crate::SimError> {
    let n_eq = dae.f_x.len();
    let mut y = step.y_seed[..n_eq].to_vec();
    let mut scratch = RuntimeProjectionScratch::default();
    let prepared = prepare_runtime_projection_in_place(dae, &mut y, ctx, step, &mut scratch)?;
    Ok((y, prepared))
}

pub(crate) fn project_algebraics_with_cached_runtime_jacobian_step_in_place<'a>(
    dae: &Dae,
    y: &mut [f64],
    mut ctx: RuntimeProjectionContext<'a>,
    step: RuntimeProjectionStep<'a>,
    cached_jacobian: &nalgebra::DMatrix<f64>,
    scratch: &mut RuntimeProjectionScratch,
) -> Result<bool, crate::SimError> {
    let n_eq = dae.f_x.len();
    if n_eq == 0 || step.n_x >= n_eq || y.len() < n_eq {
        return Ok(true);
    }

    let PreparedRuntimeProjectionInfo {
        newton_config,
        initial_free_norm,
    } = prepare_runtime_projection_in_place(dae, y, &mut ctx, &step, scratch)?;
    if initial_free_norm <= step.tol {
        return Ok(true);
    }

    let Some((r_inf, _)) = newton_init_step_from_current_rhs_in_place(
        CachedNewtonStep {
            iter: 0,
            cached_jac: Some(cached_jacobian),
            cache_built_jac: false,
        },
        dae,
        y,
        ctx.p,
        &newton_config,
        initial_free_norm,
        scratch,
    )?
    else {
        return Ok(false);
    };
    Ok(r_inf <= step.tol)
}

pub(crate) fn project_algebraics_with_fixed_states_at_time_with_context_and_cache_in_place<'a>(
    dae: &Dae,
    y: &mut [f64],
    mut ctx: RuntimeProjectionContext<'a>,
    step: RuntimeProjectionStep<'a>,
    jacobian_cache: Option<&mut Option<nalgebra::DMatrix<f64>>>,
    scratch: &mut RuntimeProjectionScratch,
) -> Result<bool, crate::SimError> {
    let n_eq = dae.f_x.len();
    if n_eq == 0 || step.n_x >= n_eq || y.len() < n_eq {
        return Ok(true);
    }

    let PreparedRuntimeProjectionInfo {
        newton_config,
        initial_free_norm,
    } = prepare_runtime_projection_in_place(dae, y, &mut ctx, &step, scratch)?;
    if initial_free_norm <= step.tol {
        return Ok(true);
    }

    let mut prev_r_inf = f64::INFINITY;
    let mut stagnant_iters = 0usize;
    let mut cached_projection_jac: Option<nalgebra::DMatrix<f64>> = None;
    let mut jacobian_cache_slot = jacobian_cache;
    let cache_requested = jacobian_cache_slot.is_some();
    let mut reusable_projection_jac: Option<nalgebra::DMatrix<f64>> = None;
    for iter in 0..12 {
        step.timeout.check()?;
        let reuse_cached_jac = iter == 1 && cached_projection_jac.is_some();
        let step_result = if iter == 0 {
            newton_init_step_from_current_rhs_in_place(
                CachedNewtonStep {
                    iter,
                    cached_jac: None,
                    cache_built_jac: true,
                },
                dae,
                y,
                ctx.p,
                &newton_config,
                initial_free_norm,
                scratch,
            )?
        } else {
            newton_init_step_with_cached_jacobian_in_place(
                CachedNewtonStep {
                    iter,
                    cached_jac: cached_projection_jac.as_ref(),
                    cache_built_jac: false,
                },
                dae,
                y,
                ctx.p,
                &newton_config,
                scratch,
            )?
        };
        let Some((r_inf, next_cached_jac)) = step_result else {
            store_runtime_projection_jacobian_cache(
                &mut jacobian_cache_slot,
                &mut reusable_projection_jac,
            );
            return Ok(false);
        };
        if iter == 0 && cache_requested {
            reusable_projection_jac = next_cached_jac.as_ref().cloned();
        }
        cached_projection_jac = if reuse_cached_jac {
            None
        } else {
            next_cached_jac
        };
        if r_inf < step.tol {
            store_runtime_projection_jacobian_cache(
                &mut jacobian_cache_slot,
                &mut reusable_projection_jac,
            );
            return Ok(true);
        }

        if prev_r_inf.is_finite() && r_inf.is_finite() {
            let ratio = r_inf / prev_r_inf.max(f64::MIN_POSITIVE);
            if ratio > 0.98 {
                stagnant_iters += 1;
            } else {
                stagnant_iters = 0;
            }
            if iter >= 4 && stagnant_iters >= 3 {
                return Ok(false);
            }
        } else {
            stagnant_iters = 0;
        }
        prev_r_inf = r_inf;
    }
    store_runtime_projection_jacobian_cache(&mut jacobian_cache_slot, &mut reusable_projection_jac);
    Ok(false)
}

pub(crate) fn project_algebraics_with_fixed_states_at_time_with_context_and_cache<'a>(
    dae: &Dae,
    mut ctx: RuntimeProjectionContext<'a>,
    step: RuntimeProjectionStep<'a>,
    jacobian_cache: Option<&mut Option<nalgebra::DMatrix<f64>>>,
) -> Result<Option<Vec<f64>>, crate::SimError> {
    let n_eq = dae.f_x.len();
    if n_eq == 0 || step.n_x >= n_eq || step.y_seed.len() < n_eq {
        return Ok(Some(
            step.y_seed.get(..n_eq).unwrap_or(step.y_seed).to_vec(),
        ));
    }

    let (
        mut y,
        PreparedRuntimeProjectionInfo {
            newton_config,
            initial_free_norm,
        },
    ) = prepare_runtime_projection(dae, &mut ctx, &step)?;
    if initial_free_norm <= step.tol {
        return Ok(Some(y));
    }

    let mut prev_r_inf = f64::INFINITY;
    let mut stagnant_iters = 0usize;
    let mut cached_projection_jac: Option<nalgebra::DMatrix<f64>> = None;
    let mut jacobian_cache_slot = jacobian_cache;
    let cache_requested = jacobian_cache_slot.is_some();
    let mut reusable_projection_jac: Option<nalgebra::DMatrix<f64>> = None;
    for iter in 0..12 {
        step.timeout.check()?;
        let reuse_cached_jac = iter == 1 && cached_projection_jac.is_some();
        let Some((r_inf, next_cached_jac)) = newton_init_step_with_cached_jacobian(
            iter,
            dae,
            &mut y,
            ctx.p,
            &newton_config,
            cached_projection_jac.as_ref(),
            iter == 0,
        )?
        else {
            store_runtime_projection_jacobian_cache(
                &mut jacobian_cache_slot,
                &mut reusable_projection_jac,
            );
            return Ok(None);
        };
        if iter == 0 && cache_requested {
            reusable_projection_jac = next_cached_jac.as_ref().cloned();
        }
        cached_projection_jac = if reuse_cached_jac {
            None
        } else {
            next_cached_jac
        };
        if r_inf < step.tol {
            store_runtime_projection_jacobian_cache(
                &mut jacobian_cache_slot,
                &mut reusable_projection_jac,
            );
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
    store_runtime_projection_jacobian_cache(&mut jacobian_cache_slot, &mut reusable_projection_jac);
    Ok(None)
}

pub(crate) fn store_runtime_projection_jacobian_cache(
    cache_slot: &mut Option<&mut Option<nalgebra::DMatrix<f64>>>,
    reusable_projection_jac: &mut Option<nalgebra::DMatrix<f64>>,
) {
    if let Some(slot) = cache_slot.as_deref_mut() {
        *slot = reusable_projection_jac.take();
    }
}

pub(crate) fn project_algebraics_with_fixed_states_at_time_with_context(
    dae: &Dae,
    y_seed: &[f64],
    n_x: usize,
    ctx: RuntimeProjectionContext<'_>,
    t_eval: f64,
    tol: f64,
    timeout: &rumoca_sim::TimeoutBudget,
) -> Result<Option<Vec<f64>>, crate::SimError> {
    project_algebraics_with_fixed_states_at_time_with_context_and_cache(
        dae,
        ctx,
        RuntimeProjectionStep {
            y_seed,
            n_x,
            t_eval,
            tol,
            timeout,
        },
        None,
    )
}

pub(crate) fn project_algebraics_with_fixed_states_at_time(
    dae: &Dae,
    y_seed: &[f64],
    n_x: usize,
    t_eval: f64,
    tol: f64,
    timeout: &rumoca_sim::TimeoutBudget,
) -> Result<Option<Vec<f64>>, crate::SimError> {
    let n_eq = dae.f_x.len();
    if n_eq == 0 || n_x >= n_eq || y_seed.len() < n_eq {
        return Ok(Some(y_seed.get(..n_eq).unwrap_or(y_seed).to_vec()));
    }
    let p = default_params(dae);
    let compiled_runtime = build_runtime_newton_context_if_needed(dae, n_eq, false)?;
    let masks = build_runtime_projection_masks(dae, n_x, n_eq);
    if sim_trace_enabled() {
        let branch_local_rows = masks
            .branch_local_analog_unknowns
            .iter()
            .filter(|unknown| unknown.is_some())
            .count();
        let branch_local_row_pairs = masks.branch_local_analog_row_pairs.len();
        if branch_local_rows > 0 || branch_local_row_pairs > 0 {
            eprintln!(
                "[sim-trace] runtime projection branch-local analog rows={} row_pairs={}",
                branch_local_rows, branch_local_row_pairs
            );
        }
    }
    project_algebraics_with_fixed_states_at_time_with_context(
        dae,
        y_seed,
        n_x,
        RuntimeProjectionContext {
            p: &p,
            compiled_runtime: compiled_runtime.as_ref(),
            fixed_cols: &masks.fixed_cols,
            ignored_rows: &masks.ignored_rows,
            branch_local_analog_cols: &masks.branch_local_analog_cols,
            direct_seed_ctx: None,
            direct_seed_env_cache: None,
        },
        t_eval,
        tol,
        timeout,
    )
}
