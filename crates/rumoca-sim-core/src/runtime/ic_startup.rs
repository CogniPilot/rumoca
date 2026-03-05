use crate::sim_types::SimError;

fn sim_trace_enabled() -> bool {
    crate::simulation::diagnostics::sim_trace_enabled()
}

fn trace_ic_newton_result(mode: &str, converged: bool) {
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC full-Newton {} converged={}",
            mode, converged
        );
    }
}

fn solve_ic_with_newton(
    dae: &mut rumoca_ir_dae::Dae,
    n_x: usize,
    atol: f64,
    budget: &crate::TimeoutBudget,
    mode: &str,
) -> Result<(), SimError> {
    let newton_ok = crate::solve_initial_algebraic(dae, n_x, atol, budget)?;
    trace_ic_newton_result(mode, newton_ok);
    Ok(())
}

fn ic_residual_requires_newton(
    dae: &rumoca_ir_dae::Dae,
    n_x: usize,
    atol: f64,
    budget: &crate::TimeoutBudget,
) -> Result<bool, SimError> {
    let residual_inf = crate::initial_free_residual_inf(dae, n_x, budget)?;
    let residual_limit = (atol * 10.0).max(1.0e-8);
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC BLT residual quality residual_inf={} limit={}",
            residual_inf, residual_limit
        );
    }
    Ok(!residual_inf.is_finite() || residual_inf > residual_limit)
}

fn force_ic_newton_via_env() -> bool {
    std::env::var("RUMOCA_SIM_FORCE_IC_NEWTON")
        .map(|v| {
            let s = v.trim().to_ascii_lowercase();
            !s.is_empty() && s != "0" && s != "false" && s != "no"
        })
        .unwrap_or(false)
}

fn log_ic_quality_fallback() {
    if sim_trace_enabled() {
        eprintln!("[sim-trace] IC BLT residual quality check failed; running full-Newton fallback");
    }
}

pub fn solve_initial_conditions(
    dae: &mut rumoca_ir_dae::Dae,
    ic_blocks: &[rumoca_phase_structural::IcBlock],
    n_x: usize,
    atol: f64,
    budget: &crate::TimeoutBudget,
) -> Result<(), SimError> {
    crate::run_timeout_result::<(), SimError, _>(budget, || {
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] IC start blocks={} n_x={} atol={}",
                ic_blocks.len(),
                n_x,
                atol
            );
        }
        if !ic_blocks.is_empty() {
            let blt_ok = crate::solve_initial_blt_with_deadline(
                dae,
                n_x,
                ic_blocks,
                atol,
                budget.deadline(),
            )
            .map_err(|e| match e {
                crate::IcSolveError::Timeout => SimError::from(budget.timeout_error()),
            })?;
            if sim_trace_enabled() {
                eprintln!("[sim-trace] IC BLT result converged={}", blt_ok);
            }
            let force_newton = force_ic_newton_via_env();
            if force_newton && sim_trace_enabled() {
                eprintln!("[sim-trace] IC full-Newton forced by RUMOCA_SIM_FORCE_IC_NEWTON");
            }
            let mut fallback_newton = !blt_ok || force_newton;
            if blt_ok && !force_newton && ic_residual_requires_newton(dae, n_x, atol, budget)? {
                fallback_newton = true;
                log_ic_quality_fallback();
            }
            if fallback_newton {
                solve_ic_with_newton(dae, n_x, atol, budget, "fallback")?;
            }
        } else {
            solve_ic_with_newton(dae, n_x, atol, budget, "only")?;
        }
        Ok(())
    })
}
