use diffsol::{OdeSolverMethod, VectorHost};

use super::{
    DiffsolBackend, IntegrationOutput,
    apply_initial_sections_and_sync_startup_state, build_compiled_discrete_event_context,
    build_output_names, configure_solver_problem_with_profile,
};
use crate::TimeoutBudget;
use crate::with_diffsol::{
    Dae, LS, MassMatrix, OutputBuffers, SimError, SimOptions, SolverStartupProfile,
    build_parameter_values, problem, sim_trace_enabled, trace_timer_elapsed_seconds,
    trace_timer_start_if,
};
pub(crate) fn try_integrate_esdirk34(
    dae: &Dae,
    opts: &SimOptions,
    eps: f64,
    n_total: usize,
    mass_matrix: &MassMatrix,
    startup_profile: SolverStartupProfile,
    budget: &TimeoutBudget,
) -> Result<(OutputBuffers, Vec<f64>), SimError> {
    let trace_enabled = sim_trace_enabled();
    let start = trace_timer_start_if(trace_enabled);
    budget.check()?;
    let mut problem = problem::build_problem(dae, opts.rtol, opts.atol, eps, mass_matrix)?;
    configure_solver_problem_with_profile(&mut problem, opts, startup_profile);
    if trace_enabled {
        eprintln!(
            "[sim-trace] ESDIRK34 start eps={} profile={:?} h0={} max_wall={:?}",
            eps, startup_profile, problem.h0, opts.max_wall_seconds
        );
    }
    let mut solver = problem
        .esdirk34::<LS>()
        .map_err(|e| SimError::SolverError(format!("Failed to create ESDIRK34 solver: {e}")))?;
    let n_x = problem::count_states(dae);
    let param_values = build_parameter_values(dae, budget)?;
    apply_initial_sections_and_sync_startup_state(
        &mut solver,
        dae,
        opts,
        startup_profile,
        &param_values,
        n_x,
        budget,
    )?;
    let mut solver_names = build_output_names(dae);
    solver_names.truncate(n_total);
    let output = IntegrationOutput::new(opts, n_total, solver.state().y.as_slice());
    let compiled_discrete_event_ctx = build_compiled_discrete_event_context(dae, n_total)?;
    let ctx = super::SolverLoopContext {
        dae: dae.clone(),
        opts: opts.clone(),
        startup_profile,
        n_x,
        param_values: param_values.clone(),
        discrete_event_ctx: compiled_discrete_event_ctx,
        budget: *budget,
    };
    let (output, stats, final_t) = {
        let mut backend = DiffsolBackend::new(solver, output, ctx, None, solver_names);
        let stats = match crate::run_with_runtime_schedule(
            &mut backend,
            dae,
            opts.t_start,
            opts.t_end,
            || budget.check().map_err(SimError::from),
        ) {
            Ok(stats) => stats,
            Err(err) => {
                let final_t = crate::SimulationBackend::read_state(&backend).t;
                if trace_enabled {
                    eprintln!(
                        "[sim-trace] ESDIRK34 step-fail eps={} profile={:?} elapsed={:.3}s t={} err={}",
                        eps,
                        startup_profile,
                        trace_timer_elapsed_seconds(start),
                        final_t,
                        err
                    );
                }
                return Err(err);
            }
        };
        let final_t = crate::SimulationBackend::read_state(&backend).t;
        let (_solver, output, _steps, _roots) = backend.into_parts();
        (output, stats, final_t)
    };
    if trace_enabled {
        eprintln!(
            "[sim-trace] ESDIRK34 done eps={} profile={:?} elapsed={:.3}s steps={} roots={} final_t={}",
            eps,
            startup_profile,
            trace_timer_elapsed_seconds(start),
            stats.steps,
            stats.root_hits,
            final_t
        );
    }
    Ok((output.buf, param_values))
}
