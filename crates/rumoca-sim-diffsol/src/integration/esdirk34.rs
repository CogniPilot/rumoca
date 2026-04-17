use super::{
    DiffsolBackend, IntegrationRunInput, PreparedIntegrationLoop,
    configure_solver_problem_with_profile, prepare_integration_loop,
};
use crate::{
    LS, OutputBuffers, SimError, SolverStartupProfile, problem, sim_trace_enabled,
    trace_timer_elapsed_seconds, trace_timer_start_if,
};
pub(crate) fn try_integrate_esdirk34(
    input: &IntegrationRunInput<'_>,
    eps: f64,
    startup_profile: SolverStartupProfile,
) -> Result<(OutputBuffers, Vec<f64>), SimError> {
    let trace_enabled = sim_trace_enabled();
    let start = trace_timer_start_if(trace_enabled);
    input.budget.check()?;
    let mut problem = problem::build_problem_with_params(
        input.dae,
        input.opts.rtol,
        input.opts.atol,
        eps,
        input.mass_matrix,
        input.param_values,
    )?;
    configure_solver_problem_with_profile(&mut problem, input.opts, startup_profile);
    if trace_enabled {
        eprintln!(
            "[sim-trace] ESDIRK34 start eps={} profile={:?} h0={} max_wall={:?}",
            eps, startup_profile, problem.h0, input.opts.max_wall_seconds
        );
    }
    let mut solver = problem
        .esdirk34::<LS>()
        .map_err(|e| SimError::SolverError(format!("Failed to create ESDIRK34 solver: {e}")))?;
    let PreparedIntegrationLoop {
        param_values,
        output,
        ctx,
        solver_names,
    } = prepare_integration_loop(&mut solver, input, startup_profile)?;
    let (output, stats, final_t) = {
        let mut backend = DiffsolBackend::new(solver, output, ctx, None, solver_names)?;
        let stats = match rumoca_sim::run_with_runtime_schedule(
            &mut backend,
            input.dae,
            input.opts.t_start,
            input.opts.t_end,
            || input.budget.check().map_err(SimError::from),
        ) {
            Ok(stats) => stats,
            Err(err) => {
                let final_t = rumoca_sim::SimulationBackend::read_state(&backend).t;
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
        let final_t = rumoca_sim::SimulationBackend::read_state(&backend).t;
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
