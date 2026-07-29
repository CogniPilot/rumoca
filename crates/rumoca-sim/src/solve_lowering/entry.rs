use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

use super::diagnostics::SimulationDiagnosticError;
use super::initial_values::runtime_vectors;

pub fn lower_dae_for_simulation(
    model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    lower_dae_for_simulation_with_stage_timing_and_param_overrides(
        model,
        opts,
        &std::collections::HashMap::new(),
        |_| {},
    )
    .map(|(model, _)| model)
}

pub fn lower_dae_for_gpu_preparation(
    model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    lower_dae_for_simulation(model, opts)
}

pub(crate) fn lower_dae_for_simulation_with_stage_timing_and_param_overrides(
    model: &dae::Dae,
    _opts: &SimOptions,
    parameter_overrides: &std::collections::HashMap<String, f64>,
    mut begin_stage: impl FnMut(&'static str),
) -> Result<(solve::SolveModel, crate::BuildSimulationTimings), SimulationDiagnosticError> {
    let mut timings = crate::BuildSimulationTimings::default();

    begin_stage("ir_solve");
    let solve_start = std::time::Instant::now();
    let problem = rumoca_phase_solve::lower_solve_problem(model)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    let artifacts = rumoca_phase_solve::lower_solve_artifacts(&problem)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    timings.ir_solve_lower_seconds = solve_start.elapsed().as_secs_f64();

    begin_stage("runtime_vectors");
    let vector_start = std::time::Instant::now();
    let vectors = runtime_vectors(model, &problem, parameter_overrides)?;
    timings.ir_solve_structural_dae_seconds = vector_start.elapsed().as_secs_f64();
    timings.ir_solve_seconds =
        timings.ir_solve_lower_seconds + timings.ir_solve_structural_dae_seconds;

    Ok((
        solve::SolveModel {
            problem,
            artifacts,
            initial_y: vectors.initial_y,
            solver_nominals: vectors.solver_nominals,
            parameters: vectors.parameters,
            external_tables: solve::ExternalTables::default(),
            visible_names: vectors.visible_names,
            visible_value_rows: vectors.visible_value_rows,
            variable_meta: vectors.variable_meta,
        },
        timings,
    ))
}
