use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

use super::diagnostics::SimulationDiagnosticError;
use super::initial_values::{evaluation_error, runtime_vectors};

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

/// Lower a model whose inputs a host writes on every dispatch.
///
/// `lower_dae_for_simulation` refuses to build runtime vectors for an input
/// nobody drives, because integrating one would publish a trajectory computed
/// from a value the model never stated. GPU preparation is the case where the
/// driver provably exists: `prepare_gpu_simulation` hands the browser the
/// input's `P` slot index and `packages/rumoca-web/runtime/rumoca_gpu.js`
/// writes that slot from the live input source before each dispatch. What the
/// prepared payload still has to state is the value the slot holds until the
/// first write, and MLS §4.4.2.1 names it: the input's `start` attribute —
/// written on the declaration where the modeler chose it (`input Real
/// throttle(start = 0)`, as every shipped model in `examples/interactive`
/// does) and otherwise the predefined type's own attribute default. Either way
/// it is read out of the checked DAE, not invented here, and an input the
/// checked declaration left without any `start` still fails.
///
/// The seed travels the same override channel a runtime provider uses, so an
/// input carrying a binding keeps its binding and headless simulation
/// (`lower_dae_for_simulation`) keeps refusing an undriven input.
pub fn lower_dae_for_gpu_preparation(
    model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    let host_driven_seeds = host_driven_input_seeds(model)?;
    lower_dae_for_simulation_with_stage_timing_and_param_overrides(
        model,
        opts,
        &host_driven_seeds,
        |_| {},
    )
    .map(|(model, _)| model)
}

/// Pre-write value of every host-driven input scalar, by scalar name.
///
/// Inputs that carry a binding are left out: the binding is already a checked
/// default the runtime vectors evaluate. Inputs the checked DAE gave no `start`
/// are left out too, so they still fail in `runtime_vectors` naming the input,
/// rather than being seeded with a value this function would have to invent.
fn host_driven_input_seeds(
    model: &dae::Dae,
) -> Result<std::collections::HashMap<String, f64>, SimulationDiagnosticError> {
    model.inspect(|view| {
        let mut evaluator = rumoca_phase_dae::numeric::NumericDaeContext::new(view);
        let mut seeds = std::collections::HashMap::new();
        for (_, variable) in view
            .variables()
            .filter(|(_, variable)| variable.role() == dae::VariableRole::Input)
        {
            seed_host_driven_input(&mut evaluator, variable, &mut seeds)?;
        }
        Ok(seeds)
    })
}

fn seed_host_driven_input<'dae>(
    evaluator: &mut rumoca_phase_dae::numeric::NumericDaeContext<'dae>,
    variable: dae::VariableView<'dae>,
    seeds: &mut std::collections::HashMap<String, f64>,
) -> Result<(), SimulationDiagnosticError> {
    if variable.binding().is_some() {
        return Ok(());
    }
    let Some(start) = variable.start() else {
        return Ok(());
    };
    let values = evaluator.expression(start).map_err(evaluation_error)?;
    for scalar in 0..variable.scalar_count() {
        let name = variable.scalar_name(scalar).ok_or_else(|| {
            preparation_error(
                format!(
                    "checked input `{}` has no scalar name at ordinal {scalar}",
                    variable.name()
                ),
                variable,
            )
        })?;
        // One start expression may cover every scalar of an array input
        // (`input Real u[3](start = 0)`), exactly as the runtime vectors
        // broadcast a scalar value across the declared shape.
        let value = match values.as_slice() {
            [single] => *single,
            many => *many.get(scalar).ok_or_else(|| {
                preparation_error(
                    format!(
                        "start value for input `{}` contains {} scalars; expected {}",
                        variable.name(),
                        many.len(),
                        variable.scalar_count()
                    ),
                    variable,
                )
            })?,
        };
        seeds.insert(name, value);
    }
    Ok(())
}

fn preparation_error(
    message: String,
    variable: dae::VariableView<'_>,
) -> SimulationDiagnosticError {
    SimulationDiagnosticError::RuntimePreparation {
        message,
        span: Some(variable.declaration().span()),
    }
}

pub(crate) fn lower_dae_for_simulation_with_stage_timing_and_param_overrides(
    model: &dae::Dae,
    _opts: &SimOptions,
    parameter_overrides: &std::collections::HashMap<String, f64>,
    mut begin_stage: impl FnMut(&'static str),
) -> Result<(solve::SolveModel, crate::BuildSimulationTimings), SimulationDiagnosticError> {
    let mut timings = crate::BuildSimulationTimings::default();

    begin_stage("ir_solve");
    let solve_start = rumoca_core::maybe_start_timer();
    let problem = rumoca_phase_solve::lower_solve_problem(model)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    let artifacts = rumoca_phase_solve::lower_solve_artifacts(&problem)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    timings.ir_solve_lower_seconds = rumoca_core::maybe_elapsed_seconds(solve_start);

    begin_stage("runtime_vectors");
    let vector_start = rumoca_core::maybe_start_timer();
    let vectors = runtime_vectors(model, &problem, parameter_overrides)?;
    timings.ir_solve_structural_dae_seconds = rumoca_core::maybe_elapsed_seconds(vector_start);
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
