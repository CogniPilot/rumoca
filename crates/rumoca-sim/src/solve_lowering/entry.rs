//! Public lowering entry points: turn a flattened DAE into a simulation-ready
//! [`solve::SolveModel`] (or the GPU-preparation / structural-artifact variants),
//! plus the per-stage timing that the diffsol build pipeline reports.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::{SimOptions, SimSolverMode};

use super::structural_lowering::{
    metadata_attachment_lower_error, structurally_lower_dae_for_simulation,
};
use super::timing::{
    log_solve_lowering_done, log_solve_lowering_start, stage_timer_elapsed_seconds,
    stage_timer_start,
};

pub fn lower_dae_for_simulation(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, rumoca_phase_solve::SolveModelLowerError> {
    lower_dae_for_simulation_with_stage_timing(dae_model, opts, |_| {}).map(|(model, _)| model)
}

pub fn lower_dae_for_gpu_preparation(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, rumoca_phase_solve::SolveModelLowerError> {
    if let Some(solve_model) = lower_direct_dae_for_gpu_preparation(dae_model)? {
        return Ok(solve_model);
    }
    let structurally_lowered = structurally_lower_dae_for_simulation(dae_model, opts)?;
    rumoca_phase_solve::lower_dae_to_solve_model_owned_for_gpu_preparation_with_metadata(
        structurally_lowered.dae,
        &structurally_lowered.metadata_dae,
    )
}

fn lower_direct_dae_for_gpu_preparation(
    dae_model: &dae::Dae,
) -> Result<Option<solve::SolveModel>, rumoca_phase_solve::SolveModelLowerError> {
    let mut metadata_dae = dae_model.clone();
    rumoca_phase_dae::attach_dae_reference_metadata(&mut metadata_dae)
        .map_err(metadata_attachment_lower_error)?;
    let lowered = metadata_dae.clone();
    match rumoca_phase_solve::lower_dae_to_solve_model_owned_for_gpu_preparation_with_metadata(
        lowered,
        &metadata_dae,
    ) {
        Ok(solve_model) => Ok(Some(solve_model)),
        Err(_) => Ok(None),
    }
}

/// Lower for simulation while applying tunable scalar-parameter overrides during
/// parameter-value computation, so parameter-derived quantities (including array
/// masks such as the airfoil's `sc/nc/sig`) re-derive from the override at
/// parameter-set time instead of being baked at the declared default. Pass an
/// empty map for the default behavior.
pub(crate) fn lower_dae_for_simulation_with_param_overrides(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    param_overrides: &std::collections::HashMap<String, f64>,
) -> Result<solve::SolveModel, rumoca_phase_solve::SolveModelLowerError> {
    let structurally_lowered = structurally_lower_dae_for_simulation(dae_model, opts)?;
    lower_structured_dae_for_simulation(structurally_lowered, opts, param_overrides)
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct SolveLoweringTimings {
    pub structural_dae_seconds: f64,
    pub solve_ir_seconds: f64,
}

impl SolveLoweringTimings {
    pub(crate) fn total_seconds(self) -> f64 {
        self.structural_dae_seconds + self.solve_ir_seconds
    }
}

pub(crate) fn lower_dae_for_simulation_with_stage_timing(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    mut begin_stage: impl FnMut(&'static str),
) -> Result<(solve::SolveModel, SolveLoweringTimings), rumoca_phase_solve::SolveModelLowerError> {
    let mut timings = SolveLoweringTimings::default();

    begin_stage("ir_solve_structural_dae");
    let structural_start = stage_timer_start();
    let structurally_lowered = structurally_lower_dae_for_simulation(dae_model, opts)?;
    timings.structural_dae_seconds = stage_timer_elapsed_seconds(structural_start);

    begin_stage("ir_solve");
    log_solve_lowering_start("solve_ir.lower_dae_to_solve_model");
    let solve_ir_start = stage_timer_start();
    let solve_model = lower_structured_dae_for_simulation(
        structurally_lowered,
        opts,
        &std::collections::HashMap::new(),
    )?;
    timings.solve_ir_seconds = stage_timer_elapsed_seconds(solve_ir_start);
    log_solve_lowering_done("solve_ir.lower_dae_to_solve_model", solve_ir_start);
    if tracing::enabled!(target: "rumoca_phase_structural", tracing::Level::DEBUG) {
        let layout = &solve_model.problem.layout;
        let mut names_by_y: std::collections::HashMap<usize, &str> =
            std::collections::HashMap::new();
        for (name, slot) in layout.bindings() {
            if let solve::ScalarSlot::Y { index, .. } = slot {
                names_by_y.insert(*index, name.as_str());
            }
        }
        for (row, target) in solve_model
            .problem
            .continuous
            .implicit_row_targets
            .iter()
            .enumerate()
        {
            let label = match target {
                Some(solve::ScalarSlot::Y { index, .. }) => format!(
                    "Y[{index}] {}",
                    names_by_y.get(&{ *index }).copied().unwrap_or("?")
                ),
                Some(other) => format!("{other:?}"),
                None => "RESIDUAL-ONLY".to_string(),
            };
            tracing::debug!(
                target: "rumoca_phase_structural",
                "[sim-trace] solve row {row} -> {label}"
            );
        }
        for (block_idx, block) in solve_model
            .problem
            .continuous
            .algebraic_projection_plan
            .blocks
            .iter()
            .enumerate()
        {
            tracing::debug!(
                target: "rumoca_phase_structural",
                "[sim-trace] projection block {block_idx}: rows={:?} y_indices={:?} ({}) causal_steps={}",
                block.rows,
                block.y_indices,
                block
                    .y_indices
                    .iter()
                    .map(|idx| names_by_y.get(idx).copied().unwrap_or("?"))
                    .collect::<Vec<_>>()
                    .join(", "),
                block.causal_steps.len()
            );
        }
    }
    Ok((solve_model, timings))
}

fn lower_structured_dae_for_simulation(
    structurally_lowered: super::structural_lowering::StructurallyLoweredDae,
    opts: &SimOptions,
    param_overrides: &std::collections::HashMap<String, f64>,
) -> Result<solve::SolveModel, rumoca_phase_solve::SolveModelLowerError> {
    match opts.solver_mode {
        SimSolverMode::RkLike => {
            rumoca_phase_solve::lower_dae_to_solve_model_owned_value_only_with_visible_expressions_and_metadata_and_overrides(
                structurally_lowered.dae,
                structurally_lowered.visible_expressions,
                &structurally_lowered.metadata_dae,
                param_overrides,
            )
        }
        SimSolverMode::Auto | SimSolverMode::Bdf => {
            rumoca_phase_solve::lower_dae_to_solve_model_owned_with_visible_expressions_and_metadata_and_overrides(
                structurally_lowered.dae,
                structurally_lowered.visible_expressions,
                &structurally_lowered.metadata_dae,
                param_overrides,
            )
        }
    }
}

pub fn structurally_lowered_dae_for_simulation_artifact(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<dae::Dae, rumoca_phase_solve::SolveModelLowerError> {
    structurally_lower_dae_for_simulation(dae_model, opts).map(|lowered| lowered.dae)
}
