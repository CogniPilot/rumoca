use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_ir_solve::RefreshProfile;
use rumoca_solver::SimOptions;

use super::diagnostics::SimulationDiagnosticError;

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

/// Prepare checked pre-write values for a GPU host that drives every input.
pub fn lower_dae_for_gpu_preparation(
    model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    lower_dae_with_host_driven_inputs(model, opts)
}

/// Prepare a checked Solve model for a host-driven native expression kernel.
///
/// This assembles storage and runtime defaults without constructing a solver or
/// advancing time. The host must write every live input before each evaluation;
/// declaration bindings and checked start attributes supply pre-write values.
pub fn lower_dae_for_native_preparation(
    model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    lower_correlated_for_host_driven_inputs(model, opts, RefreshProfile::NativeCompact)
        .map(|lowered| lowered.into_model())
}

/// Lower for a host that writes every input before each evaluation.
///
/// Native/GPU preparation hands the kernel to a driver that owns input values.
/// Checked starts supply pre-write values; missing inputs remain host obligations.
pub(super) fn lower_dae_with_host_driven_inputs(
    model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    lower_correlated_for_host_driven_inputs(model, opts, RefreshProfile::Scalar)
        .map(|lowered| lowered.into_model())
}

/// Native and GPU preparation share this explicit host pre-write contract.
pub(super) fn lower_correlated_for_host_driven_inputs<'source>(
    model: &'source dae::Dae,
    opts: &SimOptions,
    profile: RefreshProfile,
) -> Result<rumoca_phase_solve::LoweredSolveModel<'source>, SimulationDiagnosticError> {
    lower_correlated_for_inputs(
        model,
        opts,
        dae::InputInitializationPolicy::HostDrivenStart,
        profile,
    )
}

#[cfg(feature = "fmi")]
pub(super) fn lower_correlated_for_fmi_export<'source>(
    model: &'source dae::Dae,
    opts: &SimOptions,
    profile: RefreshProfile,
) -> Result<rumoca_phase_solve::LoweredSolveModel<'source>, SimulationDiagnosticError> {
    lower_correlated_for_inputs(
        model,
        opts,
        dae::InputInitializationPolicy::FmiExportDefaults,
        profile,
    )
}

fn lower_correlated_for_inputs<'source>(
    model: &'source dae::Dae,
    opts: &SimOptions,
    input_policy: dae::InputInitializationPolicy,
    profile: RefreshProfile,
) -> Result<rumoca_phase_solve::LoweredSolveModel<'source>, SimulationDiagnosticError> {
    let overrides = super::overrides::tunable_param_overrides(model, opts)?;
    let (mut lowered, _) =
        lower_correlated_with_input_policy(model, opts, &overrides, input_policy, profile, |_| {})?;
    super::overrides::apply_correlated_simulation_overrides(&mut lowered, model, opts)?;
    Ok(lowered)
}

pub(crate) fn lower_dae_for_simulation_with_stage_timing_and_param_overrides(
    model: &dae::Dae,
    opts: &SimOptions,
    parameter_overrides: &std::collections::HashMap<String, f64>,
    begin_stage: impl FnMut(&'static str),
) -> Result<(solve::SolveModel, crate::BuildSimulationTimings), SimulationDiagnosticError> {
    let (lowered, timings) = lower_correlated_for_simulation_with_stage_timing_and_param_overrides(
        model,
        opts,
        parameter_overrides,
        begin_stage,
    )?;
    Ok((lowered.into_model(), timings))
}

/// Lower while retaining the phase-owned DAE/Solve correlation until the
/// caller consumes it into the runtime FMI component.
pub(crate) fn lower_correlated_for_simulation_with_stage_timing_and_param_overrides<'source>(
    model: &'source dae::Dae,
    opts: &SimOptions,
    parameter_overrides: &std::collections::HashMap<String, f64>,
    begin_stage: impl FnMut(&'static str),
) -> Result<
    (
        rumoca_phase_solve::LoweredSolveModel<'source>,
        crate::BuildSimulationTimings,
    ),
    SimulationDiagnosticError,
> {
    lower_correlated_for_simulation_with_profile(
        model,
        opts,
        parameter_overrides,
        RefreshProfile::Scalar,
        begin_stage,
    )
}

/// Lower for the execution family `profile` names (see [`RefreshProfile`]).
pub(crate) fn lower_correlated_for_simulation_with_profile<'source>(
    model: &'source dae::Dae,
    opts: &SimOptions,
    parameter_overrides: &std::collections::HashMap<String, f64>,
    profile: RefreshProfile,
    begin_stage: impl FnMut(&'static str),
) -> Result<
    (
        rumoca_phase_solve::LoweredSolveModel<'source>,
        crate::BuildSimulationTimings,
    ),
    SimulationDiagnosticError,
> {
    lower_correlated_with_input_policy(
        model,
        opts,
        parameter_overrides,
        dae::InputInitializationPolicy::default(),
        profile,
        begin_stage,
    )
}

fn lower_correlated_with_input_policy<'source>(
    model: &'source dae::Dae,
    opts: &SimOptions,
    parameter_overrides: &std::collections::HashMap<String, f64>,
    input_policy: dae::InputInitializationPolicy,
    profile: RefreshProfile,
    mut begin_stage: impl FnMut(&'static str),
) -> Result<
    (
        rumoca_phase_solve::LoweredSolveModel<'source>,
        crate::BuildSimulationTimings,
    ),
    SimulationDiagnosticError,
> {
    let mut overrides = parameter_overrides.clone();
    overrides.extend(super::overrides::initial_input_values(model, opts)?);
    let lowered = rumoca_phase_solve::lower_solve_model_for_profile(
        model,
        &overrides,
        input_policy,
        profile,
        |stage| match stage {
            rumoca_phase_solve::SolveModelLoweringStage::Programs => begin_stage("ir_solve"),
            rumoca_phase_solve::SolveModelLoweringStage::RuntimeValues => {
                begin_stage("runtime_vectors");
            }
        },
    )
    .map_err(model_lowering_error)?;
    report_unlocalizable_guards(&lowered.model().problem.continuous.unlocalizable_guards);
    let timings = crate::BuildSimulationTimings {
        ir_solve_seconds: lowered.program_seconds() + lowered.runtime_value_seconds(),
        ir_solve_structural_dae_seconds: lowered.runtime_value_seconds(),
        ir_solve_lower_seconds: lowered.program_seconds(),
        ..crate::BuildSimulationTimings::default()
    };
    Ok((lowered, timings))
}

/// Warn, before any integration, about every ES016 fact of the Solve model
/// (SPEC_0044 ME-EVENT-008): a block whose own unknowns a relation under
/// `noEvent` switches fails with a typed fold error where its branch ends.
pub(super) fn report_unlocalizable_guards(guards: &[solve::UnlocalizableGuard]) {
    for guard in guards {
        eprintln!("warning[ES016]: {}", guard.warning());
    }
}

pub(super) fn model_lowering_error(
    error: rumoca_phase_solve::SolveModelLoweringError,
) -> SimulationDiagnosticError {
    match error {
        rumoca_phase_solve::SolveModelLoweringError::Lower(error) => {
            SimulationDiagnosticError::SolveLowering(error)
        }
        rumoca_phase_solve::SolveModelLoweringError::RuntimeValues { message, span } => {
            SimulationDiagnosticError::RuntimePreparation { message, span }
        }
        rumoca_phase_solve::SolveModelLoweringError::InvalidOverride { message } => {
            SimulationDiagnosticError::InvalidOverride { message }
        }
    }
}
