//! Checked DAE → simulation Solve-model assembly.
//!
//! The phase-solve boundary produces computable register programs. This module
//! adds runtime vectors by evaluating checked variable attributes and rejects
//! any value it cannot establish; it never rewrites the DAE or substitutes a
//! guessed value.

//!
//! Lowering is the `lowering` feature (rumoca-phase-solve). The compiled
//! guest builds without it: it loads a published product and keeps only the
//! diagnostics every simulation entry point reports through.

mod diagnostics;
#[cfg(feature = "lowering")]
mod entry;
#[cfg(all(
    feature = "lowering",
    any(feature = "fmi", feature = "solver-diffsol", feature = "solver-rk45")
))]
mod fmi;
#[cfg(feature = "lowering")]
mod linearization;
#[cfg(feature = "lowering")]
mod overrides;
#[cfg(feature = "lowering")]
mod probe;
#[cfg(feature = "lowering")]
mod structure_report;

// Re-exported through the sim facade so the root stays a curated same-crate
// facade (see `architecture_hardening_test::test_sim_facade_cross_crate_exports_are_curated`).
pub use rumoca_phase_structural::{BlockReport, StructuralReport, TearingReport};
pub use rumoca_solver::{EvalAtReport, EvalAtSlot, JacobianReport};

// The parameter-selection decision is owned by the Solve lowering phase.
pub use rumoca_ir_solve::{ExcludedParameter, ExclusionReason, ParameterClassification};
#[cfg(feature = "lowering")]
pub use rumoca_phase_solve::{independent_tunable_parameters, select_sensitivity_parameters};

pub use diagnostics::{
    EX010_SESSION_PROTOCOL_VERSION, EX011_SESSION_MALFORMED_COMMAND,
    EX012_SESSION_INVALID_ARGUMENT, SimulationDiagnosticError,
};
#[cfg(feature = "lowering")]
pub use entry::{
    lower_dae_for_gpu_preparation, lower_dae_for_native_preparation, lower_dae_for_simulation,
};
#[cfg(all(
    target_arch = "wasm32",
    feature = "lowering",
    feature = "precompiled-runtime",
    feature = "solver-rk45"
))]
pub(crate) use fmi::finish_component;
#[cfg(all(feature = "lowering", feature = "fmi"))]
pub use fmi::{
    lower_fmi_c_export_component, lower_fmi_component, lower_fmi_export_component,
    lower_fmi_scalar_export_component,
};
#[cfg(feature = "lowering")]
pub use linearization::{Linearization, linearization_for_dae};
#[cfg(feature = "lowering")]
pub use probe::{
    EvalAtProbe, JacobianProbe, ObjectiveGradientProbe, ParameterJacobianProbe,
    StateAndParameterJacobianProbe, SteadyStateSensitivityProbe, eval_dae_at, jacobian_for_dae,
    parameter_jacobian_for_dae, state_and_parameter_jacobian_for_dae,
    steady_state_adjoint_objective_gradient_for_dae, steady_state_objective_gradient_for_dae,
    steady_state_parameter_sensitivity_for_dae,
};
#[cfg(feature = "lowering")]
pub use structure_report::{
    SingularityDiagnosis, UnmatchedEquationDiagnosis, UnmatchedUnknownDiagnosis,
    diagnose_structural_singularity, structural_report_for_dae,
};

#[cfg(all(
    feature = "lowering",
    any(feature = "solver-diffsol", feature = "solver-rk45")
))]
pub(crate) use entry::lower_correlated_for_simulation_with_stage_timing_and_param_overrides;
#[cfg(all(
    feature = "lowering",
    any(feature = "solver-diffsol", feature = "solver-rk45")
))]
pub(crate) use fmi::{finish_runtime_fmi_artifact, lower_runtime_fmi_artifact};
#[cfg(all(
    feature = "lowering",
    any(feature = "solver-diffsol", feature = "solver-rk45")
))]
pub(crate) use overrides::{apply_correlated_simulation_overrides, tunable_param_overrides};
#[cfg(feature = "lowering")]
pub use overrides::{
    lower_correlated_for_native_execution_with_overrides,
    lower_correlated_for_simulation_with_overrides, lower_for_differentiation_with_overrides,
    lower_for_simulation_with_overrides,
};

#[cfg(all(test, feature = "lowering"))]
mod tests;
