//! Checked DAE → simulation Solve-model assembly.
//!
//! The phase-solve boundary produces computable register programs. This module
//! adds runtime vectors by evaluating checked variable attributes and rejects
//! any value it cannot establish; it never rewrites the DAE or substitutes a
//! guessed value.

mod diagnostics;
mod entry;
mod fmi;
mod overrides;
mod probe;
mod structure_report;

// Re-exported through the sim facade so the root stays a curated same-crate
// facade (see `architecture_hardening_test::test_sim_facade_cross_crate_exports_are_curated`).
pub use rumoca_phase_structural::{BlockReport, StructuralReport, TearingReport};
pub use rumoca_solver::{EvalAtReport, EvalAtSlot, JacobianReport};

pub use diagnostics::SimulationDiagnosticError;
pub use entry::lower_dae_for_simulation;
#[cfg(feature = "fmi")]
pub use fmi::lower_fmi_component;
pub use probe::{
    EvalAtProbe, JacobianProbe, ObjectiveGradientProbe, ParameterJacobianProbe,
    StateAndParameterJacobianProbe, SteadyStateSensitivityProbe, eval_dae_at, jacobian_for_dae,
    parameter_jacobian_for_dae, state_and_parameter_jacobian_for_dae,
    steady_state_adjoint_objective_gradient_for_dae, steady_state_objective_gradient_for_dae,
    steady_state_parameter_sensitivity_for_dae,
};
pub use structure_report::{
    SingularityDiagnosis, UnmatchedEquationDiagnosis, UnmatchedUnknownDiagnosis,
    diagnose_structural_singularity, structural_report_for_dae,
};

#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub(crate) use entry::lower_correlated_for_simulation_with_stage_timing_and_runtime_overrides;
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub(crate) use fmi::{finish_runtime_fmi_artifact, lower_runtime_fmi_artifact};
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub(crate) use overrides::construction_overrides;
pub use overrides::{
    lower_correlated_for_simulation_with_overrides, lower_for_differentiation_with_overrides,
    lower_for_simulation_with_overrides,
};

#[cfg(all(test, any(feature = "solver-diffsol", feature = "solver-rk45")))]
mod tests;
