//! Checked DAE → simulation Solve-model assembly.
//!
//! The phase-solve boundary produces computable register programs. This module
//! adds runtime vectors by evaluating checked variable attributes and rejects
//! any value it cannot establish; it never rewrites the DAE or substitutes a
//! guessed value.

mod diagnostics;
mod entry;
mod initial_values;
mod overrides;
mod probe;
mod structure_report;

// Re-exported through the sim facade so the root stays a curated same-crate
// facade (see `architecture_hardening_test::test_sim_facade_cross_crate_exports_are_curated`).
pub use rumoca_phase_structural::{BlockReport, StructuralReport, TearingReport};
pub use rumoca_solver::{EvalAtReport, EvalAtSlot, JacobianReport};

pub use diagnostics::SimulationDiagnosticError;
pub use entry::{lower_dae_for_gpu_preparation, lower_dae_for_simulation};
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
pub(crate) use entry::lower_dae_for_simulation_with_stage_timing_and_param_overrides;
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub(crate) use overrides::{apply_simulation_overrides, tunable_param_overrides};
pub use overrides::{
    lower_for_differentiation_with_overrides, lower_for_simulation_with_overrides,
};

#[cfg(test)]
mod tests;
