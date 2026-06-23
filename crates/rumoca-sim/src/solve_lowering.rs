//! DAE → simulation solve-model lowering, organized into cohesive stages:
//!
//! - [`diagnostics`] — the [`SimulationDiagnosticError`] surfaced by every entry.
//! - [`overrides`] — solver-neutral tunable-parameter / state-start overrides.
//! - [`entry`] — the public lowering entry points and per-stage timings.
//! - [`probe`] — the `--inspect eval` / `--inspect jacobian` debug probes.
//! - [`structure_report`] — the `--inspect structure` report and singularity triage.
//! - [`structural_lowering`] — the shared structural preparation + elimination funnel.
//! - [`timing`] / [`expr_util`] — stage-timer and expression helpers shared above.
//!
//! The root keeps only module wiring and a curated set of re-exports so the sim
//! facade (`lib.rs`) and the solver backends keep referring to the same paths.

mod diagnostics;
mod entry;
mod expr_util;
mod overrides;
mod probe;
mod structural_lowering;
mod structure_report;
mod timing;

// Re-exported through the sim facade so the root stays a curated same-crate
// facade (see `architecture_hardening_test::test_sim_facade_cross_crate_exports_are_curated`).
pub use rumoca_eval_solve::{EvalAtReport, EvalAtSlot, JacobianReport};
pub use rumoca_phase_structural::{BlockReport, StructuralReport, TearingReport};

pub use diagnostics::SimulationDiagnosticError;
pub use entry::{
    lower_dae_for_gpu_preparation, lower_dae_for_simulation,
    structurally_lowered_dae_for_simulation_artifact,
};
pub use probe::{EvalAtProbe, JacobianProbe, eval_dae_at, jacobian_for_dae};
pub use structure_report::{
    SingularityDiagnosis, UnmatchedEquationDiagnosis, UnmatchedUnknownDiagnosis,
    diagnose_structural_singularity, structural_report_for_dae,
};

// Only the diffsol build pipeline consumes the staged-timing entry point.
#[cfg(feature = "solver-diffsol")]
pub(crate) use entry::lower_dae_for_simulation_with_stage_timing;
pub(crate) use overrides::{apply_simulation_overrides, lower_for_simulation_with_overrides};

#[cfg(test)]
mod tests;
