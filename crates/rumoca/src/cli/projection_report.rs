//! The `rumoca sim` summary of a run's projection fallbacks (SPEC_0044
//! ME-PROJ-003).

use rumoca_sim::{ProjectionFallbackCounts, ProjectionFallbackReport, SimulationRunMetrics};

/// Simulate `dae` (re-running with NaN tracing on a non-finite-suggestive
/// failure) with the projection fallback counts reset first.
pub(super) fn simulate(
    dae: &rumoca_compile::compile::Dae,
    opts: &rumoca_sim::SimOptions,
) -> Result<rumoca_sim::SimResult, rumoca_sim::SimulationDiagnosticError> {
    rumoca_sim::reset_projection_fallbacks();
    rumoca_sim::simulate_with_diagnostics_auto_nan_trace(dae, opts)
}

/// Summarize the run's projection fallbacks, warn about every projection site
/// whose fallback rate exceeds the policy threshold, and return the counts.
pub(super) fn report_projection_fallbacks() -> ProjectionFallbackReport {
    let report = rumoca_sim::projection_fallbacks();
    let calls: u64 = report.sites.values().map(|counts| counts.calls).sum();
    let fallbacks: u64 = report
        .sites
        .values()
        .map(ProjectionFallbackCounts::total)
        .sum();
    eprintln!(
        "Projection fallbacks: {fallbacks} over {calls} projection calls at {} sites",
        report.sites.len()
    );
    for warning in report.warnings() {
        eprintln!("warning: {warning}");
    }
    report
}

/// The run metrics a report carries: the projection fallback counts.
pub(super) fn run_metrics(projection_fallbacks: ProjectionFallbackReport) -> SimulationRunMetrics {
    SimulationRunMetrics {
        projection_fallbacks: Some(projection_fallbacks),
        ..SimulationRunMetrics::default()
    }
}
