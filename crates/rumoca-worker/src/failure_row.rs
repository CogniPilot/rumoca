//! Phase attribution for compile failures recorded in worker result rows.
//!
//! The rendered strict-compile summary is *not* sufficient to classify a
//! failure: parse and resolve failures render as `"{model} could not be
//! compiled: {error}"` with no phase marker at all. Attributing those to the
//! last phase (`ToDae`) silently files them under the balance cohort, which is
//! why this module prefers the structured
//! [`StrictCompileFailure`](rumoca_compile::compile::StrictCompileFailure) and
//! only falls back to marker sniffing for compile lanes that cannot produce it.

use rumoca_compile::compile::StrictCompileFailure;

use crate::WorkerModelResult;

/// Fallback phase attribution from a rendered failure summary.
///
/// A summary with no `failed in <Phase>:` marker never reached a model phase,
/// so the fallback is `Resolve`, not `ToDae`.
pub fn strict_dae_failure_phase(failure_summary: &str) -> &'static str {
    const PHASE_MARKERS: &[(&str, &str)] = &[
        (" failed in Instantiate:", "Instantiate"),
        (" failed in Typecheck:", "Typecheck"),
        (" failed in Flatten:", "Flatten"),
        (" failed in ToDae:", "ToDae"),
    ];
    PHASE_MARKERS
        .iter()
        .find_map(|(marker, phase)| failure_summary.contains(marker).then_some(*phase))
        .unwrap_or("Resolve")
}

/// Build the failure row for a structured strict-compile failure, recording the
/// real phase, the SPEC_0008 error code and (for ED001) the balance breakdown.
pub fn strict_compile_failure_row(
    model_name: &str,
    failure: &StrictCompileFailure,
) -> WorkerModelResult {
    let mut row = WorkerModelResult::phase_failure(
        model_name.to_string(),
        failure.phase_name(),
        failure.summary.clone(),
        failure.error_code.clone(),
    );
    if let Some(detail) = failure.balance_detail.as_ref() {
        let (equations, unknowns) = detail.equations_unknowns();
        row.scalar_equations = Some(equations);
        row.scalar_unknowns = Some(unknowns);
        row.balance = Some(detail.balance());
        row.is_balanced = Some(detail.is_balanced());
        row.balance_detail = Some(detail.clone());
    }
    row
}

/// Build the failure row for a compile lane that only produced a summary
/// string (the `--allow-unbalanced-for-diagnostics` lane).
pub fn summary_only_failure_row(model_name: &str, summary: String) -> WorkerModelResult {
    let phase = strict_dae_failure_phase(&summary);
    WorkerModelResult::phase_failure(model_name.to_string(), phase, summary, None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_compile::analysis::BalanceDetail;
    use rumoca_compile::compile::FailedPhase;

    #[test]
    fn strict_dae_failure_phase_defaults_to_resolve_for_marker_free_summary() {
        // Resolve/parse failures render without a `failed in <Phase>:` marker.
        // Attributing them to ToDae is what put resolve failures into the
        // balance bucket (Modelica.Fluid.Examples.Tanks.ThreeTanks).
        assert_eq!(
            strict_dae_failure_phase(
                "M could not be compiled: unresolved component reference: 'nParallel'"
            ),
            "Resolve"
        );
        assert_eq!(
            strict_dae_failure_phase("M failed in ToDae: unbalanced model: 5 equations"),
            "ToDae"
        );
        assert_eq!(
            strict_dae_failure_phase("M failed in Flatten: bad thing"),
            "Flatten"
        );
        assert_eq!(
            strict_dae_failure_phase("M failed in Instantiate: bad thing"),
            "Instantiate"
        );
        assert_eq!(
            strict_dae_failure_phase("M failed in Typecheck: bad thing"),
            "Typecheck"
        );
    }

    #[test]
    fn strict_compile_failure_row_records_code_and_balance_detail() {
        let detail = BalanceDetail {
            algebraic_unknowns: 5,
            continuous_equations: 3,
            ..BalanceDetail::default()
        };
        let failure = StrictCompileFailure {
            summary: "M failed in ToDae: unbalanced model".to_string(),
            phase: Some(FailedPhase::ToDae),
            error_code: Some("ED001".to_string()),
            balance_detail: Some(Box::new(detail)),
            failures: Vec::new(),
        };
        let row = strict_compile_failure_row("M", &failure);
        assert_eq!(row.phase_reached, "ToDae");
        assert_eq!(row.error_code.as_deref(), Some("ED001"));
        assert_eq!(row.balance, Some(-2));
        assert_eq!(row.is_balanced, Some(false));
        assert_eq!(row.scalar_equations, Some(3));
        assert_eq!(row.scalar_unknowns, Some(5));
        assert_eq!(
            row.balance_detail
                .as_ref()
                .map(|detail| detail.continuous_equations),
            Some(3)
        );
    }

    #[test]
    fn strict_compile_failure_row_reports_resolve_for_pre_phase_failures() {
        let failure = StrictCompileFailure {
            summary: "M could not be compiled: unresolved component reference: 'x'".to_string(),
            phase: None,
            error_code: Some("ER003".to_string()),
            balance_detail: None,
            failures: Vec::new(),
        };
        let row = strict_compile_failure_row("M", &failure);
        assert_eq!(row.phase_reached, "Resolve");
        assert_eq!(row.error_code.as_deref(), Some("ER003"));
        assert!(row.balance_detail.is_none());
        assert!(row.balance.is_none());
    }

    #[test]
    fn summary_only_failure_row_uses_marker_fallback() {
        let row = summary_only_failure_row("M", "M failed in ToDae: boom".to_string());
        assert_eq!(row.phase_reached, "ToDae");
        assert!(row.error_code.is_none());
    }
}
