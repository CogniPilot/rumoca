//! The "too slow" / "failed slowly" split, rendered into the pass-rate report.
//!
//! A per-phase kill is recorded as `sim_timeout`, but only some of those models
//! are actually out of time: the rest were still working towards a hard
//! diagnostic when the monitor stopped them, and the larger-budget re-check
//! (see `balance_pipeline_core::timeout_recheck`) recovers the error they
//! produce. Reading the pass-rate tables alone, the two look identical, so this
//! section names every re-checked model, its verdict, and the code it reached.

use super::*;

/// One re-checked model: what the primary budget killed, and what a larger
/// budget then established.
#[derive(Debug, Clone, Serialize)]
pub(super) struct MslTimeoutClassificationRow {
    pub(super) model: String,
    pub(super) package: String,
    /// `MSL_TIMEOUT_RECHECK_*`: `diagnostic` is "failed slowly", everything
    /// else leaves the model reported as a timeout.
    pub(super) outcome: String,
    pub(super) killed_phase: Option<String>,
    pub(super) diagnostic_code: Option<String>,
    pub(super) diagnostic_phase: Option<String>,
    pub(super) sim_status: Option<String>,
    pub(super) primary_budget_seconds: f64,
    pub(super) primary_elapsed_seconds: f64,
    pub(super) recheck_budget_seconds: f64,
    pub(super) recheck_elapsed_seconds: f64,
}

pub(super) fn build_timeout_classification_rows(
    summary: &MslSummary,
) -> Vec<MslTimeoutClassificationRow> {
    let mut rows = summary
        .model_results
        .iter()
        .filter_map(timeout_classification_row)
        .collect::<Vec<_>>();
    rows.sort_by(|left, right| {
        left.outcome
            .cmp(&right.outcome)
            .then_with(|| left.model.cmp(&right.model))
    });
    rows
}

fn timeout_classification_row(result: &MslModelResult) -> Option<MslTimeoutClassificationRow> {
    let recheck = result.timeout_recheck.as_ref()?;
    Some(MslTimeoutClassificationRow {
        model: result.model_name.clone(),
        package: root_msl_package_name(&result.model_name)
            .unwrap_or_else(|| result.model_name.clone()),
        outcome: recheck.outcome.clone(),
        killed_phase: recheck.killed_phase.map(|phase| phase.to_string()),
        diagnostic_code: recheck.diagnostic_code.clone(),
        diagnostic_phase: recheck.diagnostic_phase.clone(),
        sim_status: result.sim_status.clone(),
        primary_budget_seconds: recheck.primary_budget_seconds,
        primary_elapsed_seconds: recheck.primary_elapsed_seconds,
        recheck_budget_seconds: recheck.recheck_budget_seconds,
        recheck_elapsed_seconds: recheck.recheck_elapsed_seconds,
    })
}

/// Per-package "failed slowly" count, for the pass-rate rows.
pub(super) fn slow_failure_counts_by_package(
    rows: &[MslTimeoutClassificationRow],
) -> BTreeMap<&str, usize> {
    let mut counts = BTreeMap::new();
    for row in rows.iter().filter(|row| row.is_slow_failure()) {
        *counts.entry(row.package.as_str()).or_insert(0) += 1;
    }
    counts
}

impl MslTimeoutClassificationRow {
    pub(super) fn is_slow_failure(&self) -> bool {
        self.outcome == MSL_TIMEOUT_RECHECK_DIAGNOSTIC
    }
}

pub(super) fn format_timeout_classification_markdown(
    rows: &[MslTimeoutClassificationRow],
) -> String {
    if rows.is_empty() {
        return String::new();
    }
    let mut markdown = String::from("\n## Phase-kill re-checks\n\n");
    markdown.push_str(&timeout_classification_legend(rows));
    markdown.push_str(
        "\n| Model | Killed in | Verdict | Code | Diagnostic phase | Status | Primary s | Re-check s |\n",
    );
    markdown.push_str("|---|---|---|---|---|---:|---:|---:|\n");
    for row in rows {
        markdown.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {:.1}/{:.1} | {:.1}/{:.1} |\n",
            row.model,
            cell(row.killed_phase.as_deref()),
            row.outcome,
            cell(row.diagnostic_code.as_deref()),
            cell(row.diagnostic_phase.as_deref()),
            cell(row.sim_status.as_deref()),
            row.primary_elapsed_seconds,
            row.primary_budget_seconds,
            row.recheck_elapsed_seconds,
            row.recheck_budget_seconds,
        ));
    }
    markdown
}

pub(super) fn format_timeout_classification_text(rows: &[MslTimeoutClassificationRow]) -> String {
    if rows.is_empty() {
        return String::new();
    }
    let mut text = String::from("\n=== Phase-kill re-checks ===\n");
    text.push_str(&timeout_classification_legend(rows));
    for row in rows {
        text.push_str(&format!(
            "  {:<10} {:<52} killed in {:<8} code={:<8} status={} ({:.1}s of {:.1}s primary, {:.1}s of {:.1}s re-check)\n",
            row.outcome,
            row.model,
            cell(row.killed_phase.as_deref()),
            cell(row.diagnostic_code.as_deref()),
            cell(row.sim_status.as_deref()),
            row.primary_elapsed_seconds,
            row.primary_budget_seconds,
            row.recheck_elapsed_seconds,
            row.recheck_budget_seconds,
        ));
    }
    text
}

fn timeout_classification_legend(rows: &[MslTimeoutClassificationRow]) -> String {
    let slow_failures = rows.iter().filter(|row| row.is_slow_failure()).count();
    format!(
        "{} model(s) were killed inside a compiler phase and re-run once under a larger budget; {} of them reached a real diagnostic (failed slowly) and are reported under that failure, the other {} stay reported as timeouts (too slow).\n",
        rows.len(),
        slow_failures,
        rows.len() - slow_failures,
    )
}

fn cell(value: Option<&str>) -> &str {
    value.unwrap_or("-")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn recheck(outcome: &str, diagnostic_code: Option<&str>) -> MslTimeoutRecheck {
        MslTimeoutRecheck {
            outcome: outcome.to_string(),
            killed_phase: Some(rumoca_worker::WorkerProgressPhase::Solve),
            primary_budget_seconds: 45.0,
            primary_elapsed_seconds: 87.5,
            recheck_budget_seconds: 180.0,
            recheck_elapsed_seconds: 61.0,
            diagnostic_code: diagnostic_code.map(str::to_string),
            diagnostic_phase: diagnostic_code.map(|_| "Solve".to_string()),
            detail: None,
        }
    }

    fn rechecked_model(
        name: &str,
        sim_status: &str,
        outcome: &str,
        diagnostic_code: Option<&str>,
    ) -> MslModelResult {
        let mut result = phase_error_result(name.to_string(), "Success", None, None);
        result.error = None;
        result.sim_status = Some(sim_status.to_string());
        result.timeout_recheck = Some(recheck(outcome, diagnostic_code));
        result
    }

    fn summary_with_rechecks() -> MslSummary {
        let mut summary = empty_summary(0, 0);
        summary.model_results = vec![
            rechecked_model(
                "Modelica.Electrical.QuasiStatic.Machines.Examples.TransformerTestbench",
                "sim_solver_fail",
                MSL_TIMEOUT_RECHECK_DIAGNOSTIC,
                Some("ES010"),
            ),
            rechecked_model(
                "Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1",
                "sim_timeout",
                MSL_TIMEOUT_RECHECK_TIMEOUT,
                None,
            ),
            {
                let mut untouched = phase_error_result(
                    "Modelica.Blocks.Examples.BooleanNetwork1".to_string(),
                    "Success",
                    None,
                    None,
                );
                untouched.error = None;
                untouched.sim_status = Some("sim_ok".to_string());
                untouched
            },
        ];
        summary
    }

    #[test]
    fn only_re_checked_models_appear_in_the_classification() {
        let rows = build_timeout_classification_rows(&summary_with_rechecks());

        assert_eq!(rows.len(), 2, "a model that was never killed has no row");
        assert_eq!(rows[0].outcome, MSL_TIMEOUT_RECHECK_DIAGNOSTIC);
        assert_eq!(rows[0].diagnostic_code.as_deref(), Some("ES010"));
        assert_eq!(rows[0].sim_status.as_deref(), Some("sim_solver_fail"));
        assert_eq!(rows[0].package, "Electrical.QuasiStatic.Machines");
        assert_eq!(rows[1].outcome, MSL_TIMEOUT_RECHECK_TIMEOUT);
        assert_eq!(rows[1].sim_status.as_deref(), Some("sim_timeout"));
    }

    #[test]
    fn slow_failures_are_counted_per_package() {
        let rows = build_timeout_classification_rows(&summary_with_rechecks());
        let counts = slow_failure_counts_by_package(&rows);

        assert_eq!(
            counts.get("Electrical.QuasiStatic.Machines").copied(),
            Some(1)
        );
        assert_eq!(
            counts.get("Mechanics.MultiBody").copied(),
            None,
            "a model that is still out of time is not a slow failure"
        );
    }

    #[test]
    fn rendered_sections_name_both_cohorts_and_the_recovered_code() {
        let rows = build_timeout_classification_rows(&summary_with_rechecks());
        let markdown = format_timeout_classification_markdown(&rows);
        let text = format_timeout_classification_text(&rows);

        for rendered in [&markdown, &text] {
            assert!(rendered.contains("ES010"));
            assert!(rendered.contains("2 model(s) were killed"));
            assert!(rendered.contains("1 of them reached a real diagnostic"));
        }
        assert!(markdown.contains("## Phase-kill re-checks"));
        assert!(text.contains("=== Phase-kill re-checks ==="));
    }

    #[test]
    fn a_run_without_phase_kills_renders_nothing() {
        let rows = build_timeout_classification_rows(&empty_summary(0, 0));

        assert!(rows.is_empty());
        assert!(format_timeout_classification_markdown(&rows).is_empty());
        assert!(format_timeout_classification_text(&rows).is_empty());
    }
}
