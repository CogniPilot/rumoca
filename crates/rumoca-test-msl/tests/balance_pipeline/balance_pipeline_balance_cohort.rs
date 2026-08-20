//! Derived balance-failure cohort for `msl_results.json`.
//!
//! Before this section existed the MSL harness could not tell a balance
//! failure from any other ToDae failure: the worker hard-coded `error_code:
//! None` and re-derived the phase by sniffing the summary text, so
//! parse/resolve failures (which render without a phase marker) were counted
//! into the ToDae bucket. This module turns the now-structured `error_code` +
//! `balance_detail` fields into an explicit, measured cohort so the roadmap
//! question ("is the ToDae gap a balance cohort?") is answered by data.

use super::MslModelResult;
use rumoca_compile::compile::core::split_first_top_level;
use serde::{Deserialize, Serialize};

/// The SPEC_0008 code for an unbalanced model (`rumoca::todae::ED001`).
pub(super) const BALANCE_ERROR_CODE: &str = "ED001";

/// One measured balance (ED001) failure.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct BalanceFailureRecord {
    pub model_name: String,
    /// Top-level MSL package, e.g. `Modelica.Fluid`.
    pub package: String,
    pub balance: i64,
    pub equations: usize,
    pub unknowns: usize,
    pub state_unknowns: usize,
    pub algebraic_unknowns: usize,
    pub output_unknowns: usize,
    pub discrete_real_unknowns: usize,
    pub discrete_value_unknowns: usize,
    pub continuous_equations: usize,
    pub discrete_real_equations: usize,
    pub discrete_value_definitions: usize,
    /// One-line exact phase-owned component breakdown.
    pub detail: String,
    /// Command that reproduces this single failure with full instrumentation.
    pub reproduction: String,
}

/// Aggregate view of the measured balance cohort.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(super) struct BalanceFailureCohort {
    /// Failures whose error code resolves to `ED001`.
    #[serde(default)]
    pub records: Vec<BalanceFailureRecord>,
    /// Every row recorded under `phase_reached == "ToDae"`, balance or not.
    #[serde(default)]
    pub todae_failures: usize,
    /// How many of those are actually balance (ED001) failures.
    #[serde(default)]
    pub balance_failures: usize,
    /// ToDae failures by SPEC_0008 error code (`<unknown>` when absent).
    #[serde(default)]
    pub todae_error_code_counts: std::collections::BTreeMap<String, usize>,
    /// Balance failures per top-level MSL package.
    #[serde(default)]
    pub balance_failures_by_package: std::collections::BTreeMap<String, usize>,
}

/// Normalize a miette-namespaced code (`rumoca::todae::ED001`) to `ED001`.
pub(super) fn short_error_code(code: &str) -> &str {
    match code.rsplit("::").next() {
        Some(short) if !short.is_empty() => short,
        _ => code,
    }
}

/// Top-level MSL package for a fully qualified model name.
///
/// `Modelica.Fluid.Examples.Tanks.ThreeTanks` -> `Modelica.Fluid`.
pub(super) fn model_package(model_name: &str) -> String {
    // Model paths are tokenized with the shared top-level helpers so bracketed
    // subscripts never split a name (SPEC_0021 architecture-hardening rule:
    // no direct dot tokenization of Modelica paths).
    let Some((first, rest)) = split_first_top_level(model_name) else {
        return model_name.to_string();
    };
    match split_first_top_level(rest) {
        Some((second, _)) => format!("{first}.{second}"),
        None => format!("{first}.{rest}"),
    }
}

fn is_balance_failure(result: &MslModelResult) -> bool {
    result
        .error_code
        .as_deref()
        .map(short_error_code)
        .is_some_and(|code| code == BALANCE_ERROR_CODE)
}

fn balance_failure_record(result: &MslModelResult) -> Option<BalanceFailureRecord> {
    let detail = result.balance_detail.as_deref()?;
    let (equations, unknowns) = detail.equations_unknowns();
    Some(BalanceFailureRecord {
        model_name: result.model_name.clone(),
        package: model_package(&result.model_name),
        balance: detail.balance(),
        equations,
        unknowns,
        state_unknowns: detail.state_unknowns,
        algebraic_unknowns: detail.algebraic_unknowns,
        output_unknowns: detail.output_unknowns,
        discrete_real_unknowns: detail.discrete_real_unknowns,
        discrete_value_unknowns: detail.discrete_value_unknowns,
        continuous_equations: detail.continuous_equations,
        discrete_real_equations: detail.discrete_real_equations,
        discrete_value_definitions: detail.discrete_value_definitions,
        detail: rumoca_compile::analysis::BalanceBreakdown::from(detail.clone()).to_string(),
        reproduction: format!(
            "cargo run -p rumoca-test-msl --bin rumoca-msl-tools -- debug-model --model {}",
            result.model_name
        ),
    })
}

/// Build the measured balance cohort from the per-model results.
pub(super) fn build_balance_failure_cohort(results: &[MslModelResult]) -> BalanceFailureCohort {
    let mut cohort = BalanceFailureCohort::default();
    for result in results {
        if result.phase_reached == "ToDae" {
            cohort.todae_failures += 1;
            let code = result
                .error_code
                .as_deref()
                .map(short_error_code)
                .unwrap_or("<unknown>")
                .to_string();
            *cohort.todae_error_code_counts.entry(code).or_insert(0) += 1;
        }
        if !is_balance_failure(result) {
            continue;
        }
        cohort.balance_failures += 1;
        *cohort
            .balance_failures_by_package
            .entry(model_package(&result.model_name))
            .or_insert(0) += 1;
        if let Some(record) = balance_failure_record(result) {
            cohort.records.push(record);
        }
    }
    cohort
        .records
        .sort_by(|a, b| a.model_name.cmp(&b.model_name));
    cohort
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::balance_pipeline::phase_error_result;
    use rumoca_compile::analysis::BalanceDetail;

    fn balance_failure_row(model_name: &str, f_x: usize, unknowns: usize) -> MslModelResult {
        let mut row = phase_error_result(
            model_name.to_string(),
            "ToDae",
            Some("unbalanced model".to_string()),
            Some("ED001".to_string()),
        );
        row.balance_detail = Some(Box::new(BalanceDetail {
            algebraic_unknowns: unknowns,
            continuous_equations: f_x,
            ..BalanceDetail::default()
        }));
        row
    }

    #[test]
    fn short_error_code_strips_miette_namespace() {
        assert_eq!(short_error_code("rumoca::todae::ED001"), "ED001");
        assert_eq!(short_error_code("ED001"), "ED001");
    }

    /// `error_code_counts` was `{}` in the promoted baseline because the worker
    /// hard-coded `error_code: None` for every compile failure. Now that the
    /// code is recorded, ToDae codes must show up in the taxonomy counts.
    #[test]
    fn error_code_counts_include_todae_codes() {
        let results = vec![
            balance_failure_row("Modelica.Fluid.Examples.A", 2, 5),
            phase_error_result(
                "Modelica.Electrical.Spice3.Examples.Graetz".to_string(),
                "ToDae",
                Some("unsupported model algorithm".to_string()),
                Some("ED013".to_string()),
            ),
        ];
        let counters = crate::balance_pipeline::summarize_msl_results(&results);
        assert_eq!(counters.error_code_counts.get("ED001"), Some(&1));
        assert_eq!(counters.error_code_counts.get("ED013"), Some(&1));
    }

    /// `error_code_counts` is the summary map serialized into
    /// `msl_results.json` and printed by the report, so the solve- and
    /// sim-stage codes have to reach it too. Adding them only to
    /// `build_mls_contract_coverage`'s per-package map left the actual handoff
    /// target counting compile-stage codes alone: a model that compiles and
    /// then fails to lower or integrate contributed nothing.
    #[test]
    fn error_code_counts_include_solve_and_sim_stage_codes() {
        let mut compiled_but_unsimulatable = phase_error_result(
            "Modelica.Mechanics.Examples.A".to_string(),
            "Success",
            None,
            None,
        );
        compiled_but_unsimulatable.ir_solve_error =
            Some("[ES010] structurally singular system".to_string());
        compiled_but_unsimulatable.ir_solve_error_code = Some("ES010".to_string());
        compiled_but_unsimulatable.sim_status = Some("sim_solver_fail".to_string());
        compiled_but_unsimulatable.sim_error = Some("solver error: step too small".to_string());
        compiled_but_unsimulatable.sim_error_code = Some("EX001".to_string());

        let results = vec![
            compiled_but_unsimulatable,
            balance_failure_row("Modelica.Fluid.Examples.A", 2, 5),
        ];
        let counters = crate::balance_pipeline::summarize_msl_results(&results);

        assert_eq!(counters.error_code_counts.get("ES010"), Some(&1));
        assert_eq!(counters.error_code_counts.get("EX001"), Some(&1));
        assert_eq!(counters.error_code_counts.get("ED001"), Some(&1));
    }

    #[test]
    fn model_package_takes_the_first_two_segments() {
        assert_eq!(
            model_package("Modelica.Fluid.Examples.Tanks.ThreeTanks"),
            "Modelica.Fluid"
        );
        assert_eq!(model_package("Modelica"), "Modelica");
    }

    #[test]
    fn cohort_separates_balance_failures_from_other_todae_failures() {
        let results = vec![
            balance_failure_row("Modelica.Fluid.Examples.A", 2, 5),
            balance_failure_row("Modelica.Magnetic.Examples.B", 4, 6),
            // A resolve failure that would previously have been filed as ToDae.
            phase_error_result(
                "Modelica.Fluid.Examples.C".to_string(),
                "Resolve",
                Some("could not be compiled: unresolved component reference".to_string()),
                Some("ER003".to_string()),
            ),
            // A genuine ToDae failure that is NOT a balance failure.
            phase_error_result(
                "Modelica.Electrical.Spice3.Examples.Graetz".to_string(),
                "ToDae",
                Some("unsupported model algorithm".to_string()),
                Some("ED013".to_string()),
            ),
        ];

        let cohort = build_balance_failure_cohort(&results);
        assert_eq!(cohort.todae_failures, 3);
        assert_eq!(cohort.balance_failures, 2);
        assert_eq!(cohort.records.len(), 2);
        assert_eq!(
            cohort.todae_error_code_counts.get("ED001").copied(),
            Some(2)
        );
        assert_eq!(
            cohort.todae_error_code_counts.get("ED013").copied(),
            Some(1)
        );
        assert_eq!(
            cohort.balance_failures_by_package.get("Modelica.Fluid"),
            Some(&1)
        );
        assert_eq!(
            cohort.balance_failures_by_package.get("Modelica.Magnetic"),
            Some(&1)
        );
        assert!(!cohort.balance_failures_by_package.contains_key("<unknown>"));
    }

    #[test]
    fn cohort_record_preserves_exact_balance_dimensions() {
        let results = vec![balance_failure_row("Modelica.Fluid.Examples.A", 2, 5)];
        let cohort = build_balance_failure_cohort(&results);
        let record = &cohort.records[0];
        assert_eq!(record.package, "Modelica.Fluid");
        assert_eq!(record.equations, 2);
        assert_eq!(record.unknowns, 5);
        assert_eq!(record.balance, -3);
        assert_eq!(record.algebraic_unknowns, 5);
        assert_eq!(record.continuous_equations, 2);
        assert!(
            record.detail.contains("algebraic=5") && record.detail.contains("continuous=2"),
            "{}",
            record.detail
        );
        assert!(record.reproduction.contains("debug-model"));
    }
}
