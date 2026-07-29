//! Measured ED001 balance cohort for the MSL triage report.
//!
//! The roadmap's open question was whether the residual ToDae gap is a balance
//! cohort. It could not be answered before, because the harness had no way to
//! tell an unbalanced model from any other ToDae failure. Now that the worker
//! records the real SPEC_0008 code and the balance breakdown, this module
//! reports the cohort as measured data: how many ToDae failures there are, how
//! many of them are actually ED001, and for each ED001 model which component
//! dominates the gap and which balance clamps were exercised.

use rumoca_compile::compile::core::split_first_top_level;
use serde::Serialize;
use serde_json::Value;
use std::collections::BTreeMap;

/// SPEC_0008 code for an unbalanced model.
pub(super) const BALANCE_ERROR_CODE: &str = "ED001";

/// One measured balance failure, in report form.
#[derive(Debug, Clone, Serialize, PartialEq)]
pub(super) struct BalanceCohortRecord {
    pub model_name: String,
    pub package: String,
    pub balance: i64,
    pub equations: u64,
    pub unknowns: u64,
    pub dominant_term: String,
    pub reproduction: String,
}

/// Cohort-level view: what the ToDae bucket actually contains.
#[derive(Debug, Clone, Default, Serialize, PartialEq)]
pub(super) struct BalanceCohort {
    pub todae_failures: usize,
    pub balance_failures: usize,
    /// ToDae failures by bare SPEC_0008 code (`<unknown>` when unrecorded).
    pub todae_error_code_counts: BTreeMap<String, usize>,
    /// Balance failures per top-level MSL package.
    pub balance_failures_by_package: BTreeMap<String, usize>,
    pub records: Vec<BalanceCohortRecord>,
}

impl BalanceCohort {
    /// Fraction of ToDae failures that are actually balance failures.
    pub(super) fn balance_share(&self) -> Option<f64> {
        (self.todae_failures > 0).then(|| self.balance_failures as f64 / self.todae_failures as f64)
    }
}

/// Normalize `rumoca::todae::ED001` to `ED001`; bare codes pass through.
pub(super) fn short_error_code(code: &str) -> &str {
    match code.rsplit("::").next() {
        Some(short) if !short.is_empty() => short,
        _ => code,
    }
}

/// Top-level MSL package for a fully qualified model name.
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

fn str_field<'a>(entry: &'a Value, key: &str) -> Option<&'a str> {
    entry.get(key).and_then(Value::as_str)
}

fn u64_field(entry: &Value, key: &str) -> Option<u64> {
    entry.get(key).and_then(Value::as_u64)
}

fn bare_error_code(entry: &Value) -> Option<&str> {
    str_field(entry, "error_code").map(short_error_code)
}

fn balance_record(entry: &Value, reproduction: String) -> Option<BalanceCohortRecord> {
    let model_name = str_field(entry, "model_name")?.to_string();
    let detail = entry.get("balance_detail")?;
    let detail: rumoca_compile::analysis::BalanceDetail =
        serde_json::from_value(detail.clone()).ok()?;
    let (equations, unknowns) = detail.equations_unknowns();
    Some(BalanceCohortRecord {
        package: model_package(&model_name),
        model_name,
        balance: detail.balance(),
        equations: equations as u64,
        unknowns: unknowns as u64,
        dominant_term: dominant_balance_term(&detail).to_string(),
        reproduction,
    })
}

/// Build the cohort from `msl_results.json` `model_results` rows.
pub(super) fn collect_balance_cohort(
    results: &[Value],
    reproduction: impl Fn(&str) -> String,
) -> BalanceCohort {
    let mut cohort = BalanceCohort::default();
    for entry in results {
        if str_field(entry, "phase_reached") == Some("ToDae") {
            cohort.todae_failures += 1;
            let code = bare_error_code(entry).unwrap_or("<unknown>").to_string();
            *cohort.todae_error_code_counts.entry(code).or_insert(0) += 1;
        }
        if bare_error_code(entry) != Some(BALANCE_ERROR_CODE) {
            continue;
        }
        cohort.balance_failures += 1;
        let Some(model_name) = str_field(entry, "model_name") else {
            continue;
        };
        *cohort
            .balance_failures_by_package
            .entry(model_package(model_name))
            .or_insert(0) += 1;
        if let Some(record) = balance_record(entry, reproduction(model_name)) {
            cohort.records.push(record);
        }
    }
    cohort.records.sort_by(|a, b| {
        a.package
            .cmp(&b.package)
            .then_with(|| a.model_name.cmp(&b.model_name))
    });
    cohort
}

/// Fall back to the `scalar_equations`/`scalar_unknowns`/`balance` columns when
/// a row is ED001 but predates the structured `balance_detail` field.
pub(super) fn balance_record_from_scalar_columns(
    entry: &Value,
    reproduction: String,
) -> Option<BalanceCohortRecord> {
    let model_name = str_field(entry, "model_name")?.to_string();
    let balance = entry.get("balance").and_then(Value::as_i64)?;
    Some(BalanceCohortRecord {
        package: model_package(&model_name),
        model_name,
        balance,
        equations: u64_field(entry, "scalar_equations").unwrap_or(0),
        unknowns: u64_field(entry, "scalar_unknowns").unwrap_or(0),
        dominant_term: "unrecorded".to_string(),
        reproduction,
    })
}

/// Render the cohort into the triage markdown, grouped by package.
pub(super) fn push_balance_cohort(out: &mut String, cohort: &BalanceCohort) {
    out.push_str("## Balance Cohort (ED001)\n\n");
    out.push_str(&format!(
        "ToDae failures: {}; of those, actual balance (ED001) failures: {}",
        cohort.todae_failures, cohort.balance_failures
    ));
    if let Some(share) = cohort.balance_share() {
        out.push_str(&format!(" ({:.0}%)", share * 100.0));
    }
    out.push_str("\n\n");

    if !cohort.todae_error_code_counts.is_empty() {
        out.push_str("| ToDae error code | Models |\n|---|---:|\n");
        for (code, count) in &cohort.todae_error_code_counts {
            out.push_str(&format!("| {code} | {count} |\n"));
        }
        out.push('\n');
    }

    if cohort.records.is_empty() {
        out.push_str("No ED001 balance failures recorded in this run.\n\n");
        return;
    }

    out.push_str("| Package | Model | Balance | Equations | Unknowns | Dominant term |\n");
    out.push_str("|---|---|---:|---:|---:|---|\n");
    for record in &cohort.records {
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} |\n",
            record.package,
            record.model_name,
            record.balance,
            record.equations,
            record.unknowns,
            record.dominant_term,
        ));
    }
    out.push('\n');
}

fn dominant_balance_term(detail: &rumoca_compile::analysis::BalanceDetail) -> &'static str {
    [
        ("state_unknowns", detail.state_unknowns),
        ("algebraic_unknowns", detail.algebraic_unknowns),
        ("output_unknowns", detail.output_unknowns),
        ("discrete_real_unknowns", detail.discrete_real_unknowns),
        ("discrete_value_unknowns", detail.discrete_value_unknowns),
        ("continuous_equations", detail.continuous_equations),
        ("discrete_real_equations", detail.discrete_real_equations),
        ("discrete_assignments", detail.discrete_assignments),
    ]
    .into_iter()
    .max_by_key(|(_, count)| *count)
    .map_or("none", |(name, _)| name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn repro(model: &str) -> String {
        format!("debug-model --model {model}")
    }

    fn ed001_row(model: &str, f_x: u64, unknowns: u64) -> Value {
        json!({
            "model_name": model,
            "phase_reached": "ToDae",
            "error_code": "rumoca::todae::ED001",
            "balance_detail": {
                "state_unknowns": 0,
                "alg_unknowns": unknowns,
                "output_unknowns": 0,
                "discrete_real_unknowns": 0,
                "discrete_valued_unknowns": 0,
                "f_x_scalar": f_x,
                "f_x_aggregate_candidate_scalar": 0,
                "f_z_scalar": 0,
                "f_m_scalar": 0,
                "f_c_scalar": 0,
                "algorithm_outputs": 0,
                "when_eq_scalar": 0,
                "interface_flow_count": 0,
                "overconstrained_interface_count": 0,
                "oc_break_edge_scalar_count": 0
            }
        })
    }

    #[test]
    fn short_error_code_strips_miette_namespace() {
        assert_eq!(short_error_code("rumoca::todae::ED001"), "ED001");
        assert_eq!(short_error_code("ED001"), "ED001");
        assert_eq!(short_error_code("rumoca::resolve::ER003"), "ER003");
    }

    #[test]
    fn model_package_uses_first_two_segments() {
        assert_eq!(
            model_package("Modelica.Fluid.Examples.Tanks.ThreeTanks"),
            "Modelica.Fluid"
        );
        assert_eq!(
            model_package("Modelica.Magnetic.QuasiStatic.X"),
            "Modelica.Magnetic"
        );
    }

    #[test]
    fn balance_cohort_groups_by_package_and_dominant_term() {
        let results = vec![
            ed001_row("Modelica.Fluid.Examples.A", 2, 5),
            ed001_row("Modelica.Magnetic.Examples.B", 1, 4),
            json!({
                "model_name": "Modelica.Electrical.Spice3.Examples.Graetz",
                "phase_reached": "ToDae",
                "error_code": "rumoca::todae::ED013",
            }),
            json!({
                "model_name": "Modelica.Fluid.Examples.C",
                "phase_reached": "Resolve",
                "error_code": "rumoca::resolve::ER003",
            }),
        ];

        let cohort = collect_balance_cohort(&results, repro);
        assert_eq!(cohort.todae_failures, 3);
        assert_eq!(cohort.balance_failures, 2);
        assert_eq!(cohort.records.len(), 2);
        assert_eq!(cohort.todae_error_code_counts.get("ED001"), Some(&2));
        assert_eq!(cohort.todae_error_code_counts.get("ED013"), Some(&1));
        assert_eq!(
            cohort.balance_failures_by_package.get("Modelica.Fluid"),
            Some(&1)
        );
        assert_eq!(
            cohort.balance_failures_by_package.get("Modelica.Magnetic"),
            Some(&1)
        );
        // Sorted by package then model.
        assert_eq!(cohort.records[0].package, "Modelica.Fluid");
        assert_eq!(cohort.records[0].balance, -3);
        assert_eq!(cohort.records[0].dominant_term, "alg_unknowns");
        assert_eq!(cohort.records[1].package, "Modelica.Magnetic");
        assert!((cohort.balance_share().expect("nonzero") - 2.0 / 3.0).abs() < 1e-9);
    }

    #[test]
    fn markdown_reports_the_cohort_split() {
        let cohort = collect_balance_cohort(&[ed001_row("Modelica.Fluid.Examples.A", 2, 5)], repro);
        let mut out = String::new();
        push_balance_cohort(&mut out, &cohort);
        assert!(out.contains("Balance Cohort (ED001)"), "{out}");
        assert!(out.contains("Modelica.Fluid"), "{out}");
        assert!(out.contains("alg_unknowns"), "{out}");
    }

    #[test]
    fn empty_cohort_states_that_no_balance_failures_were_recorded() {
        let cohort = collect_balance_cohort(&[], repro);
        let mut out = String::new();
        push_balance_cohort(&mut out, &cohort);
        assert!(out.contains("No ED001 balance failures recorded"), "{out}");
        assert_eq!(cohort.balance_share(), None);
    }

    #[test]
    fn rows_without_balance_detail_still_produce_a_record() {
        let entry = json!({
            "model_name": "Modelica.Fluid.Examples.Legacy",
            "phase_reached": "ToDae",
            "error_code": "ED001",
            "balance": -7,
            "scalar_equations": 236,
            "scalar_unknowns": 243,
        });
        let record =
            balance_record_from_scalar_columns(&entry, repro("M")).expect("scalar-column record");
        assert_eq!(record.balance, -7);
        assert_eq!(record.equations, 236);
        assert_eq!(record.unknowns, 243);
        assert_eq!(record.dominant_term, "unrecorded");
    }
}
