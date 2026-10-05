//! The "Simulation Soundness Roster" section of the MSL PR comment
//! (SPEC_0033 simulation soundness, SPEC_0025 PR report).
//!
//! Lists every model that simulated without strict-high parity and without a
//! typed trace exception, grouped by triage package so each parity lane reads
//! its own rows. A model missing from the baseline roster is marked `new`:
//! the roster only shrinks, so such a row fails the quality gate.

use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use anyhow::Result;
use serde_json::Value;

use crate::msl_tools::band_table::{self, BandRow, SimulationOutcomes};

use super::json_u64;

/// The "Simulation Outcomes" table: the run's completed simulations split
/// into verified, excepted (by kind), and unclassified, beside the compiled
/// models that did not complete one. Counts come from the quality snapshot
/// (`sim_ok`, `compiled_models`) and the band table (the split), and a split
/// that does not sum to the completions is stated, never hidden.
pub(super) fn render_outcomes_section(results_dir: &Path, quality: &Value) -> Result<String> {
    let simulated = json_u64(quality, "sim_ok").unwrap_or(0);
    let target = json_u64(quality, "sim_target_models").unwrap_or(0);
    let compiled = json_u64(quality, "compiled_models").unwrap_or(0);
    let path = band_table::band_table_path(results_dir);
    let outcomes = if path.is_file() {
        Some(band_table::load_band_table(&path)?.simulation_outcomes())
    } else {
        None
    };
    Ok(render_outcomes(
        simulated,
        target,
        compiled,
        outcomes.as_ref(),
    ))
}

fn render_outcomes(
    simulated: u64,
    target: u64,
    compiled: u64,
    outcomes: Option<&SimulationOutcomes>,
) -> String {
    let mut text = String::from(
        "#### Simulation Outcomes\n\n\
         _Target: every compiled model simulates, and every simulation is verified or carries a \
         typed trace exception._\n\n\
         | Outcome | Models |\n|---|---:|\n",
    );
    text.push_str(&format!(
        "| Simulated: ran to completion | {simulated}/{target} |\n"
    ));
    let Some(outcomes) = outcomes else {
        text.push_str(&format!(
            "| Compiled, not simulated: failed or timed out after compiling | {} |\n\n\
             _No band table was produced in this run, so completions are not split into \
             verified, excepted, and unclassified._\n",
            compiled.saturating_sub(simulated)
        ));
        return text;
    };
    let excepted = outcomes.excepted_total();
    text.push_str(&format!(
        "| Verified: strict-high trace parity with OMC | {} |\n\
         | Excepted: typed trace exception ({}) | {excepted} |\n\
         | Unclassified: soundness roster below, target 0 | {} |\n\
         | Compiled, not simulated: failed or timed out after compiling | {} |\n\n",
        outcomes.verified,
        excepted_kinds(outcomes),
        outcomes.unclassified,
        compiled.saturating_sub(simulated),
    ));
    let classified = outcomes.classified_total();
    if classified as u64 == simulated {
        text.push_str(&format!(
            "_Verified + excepted + unclassified = simulated ({} + {excepted} + {} = {simulated})._\n",
            outcomes.verified, outcomes.unclassified
        ));
    } else {
        text.push_str(&format!(
            "**The band table classifies {classified} completions, but the run recorded \
             {simulated}.**\n"
        ));
    }
    text
}

fn excepted_kinds(outcomes: &SimulationOutcomes) -> String {
    let mut kinds = outcomes
        .excepted
        .iter()
        .map(|(kind, count)| format!("{} {count}", kind.as_str()))
        .collect::<Vec<_>>();
    if outcomes.excepted_untyped > 0 {
        kinds.push(format!("untyped {}", outcomes.excepted_untyped));
    }
    if kinds.is_empty() {
        return "none".to_string();
    }
    kinds.join(", ")
}

pub(super) fn render_soundness_section(
    results_dir: &Path,
    baseline: Option<&Value>,
) -> Result<String> {
    let path = band_table::band_table_path(results_dir);
    if !path.is_file() {
        return Ok("_No band table was produced in this run._\n".to_string());
    }
    let table = band_table::load_band_table(&path)?;
    let roster = baseline.map(baseline_roster);
    Ok(render_rows(
        table.unexcepted_non_high_rows(),
        roster.as_ref(),
    ))
}

fn baseline_roster(baseline: &Value) -> BTreeSet<String> {
    baseline
        .get("unexcepted_non_high_models")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(Value::as_str)
        .map(str::to_string)
        .collect()
}

fn render_rows<'a>(
    rows: impl Iterator<Item = &'a BandRow>,
    roster: Option<&BTreeSet<String>>,
) -> String {
    let mut by_package = BTreeMap::<&str, Vec<&BandRow>>::new();
    for row in rows {
        by_package
            .entry(band_table::triage_package(&row.model_name))
            .or_default()
            .push(row);
    }
    let total = by_package.values().map(Vec::len).sum::<usize>();
    let mut text = match roster {
        Some(roster) => format!(
            "{total} model(s) simulated without strict-high parity or a typed exception \
             (baseline roster: {}; the target is 0 and the roster only shrinks).\n\n",
            roster.len()
        ),
        None => format!(
            "{total} model(s) simulated without strict-high parity or a typed exception \
             (no baseline roster to compare).\n\n"
        ),
    };
    for (package, rows) in by_package {
        text.push_str(&format!("**{package}** ({})\n\n", rows.len()));
        for row in rows {
            let marker = match roster {
                Some(roster) if !roster.contains(&row.model_name) => " **new**",
                Some(_) | None => "",
            };
            text.push_str(&format!(
                "- `{}`{marker}: {}\n",
                row.model_name,
                row.describe()
            ));
        }
        text.push('\n');
    }
    text
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::msl_tools::band_table::{BandLabel, ExitReason};

    fn row(model_name: &str, band: BandLabel, exit_reason: Option<ExitReason>) -> BandRow {
        let mut row: BandRow = serde_json::from_value(serde_json::json!({
            "model_name": model_name,
            "band": band.as_str(),
        }))
        .expect("row");
        row.exit_reason = exit_reason;
        row
    }

    #[test]
    fn roster_rows_group_by_triage_package_and_mark_new_entries() {
        let rows = [
            row(
                "Modelica.Electrical.Analog.Examples.A",
                BandLabel::Near,
                None,
            ),
            row(
                "Modelica.Electrical.Analog.Examples.B",
                BandLabel::Deviation,
                None,
            ),
            row(
                "Modelica.Clocked.Examples.C",
                BandLabel::Absent,
                Some(ExitReason::ReferenceMissing),
            ),
        ];
        let roster = BTreeSet::from([
            "Modelica.Electrical.Analog.Examples.A".to_string(),
            "Modelica.Clocked.Examples.C".to_string(),
        ]);
        let text = render_rows(rows.iter(), Some(&roster));
        assert!(text.starts_with("3 model(s)"), "{text}");
        assert!(text.contains("(baseline roster: 2;"), "{text}");
        assert!(
            text.contains("**Modelica.Electrical.Analog** (2)"),
            "{text}"
        );
        assert!(text.contains("**Modelica.Clocked.Examples** (1)"), "{text}");
        assert!(
            text.contains("`Modelica.Electrical.Analog.Examples.B` **new**"),
            "{text}"
        );
        assert!(
            !text.contains("`Modelica.Electrical.Analog.Examples.A` **new**"),
            "{text}"
        );
    }

    fn outcomes(verified: usize, unclassified: usize) -> SimulationOutcomes {
        use crate::msl_tools::common::TraceExceptionKind;
        SimulationOutcomes {
            verified,
            excepted: [
                (TraceExceptionKind::ReferenceFailure, 30),
                (TraceExceptionKind::ModelIssue, 7),
                (TraceExceptionKind::ComparatorLimitation, 22),
            ]
            .into_iter()
            .collect(),
            excepted_untyped: 0,
            unclassified,
        }
    }

    #[test]
    fn outcomes_split_the_completions_and_state_a_split_that_does_not_sum() {
        let text = render_outcomes(359, 566, 439, Some(&outcomes(299, 1)));
        assert!(
            text.contains("| Simulated: ran to completion | 359/566 |"),
            "{text}"
        );
        assert!(text.contains("| Verified: strict-high trace parity with OMC | 299 |"));
        assert!(text.contains(
            "| Excepted: typed trace exception (reference_failure 30, model_issue 7, \
             comparator_limitation 22) | 59 |"
        ));
        assert!(text.contains("| Unclassified: soundness roster below, target 0 | 1 |"));
        assert!(
            text.contains("| Compiled, not simulated: failed or timed out after compiling | 80 |")
        );
        assert!(text.contains("(299 + 59 + 1 = 359)"));

        let short = render_outcomes(359, 566, 439, Some(&outcomes(290, 1)));
        assert!(
            short.contains(
                "**The band table classifies 350 completions, but the run recorded 359.**"
            ),
            "{short}"
        );
        let unsplit = render_outcomes(359, 566, 439, None);
        assert!(unsplit.contains("| 80 |"));
        assert!(unsplit.contains("No band table was produced"));
    }
}
