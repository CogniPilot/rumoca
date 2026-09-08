use super::{BandLabel, BandRow, BandTable, ExitReason};
use anyhow::{Result, bail};
use serde::Serialize;
use std::collections::BTreeMap;

/// A model that joined the compared set.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct EnteredModel {
    pub model_name: String,
    pub after_band: BandLabel,
    /// What the previous certification said about it, when it had a row.
    pub before_exit_reason: Option<ExitReason>,
    pub before_exit_detail: Option<String>,
}

/// A model that left the compared set. Never silent: it always carries the band
/// it held and the reason it is gone.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct LeftModel {
    pub model_name: String,
    pub before_band: BandLabel,
    pub exit_reason: ExitReason,
    pub exit_detail: Option<String>,
}

/// A model compared in both runs whose band moved.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BandChangedModel {
    pub model_name: String,
    pub before_band: BandLabel,
    pub after_band: BandLabel,
}

/// A model compared in both runs that is now compared over fewer channels.
///
/// A band is a share of the channels that were compared, so a model can hold its
/// band while the evidence behind it collapses. Coverage loss is therefore its
/// own event, not a band change.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CoverageDroppedModel {
    pub model_name: String,
    pub before_compared_variables: usize,
    pub after_compared_variables: usize,
    pub band: BandLabel,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct BandTransitionCounts {
    pub before_cohort_models: usize,
    pub after_cohort_models: usize,
    pub before_compared: usize,
    pub after_compared: usize,
    pub common_compared: usize,
    pub entered: usize,
    pub left: usize,
    pub band_changed: usize,
    pub coverage_dropped: usize,
    /// Channels lost across every model that stayed compared.
    pub compared_variables_lost: usize,
    /// Departures broken down by exit reason.
    pub left_by_reason: BTreeMap<String, usize>,
}

/// Cohort and evidence movement between two certifications.
#[derive(Debug, Clone, Default, PartialEq, Serialize)]
pub struct BandTransitions {
    pub counts: BandTransitionCounts,
    pub entered: Vec<EnteredModel>,
    pub left: Vec<LeftModel>,
    pub band_changed: Vec<BandChangedModel>,
    pub coverage_dropped: Vec<CoverageDroppedModel>,
}

impl BandTransitions {
    /// The single line a gate summary prints for cohort movement.
    pub fn summary_line(&self) -> String {
        format!(
            "cohort: entered {}, left {}, band-changed {}, coverage-dropped {} (compared {} -> {}, \
             common {})",
            self.counts.entered,
            self.counts.left,
            self.counts.band_changed,
            self.counts.coverage_dropped,
            self.counts.before_compared,
            self.counts.after_compared,
            self.counts.common_compared,
        )
    }
}

/// Refuse to diff tables written for different run scopes.
pub fn ensure_diffable_pair(before: &BandTable, after: &BandTable) -> Result<()> {
    if before.run_scope != after.run_scope {
        bail!(
            "run_scope_mismatch: the before certification is '{}' over {} models and the after              certification is '{}' over {}; a cohort and a stripe are not like-for-like",
            before.run_scope.as_str(),
            before.counts.cohort_models,
            after.run_scope.as_str(),
            after.counts.cohort_models
        );
    }
    Ok(())
}

/// Diff two certifications without dropping a model that leaves the cohort.
pub fn diff_band_tables(before: &BandTable, after: &BandTable) -> BandTransitions {
    let mut transitions = BandTransitions::default();
    for before_row in before.rows.iter().filter(|row| row.band.is_compared()) {
        classify_before_row(before_row, after, &mut transitions);
    }
    for after_row in after.rows.iter().filter(|row| row.band.is_compared()) {
        if before
            .row(&after_row.model_name)
            .is_some_and(|row| row.band.is_compared())
        {
            continue;
        }
        transitions.entered.push(entered_model(after_row, before));
    }
    transitions.counts = transition_counts(before, after, &transitions);
    transitions
}

fn classify_before_row(before_row: &BandRow, after: &BandTable, out: &mut BandTransitions) {
    match after.row(&before_row.model_name) {
        Some(after_row) if after_row.band.is_compared() => {
            if after_row.band != before_row.band {
                out.band_changed.push(BandChangedModel {
                    model_name: before_row.model_name.clone(),
                    before_band: before_row.band,
                    after_band: after_row.band,
                });
            }
            if after_row.compared_variables < before_row.compared_variables {
                out.coverage_dropped.push(CoverageDroppedModel {
                    model_name: before_row.model_name.clone(),
                    before_compared_variables: before_row.compared_variables,
                    after_compared_variables: after_row.compared_variables,
                    band: after_row.band,
                });
            }
        }
        Some(after_row) => out.left.push(LeftModel {
            model_name: before_row.model_name.clone(),
            before_band: before_row.band,
            exit_reason: after_row.exit_reason.unwrap_or(ExitReason::NotCompared),
            exit_detail: after_row.exit_detail.clone(),
        }),
        None => out.left.push(LeftModel {
            model_name: before_row.model_name.clone(),
            before_band: before_row.band,
            exit_reason: ExitReason::NotCompared,
            exit_detail: Some(
                "the model has no row in the candidate band table; the run did not record why it \
                 left the compared set"
                    .to_string(),
            ),
        }),
    }
}

fn entered_model(after_row: &BandRow, before: &BandTable) -> EnteredModel {
    let before_row = before.row(&after_row.model_name);
    EnteredModel {
        model_name: after_row.model_name.clone(),
        after_band: after_row.band,
        before_exit_reason: before_row.and_then(|row| row.exit_reason),
        before_exit_detail: before_row.and_then(|row| row.exit_detail.clone()),
    }
}

fn transition_counts(
    before: &BandTable,
    after: &BandTable,
    transitions: &BandTransitions,
) -> BandTransitionCounts {
    let before_compared = before.models_compared();
    let after_compared = after.models_compared();
    let mut left_by_reason = BTreeMap::new();
    for left in &transitions.left {
        *left_by_reason
            .entry(left.exit_reason.as_str().to_string())
            .or_insert(0) += 1;
    }
    BandTransitionCounts {
        before_cohort_models: before.rows.len(),
        after_cohort_models: after.rows.len(),
        before_compared,
        after_compared,
        common_compared: before_compared - transitions.left.len(),
        entered: transitions.entered.len(),
        left: transitions.left.len(),
        band_changed: transitions.band_changed.len(),
        coverage_dropped: transitions.coverage_dropped.len(),
        compared_variables_lost: transitions
            .coverage_dropped
            .iter()
            .map(|drop| drop.before_compared_variables - drop.after_compared_variables)
            .sum(),
        left_by_reason,
    }
}
