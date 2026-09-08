//! Per-model agreement-band table: the certification artifact that makes the
//! compared set explicit.
//!
//! # Why this artifact exists
//!
//! The OMC comparator publishes aggregate band counts (`agreement_high`,
//! `agreement_minor`, `agreement_deviation`) plus a `models` map keyed by the
//! models it actually compared. A model that stops being comparable — it no
//! longer simulates, its OMC reference went missing, it was added to the
//! exclusion list — disappears from that map with no record. The counts still
//! add up, so two certifications can be compared number-to-number while the
//! *populations* behind those numbers differ.
//!
//! That is not a hypothetical. Between `results-wave3-omcref` and
//! `results-landed`, `Modelica.Electrical.Machines.Examples.DCMachines.DCPM_Start`
//! went from 165 strict-high channels to `sim_solver_fail` and vanished from
//! `.models`; `...RealSignals.Sample3` and `...OpAmps.SignalGenerator` left the
//! same way. Nothing in either certification named the departure.
//!
//! [`BandTable`] fixes that by writing one row **per cohort model** — every
//! model in the run's `sim_target_models` roster, not just the compared ones. A
//! model that is not in the compared set still gets a row, and that row carries
//! a mandatory [`ExitReason`]. Leaving the compared set is therefore a recorded
//! fact rather than an absence.
//!
//! # Acceptance contract (SPEC 0008)
//!
//! A certification artifact is **comparable** — i.e. [`ensure_comparable`]
//! accepts it — exactly when it carries:
//!
//! * `schema == `[`BAND_TABLE_SCHEMA`] and a `schema_version` this build knows
//!   ([`BAND_TABLE_SCHEMA_VERSION`]); an unknown schema is not silently read as
//!   an empty table.
//! * a `source` binding it to the comparator output it was derived from (the
//!   content hash of that run's `sim_trace_comparison.json`), so a table planted
//!   from another run is not read as this run's evidence,
//! * a `git_commit`, so its numbers can be traced to the code that produced them,
//! * a cohort roster (`cohort_roster_models`, the run's `sim_target_models`)
//!   with exactly that many rows, so the row set **is** the cohort and not
//!   "whatever the comparator happened to mention",
//! * at least one **compared** row, so the run has a band population at all,
//! * a unique `model_name` per row, so `left`/`entered` are set operations and
//!   not a bag count,
//! * every non-`absent` row carrying its channel counts (`compared_variables >
//!   0`) and no exit reason,
//! * every `absent` row carrying a named [`ExitReason`] and no band metrics, and
//! * `counts` and `rows_digest` that recompute from the rows, so the table is
//!   self-verifying: a band relabelled by hand or a count edited upward is
//!   refused even though every input binding is still intact.
//!
//! It **rejects** — loudly, never by degrading to a partial reading — every
//! violation above. A rejected table is not comparable: consumers must report
//! "not comparable" rather than diff against it.
//!
//! [`load_bound_band_table`] adds the current-proof binding check on top: a persisted table
//! whose `source` hashes do not match the artifacts sitting in the directory is
//! refused, because it describes some other run's comparator output.
//! [`load_historical_transition_band_table`] is deliberately narrower: it binds
//! the table to the recorded report and results bytes for historical transition
//! reporting, but does not admit those bytes as a current trace proof because
//! it cannot recover the run-bound source witness after the process exits.
//! [`ensure_diffable_pair`] adds the last one: two tables of different run scope
//! are two populations, and their difference is not cohort movement.
//!
//! # Rotation, once per comparator output
//!
//! [`persist_current_run_band_table`] keys the previous table on **run identity** — the
//! content hash of the comparator output the table was derived from. Re-running
//! the tool over an unchanged results directory therefore rewrites the same
//! table in place and leaves `msl_band_table_previous.json` alone, instead of
//! rotating a run's own table into the "previous" slot and diffing it against
//! itself. Only a genuinely new comparator output rotates.
//!
//! The previous slot is the **predecessor certification**, whether the last run
//! rotated it there, CI restored it there before this run, or a rotation crashed
//! before its write. A full-cohort run with no predecessor cannot state its
//! cohort movement, and the quality gate fails it rather than passing with the
//! movement rules inert.
//!
//! Owner: this module. The band classifier itself is
//! [`rumoca_sim::sim_trace_compare::classify_trace_metric_channel_distribution`];
//! this module only records what that classifier decided, per model, alongside
//! the reason every other cohort model was not classified at all.

mod transitions;

pub use transitions::{
    BandChangedModel, BandTransitionCounts, BandTransitions, CoverageDroppedModel, EnteredModel,
    LeftModel, diff_band_tables, ensure_diffable_pair,
};

use super::common::{
    TRACE_EXCLUSIONS_FILE_REL, git_worktree_content_digest, load_trace_exclusions_file,
    unix_timestamp_seconds, write_pretty_json,
};
use crate::repo_root;
use anyhow::{Context, Result, bail};
use indexmap::IndexMap;
use rumoca_sim::sim_trace_compare::{
    AgreementBand, MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE, MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
    MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE, MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
    ModelDeviationMetric, TraceCertificationProfile, TraceChannelPartition,
    classify_trace_metric_channel_distribution,
};
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

/// Stable file name of the per-model band table inside a results directory.
pub const BAND_TABLE_FILE: &str = "msl_band_table.json";
/// The previous run's table, rotated aside by [`persist_current_run_band_table`].
pub const PREVIOUS_BAND_TABLE_FILE: &str = "msl_band_table_previous.json";
/// Temporary file [`persist_current_run_band_table`] writes before renaming into place, so
/// a crash mid-write cannot leave a truncated table behind.
const BAND_TABLE_TEMP_FILE: &str = "msl_band_table.json.tmp";
/// Schema tag. Present in every table so a reader can reject foreign JSON
/// instead of deserializing it into a table with zero rows.
pub const BAND_TABLE_SCHEMA: &str = "msl_band_table";
/// Schema version this build writes and accepts.
pub const BAND_TABLE_SCHEMA_VERSION: u32 = 3;

const TRACE_COMPARISON_FILE: &str = "sim_trace_comparison.json";
const MSL_RESULTS_FILE: &str = "msl_results.json";
const OMC_SIMULATION_REFERENCE_FILE: &str = "omc_simulation_reference.json";
const PARITY_CONFIG_FILE_REL: &str = "target/msl/parity-config.json";
const SIM_OK_STATUS: &str = "sim_ok";

/// Current evidence records must carry optional values as explicit `null`.
/// Missing keys are malformed records, not older records this reader should
/// silently complete.
fn deserialize_required_option<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    Option::<T>::deserialize(deserializer)
}

/// Which agreement band the comparator placed a model in, or `Absent` when the
/// model is not in the compared set at all.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BandLabel {
    /// Strict-high agreement — the only quotable parity band.
    High,
    /// Minor ("near") agreement.
    Near,
    /// Measured deviation.
    Deviation,
    /// Not compared in this run; the row carries an [`ExitReason`].
    Absent,
}

impl BandLabel {
    /// Stable wire name, matching the serialized form.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::High => "high",
            Self::Near => "near",
            Self::Deviation => "deviation",
            Self::Absent => "absent",
        }
    }

    /// Whether this model is in the compared set.
    pub fn is_compared(self) -> bool {
        !matches!(self, Self::Absent)
    }

    fn from_agreement(band: AgreementBand) -> Self {
        match band {
            AgreementBand::HighAgreement => Self::High,
            AgreementBand::MinorAgreement => Self::Near,
            AgreementBand::Deviation => Self::Deviation,
        }
    }
}

/// How the comparator classified a candidate it did not compare.
///
/// The comparator writes this into `sim_trace_comparison.json` beside the
/// human-readable detail. It exists because attribution has to be decided where
/// the knowledge is: only the comparator knows whether a missing trace was
/// rumoca's gap or OMC's, and whether a `skipped` model was skipped by policy or
/// because the comparison itself blew up. Reading either back off a free-text
/// reason string is how a solver regression got filed as a policy exclusion.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TraceExitKind {
    /// Excluded by the tracked policy list before any trace was loaded.
    PolicyExcluded,
    /// The comparator ran on this model and failed.
    ComparatorFailed,
    /// Both traces are valid, but their channel-name universes are disjoint.
    NoCommonVariables,
    /// The comparator ran and found nothing to compare: the two traces share no
    /// variable with comparable samples. Distinct from a comparator failure
    /// because it is a property of the traces, not a defect in the comparator.
    NoComparableSamples,
    /// Pointwise comparison is non-identifying for this trace. This is an
    /// uncertified proof obligation, not an agreement band or policy skip.
    TraceNonidentifiable,
    /// Rumoca produced no usable trace for a model it reported as simulated.
    RumocaTraceMissing,
    /// The OMC reference trace is missing or unusable.
    OmcTraceMissing,
}

impl TraceExitKind {
    fn exit_reason(self) -> ExitReason {
        match self {
            Self::PolicyExcluded => ExitReason::Excluded,
            Self::ComparatorFailed => ExitReason::ComparatorFailed,
            Self::NoCommonVariables => ExitReason::NoCommonVariables,
            Self::NoComparableSamples => ExitReason::NoComparableSamples,
            Self::TraceNonidentifiable => ExitReason::TraceNonidentifiable,
            Self::RumocaTraceMissing => ExitReason::RumocaTraceMissing,
            Self::OmcTraceMissing => ExitReason::ReferenceMissing,
        }
    }
}

/// One comparator-recorded non-comparison, as it appears on the wire.
///
/// Variant payloads make the evidence required by each reason structural:
/// no-common and no-comparable outcomes cannot exist without their complete
/// channel partition, while non-identifiability cannot exist without its
/// outstanding proof profile.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum TraceExitRecord {
    PolicyExcluded {
        detail: String,
    },
    ComparatorFailed {
        detail: String,
    },
    NoCommonVariables {
        detail: String,
        channel_partition: TraceChannelPartition,
    },
    NoComparableSamples {
        detail: String,
        channel_partition: TraceChannelPartition,
    },
    TraceNonidentifiable {
        detail: String,
        certification_profile: TraceCertificationProfile,
    },
    RumocaTraceMissing {
        detail: String,
    },
    OmcTraceMissing {
        detail: String,
    },
}

impl TraceExitRecord {
    pub fn policy_excluded(detail: impl Into<String>) -> Self {
        Self::PolicyExcluded {
            detail: detail.into(),
        }
    }

    pub fn comparator_failed(detail: impl Into<String>) -> Self {
        Self::ComparatorFailed {
            detail: detail.into(),
        }
    }

    pub fn no_common_variables(
        detail: impl Into<String>,
        channel_partition: TraceChannelPartition,
    ) -> Self {
        Self::NoCommonVariables {
            detail: detail.into(),
            channel_partition,
        }
    }

    pub fn no_comparable_samples(
        detail: impl Into<String>,
        channel_partition: TraceChannelPartition,
    ) -> Self {
        Self::NoComparableSamples {
            detail: detail.into(),
            channel_partition,
        }
    }

    pub fn rumoca_trace_missing(detail: impl Into<String>) -> Self {
        Self::RumocaTraceMissing {
            detail: detail.into(),
        }
    }

    pub fn omc_trace_missing(detail: impl Into<String>) -> Self {
        Self::OmcTraceMissing {
            detail: detail.into(),
        }
    }

    /// Record an explicitly uncertified pointwise proof boundary.
    pub fn trace_nonidentifiable(profile: TraceCertificationProfile) -> Self {
        Self::TraceNonidentifiable {
            detail: format!(
                "pointwise trace certification is non-identifying ({:?}); replacement proof obligations remain outstanding",
                profile.reason()
            ),
            certification_profile: profile,
        }
    }

    pub fn kind(&self) -> TraceExitKind {
        match self {
            Self::PolicyExcluded { .. } => TraceExitKind::PolicyExcluded,
            Self::ComparatorFailed { .. } => TraceExitKind::ComparatorFailed,
            Self::NoCommonVariables { .. } => TraceExitKind::NoCommonVariables,
            Self::NoComparableSamples { .. } => TraceExitKind::NoComparableSamples,
            Self::TraceNonidentifiable { .. } => TraceExitKind::TraceNonidentifiable,
            Self::RumocaTraceMissing { .. } => TraceExitKind::RumocaTraceMissing,
            Self::OmcTraceMissing { .. } => TraceExitKind::OmcTraceMissing,
        }
    }

    pub fn detail(&self) -> &str {
        match self {
            Self::PolicyExcluded { detail }
            | Self::ComparatorFailed { detail }
            | Self::NoCommonVariables { detail, .. }
            | Self::NoComparableSamples { detail, .. }
            | Self::TraceNonidentifiable { detail, .. }
            | Self::RumocaTraceMissing { detail }
            | Self::OmcTraceMissing { detail } => detail,
        }
    }

    pub fn into_detail(self) -> String {
        match self {
            Self::PolicyExcluded { detail }
            | Self::ComparatorFailed { detail }
            | Self::NoCommonVariables { detail, .. }
            | Self::NoComparableSamples { detail, .. }
            | Self::TraceNonidentifiable { detail, .. }
            | Self::RumocaTraceMissing { detail }
            | Self::OmcTraceMissing { detail } => detail,
        }
    }

    pub fn certification_profile(&self) -> Option<&TraceCertificationProfile> {
        match self {
            Self::TraceNonidentifiable {
                certification_profile,
                ..
            } => Some(certification_profile),
            _ => None,
        }
    }
}

/// Why a cohort model is not in the compared set.
///
/// Mandatory on every `absent` row. There is no `Unknown`: "the model is gone
/// and we do not know why" is exactly the silence this table exists to remove.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ExitReason {
    /// Rumoca ran the model and the simulation failed (solver failure, NaN,
    /// timeout).
    SimFailed,
    /// The cohort target was never simulated in this run, so it was never a
    /// comparison candidate. Not-attempted is an exit reason, not an absence.
    NotAttempted,
    /// Rumoca reported the model as simulated but produced no usable trace.
    /// Distinct from [`Self::ReferenceMissing`]: this gap is ours.
    RumocaTraceMissing,
    /// The OMC reference trace is missing for this model.
    ReferenceMissing,
    /// The comparator ran on this model and failed. A comparator failure is a
    /// defect to fix, never a policy decision.
    ComparatorFailed,
    /// The two traces shared no variable with comparable samples, so there was
    /// nothing to band.
    NoCommonVariables,
    /// The two traces shared variable names, but no shared channel had a full
    /// comparable horizon.
    NoComparableSamples,
    /// Typed evidence says pointwise trace identity cannot certify this model;
    /// replacement invariant/statistical obligations are still outstanding.
    TraceNonidentifiable,
    /// Comparison is excluded by the tracked policy list, with that entry's
    /// reason.
    Excluded,
    /// Rumoca simulated it, but the comparator did not compare it and named no
    /// reason of its own.
    NotCompared,
}

impl ExitReason {
    /// Stable wire name, matching the serialized form.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::SimFailed => "sim_failed",
            Self::NotAttempted => "not_attempted",
            Self::RumocaTraceMissing => "rumoca_trace_missing",
            Self::ReferenceMissing => "reference_missing",
            Self::ComparatorFailed => "comparator_failed",
            Self::NoCommonVariables => "no_common_variables",
            Self::NoComparableSamples => "no_comparable_samples",
            Self::TraceNonidentifiable => "trace_nonidentifiable",
            Self::Excluded => "excluded",
            Self::NotCompared => "not_compared",
        }
    }
}

/// Channel-universe evidence retained by one band row.
///
/// The variant makes it impossible to confuse a successful comparison with a
/// failed comparison that nevertheless discovered the channel universe, or
/// with a run that never had two traces to inspect.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "status", rename_all = "snake_case", deny_unknown_fields)]
pub enum BandChannelAccounting {
    Compared {
        channel_partition: TraceChannelPartition,
    },
    NoComparison {
        channel_partition: TraceChannelPartition,
    },
    Unavailable,
}

/// One cohort model's certification row.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct BandRow {
    pub model_name: String,
    pub band: BandLabel,
    /// `Some` exactly when `band == BandLabel::Absent`.
    #[serde(deserialize_with = "deserialize_required_option")]
    pub exit_reason: Option<ExitReason>,
    /// Operator-facing detail behind `exit_reason` (solver status, OMC message,
    /// exclusion rationale).
    #[serde(deserialize_with = "deserialize_required_option")]
    pub exit_detail: Option<String>,
    pub channel_accounting: BandChannelAccounting,
    pub compared_variables: usize,
    pub channel_high_count: usize,
    pub channel_minor_count: usize,
    pub channel_deviation_count: usize,
    pub channel_severe_count: usize,
    /// Worst per-channel bounded normalized L1 error ("max-dev").
    #[serde(deserialize_with = "deserialize_required_option")]
    pub max_channel_bounded_normalized_l1: Option<f64>,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub mean_channel_bounded_normalized_l1: Option<f64>,
    /// Model-level bounded normalized L1 score.
    #[serde(deserialize_with = "deserialize_required_option")]
    pub bounded_normalized_l1_score: Option<f64>,
}

impl BandRow {
    /// The one predicate used by every strict certification roster.
    pub fn is_strict_high_certified(&self) -> bool {
        self.band == BandLabel::High && self.has_complete_channel_accounting()
    }

    fn has_complete_channel_accounting(&self) -> bool {
        matches!(
            &self.channel_accounting,
            BandChannelAccounting::Compared { channel_partition }
                if channel_partition.non_compared_count() == 0
        )
    }

    fn compared(model_name: &str, metric: &ModelDeviationMetric) -> Self {
        let band = BandLabel::from_agreement(classify_trace_metric_channel_distribution(
            metric,
            MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
            MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE,
            MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
            MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE,
        ));
        Self {
            model_name: model_name.to_string(),
            band,
            exit_reason: None,
            exit_detail: None,
            channel_accounting: BandChannelAccounting::Compared {
                channel_partition: metric.channel_partition().clone(),
            },
            compared_variables: metric.compared_variables(),
            channel_high_count: metric.channel_high_count(),
            channel_minor_count: metric.channel_minor_count(),
            channel_deviation_count: metric.channel_deviation_count(),
            channel_severe_count: metric.channel_severe_count(),
            max_channel_bounded_normalized_l1: Some(metric.max_channel_bounded_normalized_l1()),
            mean_channel_bounded_normalized_l1: Some(metric.mean_channel_bounded_normalized_l1()),
            bounded_normalized_l1_score: Some(metric.bounded_normalized_l1_score()),
        }
    }

    fn absent(
        model_name: &str,
        reason: ExitReason,
        detail: String,
        channel_accounting: BandChannelAccounting,
    ) -> Self {
        Self {
            model_name: model_name.to_string(),
            band: BandLabel::Absent,
            exit_reason: Some(reason),
            exit_detail: Some(detail),
            channel_accounting,
            compared_variables: 0,
            channel_high_count: 0,
            channel_minor_count: 0,
            channel_deviation_count: 0,
            channel_severe_count: 0,
            max_channel_bounded_normalized_l1: None,
            mean_channel_bounded_normalized_l1: None,
            bounded_normalized_l1_score: None,
        }
    }

    /// The row's contribution to [`rows_digest`]: every field that carries
    /// meaning, with metrics at fixed precision. See that function for why the
    /// wire form cannot be hashed directly.
    fn canonical_digest_line(&self) -> String {
        fn metric(value: Option<f64>) -> String {
            value.map_or_else(|| "-".to_string(), |value| format!("{value:.12e}"))
        }
        fn names(names: &[String]) -> String {
            names
                .iter()
                .map(|name| format!("{}:{name}", name.len()))
                .collect::<Vec<_>>()
                .join(",")
        }
        fn partition(partition: &TraceChannelPartition) -> String {
            format!(
                "c[{}]u[{}]r[{}]o[{}]",
                names(partition.compared()),
                names(partition.shared_unmeasured()),
                names(partition.rumoca_only()),
                names(partition.reference_only()),
            )
        }
        let channel_accounting = match &self.channel_accounting {
            BandChannelAccounting::Compared { channel_partition } => {
                format!("compared:{}", partition(channel_partition))
            }
            BandChannelAccounting::NoComparison { channel_partition } => {
                format!("no-comparison:{}", partition(channel_partition))
            }
            BandChannelAccounting::Unavailable => "unavailable".to_string(),
        };
        format!(
            "{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}\n",
            self.model_name,
            self.band.as_str(),
            self.exit_reason.map_or("-", ExitReason::as_str),
            self.exit_detail.as_deref().unwrap_or("-"),
            channel_accounting,
            self.compared_variables,
            self.channel_high_count,
            self.channel_minor_count,
            self.channel_deviation_count,
            self.channel_severe_count,
            metric(self.max_channel_bounded_normalized_l1),
            metric(self.mean_channel_bounded_normalized_l1),
            metric(self.bounded_normalized_l1_score),
        )
    }

    /// One-line operator rendering, used by every summary that lists a model.
    pub fn describe(&self) -> String {
        match (self.band, self.exit_reason) {
            (BandLabel::Absent, Some(reason)) => match self.exit_detail.as_deref() {
                Some(detail) => format!("absent ({}: {detail})", reason.as_str()),
                None => format!("absent ({})", reason.as_str()),
            },
            (BandLabel::Absent, None) => "absent (no reason recorded)".to_string(),
            (band, _) => format!(
                "{} (channels {}/{}/{}, max_dev={:.3e})",
                band.as_str(),
                self.channel_high_count,
                self.channel_minor_count,
                self.channel_deviation_count,
                self.max_channel_bounded_normalized_l1.unwrap_or(f64::NAN)
            ),
        }
    }
}

/// Population counts derived from [`BandTable::rows`].
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct BandTableCounts {
    pub cohort_models: usize,
    pub compared_models: usize,
    pub high: usize,
    pub near: usize,
    pub deviation: usize,
    pub absent: usize,
    pub absent_by_reason: BTreeMap<String, usize>,
}

/// The artifacts a table was derived from, so a reader can re-derive it — and
/// so a table planted from another run is detectable.
///
/// `trace_comparison_digest` is the content hash of the comparator output the
/// rows came from. It is the table's **run identity**: two tables with the same
/// digest describe the same comparator output, and a table whose digest does
/// not match the directory it sits in is not that directory's evidence.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct BandTableSource {
    pub trace_comparison_file: String,
    pub trace_comparison_digest: String,
    /// Digest of the exact OMC reference and source trace bytes validated by
    /// the opaque current-run report witness.
    pub trace_source_evidence_digest: String,
    pub results_file: String,
    pub results_digest: String,
    /// The policy exclusion list that attributed this table's `excluded` rows.
    ///
    /// Recorded because it *decides* attribution: the same certification read
    /// against a different exclusion list files the same model as policy or as a
    /// defect. Without the digest on the table, which list was used is ambient
    /// state, and two readings of one directory can disagree with nothing on
    /// record to say why.
    pub exclusions_file: String,
    pub exclusions_digest: String,
}

/// Which run wrote a table.
///
/// A Tier 1 focused run compares a handful of models; a Tier 2 cohort run
/// compares the full roster. Letting the first rotate the second aside would
/// destroy the cohort baseline, so scope travels with the table and
/// [`persist_current_run_band_table`] refuses the narrowing rotation.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BandTableRunScope {
    /// A full cohort (Tier 2) certification.
    #[default]
    Full,
    /// A focused, subset, sharded, or otherwise partial (Tier 1) run.
    Partial,
}

impl BandTableRunScope {
    /// Stable wire name, matching the serialized form.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Full => "full",
            Self::Partial => "partial",
        }
    }
}

/// The per-model band table: one row per cohort model.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct BandTable {
    pub schema: String,
    pub schema_version: u32,
    pub generated_at_unix_seconds: i64,
    pub run_scope: BandTableRunScope,
    /// Commit of the certification the rows describe, read from the run's
    /// `msl_results.json`. Never the reader's current HEAD: a table derived from
    /// an old results directory that claimed today's commit would attribute
    /// another run's numbers to this one.
    pub git_commit: String,
    /// Digest of the working tree's uncommitted content at write time, absent
    /// when the tree was clean.
    ///
    /// `git_commit` cannot separate two runs of one commit that carried
    /// different uncommitted work, which is how a change is iterated before it
    /// lands: such tables are indistinguishable by commit, and file timestamps
    /// do not order them either once a directory is copied or re-cleaned.
    /// Stamping the content makes each working-tree state self-identifying.
    #[serde(deserialize_with = "deserialize_required_option")]
    pub working_tree_digest: Option<String>,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub omc_version: Option<String>,
    pub source: BandTableSource,
    /// Size of the run's `sim_target_models` roster.
    ///
    /// Recorded so "one row per cohort target" is checkable from the table
    /// alone. [`ensure_comparable`] holds `rows.len()` to it: a table whose row
    /// set is not the cohort is not a cohort table, whatever its counts say.
    pub cohort_roster_models: usize,
    /// Digest over `rows`, so the table's *contents* are bound and not only its
    /// inputs. The source digests catch a table derived from another run; this
    /// catches a table whose rows were edited after derivation.
    pub rows_digest: String,
    pub counts: BandTableCounts,
    pub rows: Vec<BandRow>,
}

impl BandTable {
    /// Row for `model_name`, compared or absent.
    pub fn row(&self, model_name: &str) -> Option<&BandRow> {
        self.rows.iter().find(|row| row.model_name == model_name)
    }

    /// Rows in the compared set (band != `absent`).
    pub fn compared_rows(&self) -> impl Iterator<Item = &BandRow> {
        self.rows.iter().filter(|row| row.band.is_compared())
    }

    /// Strict-high numerical candidates whose complete trace-channel universe
    /// was measured. This is still not whole-model proof admission: source and
    /// IR obligations are checked by the proof-cohort harness.
    pub fn strict_high_models(&self) -> usize {
        self.rows
            .iter()
            .filter(|row| row.is_strict_high_certified())
            .count()
    }

    /// Models in the compared set.
    pub fn models_compared(&self) -> usize {
        self.compared_rows().count()
    }

    /// Models in the minor ("near") agreement band, read off the rows.
    pub fn near_models(&self) -> usize {
        self.rows_in_band(BandLabel::Near)
    }

    /// Models in the deviation band, read off the rows.
    pub fn deviation_models(&self) -> usize {
        self.rows_in_band(BandLabel::Deviation)
    }

    fn rows_in_band(&self, band: BandLabel) -> usize {
        self.rows.iter().filter(|row| row.band == band).count()
    }

    /// Whether two tables describe the same comparator output. This is run
    /// identity: it is what keeps a re-persist from rotating a run's own table
    /// into the previous slot.
    pub fn describes_same_run(&self, other: &BandTable) -> bool {
        !self.source.trace_comparison_digest.is_empty()
            && self.source.trace_comparison_digest == other.source.trace_comparison_digest
            && self.source.results_digest == other.source.results_digest
    }

    fn with_rows(rows: Vec<BandRow>, cohort_roster_models: usize, meta: BandTableMeta) -> Self {
        Self {
            schema: BAND_TABLE_SCHEMA.to_string(),
            schema_version: BAND_TABLE_SCHEMA_VERSION,
            generated_at_unix_seconds: unix_timestamp_seconds(),
            run_scope: meta.run_scope,
            git_commit: meta.git_commit,
            working_tree_digest: meta.working_tree_digest,
            omc_version: meta.omc_version,
            source: meta.source,
            cohort_roster_models,
            rows_digest: rows_digest(&rows),
            counts: count_rows(&rows),
            rows,
        }
    }
}

/// Digest over the table's rows, used to detect post-derivation edits.
///
/// The digest is taken over an explicit canonical rendering rather than over the
/// serialized JSON, because a metric's `f64` does not reliably survive a trip
/// through that JSON: the comparator produced `5.723148252362699e-9` for
/// `InvertingAmp`, and reading that text back yields a neighbouring double.
/// Hashing the wire form would make every table fail its own integrity check on
/// reload — a check that fires on ordinary use teaches readers to ignore it.
///
/// Metrics are therefore rendered at fixed precision, far finer than any band
/// decision: a relabelled band, an altered exit reason, an edited channel count,
/// and a materially changed metric are all caught, while a last-bit difference
/// from the JSON round trip is not mistaken for tampering.
fn rows_digest(rows: &[BandRow]) -> String {
    let mut hasher = blake3::Hasher::new();
    for row in rows {
        hasher.update(row.canonical_digest_line().as_bytes());
    }
    hasher.finalize().to_hex().to_string()
}

/// Provenance carried into a derived table.
#[derive(Debug, Clone, Default)]
pub struct BandTableMeta {
    pub run_scope: BandTableRunScope,
    pub git_commit: String,
    pub working_tree_digest: Option<String>,
    pub omc_version: Option<String>,
    pub source: BandTableSource,
}

fn count_rows(rows: &[BandRow]) -> BandTableCounts {
    let mut counts = BandTableCounts {
        cohort_models: rows.len(),
        ..BandTableCounts::default()
    };
    for row in rows {
        match row.band {
            BandLabel::High => counts.high += 1,
            BandLabel::Near => counts.near += 1,
            BandLabel::Deviation => counts.deviation += 1,
            BandLabel::Absent => {
                counts.absent += 1;
                let key = row
                    .exit_reason
                    .map_or("unrecorded", ExitReason::as_str)
                    .to_string();
                *counts.absent_by_reason.entry(key).or_insert(0) += 1;
            }
        }
    }
    counts.compared_models = counts.high + counts.near + counts.deviation;
    counts
}

/// Enforce the module acceptance contract. See the module docs.
///
/// Everything a consumer reads off a table is checked here against the rows
/// themselves: the declared counts are recomputed, the row digest is recomputed,
/// and the row set is held to the recorded cohort roster. A table is therefore
/// self-verifying — a hand-edited band, an inflated count, and a row set that is
/// not the cohort are all refused, not just a table from the wrong run.
pub fn ensure_comparable(table: &BandTable) -> Result<()> {
    if table.schema != BAND_TABLE_SCHEMA {
        bail!(
            "band table schema is '{}', expected '{BAND_TABLE_SCHEMA}'",
            table.schema
        );
    }
    if table.schema_version != BAND_TABLE_SCHEMA_VERSION {
        bail!(
            "band table schema_version is {}, this build reads {BAND_TABLE_SCHEMA_VERSION}",
            table.schema_version
        );
    }
    if table.source.trace_comparison_digest.is_empty() {
        bail!(
            "band table carries no comparator-output digest, so nothing binds it to the run it \
             claims to describe"
        );
    }
    if table.source.trace_source_evidence_digest.len() != 64
        || !table
            .source
            .trace_source_evidence_digest
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
    {
        bail!("band table carries no valid source-trace evidence digest");
    }
    if table.rows.is_empty() {
        bail!("band table carries no rows; a table over zero models cannot witness a departure");
    }
    ensure_unique_models(table)?;
    ensure_rows_well_formed(table)?;
    ensure_rows_are_the_cohort(table)?;
    if table.git_commit.trim().is_empty() {
        bail!(
            "band table records no git_commit, so its numbers cannot be traced to the code that \
             produced them"
        );
    }
    ensure_contents_unedited(table)?;
    if table.models_compared() == 0 {
        bail!("band table carries no compared models; the run has no band population to quote");
    }
    Ok(())
}

/// Hold the row set to the run's roster.
///
/// This is the "one row per cohort target" rule, checkable from the table alone.
/// Without it a table derived from comparator output with no `msl_results.json`
/// — 57 rows for a 566-model cohort, with policy-exclusion rows invented for
/// models the run never considered — satisfies every other check and reads as a
/// cohort table.
fn ensure_rows_are_the_cohort(table: &BandTable) -> Result<()> {
    if table.cohort_roster_models == 0 {
        bail!(
            "band table records no cohort roster, so it cannot claim one row per cohort target; \
             a table needs the run's msl_results.json `sim_target_models` to know its population"
        );
    }
    if table.rows.len() != table.cohort_roster_models {
        bail!(
            "band table carries {} rows for a cohort roster of {} models; the row set is not the \
             cohort",
            table.rows.len(),
            table.cohort_roster_models
        );
    }
    Ok(())
}

/// Recompute everything the table declares about itself.
///
/// The source digests bind a table to the artifacts it came from; they say
/// nothing about the rows after derivation. A band relabelled by hand, or a
/// count edited upward, changes what every consumer quotes while leaving the
/// binding intact.
fn ensure_contents_unedited(table: &BandTable) -> Result<()> {
    let recomputed = rows_digest(&table.rows);
    if table.rows_digest != recomputed {
        bail!(
            "band table rows do not match its recorded row digest ({} vs {}); the rows were \
             edited after the table was derived",
            digest_excerpt(&table.rows_digest),
            digest_excerpt(&recomputed)
        );
    }
    let recomputed = count_rows(&table.rows);
    if table.counts != recomputed {
        bail!(
            "band table counts do not match its rows (declared cohort={} compared={} high={} \
             near={} deviation={} absent={}, rows give cohort={} compared={} high={} near={} \
             deviation={} absent={})",
            table.counts.cohort_models,
            table.counts.compared_models,
            table.counts.high,
            table.counts.near,
            table.counts.deviation,
            table.counts.absent,
            recomputed.cohort_models,
            recomputed.compared_models,
            recomputed.high,
            recomputed.near,
            recomputed.deviation,
            recomputed.absent
        );
    }
    Ok(())
}

fn ensure_unique_models(table: &BandTable) -> Result<()> {
    let mut seen = BTreeSet::new();
    for row in &table.rows {
        if !seen.insert(row.model_name.as_str()) {
            bail!(
                "band table lists '{}' more than once; entered/left cannot be set operations",
                row.model_name
            );
        }
    }
    Ok(())
}

fn ensure_rows_well_formed(table: &BandTable) -> Result<()> {
    for row in &table.rows {
        match row.band {
            BandLabel::Absent => {
                if row.exit_reason.is_none() {
                    bail!(
                        "band table row '{}' is absent with no exit reason",
                        row.model_name
                    );
                }
                match (&row.channel_accounting, row.exit_reason) {
                    (BandChannelAccounting::Compared { .. }, _) => bail!(
                        "band table row '{}' is absent but carries compared-channel evidence",
                        row.model_name
                    ),
                    (
                        BandChannelAccounting::NoComparison { .. },
                        Some(ExitReason::NoCommonVariables | ExitReason::NoComparableSamples),
                    )
                    | (BandChannelAccounting::Unavailable, _) => {}
                    (BandChannelAccounting::NoComparison { .. }, _) => bail!(
                        "band table row '{}' carries no-comparison channel evidence for the wrong exit reason",
                        row.model_name
                    ),
                }
            }
            band => ensure_banded_row_well_formed(row, band)?,
        }
    }
    Ok(())
}

fn ensure_banded_row_well_formed(row: &BandRow, band: BandLabel) -> Result<()> {
    if row.exit_reason.is_some() {
        bail!(
            "band table row '{}' is banded '{}' but carries an exit reason",
            row.model_name,
            band.as_str()
        );
    }
    if row.compared_variables == 0 {
        bail!(
            "band table row '{}' is banded '{}' but compared no channels",
            row.model_name,
            band.as_str()
        );
    }
    let BandChannelAccounting::Compared { channel_partition } = &row.channel_accounting else {
        bail!(
            "band table row '{}' is banded '{}' without compared-channel evidence",
            row.model_name,
            band.as_str()
        );
    };
    if row.compared_variables != channel_partition.compared().len() {
        bail!(
            "band table row '{}' reports {} compared channels but accounts for {}",
            row.model_name,
            row.compared_variables,
            channel_partition.compared().len()
        );
    }
    Ok(())
}

/// Cohort model whose sim outcome the results file recorded.
struct SimAttempt {
    /// `None` when the run recorded no `sim_status` at all: the model never
    /// reached simulation.
    status: Option<String>,
    /// Detail behind a failed simulation (`sim_error_code` / `sim_error`).
    failure_detail: Option<String>,
    /// Why the run never simulated it (the phase it stopped at).
    unattempted_detail: String,
}

/// Build a table from the comparator's trace-comparison payload plus the run's
/// `msl_results.json` (which names the cohort roster and why a model is not in
/// the compared set).
///
/// Every model in the roster gets a row. A roster model the run never attempted
/// is [`ExitReason::NotAttempted`], not an absence: "we never ran it" is a
/// recorded outcome. A comparator row for a model outside the roster is a hard
/// error — the table's population would not be the run's cohort.
pub fn derive_band_table(
    trace: &Value,
    results: Option<&Value>,
    exclusions: &BTreeMap<String, String>,
    meta: BandTableMeta,
) -> Result<BandTable> {
    let mut rows: IndexMap<String, BandRow> = collect_compared_rows(trace)?;
    add_exit_absences(&mut rows, &collect_exit_map(trace, "missing_trace")?);
    add_exit_absences(
        &mut rows,
        // Only the `skipped` map merges policy with comparator failure, and the
        // tracked exclusion list is the authority on which entries are policy.
        &collect_exit_map(trace, "skipped")?,
    );
    add_exit_absences(
        &mut rows,
        &collect_exit_map(trace, "trace_nonidentifiable")?,
    );
    let roster = results.map(collect_cohort_roster).transpose()?;
    // The comparator may only speak about cohort members: a band or a recorded
    // non-comparison for a model outside the roster means the two artifacts
    // describe different populations, and the table would not be the run's.
    if let Some(roster) = roster.as_ref() {
        ensure_rows_within_cohort(&rows, roster)?;
    }
    let attempts = results
        .map(collect_sim_attempts)
        .transpose()?
        .unwrap_or_default();

    // Policy exclusions are cohort members or nothing. Without a roster the run's
    // population is unknown, and adding a row per tracked exclusion would invent
    // membership for models the run never considered.
    if let Some(roster) = roster.as_ref() {
        add_policy_absences(&mut rows, exclusions, roster, &attempts);
    }
    add_sim_absences(&mut rows, &attempts, roster.as_ref());
    if let Some(roster) = roster.as_ref() {
        add_unrecorded_targets(&mut rows, roster);
    }

    let mut rows = rows.into_values().collect::<Vec<_>>();
    rows.sort_by(|left, right| left.model_name.cmp(&right.model_name));
    let cohort_roster_models = roster.as_ref().map_or(0, BTreeSet::len);
    Ok(BandTable::with_rows(rows, cohort_roster_models, meta))
}

fn in_cohort(roster: Option<&BTreeSet<String>>, model_name: &str) -> bool {
    roster.is_none_or(|roster| roster.contains(model_name))
}

fn collect_compared_rows(trace: &Value) -> Result<IndexMap<String, BandRow>> {
    let Some(models) = trace.get("models").and_then(Value::as_object) else {
        bail!("trace comparison JSON is missing the `models` object");
    };
    let mut rows = IndexMap::new();
    for (model_name, payload) in models {
        let metric = super::omc_simulation_reference::parse_trace_model_metric(payload.clone())
            .with_context(|| format!("invalid trace metric for {model_name}"))?;
        if metric.model_name() != model_name.as_str() {
            bail!(
                "trace metric key `{model_name}` does not match embedded model `{}`",
                metric.model_name()
            );
        }
        rows.insert(model_name.clone(), BandRow::compared(model_name, &metric));
    }
    Ok(rows)
}

/// Read one of the comparator's non-comparison maps.
///
/// Current evidence has one closed tagged record per non-comparison. Any other
/// shape is malformed; there is no legacy string fallback.
fn collect_exit_map(trace: &Value, key: &str) -> Result<BTreeMap<String, BandRow>> {
    let entries = trace
        .get(key)
        .and_then(Value::as_object)
        .with_context(|| format!("trace comparison JSON is missing the `{key}` object"))?;
    let mut rows = BTreeMap::new();
    for (model_name, entry) in entries {
        let record = serde_json::from_value::<TraceExitRecord>(entry.clone())
            .with_context(|| format!("invalid `{key}` exit record for {model_name}"))?;
        let (reason, detail, channel_accounting) = match record {
            TraceExitRecord::NoCommonVariables {
                detail,
                channel_partition,
            } => (
                ExitReason::NoCommonVariables,
                detail,
                BandChannelAccounting::NoComparison { channel_partition },
            ),
            TraceExitRecord::NoComparableSamples {
                detail,
                channel_partition,
            } => (
                ExitReason::NoComparableSamples,
                detail,
                BandChannelAccounting::NoComparison { channel_partition },
            ),
            record => (
                record.kind().exit_reason(),
                record.into_detail(),
                BandChannelAccounting::Unavailable,
            ),
        };
        rows.insert(
            model_name.clone(),
            BandRow::absent(model_name, reason, detail, channel_accounting),
        );
    }
    Ok(rows)
}

/// The operator-facing detail of one `skipped` / `missing_trace` entry,
/// as a checked current record. Malformed evidence is never rendered as if it
/// were a historical free-text reason.
pub fn trace_exit_detail(entry: &Value) -> Result<String> {
    serde_json::from_value::<TraceExitRecord>(entry.clone())
        .context("invalid trace exit record")
        .map(TraceExitRecord::into_detail)
}

/// The run's `sim_target_models`: the models this certification set out to
/// simulate. This — not the comparator's `models` map — is the cohort.
fn collect_cohort_roster(results: &Value) -> Result<BTreeSet<String>> {
    let Some(targets) = results.get("sim_target_models").and_then(Value::as_array) else {
        bail!(
            "MSL results JSON is missing the `sim_target_models` roster; without it the table \
             cannot state which models the run set out to compare"
        );
    };
    let mut names = BTreeSet::new();
    for target in targets {
        let Some(name) = target.as_str() else {
            bail!("MSL results JSON has a non-string entry in `sim_target_models`");
        };
        names.insert(name.to_string());
    }
    Ok(names)
}

fn collect_sim_attempts(results: &Value) -> Result<BTreeMap<String, SimAttempt>> {
    let Some(model_results) = results.get("model_results").and_then(Value::as_array) else {
        bail!("MSL results JSON is missing the `model_results` array");
    };
    let mut attempts = BTreeMap::new();
    for model in model_results {
        let Some(model_name) = model.get("model_name").and_then(Value::as_str) else {
            bail!("MSL results JSON has a model_results entry without `model_name`");
        };
        attempts.insert(
            model_name.to_string(),
            SimAttempt {
                status: model
                    .get("sim_status")
                    .and_then(Value::as_str)
                    .map(str::to_string),
                failure_detail: sim_failure_detail(model),
                unattempted_detail: compile_failure_detail(model),
            },
        );
    }
    Ok(attempts)
}

fn sim_failure_detail(model: &Value) -> Option<String> {
    let code = model.get("sim_error_code").and_then(Value::as_str);
    let error = model.get("sim_error").and_then(Value::as_str);
    match (code, error) {
        (Some(code), Some(error)) => Some(format!("{code}: {}", first_line(error))),
        (Some(code), None) => Some(code.to_string()),
        (None, Some(error)) => Some(first_line(error).to_string()),
        (None, None) => None,
    }
}

fn compile_failure_detail(model: &Value) -> String {
    let phase = model
        .get("phase_reached")
        .and_then(Value::as_str)
        .unwrap_or("unknown");
    match model.get("error").and_then(Value::as_str) {
        Some(error) if !error.trim().is_empty() => {
            format!(
                "the run never simulated it (phase_reached {phase}: {})",
                first_line(error)
            )
        }
        _ => format!("the run never simulated it (phase_reached {phase})"),
    }
}

fn first_line(text: &str) -> &str {
    text.lines().next().unwrap_or(text).trim()
}

fn add_exit_absences(rows: &mut IndexMap<String, BandRow>, absences: &BTreeMap<String, BandRow>) {
    for (model_name, row) in absences {
        if rows.contains_key(model_name) {
            continue;
        }
        rows.insert(model_name.clone(), row.clone());
    }
}

/// Record the tracked policy exclusions the comparator never mentioned.
///
/// The comparator only reaches its exclusion check for models that simulated, so
/// this matches that precedence: a model on the exclusion list that never
/// simulated is recorded by its sim outcome, not as `excluded`. Otherwise a
/// solver regression in an excluded model would be filed as a policy decision —
/// the same misattribution, one gate earlier.
fn add_policy_absences(
    rows: &mut IndexMap<String, BandRow>,
    exclusions: &BTreeMap<String, String>,
    roster: &BTreeSet<String>,
    attempts: &BTreeMap<String, SimAttempt>,
) {
    for (model_name, reason) in exclusions {
        if rows.contains_key(model_name) || !roster.contains(model_name) {
            continue;
        }
        let simulated = attempts
            .get(model_name)
            .is_some_and(|attempt| attempt.status.as_deref() == Some(SIM_OK_STATUS));
        if !simulated && attempts.contains_key(model_name) {
            continue;
        }
        rows.insert(
            model_name.clone(),
            BandRow::absent(
                model_name,
                ExitReason::Excluded,
                reason.clone(),
                BandChannelAccounting::Unavailable,
            ),
        );
    }
}

fn add_sim_absences(
    rows: &mut IndexMap<String, BandRow>,
    attempts: &BTreeMap<String, SimAttempt>,
    roster: Option<&BTreeSet<String>>,
) {
    for (model_name, attempt) in attempts {
        if rows.contains_key(model_name) || !in_cohort(roster, model_name) {
            continue;
        }
        let row = match attempt.status.as_deref() {
            Some(SIM_OK_STATUS) => BandRow::absent(
                model_name,
                ExitReason::NotCompared,
                "rumoca simulated the model but the comparator did not compare it".to_string(),
                BandChannelAccounting::Unavailable,
            ),
            Some(status) => BandRow::absent(
                model_name,
                ExitReason::SimFailed,
                sim_detail(attempt, status),
                BandChannelAccounting::Unavailable,
            ),
            // No `sim_status` at all: the run never reached simulation for this
            // cohort target. That is an exit reason, not an absence — on the
            // 566-model sweep it is 467 of the 566 rows, every one of which the
            // table used to leave out entirely.
            None => BandRow::absent(
                model_name,
                ExitReason::NotAttempted,
                attempt.unattempted_detail.clone(),
                BandChannelAccounting::Unavailable,
            ),
        };
        rows.insert(model_name.clone(), row);
    }
}

/// Give every roster model the run recorded nothing about its own row.
///
/// Without this the table's `cohort_models` is "models the comparator or the
/// simulator happened to mention" rather than the cohort the run declared.
fn add_unrecorded_targets(rows: &mut IndexMap<String, BandRow>, roster: &BTreeSet<String>) {
    for model_name in roster {
        if rows.contains_key(model_name) {
            continue;
        }
        rows.insert(
            model_name.clone(),
            BandRow::absent(
                model_name,
                ExitReason::NotAttempted,
                "the run recorded no result at all for this cohort target".to_string(),
                BandChannelAccounting::Unavailable,
            ),
        );
    }
}

fn ensure_rows_within_cohort(
    rows: &IndexMap<String, BandRow>,
    roster: &BTreeSet<String>,
) -> Result<()> {
    let outside = rows
        .keys()
        .filter(|model_name| !roster.contains(*model_name))
        .cloned()
        .collect::<Vec<_>>();
    if outside.is_empty() {
        return Ok(());
    }
    bail!(
        "the comparator names {} model(s) outside the run's sim_target_models roster, so the two \
         artifacts describe different populations: {}",
        outside.len(),
        outside.join(", ")
    )
}

fn sim_detail(attempt: &SimAttempt, status: &str) -> String {
    match attempt.failure_detail.as_deref() {
        Some(detail) => format!("{status} ({detail})"),
        None => status.to_string(),
    }
}

/// Path of the band table inside `results_dir`.
pub fn band_table_path(results_dir: &Path) -> PathBuf {
    results_dir.join(BAND_TABLE_FILE)
}

/// Path of the previous run's rotated band table inside `results_dir`.
pub fn previous_band_table_path(results_dir: &Path) -> PathBuf {
    results_dir.join(PREVIOUS_BAND_TABLE_FILE)
}

/// Read a band table and enforce the acceptance contract on it.
/// This checks the table's internal shape only. A table read as a *directory's*
/// evidence must additionally be bound to that directory's comparator output —
/// see [`load_bound_band_table`].
pub fn load_band_table(path: &Path) -> Result<BandTable> {
    let raw = fs::read_to_string(path)
        .with_context(|| format!("failed to read band table '{}'", path.display()))?;
    let table: BandTable = serde_json::from_str(&raw)
        .with_context(|| format!("failed to parse band table '{}'", path.display()))?;
    ensure_comparable(&table)
        .with_context(|| format!("band table '{}' is not comparable", path.display()))?;
    Ok(table)
}

/// Read the table persisted in `results_dir` and check it against the artifacts
/// sitting there.
/// A table is that directory's evidence only when it was derived from that
/// directory's comparator output. Without this check, a well-formed table copied
/// in from another run (or left behind by an earlier one) reads as the run's own
/// band population, and every consumer downstream quotes it.
pub fn load_bound_band_table(results_dir: &Path) -> Result<BandTable> {
    let table = load_historical_transition_band_table(results_dir)?;
    ensure_source_evidence_bound_to_dir(&table, results_dir)?;
    Ok(table)
}

/// Load historical transition data bound to its report and results bytes.
/// This is not current proof admission: only the in-memory current-run receipt
/// proves which source traces produced the report. It exists so an explicitly
/// persisted certification can be compared later without pretending that
/// reopening ambient source files recreates that receipt.
pub fn load_historical_transition_band_table(results_dir: &Path) -> Result<BandTable> {
    let path = band_table_path(results_dir);
    let table = load_band_table(&path)?;
    ensure_report_and_results_bound_to_dir(&table, results_dir)
        .with_context(|| format!("band table '{}' is not this run's", path.display()))?;
    Ok(table)
}

/// Reject a table that does not describe the comparator output in `results_dir`.
pub fn ensure_bound_to_dir(table: &BandTable, results_dir: &Path) -> Result<()> {
    ensure_report_and_results_bound_to_dir(table, results_dir)?;
    ensure_source_evidence_bound_to_dir(table, results_dir)
}

fn ensure_report_and_results_bound_to_dir(table: &BandTable, results_dir: &Path) -> Result<()> {
    let trace_file = results_dir.join(TRACE_COMPARISON_FILE);
    let trace_bytes = fs::read(&trace_file)
        .with_context(|| format!("failed to read '{}'", trace_file.display()))?;
    let trace_digest = blake3::hash(&trace_bytes).to_hex().to_string();
    if table.source.trace_comparison_digest != trace_digest {
        bail!(
            "band table was derived from a different comparator output (table digest {}, '{}' \
             digest {})",
            digest_excerpt(&table.source.trace_comparison_digest),
            trace_file.display(),
            digest_excerpt(&trace_digest)
        );
    }
    let results_file = results_dir.join(MSL_RESULTS_FILE);
    let results_digest = optional_file_digest(&results_file)?;
    if table.source.results_digest != results_digest {
        bail!(
            "band table was derived from a different results file (table digest {}, '{}' digest \
             {})",
            digest_excerpt(&table.source.results_digest),
            results_file.display(),
            digest_excerpt(&results_digest)
        );
    }
    Ok(())
}

fn ensure_source_evidence_bound_to_dir(table: &BandTable, results_dir: &Path) -> Result<()> {
    let trace_file = results_dir.join(TRACE_COMPARISON_FILE);
    let trace_bytes = fs::read(&trace_file)
        .with_context(|| format!("failed to read '{}'", trace_file.display()))?;
    let reference_file = results_dir.join(OMC_SIMULATION_REFERENCE_FILE);
    let reference_bytes = fs::read(&reference_file)
        .with_context(|| format!("failed to read '{}'", reference_file.display()))?;
    let paths = super::common::MslPaths::current().with_results_dir(results_dir);
    let validated = super::omc_simulation_reference::validate_trace_report_against_sources(
        &paths,
        &trace_bytes,
        &reference_bytes,
    )?;
    if table.source.trace_source_evidence_digest != validated.source_evidence_digest() {
        bail!(
            "band table was derived from different source traces (table digest {}, current digest {})",
            digest_excerpt(&table.source.trace_source_evidence_digest),
            digest_excerpt(validated.source_evidence_digest())
        );
    }
    Ok(())
}

/// Read a results directory's comparator output, naming its absence.
///
/// A cited results directory with no `sim_trace_comparison.json` never compared
/// anything. `target/msl/task65-canary-parity` is exactly that shape, and it sat
/// alongside directories that *had* comparator output — indistinguishable to
/// anything that treated a missing file as "nothing to check".
#[cfg(test)]
fn read_comparator_output(trace_file: &Path) -> Result<Value> {
    if !trace_file.is_file() {
        bail!(
            "no comparator output at '{}': this results directory ran no trace comparison, so it \
             carries no parity evidence — sim_ok is completion, never parity",
            trace_file.display()
        );
    }
    read_required_json(trace_file)
}

/// Reject a comparator output that compared nothing.
/// `models_compared: 0` with all-zero agreement bands is a **vacuous
/// comparison**: every band count is trivially satisfied and every percentage is
/// 0/0. `target/msl/task4445-after` and `target/msl/task65-canary` both carry it,
/// and read as neutral — a run that "found no deviations" — unless the zero is
/// rejected outright at the boundary.
///
/// The declared count and the `models` map must also agree: a header claiming
/// comparisons that the map does not contain is a doctored or truncated
/// artifact, and this is the same trust boundary as the table-binding check.
pub fn ensure_comparison_not_vacuous(trace: &Value, trace_file: &Path) -> Result<()> {
    let compared_models = trace
        .get("models")
        .and_then(Value::as_object)
        .map_or(0, serde_json::Map::len);
    let declared = trace
        .get("models_compared")
        .and_then(Value::as_u64)
        .map(|count| usize::try_from(count).unwrap_or(usize::MAX));
    if compared_models == 0 {
        bail!(
            "vacuous comparison in '{}': the comparator compared 0 models, so its agreement bands \
             are 0/0 and cannot be cited as parity evidence",
            trace_file.display()
        );
    }
    if let Some(declared) = declared
        && declared != compared_models
    {
        bail!(
            "'{}' declares models_compared={declared} but carries {compared_models} model \
             entries; the artifact does not describe its own contents",
            trace_file.display()
        );
    }
    Ok(())
}

fn digest_excerpt(digest: &str) -> &str {
    if digest.is_empty() {
        return "<none>";
    }
    &digest[..digest.len().min(12)]
}

fn file_digest(path: &Path) -> Result<String> {
    let bytes =
        fs::read(path).with_context(|| format!("failed to read '{}' to hash", path.display()))?;
    Ok(blake3::hash(&bytes).to_hex().to_string())
}

fn optional_file_digest(path: &Path) -> Result<String> {
    if !path.is_file() {
        return Ok(String::new());
    }
    file_digest(path)
}

#[cfg(test)]
pub fn derive_band_table_from_test_artifacts(
    results_dir: &Path,
    run_scope: BandTableRunScope,
) -> Result<BandTable> {
    let trace_file = results_dir.join(TRACE_COMPARISON_FILE);
    let trace = read_comparator_output(&trace_file)?;
    ensure_comparison_not_vacuous(&trace, &trace_file)?;
    let trace_digest = file_digest(&trace_file)?;
    derive_band_table_from_payload(
        results_dir,
        run_scope,
        &trace,
        &trace_digest,
        &trace_digest,
        read_omc_version(results_dir),
    )
}

/// Derive a table from a validated current-run trace payload.
///
/// This boundary never rediscovers `sim_trace_comparison.json`. The caller
/// supplies the payload and digest from
/// the opaque orchestration receipt minted immediately after comparison.
fn derive_band_table_from_current_trace(
    results_dir: &Path,
    run_scope: BandTableRunScope,
    validated: &super::omc_simulation_reference::ValidatedTraceReport,
) -> Result<BandTable> {
    let trace = validated.payload();
    let trace_digest = validated.report_digest();
    derive_band_table_from_payload(
        results_dir,
        run_scope,
        trace,
        trace_digest,
        validated.source_evidence_digest(),
        validated.reference_omc_version(),
    )
}

fn derive_band_table_from_payload(
    results_dir: &Path,
    run_scope: BandTableRunScope,
    trace: &Value,
    trace_digest: &str,
    trace_source_evidence_digest: &str,
    omc_version: Option<String>,
) -> Result<BandTable> {
    if trace_digest.len() != 64
        || !trace_digest
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
    {
        bail!("current-run trace comparison carries an invalid exact-byte digest");
    }
    ensure_comparison_not_vacuous(trace, Path::new("<current-run-trace-receipt>"))?;
    let trace_file = results_dir.join(TRACE_COMPARISON_FILE);
    let results_file = results_dir.join(MSL_RESULTS_FILE);
    let results = read_optional_json(&results_file)?;
    let git_commit = results
        .as_ref()
        .and_then(|results| results.get("git_commit"))
        .and_then(Value::as_str)
        // A directory that carries only comparator output still has provenance:
        // the comparator stamps its own commit at write time.
        .or_else(|| trace.get("git_commit").and_then(Value::as_str))
        .unwrap_or_default()
        .to_string();
    let exclusions = tracked_exclusions()?;
    let meta = BandTableMeta {
        run_scope,
        git_commit,
        working_tree_digest: git_worktree_content_digest(&repo_root()),
        omc_version,
        source: BandTableSource {
            trace_comparison_file: trace_file.display().to_string(),
            trace_comparison_digest: trace_digest.to_string(),
            trace_source_evidence_digest: trace_source_evidence_digest.to_string(),
            results_file: results_file.display().to_string(),
            results_digest: optional_file_digest(&results_file)?,
            exclusions_file: exclusions.file,
            exclusions_digest: exclusions.digest,
        },
    };
    derive_band_table(trace, results.as_ref(), &exclusions.entries, meta)
}

/// The tracked policy exclusions, keyed by model name, with the digest of the
/// list they came from.
///
/// The list supplies reviewed policy absences for cohort members the comparator
/// never reached. It never repairs or reclassifies a malformed comparator
/// record. A list that cannot be read is a hard error, and the digest travels
/// into the table so the reading is attributable after the fact.
#[derive(Debug)]
struct TrackedExclusions {
    entries: BTreeMap<String, String>,
    file: String,
    digest: String,
}

fn tracked_exclusions() -> Result<TrackedExclusions> {
    exclusions_from(&crate::repo_root().join(TRACE_EXCLUSIONS_FILE_REL))
}

fn exclusions_from(path: &Path) -> Result<TrackedExclusions> {
    let entries = load_trace_exclusions_file(path).with_context(|| {
        format!(
            "cannot attribute policy exclusions without the tracked list '{}'; every `skipped` \
             model would be recorded as a comparator defect instead",
            path.display()
        )
    })?;
    Ok(TrackedExclusions {
        entries,
        file: path.display().to_string(),
        digest: file_digest(path)?,
    })
}

#[cfg(test)]
pub fn load_or_derive_test_band_table(results_dir: &Path) -> Result<BandTable> {
    let path = band_table_path(results_dir);
    if path.is_file() {
        return load_historical_transition_band_table(results_dir);
    }
    let table = derive_band_table_from_test_artifacts(results_dir, BandTableRunScope::Full)?;
    ensure_comparable(&table).with_context(|| {
        format!(
            "band table derived from '{}' is not comparable",
            results_dir.display()
        )
    })?;
    Ok(table)
}

/// The outcome of persisting a run's band table.
#[derive(Debug, Clone)]
pub struct PersistedBandTable {
    /// The table just written for this run.
    pub table: BandTable,
    /// The previous *run's* table, when the directory carried a comparable one
    /// from a different comparator output. Never this run's own table.
    pub previous: Option<BandTable>,
    /// Set when the previous slot held something that could not be diffed
    /// against — unreadable, or written at a different run scope. It is stated
    /// rather than swallowed, so "no previous table", "the previous table was
    /// unreadable", and "the previous table was a shard stripe" are never
    /// spelled the same way.
    pub previous_not_diffable: Option<String>,
    /// Set when the directory already held this run's table. Nothing rotated:
    /// re-persisting the same comparator output must not push a run's own table
    /// into the previous slot and then diff against it.
    pub rewrote_same_run: bool,
    /// Whether the table was written to disk.
    pub persisted: bool,
    /// Why the table was not written, when it was not. A refusal is a designed
    /// outcome, not a failure — but it is never silent, because a run that
    /// believes it persisted a table and did not would leave the next run
    /// diffing against the wrong certification.
    pub not_persisted_reason: Option<String>,
}

/// Derive and persist the band table for `results_dir`, rotating an *earlier
/// run's* table to [`PREVIOUS_BAND_TABLE_FILE`] first.
///
/// Rotation is keyed on run identity (the comparator-output digest), so this is
/// idempotent: calling it twice over one certification rewrites the same table
/// and leaves the real previous table untouched.
///
/// A [`BandTableRunScope::Partial`] run never rotates a `Full` table aside — a
/// focused run must not consume the cohort baseline. That case returns the
/// derived table with `persisted: false` and a stated reason rather than an
/// error: a focused run in a fresh directory (a CI shard) still writes its
/// table, and only a focused run that would *displace* cohort evidence declines.
///
/// The write is crash-atomic: the new table lands in a temporary file first, so
/// an interrupted call leaves either the old table or the new one, never a
/// truncated file.
#[cfg(test)]
pub fn persist_test_band_table(
    results_dir: &Path,
    run_scope: BandTableRunScope,
) -> Result<PersistedBandTable> {
    let table = derive_band_table_from_test_artifacts(results_dir, run_scope)?;
    persist_derived_band_table(results_dir, run_scope, table)
}

/// Persist the band table derived from an already validated current-run trace
/// receipt. This is the only persistence route used by the active MSL gate.
pub fn persist_current_run_band_table(
    run_scope: BandTableRunScope,
    validated: &super::omc_simulation_reference::ValidatedTraceReport,
) -> Result<PersistedBandTable> {
    let results_dir = validated.results_dir();
    let table = derive_band_table_from_current_trace(results_dir, run_scope, validated)?;
    persist_derived_band_table(results_dir, run_scope, table)
}

fn persist_derived_band_table(
    results_dir: &Path,
    run_scope: BandTableRunScope,
    table: BandTable,
) -> Result<PersistedBandTable> {
    let path = band_table_path(results_dir);
    let previous_path = previous_band_table_path(results_dir);
    ensure_comparable(&table)?;
    let (existing, mut previous_not_diffable) = read_existing_table(&path);

    if let Some(existing) = existing.as_ref()
        && existing.describes_same_run(&table)
    {
        // Same comparator output: this is a re-persist of one certification.
        // Rewrite in place and keep whatever the previous slot already holds.
        write_table_atomically(&path, &table)?;
        let previous = load_band_table(&previous_path)
            .ok()
            .filter(|previous| !previous.describes_same_run(&table));
        return Ok(PersistedBandTable {
            table,
            previous,
            previous_not_diffable,
            rewrote_same_run: true,
            persisted: true,
            not_persisted_reason: None,
        });
    }

    if let Some(existing) = existing.as_ref()
        && run_scope == BandTableRunScope::Partial
        && existing.run_scope == BandTableRunScope::Full
    {
        return Ok(PersistedBandTable {
            table,
            previous: None,
            previous_not_diffable,
            rewrote_same_run: false,
            persisted: false,
            not_persisted_reason: Some(format!(
                "'{}' holds a full-cohort band table over {} models; a partial run must not \
                 rotate the cohort baseline aside, so this run's table was not written",
                results_dir.display(),
                existing.counts.cohort_models
            )),
        });
    }

    // The previous slot is the predecessor certification: either rotated there by
    // the last run, restored there by CI before this one, or left there by a
    // rotation that crashed before its write. Reading it back is what keeps any
    // of those from being spelled the same way as "first certification".
    let previous = match existing {
        Some(existing) => Some(existing),
        None => load_band_table(&previous_path)
            .ok()
            .filter(|previous| !previous.describes_same_run(&table)),
    };

    // Scope guards run both ways. A partial run must not displace the cohort
    // table (handled above); a full run may rotate a shard stripe aside, but it
    // must not then diff the cohort against that stripe and publish the
    // difference as cohort movement.
    let previous = match previous {
        Some(previous) if previous.run_scope != table.run_scope => {
            previous_not_diffable = Some(format!(
                "run_scope_mismatch: the previous table is '{}' over {} models and this run is \
                 '{}' over {}; a cohort and a stripe are not like-for-like, so no movement was \
                 computed",
                previous.run_scope.as_str(),
                previous.counts.cohort_models,
                table.run_scope.as_str(),
                table.counts.cohort_models
            ));
            None
        }
        previous => previous,
    };

    write_table_atomically(&results_dir.join(BAND_TABLE_TEMP_FILE), &table)?;
    rotate_existing_table(&path, &previous_path)?;
    fs::rename(results_dir.join(BAND_TABLE_TEMP_FILE), &path)
        .with_context(|| format!("failed to move the new band table into {}", path.display()))?;
    Ok(PersistedBandTable {
        table,
        previous,
        previous_not_diffable,
        rewrote_same_run: false,
        persisted: true,
        not_persisted_reason: None,
    })
}

fn write_table_atomically(path: &Path, table: &BandTable) -> Result<()> {
    let temp_path = path.with_extension("json.writing");
    write_pretty_json(&temp_path, table)?;
    fs::rename(&temp_path, path).with_context(|| {
        format!(
            "failed to move {} into {}",
            temp_path.display(),
            path.display()
        )
    })
}

fn read_existing_table(path: &Path) -> (Option<BandTable>, Option<String>) {
    if !path.is_file() {
        return (None, None);
    }
    match load_band_table(path) {
        Ok(table) => (Some(table), None),
        Err(error) => (None, Some(format!("{error:#}"))),
    }
}

/// Move any existing table aside byte-for-byte, so an unreadable one is
/// preserved for inspection rather than overwritten.
fn rotate_existing_table(path: &Path, previous_path: &Path) -> Result<()> {
    if !path.is_file() {
        return Ok(());
    }
    fs::rename(path, previous_path).with_context(|| {
        format!(
            "failed to rotate {} to {}",
            path.display(),
            previous_path.display()
        )
    })
}

#[cfg(test)]
fn read_omc_version(results_dir: &Path) -> Option<String> {
    let path = results_dir.join(OMC_SIMULATION_REFERENCE_FILE);
    let raw = fs::read_to_string(path).ok()?;
    let payload: Value = serde_json::from_str(&raw).ok()?;
    payload
        .get("omc_version")
        .and_then(Value::as_str)
        .map(str::trim)
        .filter(|version| !version.is_empty())
        .map(str::to_string)
}

fn read_required_json(path: &Path) -> Result<Value> {
    if !path.is_file() {
        bail!("missing input file '{}'", path.display());
    }
    let raw =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    serde_json::from_str(&raw).with_context(|| format!("invalid JSON {}", path.display()))
}

fn read_optional_json(path: &Path) -> Result<Option<Value>> {
    if !path.is_file() {
        return Ok(None);
    }
    read_required_json(path).map(Some)
}

#[derive(Debug, Clone, clap::Args)]
pub struct Args {
    /// Results directory containing the already persisted current-schema band
    /// table. Missing tables are never synthesized from ambient artifacts.
    #[arg(long)]
    results_dir: Option<PathBuf>,
}

pub fn run(args: Args) -> Result<()> {
    let repo_root = crate::repo_root();
    let results_dir = args
        .results_dir
        .map(|dir| resolve_results_dir(&repo_root, dir))
        .unwrap_or_else(|| default_results_dir(&repo_root));
    let table = load_bound_band_table(&results_dir)?;
    print_band_table_summary(&results_dir, &table, None);
    Ok(())
}

fn resolve_results_dir(repo_root: &Path, dir: PathBuf) -> PathBuf {
    if dir.is_absolute() {
        dir
    } else {
        repo_root.join(dir)
    }
}

/// The results directory the harness is configured to use.
///
/// `cargo xtask verify msl-parity --results-dir DIR` writes that choice into
/// `target/msl/parity-config.json`; reading it here means the tool inspects the
/// run the harness just produced instead of a stale `target/msl/results`.
fn default_results_dir(repo_root: &Path) -> PathBuf {
    let fallback = repo_root.join("target/msl/results");
    let Ok(raw) = fs::read_to_string(repo_root.join(PARITY_CONFIG_FILE_REL)) else {
        return fallback;
    };
    let Ok(config) = serde_json::from_str::<Value>(&raw) else {
        return fallback;
    };
    config
        .get("results_dir")
        .and_then(Value::as_str)
        .map(str::trim)
        .filter(|dir| !dir.is_empty())
        .map_or(fallback, |dir| resolve_results_dir(repo_root, dir.into()))
}

fn print_band_table_summary(results_dir: &Path, table: &BandTable, previous: Option<&BandTable>) {
    println!(
        "MSL band table ({}, scope={}): cohort={} compared={} high={} near={} deviation={} \
         absent={}",
        results_dir.display(),
        table.run_scope.as_str(),
        table.counts.cohort_models,
        table.counts.compared_models,
        table.counts.high,
        table.counts.near,
        table.counts.deviation,
        table.counts.absent,
    );
    for (reason, count) in &table.counts.absent_by_reason {
        println!("    absent {reason}: {count}");
    }
    let Some(previous) = previous else {
        return;
    };
    let transitions = diff_band_tables(previous, table);
    println!("  {}", transitions.summary_line());
    for left in &transitions.left {
        println!(
            "    LEFT {} (was {}): {}",
            left.model_name,
            left.before_band.as_str(),
            left.exit_reason.as_str()
        );
    }
    for drop in &transitions.coverage_dropped {
        println!(
            "    COVERAGE-DROPPED {}: {} -> {} channels (still {})",
            drop.model_name,
            drop.before_compared_variables,
            drop.after_compared_variables,
            drop.band.as_str()
        );
    }
}

#[cfg(test)]
mod tests;
