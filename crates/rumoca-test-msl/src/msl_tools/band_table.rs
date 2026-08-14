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
//! [`load_bound_band_table`] adds the binding check on top: a persisted table
//! whose `source` hashes do not match the artifacts sitting in the directory is
//! refused, because it describes some other run's comparator output.
//! [`ensure_diffable_pair`] adds the last one: two tables of different run scope
//! are two populations, and their difference is not cohort movement.
//!
//! # Rotation, once per comparator output
//!
//! [`persist_band_table`] keys the previous table on **run identity** — the
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
    ModelDeviationMetric, TraceCertificationProfile, classify_trace_metric_channel_distribution,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

/// Stable file name of the per-model band table inside a results directory.
pub const BAND_TABLE_FILE: &str = "msl_band_table.json";
/// The previous run's table, rotated aside by [`persist_band_table`].
pub const PREVIOUS_BAND_TABLE_FILE: &str = "msl_band_table_previous.json";
/// Temporary file [`persist_band_table`] writes before renaming into place, so
/// a crash mid-write cannot leave a truncated table behind.
const BAND_TABLE_TEMP_FILE: &str = "msl_band_table.json.tmp";
/// Schema tag. Present in every table so a reader can reject foreign JSON
/// instead of deserializing it into a table with zero rows.
pub const BAND_TABLE_SCHEMA: &str = "msl_band_table";
/// Schema version this build writes and accepts.
pub const BAND_TABLE_SCHEMA_VERSION: u32 = 2;

const TRACE_COMPARISON_FILE: &str = "sim_trace_comparison.json";
const MSL_RESULTS_FILE: &str = "msl_results.json";
const OMC_SIMULATION_REFERENCE_FILE: &str = "omc_simulation_reference.json";
const PARITY_CONFIG_FILE_REL: &str = "target/msl/parity-config.json";
const SIM_OK_STATUS: &str = "sim_ok";

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
            Self::NoComparableSamples => ExitReason::NoComparableSamples,
            Self::TraceNonidentifiable => ExitReason::TraceNonidentifiable,
            Self::RumocaTraceMissing => ExitReason::RumocaTraceMissing,
            Self::OmcTraceMissing => ExitReason::ReferenceMissing,
        }
    }
}

/// One comparator-recorded non-comparison, as it appears on the wire.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraceExitRecord {
    pub kind: TraceExitKind,
    pub detail: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub certification_profile: Option<TraceCertificationProfile>,
}

impl TraceExitRecord {
    /// Record a candidate the comparator did not compare.
    pub fn new(kind: TraceExitKind, detail: impl Into<String>) -> Self {
        Self {
            kind,
            detail: detail.into(),
            certification_profile: None,
        }
    }

    /// Record an explicitly uncertified pointwise proof boundary.
    pub fn trace_nonidentifiable(profile: TraceCertificationProfile) -> Self {
        Self {
            kind: TraceExitKind::TraceNonidentifiable,
            detail: format!(
                "pointwise trace certification is non-identifying ({:?}); replacement proof obligations remain outstanding",
                profile.reason()
            ),
            certification_profile: Some(profile),
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
    /// A trace was missing, but the run recorded which side only as free text.
    /// Written for certifications produced before the comparator recorded a
    /// [`TraceExitKind`]; a current run never produces it.
    TraceMissingSideUnrecorded,
    /// The comparator ran on this model and failed. A comparator failure is a
    /// defect to fix, never a policy decision.
    ComparatorFailed,
    /// The two traces shared no variable with comparable samples, so there was
    /// nothing to band.
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
            Self::TraceMissingSideUnrecorded => "trace_missing_side_unrecorded",
            Self::ComparatorFailed => "comparator_failed",
            Self::NoComparableSamples => "no_comparable_samples",
            Self::TraceNonidentifiable => "trace_nonidentifiable",
            Self::Excluded => "excluded",
            Self::NotCompared => "not_compared",
        }
    }
}

/// One cohort model's certification row.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BandRow {
    pub model_name: String,
    pub band: BandLabel,
    /// `Some` exactly when `band == BandLabel::Absent`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub exit_reason: Option<ExitReason>,
    /// Operator-facing detail behind `exit_reason` (solver status, OMC message,
    /// exclusion rationale).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub exit_detail: Option<String>,
    #[serde(default)]
    pub compared_variables: usize,
    #[serde(default)]
    pub channel_high_count: usize,
    #[serde(default)]
    pub channel_minor_count: usize,
    #[serde(default)]
    pub channel_deviation_count: usize,
    #[serde(default)]
    pub channel_severe_count: usize,
    /// Worst per-channel bounded normalized L1 error ("max-dev").
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_channel_bounded_normalized_l1: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mean_channel_bounded_normalized_l1: Option<f64>,
    /// Model-level bounded normalized L1 score.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bounded_normalized_l1_score: Option<f64>,
}

impl BandRow {
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
            compared_variables: metric.compared_variables,
            channel_high_count: metric.channel_high_count,
            channel_minor_count: metric.channel_minor_count,
            channel_deviation_count: metric.channel_deviation_count,
            channel_severe_count: metric.channel_severe_count,
            max_channel_bounded_normalized_l1: Some(metric.max_channel_bounded_normalized_l1),
            mean_channel_bounded_normalized_l1: Some(metric.mean_channel_bounded_normalized_l1),
            bounded_normalized_l1_score: Some(metric.bounded_normalized_l1_score),
        }
    }

    fn absent(model_name: &str, reason: ExitReason, detail: String) -> Self {
        Self {
            model_name: model_name.to_string(),
            band: BandLabel::Absent,
            exit_reason: Some(reason),
            exit_detail: Some(detail),
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
        format!(
            "{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}\n",
            self.model_name,
            self.band.as_str(),
            self.exit_reason.map_or("-", ExitReason::as_str),
            self.exit_detail.as_deref().unwrap_or("-"),
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
pub struct BandTableCounts {
    pub cohort_models: usize,
    pub compared_models: usize,
    pub high: usize,
    pub near: usize,
    pub deviation: usize,
    pub absent: usize,
    #[serde(default)]
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
pub struct BandTableSource {
    #[serde(default)]
    pub trace_comparison_file: String,
    #[serde(default)]
    pub trace_comparison_digest: String,
    #[serde(default)]
    pub results_file: String,
    #[serde(default)]
    pub results_digest: String,
    /// The policy exclusion list that attributed this table's `excluded` rows.
    ///
    /// Recorded because it *decides* attribution: the same certification read
    /// against a different exclusion list files the same model as policy or as a
    /// defect. Without the digest on the table, which list was used is ambient
    /// state, and two readings of one directory can disagree with nothing on
    /// record to say why.
    #[serde(default)]
    pub exclusions_file: String,
    #[serde(default)]
    pub exclusions_digest: String,
}

/// Which run wrote a table.
///
/// A Tier 1 focused run compares a handful of models; a Tier 2 cohort run
/// compares the full roster. Letting the first rotate the second aside would
/// destroy the cohort baseline, so scope travels with the table and
/// [`persist_band_table`] refuses the narrowing rotation.
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
pub struct BandTable {
    pub schema: String,
    pub schema_version: u32,
    pub generated_at_unix_seconds: i64,
    #[serde(default)]
    pub run_scope: BandTableRunScope,
    /// Commit of the certification the rows describe, read from the run's
    /// `msl_results.json`. Never the reader's current HEAD: a table derived from
    /// an old results directory that claimed today's commit would attribute
    /// another run's numbers to this one.
    #[serde(default)]
    pub git_commit: String,
    /// Digest of the working tree's uncommitted content at write time, absent
    /// when the tree was clean.
    ///
    /// `git_commit` cannot separate two runs of one commit that carried
    /// different uncommitted work, which is how a change is iterated before it
    /// lands: such tables are indistinguishable by commit, and file timestamps
    /// do not order them either once a directory is copied or re-cleaned.
    /// Stamping the content makes each working-tree state self-identifying.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub working_tree_digest: Option<String>,
    #[serde(default)]
    pub omc_version: Option<String>,
    #[serde(default)]
    pub source: BandTableSource,
    /// Size of the run's `sim_target_models` roster.
    ///
    /// Recorded so "one row per cohort target" is checkable from the table
    /// alone. [`ensure_comparable`] holds `rows.len()` to it: a table whose row
    /// set is not the cohort is not a cohort table, whatever its counts say.
    #[serde(default)]
    pub cohort_roster_models: usize,
    /// Digest over `rows`, so the table's *contents* are bound and not only its
    /// inputs. The source digests catch a table derived from another run; this
    /// catches a table whose rows were edited after derivation.
    #[serde(default)]
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

    /// Models the comparator placed in the strict-high band. This is the only
    /// count the gate is allowed to quote as parity.
    pub fn strict_high_models(&self) -> usize {
        self.rows
            .iter()
            .filter(|row| row.band == BandLabel::High)
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

    /// Absent models carrying `reason`, read off the rows.
    ///
    /// Every consumer that quotes a population reads it from the rows, so no
    /// number a run publishes can differ from the per-model evidence behind it.
    pub fn absent_for(&self, reason: ExitReason) -> usize {
        self.rows
            .iter()
            .filter(|row| row.exit_reason == Some(reason))
            .count()
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
    Ok(())
}

// ---------------------------------------------------------------------------
// Derivation
// ---------------------------------------------------------------------------

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
    add_exit_absences(
        &mut rows,
        &collect_exit_map(
            trace,
            "missing_trace",
            ExitReason::TraceMissingSideUnrecorded,
            None,
        ),
    );
    add_exit_absences(
        &mut rows,
        // Only the `skipped` map merges policy with comparator failure, and the
        // tracked exclusion list is the authority on which entries are policy.
        &collect_exit_map(
            trace,
            "skipped",
            ExitReason::ComparatorFailed,
            Some(exclusions),
        ),
    );
    add_exit_absences(
        &mut rows,
        &collect_exit_map(
            trace,
            "trace_nonidentifiable",
            ExitReason::TraceNonidentifiable,
            None,
        ),
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
        let metric: ModelDeviationMetric = serde_json::from_value(payload.clone())
            .with_context(|| format!("invalid trace metric for {model_name}"))?;
        rows.insert(model_name.clone(), BandRow::compared(model_name, &metric));
    }
    Ok(rows)
}

/// Read one of the comparator's non-comparison maps.
///
/// A current comparator writes `{"kind": ..., "detail": ...}` per entry and the
/// kind decides the reason. A certification written before the comparator
/// recorded kinds carries a bare string, which cannot distinguish the two
/// boundaries the map merges; `untyped` names the honest fallback for that map
/// rather than guessing from the text.
///
/// `policy` is the tracked exclusion list, passed only for the `skipped` map.
/// Membership in that list is a *fact about the run's configuration*, not a
/// reading of the reason text, so it can attribute an untyped entry without the
/// string-sniffing this type exists to remove.
fn collect_exit_map(
    trace: &Value,
    key: &str,
    untyped: ExitReason,
    policy: Option<&BTreeMap<String, String>>,
) -> BTreeMap<String, BandRow> {
    trace
        .get(key)
        .and_then(Value::as_object)
        .map(|entries| {
            entries
                .iter()
                .map(|(model_name, entry)| {
                    (
                        model_name.clone(),
                        exit_row(model_name, entry, untyped, policy),
                    )
                })
                .collect()
        })
        .unwrap_or_default()
}

fn exit_row(
    model_name: &str,
    entry: &Value,
    untyped: ExitReason,
    policy: Option<&BTreeMap<String, String>>,
) -> BandRow {
    if let Ok(record) = serde_json::from_value::<TraceExitRecord>(entry.clone()) {
        return BandRow::absent(model_name, record.kind.exit_reason(), record.detail);
    }
    let detail = reason_text(entry);
    if policy.is_some_and(|policy| policy.contains_key(model_name)) {
        return BandRow::absent(model_name, ExitReason::Excluded, detail);
    }
    BandRow::absent(model_name, untyped, detail)
}

/// The comparator's recorded kind for one `skipped` / `missing_trace` entry.
///
/// `None` for a certification written before the comparator recorded kinds: that
/// artifact genuinely does not say which boundary stopped the comparison, and a
/// consumer must treat "not recorded" as its own answer rather than guessing
/// from the reason text.
pub fn trace_exit_kind(entry: &Value) -> Option<TraceExitKind> {
    serde_json::from_value::<TraceExitRecord>(entry.clone())
        .ok()
        .map(|record| record.kind)
}

/// The operator-facing detail of one `skipped` / `missing_trace` entry,
/// whichever shape the certification that wrote it used.
///
/// Consumers that only want the human text go through here so a certification
/// written before the comparator recorded [`TraceExitKind`]s still renders.
pub fn trace_exit_detail(entry: &Value) -> String {
    match serde_json::from_value::<TraceExitRecord>(entry.clone()) {
        Ok(record) => record.detail,
        Err(_) => reason_text(entry),
    }
}

fn reason_text(reason: &Value) -> String {
    reason
        .as_str()
        .map(str::to_string)
        .unwrap_or_else(|| reason.to_string())
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
            BandRow::absent(model_name, ExitReason::Excluded, reason.clone()),
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
            ),
            Some(status) => BandRow::absent(
                model_name,
                ExitReason::SimFailed,
                sim_detail(attempt, status),
            ),
            // No `sim_status` at all: the run never reached simulation for this
            // cohort target. That is an exit reason, not an absence — on the
            // 566-model sweep it is 467 of the 566 rows, every one of which the
            // table used to leave out entirely.
            None => BandRow::absent(
                model_name,
                ExitReason::NotAttempted,
                attempt.unattempted_detail.clone(),
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

// ---------------------------------------------------------------------------
// Persistence
// ---------------------------------------------------------------------------

/// Path of the band table inside `results_dir`.
pub fn band_table_path(results_dir: &Path) -> PathBuf {
    results_dir.join(BAND_TABLE_FILE)
}

/// Path of the previous run's rotated band table inside `results_dir`.
pub fn previous_band_table_path(results_dir: &Path) -> PathBuf {
    results_dir.join(PREVIOUS_BAND_TABLE_FILE)
}

/// Read a band table and enforce the acceptance contract on it.
///
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
///
/// A table is that directory's evidence only when it was derived from that
/// directory's comparator output. Without this check, a well-formed table copied
/// in from another run (or left behind by an earlier one) reads as the run's own
/// band population, and every consumer downstream quotes it.
pub fn load_bound_band_table(results_dir: &Path) -> Result<BandTable> {
    let path = band_table_path(results_dir);
    let table = load_band_table(&path)?;
    ensure_bound_to_dir(&table, results_dir)
        .with_context(|| format!("band table '{}' is not this run's", path.display()))?;
    Ok(table)
}

/// Reject a table that does not describe the comparator output in `results_dir`.
pub fn ensure_bound_to_dir(table: &BandTable, results_dir: &Path) -> Result<()> {
    let trace_file = results_dir.join(TRACE_COMPARISON_FILE);
    let trace_digest = file_digest(&trace_file)?;
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

/// Read a results directory's comparator output, naming its absence.
///
/// A cited results directory with no `sim_trace_comparison.json` never compared
/// anything. `target/msl/task65-canary-parity` is exactly that shape, and it sat
/// alongside directories that *had* comparator output — indistinguishable to
/// anything that treated a missing file as "nothing to check".
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
///
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

/// Derive a table from the raw artifacts in `results_dir`.
///
/// Used for certifications written before the table existed, so an older results
/// directory stays diffable instead of being silently uncomparable.
pub fn derive_band_table_from_dir(
    results_dir: &Path,
    run_scope: BandTableRunScope,
) -> Result<BandTable> {
    let trace_file = results_dir.join(TRACE_COMPARISON_FILE);
    let results_file = results_dir.join(MSL_RESULTS_FILE);
    let trace = read_comparator_output(&trace_file)?;
    ensure_comparison_not_vacuous(&trace, &trace_file)?;
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
        omc_version: read_omc_version(results_dir),
        source: BandTableSource {
            trace_comparison_file: trace_file.display().to_string(),
            trace_comparison_digest: file_digest(&trace_file)?,
            results_file: results_file.display().to_string(),
            results_digest: optional_file_digest(&results_file)?,
            exclusions_file: exclusions.file,
            exclusions_digest: exclusions.digest,
        },
    };
    derive_band_table(&trace, results.as_ref(), &exclusions.entries, meta)
}

/// The tracked policy exclusions, keyed by model name, with the digest of the
/// list they came from.
///
/// The list *decides* attribution: an untyped `skipped` entry is a policy
/// exclusion when the model is on this list and a comparator defect when it is
/// not. Reading it must therefore never fall back to "no exclusions" — that
/// default silently reclassifies every policy skip as a defect, and it would do
/// so as a function of the working directory, since the path is resolved from
/// the workspace root found by walking up from the CWD. A list that cannot be
/// read is a hard error, and the digest travels into the table so the reading is
/// attributable after the fact.
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

/// The persisted table when one exists and belongs to this directory, else a
/// table derived from the raw artifacts in `results_dir`.
///
/// The binding check is what makes this safe to use as evidence: a planted or
/// stale `msl_band_table.json` is refused rather than read as the directory's
/// population.
pub fn load_or_derive_band_table(results_dir: &Path) -> Result<BandTable> {
    let path = band_table_path(results_dir);
    if path.is_file() {
        return load_bound_band_table(results_dir);
    }
    let table = derive_band_table_from_dir(results_dir, BandTableRunScope::Full)?;
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
pub fn persist_band_table(
    results_dir: &Path,
    run_scope: BandTableRunScope,
) -> Result<PersistedBandTable> {
    let path = band_table_path(results_dir);
    let previous_path = previous_band_table_path(results_dir);
    let table = derive_band_table_from_dir(results_dir, run_scope)?;
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

// ---------------------------------------------------------------------------
// Transitions
// ---------------------------------------------------------------------------

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
/// band while the evidence behind it collapses — 165 channels down to 3 still
/// reads as `high`. Coverage loss is therefore its own event, not a band change.
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
    /// Channels lost across every model that stayed compared. A run can hold
    /// every band and still lose most of its evidence.
    pub compared_variables_lost: usize,
    /// `left` broken down by exit reason, so a wave of solver regressions is
    /// distinguishable from a wave of missing references.
    pub left_by_reason: BTreeMap<String, usize>,
}

/// The full ENTERED / LEFT / BAND-CHANGED / COVERAGE-DROPPED surface between two
/// certifications.
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

    /// Models that held the strict-high band in `before` and are no longer
    /// compared. This is the departure that silently shrinks a parity claim.
    pub fn departed_strict_high(&self) -> impl Iterator<Item = &LeftModel> {
        self.left
            .iter()
            .filter(|left| left.before_band == BandLabel::High)
    }
}

/// Refuse to diff two tables written at different run scopes.
///
/// A cohort table and a shard stripe are both valid tables of different
/// populations. Diffing them yields an `entered`/`left` count that is really the
/// difference between the two model sets, and nothing in the numbers says so.
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

/// Diff two certifications' band tables.
///
/// A model compared in `before` but not in `after` becomes a [`LeftModel`] with
/// the exit reason `after` recorded. If `after` has no row for it at all, the
/// departure is still reported — as [`ExitReason::NotCompared`] naming the
/// missing row — because an unexplained disappearance is the defect this diff
/// exists to catch, not a reason to drop the model.
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

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, clap::Args)]
pub struct Args {
    /// Results directory to read the comparator artifacts from and write the
    /// band table into. Defaults to the results directory the parity config
    /// names, so the tool reads the same run the harness just wrote.
    #[arg(long)]
    results_dir: Option<PathBuf>,
    /// Only check that the directory yields a comparable table bound to its own
    /// comparator output; write nothing.
    #[arg(long, default_value_t = false)]
    check: bool,
    /// Record the table as a focused (Tier 1) run's, which refuses to rotate a
    /// full-cohort table aside.
    #[arg(long, default_value_t = false)]
    partial: bool,
}

pub fn run(args: Args) -> Result<()> {
    let repo_root = crate::repo_root();
    let results_dir = args
        .results_dir
        .map(|dir| resolve_results_dir(&repo_root, dir))
        .unwrap_or_else(|| default_results_dir(&repo_root));
    if args.check {
        let table = load_or_derive_band_table(&results_dir)?;
        print_band_table_summary(&results_dir, &table, None);
        return Ok(());
    }
    let run_scope = if args.partial {
        BandTableRunScope::Partial
    } else {
        BandTableRunScope::Full
    };
    let persisted = persist_band_table(&results_dir, run_scope)?;
    if let Some(detail) = persisted.previous_not_diffable.as_deref() {
        println!("MSL band table: no diff against the previous table ({detail})");
    }
    if let Some(reason) = persisted.not_persisted_reason.as_deref() {
        println!("MSL band table: NOT WRITTEN — {reason}");
    }
    if persisted.rewrote_same_run {
        println!(
            "MSL band table: rewrote the table for this comparator output; the previous run's \
             table was left in place"
        );
    }
    print_band_table_summary(&results_dir, &persisted.table, persisted.previous.as_ref());
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
