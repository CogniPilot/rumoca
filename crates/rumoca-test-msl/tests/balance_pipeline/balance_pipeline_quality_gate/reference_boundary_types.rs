//! The reviewed reference boundary chain's types: each boundary's metric
//! migration, its evidence and the roster additions it names.

use super::*;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub(in super::super) struct MslMetricSchemaMigration {
    pub(super) from_quality_gate_version: u32,
    pub(super) to_quality_gate_version: u32,
    pub(super) change: String,
    pub(super) strict_high_before: usize,
    pub(super) strict_high_after: usize,
    pub(super) policy_excluded_after: usize,
    pub(super) excluded_strict_high_before: usize,
    pub(super) excluded_non_high_before: usize,
    pub(super) exclusions_file: String,
    pub(super) exclusions_sha256: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub(in super::super) struct MslReferenceBoundaryMigration {
    #[serde(flatten)]
    pub(super) metric: MslMetricSchemaMigration,
    pub(super) evidence_git_commit: String,
    pub(super) evidence_run: String,
    pub(super) policy_excluded_before: usize,
    /// Models this boundary adds to the unexcepted non-high roster, each
    /// naming its open defect (SPEC_0050).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub(super) roster_additions: Vec<MslRosterAddition>,
    /// The pinned old-side model states of a crossing from a baseline that predates them.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(super) evidence_model_states: Option<serde_json::Value>,
    /// What this boundary allows to change per model (`rumoca_msl_quality::ReviewedChanges`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(super) reviewed_changes: Option<serde_json::Value>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(super) previous: Option<Box<MslReferenceBoundaryMigration>>,
}

/// A reviewed roster addition: a completion that is neither strict-high nor
/// covered by a typed exception because of an open Rumoca defect, which the
/// roster names instead of an exception row hiding it.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub(in super::super) struct MslRosterAddition {
    pub(super) model_name: String,
    pub(super) cause: String,
    pub(super) facts: Vec<String>,
    pub(super) owner: String,
    /// The reviewed count of the model's channels in the deviation band, severe
    /// ones included (what a baseline crossing deducts from the bad-channel
    /// total for it).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(super) deviation_channels: Option<usize>,
    /// The reviewed count of those deviation channels at or above the severe
    /// threshold (a subset of `deviation_channels`, deducted from the
    /// severe-channel total).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(super) severe_channels: Option<usize>,
}
