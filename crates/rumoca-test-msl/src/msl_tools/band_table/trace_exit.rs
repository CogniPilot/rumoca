//! The comparator's typed record of a candidate it did not compare.

use rumoca_sim::sim_trace_compare::TraceCertificationProfile;
use serde::{Deserialize, Serialize};

use super::ExitReason;

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
    pub(super) fn exit_reason(self) -> ExitReason {
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
