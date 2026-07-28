//! Why a structured family declined native tensor lowering.
//!
//! Each variant names one branch in [`super`]'s stencil search. `code()` is the
//! trace-facing identifier; `fallback_reason()` is the report-facing one, and is
//! `None` for declines that resolve no owning family.

use crate::tensor_declines::{TensorDeclineJournal, TensorFallbackReason};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum StructuredTensorDecline {
    /// The row carries no structured slot metadata, so it belongs to no family.
    /// Row-level only: there is no family to attribute this to, and the
    /// preservation report never counts such a row against a family.
    MissingStructuredSlot,
    /// The row's slot names a family index the DAE no longer has. Same
    /// row-level status as [`Self::MissingStructuredSlot`].
    MissingStructuredFamily,
    /// The family's domain has fewer than two points.
    SingletonFamilyDomain,
    /// The family spans two or more points but its rows did not arrive as one
    /// contiguous block starting at the slot's iteration index.
    StructuredRowsLost,
    NonCompactCandidateDomain,
    MismatchedDaeBodyShape,
    MissingAffineAccessProof,
    NonAffineOutputMap,
}

impl StructuredTensorDecline {
    pub(super) const fn code(self) -> &'static str {
        match self {
            Self::MissingStructuredSlot => "solve:missing-structured-slot",
            Self::MissingStructuredFamily => "solve:missing-structured-family",
            Self::SingletonFamilyDomain => "solve:singleton-family-domain",
            Self::StructuredRowsLost => "solve:structured-rows-lost",
            Self::NonCompactCandidateDomain => "solve:non-compact-candidate-domain",
            Self::MismatchedDaeBodyShape => "solve:mismatched-dae-body-shape",
            Self::MissingAffineAccessProof => "solve:missing-affine-access-proof",
            Self::NonAffineOutputMap => "solve:non-affine-output-map",
        }
    }

    /// The report-level reason this decline is journaled as, or `None` when the
    /// decline could not be attributed to a surviving family.
    pub(super) const fn fallback_reason(self) -> Option<TensorFallbackReason> {
        match self {
            Self::MissingStructuredSlot | Self::MissingStructuredFamily => None,
            Self::SingletonFamilyDomain => Some(TensorFallbackReason::SingletonFamilyDomain),
            Self::StructuredRowsLost => Some(TensorFallbackReason::StructuredRowsLost),
            Self::NonCompactCandidateDomain => {
                Some(TensorFallbackReason::NonCompactCandidateDomain)
            }
            Self::MismatchedDaeBodyShape => Some(TensorFallbackReason::MismatchedDaeBodyShape),
            Self::MissingAffineAccessProof => Some(TensorFallbackReason::MissingAffineAccessProof),
            Self::NonAffineOutputMap => Some(TensorFallbackReason::NonAffineOutputMap),
        }
    }
}

/// Journal a decline against the family it declined for. Row-level declines
/// (`MissingStructuredSlot`, `MissingStructuredFamily`) resolve no family, so
/// they are traced but never attributed -- inventing a family for them would
/// make the report authoritative about something the code cannot know.
pub(super) fn record_decline(
    declines: &mut TensorDeclineJournal,
    family_index: Option<usize>,
    decline: StructuredTensorDecline,
) {
    if let (Some(family_index), Some(reason)) = (family_index, decline.fallback_reason()) {
        declines.record(family_index, reason);
    }
}
