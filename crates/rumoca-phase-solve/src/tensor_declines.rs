//! Why a structured equation family did not survive as a native tensor node.
//!
//! Solve lowering decides per family whether it can emit a `Map`/`AffineStencil`
//! covering the family's whole index domain. When it cannot, the family is
//! scalarized. [`tensor_preservation_report`](crate::tensor_preservation_report)
//! measures how often that happens, but a measurement taken after lowering can
//! only observe the absence of a node -- it cannot see the branch that produced
//! it. The journal in this module carries the branch out of the decline site so
//! the report can name it.
//!
//! Every [`TensorFallbackReason`] here corresponds to exactly one decline branch
//! in [`crate::stencil`], or to one of the three report-side residues
//! ([`TensorFallbackReason::FamilyRowsAbsent`],
//! [`TensorFallbackReason::UnattributedFamilyDecline`],
//! [`TensorFallbackReason::IncompleteTensorCoverage`]). The residues are
//! themselves facts about what the pipeline recorded -- never observed, observed
//! with nothing declined, and measured with no journal at all -- so no reason
//! this module can report is a guess dressed as a diagnosis. Codes are stable
//! identifiers: once shipped they are never renumbered or reused.

use std::collections::{BTreeMap, BTreeSet};

use rumoca_ir_dae as dae;

/// Whether scalarizing a declining family is the correct lowering or lost
/// structure. This is the classification that turns a raw "N% scalar" KPI into
/// an actionable ceiling estimate.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum TensorHeadroom {
    /// Scalarizing is the right lowering: the family has no shared kernel to
    /// preserve, or its domain is too small for one.
    CorrectlyScalar,
    /// The family could have stayed compact; structure was lost to a compiler
    /// limitation rather than to the model.
    RealHeadroom,
    /// Not decidable from the information available at the decline site.
    Unknown,
}

impl TensorHeadroom {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::CorrectlyScalar => "correctly-scalar",
            Self::RealHeadroom => "real-headroom",
            Self::Unknown => "unknown",
        }
    }
}

/// A code-derived reason a structured family body was scalarized.
///
/// Each variant documents the decline branch it is produced by. A variant that
/// no branch can produce would be fiction dressed as a diagnosis, so the crate
/// keeps one test per variant proving the branch is reachable.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum TensorFallbackReason {
    /// The family's index domain holds fewer than two points, so there is no
    /// kernel to share. `stencil::structured_tensor_at`, the
    /// `family_row_indices.len() < 2` guard with a singleton point count.
    SingletonFamilyDomain,
    /// The family spans two or more index points but its lowered rows did not
    /// arrive as one contiguous block -- rows were consumed, eliminated, or
    /// re-owned before the stencil could see them.
    /// `stencil::structured_tensor_at` (same guard, multi-point domain) and the
    /// partial-row guard in `stencil::regular_family_full_domain`.
    ///
    /// The same guard also fires on the leftover tail of a family the
    /// shrinking-prefix search split. That is not this reason: the search
    /// journals the check that rejected the larger candidate at the moment it
    /// shrinks, and the first record wins, so a split family reports its real
    /// blocker instead of the tail it left behind.
    StructuredRowsLost,
    /// No prefix of the family's row-major enumeration forms a compact index
    /// box, so the shrinking-prefix search had no candidate domain to validate.
    /// `stencil::max_structured_affine_domain`, the `compact_domain_from_tuples`
    /// branch.
    NonCompactCandidateDomain,
    /// The DAE bodies behind the family's rows are not the same expression
    /// shape at every index, so there is no single kernel to lift.
    /// `stencil::max_structured_affine_domain` (`structured_dae_body_shapes_match`)
    /// and `stencil::regular_family_full_domain` (`corner_dae_body_shapes_match`).
    MismatchedDaeBodyShape,
    /// The family's operand accesses could not be proven affine in the domain
    /// binders, so no load/const strides could be derived.
    /// `stencil::max_structured_affine_domain`, `stencil::regular_family_full_domain`,
    /// and the `affine_strides_for_family` branch of `stencil::structured_tensor_at`.
    MissingAffineAccessProof,
    /// Bodies and accesses lifted, but the rows' output slots are not an affine
    /// function of the index tuple, so the node has no output map.
    /// `stencil::max_structured_affine_domain`, `stencil::regular_family_full_domain`,
    /// and the `output_map_for_family` branch of `stencil::structured_tensor_at`.
    NonAffineOutputMap,
    /// No row carrying this family's slot metadata ever reached structured
    /// lowering: the family survived into the DAE but its equations were lowered
    /// by another path (or not at all). Report-side, and it is a decline-site
    /// fact: [`TensorDeclineJournal::observe_family`] runs in
    /// `stencil::push_structured_programs` for every row slot that resolves, so
    /// absence from that set is recorded, not assumed.
    FamilyRowsAbsent,
    /// The family's rows DID reach structured lowering, no lowering branch
    /// declined it, and yet a tensor node covering its whole domain is missing.
    /// The pipeline holds no fact explaining that, so this reason names the gap
    /// instead of a cause. Report-side, from the journal's observed set.
    UnattributedFamilyDecline,
    /// The family was scalarized and no decline journal was available to
    /// attribute it. This is the residue that remains when a caller uses the
    /// unattributed [`crate::tensor_preservation_report`] entry point.
    IncompleteTensorCoverage,
}

impl TensorFallbackReason {
    /// Stable diagnostic code. Never renumber or reuse a shipped code.
    #[must_use]
    pub const fn code(self) -> &'static str {
        match self {
            Self::SingletonFamilyDomain => "solve:singleton-family-domain",
            Self::StructuredRowsLost => "solve:structured-rows-lost",
            Self::NonCompactCandidateDomain => "solve:non-compact-candidate-domain",
            Self::MismatchedDaeBodyShape => "solve:mismatched-dae-body-shape",
            Self::MissingAffineAccessProof => "solve:missing-affine-access-proof",
            Self::NonAffineOutputMap => "solve:non-affine-output-map",
            Self::FamilyRowsAbsent => "solve:family-rows-absent",
            Self::UnattributedFamilyDecline => "solve:unattributed-family-decline",
            Self::IncompleteTensorCoverage => "solve:incomplete-tensor-coverage",
        }
    }

    /// Every reason this crate can report, in stable order. Aggregators iterate
    /// this so a code that never fires still appears with a zero count.
    #[must_use]
    pub const fn all() -> &'static [Self] {
        &[
            Self::SingletonFamilyDomain,
            Self::StructuredRowsLost,
            Self::NonCompactCandidateDomain,
            Self::MismatchedDaeBodyShape,
            Self::MissingAffineAccessProof,
            Self::NonAffineOutputMap,
            Self::FamilyRowsAbsent,
            Self::UnattributedFamilyDecline,
            Self::IncompleteTensorCoverage,
        ]
    }

    /// Whether scalarizing for this reason is correct or leaves headroom.
    ///
    /// The rationale per variant is deliberately conservative: anything that
    /// could be either a model property or a missing compiler capability is
    /// [`TensorHeadroom::Unknown`], not headroom.
    #[must_use]
    pub const fn headroom(self) -> TensorHeadroom {
        match self {
            // A one-point domain has no kernel to share, and differing per-index
            // bodies have no single kernel to lift. Both are correct scalar
            // lowerings, not missed opportunities.
            Self::SingletonFamilyDomain | Self::MismatchedDaeBodyShape => {
                TensorHeadroom::CorrectlyScalar
            }
            // Compact family metadata survived but the rows or the candidate
            // domain shape did not, and the output-map case already proved both
            // body shape and access affinity. All three are implementable with a
            // richer node shape or a preserved row block.
            Self::StructuredRowsLost
            | Self::NonCompactCandidateDomain
            | Self::NonAffineOutputMap => TensorHeadroom::RealHeadroom,
            // A missing affine proof is either a genuinely non-affine access or
            // a gap in the prover; the decline site cannot tell which. A family
            // that never reached structured lowering, one whose decline no
            // branch recorded, or one measured without a journal, carries no
            // evidence at all.
            Self::MissingAffineAccessProof
            | Self::FamilyRowsAbsent
            | Self::UnattributedFamilyDecline
            | Self::IncompleteTensorCoverage => TensorHeadroom::Unknown,
        }
    }
}

/// The measurable identity of one structured family in the DAE that lowering
/// actually consumed.
///
/// Family attribution is POSITIONAL: `family_index` indexes
/// `dae.continuous.structured_equations`. Recording this list at the lowering
/// site is what lets the report prove it is measuring the same DAE, instead of
/// trusting a caller-supplied one that may have been rewritten since.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LoweredFamily {
    pub span: rumoca_core::Span,
    pub domain: rumoca_core::StructuredIndexDomain,
    pub equations_per_point: usize,
    /// Where the family's rows start in `dae.continuous.equations`. Not needed
    /// to measure the family, but two family lists that agree on everything
    /// else and disagree here are lowering two different equation blocks.
    pub first_equation_index: usize,
}

impl LoweredFamily {
    fn from_family(family: &dae::StructuredEquationFamily) -> Self {
        Self {
            span: family.span,
            domain: family.domain.clone(),
            equations_per_point: family.equations_per_point,
            first_equation_index: family.first_equation_index,
        }
    }

    fn matches(&self, family: &dae::StructuredEquationFamily) -> bool {
        self.span == family.span
            && self.domain == family.domain
            && self.equations_per_point == family.equations_per_point
            && self.first_equation_index == family.first_equation_index
    }
}

/// The continuous partition lowering consumed, in the positional order the
/// report's `family_index` attributions refer to.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct LoweredContinuousFamilies {
    families: Vec<LoweredFamily>,
    structural_equation_rows: usize,
}

impl LoweredContinuousFamilies {
    #[must_use]
    pub fn families(&self) -> &[LoweredFamily] {
        &self.families
    }

    #[must_use]
    pub const fn structural_equation_rows(&self) -> usize {
        self.structural_equation_rows
    }

    /// Whether `dae_model`'s continuous partition is positionally identical to
    /// the one lowering saw. Returns the first family index that differs, so
    /// the caller can report a span; a differing equation-row count reports
    /// index zero, since it makes every family's row block suspect.
    pub(crate) fn first_mismatch(&self, dae_model: &dae::Dae) -> Option<usize> {
        let families = &dae_model.continuous.structured_equations;
        if self.structural_equation_rows != dae_model.continuous.equations.len() {
            return Some(0);
        }
        if self.families.len() != families.len() {
            return Some(self.families.len().min(families.len()));
        }
        self.families
            .iter()
            .zip(families)
            .position(|(recorded, family)| !recorded.matches(family))
    }
}

/// One decline observed at a lowering site, attributed to a family.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TensorDeclineRecord {
    pub family_index: usize,
    pub reason: TensorFallbackReason,
}

/// Declines observed while lowering one DAE into Solve IR.
///
/// The journal is threaded explicitly through lowering rather than kept in
/// process state, so two concurrent lowerings cannot mix their attributions and
/// a stale journal cannot survive into a later model.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct TensorDeclineJournal {
    observed_families: BTreeSet<usize>,
    records: Vec<TensorDeclineRecord>,
    lowered: Option<LoweredContinuousFamilies>,
}

impl TensorDeclineJournal {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Note that at least one lowered row carrying `family_index`'s slot
    /// metadata reached structured lowering. A family never observed here was
    /// never a candidate, which is a different fact from declining.
    pub(crate) fn observe_family(&mut self, family_index: usize) {
        self.observed_families.insert(family_index);
    }

    pub(crate) fn record(&mut self, family_index: usize, reason: TensorFallbackReason) {
        self.observed_families.insert(family_index);
        self.records.push(TensorDeclineRecord {
            family_index,
            reason,
        });
    }

    #[must_use]
    pub fn records(&self) -> &[TensorDeclineRecord] {
        &self.records
    }

    #[must_use]
    pub fn was_observed(&self, family_index: usize) -> bool {
        self.observed_families.contains(&family_index)
    }

    /// Record the continuous structured families of the DAE this journal's
    /// lowering consumed. Without this the report cannot tell a positional
    /// `family_index` attribution about DAE A from one about DAE B.
    pub(crate) fn record_lowered_continuous(&mut self, dae_model: &dae::Dae) {
        self.lowered = Some(LoweredContinuousFamilies {
            families: dae_model
                .continuous
                .structured_equations
                .iter()
                .map(LoweredFamily::from_family)
                .collect(),
            structural_equation_rows: dae_model.continuous.equations.len(),
        });
    }

    /// The families lowering consumed, when this journal came from a full
    /// lowering entry point. `None` for a journal built directly by a unit test
    /// or by a partial-lowering fixture: those cannot be provenance-checked.
    #[must_use]
    pub fn lowered_continuous(&self) -> Option<&LoweredContinuousFamilies> {
        self.lowered.as_ref()
    }

    /// The reason to report for a family that ended up scalarized, or `None`
    /// when the journal holds no decline for it.
    ///
    /// The first recorded decline wins: later records for the same family come
    /// from the shrinking-prefix retry, which re-declines an already-explained
    /// family at a narrower candidate domain.
    #[must_use]
    pub fn recorded_reason_for_family(&self, family_index: usize) -> Option<TensorFallbackReason> {
        self.records
            .iter()
            .find(|record| record.family_index == family_index)
            .map(|record| record.reason)
    }

    /// The reason to report for a family that ended up scalarized.
    ///
    /// Three distinct facts, never conflated:
    /// * a recorded decline -- the lowering branch that declined it;
    /// * observed with no recorded decline -- lowering saw the family and
    ///   declined nothing, so the report has no cause to name
    ///   ([`TensorFallbackReason::UnattributedFamilyDecline`]);
    /// * never observed -- no row carrying the family's slot metadata reached
    ///   structured lowering ([`TensorFallbackReason::FamilyRowsAbsent`]).
    #[must_use]
    pub fn reason_for_family(&self, family_index: usize) -> TensorFallbackReason {
        match self.recorded_reason_for_family(family_index) {
            Some(reason) => reason,
            None if self.was_observed(family_index) => {
                TensorFallbackReason::UnattributedFamilyDecline
            }
            None => TensorFallbackReason::FamilyRowsAbsent,
        }
    }
}

/// Per-reason totals over one report's fallbacks.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TensorFallbackCount {
    pub reason: TensorFallbackReason,
    /// Families that reported this reason.
    pub families: usize,
    /// Canonical family bodies scalarized for this reason.
    pub scalarized_bodies: usize,
    /// Scalar rows those bodies expand to.
    pub scalarized_rows: usize,
}

impl TensorFallbackCount {
    #[must_use]
    pub const fn code(&self) -> &'static str {
        self.reason.code()
    }

    #[must_use]
    pub const fn headroom(&self) -> TensorHeadroom {
        self.reason.headroom()
    }
}

/// Fold fallbacks into per-reason totals in stable code order. Reasons with no
/// fallback are omitted; [`TensorFallbackReason::all`] gives callers the full
/// code list when they need zero rows too.
pub(crate) fn fallback_counts(
    fallbacks: &[crate::tensor_report::TensorFallback],
) -> Vec<TensorFallbackCount> {
    let mut totals = BTreeMap::<TensorFallbackReason, (usize, usize, usize)>::new();
    for fallback in fallbacks {
        let entry = totals.entry(fallback.reason).or_insert((0, 0, 0));
        entry.0 = entry.0.saturating_add(1);
        entry.1 = entry.1.saturating_add(fallback.scalarized_bodies);
        entry.2 = entry.2.saturating_add(fallback.scalarized_rows);
    }
    totals
        .into_iter()
        .map(
            |(reason, (families, scalarized_bodies, scalarized_rows))| TensorFallbackCount {
                reason,
                families,
                scalarized_bodies,
                scalarized_rows,
            },
        )
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fallback_reason_codes_are_stable_strings() {
        let codes = TensorFallbackReason::all()
            .iter()
            .map(|reason| reason.code())
            .collect::<Vec<_>>();
        assert_eq!(
            codes,
            vec![
                "solve:singleton-family-domain",
                "solve:structured-rows-lost",
                "solve:non-compact-candidate-domain",
                "solve:mismatched-dae-body-shape",
                "solve:missing-affine-access-proof",
                "solve:non-affine-output-map",
                "solve:family-rows-absent",
                "solve:unattributed-family-decline",
                "solve:incomplete-tensor-coverage",
            ]
        );
    }

    #[test]
    fn every_reason_code_is_unique_and_solve_scoped() {
        let mut seen = BTreeSet::new();
        for reason in TensorFallbackReason::all() {
            let code = reason.code();
            assert!(code.starts_with("solve:"), "unscoped code {code}");
            assert!(seen.insert(code), "duplicate code {code}");
        }
        assert_eq!(seen.len(), TensorFallbackReason::all().len());
    }

    #[test]
    fn headroom_classification_covers_every_reason() {
        let unknown = TensorFallbackReason::all()
            .iter()
            .filter(|reason| reason.headroom() == TensorHeadroom::Unknown)
            .count();
        let correctly_scalar = TensorFallbackReason::all()
            .iter()
            .filter(|reason| reason.headroom() == TensorHeadroom::CorrectlyScalar)
            .count();
        let headroom = TensorFallbackReason::all()
            .iter()
            .filter(|reason| reason.headroom() == TensorHeadroom::RealHeadroom)
            .count();
        assert_eq!(unknown + correctly_scalar + headroom, 9);
        assert_eq!(correctly_scalar, 2);
        assert_eq!(headroom, 3);
        assert_eq!(unknown, 4);
        assert_eq!(TensorHeadroom::RealHeadroom.as_str(), "real-headroom");
    }

    #[test]
    fn absent_rows_and_unattributed_declines_are_different_facts() {
        let mut journal = TensorDeclineJournal::new();
        journal.observe_family(7);
        assert!(journal.was_observed(7));
        assert!(!journal.was_observed(9));
        // Observed, nothing declined: the journal holds no cause, and saying
        // "lowering never saw a candidate" here would be fiction.
        assert_eq!(
            journal.reason_for_family(7),
            TensorFallbackReason::UnattributedFamilyDecline
        );
        assert_eq!(journal.recorded_reason_for_family(7), None);
        // Never observed: absence from the observed set is the recorded fact.
        assert_eq!(
            journal.reason_for_family(9),
            TensorFallbackReason::FamilyRowsAbsent
        );
    }

    #[test]
    fn first_recorded_decline_wins_for_a_family() {
        let mut journal = TensorDeclineJournal::new();
        journal.record(3, TensorFallbackReason::NonAffineOutputMap);
        journal.record(3, TensorFallbackReason::MismatchedDaeBodyShape);
        assert_eq!(
            journal.reason_for_family(3),
            TensorFallbackReason::NonAffineOutputMap
        );
        assert_eq!(journal.records().len(), 2);
    }
}
