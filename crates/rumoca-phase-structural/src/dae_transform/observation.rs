//! Observation-only seam for structural index reduction.
//!
//! Every borrowed type here names data the reducer already computed to make
//! its own decision; nothing is computed for observation's sake. `observe`
//! returns `()` and its argument only borrows, so an observer cannot feed
//! anything back into the reduction it is watching — that is a type-level
//! guarantee, not a promise. `()` is the production observer: its `observe`
//! body is empty, so [`crate::prepare_for_solve`] allocates and clones
//! nothing for this seam. `ReductionRecorder` is the one observer that does
//! real work, turning each borrowed event into an owned [`ReductionRecord`]
//! for [`crate::inspect_prepare_for_solve`], the sole crate-external surface.
//! Everything else in this module — the trait, the borrowed event types, the
//! recorder — stays private to [`super`], so no consumer can acquire the
//! reduction protocol itself, only the data one recorder already extracted
//! from it.

use rumoca_core::Span;

use super::{DirectStateConstraint, HolonomicConstraint};
use crate::StructuralError;

/// Which fixed-point lane a reduction event belongs to.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Lane {
    Direct,
    Holonomic,
}

/// Which proof boundary produced one candidate list.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum CandidateGroup {
    DirectAdmissible,
    DirectConditional,
    Holonomic,
}

/// A borrowed identity for one attempted direct-state candidate.
#[derive(Clone, Copy)]
pub(super) struct DirectIdentity {
    pub(super) state_ordinal: u32,
    pub(super) rhs_ordinal: u32,
    pub(super) provenance_span: Span,
}

impl From<&DirectStateConstraint> for DirectIdentity {
    fn from(candidate: &DirectStateConstraint) -> Self {
        Self {
            state_ordinal: candidate.state,
            rhs_ordinal: candidate.rhs,
            provenance_span: candidate.owner.span(),
        }
    }
}

/// A borrowed identity for one attempted holonomic candidate.
#[derive(Clone, Copy)]
pub(super) struct HolonomicIdentity<'a> {
    pub(super) owner_ordinal: usize,
    pub(super) residual_ordinal: u32,
    pub(super) owner_span: Span,
    pub(super) anchored_state_ordinals: &'a [u32],
}

impl<'a> From<&'a HolonomicConstraint> for HolonomicIdentity<'a> {
    fn from(candidate: &'a HolonomicConstraint) -> Self {
        Self {
            owner_ordinal: candidate.owner_ordinal,
            residual_ordinal: candidate.residual,
            owner_span: candidate.owner.span(),
            anchored_state_ordinals: &candidate.proof.anchored_states,
        }
    }
}

/// Which lane-specific identity one event names. Candidate counts alone
/// cannot distinguish discovery from selection, so every attempted candidate
/// carries its own identity rather than a shared ordinal space.
#[derive(Clone, Copy)]
pub(super) enum Identity<'a> {
    Direct(DirectIdentity),
    Holonomic(HolonomicIdentity<'a>),
}

/// What one attempted candidate's rebuilt-and-resorted system proved.
///
/// An improving attempt is not proof that it was chosen — `Reduced`/`Held`
/// candidates that lose to another candidate in the same pass never become a
/// [`ReductionEvent::Selected`]. Reconstruction errors are recorded here,
/// immediately before the existing `?` propagates them out of the pass.
#[derive(Clone, Copy)]
pub(super) enum AttemptOutcome<'a> {
    /// The rebuilt system matched completely; nothing residual is left.
    Sorted,
    /// The rebuilt system is still singular, with a strictly smaller residue.
    Reduced { residue: usize },
    /// The rebuilt system is still singular, with an unchanged residue. The
    /// direct lane may keep it as a fallback; the holonomic lane refuses it.
    Held { residue: usize },
    /// The rebuilt system's residue is larger; never accepted.
    Raised { residue: usize },
    /// Reconstruction failed, or the rebuilt system failed to sort for a
    /// reason other than an ordinary singularity; never accepted.
    NonSingularFailure { error: &'a StructuralError },
    /// The rebuilt system no longer states an MLS 3.6 section 8.6 initial
    /// value the source system stated; never accepted.
    WouldDiscardInitial { variable: &'a str, span: Span },
    /// Demotion would invalidate a retained state-manifold projection row.
    WouldInvalidateManifold,
    /// Holonomic reconstruction produced no residual scalar containing a
    /// continuous unknown. Such an equation cannot participate in the
    /// structural replacement the certificate promises.
    WouldCreateVacuousResidual,
}

/// The final disposition [`crate::prepare_for_solve`] reached.
#[derive(Clone, Copy)]
pub(super) enum StoppedOutcome<'a> {
    Borrowed,
    Sorted,
    Failure { error: &'a StructuralError },
    Singular { error: &'a StructuralError },
    DiscardsInitial { variable: &'a str, span: Span },
}

/// One fact the reducer already computed, borrowed for the duration of one
/// `observe` call.
pub(super) enum ReductionEvent<'a> {
    /// One lane's residue and unmatched names as of round entry, read
    /// straight off the [`StructuralError::Singular`] that proved them — no
    /// new computation, no `matching`/`incidence` involvement.
    Round {
        lane: Lane,
        round: u32,
        error: &'a StructuralError,
    },
    /// How many candidates this pass discovered, before any is attempted.
    Candidates {
        lane: Lane,
        group: CandidateGroup,
        discovered: usize,
    },
    /// One attempted candidate and what its rebuilt system proved.
    Attempt {
        lane: Lane,
        identity: Identity<'a>,
        outcome: AttemptOutcome<'a>,
    },
    /// The candidate this pass actually chose, fired only after the
    /// `reduced.or(held)` / owner-order choice is final.
    Selected {
        lane: Lane,
        identity: Identity<'a>,
        residue_before: usize,
        residue_after: Option<usize>,
    },
    /// A stalled reduction over the demoted DAE was discarded before the
    /// reducer retried the pristine source DAE.
    RetriedPristine { lane: Lane },
    /// The one terminal event for a whole [`crate::prepare_for_solve`] call.
    Stopped { outcome: StoppedOutcome<'a> },
}

/// The read-only seam production and diagnostic code share.
///
/// `observe` returns `()` and its argument is built entirely from data the
/// caller already had in hand, so an implementation cannot influence which
/// candidate is tried next, which is accepted, or what the reduction returns
/// — the trait admits no channel back into the reduction it watches.
pub(super) trait ReductionObserver {
    fn observe(&mut self, event: ReductionEvent<'_>);
}

impl ReductionObserver for () {
    fn observe(&mut self, _event: ReductionEvent<'_>) {}
}

/// A coarse, presentation-only classification of one unmatched unknown name.
///
/// Classified from the already-rendered [`StructuralError::Singular`] string
/// at recording time, never from IR: matching, incidence, and production
/// diagnostics never see this enum. The three forms mirror exactly the four
/// render arms of `unknown_label` in `lib.rs` — `der(name)` is a state
/// derivative, `y[i]` and `<unmatched f_x[i]>` are solver/unmatched
/// fallbacks classified as `Other`, and every other rendered name is a bare
/// scalar name, which for an unmatched unknown is always algebraic.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnmatchedKind {
    Derivative,
    Algebraic,
    Other,
}

fn classify_unmatched_kind(name: &str) -> UnmatchedKind {
    if name.starts_with("der(") {
        UnmatchedKind::Derivative
    } else if name.starts_with("y[") || name.starts_with("<unmatched") {
        UnmatchedKind::Other
    } else {
        UnmatchedKind::Algebraic
    }
}

/// One unmatched unknown's rendered name and presentation-only kind.
#[derive(Clone, Debug)]
pub struct UnmatchedName {
    pub name: String,
    pub kind: UnmatchedKind,
}

/// Which fixed-point lane an owned [`ReductionRecord`] reports.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReductionLane {
    Direct,
    Holonomic,
}

/// Which proof boundary produced a recorded candidate list.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReductionCandidateGroup {
    DirectAdmissible,
    DirectConditional,
    Holonomic,
}

impl From<CandidateGroup> for ReductionCandidateGroup {
    fn from(group: CandidateGroup) -> Self {
        match group {
            CandidateGroup::DirectAdmissible => Self::DirectAdmissible,
            CandidateGroup::DirectConditional => Self::DirectConditional,
            CandidateGroup::Holonomic => Self::Holonomic,
        }
    }
}

impl From<Lane> for ReductionLane {
    fn from(lane: Lane) -> Self {
        match lane {
            Lane::Direct => Self::Direct,
            Lane::Holonomic => Self::Holonomic,
        }
    }
}

/// The owned twin of `Identity`: one attempted candidate's lane-specific
/// identity, with every borrow resolved to an owned value.
#[derive(Clone, Debug)]
pub enum ReductionIdentity {
    Direct {
        state_ordinal: u32,
        rhs_ordinal: u32,
        provenance_span: Span,
    },
    Holonomic {
        owner_ordinal: usize,
        residual_ordinal: u32,
        owner_span: Span,
        anchored_state_ordinals: Vec<u32>,
    },
}

impl From<Identity<'_>> for ReductionIdentity {
    fn from(identity: Identity<'_>) -> Self {
        match identity {
            Identity::Direct(DirectIdentity {
                state_ordinal,
                rhs_ordinal,
                provenance_span,
            }) => Self::Direct {
                state_ordinal,
                rhs_ordinal,
                provenance_span,
            },
            Identity::Holonomic(HolonomicIdentity {
                owner_ordinal,
                residual_ordinal,
                owner_span,
                anchored_state_ordinals,
            }) => Self::Holonomic {
                owner_ordinal,
                residual_ordinal,
                owner_span,
                anchored_state_ordinals: anchored_state_ordinals.to_vec(),
            },
        }
    }
}

/// The owned twin of `AttemptOutcome`.
#[derive(Clone, Debug)]
pub enum ReductionOutcome {
    Sorted,
    Reduced { residue: usize },
    Held { residue: usize },
    Raised { residue: usize },
    NonSingularFailure { error: StructuralError },
    WouldDiscardInitial { variable: String, span: Span },
    WouldInvalidateManifold,
    WouldCreateVacuousResidual,
}

impl From<AttemptOutcome<'_>> for ReductionOutcome {
    fn from(outcome: AttemptOutcome<'_>) -> Self {
        match outcome {
            AttemptOutcome::Sorted => Self::Sorted,
            AttemptOutcome::Reduced { residue } => Self::Reduced { residue },
            AttemptOutcome::Held { residue } => Self::Held { residue },
            AttemptOutcome::Raised { residue } => Self::Raised { residue },
            AttemptOutcome::NonSingularFailure { error } => Self::NonSingularFailure {
                error: error.clone(),
            },
            AttemptOutcome::WouldDiscardInitial { variable, span } => Self::WouldDiscardInitial {
                variable: variable.to_string(),
                span,
            },
            AttemptOutcome::WouldInvalidateManifold => Self::WouldInvalidateManifold,
            AttemptOutcome::WouldCreateVacuousResidual => Self::WouldCreateVacuousResidual,
        }
    }
}

/// The owned twin of `StoppedOutcome`.
#[derive(Clone, Debug)]
pub enum ReductionStop {
    Borrowed,
    Sorted,
    Failure { error: StructuralError },
    Singular { residue: usize },
    DiscardsInitial { variable: String, span: Span },
}

impl From<StoppedOutcome<'_>> for ReductionStop {
    fn from(outcome: StoppedOutcome<'_>) -> Self {
        match outcome {
            StoppedOutcome::Borrowed => Self::Borrowed,
            StoppedOutcome::Sorted => Self::Sorted,
            StoppedOutcome::Failure { error } => Self::Failure {
                error: error.clone(),
            },
            StoppedOutcome::Singular { error } => Self::Singular {
                residue: super::unmatched_residue(error)
                    .expect("Stopped::Singular only fires for a Singular structural error"),
            },
            StoppedOutcome::DiscardsInitial { variable, span } => Self::DiscardsInitial {
                variable: variable.to_string(),
                span,
            },
        }
    }
}

/// One recorded fact from a traced [`crate::inspect_prepare_for_solve`] call,
/// the owned twin of `ReductionEvent`.
#[derive(Clone, Debug)]
pub enum ReductionRecord {
    Round {
        lane: ReductionLane,
        round: u32,
        residue: usize,
        unmatched_equations: Vec<String>,
        unmatched_unknowns: Vec<UnmatchedName>,
    },
    Candidates {
        lane: ReductionLane,
        group: ReductionCandidateGroup,
        discovered: usize,
    },
    Attempt {
        lane: ReductionLane,
        identity: ReductionIdentity,
        outcome: ReductionOutcome,
    },
    Selected {
        lane: ReductionLane,
        identity: ReductionIdentity,
        residue_before: usize,
        residue_after: Option<usize>,
    },
    RetriedPristine {
        lane: ReductionLane,
    },
    Stopped {
        outcome: ReductionStop,
    },
}

/// The owned report [`crate::inspect_prepare_for_solve`] returns: every event
/// the traced call actually observed, in the order it observed them.
#[derive(Clone, Debug, Default)]
pub struct ReductionReport {
    pub records: Vec<ReductionRecord>,
}

/// The one `ReductionObserver` outside `()` used in this crate: it clones
/// and classifies each borrowed event into an owned [`ReductionRecord`],
/// which is exactly the work `()` skips on every other call.
#[derive(Default)]
pub(super) struct ReductionRecorder {
    records: Vec<ReductionRecord>,
}

impl ReductionRecorder {
    pub(super) fn finish(self) -> ReductionReport {
        ReductionReport {
            records: self.records,
        }
    }
}

impl ReductionObserver for ReductionRecorder {
    fn observe(&mut self, event: ReductionEvent<'_>) {
        let record = match event {
            ReductionEvent::Round { lane, round, error } => {
                let StructuralError::Singular {
                    unmatched_equations,
                    unmatched_unknowns,
                    ..
                } = error
                else {
                    unreachable!("Round events only fire for a Singular structural error")
                };
                ReductionRecord::Round {
                    lane: lane.into(),
                    round,
                    residue: super::unmatched_residue(error)
                        .expect("Round events only fire for a Singular structural error"),
                    unmatched_equations: unmatched_equations.clone(),
                    unmatched_unknowns: unmatched_unknowns
                        .iter()
                        .map(|name| UnmatchedName {
                            kind: classify_unmatched_kind(name),
                            name: name.clone(),
                        })
                        .collect(),
                }
            }
            ReductionEvent::Candidates {
                lane,
                group,
                discovered,
            } => ReductionRecord::Candidates {
                lane: lane.into(),
                group: group.into(),
                discovered,
            },
            ReductionEvent::Attempt {
                lane,
                identity,
                outcome,
            } => ReductionRecord::Attempt {
                lane: lane.into(),
                identity: identity.into(),
                outcome: outcome.into(),
            },
            ReductionEvent::Selected {
                lane,
                identity,
                residue_before,
                residue_after,
            } => ReductionRecord::Selected {
                lane: lane.into(),
                identity: identity.into(),
                residue_before,
                residue_after,
            },
            ReductionEvent::RetriedPristine { lane } => {
                ReductionRecord::RetriedPristine { lane: lane.into() }
            }
            ReductionEvent::Stopped { outcome } => ReductionRecord::Stopped {
                outcome: outcome.into(),
            },
        };
        self.records.push(record);
    }
}
