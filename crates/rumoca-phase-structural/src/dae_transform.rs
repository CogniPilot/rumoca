//! Constructor-only structural DAE-to-DAE lowering.
//!
//! Regular systems remain borrowed. A singular system is rebuilt only when a
//! scalar state is directly defined by a differentiable constraint. The
//! replacement DAE demotes that state and substitutes the exact symbolic
//! derivative of its definition at every derivative occurrence.

mod constraints;
mod declarations;
mod differentiation;
mod equalities;
mod event_owners;
mod expressions;
mod functions;
mod initial_pins;
mod observation;
mod reconstruction;
mod runtime_quotients;
mod semantic_owners;
mod temporal;
#[cfg(test)]
mod tests;
mod variables;

use rumoca_ir_dae as dae;
use std::collections::BTreeSet;

#[cfg(test)]
use self::constraints::holonomic_constraints;
use self::constraints::{
    DiscardedInitialValue, direct_state_constraints, discarded_stated_initial_value,
    index_reduction_constraints,
};
use self::initial_pins::{represented_initial_values, transferred_initial_values};
use self::observation::{
    AttemptOutcome, CandidateGroup, DirectIdentity, HolonomicIdentity, Identity, Lane,
    ReductionEvent, ReductionObserver, ReductionRecorder, StoppedOutcome,
};
#[cfg(test)]
use self::reconstruction::rebuild_with_state_demotion;
use self::reconstruction::{
    rebuild_holonomic_constraint, rebuild_with_state_demotion_and_manifold,
};
use crate::{
    BltBlock, EquationRef, SortedDae, StructuralError, StructuredScalarBlock, UnknownId, sort,
};

pub use self::initial_pins::{InitialValuePin, InitialValueRole, PinTerm};
pub use self::observation::{
    ReductionCandidateGroup, ReductionIdentity, ReductionLane, ReductionOutcome, ReductionRecord,
    ReductionReport, ReductionStop, UnmatchedKind, UnmatchedName,
};

/// A finalized DAE ready for Solve lowering.
pub enum PreparedDae<'source> {
    Borrowed {
        dae: &'source dae::Dae,
        pins: Box<[InitialValuePin]>,
        structural: PreparedStructuralAnalysis,
    },
    Transformed {
        dae: Box<dae::Dae>,
        manifold: Box<[u32]>,
        pins: Box<[InitialValuePin]>,
        structural: PreparedStructuralAnalysis,
    },
}

impl PreparedDae<'_> {
    pub fn as_dae(&self) -> &dae::Dae {
        match self {
            Self::Borrowed { dae, .. } => dae,
            Self::Transformed { dae, .. } => dae,
        }
    }

    pub fn inspect<R>(&self, inspect: impl for<'dae> FnOnce(PreparedSystem<'_, 'dae>) -> R) -> R {
        let (manifold, pins, structural) = match self {
            Self::Borrowed {
                pins, structural, ..
            } => ([].as_slice(), pins, structural),
            Self::Transformed {
                manifold,
                pins,
                structural,
                ..
            } => (&**manifold, pins, structural),
        };
        self.as_dae().inspect(|view| {
            let manifold = manifold
                .iter()
                .map(|index| {
                    view.expression_id(*index as usize)
                        .expect("prepared manifold expression resolves")
                })
                .collect::<Vec<_>>();
            inspect(PreparedSystem {
                view,
                manifold: &manifold,
                pins,
                structural: structural.bind(view),
            })
        })
    }
}

/// One prepared system, as the Solve lowering reads it.
///
/// The manifold expressions and the transferred initial values are products of
/// this phase's proof, not of the DAE: they name ordinals inside `view`, so
/// they are handed over together with the view they are branded against.
pub struct PreparedSystem<'prepared, 'dae> {
    pub view: dae::DaeView<'dae>,
    /// Constraint expressions an index reduction left on the state manifold.
    pub manifold: &'prepared [dae::ExprId<'dae>],
    /// MLS 3.6 §8.6 initial equations rewritten onto the state each one
    /// determines. The proof that decides which stated value defines a state and
    /// which one only restates it lives in this phase's `initial_pins` module,
    /// which is private — naming it as a doc link would make the public page
    /// point at an item its reader cannot open.
    pub pins: &'prepared [InitialValuePin],
    /// Structural matching and BLT analysis issued while this exact finalized
    /// DAE was admitted by structural preparation.
    pub structural: Option<SortedDae<'dae>>,
}

/// The structural analysis coupled to one prepared DAE root.
///
/// Identities are temporarily stored as DAE-local ordinals because the owned
/// transformed root cannot carry a self-borrowing brand. `PreparedDae::inspect`
/// is the only place that rebrands them, against the same root that issued the
/// analysis. This product is never serialized or accepted from callers.
pub struct PreparedStructuralAnalysis {
    sorted: Option<ErasedSortedDae>,
}

struct ErasedSortedDae {
    blocks: Vec<ErasedBltBlock>,
    matching: Vec<(EquationRef, ErasedUnknown)>,
    diagnostics: Vec<rumoca_core::Diagnostic>,
}

enum ErasedBltBlock {
    Scalar {
        equation: EquationRef,
        unknown: ErasedUnknown,
    },
    AlgebraicLoop {
        equations: Vec<EquationRef>,
        unknowns: Vec<ErasedUnknown>,
    },
    StructuredScalar(StructuredScalarBlock),
}

#[derive(Clone, Copy)]
enum ErasedUnknown {
    Derivative { variable: u32, scalar: u32 },
    Algebraic { variable: u32, scalar: u32 },
    Solver(usize),
    Unmatched { equation: usize },
}

impl PreparedStructuralAnalysis {
    fn empty() -> Self {
        Self { sorted: None }
    }

    fn issue(sorted: SortedDae<'_>) -> Self {
        Self {
            sorted: Some(ErasedSortedDae::erase(sorted)),
        }
    }

    fn bind<'dae>(&self, view: dae::DaeView<'dae>) -> Option<SortedDae<'dae>> {
        self.sorted.as_ref().map(|sorted| sorted.bind(view))
    }
}

impl ErasedSortedDae {
    fn erase(sorted: SortedDae<'_>) -> Self {
        Self {
            blocks: sorted
                .blocks
                .into_iter()
                .map(ErasedBltBlock::erase)
                .collect(),
            matching: sorted
                .matching
                .into_iter()
                .map(|(equation, unknown)| (equation, ErasedUnknown::erase(unknown)))
                .collect(),
            diagnostics: sorted.diagnostics,
        }
    }

    fn bind<'dae>(&self, view: dae::DaeView<'dae>) -> SortedDae<'dae> {
        SortedDae {
            blocks: self.blocks.iter().map(|block| block.bind(view)).collect(),
            matching: self
                .matching
                .iter()
                .map(|(equation, unknown)| (*equation, unknown.bind(view)))
                .collect(),
            diagnostics: self.diagnostics.clone(),
        }
    }
}

impl ErasedBltBlock {
    fn erase(block: BltBlock<'_>) -> Self {
        match block {
            BltBlock::Scalar { equation, unknown } => Self::Scalar {
                equation,
                unknown: ErasedUnknown::erase(unknown),
            },
            BltBlock::AlgebraicLoop {
                equations,
                unknowns,
            } => Self::AlgebraicLoop {
                equations,
                unknowns: unknowns.into_iter().map(ErasedUnknown::erase).collect(),
            },
            BltBlock::StructuredScalar(block) => Self::StructuredScalar(block),
        }
    }

    fn bind<'dae>(&self, view: dae::DaeView<'dae>) -> BltBlock<'dae> {
        match self {
            Self::Scalar { equation, unknown } => BltBlock::Scalar {
                equation: *equation,
                unknown: unknown.bind(view),
            },
            Self::AlgebraicLoop {
                equations,
                unknowns,
            } => BltBlock::AlgebraicLoop {
                equations: equations.clone(),
                unknowns: unknowns.iter().map(|unknown| unknown.bind(view)).collect(),
            },
            Self::StructuredScalar(block) => BltBlock::StructuredScalar(block.clone()),
        }
    }
}

impl ErasedUnknown {
    fn erase(unknown: UnknownId<'_>) -> Self {
        match unknown {
            UnknownId::Derivative { state, scalar } => Self::Derivative {
                variable: state.index(),
                scalar,
            },
            UnknownId::Algebraic { variable, scalar } => Self::Algebraic {
                variable: variable.index(),
                scalar,
            },
            UnknownId::Solver(index) => Self::Solver(index),
            UnknownId::Unmatched { equation } => Self::Unmatched { equation },
        }
    }

    fn bind<'dae>(self, view: dae::DaeView<'dae>) -> UnknownId<'dae> {
        match self {
            Self::Derivative { variable, scalar } => {
                let id = view
                    .variable_id(variable as usize)
                    .and_then(|id| view.variable(id))
                    .and_then(|variable| match variable.identity() {
                        dae::VariableIdentity::State(state) => Some(state),
                        _ => None,
                    })
                    .expect("prepared derivative identity resolves against its issuing DAE");
                UnknownId::Derivative { state: id, scalar }
            }
            Self::Algebraic { variable, scalar } => {
                let id = view
                    .variable_id(variable as usize)
                    .and_then(|id| view.variable(id))
                    .and_then(|variable| match variable.identity() {
                        dae::VariableIdentity::Algebraic(algebraic) => Some(algebraic),
                        _ => None,
                    })
                    .expect("prepared algebraic identity resolves against its issuing DAE");
                UnknownId::Algebraic {
                    variable: id,
                    scalar,
                }
            }
            Self::Solver(index) => UnknownId::Solver(index),
            Self::Unmatched { equation } => UnknownId::Unmatched { equation },
        }
    }
}

fn structural_analysis(model: &dae::Dae) -> Result<PreparedStructuralAnalysis, StructuralError> {
    model.inspect(|view| sort(view).map(PreparedStructuralAnalysis::issue))
}

#[derive(Clone, Copy)]
struct DirectStateConstraint {
    state: u32,
    rhs: u32,
    rhs_sign: self::equalities::EqualitySign,
    owner: dae::DaeProvenance,
}

#[derive(Clone)]
struct HolonomicConstraint {
    owner_ordinal: usize,
    body_ordinal: Option<usize>,
    residual: u32,
    owner: dae::DaeProvenance,
    proof: HolonomicDifferentiationProof,
    lifted_algebraic: Option<u32>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ManifoldConstraint {
    expression: u32,
    lifted: Option<LiftedManifoldOwner>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct LiftedManifoldOwner {
    state: u32,
    owner_ordinal: usize,
    body_ordinal: Option<usize>,
    residual: u32,
}

/// Evidence collected from the finalized source DAE before a residual may be
/// differentiated for index reduction.
#[derive(Clone)]
struct HolonomicDifferentiationProof {
    residual: u32,
    maximum_order: u8,
    anchored_states: Box<[u32]>,
}

/// Prepare a finalized DAE for Solve without admitting a weaker intermediate.
///
/// The currently accepted index-one subset is intentionally narrow: primitive
/// scalar expressions, direct state definitions, and unstructured continuous
/// and initialization residuals. A model outside that subset retains its
/// original structural error instead of receiving a guessed transformation.
///
/// State demotions accumulate to a fixed point. A candidate that matches the
/// whole system wins outright; otherwise the first candidate that strictly
/// shrinks the unmatched residue is applied and the remaining candidates are
/// re-tested against that updated system. A singularity that only several
/// simultaneous demotions resolve therefore reduces, while a model that no
/// demotion improves still reports its original singularity.
///
/// A differentiation chain can pass through steps that leave the residue
/// unchanged. Such a step is accepted only when it consumes one proved direct
/// demotion or promotes one frontier algebraic into a state. Every accepted
/// round therefore strictly decreases the lexicographic pair of unmatched
/// residue and remaining eligible coordinates.
pub fn prepare_for_solve(model: &dae::Dae) -> Result<PreparedDae<'_>, StructuralError> {
    prepare_for_solve_with_observer(model, &mut ())
}

/// The diagnostic inspection surface: the exact same reduction as
/// [`prepare_for_solve`], paired with an owned [`ReductionReport`] of every
/// round this call actually traversed.
///
/// `ReductionRecorder` is the only `ReductionObserver` this crate builds
/// besides the no-op `()` `prepare_for_solve` uses, so this function and
/// `prepare_for_solve` are the two instantiations of one generic seam,
/// `prepare_for_solve_with_observer`. Its observer type is erased from both
/// public signatures: a caller of this function receives the owned data one
/// recorder already extracted, never the borrowed callback protocol that
/// produced it.
pub fn inspect_prepare_for_solve(
    model: &dae::Dae,
) -> (Result<PreparedDae<'_>, StructuralError>, ReductionReport) {
    let mut recorder = ReductionRecorder::default();
    let result = prepare_for_solve_with_observer(model, &mut recorder);
    (result, recorder.finish())
}

fn prepare_for_solve_with_observer<'source>(
    model: &'source dae::Dae,
    observer: &mut impl ReductionObserver,
) -> Result<PreparedDae<'source>, StructuralError> {
    let singular = match structural_analysis(model) {
        Ok(structural) => return borrowed_with_observer(model, structural, observer),
        Err(error @ StructuralError::Singular { .. }) => error,
        Err(StructuralError::EmptySystem) => {
            return borrowed_with_observer(model, PreparedStructuralAnalysis::empty(), observer);
        }
        Err(error) => {
            observer.observe(ReductionEvent::Stopped {
                outcome: StoppedOutcome::Failure { error: &error },
            });
            return Err(error);
        }
    };
    let mut residue =
        unmatched_residue(&singular).expect("singular system reports its unmatched residue");
    let mut demoted: Option<dae::Dae> = None;
    let mut demoted_error: Option<StructuralError> = None;
    let mut round_number: u32 = 0;
    let blocked = loop {
        round_number += 1;
        let current_model = demoted.as_ref().unwrap_or(model);
        let current_error = demoted_error.as_ref().unwrap_or(&singular);
        observer.observe(ReductionEvent::Round {
            lane: Lane::Direct,
            round: round_number,
            error: current_error,
        });
        let round =
            match demote_direct_state_with_observer(current_model, residue, &[], true, observer) {
                Ok(round) => round,
                Err(error) => return observed_failure(error, observer),
            };
        match round.step {
            None => break round.blocked,
            Some(DemotionStep::Sorted {
                dae, structural, ..
            }) => {
                return transformed_with_observer(dae, Vec::new(), structural, observer);
            }
            Some(DemotionStep::Reduced {
                dae,
                residue: next,
                error,
                ..
            }) => {
                residue = next;
                demoted_error = Some(error);
                demoted = Some(dae);
            }
        }
    };
    let mut holonomic = match reduce_holonomic_constraint_with_observer(
        demoted.as_ref().unwrap_or(model),
        |_| {},
        observer,
    ) {
        Ok(round) => round,
        Err(error) => return observed_failure(error, observer),
    };
    if holonomic.step.is_none() && demoted.is_some() {
        observer.observe(ReductionEvent::RetriedPristine {
            lane: Lane::Holonomic,
        });
        let pristine = match reduce_holonomic_constraint_with_observer(model, |_| {}, observer) {
            Ok(round) => round,
            Err(error) => return observed_failure(error, observer),
        };
        holonomic = HolonomicRound {
            step: pristine.step,
            blocked: pristine.blocked.or(holonomic.blocked),
        };
    }
    match (holonomic.step, blocked.or(holonomic.blocked)) {
        (Some((dae, manifold, structural)), _) => transformed_with_observer(
            dae,
            manifold.into_iter().map(|entry| entry.expression).collect(),
            structural,
            observer,
        ),
        // The only reduction left was one that would have discarded a stated
        // initial condition. Report that, not the singularity it hides behind:
        // a bare `ES010` would send a modeller looking for a missing equation.
        (None, Some(blocked)) => {
            observer.observe(ReductionEvent::Stopped {
                outcome: StoppedOutcome::DiscardsInitial {
                    variable: &blocked.variable,
                    span: blocked.span,
                },
            });
            Err(StructuralError::DroppedStatedInitialValue {
                variable: blocked.variable,
                span: blocked.span,
            })
        }
        (None, None) => {
            observer.observe(ReductionEvent::Stopped {
                outcome: StoppedOutcome::Singular { error: &singular },
            });
            Err(singular)
        }
    }
}

/// Hand back a system this phase did not have to rewrite, with the initial
/// values its equalities carry onto the states the runtime seeds.
fn borrowed(
    model: &dae::Dae,
    structural: PreparedStructuralAnalysis,
) -> Result<PreparedDae<'_>, StructuralError> {
    let pins = model.inspect(transferred_initial_values)?;
    Ok(PreparedDae::Borrowed {
        dae: model,
        pins: pins.into_boxed_slice(),
        structural,
    })
}

fn observed_failure<T>(
    error: StructuralError,
    observer: &mut impl ReductionObserver,
) -> Result<T, StructuralError> {
    observer.observe(ReductionEvent::Stopped {
        outcome: StoppedOutcome::Failure { error: &error },
    });
    Err(error)
}

fn borrowed_with_observer<'source>(
    model: &'source dae::Dae,
    structural: PreparedStructuralAnalysis,
    observer: &mut impl ReductionObserver,
) -> Result<PreparedDae<'source>, StructuralError> {
    let result = borrowed(model, structural);
    observer.observe(ReductionEvent::Stopped {
        outcome: match &result {
            Ok(_) => StoppedOutcome::Borrowed,
            Err(error) => StoppedOutcome::Failure { error },
        },
    });
    result
}

/// Hand back a rewritten system, reading its initial values off the *replacement*
/// so that a demotion's new roles decide which coordinate the runtime seeds.
fn transformed(
    model: dae::Dae,
    manifold: Vec<u32>,
    structural: PreparedStructuralAnalysis,
) -> Result<PreparedDae<'static>, StructuralError> {
    let pins = model.inspect(transferred_initial_values)?;
    Ok(PreparedDae::Transformed {
        dae: Box::new(model),
        manifold: manifold.into_boxed_slice(),
        pins: pins.into_boxed_slice(),
        structural,
    })
}

fn transformed_with_observer(
    model: dae::Dae,
    manifold: Vec<u32>,
    structural: PreparedStructuralAnalysis,
    observer: &mut impl ReductionObserver,
) -> Result<PreparedDae<'static>, StructuralError> {
    let result = transformed(model, manifold, structural);
    observer.observe(ReductionEvent::Stopped {
        outcome: match &result {
            Ok(_) => StoppedOutcome::Sorted,
            Err(error) => StoppedOutcome::Failure { error },
        },
    });
    result
}

/// One accepted state demotion: either a fully matched replacement or a
/// non-increasing residue that the next round keeps working on. `Reduced`
/// retains the exact [`StructuralError::Singular`] its residue was read from,
/// so the next round's observed [`ReductionEvent::Round`] costs no
/// recomputation — it borrows the same proof the accumulation already made.
enum DemotionStep {
    Sorted {
        dae: dae::Dae,
        manifold: Vec<ManifoldConstraint>,
        structural: PreparedStructuralAnalysis,
    },
    Reduced {
        dae: dae::Dae,
        manifold: Vec<ManifoldConstraint>,
        residue: usize,
        error: StructuralError,
    },
}

/// What one demotion round found.
struct DemotionRound {
    /// The demotion this round took, if any.
    step: Option<DemotionStep>,
    /// Only ever set when `step` is `None`: a stated initial value that the only
    /// demotions left would have discarded. That refusal is the reason the
    /// system stops reducing here, so it is what the phase reports rather than
    /// the singularity it hides behind.
    blocked: Option<DiscardedInitialValue>,
}

#[derive(Clone, Copy)]
struct DemotionPassPolicy {
    group: CandidateGroup,
    allow_held: bool,
}

/// Demote one directly defined state of `model`.
///
/// Every candidate is tested against `model` itself, so an accumulated
/// demotion is re-tested against the system it produced rather than against a
/// stale pristine one. `residue` is the unmatched residue of `model`. A
/// strictly shrinking candidate is preferred; a candidate that merely holds the
/// residue is kept only as a fallback, because a higher-index chain has to pass
/// through such a step before the next demotion can pay for it. A candidate
/// that raises the residue is never accepted, so each accepted round strictly
/// decreases the pair (residue, remaining states) and the accumulation stops.
///
/// Two passes, and the order between them is the whole point: a demotion that
/// costs no stated initial value is taken before one whose legality rests on
/// carrying a value elsewhere, so a model that can reduce without moving an
/// initial condition never moves one. Both passes prove the MLS 3.6 §8.6
/// obligations of `model` survive into the system they hand back — see
/// [`discarded_stated_initial_value`] — so the values the *original* system
/// stated survive the whole accumulation by induction over its rounds.
fn demote_direct_state_with_observer(
    model: &dae::Dae,
    residue: usize,
    prior_manifold: &[ManifoldConstraint],
    allow_held: bool,
    observer: &mut impl ReductionObserver,
) -> Result<DemotionRound, StructuralError> {
    let candidates = model.inspect(direct_state_constraints);
    let stated = model.inspect(represented_initial_values);
    let unconditional = demotion_pass_with_observer(
        model,
        residue,
        &stated,
        &candidates.admissible,
        prior_manifold,
        DemotionPassPolicy {
            group: CandidateGroup::DirectAdmissible,
            allow_held,
        },
        observer,
    )?;
    if unconditional.step.is_some() {
        return Ok(unconditional);
    }
    let carried = demotion_pass_with_observer(
        model,
        residue,
        &stated,
        &candidates.conditional,
        prior_manifold,
        DemotionPassPolicy {
            group: CandidateGroup::DirectConditional,
            allow_held,
        },
        observer,
    )?;
    Ok(DemotionRound {
        blocked: carried.blocked.or(unconditional.blocked),
        step: carried.step,
    })
}

/// What attempting one direct-state candidate against `model` found. Every
/// variant's `Attempt` (and, for `Sorted`, `Selected`) event is already
/// recorded by the time [`attempt_direct_candidate`] returns it, so the
/// caller only has to act on the outcome.
enum DirectAttempt {
    /// The rebuilt system matched completely.
    Sorted {
        rebuilt: dae::Dae,
        manifold: Vec<ManifoldConstraint>,
        structural: PreparedStructuralAnalysis,
    },
    /// A candidate this round would take, if nothing else outranks it.
    Accepted {
        candidate: DirectStateConstraint,
        residue: usize,
        step: DemotionStep,
    },
    /// A candidate this round would take, but it discards a stated initial value.
    Blocked(DiscardedInitialValue),
    /// A candidate this round never takes.
    Rejected,
}

fn discarded_initial_after_attempt(
    model: &dae::Dae,
    rebuilt: &dae::Dae,
    stated: &[u32],
    lane: Lane,
    identity: Identity<'_>,
    observer: &mut impl ReductionObserver,
) -> Result<Option<DiscardedInitialValue>, StructuralError> {
    match model.inspect(|source| {
        rebuilt.inspect(|view| discarded_stated_initial_value(source, view, stated))
    }) {
        Ok(discarded) => Ok(discarded),
        Err(error) => {
            observer.observe(ReductionEvent::Attempt {
                lane,
                identity,
                outcome: AttemptOutcome::NonSingularFailure { error: &error },
            });
            Err(error)
        }
    }
}

/// Try one candidate, observing its identity and outcome. Every decision this
/// makes is exactly the one the pre-observation code made at this same branch
/// point; `observer.observe` calls are interleaved without moving, adding, or
/// removing any of them.
fn attempt_direct_candidate(
    model: &dae::Dae,
    residue: usize,
    stated: &[u32],
    candidate: &DirectStateConstraint,
    prior_manifold: &[ManifoldConstraint],
    observer: &mut impl ReductionObserver,
) -> Result<DirectAttempt, StructuralError> {
    let identity = Identity::Direct(DirectIdentity::from(candidate));
    let (rebuilt, manifold) =
        match rebuild_with_state_demotion_and_manifold(model, *candidate, prior_manifold) {
            Ok(rebuilt) => rebuilt,
            Err(error) => {
                observer.observe(ReductionEvent::Attempt {
                    lane: Lane::Direct,
                    identity,
                    outcome: AttemptOutcome::NonSingularFailure { error: &error },
                });
                return Err(error);
            }
        };
    if !manifold.is_empty() && !manifold_is_state_only(&rebuilt, &manifold) {
        observer.observe(ReductionEvent::Attempt {
            lane: Lane::Direct,
            identity,
            outcome: AttemptOutcome::WouldInvalidateManifold,
        });
        return Ok(DirectAttempt::Rejected);
    }
    let (next, retained_error, structural) = match structural_analysis(&rebuilt) {
        Ok(structural) => (None, None, Some(structural)),
        Err(error) => match unmatched_residue(&error) {
            Some(next) if next <= residue => (Some(next), Some(error), None),
            Some(next) => {
                observer.observe(ReductionEvent::Attempt {
                    lane: Lane::Direct,
                    identity,
                    outcome: AttemptOutcome::Raised { residue: next },
                });
                return Ok(DirectAttempt::Rejected);
            }
            None => {
                observer.observe(ReductionEvent::Attempt {
                    lane: Lane::Direct,
                    identity,
                    outcome: AttemptOutcome::NonSingularFailure { error: &error },
                });
                return Ok(DirectAttempt::Rejected);
            }
        },
    };
    let discarded =
        discarded_initial_after_attempt(model, &rebuilt, stated, Lane::Direct, identity, observer)?;
    if let Some(discarded) = discarded {
        observer.observe(ReductionEvent::Attempt {
            lane: Lane::Direct,
            identity,
            outcome: AttemptOutcome::WouldDiscardInitial {
                variable: &discarded.variable,
                span: discarded.span,
            },
        });
        return Ok(DirectAttempt::Blocked(discarded));
    }
    let Some(next) = next else {
        observer.observe(ReductionEvent::Attempt {
            lane: Lane::Direct,
            identity,
            outcome: AttemptOutcome::Sorted,
        });
        observer.observe(ReductionEvent::Selected {
            lane: Lane::Direct,
            identity,
            residue_before: residue,
            residue_after: None,
        });
        return Ok(DirectAttempt::Sorted {
            rebuilt,
            manifold,
            structural: structural.expect("a sorted direct attempt retains its analysis"),
        });
    };
    observer.observe(ReductionEvent::Attempt {
        lane: Lane::Direct,
        identity,
        outcome: if next < residue {
            AttemptOutcome::Reduced { residue: next }
        } else {
            AttemptOutcome::Held { residue: next }
        },
    });
    Ok(DirectAttempt::Accepted {
        candidate: *candidate,
        residue: next,
        step: DemotionStep::Reduced {
            dae: rebuilt,
            manifold,
            residue: next,
            error: retained_error.expect("reduced/held candidate retains its proving error"),
        },
    })
}

/// Try one list of demotion candidates against `model`.
fn demotion_pass_with_observer(
    model: &dae::Dae,
    residue: usize,
    stated: &[u32],
    candidates: &[DirectStateConstraint],
    prior_manifold: &[ManifoldConstraint],
    policy: DemotionPassPolicy,
    observer: &mut impl ReductionObserver,
) -> Result<DemotionRound, StructuralError> {
    observer.observe(ReductionEvent::Candidates {
        lane: Lane::Direct,
        group: policy.group,
        discovered: candidates.len(),
    });
    let mut reduced: Option<(DirectStateConstraint, usize, DemotionStep)> = None;
    let mut held: Option<(DirectStateConstraint, usize, DemotionStep)> = None;
    let mut blocked = None;
    for candidate in candidates {
        match attempt_direct_candidate(model, residue, stated, candidate, prior_manifold, observer)?
        {
            DirectAttempt::Sorted {
                rebuilt,
                manifold,
                structural,
            } => {
                return Ok(DemotionRound {
                    step: Some(DemotionStep::Sorted {
                        dae: rebuilt,
                        manifold,
                        structural,
                    }),
                    blocked: None,
                });
            }
            DirectAttempt::Accepted {
                candidate,
                residue: next,
                step,
            } => {
                let slot = if next < residue {
                    &mut reduced
                } else {
                    &mut held
                };
                if next < residue {
                    slot.get_or_insert((candidate, next, step));
                } else {
                    *slot = Some((candidate, next, step));
                }
            }
            DirectAttempt::Blocked(discarded) => {
                blocked.get_or_insert(discarded);
            }
            DirectAttempt::Rejected => {}
        }
    }
    match reduced.or(if policy.allow_held { held } else { None }) {
        Some((candidate, residue_after, step)) => {
            observer.observe(ReductionEvent::Selected {
                lane: Lane::Direct,
                identity: Identity::Direct(DirectIdentity::from(&candidate)),
                residue_before: residue,
                residue_after: Some(residue_after),
            });
            Ok(DemotionRound {
                step: Some(step),
                blocked: None,
            })
        }
        None => Ok(DemotionRound {
            step: None,
            blocked,
        }),
    }
}

/// What one holonomic reduction found.
struct HolonomicRound {
    /// The replacement DAE and its manifold expressions, if one matched.
    step: Option<(
        dae::Dae,
        Vec<ManifoldConstraint>,
        PreparedStructuralAnalysis,
    )>,
    /// A stated initial value the matching reductions would have discarded.
    blocked: Option<DiscardedInitialValue>,
}

/// One accepted holonomic replacement. A singular intermediate remains
/// private to this phase and is carried only to the next proved round.
/// `Reduced` retains the exact [`StructuralError::Singular`] its residue was
/// read from, for the same reason [`DemotionStep::Reduced`] does: the next
/// round's observed [`ReductionEvent::Round`] borrows a proof already made
/// rather than recomputing one.
enum HolonomicStep {
    Sorted {
        dae: dae::Dae,
        manifold: Vec<ManifoldConstraint>,
        structural: PreparedStructuralAnalysis,
    },
    Reduced {
        dae: dae::Dae,
        manifold: Vec<ManifoldConstraint>,
        residue: usize,
        error: StructuralError,
    },
}

/// What one fixed-point round found against the current finalized DAE.
struct HolonomicPass {
    step: Option<HolonomicStep>,
    held: Option<(HolonomicConstraint, usize, HolonomicStep)>,
    blocked: Option<DiscardedInitialValue>,
}

struct HolonomicReductionState {
    reduced: Option<dae::Dae>,
    manifold: Vec<ManifoldConstraint>,
    residue: usize,
    current_error: StructuralError,
    round_number: u32,
    direct_round_number: u32,
    differentiated_owners: BTreeSet<(usize, Option<usize>, Option<u32>)>,
    blocked: Option<DiscardedInitialValue>,
}

impl HolonomicReductionState {
    fn new(residue: usize, current_error: StructuralError) -> Self {
        Self {
            reduced: None,
            manifold: Vec::new(),
            residue,
            current_error,
            round_number: 0,
            direct_round_number: 0,
            differentiated_owners: BTreeSet::new(),
            blocked: None,
        }
    }

    fn current<'a>(&'a self, original: &'a dae::Dae) -> &'a dae::Dae {
        self.reduced.as_ref().unwrap_or(original)
    }

    fn accept_reduced(&mut self, step: HolonomicStep) {
        let HolonomicStep::Reduced {
            dae,
            manifold,
            residue,
            error,
        } = step
        else {
            unreachable!("a sorted step completes the reduction immediately")
        };
        self.reduced = Some(dae);
        self.manifold = manifold;
        self.residue = residue;
        self.current_error = error;
    }

    fn accept_held(
        &mut self,
        held: (HolonomicConstraint, usize, HolonomicStep),
        observer: &mut impl ReductionObserver,
    ) {
        let (constraint, next_residue, step) = held;
        self.differentiated_owners.insert((
            constraint.owner_ordinal,
            constraint.body_ordinal,
            constraint.lifted_algebraic,
        ));
        observer.observe(ReductionEvent::Selected {
            lane: Lane::Holonomic,
            identity: Identity::Holonomic(HolonomicIdentity::from(&constraint)),
            residue_before: self.residue,
            residue_after: Some(next_residue),
        });
        let HolonomicStep::Reduced { residue, .. } = &step else {
            unreachable!("a held holonomic step remains singular")
        };
        debug_assert_eq!(*residue, next_residue);
        self.accept_reduced(step);
    }

    fn direct_or_held(
        &mut self,
        original: &dae::Dae,
        held: Option<(HolonomicConstraint, usize, HolonomicStep)>,
        observer: &mut impl ReductionObserver,
    ) -> Result<Option<HolonomicRound>, StructuralError> {
        self.direct_round_number += 1;
        observer.observe(ReductionEvent::Round {
            lane: Lane::Direct,
            round: self.direct_round_number,
            error: &self.current_error,
        });
        let round = demote_direct_state_with_observer(
            self.current(original),
            self.residue,
            &self.manifold,
            held.is_none(),
            observer,
        )?;
        self.blocked = self.blocked.take().or(round.blocked);
        match round.step {
            Some(DemotionStep::Sorted {
                dae,
                manifold,
                structural,
            }) => sorted_holonomic_round(dae, manifold, structural).map(Some),
            Some(step @ DemotionStep::Reduced { .. }) => {
                self.accept_reduced(step.into());
                Ok(None)
            }
            None => match held {
                Some(held) => {
                    self.accept_held(held, observer);
                    Ok(None)
                }
                None => Ok(Some(HolonomicRound {
                    step: None,
                    blocked: self.blocked.take(),
                })),
            },
        }
    }
}

impl From<DemotionStep> for HolonomicStep {
    fn from(step: DemotionStep) -> Self {
        match step {
            DemotionStep::Reduced {
                dae,
                manifold,
                residue,
                error,
            } => Self::Reduced {
                dae,
                manifold,
                residue,
                error,
            },
            DemotionStep::Sorted { .. } => {
                unreachable!("a sorted demotion completes before conversion")
            }
        }
    }
}

fn sorted_holonomic_round(
    dae: dae::Dae,
    manifold: Vec<ManifoldConstraint>,
    structural: PreparedStructuralAnalysis,
) -> Result<HolonomicRound, StructuralError> {
    let (dae, manifold, structural) = select_dummy_states(dae, manifold, structural)?;
    Ok(HolonomicRound {
        step: Some((dae, manifold, structural)),
        blocked: None,
    })
}

/// Accumulate proved holonomic replacements until the differentiated system
/// matches, reporting only that final replacement and its manifold.
///
/// Candidates are recollected from the current finalized DAE every round, so a
/// differentiability certificate that an earlier replacement invalidated can
/// never be replayed stale. Owner ordinals impose the deterministic choice
/// order. A replacement may hold the unmatched residue only while it consumes
/// one exact equation owner/body or one frontier algebraic; the reducer records
/// consumed owners so it can never differentiate the same semantic equation
/// twice. The lexicographic tuple of residue, remaining equation owners, and
/// remaining algebraics is therefore the terminating measure for this lane.
///
/// Every accepted edge is held to the same MLS 3.6 §8.6 postcondition as a
/// state demotion. `rebuild_holonomic_constraint` replaces the source residual
/// with its second derivative, so the equality that carried a stated value onto
/// another coordinate can leave the system with it. The values stated by the
/// original system survive the full chain by induction over these per-round
/// comparisons. If the chain stalls, its singular intermediates and partial
/// manifold are discarded and the caller reports the original typed error.
#[cfg(test)]
fn reduce_holonomic_constraint(model: &dae::Dae) -> Result<HolonomicRound, StructuralError> {
    reduce_holonomic_constraint_with_enumeration(model, |_| {})
}

/// Testable enumeration seam: production supplies the identity operation,
/// while the adversary reverses discovery before the mandatory owner sort.
#[cfg(test)]
fn reduce_holonomic_constraint_with_enumeration(
    model: &dae::Dae,
    perturb_enumeration: impl FnMut(&mut Vec<HolonomicConstraint>),
) -> Result<HolonomicRound, StructuralError> {
    reduce_holonomic_constraint_with_observer(model, perturb_enumeration, &mut ())
}

/// The observed core [`reduce_holonomic_constraint_with_enumeration`] and
/// `prepare_for_solve_with_observer` both delegate to.
fn reduce_holonomic_constraint_with_observer(
    model: &dae::Dae,
    mut perturb_enumeration: impl FnMut(&mut Vec<HolonomicConstraint>),
    observer: &mut impl ReductionObserver,
) -> Result<HolonomicRound, StructuralError> {
    let outcome = structural_analysis(model);
    let (residue, current_error) = match outcome {
        Ok(_) => {
            return Ok(HolonomicRound {
                step: None,
                blocked: None,
            });
        }
        Err(error) => match unmatched_residue(&error) {
            Some(residue) => (residue, error),
            None => {
                return Ok(HolonomicRound {
                    step: None,
                    blocked: None,
                });
            }
        },
    };
    let mut state = HolonomicReductionState::new(residue, current_error);
    loop {
        state.round_number += 1;
        observer.observe(ReductionEvent::Round {
            lane: Lane::Holonomic,
            round: state.round_number,
            error: &state.current_error,
        });
        let current = state.reduced.as_ref().unwrap_or(model);
        let pass = holonomic_pass_with_observer(
            current,
            state.residue,
            &state.manifold,
            &mut state.differentiated_owners,
            &mut perturb_enumeration,
            observer,
        )?;
        state.blocked = state.blocked.take().or(pass.blocked);
        match pass.step {
            Some(HolonomicStep::Sorted {
                dae,
                manifold,
                structural,
            }) => {
                return sorted_holonomic_round(dae, manifold, structural);
            }
            Some(step @ HolonomicStep::Reduced { .. }) => state.accept_reduced(step),
            None => {
                if let Some(round) = state.direct_or_held(model, pass.held, observer)? {
                    return Ok(round);
                }
            }
        }
    }
}

/// Remove every proved dummy state whose demotion preserves both a perfect
/// matching and the state-only manifold postcondition. A temporary Pantelides
/// state is correlated with the retained equality that justified its
/// promotion, so demoting it removes exactly that entry during reconstruction.
fn select_dummy_states(
    mut model: dae::Dae,
    mut manifold: Vec<ManifoldConstraint>,
    mut structural: PreparedStructuralAnalysis,
) -> Result<
    (
        dae::Dae,
        Vec<ManifoldConstraint>,
        PreparedStructuralAnalysis,
    ),
    StructuralError,
> {
    loop {
        let round = demote_direct_state_with_observer(&model, 0, &manifold, false, &mut ())?;
        match round.step {
            Some(DemotionStep::Sorted {
                dae,
                manifold: next,
                structural: next_structural,
            }) => {
                model = dae;
                manifold = next;
                structural = next_structural;
            }
            Some(DemotionStep::Reduced { .. }) => {
                unreachable!("a singular DAE cannot have zero unmatched residue")
            }
            None => return Ok((model, manifold, structural)),
        }
    }
}

fn manifold_is_state_only(model: &dae::Dae, manifold: &[ManifoldConstraint]) -> bool {
    model.inspect(|view| {
        let mut cache = rumoca_eval_dae::ScalarCoordinateProjectionCache::default();
        manifold
            .iter()
            .all(|entry| manifold_entry_is_state_only(view, entry, &mut cache))
    })
}

fn manifold_entry_is_state_only<'dae>(
    view: dae::DaeView<'dae>,
    entry: &ManifoldConstraint,
    cache: &mut rumoca_eval_dae::ScalarCoordinateProjectionCache<'dae>,
) -> bool {
    let expression = view
        .expression_id(entry.expression as usize)
        .expect("retained manifold expression resolves");
    let scalar_count = view
        .expression(expression)
        .expect("retained manifold expression resolves")
        .value_type()
        .scalar_count()
        .expect("final DAE manifold type has a scalar capacity");
    (0..scalar_count).all(|scalar| manifold_scalar_is_state_only(view, expression, scalar, cache))
}

fn manifold_scalar_is_state_only<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    scalar: usize,
    cache: &mut rumoca_eval_dae::ScalarCoordinateProjectionCache<'dae>,
) -> bool {
    let mut saw_state = false;
    let mut valid = true;
    let projected = rumoca_eval_dae::for_each_scalar_coordinate_cached(
        view,
        expression,
        scalar,
        None,
        cache,
        |coordinate, _| match coordinate {
            dae::CoordinateView::State(_) => saw_state = true,
            dae::CoordinateView::Algebraic(_) | dae::CoordinateView::Derivative(_) => {
                valid = false;
            }
            _ => {}
        },
    );
    projected.is_ok() && valid && saw_state
}

/// What attempting one holonomic candidate against `model` found, mirroring
/// [`DirectAttempt`]: every recorded event is already emitted by the time
/// this returns.
enum HolonomicAttempt {
    Sorted {
        dae: Box<dae::Dae>,
        manifold: Vec<ManifoldConstraint>,
        structural: PreparedStructuralAnalysis,
    },
    Accepted {
        constraint: HolonomicConstraint,
        residue: usize,
        step: Box<HolonomicStep>,
    },
    Blocked(DiscardedInitialValue),
    Rejected,
}

fn refused_holonomic_outcome(next: usize, residue: usize) -> AttemptOutcome<'static> {
    debug_assert!(next >= residue);
    if next == residue {
        AttemptOutcome::Held { residue: next }
    } else {
        AttemptOutcome::Raised { residue: next }
    }
}

fn residual_scalar_is_structurally_active<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    scalar: usize,
    domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
    cache: &mut rumoca_eval_dae::ScalarCoordinateProjectionCache<'dae>,
) -> bool {
    let mut active = false;
    let projected = rumoca_eval_dae::for_each_scalar_coordinate_cached(
        view,
        expression,
        scalar,
        domain_point,
        cache,
        |coordinate, _| {
            active |= matches!(
                coordinate,
                dae::CoordinateView::Derivative(_) | dae::CoordinateView::Algebraic(_)
            );
        },
    );
    projected.is_ok() && active
}

/// Whether the exact replacement for one holonomic owner still contributes a
/// continuous unknown in at least one scalar equation it owns.
///
/// Differentiation can prove an expression admissible and nevertheless reduce
/// it to an exact shaped zero once causal definitions are substituted. Such a
/// residual is not a valid Pantelides replacement: retaining it would add an
/// equation row that can never match an unknown. Check the rebuilt owner, not
/// the source syntax, so this postcondition covers scalar residuals and compact
/// structured families through the same scalar projection used by incidence.
fn holonomic_replacement_is_structurally_active(
    model: &dae::Dae,
    constraint: &HolonomicConstraint,
) -> bool {
    model.inspect(|view| {
        let Some(owner) = view.continuous_owners().nth(constraint.owner_ordinal) else {
            return false;
        };
        let mut cache = rumoca_eval_dae::ScalarCoordinateProjectionCache::default();
        match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                let residual = equation.residual();
                let Some(scalar_count) = view
                    .expression(residual)
                    .and_then(|expression| expression.value_type().scalar_count())
                else {
                    return false;
                };
                (0..scalar_count).any(|scalar| {
                    residual_scalar_is_structurally_active(view, residual, scalar, None, &mut cache)
                })
            }
            dae::ContinuousOwnerView::Structured { family, .. } => {
                structured_replacement_is_active(view, family, constraint, &mut cache)
            }
        }
    })
}

fn structured_replacement_is_active<'dae>(
    view: dae::DaeView<'dae>,
    family: dae::StructuredFamilyView<'dae>,
    constraint: &HolonomicConstraint,
    cache: &mut rumoca_eval_dae::ScalarCoordinateProjectionCache<'dae>,
) -> bool {
    let Some(body_ordinal) = constraint.body_ordinal else {
        return false;
    };
    let Some(residual) = family.bodies().get(body_ordinal) else {
        return false;
    };
    let Some(domain) = view.domain(family.domain()) else {
        return false;
    };
    let structured = domain.structured();
    (0..domain.scalar_count() as usize).any(|point| {
        let Ok(Some(values)) = structured.index_tuple_at(point) else {
            return false;
        };
        let Some(scalar) = family.scalar_view().body_scalar(point, domain.extents()) else {
            return false;
        };
        residual_scalar_is_structurally_active(
            view,
            residual,
            scalar,
            Some((family.domain(), values.as_slice())),
            cache,
        )
    })
}

/// Try one certificate, observing its identity and outcome. Every decision
/// this makes is exactly the one the pre-observation code made at this same
/// branch point.
fn observe_discarded_holonomic_initial(
    observer: &mut impl ReductionObserver,
    identity: Identity,
    discarded: &DiscardedInitialValue,
) {
    observer.observe(ReductionEvent::Attempt {
        lane: Lane::Holonomic,
        identity,
        outcome: AttemptOutcome::WouldDiscardInitial {
            variable: &discarded.variable,
            span: discarded.span,
        },
    });
}

fn attempt_holonomic_candidate(
    model: &dae::Dae,
    residue: usize,
    prior_manifold: &[ManifoldConstraint],
    stated: &[u32],
    constraint: HolonomicConstraint,
    observer: &mut impl ReductionObserver,
) -> Result<HolonomicAttempt, StructuralError> {
    let identity = Identity::Holonomic(HolonomicIdentity::from(&constraint));
    if let Some(discarded) = stated_initial_on_holonomic_manifold(model, stated, &constraint) {
        observe_discarded_holonomic_initial(observer, identity, &discarded);
        return Ok(HolonomicAttempt::Blocked(discarded));
    }
    let (rebuilt, manifold) = match rebuild_holonomic_constraint(model, &constraint, prior_manifold)
    {
        Ok(pair) => pair,
        Err(error) => {
            observer.observe(ReductionEvent::Attempt {
                lane: Lane::Holonomic,
                identity,
                outcome: AttemptOutcome::NonSingularFailure { error: &error },
            });
            return Err(error);
        }
    };
    if !holonomic_replacement_is_structurally_active(&rebuilt, &constraint) {
        observer.observe(ReductionEvent::Attempt {
            lane: Lane::Holonomic,
            identity,
            outcome: AttemptOutcome::WouldCreateVacuousResidual,
        });
        return Ok(HolonomicAttempt::Rejected);
    }
    let (next, retained_error, structural) = match structural_analysis(&rebuilt) {
        Ok(structural) => (None, None, Some(structural)),
        Err(error) => match unmatched_residue(&error) {
            Some(next) if next <= residue => (Some(next), Some(error), None),
            Some(next) => {
                observer.observe(ReductionEvent::Attempt {
                    lane: Lane::Holonomic,
                    identity,
                    outcome: refused_holonomic_outcome(next, residue),
                });
                return Ok(HolonomicAttempt::Rejected);
            }
            None => {
                observer.observe(ReductionEvent::Attempt {
                    lane: Lane::Holonomic,
                    identity,
                    outcome: AttemptOutcome::NonSingularFailure { error: &error },
                });
                return Ok(HolonomicAttempt::Rejected);
            }
        },
    };
    let discarded = discarded_initial_after_attempt(
        model,
        &rebuilt,
        stated,
        Lane::Holonomic,
        identity,
        observer,
    )?;
    if let Some(discarded) = discarded {
        observe_discarded_holonomic_initial(observer, identity, &discarded);
        return Ok(HolonomicAttempt::Blocked(discarded));
    }
    let Some(next) = next else {
        observer.observe(ReductionEvent::Attempt {
            lane: Lane::Holonomic,
            identity,
            outcome: AttemptOutcome::Sorted,
        });
        observer.observe(ReductionEvent::Selected {
            lane: Lane::Holonomic,
            identity,
            residue_before: residue,
            residue_after: None,
        });
        return Ok(HolonomicAttempt::Sorted {
            dae: Box::new(rebuilt),
            manifold,
            structural: structural.expect("a sorted holonomic attempt retains its analysis"),
        });
    };
    observer.observe(ReductionEvent::Attempt {
        lane: Lane::Holonomic,
        identity,
        outcome: if next < residue {
            AttemptOutcome::Reduced { residue: next }
        } else {
            AttemptOutcome::Held { residue: next }
        },
    });
    Ok(HolonomicAttempt::Accepted {
        constraint,
        residue: next,
        step: Box::new(HolonomicStep::Reduced {
            dae: rebuilt,
            manifold,
            residue: next,
            error: retained_error.expect("reduced candidate retains its proving error"),
        }),
    })
}

/// A stated state value that the current manifold projection certificate does
/// not prove immutable.
///
/// MLS 3.6 §8.6 makes a `fixed = true` start an initialization equation. The
/// retained manifold is projected after that initialization system settles,
/// and today's projection certificate permits every state named by the
/// manifold to move. Retaining the pin on the rewritten DAE is therefore not
/// sufficient: projection could silently replace the stated value. Until the
/// IR carries a separate initial-manifold plan that excludes initialized state
/// owners, fail closed whenever this holonomic edge anchors a stated state.
fn stated_initial_on_holonomic_manifold(
    model: &dae::Dae,
    stated: &[u32],
    constraint: &HolonomicConstraint,
) -> Option<DiscardedInitialValue> {
    let variable = stated.iter().copied().find(|variable| {
        constraint
            .proof
            .anchored_states
            .binary_search(variable)
            .is_ok()
    })?;
    model.inspect(|view| {
        let declaration = view.variable(view.variable_id(variable as usize)?)?;
        Some(DiscardedInitialValue {
            variable: declaration.name().as_str().to_string(),
            span: declaration.declaration().span(),
        })
    })
}

/// Try every current certificate in deterministic owner order, preferring a
/// strict residue reduction over a once-only owner-consuming held step.
fn holonomic_pass_with_observer(
    model: &dae::Dae,
    residue: usize,
    prior_manifold: &[ManifoldConstraint],
    differentiated_owners: &mut BTreeSet<(usize, Option<usize>, Option<u32>)>,
    perturb_enumeration: &mut impl FnMut(&mut Vec<HolonomicConstraint>),
    observer: &mut impl ReductionObserver,
) -> Result<HolonomicPass, StructuralError> {
    let stated = model.inspect(represented_initial_values);
    let (mut candidates, incident) = model.inspect(|view| {
        let candidates = index_reduction_constraints(view);
        let incident = crate::overdetermined_block_variables(view)?;
        Ok::<_, StructuralError>((candidates, incident))
    })?;
    perturb_enumeration(&mut candidates);
    candidates.retain(|candidate| {
        !differentiated_owners.contains(&(
            candidate.owner_ordinal,
            candidate.body_ordinal,
            candidate.lifted_algebraic,
        )) && candidate
            .lifted_algebraic
            .is_none_or(|variable| incident.contains(&variable))
    });
    candidates.sort_by_key(|candidate| {
        (
            usize::from(candidate.lifted_algebraic.is_some()),
            candidate.owner_ordinal,
            candidate.body_ordinal,
        )
    });
    observer.observe(ReductionEvent::Candidates {
        lane: Lane::Holonomic,
        group: CandidateGroup::Holonomic,
        discovered: candidates.len(),
    });
    let mut reduced: Option<(HolonomicConstraint, usize, HolonomicStep)> = None;
    let mut held: Option<(HolonomicConstraint, usize, HolonomicStep)> = None;
    let mut blocked = None;
    for constraint in candidates {
        let attempt = attempt_holonomic_candidate(
            model,
            residue,
            prior_manifold,
            &stated,
            constraint,
            observer,
        )?;
        match attempt {
            HolonomicAttempt::Sorted {
                dae,
                manifold,
                structural,
            } => {
                return Ok(HolonomicPass {
                    step: Some(HolonomicStep::Sorted {
                        dae: *dae,
                        manifold,
                        structural,
                    }),
                    held: None,
                    blocked: None,
                });
            }
            HolonomicAttempt::Accepted {
                constraint,
                residue: next,
                step,
            } => {
                let slot = if next < residue {
                    &mut reduced
                } else {
                    &mut held
                };
                slot.get_or_insert((constraint, next, *step));
            }
            HolonomicAttempt::Blocked(discarded) => {
                blocked.get_or_insert(discarded);
            }
            HolonomicAttempt::Rejected => {}
        }
    }
    match reduced {
        Some((constraint, residue_after, step)) => {
            differentiated_owners.insert((
                constraint.owner_ordinal,
                constraint.body_ordinal,
                constraint.lifted_algebraic,
            ));
            observer.observe(ReductionEvent::Selected {
                lane: Lane::Holonomic,
                identity: Identity::Holonomic(HolonomicIdentity::from(&constraint)),
                residue_before: residue,
                residue_after: Some(residue_after),
            });
            Ok(HolonomicPass {
                step: Some(step),
                held: None,
                blocked: None,
            })
        }
        None => Ok(HolonomicPass {
            step: None,
            held,
            blocked,
        }),
    }
}

/// Equations and unknowns that a maximum matching leaves unpaired, which is
/// zero exactly when the matching is perfect. Reported only for a singular
/// system; any other structural failure has no comparable residue.
fn unmatched_residue(error: &StructuralError) -> Option<usize> {
    let StructuralError::Singular {
        n_equations,
        n_unknowns,
        n_matched,
        ..
    } = error
    else {
        return None;
    };
    Some((n_equations - n_matched) + (n_unknowns - n_matched))
}
