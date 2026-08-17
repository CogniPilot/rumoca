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

use self::constraints::{
    DiscardedInitialValue, direct_state_constraints, discarded_stated_initial_value,
    holonomic_constraints,
};
use self::initial_pins::{represented_initial_values, transferred_initial_values};
use self::observation::{
    AttemptOutcome, CandidateGroup, DirectIdentity, HolonomicIdentity, Identity, Lane,
    ReductionEvent, ReductionObserver, ReductionRecorder, StoppedOutcome,
};
use self::reconstruction::{rebuild_holonomic_constraint, rebuild_with_state_demotion};
use crate::{StructuralError, sort};

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
    },
    Transformed {
        dae: Box<dae::Dae>,
        manifold: Box<[u32]>,
        pins: Box<[InitialValuePin]>,
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
        let (manifold, pins) = match self {
            Self::Borrowed { pins, .. } => ([].as_slice(), pins),
            Self::Transformed { manifold, pins, .. } => (&**manifold, pins),
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
    residual: u32,
    owner: dae::DaeProvenance,
    proof: HolonomicDifferentiationProof,
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
/// A differentiation chain of index three or more passes through steps that
/// leave the residue unchanged: demoting a rigidly held angle turns
/// `w = der(phi)` into `w = 0`, which only becomes solvable once `w` is demoted
/// in turn. Such a step is accepted as a fallback, after every strictly
/// shrinking candidate has been tried. The accumulation still terminates
/// because each round demotes one more state and never raises the residue, so
/// the pair (residue, remaining states) strictly decreases.
pub fn prepare_for_solve(model: &dae::Dae) -> Result<PreparedDae<'_>, StructuralError> {
    prepare_for_solve_with_observer(model, &mut ())
}

/// The diagnostic inspection surface: the exact same reduction as
/// [`prepare_for_solve`], paired with an owned [`ReductionReport`] of every
/// round this call actually traversed.
///
/// [`ReductionRecorder`] is the only [`ReductionObserver`] this crate builds
/// besides the no-op `()` `prepare_for_solve` uses, so this function and
/// `prepare_for_solve` are the two instantiations of one generic seam,
/// [`prepare_for_solve_with_observer`]. Its observer type is erased from both
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
    let singular = match model.inspect(|view| sort(view).map(|_| ())) {
        Ok(_) => return borrowed_with_observer(model, observer),
        Err(error @ StructuralError::Singular { .. }) => error,
        Err(StructuralError::EmptySystem) => return borrowed_with_observer(model, observer),
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
        let round = match demote_direct_state_with_observer(current_model, residue, observer) {
            Ok(round) => round,
            Err(error) => return observed_failure(error, observer),
        };
        match round.step {
            None => break round.blocked,
            Some(DemotionStep::Sorted(dae)) => {
                return transformed_with_observer(dae, Vec::new(), observer);
            }
            Some(DemotionStep::Reduced {
                dae,
                residue: next,
                error,
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
        (Some((dae, manifold)), _) => transformed_with_observer(dae, manifold, observer),
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
fn borrowed(model: &dae::Dae) -> Result<PreparedDae<'_>, StructuralError> {
    let pins = model.inspect(transferred_initial_values)?;
    Ok(PreparedDae::Borrowed {
        dae: model,
        pins: pins.into_boxed_slice(),
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
    observer: &mut impl ReductionObserver,
) -> Result<PreparedDae<'source>, StructuralError> {
    let result = borrowed(model);
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
) -> Result<PreparedDae<'static>, StructuralError> {
    let pins = model.inspect(transferred_initial_values)?;
    Ok(PreparedDae::Transformed {
        dae: Box::new(model),
        manifold: manifold.into_boxed_slice(),
        pins: pins.into_boxed_slice(),
    })
}

fn transformed_with_observer(
    model: dae::Dae,
    manifold: Vec<u32>,
    observer: &mut impl ReductionObserver,
) -> Result<PreparedDae<'static>, StructuralError> {
    let result = transformed(model, manifold);
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
    Sorted(dae::Dae),
    Reduced {
        dae: dae::Dae,
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
    observer: &mut impl ReductionObserver,
) -> Result<DemotionRound, StructuralError> {
    let candidates = model.inspect(direct_state_constraints);
    let stated = model.inspect(represented_initial_values);
    let unconditional = demotion_pass_with_observer(
        model,
        residue,
        &stated,
        &candidates.admissible,
        CandidateGroup::DirectAdmissible,
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
        CandidateGroup::DirectConditional,
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
    Sorted(dae::Dae),
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
    observer: &mut impl ReductionObserver,
) -> Result<DirectAttempt, StructuralError> {
    let identity = Identity::Direct(DirectIdentity::from(candidate));
    let rebuilt = match rebuild_with_state_demotion(model, *candidate) {
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
    let (next, retained_error) = match rebuilt.inspect(|view| sort(view).map(|_| ())) {
        Ok(()) => (None, None),
        Err(error) => match unmatched_residue(&error) {
            Some(next) if next <= residue => (Some(next), Some(error)),
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
        return Ok(DirectAttempt::Sorted(rebuilt));
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
    group: CandidateGroup,
    observer: &mut impl ReductionObserver,
) -> Result<DemotionRound, StructuralError> {
    observer.observe(ReductionEvent::Candidates {
        lane: Lane::Direct,
        group,
        discovered: candidates.len(),
    });
    let mut reduced: Option<(DirectStateConstraint, usize, DemotionStep)> = None;
    let mut held: Option<(DirectStateConstraint, usize, DemotionStep)> = None;
    let mut blocked = None;
    for candidate in candidates {
        match attempt_direct_candidate(model, residue, stated, candidate, observer)? {
            DirectAttempt::Sorted(rebuilt) => {
                return Ok(DemotionRound {
                    step: Some(DemotionStep::Sorted(rebuilt)),
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
                slot.get_or_insert((candidate, next, step));
            }
            DirectAttempt::Blocked(discarded) => {
                blocked.get_or_insert(discarded);
            }
            DirectAttempt::Rejected => {}
        }
    }
    match reduced.or(held) {
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
    step: Option<(dae::Dae, Vec<u32>)>,
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
        manifold: Vec<u32>,
    },
    Reduced {
        dae: dae::Dae,
        manifold: Vec<u32>,
        residue: usize,
        error: StructuralError,
    },
}

/// What one fixed-point round found against the current finalized DAE.
struct HolonomicPass {
    step: Option<HolonomicStep>,
    blocked: Option<DiscardedInitialValue>,
}

/// Accumulate proved holonomic replacements until the differentiated system
/// matches, reporting only that final replacement and its manifold.
///
/// Candidates are recollected from the current finalized DAE every round, so a
/// differentiability certificate that an earlier replacement invalidated can
/// never be replayed stale. Owner ordinals impose the deterministic choice
/// order. Every accepted singular intermediate strictly decreases the unmatched
/// residue, which is the terminating measure for this lane. A residue-holding
/// replacement fails closed; only the separately proved direct-demotion lane
/// may carry such an intermediate.
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
/// [`prepare_for_solve_with_observer`] both delegate to.
fn reduce_holonomic_constraint_with_observer(
    model: &dae::Dae,
    mut perturb_enumeration: impl FnMut(&mut Vec<HolonomicConstraint>),
    observer: &mut impl ReductionObserver,
) -> Result<HolonomicRound, StructuralError> {
    let outcome = model.inspect(|view| sort(view).map(|_| ()));
    let (mut residue, mut current_error) = match outcome {
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
    let mut reduced: Option<dae::Dae> = None;
    let mut manifold = Vec::new();
    let mut round_number: u32 = 0;
    loop {
        round_number += 1;
        observer.observe(ReductionEvent::Round {
            lane: Lane::Holonomic,
            round: round_number,
            error: &current_error,
        });
        let current = reduced.as_ref().unwrap_or(model);
        let pass = holonomic_pass_with_observer(
            current,
            residue,
            &manifold,
            &mut perturb_enumeration,
            observer,
        )?;
        match pass.step {
            Some(HolonomicStep::Sorted {
                dae,
                manifold: complete,
            }) => {
                return Ok(HolonomicRound {
                    step: Some((dae, complete)),
                    blocked: None,
                });
            }
            Some(HolonomicStep::Reduced {
                dae,
                manifold: next_manifold,
                residue: next_residue,
                error,
            }) => {
                reduced = Some(dae);
                manifold = next_manifold;
                residue = next_residue;
                current_error = error;
            }
            None => {
                return Ok(HolonomicRound {
                    step: None,
                    blocked: pass.blocked,
                });
            }
        }
    }
}

/// What attempting one holonomic candidate against `model` found, mirroring
/// [`DirectAttempt`]: every recorded event is already emitted by the time
/// this returns.
enum HolonomicAttempt {
    Sorted {
        dae: dae::Dae,
        manifold: Vec<u32>,
    },
    Accepted {
        constraint: HolonomicConstraint,
        residue: usize,
        step: HolonomicStep,
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

/// Try one certificate, observing its identity and outcome. Every decision
/// this makes is exactly the one the pre-observation code made at this same
/// branch point.
fn attempt_holonomic_candidate(
    model: &dae::Dae,
    residue: usize,
    prior_manifold: &[u32],
    stated: &[u32],
    constraint: HolonomicConstraint,
    observer: &mut impl ReductionObserver,
) -> Result<HolonomicAttempt, StructuralError> {
    let identity = Identity::Holonomic(HolonomicIdentity::from(&constraint));
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
    let (next, retained_error) = match rebuilt.inspect(|view| sort(view).map(|_| ())) {
        Ok(()) => (None, None),
        Err(error) => match unmatched_residue(&error) {
            Some(next) if next < residue => (Some(next), Some(error)),
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
        observer.observe(ReductionEvent::Attempt {
            lane: Lane::Holonomic,
            identity,
            outcome: AttemptOutcome::WouldDiscardInitial {
                variable: &discarded.variable,
                span: discarded.span,
            },
        });
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
            dae: rebuilt,
            manifold,
        });
    };
    observer.observe(ReductionEvent::Attempt {
        lane: Lane::Holonomic,
        identity,
        outcome: AttemptOutcome::Reduced { residue: next },
    });
    Ok(HolonomicAttempt::Accepted {
        constraint,
        residue: next,
        step: HolonomicStep::Reduced {
            dae: rebuilt,
            manifold,
            residue: next,
            error: retained_error.expect("reduced candidate retains its proving error"),
        },
    })
}

/// Try every current certificate in deterministic owner order and accept only
/// the first replacement that strictly reduces the unmatched residue.
fn holonomic_pass_with_observer(
    model: &dae::Dae,
    residue: usize,
    prior_manifold: &[u32],
    perturb_enumeration: &mut impl FnMut(&mut Vec<HolonomicConstraint>),
    observer: &mut impl ReductionObserver,
) -> Result<HolonomicPass, StructuralError> {
    let stated = model.inspect(represented_initial_values);
    let mut candidates = model.inspect(holonomic_constraints);
    perturb_enumeration(&mut candidates);
    candidates.sort_by_key(|candidate| candidate.owner_ordinal);
    observer.observe(ReductionEvent::Candidates {
        lane: Lane::Holonomic,
        group: CandidateGroup::Holonomic,
        discovered: candidates.len(),
    });
    let mut reduced: Option<(HolonomicConstraint, usize, HolonomicStep)> = None;
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
            HolonomicAttempt::Sorted { dae, manifold } => {
                return Ok(HolonomicPass {
                    step: Some(HolonomicStep::Sorted { dae, manifold }),
                    blocked: None,
                });
            }
            HolonomicAttempt::Accepted {
                constraint,
                residue: next,
                step,
            } => {
                reduced.get_or_insert((constraint, next, step));
            }
            HolonomicAttempt::Blocked(discarded) => {
                blocked.get_or_insert(discarded);
            }
            HolonomicAttempt::Rejected => {}
        }
    }
    match reduced {
        Some((constraint, residue_after, step)) => {
            observer.observe(ReductionEvent::Selected {
                lane: Lane::Holonomic,
                identity: Identity::Holonomic(HolonomicIdentity::from(&constraint)),
                residue_before: residue,
                residue_after: Some(residue_after),
            });
            Ok(HolonomicPass {
                step: Some(step),
                blocked: None,
            })
        }
        None => Ok(HolonomicPass {
            step: None,
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
