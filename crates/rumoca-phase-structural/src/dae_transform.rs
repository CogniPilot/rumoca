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
mod reconstruction;
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
use self::reconstruction::{rebuild_holonomic_constraint, rebuild_with_state_demotion};
use crate::{StructuralError, sort};

pub use self::initial_pins::{InitialValuePin, InitialValueRole, PinTerm};

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
    let singular = match model.inspect(|view| sort(view).map(|_| ())) {
        Ok(_) => return borrowed(model),
        Err(error @ StructuralError::Singular { .. }) => error,
        Err(StructuralError::EmptySystem) => return borrowed(model),
        Err(error) => return Err(error),
    };
    let mut residue =
        unmatched_residue(&singular).expect("singular system reports its unmatched residue");
    let mut demoted: Option<dae::Dae> = None;
    let blocked = loop {
        let round = demote_direct_state(demoted.as_ref().unwrap_or(model), residue)?;
        match round.step {
            None => break round.blocked,
            Some(DemotionStep::Sorted(dae)) => return transformed(dae, Vec::new()),
            Some(DemotionStep::Reduced { dae, residue: next }) => {
                residue = next;
                demoted = Some(dae);
            }
        }
    };
    let mut holonomic = reduce_holonomic_constraint(demoted.as_ref().unwrap_or(model))?;
    if holonomic.step.is_none() && demoted.is_some() {
        let pristine = reduce_holonomic_constraint(model)?;
        holonomic = HolonomicRound {
            step: pristine.step,
            blocked: pristine.blocked.or(holonomic.blocked),
        };
    }
    match (holonomic.step, blocked.or(holonomic.blocked)) {
        (Some((dae, manifold)), _) => transformed(dae, manifold),
        // The only reduction left was one that would have discarded a stated
        // initial condition. Report that, not the singularity it hides behind:
        // a bare `ES010` would send a modeller looking for a missing equation.
        (None, Some(blocked)) => Err(StructuralError::DroppedStatedInitialValue {
            variable: blocked.variable,
            span: blocked.span,
        }),
        (None, None) => Err(singular),
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

/// One accepted state demotion: either a fully matched replacement or a
/// non-increasing residue that the next round keeps working on.
enum DemotionStep {
    Sorted(dae::Dae),
    Reduced { dae: dae::Dae, residue: usize },
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
fn demote_direct_state(model: &dae::Dae, residue: usize) -> Result<DemotionRound, StructuralError> {
    let candidates = model.inspect(direct_state_constraints);
    let stated = model.inspect(represented_initial_values);
    let unconditional = demotion_pass(model, residue, &stated, &candidates.admissible)?;
    if unconditional.step.is_some() {
        return Ok(unconditional);
    }
    let carried = demotion_pass(model, residue, &stated, &candidates.conditional)?;
    Ok(DemotionRound {
        blocked: carried.blocked.or(unconditional.blocked),
        step: carried.step,
    })
}

/// Try one list of demotion candidates against `model`.
fn demotion_pass(
    model: &dae::Dae,
    residue: usize,
    stated: &[u32],
    candidates: &[DirectStateConstraint],
) -> Result<DemotionRound, StructuralError> {
    let mut reduced = None;
    let mut held: Option<DemotionStep> = None;
    let mut blocked = None;
    for candidate in candidates {
        let rebuilt = rebuild_with_state_demotion(model, *candidate)?;
        let next = match rebuilt.inspect(|view| sort(view).map(|_| ())) {
            Ok(()) => None,
            Err(error) => match unmatched_residue(&error) {
                Some(next) if next <= residue => Some(next),
                // A candidate that raises the residue, or fails for a reason
                // that has no residue at all, is never taken — so what it would
                // have cost is not worth computing, and a refusal it would have
                // reported would blame an initial condition this system does not
                // depend on.
                _ => continue,
            },
        };
        if let Some(discarded) = model.inspect(|source| {
            rebuilt.inspect(|view| discarded_stated_initial_value(source, view, stated))
        })? {
            // Every candidate that reaches here is one this round would have
            // taken — a fallback that merely holds the residue is still a step
            // the accumulation relies on to reach the next demotion. Refusing it
            // is therefore always the reason the system stops, so it is always
            // recorded: a bare singularity in its place would send a modeller
            // looking for a missing equation.
            blocked.get_or_insert(discarded);
            continue;
        }
        let Some(next) = next else {
            return Ok(DemotionRound {
                step: Some(DemotionStep::Sorted(rebuilt)),
                blocked: None,
            });
        };
        let slot = if next < residue {
            &mut reduced
        } else {
            &mut held
        };
        slot.get_or_insert(DemotionStep::Reduced {
            dae: rebuilt,
            residue: next,
        });
    }
    match reduced.or(held) {
        Some(step) => Ok(DemotionRound {
            step: Some(step),
            blocked: None,
        }),
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
enum HolonomicStep {
    Sorted {
        dae: dae::Dae,
        manifold: Vec<u32>,
    },
    Reduced {
        dae: dae::Dae,
        manifold: Vec<u32>,
        residue: usize,
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
fn reduce_holonomic_constraint(model: &dae::Dae) -> Result<HolonomicRound, StructuralError> {
    reduce_holonomic_constraint_with_enumeration(model, |_| {})
}

/// Testable enumeration seam: production supplies the identity operation,
/// while the adversary reverses discovery before the mandatory owner sort.
fn reduce_holonomic_constraint_with_enumeration(
    model: &dae::Dae,
    mut perturb_enumeration: impl FnMut(&mut Vec<HolonomicConstraint>),
) -> Result<HolonomicRound, StructuralError> {
    let residue = model
        .inspect(|view| sort(view).map(|_| ()))
        .map_or_else(|error| unmatched_residue(&error), |_| None);
    let Some(mut residue) = residue else {
        return Ok(HolonomicRound {
            step: None,
            blocked: None,
        });
    };
    let mut reduced: Option<dae::Dae> = None;
    let mut manifold = Vec::new();
    loop {
        let current = reduced.as_ref().unwrap_or(model);
        let pass = holonomic_pass(current, residue, &manifold, &mut perturb_enumeration)?;
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
            }) => {
                reduced = Some(dae);
                manifold = next_manifold;
                residue = next_residue;
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

/// Try every current certificate in deterministic owner order and accept only
/// the first replacement that strictly reduces the unmatched residue.
fn holonomic_pass(
    model: &dae::Dae,
    residue: usize,
    prior_manifold: &[u32],
    perturb_enumeration: &mut impl FnMut(&mut Vec<HolonomicConstraint>),
) -> Result<HolonomicPass, StructuralError> {
    let stated = model.inspect(represented_initial_values);
    let mut candidates = model.inspect(holonomic_constraints);
    perturb_enumeration(&mut candidates);
    candidates.sort_by_key(|candidate| candidate.owner_ordinal);
    let mut reduced = None;
    let mut blocked = None;
    for constraint in candidates {
        let (rebuilt, manifold) = rebuild_holonomic_constraint(model, &constraint, prior_manifold)?;
        let next = match rebuilt.inspect(|view| sort(view).map(|_| ())) {
            Ok(()) => None,
            Err(error) => match unmatched_residue(&error) {
                Some(next) if next < residue => Some(next),
                _ => continue,
            },
        };
        if let Some(discarded) = model.inspect(|source| {
            rebuilt.inspect(|view| discarded_stated_initial_value(source, view, &stated))
        })? {
            blocked.get_or_insert(discarded);
            continue;
        }
        let Some(next) = next else {
            return Ok(HolonomicPass {
                step: Some(HolonomicStep::Sorted {
                    dae: rebuilt,
                    manifold,
                }),
                blocked: None,
            });
        };
        reduced.get_or_insert(HolonomicStep::Reduced {
            dae: rebuilt,
            manifold,
            residue: next,
        });
    }
    Ok(HolonomicPass {
        step: reduced,
        blocked,
    })
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
