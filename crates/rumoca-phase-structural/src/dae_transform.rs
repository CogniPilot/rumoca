//! Constructor-only structural DAE-to-DAE lowering.
//!
//! Requested states are selected before matching; already selected regular
//! systems remain borrowed. A singular system is rebuilt when a
//! scalar state is directly defined by a differentiable constraint. The
//! replacement DAE demotes that state and substitutes the exact symbolic
//! derivative of its definition at every derivative occurrence.

mod auxiliary_blocks;
mod builtin_profiles;
mod component_constraint;
mod component_projection;
mod constraints;
mod declarations;
mod demotion_bounds;
mod derivative_aliases;
mod differentiation;
mod equalities;
mod equation_activity;
mod event_owners;
mod expressions;
mod formal_derivatives;
mod function_derivatives;
mod functions;
mod initial_pins;
mod observation;
mod parameter_conditionals;
mod reconstruction;
mod runtime_quotients;
mod semantic_owners;
mod source;
mod temporal;
mod tensor_maps;
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
use self::initial_pins::{stated_initial_variables, transferred_initial_values};
use self::observation::{
    AttemptOutcome, CandidateGroup, DirectIdentity, HolonomicIdentity, Identity, Lane,
    ReductionEvent, ReductionObserver, ReductionRecorder, StoppedOutcome,
};
#[cfg(test)]
use self::reconstruction::rebuild_with_state_demotion;
use self::reconstruction::{
    rebuild_holonomic_constraint, rebuild_with_state_demotion_and_manifold,
};
use self::source::ReductionSource;
use crate::{
    BltBlock, EquationRef, SortedDae, StructuralError, StructuredScalarBlock, UnknownId, sort,
};

pub use self::formal_derivatives::{
    FormalDerivativeStage, FormalDerivativeSystem, FormalDerivativeView, FormalStageCoordinate,
    FormalStageEquation, FormalStateCandidate, FormalStateCandidateView, FormalStateCoordinate,
    ReducedSelectionChart, StateSelection, construct_formal_derivatives,
};
pub use self::initial_pins::{InitialValuePin, InitialValueRole, PinTerm};
pub use self::observation::{
    ReductionCandidateGroup, ReductionIdentity, ReductionLane, ReductionOutcome, ReductionRecord,
    ReductionReport, ReductionSnapshot, ReductionStop, UnmatchedKind, UnmatchedName,
};

/// One admissible reduced state-selection chart, in the finalized transformed
/// DAE's own variable-ordinal space.
///
/// The reduced state selection picks a Dependent/Independent split of a
/// definitional first-integral coordinate group. A conserved first integral has
/// no globally injective reduced chart, so the fixed primary split folds when a
/// dependent coordinate passes through zero. This records one alternate split of
/// that same group: the `dependent` coordinates are reconstructed and the
/// `independent` coordinates are integrated. Each coordinate is a
/// `(transformed variable ordinal, scalar)` pair naming a scalar of the finalized
/// DAE that `PreparedDae::inspect` binds. The primary split is chart index zero.
#[derive(Clone, Debug)]
pub struct PreparedReducedChart {
    pub dependent: Box<[(u32, u32)]>,
    pub independent: Box<[(u32, u32)]>,
    /// Reciprocal conditioning of this chart's dependent Jacobian at the
    /// construction trial point, and the singular threshold it is measured
    /// against. The mirror of a folding coordinate may sit at or below the
    /// threshold here because it is regular at a different configuration.
    pub trial_rcond: f64,
    pub trial_singular_threshold: f64,
}

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
        /// Redundancy classification parallel to `manifold`: `true` marks a row
        /// of a loop-closure constraint that must reduce, `false` a conserved
        /// first integral that may be retained.
        manifold_redundant: Box<[bool]>,
        pins: Box<[InitialValuePin]>,
        structural: PreparedStructuralAnalysis,
        /// Admissible reduced state-selection charts issued by the
        /// formal-derivative selection. Empty except on the reduced-selection
        /// path that finalizes a folding definitional first-integral group.
        charts: Box<[PreparedReducedChart]>,
    },
}

impl PreparedDae<'_> {
    pub fn as_dae(&self) -> &dae::Dae {
        match self {
            Self::Borrowed { dae, .. } => dae,
            Self::Transformed { dae, .. } => dae,
        }
    }

    /// True when the retained manifold carries a redundant loop-closure
    /// constraint: one whose position form is over-determining and is closed
    /// only by differentiating to acceleration, introducing a multiplier. State
    /// selection reduces such a system to an independent basis. A system whose
    /// manifold constraints are all conserved first integrals returns false:
    /// retaining its source coordinates and enforcing the invariants through the
    /// manifold projection stays regular, whereas reducing folds when a selected
    /// coordinate passes through zero.
    pub fn manifold_requires_reduction(&self) -> bool {
        match self {
            Self::Borrowed { .. } => false,
            Self::Transformed {
                manifold_redundant, ..
            } => manifold_redundant.contains(&true),
        }
    }

    pub fn inspect<R>(&self, inspect: impl for<'dae> FnOnce(PreparedSystem<'_, 'dae>) -> R) -> R {
        let (manifold, pins, structural, charts) = match self {
            Self::Borrowed {
                pins, structural, ..
            } => ([].as_slice(), pins, structural, [].as_slice()),
            Self::Transformed {
                manifold,
                pins,
                structural,
                charts,
                ..
            } => (&**manifold, pins, structural, &**charts),
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
                charts,
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
    /// Admissible reduced state-selection charts for a folding definitional
    /// first-integral coordinate group, naming scalars of `view`. Empty for
    /// every prepared system without such a group.
    pub charts: &'prepared [PreparedReducedChart],
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
        tearing: Option<crate::tearing::TearingResult>,
        guarded_tearing: Option<crate::tearing::TearingResult>,
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
                tearing,
                guarded_tearing,
            } => Self::AlgebraicLoop {
                equations,
                unknowns: unknowns.into_iter().map(ErasedUnknown::erase).collect(),
                tearing,
                guarded_tearing,
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
                tearing,
                guarded_tearing,
            } => BltBlock::AlgebraicLoop {
                equations: equations.clone(),
                unknowns: unknowns.iter().map(|unknown| unknown.bind(view)).collect(),
                tearing: tearing.clone(),
                guarded_tearing: guarded_tearing.clone(),
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

/// One direct-demotion round's structural analysis, retaining the incidence so
/// the next round can reuse the rows this demotion did not touch.
///
/// `reuse` supplies the prior round's incidence and the owners the demotion
/// rewrites; when it is `None` the incidence is built from scratch. The
/// [`ReusableIncidence`] is returned whenever the incidence built, even if the
/// system is singular, because a singular-with-residue round is exactly the one
/// whose incidence the next round reuses.
fn structural_analysis_capturing(
    model: &dae::Dae,
    reuse: Option<&crate::incidence::ReusableIncidence>,
    touched: Option<&[bool]>,
) -> (
    Result<PreparedStructuralAnalysis, StructuralError>,
    Option<crate::incidence::ReusableIncidence>,
) {
    let reuse = reuse
        .zip(touched)
        .map(|(prev, mask)| crate::incidence::IncidenceReuse::new(prev, mask));
    model.inspect(|view| {
        let incidence = match reuse {
            Some(reuse) => crate::incidence::build_incidence_reusing(view, reuse),
            None => crate::incidence::build_incidence(view),
        };
        match incidence {
            Ok(incidence) => {
                let reusable = crate::incidence::ReusableIncidence::from_incidence(&incidence);
                let analysis = crate::sort_from_incidence(view, &incidence)
                    .map(PreparedStructuralAnalysis::issue);
                (analysis, Some(reusable))
            }
            Err(error) => (Err(error), None),
        }
    })
}

#[derive(Clone, Copy)]
struct DirectStateConstraint {
    state: u32,
    rhs: StateDefinition,
    rhs_sign: self::equalities::EqualitySign,
    owner: dae::DaeProvenance,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum StateDefinition {
    Expression(u32),
    DerivativeExpression(u32),
    Auxiliary(u32),
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct HolonomicOwnerKey {
    owner: usize,
    body: Option<usize>,
    lifted: Option<u32>,
    component: Option<usize>,
}

impl HolonomicConstraint {
    fn owner_key(&self) -> HolonomicOwnerKey {
        HolonomicOwnerKey {
            owner: self.owner_ordinal,
            body: self.body_ordinal,
            lifted: self.lifted_algebraic,
            component: self
                .proof
                .component
                .as_ref()
                .map(|component| component.scalar),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ManifoldConstraint {
    expression: u32,
    lifted: Option<LiftedManifoldOwner>,
    /// Whether this row belongs to a redundant loop-closure constraint. A
    /// conserved first integral (its lower-order form is implied by the ODE, so
    /// one differentiation already reconstructs a matched state derivative) is
    /// definitional and keeps `false`; a genuine loop closure, whose position
    /// form is over-determining and is closed only by differentiating to
    /// acceleration through a multiplier, is redundant and carries `true`.
    redundant: bool,
}

/// One retained manifold constraint as the reduction pipeline hands it over: a
/// DAE-local expression ordinal and whether it belongs to a redundant loop
/// closure. The finalized [`PreparedDae`] stores these split into two primitive
/// arrays so no phase-private type leaks through its public shape; state
/// selection reads the redundancy classification through
/// [`PreparedDae::manifold_requires_reduction`] without re-deriving structure.
#[derive(Clone, Copy)]
struct ManifoldEntry {
    expression: u32,
    redundant: bool,
}

impl ManifoldEntry {
    fn from_constraint(constraint: ManifoldConstraint) -> Self {
        Self {
            expression: constraint.expression,
            redundant: constraint.redundant,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct LiftedManifoldOwner {
    state: u32,
    owner_ordinal: usize,
    body_ordinal: Option<usize>,
    residual: u32,
    value_residual: u32,
}

/// Evidence collected from the finalized source DAE before a residual may be
/// differentiated for index reduction.
#[derive(Clone)]
struct HolonomicDifferentiationProof {
    residual: u32,
    maximum_order: u8,
    derivative_anchors: equalities::DerivativeAnchors,
    anchored_states: Box<[u32]>,
    component: Option<component_constraint::ComponentConstraint>,
    lifted_value: Option<std::sync::Arc<constraints::lifted_values::LiftedValueProof>>,
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
    let report = recorder.finish(result.is_err());
    (result, report)
}

fn prepare_for_solve_with_observer<'source>(
    model: &'source dae::Dae,
    observer: &mut impl ReductionObserver,
) -> Result<PreparedDae<'source>, StructuralError> {
    let selected = match reconstruction::rebuild_requested_states(model) {
        Ok(Some(selected)) => selected,
        Ok(None) => return reduce_for_solve_with_observer(model, observer),
        Err(error) => return observed_failure(error, observer),
    };
    match reduce_for_solve_with_observer(&selected, observer)? {
        PreparedDae::Borrowed {
            pins, structural, ..
        } => Ok(PreparedDae::Transformed {
            dae: Box::new(selected),
            manifold: Box::new([]),
            manifold_redundant: Box::new([]),
            pins,
            structural,
            charts: Box::new([]),
        }),
        PreparedDae::Transformed {
            dae,
            manifold,
            manifold_redundant,
            pins,
            structural,
            charts,
        } => Ok(PreparedDae::Transformed {
            dae,
            manifold,
            manifold_redundant,
            pins,
            structural,
            charts,
        }),
    }
}

fn reduce_for_solve_with_observer<'source>(
    model: &'source dae::Dae,
    observer: &mut impl ReductionObserver,
) -> Result<PreparedDae<'source>, StructuralError> {
    let (singular, mut current_reusable) = match structural_analysis_capturing(model, None, None) {
        (Ok(structural), _) => return borrowed_with_observer(model, structural, observer),
        (Err(error @ StructuralError::Singular { .. }), reusable) => (error, reusable),
        (Err(StructuralError::EmptySystem), _) => {
            return borrowed_with_observer(model, PreparedStructuralAnalysis::empty(), observer);
        }
        (Err(error), _) => {
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
        let round = match demote_direct_state_with_observer(
            current_model,
            residue,
            &[],
            true,
            current_reusable.as_ref(),
            observer,
        ) {
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
                reusable,
                ..
            }) => {
                residue = next;
                demoted_error = Some(error);
                demoted = Some(dae);
                current_reusable = Some(reusable);
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
        discard_demoted_dae(demoted.take(), demoted_error.take(), observer);
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
    finalize_holonomic_outcome(holonomic, blocked, singular, observer)
}

/// Turn the final holonomic round into a prepared system: a reduction becomes a
/// transformed DAE carrying the classified manifold, a blocked stated initial
/// value surfaces as its own diagnostic, and an exhausted system reports the
/// original singularity.
fn finalize_holonomic_outcome(
    holonomic: HolonomicRound,
    outer_blocked: Option<DiscardedInitialValue>,
    singular: StructuralError,
    observer: &mut impl ReductionObserver,
) -> Result<PreparedDae<'static>, StructuralError> {
    match (holonomic.step, outer_blocked.or(holonomic.blocked)) {
        (Some((dae, manifold, structural)), _) => transformed_with_observer(
            dae,
            manifold
                .into_iter()
                .map(ManifoldEntry::from_constraint)
                .collect(),
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

fn discard_demoted_dae(
    model: Option<dae::Dae>,
    error: Option<StructuralError>,
    observer: &mut impl ReductionObserver,
) {
    if let (Some(model), Some(error)) = (model, error) {
        observer.discard_stalled(model, Vec::new(), &error);
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
    let result = borrowed(model, structural).and_then(derivative_aliases::normalize);
    observer.observe(ReductionEvent::Stopped {
        outcome: match &result {
            Ok(PreparedDae::Borrowed { .. }) => StoppedOutcome::Borrowed,
            Ok(PreparedDae::Transformed { .. }) => StoppedOutcome::Sorted,
            Err(error) => StoppedOutcome::Failure { error },
        },
    });
    result
}

/// Hand back a rewritten system, reading its initial values off the *replacement*
/// so that a demotion's new roles decide which coordinate the runtime seeds.
fn transformed(
    model: dae::Dae,
    manifold: Vec<ManifoldEntry>,
    structural: PreparedStructuralAnalysis,
    charts: Box<[PreparedReducedChart]>,
) -> Result<PreparedDae<'static>, StructuralError> {
    let pins = model.inspect(transferred_initial_values)?;
    let (expressions, redundant): (Vec<u32>, Vec<bool>) = manifold
        .into_iter()
        .map(|entry| (entry.expression, entry.redundant))
        .unzip();
    Ok(PreparedDae::Transformed {
        dae: Box::new(model),
        manifold: expressions.into_boxed_slice(),
        manifold_redundant: redundant.into_boxed_slice(),
        pins: pins.into_boxed_slice(),
        structural,
        charts,
    })
}

fn transformed_with_observer(
    model: dae::Dae,
    manifold: Vec<ManifoldEntry>,
    structural: PreparedStructuralAnalysis,
    observer: &mut impl ReductionObserver,
) -> Result<PreparedDae<'static>, StructuralError> {
    let result = transformed(model, manifold, structural, Box::new([]))
        .and_then(derivative_aliases::normalize);
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
        /// The incidence of the reduced system, carried to the next round so it
        /// reuses the rows this demotion did not touch.
        reusable: crate::incidence::ReusableIncidence,
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
struct DemotionPassPolicy<'a> {
    group: CandidateGroup,
    allow_held: bool,
    /// The prior round's incidence, reused for owners this demotion leaves
    /// untouched; `None` disables reuse and rebuilds the incidence in full.
    reuse: Option<&'a crate::incidence::ReusableIncidence>,
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
    reuse: Option<&crate::incidence::ReusableIncidence>,
    observer: &mut impl ReductionObserver,
) -> Result<DemotionRound, StructuralError> {
    let source = ReductionSource::new(model);
    let candidates = source.inspect(direct_state_constraints);
    let stated = model.inspect(stated_initial_variables);
    let unconditional = demotion_pass_with_observer(
        &source,
        residue,
        &stated,
        &candidates.admissible,
        prior_manifold,
        DemotionPassPolicy {
            group: CandidateGroup::DirectAdmissible,
            allow_held,
            reuse,
        },
        observer,
    )?;
    if unconditional.step.is_some() {
        return Ok(unconditional);
    }
    let carried = demotion_pass_with_observer(
        &source,
        residue,
        &stated,
        &candidates.conditional,
        prior_manifold,
        DemotionPassPolicy {
            group: CandidateGroup::DirectConditional,
            allow_held,
            reuse,
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

/// Try one candidate whose derivative and retained-value obligations hold,
/// observing its identity and outcome without changing the decision.
fn attempt_direct_candidate(
    source: &ReductionSource<'_>,
    residue: usize,
    stated: &[u32],
    candidate: &DirectStateConstraint,
    prior_manifold: &[ManifoldConstraint],
    reuse: Option<&crate::incidence::ReusableIncidence>,
    observer: &mut impl ReductionObserver,
) -> Result<DirectAttempt, StructuralError> {
    let identity = Identity::Direct(DirectIdentity::from(candidate));
    if !source.inspect(|view, facts| {
        constraints::demotion_preserves_manifold_values(view, facts, candidate, prior_manifold)
    }) {
        observer.observe(ReductionEvent::Attempt {
            lane: Lane::Direct,
            identity,
            outcome: AttemptOutcome::WouldInvalidateManifold,
        });
        return Ok(DirectAttempt::Rejected);
    }
    reconstruct_direct_candidate(
        source,
        residue,
        stated,
        candidate,
        prior_manifold,
        reuse,
        observer,
    )
}

fn reconstruct_direct_candidate(
    source: &ReductionSource<'_>,
    residue: usize,
    stated: &[u32],
    candidate: &DirectStateConstraint,
    prior_manifold: &[ManifoldConstraint],
    reuse: Option<&crate::incidence::ReusableIncidence>,
    observer: &mut impl ReductionObserver,
) -> Result<DirectAttempt, StructuralError> {
    let model = source.model();
    let identity = Identity::Direct(DirectIdentity::from(candidate));
    let (rebuilt, manifold) =
        match rebuild_with_state_demotion_and_manifold(source, *candidate, prior_manifold) {
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
    if let Some(outcome) = direct_reconstruction_rejection(model, &rebuilt, &manifold) {
        observer.observe(ReductionEvent::Attempt {
            lane: Lane::Direct,
            identity,
            outcome,
        });
        return Ok(DirectAttempt::Rejected);
    }
    // A direct demotion with no retained manifold rewrites only the owners that
    // reference the demoted variable, so the next round can reuse every other
    // row of this round's incidence rather than reproject the whole system.
    let touched = (reuse.is_some() && prior_manifold.is_empty())
        .then(|| source.demotion_rows.touched_owners(candidate.state));
    let (analysis, reusable) = structural_analysis_capturing(&rebuilt, reuse, touched.as_deref());
    let (next, retained_error, structural) = match analysis {
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
            reusable: reusable.expect("a reduced candidate built its incidence"),
        },
    })
}

fn direct_reconstruction_rejection(
    source: &dae::Dae,
    rebuilt: &dae::Dae,
    manifold: &[ManifoldConstraint],
) -> Option<AttemptOutcome<'static>> {
    if !equation_activity::preserves_equations(source, rebuilt) {
        Some(AttemptOutcome::WouldCreateVacuousResidual)
    } else if !manifold.is_empty() && !manifold_is_state_only(rebuilt, manifold) {
        Some(AttemptOutcome::WouldInvalidateManifold)
    } else {
        None
    }
}

/// Try one list of demotion candidates against `model`.
fn demotion_pass_with_observer(
    source: &ReductionSource<'_>,
    residue: usize,
    stated: &[u32],
    candidates: &[DirectStateConstraint],
    prior_manifold: &[ManifoldConstraint],
    policy: DemotionPassPolicy<'_>,
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
        if reduced.is_some()
            && source
                .demotion_rows
                .cannot_sort(candidate.state, residue, prior_manifold)
        {
            continue;
        }
        match attempt_direct_candidate(
            source,
            residue,
            stated,
            candidate,
            prior_manifold,
            policy.reuse,
            observer,
        )? {
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
    differentiated_owners: BTreeSet<HolonomicOwnerKey>,
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

    fn discard_stalled(&mut self, observer: &mut impl ReductionObserver) {
        if let Some(model) = self.reduced.take() {
            observer.discard_stalled(
                model,
                std::mem::take(&mut self.manifold),
                &self.current_error,
            );
        }
    }

    fn accept_held(
        &mut self,
        held: (HolonomicConstraint, usize, HolonomicStep),
        observer: &mut impl ReductionObserver,
    ) {
        let (constraint, next_residue, step) = held;
        self.differentiated_owners.insert(constraint.owner_key());
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

    fn direct_step(
        &mut self,
        original: &dae::Dae,
        allow_held: bool,
        observer: &mut impl ReductionObserver,
    ) -> Result<Option<DemotionStep>, StructuralError> {
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
            allow_held,
            None,
            observer,
        )?;
        self.blocked = self.blocked.take().or(round.blocked);
        Ok(round.step)
    }

    fn exhaust_direct(
        &mut self,
        original: &dae::Dae,
        observer: &mut impl ReductionObserver,
    ) -> Result<Option<HolonomicRound>, StructuralError> {
        while let Some(step) = self.direct_step(original, true, observer)? {
            match step {
                DemotionStep::Sorted {
                    dae,
                    manifold,
                    structural,
                } => return sorted_holonomic_round(dae, manifold, structural).map(Some),
                step @ DemotionStep::Reduced { .. } => self.accept_reduced(step.into()),
            }
        }
        Ok(None)
    }

    fn direct_or_held(
        &mut self,
        original: &dae::Dae,
        held: Option<(HolonomicConstraint, usize, HolonomicStep)>,
        observer: &mut impl ReductionObserver,
    ) -> Result<Option<HolonomicRound>, StructuralError> {
        match self.direct_step(original, held.is_none(), observer)? {
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
                None => {
                    self.discard_stalled(observer);
                    Ok(Some(HolonomicRound {
                        step: None,
                        blocked: self.blocked.take(),
                    }))
                }
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
                reusable: _,
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
    let mut outcome = structural_analysis(model);
    let normalized = if matches!(outcome, Err(StructuralError::Singular { .. })) {
        derivative_aliases::normalize_tensors(model)?
    } else {
        None
    };
    if let Some(normalized) = &normalized {
        outcome = structural_analysis(normalized);
    }
    let (residue, current_error) = match outcome {
        Ok(structural) => {
            if let Some(normalized) = normalized {
                return sorted_holonomic_round(normalized, Vec::new(), structural);
            }
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
    state.reduced = normalized;
    if state.reduced.is_some()
        && let Some(round) = state.exhaust_direct(model, observer)?
    {
        return Ok(round);
    }
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
        let round = demote_direct_state_with_observer(&model, 0, &manifold, false, None, &mut ())?;
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
                    constraint
                        .proof
                        .component
                        .as_ref()
                        .is_none_or(|component| component.scalar == scalar)
                        && residual_scalar_is_structurally_active(
                            view, residual, scalar, None, &mut cache,
                        )
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
        if constraint
            .proof
            .component
            .as_ref()
            .is_some_and(|component| component.scalar != scalar)
        {
            return false;
        }
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
    source: &ReductionSource<'_>,
    residue: usize,
    prior_manifold: &[ManifoldConstraint],
    stated: &[u32],
    constraint: HolonomicConstraint,
    observer: &mut impl ReductionObserver,
) -> Result<HolonomicAttempt, StructuralError> {
    let model = source.model();
    let identity = Identity::Holonomic(HolonomicIdentity::from(&constraint));
    let (rebuilt, manifold) =
        match rebuild_holonomic_constraint(source, &constraint, prior_manifold) {
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
    if !equation_activity::preserves_equations(source.model(), &rebuilt)
        || !holonomic_replacement_is_structurally_active(&rebuilt, &constraint)
    {
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

/// Try every current certificate in deterministic owner order, preferring a
/// strict residue reduction over a once-only owner-consuming held step.
fn holonomic_pass_with_observer(
    model: &dae::Dae,
    residue: usize,
    prior_manifold: &[ManifoldConstraint],
    differentiated_owners: &mut BTreeSet<HolonomicOwnerKey>,
    perturb_enumeration: &mut impl FnMut(&mut Vec<HolonomicConstraint>),
    observer: &mut impl ReductionObserver,
) -> Result<HolonomicPass, StructuralError> {
    let source = ReductionSource::new(model);
    let stated = model.inspect(stated_initial_variables);
    let (mut candidates, incident) = source.inspect(|view, facts| {
        let candidates = index_reduction_constraints(view, facts);
        let incident = crate::overdetermined_block_variables(view)?;
        Ok::<_, StructuralError>((candidates, incident))
    })?;
    perturb_enumeration(&mut candidates);
    candidates.retain(|candidate| {
        !differentiated_owners.contains(&candidate.owner_key())
            && candidate
                .lifted_algebraic
                .is_none_or(|variable| incident.contains(&variable))
    });
    candidates.sort_by_key(|candidate| {
        (
            usize::from(candidate.lifted_algebraic.is_some()),
            candidate.owner_ordinal,
            candidate.body_ordinal,
            candidate
                .proof
                .component
                .as_ref()
                .map(|component| component.scalar),
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
            &source,
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
            differentiated_owners.insert(constraint.owner_key());
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

// SPEC_0021 file-size exception: this file is 2026 lines, over the 2000-line
// action threshold; split plan: move the reconstruction and holonomic-constraint helpers into the existing dae_transform/ submodules.
