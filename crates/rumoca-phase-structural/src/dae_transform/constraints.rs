//! Recognize the singular-system constraints this phase is allowed to reduce.
//!
//! The reduction these preflights gate is Pantelides differentiation with
//! dummy-derivative state selection; see the References section of the parent
//! module, [`super`], for the citations.
//!
//! Nothing here rewrites a DAE. Each function reports what the source system
//! already proves: which states a residual defines directly, which residuals
//! are holonomic, and whether an expression survives the exact symbolic
//! differentiation the reconstruction will later perform. A candidate that
//! fails a preflight here is never handed to reconstruction, so every
//! `unreachable!` in the differentiation code stands on a check made here.
//!
//! Two facts let detection see past a single residual. [`SystemEqualities`]
//! closes the connector alias chains an MSL model writes as bare `a - b`
//! potential equalities, so a state that is provably the same quantity as
//! another state or a parameter is recognized as redundant. Explicit derivative
//! definitions are read in either orientation, so the `w - der(phi)` form MSL
//! components use supplies `d/dt phi` exactly as `der(phi) - w` would.

mod alternative_definitions;
pub(super) mod lifted_values;
mod materialization;
mod state_derivative;
mod value_identity;

use state_derivative::has_state_only_first_derivative;

use materialization::{
    can_materialize_holonomic_value, can_materialize_holonomic_value_in_context,
};

use rumoca_core::{Span, StateSelect};
use rumoca_eval_dae::FunctionCallContext;
use rumoca_ir_dae as dae;

use crate::CausalDefinitions;
use crate::residual_normalization::equation_sides;

use super::builtin_profiles::{is_differentiable_binary, is_differentiable_builtin};
use super::component_constraint::ComponentConstraint;
use super::component_projection::projected_element;
use super::equalities::{
    DerivativeAnchors, EqualityAnchor, EqualitySign, SystemEqualities, forwarded_call_argument,
    is_time_invariant,
};
use super::initial_pins::stated_initial_variables;
use super::tensor_maps::has_invariant_subscripts;
use super::{
    DirectStateConstraint, HolonomicConstraint, HolonomicDifferentiationProof, ManifoldConstraint,
    StateDefinition,
};
use crate::StructuralError;

#[derive(Clone, Copy)]
pub(super) struct ExplicitDerivativeDefinition {
    pub(super) residual: u32,
    pub(super) expression: u32,
}

/// The exact indirections reconstruction is allowed to follow while
/// differentiating, gathered once per source system.
pub(super) struct DifferentiationFacts {
    pub(super) equalities: SystemEqualities,
    additive_values: lifted_values::AdditiveValueFacts,
    pub(super) derivative_definitions: Vec<Option<ExplicitDerivativeDefinition>>,
    pub(super) algebraic_definitions: Vec<Option<u32>>,
    pub(super) component_definitions: Vec<Option<std::sync::Arc<ComponentConstraint>>>,
    pub(super) auxiliary_blocks:
        Vec<Option<std::sync::Arc<super::auxiliary_blocks::AuxiliaryBlock>>>,
    /// Algebraic variables whose value is a parameter-constant and the pure
    /// functions that preserve that constancy. The time derivative of such a
    /// coordinate is zero.
    pub(super) invariance: crate::time_invariant::TimeInvariance,
}

#[cfg(test)]
thread_local! {
    pub(super) static FACT_COLLECTIONS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

impl DifferentiationFacts {
    pub(super) fn collect(view: dae::DaeView<'_>) -> Self {
        #[cfg(test)]
        FACT_COLLECTIONS.with(|count| count.set(count.get() + 1));
        let causal = CausalDefinitions::derive(view);
        let algebraic_definitions = view
            .variables()
            .map(|(id, variable)| {
                (variable.role() == dae::VariableRole::Algebraic)
                    .then(|| causal.definition_for_variable(id))
                    .flatten()
                    .map(dae::ExprId::index)
            })
            .collect();
        let mut facts = Self {
            equalities: SystemEqualities::collect(view),
            additive_values: lifted_values::AdditiveValueFacts::collect(view),
            derivative_definitions: explicit_derivative_definitions(view),
            algebraic_definitions,
            component_definitions: vec![None; view.variable_count()],
            auxiliary_blocks: vec![None; view.variable_count()],
            invariance: crate::time_invariant::TimeInvariance::derive_with_causal(view, &causal),
        };
        alternative_definitions::complete(view, &mut facts);
        facts.complete_reconstruction_facts(view);
        for block in super::auxiliary_blocks::derive_state_blocks(view, &facts) {
            let variable = block.variable;
            facts.auxiliary_blocks[variable as usize] = Some(block);
        }
        facts
    }

    /// Preserve each admitted witness while extending both aggregate and
    /// component proofs. A later whole-vector solve cannot hide an already
    /// proved independent component of its original source definition.
    fn complete_reconstruction_facts(&mut self, view: dae::DaeView<'_>) {
        loop {
            let blocks = super::auxiliary_blocks::derive_blocks(view, self);
            let added_blocks = extend_proofs(&mut self.auxiliary_blocks, blocks);
            let components = super::component_constraint::derive_definitions(view, self);
            let added_components = extend_proofs(&mut self.component_definitions, components);
            if !added_blocks && !added_components {
                return;
            }
        }
    }

    pub(super) fn algebraic_definition<'dae>(
        &self,
        view: dae::DaeView<'dae>,
        algebraic: dae::AlgebraicId<'dae>,
    ) -> Option<dae::ExprId<'dae>> {
        if self.auxiliary_blocks[algebraic.index() as usize].is_some() {
            return None;
        }
        self.algebraic_definitions[algebraic.index() as usize]
            .and_then(|definition| view.expression_id(definition as usize))
    }

    /// Select the same exact value for materialization proof and reconstruction.
    /// An invariant class can prove a zero derivative without naming its value.
    pub(super) fn algebraic_value_definition<'dae>(
        &self,
        view: dae::DaeView<'dae>,
        algebraic: dae::AlgebraicId<'dae>,
    ) -> Option<(dae::ExprId<'dae>, EqualitySign)> {
        self.equalities
            .value_anchor_of(algebraic.index())
            .and_then(|(anchor, sign)| {
                let expression = self.equalities.payload_anchor_expression(anchor)?;
                Some((view.expression_id(expression as usize)?, sign))
            })
            .or_else(|| {
                self.algebraic_definition(view, algebraic)
                    .map(|expression| (expression, EqualitySign::Same))
            })
    }

    pub(super) fn can_materialize_value(&self, view: dae::DaeView<'_>, expression: u32) -> bool {
        self.materialized_state_anchors(view, expression).is_some()
    }

    pub(super) fn materialized_state_anchors(
        &self,
        view: dae::DaeView<'_>,
        expression: u32,
    ) -> Option<Vec<u32>> {
        self.materialized_state_anchors_in_context(
            view,
            expression,
            &FunctionCallContext::default(),
        )
    }

    pub(super) fn materialized_state_anchors_in_context<'dae>(
        &self,
        view: dae::DaeView<'dae>,
        expression: u32,
        context: &FunctionCallContext<'dae>,
    ) -> Option<Vec<u32>> {
        let expression = view.expression_id(expression as usize)?;
        let mut states = Vec::new();
        can_materialize_holonomic_value_in_context(
            view,
            self,
            expression,
            &mut vec![Visit::Pending; view.expression_count()],
            context,
            &mut states,
        )
        .then_some(states)
    }

    /// Whether the finalized source proves this instantiated expression is the
    /// additive zero of its Real payload.
    pub(super) fn expression_is_zero<'dae>(
        &self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> bool {
        let scoped_context = context.scoped_to_expression(view, expression);
        let context = &scoped_context;
        if let Some(branch) = context.selected_branch(view, expression) {
            return self.expression_is_zero(view, branch, context);
        }
        if let Some(element) = projected_element(view, self, expression) {
            return self.expression_is_zero(view, element, context);
        }
        if let Some((result, nested)) = context.call_result(view, expression) {
            return self.expression_is_zero(view, result, &nested);
        }
        let Some(node) = view.expression(expression) else {
            return false;
        };
        match node.operation() {
            dae::ExpressionOperation::Literal(
                dae::DaeLiteral::Real(0.0) | dae::DaeLiteral::Integer(0),
            )
            | dae::ExpressionOperation::Builtin {
                builtin: dae::PureBuiltin::Zeros,
                ..
            } => true,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => context
                .parameter_argument(parameter)
                .is_some_and(|argument| self.expression_is_zero(view, argument, context)),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) => self
                .equalities
                .value_anchor_of(algebraic.index())
                .and_then(|(anchor, _)| self.equalities.payload_anchor_expression(anchor))
                .and_then(|anchor| view.expression_id(anchor as usize))
                .or_else(|| self.algebraic_definition(view, algebraic))
                .is_some_and(|definition| self.expression_is_zero(view, definition, context)),
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => self.expression_is_zero(view, operand, context),
            dae::ExpressionOperation::Binary {
                operator: dae::BinaryOperator::Multiply,
                lhs,
                rhs,
            } => {
                self.expression_is_zero(view, lhs, context)
                    || self.expression_is_zero(view, rhs, context)
            }
            dae::ExpressionOperation::Field { base, field } => context
                .projected_field(view, base, field)
                .is_some_and(|(projected, projected_context)| {
                    self.expression_is_zero(view, projected, &projected_context)
                }),
            _ => false,
        }
    }
}

fn extend_proofs<T>(current: &mut [Option<T>], proposed: Vec<Option<T>>) -> bool {
    let mut added = false;
    for (current, proposed) in current.iter_mut().zip(proposed) {
        if current.is_none() && proposed.is_some() {
            *current = proposed;
            added = true;
        }
    }
    added
}

/// Where one expression stands in a differentiability walk.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Visit {
    Pending,
    InProgress,
    Differentiable,
}

/// Reusable visitation state for all holonomic roots in one finalized DAE.
///
/// Each expression packs the six `(order, context)` states into the low twelve
/// bits of one word and the root epoch into the rest. Advancing the epoch makes
/// every slot logically pending in O(1), while keeping storage to one word per
/// expression. The backing table is cleared only after the practically
/// unreachable epoch wraparound.
struct HolonomicProofScratch {
    epoch: u64,
    visited: Vec<u64>,
}

impl HolonomicProofScratch {
    const STATE_BITS: u32 = 2;
    const STATE_SLOT_COUNT: u32 = 6;
    const MAX_EPOCH: u64 = u64::MAX >> (Self::STATE_BITS * Self::STATE_SLOT_COUNT);

    fn new(expression_count: usize) -> Self {
        Self {
            epoch: 0,
            visited: vec![0; expression_count],
        }
    }

    fn begin_root(&mut self) {
        if self.epoch == Self::MAX_EPOCH {
            self.visited.fill(0);
            self.epoch = 1;
        } else {
            self.epoch += 1;
        }
    }

    fn state(&self, expression: usize, order: usize, context: usize) -> Visit {
        let packed = self.visited[expression];
        if packed >> (Self::STATE_BITS * Self::STATE_SLOT_COUNT) != self.epoch {
            return Visit::Pending;
        }
        match (packed >> Self::slot_shift(order, context)) & 0b11 {
            1 => Visit::InProgress,
            2 => Visit::Differentiable,
            _ => Visit::Pending,
        }
    }

    fn set_state(&mut self, expression: usize, order: usize, context: usize, state: Visit) {
        let shift = Self::slot_shift(order, context);
        let state = match state {
            Visit::Pending => 0,
            Visit::InProgress => 1,
            Visit::Differentiable => 2,
        };
        let packed = &mut self.visited[expression];
        if *packed >> (Self::STATE_BITS * Self::STATE_SLOT_COUNT) != self.epoch {
            *packed = self.epoch << (Self::STATE_BITS * Self::STATE_SLOT_COUNT);
        }
        *packed = (*packed & !(0b11 << shift)) | (state << shift);
    }

    const fn slot_shift(order: usize, context: usize) -> u32 {
        ((order * 2 + context) as u32) * Self::STATE_BITS
    }
}

/// Every state demotion one system offers, split by what taking it costs the
/// initial values the model states.
pub(super) struct StateDemotionCandidates {
    /// Demotions the source system alone proves keep every stated initial
    /// value, in the order the reduction should try them.
    pub(super) admissible: Vec<DirectStateConstraint>,
    /// Demotions that drop the MLS 3.6 §8.6 initial equation of the coordinate
    /// they demote, as the source system reads it. §8.6 states that equation
    /// about a *quantity*, so such a demotion is legal exactly when the system
    /// it produces still states the value about a coordinate the runtime
    /// answers — a question only the rebuilt system can settle, which
    /// [`discarded_stated_initial_value`] asks of it. Tried after every
    /// unconditional demotion, so a reduction never spends a stated value while
    /// another way forward is open.
    pub(super) conditional: Vec<DirectStateConstraint>,
}

/// One stated initial value a rebuilt system no longer states.
#[derive(Clone)]
pub(super) struct DiscardedInitialValue {
    /// The pinned variable, named as the model declares it.
    pub(super) variable: String,
    /// That variable's declaration.
    pub(super) span: Span,
}

pub(super) fn direct_state_constraints(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
) -> StateDemotionCandidates {
    let mut constraints = view
        .continuous_owners()
        .flat_map(|owner| match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                direct_state_constraint(view, facts, equation.residual(), equation.provenance())
                    .into_iter()
                    .collect::<Vec<_>>()
            }
            dae::ContinuousOwnerView::Structured { family, .. }
                if family.scalar_view()
                    == rumoca_core::ComprehensionScalarView::RowMajorProjection =>
            {
                family
                    .bodies()
                    .iter()
                    .filter_map(|body| {
                        direct_state_constraint(view, facts, body, family.provenance())
                    })
                    .collect()
            }
            dae::ContinuousOwnerView::Structured { .. } => Vec::new(),
        })
        .collect::<Vec<_>>();
    constraints.extend(redundant_state_constraints(view, &facts.equalities));
    let direct_states = constraints
        .iter()
        .map(|candidate| candidate.state)
        .collect::<std::collections::BTreeSet<_>>();
    constraints.extend(
        auxiliary_state_constraints(view, facts)
            .into_iter()
            .filter(|candidate| !direct_states.contains(&candidate.state)),
    );
    constraints.sort_by_key(|candidate| {
        let selection = view
            .variable(
                view.variable_id(candidate.state as usize)
                    .expect("candidate state identity resolves"),
            )
            .expect("candidate state declaration resolves")
            .state_select();
        (
            state_demotion_priority(selection),
            candidate.state,
            candidate.rhs,
            usize::from(candidate.rhs_sign == EqualitySign::Opposite),
        )
    });
    // A state can have several exact definitions. They are distinct
    // reconstruction proofs because `der(state)` is replaced by the derivative
    // of the selected RHS; discarding all but the first can turn a useful
    // kinematic definition into a tautology. Only byte-identical substitutions
    // are duplicates.
    constraints.dedup_by_key(|candidate| (candidate.state, candidate.rhs, candidate.rhs_sign));
    debug_assert!(
        constraints
            .iter()
            .all(|candidate| carries_a_differentiable_definition(view, facts, *candidate)),
        "a demotion candidate must satisfy the contract its RHS is consumed under"
    );
    // MLS 3.6 §8.6 turns every `fixed = true` start into an initialization
    // equation, and a demoted state has no initialization equation of its own
    // left. The split runs over the merged candidate list, so it covers the
    // residual path and the equality-closure path alike, and it reads only the
    // state each candidate demotes, so every exact definition for one state
    // lands on the same side of it.
    let (admissible, conditional) = constraints.into_iter().partition(|candidate| {
        keeps_stated_initial_value(view, &facts.equalities, candidate.state)
    });
    StateDemotionCandidates {
        admissible,
        conditional,
    }
}

/// A derivative proof alone cannot reconstruct a retained position value.
/// Require the exact RHS value only when a surviving manifold uses this state.
pub(super) fn demotion_preserves_manifold_values(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
    candidate: &DirectStateConstraint,
    manifold: &[ManifoldConstraint],
) -> bool {
    let state = view
        .variable_id(candidate.state as usize)
        .expect("candidate state resolves");
    let needs_value = manifold.iter().any(|entry| {
        if entry
            .lifted
            .is_some_and(|lifted| lifted.state == candidate.state)
        {
            return false;
        }
        let expression = view
            .expression_id(entry.expression as usize)
            .expect("retained manifold expression resolves");
        dae::expr_contains_var(view, expression, state)
    });
    if !needs_value {
        return true;
    }
    match candidate.rhs {
        StateDefinition::DerivativeExpression(_) => false,
        StateDefinition::Expression(rhs) => facts
            .materialized_state_anchors(view, rhs)
            .is_some_and(|states| !states.contains(&candidate.state)),
        StateDefinition::Auxiliary(variable) => facts.auxiliary_blocks[variable as usize]
            .as_ref()
            .is_some_and(|block| !block.state_anchors.contains(&candidate.state)),
    }
}

/// Check that reconstruction retains every source-fixed declaration.
///
/// Variable attributes carry the complete initial equation through checked
/// reconstruction. Solve inventories those attributes independently of state
/// roles, so demoting a coordinate or replacing a continuous residual does not
/// discard its initial equation. This check detects removal from that inventory;
/// equality-class transfer is only an equivalent way to lower the same row.
pub(super) fn discarded_stated_initial_value(
    source: dae::DaeView<'_>,
    rebuilt: dae::DaeView<'_>,
    stated: &[u32],
) -> Result<Option<DiscardedInitialValue>, StructuralError> {
    if stated.is_empty() {
        return Ok(None);
    }
    let kept = stated_initial_variables(rebuilt);
    let mut discarded = None;
    for variable in stated.iter().copied() {
        // The two systems are compared by variable ordinal, which reconstruction
        // preserves by reserving one target variable per source variable in
        // source order. Every ordinal this comparison rests on is checked, on
        // the path that finds a discarded value and on the path that finds none,
        // because a silent renumbering would make the second one a lie.
        let declaration = |view: dae::DaeView<'_>| {
            view.variable_id(variable as usize)
                .and_then(|id| view.variable(id))
                .map(|declaration| {
                    (
                        declaration.name().as_str().to_string(),
                        declaration.declaration().span(),
                    )
                })
        };
        let (Some((name, span)), Some((rebuilt_name, _))) =
            (declaration(source), declaration(rebuilt))
        else {
            return Err(renumbered(variable));
        };
        if name != rebuilt_name {
            return Err(renumbered(variable));
        }
        if !kept.contains(&variable) && discarded.is_none() {
            discarded = Some(DiscardedInitialValue {
                variable: name,
                span,
            });
        }
    }
    Ok(discarded)
}

fn renumbered(variable: u32) -> StructuralError {
    StructuralError::UnspannedContractViolation {
        reason: format!(
            "a structural reduction renumbered variable {variable}, so the stated initial \
             values proved about the source system name nothing in the rebuilt one"
        ),
    }
}

/// Whether `candidate` satisfies the one contract its RHS is consumed under.
///
/// A source-expression definition must be scoped to the model and exclude the
/// demoted coordinate. An auxiliary definition instead carries a source block
/// whose coefficient and value anchors exclude that state. Both retain the
/// original value equations; reconstruction replaces derivative coordinates
/// and materializes the value only for surviving manifold obligations.
fn carries_a_differentiable_definition(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
    candidate: DirectStateConstraint,
) -> bool {
    let (StateDefinition::Expression(rhs) | StateDefinition::DerivativeExpression(rhs)) =
        candidate.rhs
    else {
        return facts.auxiliary_blocks[candidate.state as usize]
            .as_ref()
            .is_some_and(|block| !block.state_anchors.contains(&candidate.state));
    };
    let Some(rhs) = view.expression_id(rhs as usize) else {
        return false;
    };
    let Some(node) = view.expression(rhs) else {
        return false;
    };
    if node.function_scope().is_some() || node.binder_domain().is_some() {
        return false;
    }
    let Some(state) = view
        .variable_id(candidate.state as usize)
        .and_then(|id| view.variable(id))
    else {
        return false;
    };
    !dae::expr_contains_var(view, rhs, state.id())
}

fn state_demotion_priority(selection: StateSelect) -> u8 {
    match selection {
        StateSelect::Never => 0,
        StateSelect::Avoid => 1,
        StateSelect::Default => 2,
        StateSelect::Prefer => 3,
        StateSelect::Always => 4,
    }
}

fn auxiliary_state_constraints(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
) -> Vec<DirectStateConstraint> {
    view.variables()
        .filter_map(|(id, variable)| {
            let dae::VariableIdentity::State(state) = variable.identity() else {
                return None;
            };
            let block = facts.auxiliary_blocks[id.index() as usize].as_ref()?;
            let mut visited = vec![Visit::Pending; view.expression_count()];
            if variable.state_select() == StateSelect::Always
                || !auxiliary_is_differentiable(view, facts, block, state, &mut visited)
            {
                return None;
            }
            let residual = view.expression_id(block.residual() as usize)?;
            Some(DirectStateConstraint {
                state: id.index(),
                rhs: StateDefinition::Auxiliary(id.index()),
                rhs_sign: EqualitySign::Same,
                owner: view.expression(residual)?.provenance(),
            })
        })
        .collect()
}

fn auxiliary_is_differentiable<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    block: &super::auxiliary_blocks::AuxiliaryBlock,
    demoted: dae::StateId<'dae>,
    visited: &mut [Visit],
) -> bool {
    !block.state_anchors.contains(&demoted.index())
        && block.operands().all(|operand| {
            is_differentiable_in_context(
                view,
                facts,
                view.expression_id(operand.expression as usize).unwrap(),
                demoted,
                visited,
                &operand.context(view),
            )
        })
}

/// States an asserted coordinate equality proves redundant.
///
/// The equality holds for all time, so the demoted state keeps the residual
/// that defines it and every `der` of it becomes the exact derivative of the
/// class anchor — zero when the class is pinned to a parameter.
fn redundant_state_constraints(
    view: dae::DaeView<'_>,
    equalities: &SystemEqualities,
) -> Vec<DirectStateConstraint> {
    equalities
        .redundant_states()
        .filter_map(|(state, anchor, rhs_sign)| {
            let variable = view.variable(view.variable_id(state as usize)?)?;
            if variable.state_select() == StateSelect::Always
                || variable.value_type().scalar_type() != dae::ScalarType::Real
            {
                return None;
            }
            Some(DirectStateConstraint {
                state,
                rhs: StateDefinition::Expression(equalities.anchor_expression(
                    view,
                    anchor,
                    variable.value_type(),
                )?),
                rhs_sign,
                owner: equalities.witness(state)?,
            })
        })
        .collect()
}

/// Whether the source system alone proves that demoting `state` keeps every
/// initial value the model states about the quantity it names.
///
/// MLS 3.6 §8.6: "For every Real variable vc with fixed = true, the equation
/// vc = startExpression is added to the initialization equations." A demoted
/// state is no longer a state, so it has no initialization equation of its own
/// left; that stated equation survives only when an equation the reduction
/// keeps reproduces it. Exactly one such proof is available from the source
/// system by itself, read off the offset-free equality layer because it is the
/// only one that proves anything about a *value*: a class anchored on another
/// state keeps that state, so the obligation survives when the anchor carries
/// the same one — `fixed = true` with the same stated start, same-signed.
///
/// Everything else is not a refusal but a *question*, and this layer is the
/// wrong place to answer it. A state that anchors its own class, an
/// opposite-signed member, an unpinned anchor, a class pinned to a
/// time-invariant value, two pinned members that disagree: whether the value
/// survives depends on the classes, roles and displacements of the system the
/// demotion produces, none of which exist yet. Those candidates go to
/// [`StateDemotionCandidates::conditional`], where the rebuilt system answers
/// with [`discarded_stated_initial_value`].
///
/// In particular an [`EqualityAnchor::Invariant`] class proves only that the
/// class holds *some* constant — `EqualityAnchor::Invariant { value: None, .. }`
/// names no expression at all, and the anchor carries no displacement — so it
/// cannot show the constant is the stated start. Accepting it here is what let
/// `x(start = 1, fixed = true)` in a class the system pins to 5 initialize at 5.
fn keeps_stated_initial_value(
    view: dae::DaeView<'_>,
    equalities: &SystemEqualities,
    state: u32,
) -> bool {
    let Some(variable) = view
        .variable_id(state as usize)
        .and_then(|id| view.variable(id))
    else {
        return false;
    };
    if variable.fixed_uniform() != Some(true) {
        return true;
    }
    match equalities.value_anchor_of(state) {
        Some((EqualityAnchor::State(anchor), EqualitySign::Same)) if anchor != state => view
            .variable_id(anchor as usize)
            .and_then(|id| view.variable(id))
            .is_some_and(|anchor| {
                anchor.fixed_uniform() == Some(true)
                    && states_the_same_start(view, variable, anchor)
            }),
        _ => false,
    }
}

/// Whether two declarations state the same initial value.
///
/// An absent `start` is the MLS 3.6 §4.8 attribute default of zero for a Real,
/// so an omitted start and an explicit `start = 0` state the same thing. Two
/// present starts are compared as written: they are time-invariant expressions,
/// and structural equality is the only equality this phase can prove about them
/// without evaluating them. Comparing conservatively can only refuse a demotion
/// that would have been safe; it can never accept one that drops a start.
fn states_the_same_start<'dae>(
    view: dae::DaeView<'dae>,
    left: dae::VariableView<'dae>,
    right: dae::VariableView<'dae>,
) -> bool {
    match (left.start(), right.start()) {
        (None, None) => true,
        (Some(start), None) | (None, Some(start)) => numeric_literal(view, start) == Some(0.0),
        (Some(left), Some(right)) => states_the_same_expression(view, left, right),
    }
}

/// Whether two time-invariant expressions are the same expression as written.
pub(super) fn states_the_same_expression<'dae>(
    view: dae::DaeView<'dae>,
    left: dae::ExprId<'dae>,
    right: dae::ExprId<'dae>,
) -> bool {
    if left == right {
        return true;
    }
    let (Some(left), Some(right)) = (view.expression(left), view.expression(right)) else {
        return false;
    };
    match (left.operation(), right.operation()) {
        // `start = 0` on a Real may be written as either literal kind, so the
        // two numeric literals are compared by value rather than by spelling.
        (dae::ExpressionOperation::Literal(left), dae::ExpressionOperation::Literal(right)) => {
            match (literal_value(left), literal_value(right)) {
                (Some(left), Some(right)) => left == right,
                _ => left == right,
            }
        }
        (
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(left)),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(right)),
        ) => left == right,
        (
            dae::ExpressionOperation::Unary {
                operator: left_operator,
                operand: left,
            },
            dae::ExpressionOperation::Unary {
                operator: right_operator,
                operand: right,
            },
        ) => left_operator == right_operator && states_the_same_expression(view, left, right),
        (
            dae::ExpressionOperation::Binary {
                operator: left_operator,
                lhs: left_lhs,
                rhs: left_rhs,
            },
            dae::ExpressionOperation::Binary {
                operator: right_operator,
                lhs: right_lhs,
                rhs: right_rhs,
            },
        ) => {
            left_operator == right_operator
                && states_the_same_expression(view, left_lhs, right_lhs)
                && states_the_same_expression(view, left_rhs, right_rhs)
        }
        _ => false,
    }
}

pub(super) fn numeric_literal<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<f64> {
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Literal(literal) => literal_value(literal),
        _ => None,
    }
}

/// The numeric value of a literal, for the two kinds a Real `start` can carry.
///
/// An `Integer` too wide to convert exactly reports nothing rather than a
/// rounded value, which leaves the comparison above to fall back on spelling
/// equality — the conservative answer.
fn literal_value(literal: &dae::DaeLiteral) -> Option<f64> {
    match literal {
        dae::DaeLiteral::Real(value) => Some(*value),
        dae::DaeLiteral::Integer(value) => i32::try_from(*value).ok().map(f64::from),
        _ => None,
    }
}

fn direct_state_constraint<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    residual_id: dae::ExprId<'dae>,
    owner: dae::DaeProvenance,
) -> Option<DirectStateConstraint> {
    let (lhs, rhs) = equation_sides(view, residual_id)?;
    [DerivativeAnchors::Exact, DerivativeAnchors::Affine]
        .into_iter()
        .find_map(|anchors| {
            direct_state_definition(view, facts, lhs, rhs, owner, anchors)
                .or_else(|| direct_state_definition(view, facts, rhs, lhs, owner, anchors))
        })
}

fn direct_state_definition<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    lhs: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
    owner: dae::DaeProvenance,
    anchors: DerivativeAnchors,
) -> Option<DirectStateConstraint> {
    let (state, rhs_sign) = state_anchor(view, &facts.equalities, lhs, anchors)?;
    let variable = view.variable(view.variable_id(state.index() as usize)?)?;
    if variable.state_select() == StateSelect::Always
        || variable.value_type().scalar_type() != dae::ScalarType::Real
        || variable.value_type().dimensions() != view.expression(rhs)?.value_type().dimensions()
        || dae::expr_contains_var(view, rhs, variable.id())
        || reaches_demoted_derivative(view, facts, rhs, state)
        || !is_differentiable(
            view,
            facts,
            rhs,
            state,
            &mut vec![Visit::Pending; view.expression_count()],
        )
    {
        return None;
    }
    Some(DirectStateConstraint {
        state: state.index(),
        rhs: match anchors {
            DerivativeAnchors::Exact => StateDefinition::Expression(rhs.index()),
            DerivativeAnchors::Affine => StateDefinition::DerivativeExpression(rhs.index()),
        },
        rhs_sign,
        owner,
    })
}

pub(super) fn exact_state_anchor<'dae>(
    view: dae::DaeView<'dae>,
    equalities: &SystemEqualities,
    expression: dae::ExprId<'dae>,
) -> Option<(dae::StateId<'dae>, EqualitySign)> {
    state_anchor(view, equalities, expression, DerivativeAnchors::Exact)
}

fn state_anchor<'dae>(
    view: dae::DaeView<'dae>,
    equalities: &SystemEqualities,
    expression: dae::ExprId<'dae>,
    anchors: DerivativeAnchors,
) -> Option<(dae::StateId<'dae>, EqualitySign)> {
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) => {
            Some((state, EqualitySign::Same))
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) => {
            let (EqualityAnchor::State(state), sign) =
                equalities.derivative_anchor(algebraic.index(), anchors)?
            else {
                return None;
            };
            let dae::VariableIdentity::State(state) =
                view.variable(view.variable_id(state as usize)?)?.identity()
            else {
                return None;
            };
            Some((state, sign))
        }
        _ => None,
    }
}

/// Whether the definition closure of `root` names `der(demoted)`.
///
/// Reconstruction replaces `der(demoted)` by the derivative of this very
/// expression, so a definition it reaches that names `der(demoted)` again would
/// make the substitution refer to itself. The closure follows exactly the
/// indirections differentiation follows — state and derivative definitions — so
/// a candidate that clears this check cannot re-enter its own substitution.
fn reaches_demoted_derivative<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    root: dae::ExprId<'dae>,
    demoted: dae::StateId<'dae>,
) -> bool {
    let mut pending = vec![root];
    let mut expanded = vec![false; view.expression_count()];
    while let Some(root) = pending.pop() {
        if std::mem::replace(&mut expanded[root.index() as usize], true) {
            continue;
        }
        let mut found = false;
        let mut definitions = Vec::new();
        dae::for_each_expression(view, root, |_, expression| {
            let dae::ExpressionOperation::Coordinate(coordinate) = expression.operation() else {
                return;
            };
            let state = match coordinate {
                dae::CoordinateView::Derivative(state) if state == demoted => {
                    found = true;
                    return;
                }
                dae::CoordinateView::State(state) | dae::CoordinateView::Derivative(state) => state,
                _ => return,
            };
            definitions
                .extend(facts.derivative_definitions[state.index() as usize].map(|d| d.expression));
        });
        if found {
            return true;
        }
        pending.extend(
            definitions
                .into_iter()
                .filter_map(|definition| view.expression_id(definition as usize)),
        );
    }
    false
}

#[cfg(test)]
pub(super) fn holonomic_constraints(view: dae::DaeView<'_>) -> Vec<HolonomicConstraint> {
    index_reduction_constraints(view, &DifferentiationFacts::collect(view))
        .into_iter()
        .filter(|constraint| constraint.lifted_algebraic.is_none())
        .collect()
}

pub(super) fn index_reduction_constraints(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
) -> Vec<HolonomicConstraint> {
    let causal = CausalDefinitions::derive(view);
    let mut scratch = HolonomicProofScratch::new(view.expression_count());
    view.continuous_owners()
        .enumerate()
        .flat_map(|(owner_ordinal, owner)| {
            let (owner, residuals): (_, Box<dyn Iterator<Item = _>>) = match owner {
                dae::ContinuousOwnerView::Residual { equation, .. } => (
                    equation.provenance(),
                    Box::new(std::iter::once((None, equation.residual()))),
                ),
                dae::ContinuousOwnerView::Structured { family, .. }
                    if family.scalar_view()
                        == rumoca_core::ComprehensionScalarView::RowMajorProjection =>
                {
                    (
                        family.provenance(),
                        Box::new(
                            family
                                .bodies()
                                .iter()
                                .enumerate()
                                .map(|(body, residual)| (Some(body), residual)),
                        ),
                    )
                }
                dae::ContinuousOwnerView::Structured { .. } => {
                    return Vec::new().into_iter();
                }
            };
            residuals
                .flat_map(|(body_ordinal, residual)| {
                    let ordinary =
                        holonomic_differentiation_proofs(view, facts, &mut scratch, residual)
                            .into_iter()
                            .map(move |proof| HolonomicConstraint {
                                owner_ordinal,
                                body_ordinal,
                                residual: residual.index(),
                                owner,
                                proof,
                                lifted_algebraic: None,
                            });
                    let lifted = causal_definition(view, &causal, residual).and_then(
                        |(algebraic, definition)| {
                            let proof = prove_algebraic_lift_differentiation(
                                view,
                                facts,
                                &mut scratch,
                                definition,
                                (residual.index(), algebraic.index()),
                            );
                            proof.map(|proof| HolonomicConstraint {
                                owner_ordinal,
                                body_ordinal,
                                residual: residual.index(),
                                owner,
                                proof,
                                lifted_algebraic: Some(algebraic.index()),
                            })
                        },
                    );
                    ordinary.chain(lifted)
                })
                .collect::<Vec<_>>()
                .into_iter()
        })
        .collect()
}

/// Prove that one unstructured residual is a state-manifold constraint whose
/// structurally useful derivative reconstruction can form exactly.
///
/// Algebraic coordinates are admitted only through the signed equality class
/// already asserted by the source system. A pinned class differentiates to
/// zero; a state-anchored class differentiates as that state. Source value
/// identities and self-materialized definitions are excluded before this walk.
/// An independent constraint can depend on one scalar or tensor state through
/// an algebraic observation; declaration counts do not establish independence.
fn holonomic_differentiation_proofs<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    scratch: &mut HolonomicProofScratch,
    residual: dae::ExprId<'dae>,
) -> Vec<HolonomicDifferentiationProof> {
    if is_materialized_definition(view, facts, residual) {
        return Vec::new();
    }
    if let Some(proof) = prove_holonomic_differentiation(view, facts, scratch, residual, None) {
        return vec![proof];
    }
    let count = view
        .expression(residual)
        .and_then(|expression| expression.value_type().scalar_count())
        .unwrap_or(0);
    (0..count)
        .filter_map(|scalar| {
            let component = ComponentConstraint::derive(view, facts, residual, scalar)?;
            prove_holonomic_differentiation(view, facts, scratch, residual, Some(component))
        })
        .collect()
}

/// The value proof would substitute this very equation's RHS for its target,
/// so differentiating the residual yields an identity and loses its owner.
fn is_materialized_definition<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    residual: dae::ExprId<'dae>,
) -> bool {
    if value_identity::materializes_to_zero(view, facts, residual) {
        return true;
    }
    let Some((lhs, rhs)) = equation_sides(view, residual) else {
        return false;
    };
    [(lhs, rhs), (rhs, lhs)].into_iter().any(|(target, value)| {
        let Some(node) = view.expression(target) else {
            return false;
        };
        let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) =
            node.operation()
        else {
            return false;
        };
        facts
            .equalities
            .value_anchor_of(algebraic.index())
            .is_none()
            && facts.algebraic_definition(view, algebraic) == Some(value)
    })
}

fn prove_holonomic_differentiation<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    scratch: &mut HolonomicProofScratch,
    residual: dae::ExprId<'dae>,
    component: Option<ComponentConstraint>,
) -> Option<HolonomicDifferentiationProof> {
    scratch.begin_root();
    let mut walk = HolonomicProofWalk {
        view,
        facts,
        excluded_residual: residual.index(),
        derivative_anchors: DerivativeAnchors::Affine,
        anchored_states: Vec::new(),
        function_context: FunctionCallContext::default(),
        scratch,
    };
    let leaves = component
        .as_ref()
        .map_or_else(|| vec![residual.index()], ComponentConstraint::leaves);
    let leaves = leaves
        .into_iter()
        .map(|index| view.expression_id(index as usize).unwrap())
        .collect::<Vec<_>>();
    if !leaves
        .iter()
        .all(|&leaf| walk.can_differentiate_order(leaf, 1, true))
    {
        return None;
    }
    let mut value_visited = vec![Visit::Pending; view.expression_count()];
    if !leaves
        .iter()
        .all(|&leaf| can_materialize_holonomic_value(view, facts, leaf, &mut value_visited))
    {
        return None;
    }
    let second_order = leaves
        .iter()
        .all(|&leaf| walk.can_differentiate_order(leaf, 2, true));
    let maximum_order = if second_order {
        let mut derivative_visited = vec![Visit::Pending; view.expression_count()];
        let mut derivative_states = vec![Visit::Pending; view.variable_count()];
        if leaves.iter().all(|&leaf| {
            has_state_only_first_derivative(
                view,
                facts,
                leaf,
                &mut derivative_visited,
                &mut derivative_states,
                &mut value_visited,
            )
        }) {
            2
        } else {
            1
        }
    } else {
        1
    };
    walk.anchored_states.sort_unstable();
    walk.anchored_states.dedup();
    if walk.anchored_states.is_empty() {
        return None;
    }
    Some(HolonomicDifferentiationProof {
        residual: residual.index(),
        maximum_order,
        derivative_anchors: walk.derivative_anchors,
        anchored_states: walk.anchored_states.into_boxed_slice(),
        component,
        lifted_value: None,
    })
}

fn causal_definition<'dae>(
    view: dae::DaeView<'dae>,
    causal: &CausalDefinitions<'dae>,
    residual: dae::ExprId<'dae>,
) -> Option<(dae::AlgebraicId<'dae>, dae::ExprId<'dae>)> {
    let (lhs, rhs) = equation_sides(view, residual)?;
    [(lhs, rhs), (rhs, lhs)]
        .into_iter()
        .find_map(|(target, value)| {
            let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) =
                view.expression(target)?.operation()
            else {
                return None;
            };
            let variable = view.variable(algebraic.into())?;
            if variable.role() != dae::VariableRole::Algebraic
                || variable.variability() != dae::ExpressionVariability::Continuous
                || variable.value_type().scalar_type() != dae::ScalarType::Real
                || variable.fixed_any_true()
                || causal.event_holds_variable(algebraic.into())
            {
                return None;
            }
            (causal.definition(algebraic) == Some(value)).then_some((algebraic, value))
        })
}

fn prove_algebraic_lift_differentiation<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    scratch: &mut HolonomicProofScratch,
    definition: dae::ExprId<'dae>,
    lifted: (u32, u32),
) -> Option<HolonomicDifferentiationProof> {
    let value = if super::equalities::additive_operands(view, definition).is_some() {
        Some(std::sync::Arc::new(
            facts
                .additive_values
                .prove(view, lifted.0, lifted.1, definition)?,
        ))
    } else {
        None
    };
    scratch.begin_root();
    let mut walk = HolonomicProofWalk {
        view,
        facts,
        excluded_residual: lifted.0,
        derivative_anchors: DerivativeAnchors::Exact,
        anchored_states: Vec::new(),
        function_context: FunctionCallContext::default(),
        scratch,
    };
    let sources = value.as_ref().map_or_else(
        || vec![definition.index()],
        |value| value.sources().collect(),
    );
    if !sources.iter().all(|&source| {
        walk.can_differentiate_order(view.expression_id(source as usize).unwrap(), 1, true)
    }) {
        return None;
    }
    let mut value_visited = vec![Visit::Pending; view.expression_count()];
    if !sources.iter().all(|&source| {
        can_materialize_holonomic_value(
            view,
            facts,
            view.expression_id(source as usize).unwrap(),
            &mut value_visited,
        )
    }) {
        return None;
    }
    walk.anchored_states.sort_unstable();
    walk.anchored_states.dedup();
    if walk.anchored_states.is_empty() {
        return None;
    }
    Some(HolonomicDifferentiationProof {
        residual: definition.index(),
        maximum_order: 1,
        derivative_anchors: walk.derivative_anchors,
        anchored_states: walk.anchored_states.into_boxed_slice(),
        component: None,
        lifted_value: value,
    })
}

struct HolonomicProofWalk<'facts, 'dae> {
    view: dae::DaeView<'dae>,
    facts: &'facts DifferentiationFacts,
    excluded_residual: u32,
    derivative_anchors: DerivativeAnchors,
    anchored_states: Vec<u32>,
    function_context: FunctionCallContext<'dae>,
    scratch: &'facts mut HolonomicProofScratch,
}

impl<'facts, 'dae> HolonomicProofWalk<'facts, 'dae> {
    fn can_apply_derivative(
        &mut self,
        selected: super::function_derivatives::SelectedFunctionDerivative<'dae>,
        on_residual: bool,
    ) -> bool {
        let mut value_visited = vec![Visit::Pending; self.view.expression_count()];
        selected.arguments.iter().all(|argument| {
            if argument.order == 0 {
                can_materialize_holonomic_value_in_context(
                    self.view,
                    self.facts,
                    argument.source,
                    &mut value_visited,
                    &self.function_context,
                    &mut Vec::new(),
                )
            } else {
                self.can_differentiate_order(argument.source, argument.order, on_residual)
            }
        })
    }

    fn can_differentiate_order(
        &mut self,
        expression: dae::ExprId<'dae>,
        order: u8,
        on_residual: bool,
    ) -> bool {
        let scoped_context = self
            .function_context
            .scoped_to_expression(self.view, expression);
        let previous_context = std::mem::replace(&mut self.function_context, scoped_context);
        let differentiable = self.can_differentiate_order_scoped(expression, order, on_residual);
        self.function_context = previous_context;
        differentiable
    }

    fn can_differentiate_order_scoped(
        &mut self,
        expression: dae::ExprId<'dae>,
        order: u8,
        on_residual: bool,
    ) -> bool {
        if let Some(branch) = self.function_context.selected_branch(self.view, expression) {
            return self.can_differentiate_order(branch, order, on_residual);
        }
        if let Some(element) = projected_element(self.view, self.facts, expression) {
            return self.can_differentiate_order(element, order, on_residual);
        }
        let index = expression.index() as usize;
        let context = usize::from(on_residual);
        if let Some(selected) = super::function_derivatives::select_derivative(
            self.view,
            &self.function_context,
            expression,
            order,
        ) {
            return self
                .facts
                .expression_is_zero(self.view, expression, &self.function_context)
                || self.can_apply_derivative(selected, on_residual);
        }
        if self.function_context.is_empty() {
            match self.scratch.state(index, order as usize, context) {
                Visit::Differentiable => return true,
                Visit::InProgress => return false,
                Visit::Pending => {
                    self.scratch
                        .set_state(index, order as usize, context, Visit::InProgress);
                }
            }
        }
        if let Some((result, nested)) = self.function_context.call_result(self.view, expression) {
            let previous = std::mem::replace(&mut self.function_context, nested);
            let differentiable = self.can_differentiate_order(result, order, on_residual);
            self.function_context = previous;
            self.cache_differentiability(index, order, context, differentiable);
            return differentiable;
        }
        if self
            .facts
            .expression_is_zero(self.view, expression, &self.function_context)
        {
            self.cache_differentiability(index, order, context, true);
            return true;
        }
        if let Some(argument) = forwarded_call_argument(self.view, expression) {
            let differentiable = self.can_differentiate_order(argument, order, on_residual);
            self.cache_differentiability(index, order, context, differentiable);
            return differentiable;
        }
        let expression_id = expression;
        let expression = self
            .view
            .expression(expression)
            .expect("checked differentiability expression resolves");
        if self.function_context.is_empty() && is_time_invariant(self.view, expression_id) {
            self.cache_differentiability(index, order, context, true);
            return true;
        }
        let differentiable = self.can_differentiate_operation(expression, order, on_residual);
        self.cache_differentiability(index, order, context, differentiable);
        differentiable
    }

    fn can_differentiate_operation(
        &mut self,
        expression: dae::ExpressionView<'dae>,
        order: u8,
        on_residual: bool,
    ) -> bool {
        match expression.operation() {
            dae::ExpressionOperation::Literal(_) => true,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self
                .function_context
                .parameter_argument(parameter)
                .is_some_and(|argument| self.can_differentiate_order(argument, order, on_residual)),
            dae::ExpressionOperation::Coordinate(coordinate) => {
                self.can_differentiate_coordinate(coordinate, order, on_residual)
            }
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => self.can_differentiate_order(operand, order, on_residual),
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                is_differentiable_binary(operator)
                    && self.can_differentiate_order(lhs, order, on_residual)
                    && self.can_differentiate_order(rhs, order, on_residual)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                has_invariant_subscripts(self.view, subscripts)
                    && self.can_differentiate_order(base, order, on_residual)
            }
            dae::ExpressionOperation::Builtin { builtin, arguments }
                if is_differentiable_builtin(builtin, order) =>
            {
                arguments
                    .iter()
                    .all(|argument| self.can_differentiate_order(argument, order, on_residual))
            }
            dae::ExpressionOperation::Array(elements) => elements
                .iter()
                .all(|element| self.can_differentiate_order(element, order, on_residual)),
            dae::ExpressionOperation::Conditional(operands) => {
                super::parameter_conditionals::has_parameter_guards(
                    self.view,
                    &self.function_context,
                    operands,
                ) && super::parameter_conditionals::values(operands)
                    .all(|value| self.can_differentiate_order(value, order, on_residual))
            }
            dae::ExpressionOperation::Field { base, field } => self
                .function_context
                .projected_field(self.view, base, field)
                .is_some_and(|(projected, projected_context)| {
                    let previous = std::mem::replace(&mut self.function_context, projected_context);
                    let differentiable =
                        self.can_differentiate_order(projected, order, on_residual);
                    self.function_context = previous;
                    differentiable
                }),
            _ => false,
        }
    }

    fn cache_differentiability(
        &mut self,
        expression: usize,
        order: u8,
        context: usize,
        differentiable: bool,
    ) {
        if self.function_context.is_empty() {
            self.scratch.set_state(
                expression,
                order as usize,
                context,
                if differentiable {
                    Visit::Differentiable
                } else {
                    Visit::Pending
                },
            );
        }
    }

    fn can_differentiate_coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        order: u8,
        on_residual: bool,
    ) -> bool {
        match coordinate {
            dae::CoordinateView::Parameter(_) | dae::CoordinateView::Time => true,
            dae::CoordinateView::State(state) => {
                self.can_differentiate_state(state.index(), order, on_residual)
            }
            dae::CoordinateView::Derivative(state) => self.facts.derivative_definitions
                [state.index() as usize]
                .is_some_and(|definition| {
                    definition.residual != self.excluded_residual
                        && self.can_differentiate_order(
                            self.view
                                .expression_id(definition.expression as usize)
                                .unwrap(),
                            order,
                            on_residual,
                        )
                }),
            dae::CoordinateView::Algebraic(algebraic) => {
                if let Some(block) = self.facts.auxiliary_blocks[algebraic.index() as usize].clone()
                {
                    return self.can_differentiate_auxiliary(&block, order, on_residual);
                }
                if let Some(definition) =
                    self.facts.component_definitions[algebraic.index() as usize].clone()
                {
                    return self.can_differentiate_component_definition(
                        &definition,
                        order,
                        on_residual,
                    );
                }
                match self
                    .facts
                    .equalities
                    .derivative_anchor(algebraic.index(), self.derivative_anchors)
                {
                    Some((EqualityAnchor::Invariant { .. }, _)) => true,
                    Some((anchor @ EqualityAnchor::State(state), _)) => {
                        self.can_differentiate_equality_anchor(anchor, state, order, on_residual)
                    }
                    None => self
                        .facts
                        .algebraic_definition(self.view, algebraic)
                        .is_some_and(|definition| {
                            self.can_differentiate_order(definition, order, on_residual)
                        }),
                }
            }
            _ => false,
        }
    }

    fn can_differentiate_component_definition(
        &mut self,
        definition: &ComponentConstraint,
        order: u8,
        on_residual: bool,
    ) -> bool {
        definition.leaves().into_iter().all(|leaf| {
            self.can_differentiate_order(
                self.view.expression_id(leaf as usize).unwrap(),
                order,
                on_residual,
            )
        })
    }

    fn can_differentiate_auxiliary(
        &mut self,
        block: &super::auxiliary_blocks::AuxiliaryBlock,
        order: u8,
        on_residual: bool,
    ) -> bool {
        if block.contains_residual(self.excluded_residual) {
            return false;
        }
        if on_residual {
            self.anchored_states.extend_from_slice(&block.state_anchors);
        }
        block.operands().all(|operand| {
            let previous =
                std::mem::replace(&mut self.function_context, operand.context(self.view));
            let result = self.can_differentiate_order(
                self.view
                    .expression_id(operand.expression as usize)
                    .unwrap(),
                order,
                false,
            );
            self.function_context = previous;
            result
        })
    }

    fn can_differentiate_equality_anchor(
        &mut self,
        anchor: EqualityAnchor,
        state: u32,
        order: u8,
        on_residual: bool,
    ) -> bool {
        let Some(expression) = self
            .facts
            .equalities
            .anchor_expression(
                self.view,
                anchor,
                self.view
                    .variable(self.view.variable_id(state as usize).unwrap())
                    .unwrap()
                    .value_type(),
            )
            .and_then(|expression| self.view.expression_id(expression as usize))
            .and_then(|expression| self.view.expression(expression))
        else {
            return false;
        };
        matches!(
            expression.operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(candidate))
                if candidate.index() == state
        ) && self.can_differentiate_state(state, order, on_residual)
    }

    fn can_differentiate_state(&mut self, state: u32, order: u8, on_residual: bool) -> bool {
        if on_residual {
            self.anchored_states.push(state);
        }
        order == 1
            || self.facts.derivative_definitions[state as usize].is_some_and(|definition| {
                self.view
                    .expression_id(definition.expression as usize)
                    .is_some_and(|definition| {
                        self.can_differentiate_order(definition, order - 1, false)
                    })
            })
    }
}

/// Whether reconstruction can differentiate `expression` exactly.
///
/// Mirrors the differentiation walk one for one, including the two
/// indirections it is allowed to follow: an algebraic coordinate resolved to
/// its equality anchor, and a derivative coordinate resolved to its explicit
/// definition. Re-entering an expression that is still being walked is a cycle
/// the differentiator could not terminate on, so it is rejected outright.
fn is_differentiable<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    expression: dae::ExprId<'dae>,
    demoted: dae::StateId<'dae>,
    visited: &mut [Visit],
) -> bool {
    is_differentiable_in_context(
        view,
        facts,
        expression,
        demoted,
        visited,
        &FunctionCallContext::default(),
    )
}

fn is_differentiable_in_context<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    expression: dae::ExprId<'dae>,
    demoted: dae::StateId<'dae>,
    visited: &mut [Visit],
    context: &FunctionCallContext<'dae>,
) -> bool {
    let scoped_context = context.scoped_to_expression(view, expression);
    let context = &scoped_context;
    if let Some(branch) = context.selected_branch(view, expression) {
        return is_differentiable_in_context(view, facts, branch, demoted, visited, context);
    }
    if let Some(element) = projected_element(view, facts, expression) {
        return is_differentiable_in_context(view, facts, element, demoted, visited, context);
    }
    let index = expression.index() as usize;
    if let Some(selected) =
        super::function_derivatives::select_derivative(view, context, expression, 1)
    {
        return selected_derivative_is_differentiable(
            view, facts, selected, demoted, visited, context,
        );
    }
    if context.is_empty() {
        match visited[index] {
            Visit::Differentiable => return true,
            Visit::InProgress => return false,
            Visit::Pending => visited[index] = Visit::InProgress,
        }
    }
    if let Some((result, nested)) = context.call_result(view, expression) {
        let differentiable =
            is_differentiable_in_context(view, facts, result, demoted, visited, &nested);
        if context.is_empty() {
            visited[index] = if differentiable {
                Visit::Differentiable
            } else {
                Visit::Pending
            };
        }
        return differentiable;
    }
    if facts.expression_is_zero(view, expression, context) {
        if context.is_empty() {
            visited[index] = Visit::Differentiable;
        }
        return true;
    }
    let Some(node) = view.expression(expression) else {
        return false;
    };
    if context.is_empty() && is_time_invariant(view, expression) {
        visited[index] = Visit::Differentiable;
        return true;
    }
    let differentiable =
        operation_is_differentiable(view, facts, node.operation(), demoted, visited, context);
    if context.is_empty() {
        visited[index] = if differentiable {
            Visit::Differentiable
        } else {
            Visit::Pending
        };
    }
    differentiable
}

fn operation_is_differentiable<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    operation: dae::ExpressionOperation<'dae>,
    demoted: dae::StateId<'dae>,
    visited: &mut [Visit],
    context: &FunctionCallContext<'dae>,
) -> bool {
    match operation {
        dae::ExpressionOperation::Literal(_) => true,
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(parameter)) => {
            context
                .parameter_argument(parameter)
                .is_some_and(|argument| {
                    is_differentiable_in_context(view, facts, argument, demoted, visited, context)
                })
        }
        dae::ExpressionOperation::Coordinate(coordinate) => {
            is_differentiable_coordinate(view, facts, coordinate, demoted, visited)
        }
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => is_differentiable_in_context(view, facts, operand, demoted, visited, context),
        dae::ExpressionOperation::Binary { operator, lhs, rhs }
            if is_differentiable_binary(operator) =>
        {
            is_differentiable_in_context(view, facts, lhs, demoted, visited, context)
                && is_differentiable_in_context(view, facts, rhs, demoted, visited, context)
        }
        dae::ExpressionOperation::Builtin { builtin, arguments } => {
            builtin_is_differentiable(view, facts, builtin, arguments, demoted, visited, context)
        }
        dae::ExpressionOperation::Array(elements) => elements.iter().all(|element| {
            is_differentiable_in_context(view, facts, element, demoted, visited, context)
        }),
        dae::ExpressionOperation::Conditional(operands) => {
            super::parameter_conditionals::has_parameter_guards(view, context, operands)
                && super::parameter_conditionals::values(operands).all(|value| {
                    is_differentiable_in_context(view, facts, value, demoted, visited, context)
                })
        }
        dae::ExpressionOperation::Field { base, field } => context
            .projected_field(view, base, field)
            .is_some_and(|(projected, projected_context)| {
                is_differentiable_in_context(
                    view,
                    facts,
                    projected,
                    demoted,
                    visited,
                    &projected_context,
                )
            }),
        dae::ExpressionOperation::Index { base, subscripts } => {
            has_invariant_subscripts(view, subscripts)
                && is_differentiable_in_context(view, facts, base, demoted, visited, context)
        }
        _ => false,
    }
}

fn selected_derivative_is_differentiable<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    selected: super::function_derivatives::SelectedFunctionDerivative<'dae>,
    demoted: dae::StateId<'dae>,
    visited: &mut [Visit],
    context: &FunctionCallContext<'dae>,
) -> bool {
    let mut value_visited = vec![Visit::Pending; view.expression_count()];
    selected
        .arguments
        .iter()
        .all(|argument| match argument.order {
            0 => can_materialize_holonomic_value_in_context(
                view,
                facts,
                argument.source,
                &mut value_visited,
                context,
                &mut Vec::new(),
            ),
            1 => is_differentiable_in_context(
                view,
                facts,
                argument.source,
                demoted,
                visited,
                context,
            ),
            _ => false,
        })
}

/// Admit exactly the pure builtins whose first derivative has a closed DAE
/// representation. The checked DAE constructor already proves builtin arity
/// and tensor shapes; this walk proves only that each value operand can itself
/// be differentiated under the current function-call substitution.
fn builtin_is_differentiable<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    builtin: dae::PureBuiltin,
    arguments: dae::ExpressionOperands<'dae>,
    demoted: dae::StateId<'dae>,
    visited: &mut [Visit],
    context: &FunctionCallContext<'dae>,
) -> bool {
    is_differentiable_builtin(builtin, 1)
        && arguments.iter().all(|argument| {
            is_differentiable_in_context(view, facts, argument, demoted, visited, context)
        })
}

fn is_differentiable_coordinate<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    coordinate: dae::CoordinateView<'dae>,
    demoted: dae::StateId<'dae>,
    visited: &mut [Visit],
) -> bool {
    match coordinate {
        dae::CoordinateView::Parameter(_) | dae::CoordinateView::Time => true,
        dae::CoordinateView::State(state) => state != demoted,
        dae::CoordinateView::Algebraic(algebraic) => {
            if let Some(block) = &facts.auxiliary_blocks[algebraic.index() as usize] {
                return auxiliary_is_differentiable(view, facts, block, demoted, visited);
            }
            if let Some(definition) = &facts.component_definitions[algebraic.index() as usize] {
                return definition.leaves().into_iter().all(|leaf| {
                    is_differentiable(
                        view,
                        facts,
                        view.expression_id(leaf as usize).unwrap(),
                        demoted,
                        visited,
                    )
                });
            }
            match facts
                .equalities
                .anchor_for_demotion(algebraic.index(), demoted.index())
            {
                Some((EqualityAnchor::Invariant { .. }, _)) => true,
                Some((anchor @ EqualityAnchor::State(_), _)) => facts
                    .equalities
                    .payload_anchor_expression(anchor)
                    .and_then(|anchor| view.expression_id(anchor as usize))
                    .is_some_and(|anchor| is_differentiable(view, facts, anchor, demoted, visited)),
                None => facts
                    .algebraic_definition(view, algebraic)
                    .is_some_and(|definition| {
                        is_differentiable(view, facts, definition, demoted, visited)
                    }),
            }
        }
        dae::CoordinateView::Derivative(state) => {
            state != demoted
                && facts.derivative_definitions[state.index() as usize].is_some_and(|definition| {
                    view.expression_id(definition.expression as usize)
                        .is_some_and(|definition| {
                            is_differentiable(view, facts, definition, demoted, visited)
                        })
                })
        }
        _ => false,
    }
}

/// Residuals that state one state derivative outright, in either orientation.
///
/// MSL writes both `der(phi) = w` and the equally common `w = der(phi)`, so a
/// detector that reads only the first form loses `d/dt phi` on most mechanical
/// components. Two residuals defining the same derivative leave it undefined
/// here rather than picking one arbitrarily.
pub(super) fn explicit_derivative_definitions(
    view: dae::DaeView<'_>,
) -> Vec<Option<ExplicitDerivativeDefinition>> {
    let mut definitions = vec![None; view.variable_count()];
    let mut duplicate = vec![false; view.variable_count()];
    for owner in view.continuous_owners() {
        let residuals: Box<dyn Iterator<Item = _>> = match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                Box::new(std::iter::once(equation.residual()))
            }
            dae::ContinuousOwnerView::Structured { family, .. }
                if family.scalar_view()
                    == rumoca_core::ComprehensionScalarView::RowMajorProjection =>
            {
                Box::new(family.bodies().iter())
            }
            dae::ContinuousOwnerView::Structured { .. } => continue,
        };
        for residual in residuals {
            let Some((lhs, rhs)) = equation_sides(view, residual) else {
                continue;
            };
            let Some((state, definition)) = derivative_definition(view, lhs, rhs) else {
                continue;
            };
            let index = state as usize;
            let definition = ExplicitDerivativeDefinition {
                residual: residual.index(),
                expression: definition,
            };
            if definitions[index].replace(definition).is_some() {
                duplicate[index] = true;
            }
        }
    }
    for (definition, duplicate) in definitions.iter_mut().zip(duplicate) {
        if duplicate {
            *definition = None;
        }
    }
    definitions
}

/// The state whose derivative one side of `a - b` names, and the other side.
fn derivative_definition<'dae>(
    view: dae::DaeView<'dae>,
    lhs: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
) -> Option<(u32, u32)> {
    let derivative = |side: dae::ExprId<'dae>| {
        let expression = view.expression(side)?;
        if expression.function_scope().is_some() || expression.binder_domain().is_some() {
            return None;
        }
        let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) =
            expression.operation()
        else {
            return None;
        };
        Some(state.index())
    };
    match (derivative(lhs), derivative(rhs)) {
        (Some(state), None) => Some((state, rhs.index())),
        (None, Some(state)) => Some((state, lhs.index())),
        _ => None,
    }
}

#[cfg(test)]
mod proof_scratch_tests {
    use super::*;

    #[test]
    fn proof_scratch_reuses_one_expression_sized_table_across_many_roots() {
        const EXPRESSION_COUNT: usize = 16_384;
        const ROOT_COUNT: usize = 4_096;
        let mut scratch = HolonomicProofScratch::new(EXPRESSION_COUNT);
        let storage = scratch.visited.as_ptr();

        for root in 0..ROOT_COUNT {
            scratch.begin_root();
            let expression = root % EXPRESSION_COUNT;
            scratch.set_state(expression, 2, 1, Visit::Differentiable);
            assert_eq!(scratch.state(expression, 2, 1), Visit::Differentiable);
            if expression > 0 {
                assert_eq!(scratch.state(expression - 1, 2, 1), Visit::Pending);
            }
        }

        assert_eq!(scratch.visited.len(), EXPRESSION_COUNT);
        assert_eq!(scratch.visited.as_ptr(), storage);
    }
}
