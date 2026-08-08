//! Coordinate equalities the source system already asserts.
//!
//! A connected MSL model states most of its structure as bare additive
//! balances: every `connect` between two rotational flanges lowers to a
//! potential residual `a.phi - b.phi`, every two-terminal node lowers to a flow
//! residual `a.i + b.i`, a component body adds `phi - flange_a.phi`, and an
//! unused support adds `phi_support - 0`. Read literally, a chain like that
//! hides the fact that two *states* are the same quantity behind a run of
//! connector algebraics, so a candidate detector that only inspects one
//! residual at a time sees nothing to reduce.
//!
//! [`SystemEqualities`] closes those chains exactly. Each accepted residual is
//! an unconditional continuous equation, so `a - b = 0` proves `a ≡ b` and
//! `a + b = 0` proves `a ≡ -b` for all time; the transitive closure of those
//! signed edges is equally exact. Nothing here inspects a name: membership
//! comes from branded coordinate identities, and the class anchor is picked by
//! variable role and `StateSelect`.
//!
//! A rigid body writes its ends as `flange_a.s - (s - L/2)`, which states the
//! same equality displaced by a time-invariant length. That displacement is
//! exact for a derivative — `d/dt (x + c) = d/dt x` whenever `c` is constant —
//! and inexact for a value, so the two facts are closed separately. The
//! `exact` layer carries only offset-free edges and answers value questions;
//! the `affine` layer additionally carries displaced edges and answers
//! derivative questions. Every reader picks the layer its claim needs, so an
//! offset can never leak into a substitution that names a value.

use rumoca_core::StateSelect;
use rumoca_ir_dae as dae;

/// The class member whose time derivative is known.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum EqualityAnchor {
    /// A time-invariant value the class is pinned to: every member of the class
    /// has derivative zero. `value` names the source expression every member
    /// equals when the pinning residual states that value directly, and is
    /// `None` when the residual only proves the class constant. `ordinal` is
    /// the pinning residual, which keeps anchor selection deterministic.
    Invariant { value: Option<u32>, ordinal: u32 },
    /// The state the class keeps; every other state in the class is redundant.
    State(u32),
}

/// How a class member relates to its anchor: `Same` for `x ≡ anchor`,
/// `Opposite` for `x ≡ -anchor`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum EqualitySign {
    Same,
    Opposite,
}

impl EqualitySign {
    const fn of(parity: bool) -> Self {
        if parity { Self::Opposite } else { Self::Same }
    }
}

/// Signed coordinate classes closed transitively over one set of edges.
struct SignedClasses {
    /// Union-find parent per variable ordinal.
    parent: Vec<u32>,
    /// Union-by-rank bound on parent depth; ties keep the lower root. Variable
    /// identities are `u32`, so the rank cannot exceed 32.
    rank: Vec<u8>,
    /// Sign of each variable relative to its union-find parent.
    parity: Vec<bool>,
    /// Anchor of the class rooted at each variable ordinal.
    anchor: Vec<Option<EqualityAnchor>>,
    /// Classes whose edges contradict each other and prove nothing usable.
    inconsistent: Vec<bool>,
}

impl SignedClasses {
    fn new(count: usize) -> Self {
        Self {
            parent: (0..count as u32).collect(),
            rank: vec![0; count],
            parity: vec![false; count],
            anchor: vec![None; count],
            inconsistent: vec![false; count],
        }
    }

    /// The class root of `variable` and its sign relative to that root.
    fn find(&self, variable: u32) -> (u32, bool) {
        let mut current = variable;
        let mut parity = false;
        while self.parent[current as usize] != current {
            parity ^= self.parity[current as usize];
            current = self.parent[current as usize];
        }
        (current, parity)
    }

    fn union(&mut self, left: u32, right: u32, opposite: bool) {
        let (left, left_parity) = self.find(left);
        let (right, right_parity) = self.find(right);
        // Sign of `left` relative to `right` once both are lifted to their roots.
        let relative = left_parity ^ right_parity ^ opposite;
        if left == right {
            if relative {
                self.inconsistent[left as usize] = true;
            }
            return;
        }
        let left_rank = self.rank[left as usize];
        let right_rank = self.rank[right as usize];
        let (keep, merged, raise_rank) = match left_rank.cmp(&right_rank) {
            std::cmp::Ordering::Greater => (left, right, false),
            std::cmp::Ordering::Less => (right, left, false),
            std::cmp::Ordering::Equal if left < right => (left, right, true),
            std::cmp::Ordering::Equal => (right, left, true),
        };
        self.parent[merged as usize] = keep;
        self.parity[merged as usize] = relative;
        self.inconsistent[keep as usize] |= self.inconsistent[merged as usize];
        if raise_rank {
            self.rank[keep as usize] = self.rank[keep as usize].saturating_add(1);
        }
    }

    /// The class member whose derivative is known, and how `variable` signs
    /// against it, if the class has such a member.
    ///
    /// A pinned class always reports [`EqualitySign::Same`]: a time-invariant
    /// value has the same zero derivative under either sign, so the sign
    /// carries no information a caller could act on.
    fn anchor_of(&self, variable: u32) -> Option<(EqualityAnchor, EqualitySign)> {
        let (root, parity) = self.find(variable);
        if self.inconsistent[root as usize] {
            return None;
        }
        match self.anchor[root as usize]? {
            anchor @ EqualityAnchor::Invariant { .. } => Some((anchor, EqualitySign::Same)),
            anchor @ EqualityAnchor::State(state) => {
                let (_, anchor_parity) = self.find(state);
                Some((anchor, EqualitySign::of(parity != anchor_parity)))
            }
        }
    }

    /// Keep `candidate` as the anchor of the class holding `variable` when it
    /// outranks whatever that class already reports.
    fn offer_anchor(&mut self, view: dae::DaeView<'_>, variable: u32, candidate: EqualityAnchor) {
        let (root, _) = self.find(variable);
        let replace = match self.anchor[root as usize] {
            None => true,
            Some(current) => {
                (anchor_rank(view, candidate), anchor_ordinal(candidate))
                    > (anchor_rank(view, current), anchor_ordinal(current))
            }
        };
        if replace {
            self.anchor[root as usize] = Some(candidate);
        }
    }

    #[cfg(test)]
    fn maximum_depth(&self) -> usize {
        (0..self.parent.len() as u32)
            .map(|variable| {
                let mut current = variable;
                let mut depth = 0;
                while self.parent[current as usize] != current {
                    current = self.parent[current as usize];
                    depth += 1;
                }
                depth
            })
            .max()
            .unwrap_or(0)
    }
}

/// Exact coordinate equalities, closed transitively over one finalized DAE.
pub(super) struct SystemEqualities {
    /// Classes proven equal up to sign, with no displacement between members.
    /// Answers every question about a value.
    exact: SignedClasses,
    /// Classes proven equal up to sign and a time-invariant displacement.
    /// Members share a derivative but not necessarily a value.
    affine: SignedClasses,
    /// Whether each variable ordinal is declared a continuous state.
    state: Vec<bool>,
    /// Lowest whole-model coordinate expression ordinal naming each variable.
    coordinate: Vec<Option<u32>>,
    /// One asserted equality incident to each variable, for candidate owners.
    witness: Vec<Option<dae::DaeProvenance>>,
}

impl SystemEqualities {
    pub(super) fn collect(view: dae::DaeView<'_>) -> Self {
        let count = view.variable_count();
        let mut equalities = Self {
            exact: SignedClasses::new(count),
            affine: SignedClasses::new(count),
            state: view
                .variables()
                .map(|(_, variable)| variable.role() == dae::VariableRole::State)
                .collect(),
            coordinate: coordinate_expressions(view),
            witness: vec![None; count],
        };
        let mut pinned = Vec::new();
        for owner in view.continuous_owners() {
            let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
                continue;
            };
            let Some(equality) = asserted_equality(view, equation.residual()) else {
                continue;
            };
            for variable in equality.variables() {
                equalities.witness[variable as usize].get_or_insert(equation.provenance());
            }
            match equality {
                AssertedEquality::Aliased {
                    left,
                    right,
                    opposite,
                    displaced,
                } => equalities.alias(left, right, opposite, displaced),
                AssertedEquality::Pinned { variable, anchor } => pinned.push((variable, anchor)),
            }
        }
        equalities.resolve_anchors(view, &pinned);
        equalities
    }

    /// Record one asserted alias in the layers it holds for.
    ///
    /// A displaced pair shares only its derivative, so it never reaches the
    /// offset-free layer that answers value questions.
    fn alias(&mut self, left: u32, right: u32, opposite: bool, displaced: bool) {
        self.affine.union(left, right, opposite);
        if !displaced {
            self.exact.union(left, right, opposite);
        }
    }

    /// The class member whose derivative is known, and how `variable` signs
    /// against it, if the system proves the class has such a member.
    ///
    /// The offset-free class is reported whenever it has an anchor, and the
    /// displaced class otherwise. Both prove the same derivative, so preferring
    /// the offset-free one keeps a derivative claim on the same witness a value
    /// claim about the variable would use.
    pub(super) fn anchor_of(&self, variable: u32) -> Option<(EqualityAnchor, EqualitySign)> {
        self.exact
            .anchor_of(variable)
            .or_else(|| self.affine.anchor_of(variable))
    }

    /// The class anchor of `variable` on the offset-free layer only.
    ///
    /// [`Self::anchor_of`] answers derivative questions and may fall back on the
    /// displaced layer, where members share a derivative but not a value. A
    /// caller reasoning about a *value* — an initial condition, say — must not
    /// see that fallback, so it reads this instead.
    pub(super) fn value_anchor_of(&self, variable: u32) -> Option<(EqualityAnchor, EqualitySign)> {
        self.exact.anchor_of(variable)
    }

    /// The source expression a demotion onto `anchor` differentiates.
    pub(super) fn anchor_expression(&self, anchor: EqualityAnchor) -> Option<u32> {
        match anchor {
            EqualityAnchor::Invariant { value, .. } => value,
            EqualityAnchor::State(state) => self.coordinate[state as usize],
        }
    }

    /// One equality this system asserts about `variable`.
    pub(super) fn witness(&self, variable: u32) -> Option<dae::DaeProvenance> {
        self.witness[variable as usize]
    }

    /// States that a class equality proves are the anchor up to an exact sign.
    ///
    /// Only the offset-free layer is consulted: a demotion substitutes the
    /// anchor's value for the state, which a displaced equality does not prove.
    /// The sign is part of the proof: reconstruction differentiates the anchor
    /// and applies that sign, so `a + b = 0` substitutes `der(b) = -der(a)`
    /// without synthesizing or recovering a source expression for `-a`.
    pub(super) fn redundant_states(
        &self,
    ) -> impl Iterator<Item = (u32, EqualityAnchor, EqualitySign)> + '_ {
        (0..self.state.len() as u32).filter_map(move |variable| {
            if !self.state[variable as usize] {
                return None;
            }
            let (anchor, sign) = self.exact.anchor_of(variable)?;
            (anchor != EqualityAnchor::State(variable)).then_some((variable, anchor, sign))
        })
    }

    /// Pick the anchor of every class: a time-invariant pin fixes the whole
    /// class, otherwise the class keeps the state a solver would prefer.
    fn resolve_anchors(&mut self, view: dae::DaeView<'_>, pinned: &[(u32, EqualityAnchor)]) {
        for (id, variable) in view.variables() {
            let index = id.index();
            if variable.role() != dae::VariableRole::State
                || !is_scalar_real(variable)
                || self.coordinate[index as usize].is_none()
            {
                continue;
            }
            self.exact
                .offer_anchor(view, index, EqualityAnchor::State(index));
            self.affine
                .offer_anchor(view, index, EqualityAnchor::State(index));
        }
        for (variable, anchor) in pinned.iter().copied() {
            self.exact.offer_anchor(view, variable, anchor);
            self.affine.offer_anchor(view, variable, anchor);
        }
    }
}

/// Order anchors so a pinned class reports its invariant, and a free class
/// keeps the state a solver most wants to integrate.
///
/// The second component keeps a fixed initial value inside the class. Demoting
/// a state drops its initial equation, so a class member the model pins with
/// `fixed = true` has to be the one that survives — otherwise the reduction
/// would silently replace a stated initial condition with a guess. Between two
/// pins the one that names its value outranks the one that only proves
/// constancy, because only the named value can define a demoted state.
fn anchor_rank(view: dae::DaeView<'_>, anchor: EqualityAnchor) -> (u8, u8) {
    match anchor {
        // A time-invariant pin proves the whole class constant, which is
        // strictly more information than any state selection.
        EqualityAnchor::Invariant { value, .. } => (u8::MAX, u8::from(value.is_some())),
        EqualityAnchor::State(variable) => {
            let Some(variable) = view
                .variable_id(variable as usize)
                .and_then(|id| view.variable(id))
            else {
                return (0, 0);
            };
            let selection = match variable.state_select() {
                StateSelect::Never => 0,
                StateSelect::Avoid => 1,
                StateSelect::Default => 2,
                StateSelect::Prefer => 3,
                StateSelect::Always => 4,
            };
            (selection, u8::from(variable.fixed() == Some(true)))
        }
    }
}

/// Break an anchor rank tie deterministically: the lowest ordinal wins, so the
/// choice never depends on the order equations happened to be visited in.
fn anchor_ordinal(anchor: EqualityAnchor) -> std::cmp::Reverse<u32> {
    let ordinal = match anchor {
        EqualityAnchor::Invariant { ordinal, .. } => ordinal,
        EqualityAnchor::State(variable) => variable,
    };
    std::cmp::Reverse(ordinal)
}

/// One equality an additive residual proves.
enum AssertedEquality {
    /// Two variables the residual proves equal, up to sign and a possible
    /// time-invariant displacement.
    Aliased {
        left: u32,
        right: u32,
        opposite: bool,
        /// Whether the two variables are separated by a displacement the
        /// residual does not prove zero. A displaced pair still shares a
        /// derivative; it does not share a value.
        displaced: bool,
    },
    /// One variable the residual proves time-invariant.
    Pinned {
        variable: u32,
        anchor: EqualityAnchor,
    },
}

impl AssertedEquality {
    fn variables(&self) -> Vec<u32> {
        match *self {
            Self::Aliased { left, right, .. } => vec![left, right],
            Self::Pinned { variable, .. } => vec![variable],
        }
    }
}

/// The signed operands of one additive residual, split by what they name.
#[derive(Default)]
pub(super) struct AdditiveOperands {
    /// Scalar real coordinates, with the sign each carries in the residual.
    pub(super) variables: Vec<(u32, bool)>,
    /// Time-invariant operands, with their sign and whether they are exactly
    /// zero.
    pub(super) invariants: Vec<AdditiveInvariant>,
}

#[derive(Clone, Copy)]
pub(super) struct AdditiveInvariant {
    pub(super) expression: u32,
    pub(super) negated: bool,
    pub(super) zero: bool,
}

impl AdditiveOperands {
    /// Whether no operand displaces the balance: an omitted term and a literal
    /// zero both contribute exactly zero.
    fn offset_free(&self) -> bool {
        self.invariants.iter().all(|invariant| invariant.zero)
    }

    /// The single expression a pinned variable equals, when the residual states
    /// that value directly.
    ///
    /// A residual `s·v + Σ sᵢ·kᵢ = 0` puts the variable at `-(Σ sᵢ·kᵢ)/s`, so
    /// only a lone invariant of the opposite sign is the value as written. A
    /// balance whose invariants are all zero puts the variable at zero, which
    /// any of those zeros names.
    fn pinned_value(&self, negated: bool) -> Option<u32> {
        if self.offset_free() {
            return self
                .invariants
                .first()
                .map(|invariant| invariant.expression);
        }
        match self.invariants.as_slice() {
            [only] if only.negated != negated => Some(only.expression),
            _ => None,
        }
    }

    /// The equality this operand list proves, if it proves one.
    fn classify(&self, residual: u32) -> Option<AssertedEquality> {
        match *self.variables.as_slice() {
            [(left, left_negated), (right, right_negated)] => {
                (left != right).then_some(AssertedEquality::Aliased {
                    left,
                    right,
                    // `a - b = 0` proves `a ≡ b`; `a + b = 0` proves `a ≡ -b`.
                    opposite: left_negated == right_negated,
                    displaced: !self.offset_free(),
                })
            }
            [(variable, negated)] => Some(AssertedEquality::Pinned {
                variable,
                anchor: EqualityAnchor::Invariant {
                    value: self.pinned_value(negated),
                    ordinal: residual,
                },
            }),
            _ => None,
        }
    }
}

/// The equality one additive residual proves.
///
/// The residual is read as a signed sum of leaves, so every spelling of the
/// same balance reduces to the same operand list: a DAE writes `0 = a.i + b.i`
/// as `0 - (a.i + b.i)` and `phi_support = 0` as `phi_support - 0`, and a rigid
/// body writes `flange_a.s = s - L/2`. A residual that reaches a leaf which is
/// neither a scalar real coordinate nor a time-invariant expression proves
/// nothing this closure may use, and is dropped whole.
fn asserted_equality<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> Option<AssertedEquality> {
    let mut operands = AdditiveOperands::default();
    flatten_additive(view, residual, false, &mut operands)
        .then(|| operands.classify(residual.index()))
        .flatten()
}

/// Split `expression` into signed additive leaves, reporting whether every leaf
/// is one a reader can use.
///
/// The whole residual is read, however many coordinates it names, and each
/// reader then decides what the operand list proves: [`AdditiveOperands::classify`]
/// records nothing about a balance over more than two coordinates, and the
/// initial-value closure defers one until enough of its coordinates are known
/// constant. Counting after the walk rather than during it is what makes
/// admissibility a property of the *equation*: `f1 + f2 + f3 + f4 = 0`,
/// `0 = f1 + f2 + f3 + f4` and `f4 = -(f1 + f2 + f3)` are the same balance, and
/// a bound applied mid-walk would accept them by where the association happened
/// to put the leaves.
pub(super) fn flatten_additive<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    negated: bool,
    operands: &mut AdditiveOperands,
) -> bool {
    let Some(node) = whole_model_expression(view, expression) else {
        return false;
    };
    match node.operation() {
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus,
            operand,
        } => flatten_additive(view, operand, negated, operands),
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Negate,
            operand,
        } => flatten_additive(view, operand, !negated, operands),
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Add,
            lhs,
            rhs,
        } => {
            flatten_additive(view, lhs, negated, operands)
                && flatten_additive(view, rhs, negated, operands)
        }
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Subtract,
            lhs,
            rhs,
        } => {
            flatten_additive(view, lhs, negated, operands)
                && flatten_additive(view, rhs, !negated, operands)
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) => {
            push_variable(view, state.index(), negated, operands)
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) => {
            push_variable(view, algebraic.index(), negated, operands)
        }
        _ => {
            if !is_time_invariant(view, expression) {
                return false;
            }
            operands.invariants.push(AdditiveInvariant {
                expression: expression.index(),
                negated,
                zero: is_zero_literal(view, expression),
            });
            true
        }
    }
}

/// Record one scalar real coordinate operand.
///
/// A coordinate of any other shape is not a value this closure may chain, so
/// the whole residual is dropped rather than read without it.
fn push_variable(
    view: dae::DaeView<'_>,
    variable: u32,
    negated: bool,
    operands: &mut AdditiveOperands,
) -> bool {
    let Some(declaration) = view
        .variable_id(variable as usize)
        .and_then(|id| view.variable(id))
    else {
        return false;
    };
    if !is_scalar_real(declaration) {
        return false;
    }
    operands.variables.push((variable, negated));
    true
}

/// Whether `expression` has the same value at every instant.
///
/// Restricted to the forms differentiation resolves to zero outright: literal
/// numbers, parameter coordinates, and the arithmetic over them. A coordinate
/// that varies, a reference to `time`, and anything scoped to a function or a
/// comprehension are all excluded, so an accepted expression really is a
/// constant of the whole-model system.
pub(super) fn is_time_invariant<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> bool {
    let Some(node) = whole_model_expression(view, expression) else {
        return false;
    };
    match node.operation() {
        dae::ExpressionOperation::Literal(
            dae::DaeLiteral::Real(_) | dae::DaeLiteral::Integer(_),
        )
        | dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(_)) => true,
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => is_time_invariant(view, operand),
        dae::ExpressionOperation::Binary {
            operator:
                dae::BinaryOperator::Add
                | dae::BinaryOperator::Subtract
                | dae::BinaryOperator::Multiply
                | dae::BinaryOperator::Divide,
            lhs,
            rhs,
        } => is_time_invariant(view, lhs) && is_time_invariant(view, rhs),
        _ => false,
    }
}

fn is_zero_literal<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> bool {
    view.expression(expression).is_some_and(|expression| {
        matches!(
            expression.operation(),
            dae::ExpressionOperation::Literal(
                dae::DaeLiteral::Real(0.0) | dae::DaeLiteral::Integer(0)
            )
        )
    })
}

fn whole_model_expression<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<dae::ExpressionView<'dae>> {
    let expression = view.expression(expression)?;
    (expression.function_scope().is_none() && expression.binder_domain().is_none())
        .then_some(expression)
}

pub(super) fn is_scalar_real(variable: dae::VariableView<'_>) -> bool {
    variable.value_type().is_scalar()
        && variable.value_type().scalar_type() == dae::ScalarType::Real
}

/// Index the lowest whole-model coordinate expression naming each state.
///
/// A demotion hands one of these ordinals to reconstruction as the definition
/// it differentiates, so a scoped expression must never be indexed here: its
/// identity is only meaningful inside its function or comprehension.
fn coordinate_expressions(view: dae::DaeView<'_>) -> Vec<Option<u32>> {
    let mut coordinates = vec![None; view.variable_count()];
    for index in 0..view.expression_count() {
        let Some(expression) = view
            .expression_id(index)
            .and_then(|id| whole_model_expression(view, id))
        else {
            continue;
        };
        let dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) =
            expression.operation()
        else {
            continue;
        };
        coordinates[state.index() as usize].get_or_insert(index as u32);
    }
    coordinates
}

#[cfg(test)]
mod signed_class_scaling_tests {
    use super::SignedClasses;

    #[test]
    fn reverse_order_alias_chain_keeps_logarithmic_parent_depth() {
        const VARIABLE_COUNT: usize = 65_536;
        let mut classes = SignedClasses::new(VARIABLE_COUNT);
        for variable in (1..VARIABLE_COUNT as u32).rev() {
            classes.union(variable, variable - 1, variable % 2 == 0);
        }

        assert!(classes.maximum_depth() <= u32::BITS as usize);
    }
}
