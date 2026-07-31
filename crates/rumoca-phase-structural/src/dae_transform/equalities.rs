//! Coordinate equalities the source system already asserts.
//!
//! A connected MSL model states most of its structure as bare two-operand
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

use rumoca_core::StateSelect;
use rumoca_ir_dae as dae;

/// The class member whose time derivative is known.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum EqualityAnchor {
    /// A time-invariant expression the class is pinned to, by source ordinal:
    /// every member of the class has derivative zero.
    Invariant(u32),
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

/// Exact coordinate equalities, closed transitively over one finalized DAE.
pub(super) struct SystemEqualities {
    /// Union-find parent per variable ordinal.
    parent: Vec<u32>,
    /// Sign of each variable relative to its union-find parent.
    parity: Vec<bool>,
    /// Whether each variable ordinal is declared a continuous state.
    state: Vec<bool>,
    /// Anchor of the class rooted at each variable ordinal.
    anchor: Vec<Option<EqualityAnchor>>,
    /// Classes whose edges contradict each other and prove nothing usable.
    inconsistent: Vec<bool>,
    /// Lowest whole-model coordinate expression ordinal naming each variable.
    coordinate: Vec<Option<u32>>,
    /// One asserted equality incident to each variable, for candidate owners.
    witness: Vec<Option<dae::DaeProvenance>>,
}

impl SystemEqualities {
    pub(super) fn collect(view: dae::DaeView<'_>) -> Self {
        let count = view.variable_count();
        let mut equalities = Self {
            parent: (0..count as u32).collect(),
            parity: vec![false; count],
            state: view
                .variables()
                .map(|(_, variable)| variable.role() == dae::VariableRole::State)
                .collect(),
            anchor: vec![None; count],
            inconsistent: vec![false; count],
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
                } => equalities.union(left, right, opposite),
                AssertedEquality::Pinned {
                    variable,
                    expression,
                } => pinned.push((variable, expression)),
            }
        }
        equalities.resolve_anchors(view, &pinned);
        equalities
    }

    /// The class member whose derivative is known, and how `variable` signs
    /// against it, if the class has such a member.
    ///
    /// A pinned class always reports [`EqualitySign::Same`]: a time-invariant
    /// value has the same zero derivative under either sign, so the sign
    /// carries no information a caller could act on.
    pub(super) fn anchor_of(&self, variable: u32) -> Option<(EqualityAnchor, EqualitySign)> {
        let (root, parity) = self.find(variable);
        if self.inconsistent[root as usize] {
            return None;
        }
        match self.anchor[root as usize]? {
            anchor @ EqualityAnchor::Invariant(_) => Some((anchor, EqualitySign::Same)),
            anchor @ EqualityAnchor::State(state) => {
                let (_, anchor_parity) = self.find(state);
                Some((anchor, EqualitySign::of(parity != anchor_parity)))
            }
        }
    }

    /// The source expression a demotion onto `anchor` differentiates.
    pub(super) fn anchor_expression(&self, anchor: EqualityAnchor) -> Option<u32> {
        match anchor {
            EqualityAnchor::Invariant(expression) => Some(expression),
            EqualityAnchor::State(state) => self.coordinate[state as usize],
        }
    }

    /// One equality this system asserts about `variable`.
    pub(super) fn witness(&self, variable: u32) -> Option<dae::DaeProvenance> {
        self.witness[variable as usize]
    }

    /// States that a class equality proves are the anchor itself.
    ///
    /// A state anchor is only reported for same-sign members: a demotion hands
    /// reconstruction an existing coordinate expression as the state's
    /// definition, and the DAE need not contain a negated form of it.
    pub(super) fn redundant_states(&self) -> impl Iterator<Item = (u32, EqualityAnchor)> + '_ {
        (0..self.parent.len() as u32).filter_map(move |variable| {
            if !self.state[variable as usize] {
                return None;
            }
            let (anchor, sign) = self.anchor_of(variable)?;
            (sign == EqualitySign::Same && anchor != EqualityAnchor::State(variable))
                .then_some((variable, anchor))
        })
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
        let (keep, merged) = if left < right {
            (left, right)
        } else {
            (right, left)
        };
        self.parent[merged as usize] = keep;
        self.parity[merged as usize] = relative;
        self.inconsistent[keep as usize] |= self.inconsistent[merged as usize];
    }

    /// Pick the anchor of every class: a time-invariant pin fixes the whole
    /// class, otherwise the class keeps the state a solver would prefer.
    fn resolve_anchors(&mut self, view: dae::DaeView<'_>, pinned: &[(u32, u32)]) {
        for (id, variable) in view.variables() {
            let index = id.index();
            if variable.role() != dae::VariableRole::State
                || !is_scalar_real(variable)
                || self.coordinate[index as usize].is_none()
            {
                continue;
            }
            self.offer_anchor(view, index, EqualityAnchor::State(index));
        }
        for (variable, expression) in pinned.iter().copied() {
            self.offer_anchor(view, variable, EqualityAnchor::Invariant(expression));
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
}

/// Order anchors so a pinned class reports its invariant, and a free class
/// keeps the state a solver most wants to integrate.
///
/// The second component keeps a fixed initial value inside the class. Demoting
/// a state drops its initial equation, so a class member the model pins with
/// `fixed = true` has to be the one that survives — otherwise the reduction
/// would silently replace a stated initial condition with a guess.
fn anchor_rank(view: dae::DaeView<'_>, anchor: EqualityAnchor) -> (u8, u8) {
    match anchor {
        // A time-invariant pin proves the whole class constant, which is
        // strictly more information than any state selection.
        EqualityAnchor::Invariant(_) => (u8::MAX, u8::MAX),
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
        EqualityAnchor::Invariant(expression) => expression,
        EqualityAnchor::State(variable) => variable,
    };
    std::cmp::Reverse(ordinal)
}

/// One equality a two-operand residual proves.
enum AssertedEquality {
    /// Two variables the residual proves equal, up to sign.
    Aliased {
        left: u32,
        right: u32,
        opposite: bool,
    },
    /// One variable the residual pins to a time-invariant expression.
    Pinned { variable: u32, expression: u32 },
}

impl AssertedEquality {
    fn variables(&self) -> Vec<u32> {
        match *self {
            Self::Aliased { left, right, .. } => vec![left, right],
            Self::Pinned { variable, .. } => vec![variable],
        }
    }
}

/// One side of a two-operand residual, once its sign has been peeled off.
#[derive(Clone, Copy)]
enum EqualityOperand {
    Variable {
        variable: u32,
        negated: bool,
    },
    /// A literal or parameter coordinate: time-invariant, and differentiable to
    /// zero whichever sign it carries.
    Invariant {
        expression: u32,
    },
}

/// The equality a bare `a ± b` residual proves.
///
/// Both the residual as written and its zero-stripped form are tried, because a
/// DAE spells `0 = a.i + b.i` as `0 - (a.i + b.i)` while spelling
/// `phi_support = 0` as `phi_support - 0`; stripping is right for the first and
/// destroys the second.
fn asserted_equality<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> Option<AssertedEquality> {
    two_operand_equality(view, residual)
        .or_else(|| two_operand_equality(view, zero_normalized(view, residual)))
}

fn two_operand_equality<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> Option<AssertedEquality> {
    let dae::ExpressionOperation::Binary { operator, lhs, rhs } =
        view.expression(residual)?.operation()
    else {
        return None;
    };
    let opposite = match operator {
        dae::BinaryOperator::Subtract => false,
        dae::BinaryOperator::Add => true,
        _ => return None,
    };
    match (equality_operand(view, lhs)?, equality_operand(view, rhs)?) {
        (
            EqualityOperand::Variable {
                variable: left,
                negated: left_negated,
            },
            EqualityOperand::Variable {
                variable: right,
                negated: right_negated,
            },
        ) => (left != right).then_some(AssertedEquality::Aliased {
            left,
            right,
            opposite: opposite ^ left_negated ^ right_negated,
        }),
        (EqualityOperand::Variable { variable, .. }, EqualityOperand::Invariant { expression })
        | (EqualityOperand::Invariant { expression }, EqualityOperand::Variable { variable, .. }) => {
            Some(AssertedEquality::Pinned {
                variable,
                expression,
            })
        }
        (EqualityOperand::Invariant { .. }, EqualityOperand::Invariant { .. }) => None,
    }
}

/// Strip the sign and zero padding a residual may carry around its balance.
///
/// A DAE lowers `0 = a.i + b.i` as `0 - (a.i + b.i)`, and an equation written
/// the other way round as `(a.i + b.i) - 0`. Both assert exactly what the inner
/// balance asserts, and so does any overall sign, so peeling them loses nothing
/// and lets one matcher read every spelling of the same equality.
fn zero_normalized<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> dae::ExprId<'dae> {
    let mut current = residual;
    loop {
        let Some(expression) = view.expression(current) else {
            return current;
        };
        if expression.function_scope().is_some() || expression.binder_domain().is_some() {
            return current;
        }
        current = match expression.operation() {
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => operand,
            dae::ExpressionOperation::Binary {
                operator: dae::BinaryOperator::Subtract,
                lhs,
                rhs,
            } if is_zero_literal(view, lhs) => rhs,
            dae::ExpressionOperation::Binary {
                operator: dae::BinaryOperator::Add | dae::BinaryOperator::Subtract,
                lhs,
                rhs,
            } if is_zero_literal(view, rhs) => lhs,
            _ => return current,
        };
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

/// Read one residual side as a signed variable or a time-invariant pin.
///
/// Only whole-model operands qualify: a function-scoped or comprehension-bound
/// expression does not name a whole-model unknown. A derivative or discrete
/// coordinate is not a value this closure may chain, and an invariant is
/// restricted to the two forms differentiation resolves to zero outright.
fn equality_operand<'dae>(
    view: dae::DaeView<'dae>,
    operand: dae::ExprId<'dae>,
) -> Option<EqualityOperand> {
    let expression = whole_model_expression(view, operand)?;
    let (operand, negated) = match expression.operation() {
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Negate,
            operand,
        } => (operand, true),
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus,
            operand,
        } => (operand, false),
        _ => (operand, false),
    };
    let expression = whole_model_expression(view, operand)?;
    let variable = match expression.operation() {
        dae::ExpressionOperation::Literal(
            dae::DaeLiteral::Real(_) | dae::DaeLiteral::Integer(_),
        ) => {
            return Some(EqualityOperand::Invariant {
                expression: operand.index(),
            });
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(_)) => {
            return Some(EqualityOperand::Invariant {
                expression: operand.index(),
            });
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) => state.index(),
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) => {
            algebraic.index()
        }
        _ => return None,
    };
    let declaration = view.variable(view.variable_id(variable as usize)?)?;
    is_scalar_real(declaration).then_some(EqualityOperand::Variable { variable, negated })
}

fn whole_model_expression<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<dae::ExpressionView<'dae>> {
    let expression = view.expression(expression)?;
    (expression.function_scope().is_none() && expression.binder_domain().is_none())
        .then_some(expression)
}

fn is_scalar_real(variable: dae::VariableView<'_>) -> bool {
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
