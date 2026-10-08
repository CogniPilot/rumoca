//! Value facts proven by the conditions, assignments and array accesses that
//! reach a program point of a function body.
//!
//! MLS 3.7 §11.2.6 runs a branch only when its condition holds and every
//! earlier condition of the same `if` failed, so inside the branch those
//! relations are facts about the values they compare: `radius <= 4` bounds
//! `radius` above. An assignment bounds its target by the interval of the
//! value it stores. A local assigned a choice between literals carries the
//! facts of the path that selects each literal (a Boolean
//! `linear := radius > 4` makes `not linear` imply `radius <= 4`; a Real
//! `valid := if c then 1.0 else 0.0` makes `valid > 0.0` imply `c`). An
//! element access `a[k]` that completes proves `1 <= k <= size(a, d)`
//! (MLS §10.5: any other index is an error). Every fact is forgotten when a
//! statement writes a value it mentions, so a fact is only ever read while the
//! values it relates are the ones it was proven about. A loop carries the
//! facts that hold at its head on every pass: a fixed point of the facts
//! before it joined with the facts after its body, reached by widening the
//! bounds that keep moving (see `transfer`).
//!
//! The subject of a fact is a scalar value or an element of an array value
//! named with literal subscripts (`settings[7]`). Integer facts are exact
//! intervals (`rumoca_core::IntegerInterval`). A Real fact is a closed
//! interval (`rumoca_core::RealInterval`) proven only by a literal assignment
//! or by a comparison that implies both operands are ordered (a relation that
//! holds, other than `<>`; or a `<>` that fails), so no Real fact is ever held
//! by a NaN; a strict comparison keeps its closed hull, a superset, so an
//! empty Real interval proves a contradiction and a nonempty one proves no
//! more than its hull.
//!
//! The facts bound compact dependent domains and while-loop pass counts, and
//! prove that a path on which a value is undefined cannot reach a later
//! branch. They never select a branch or fold a value.

mod intervals;
mod path_set;
mod selections;
#[cfg(test)]
mod tests;
mod transfer;

use super::*;
pub(super) use path_set::PathSet;
use rumoca_core::{IntegerInterval, RealInterval};
use selections::{ArmValue, Selection, single_branch_and};
use std::collections::BTreeMap;

/// The proven set of one subject's values.
#[derive(Clone, Copy, PartialEq, Debug)]
pub(super) enum ValueFact {
    Integer(IntegerInterval),
    Real(RealInterval),
}

impl ValueFact {
    fn meet(self, other: Self) -> Self {
        match (self, other) {
            (Self::Integer(lhs), Self::Integer(rhs)) => Self::Integer(lhs.meet(rhs)),
            (Self::Real(lhs), Self::Real(rhs)) => Self::Real(lhs.meet(rhs)),
            // One name has one declared type; a mixed pair keeps the first.
            (fact, _) => fact,
        }
    }

    fn hull(self, other: Self) -> Option<Self> {
        let joined = match (self, other) {
            (Self::Integer(lhs), Self::Integer(rhs)) => Self::Integer(lhs.hull(rhs)),
            (Self::Real(lhs), Self::Real(rhs)) => Self::Real(lhs.hull(rhs)),
            _ => return None,
        };
        (!joined.is_unbounded()).then_some(joined)
    }

    fn is_empty(self) -> bool {
        match self {
            Self::Integer(interval) => interval.is_empty(),
            Self::Real(interval) => interval.is_empty(),
        }
    }

    fn is_unbounded(self) -> bool {
        match self {
            Self::Integer(interval) => interval.is_unbounded(),
            Self::Real(interval) => interval.is_unbounded(),
        }
    }
}

/// A scalar value, or one element of an array value named by literal
/// subscripts. A write of `name` (whole or any part) forgets all of them.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(super) struct FactSubject {
    name: VarName,
    element: Box<[i64]>,
}

impl FactSubject {
    fn scalar(name: VarName) -> Self {
        Self {
            name,
            element: Box::new([]),
        }
    }

    fn is_scalar(&self) -> bool {
        self.element.is_empty()
    }

    /// The subject `expression` reads: a scalar reference, or an element
    /// reference whose every subscript is an Integer literal.
    fn of(expression: &Expression) -> Option<Self> {
        let (name, subscripts) = named_access(expression)?;
        let element = subscripts
            .iter()
            .map(|subscript| match subscript {
                Subscript::Index { value, .. } => Some(*value),
                Subscript::Expr { expr, .. } => integer_literal(expr),
                Subscript::Colon { .. } => None,
            })
            .collect::<Option<Box<[i64]>>>()?;
        Some(Self {
            name: name.clone(),
            element,
        })
    }
}

/// The declared value `expression` reads and the subscripts it applies: a
/// reference `a[i, j]`, or an indexing of a plain reference, which is the
/// same access.
fn named_access(expression: &Expression) -> Option<(&VarName, &[Subscript])> {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } => Some((name.var_name(), subscripts)),
        Expression::Index {
            base, subscripts, ..
        } => match base.as_ref() {
            Expression::VarRef {
                name,
                subscripts: outer,
                ..
            } if outer.is_empty() => Some((name.var_name(), subscripts)),
            _ => None,
        },
        _ => None,
    }
}

/// How many Booleans deep a selection follows what its arms prove.
const MAX_IMPLICATION_DEPTH: usize = 8;

/// Facts that hold on one path: a fact per subject. `None` is an unreachable
/// path, where every fact holds.
type PathFacts = Option<BTreeMap<FactSubject, ValueFact>>;

fn conjoin(lhs: PathFacts, rhs: PathFacts) -> PathFacts {
    let (mut lhs, rhs) = (lhs?, rhs?);
    for (subject, fact) in rhs {
        let merged = match lhs.get(&subject) {
            Some(existing) => existing.meet(fact),
            None => fact,
        };
        if merged.is_empty() {
            return None;
        }
        lhs.insert(subject, merged);
    }
    Some(lhs)
}

/// What holds on either of two paths: a fact both paths prove, widened to
/// cover both.
fn disjoin(lhs: PathFacts, rhs: PathFacts) -> PathFacts {
    let (Some(lhs), Some(rhs)) = (&lhs, &rhs) else {
        return lhs.or(rhs);
    };
    Some(
        lhs.iter()
            .filter_map(|(subject, fact)| Some((subject.clone(), fact.hull(*rhs.get(subject)?)?)))
            .collect(),
    )
}

fn trivial(facts: &PathFacts) -> bool {
    facts.as_ref().is_some_and(BTreeMap::is_empty)
}

/// What a fact owner reads beside the facts: the proven extents and values,
/// and which declared values (scalars and arrays, by element type) are
/// Integer and which are Real.
#[derive(Clone, Copy)]
pub(super) struct FactScope<'a> {
    pub(super) shapes: &'a ShapeEnvironment,
    pub(super) integers: &'a HashSet<VarName>,
    pub(super) reals: &'a HashSet<VarName>,
    /// The generated Booleans, each captured by the `Empty` statement at its
    /// source span.
    pub(super) generated: &'a [function_returns::GeneratedBooleanDefinition],
}

impl FactScope<'_> {
    fn is_integer(&self, subject: &FactSubject) -> bool {
        self.integers.contains(&subject.name)
    }

    fn is_real(&self, subject: &FactSubject) -> bool {
        self.reals.contains(&subject.name)
    }

    /// Whether `name` is a declared value proven to be an array, which a fact
    /// about a scalar never describes.
    fn is_array(&self, name: &VarName) -> bool {
        self.shapes.get(name).is_some_and(|shape| !shape.is_empty())
    }
}

/// The facts proven at one program point of a function body.
#[derive(Clone, PartialEq, Debug)]
pub(super) struct GuardFacts {
    path: PathFacts,
    /// The facts implied by each value a local selected among literals can
    /// hold.
    selections: BTreeMap<VarName, Selection>,
}

impl GuardFacts {
    /// No facts.
    pub(super) fn entry() -> Self {
        Self {
            path: Some(BTreeMap::new()),
            selections: BTreeMap::new(),
        }
    }

    /// The facts on entry to `function`'s algorithm: every local declared
    /// with a binding holds the value of that binding (MLS §12.4.4).
    pub(super) fn function_entry(function: &rumoca_core::Function, scope: FactScope<'_>) -> Self {
        let mut facts = Self::entry();
        for local in &function.locals {
            if let Some(default) = &local.default
                && local.effective_type.dimensions().is_empty()
            {
                facts.assign(VarName::new(&local.name), default, scope);
            }
        }
        facts
    }

    /// A Boolean whose arms prove a value among `written`, with more than one
    /// arm left on this path.
    pub(super) fn correlated_boolean(&self, written: &HashSet<String>) -> Option<VarName> {
        self.selections
            .iter()
            .find(|(_, selection)| selection.correlates_with(written))
            .map(|(name, _)| name.clone())
    }

    /// This path where the Boolean `name` has `value`; unreachable when no arm
    /// of any selection admits it.
    pub(super) fn assuming_boolean(&self, name: &VarName, value: bool) -> Self {
        let mut assumed = self.clone();
        assumed.narrow_boolean(name, value, 0);
        if assumed.selections.values().any(Selection::is_empty) {
            assumed.path = None;
            assumed.selections.clear();
        }
        assumed
    }

    /// Whether both paths hold the same literal values for each value that
    /// has a selection.
    pub(super) fn same_selected_values(&self, other: &Self) -> bool {
        self.selections.len() == other.selections.len()
            && self.selections.iter().all(|(name, selection)| {
                other
                    .selections
                    .get(name)
                    .is_some_and(|candidate| selection.same_values(candidate))
            })
    }

    /// Whether no execution reaches this path: its facts contradict.
    pub(super) fn is_unreachable(&self) -> bool {
        self.path.is_none()
    }

    /// Whether these facts constrain nothing, so no later fact can contradict
    /// them.
    pub(super) fn is_trivial(&self) -> bool {
        trivial(&self.path) && self.selections.is_empty()
    }

    /// Conjoin every proven Integer path fact about a scalar with what
    /// `shapes` already proves.
    pub(super) fn refine(&self, shapes: &mut ShapeEnvironment) {
        for (subject, fact) in self.path.iter().flatten() {
            if let ValueFact::Integer(interval) = fact
                && subject.is_scalar()
            {
                shapes.refine_integer_interval(subject.name.clone(), *interval);
            }
        }
    }

    /// The facts at the start of each branch of an `if`, and on its
    /// fall-through path when it has no `else` (the last entry). A condition
    /// is evaluated only when every earlier one failed, so the element
    /// accesses it completes are facts from there on.
    pub(super) fn branch_entries(
        &self,
        conditions: &[&Expression],
        scope: FactScope<'_>,
    ) -> Vec<Self> {
        let mut entries = Vec::with_capacity(conditions.len() + 1);
        let mut remaining = self.clone();
        for condition in conditions {
            remaining.observe_accesses(condition, scope);
            entries.push(remaining.assuming(condition, true, scope));
            remaining = remaining.assuming(condition, false, scope);
        }
        entries.push(remaining);
        entries
    }

    /// The facts after paths that rejoin.
    pub(super) fn join(paths: &[Self]) -> Self {
        let mut joined = Self {
            path: None,
            selections: BTreeMap::new(),
        };
        for path in paths {
            joined.join_path(path);
        }
        joined
    }

    /// Widen to also hold on `other`.
    pub(super) fn join_path(&mut self, other: &Self) {
        // An unreachable path contributes nothing to a join.
        if other.is_unreachable() {
            return;
        }
        if self.is_unreachable() {
            *self = other.clone();
            return;
        }
        self.path = disjoin(self.path.take(), other.path.clone());
        self.selections
            .retain(|name, selection| selection.join(other.selections.get(name)));
    }

    /// The facts after the whole scalar `target` is assigned `value`.
    pub(super) fn assign(&mut self, target: VarName, value: &Expression, scope: FactScope<'_>) {
        // The value is read before the write, so its facts use the facts that
        // hold before the target changes.
        let subject = FactSubject::scalar(target.clone());
        let fact = self.assigned_fact(&subject, value, scope);
        // Each arm's path facts describe the target's old value, which the
        // write ends.
        let selection = self
            .selection_of(&subject, value, scope)
            .map(|mut selection| {
                selection.forget_name(&target);
                selection
            });
        self.forget(&target);
        if let (Some(path), Some(fact)) = (&mut self.path, fact) {
            path.insert(subject, fact);
        }
        if let Some(selection) = selection {
            self.selections.insert(target, selection);
        }
    }

    /// The fact a write of `value` proves for `subject`: the interval of an
    /// Integer value, the exact value of a Real literal, or the hull of the
    /// Real literals a selection can store.
    fn assigned_fact(
        &self,
        subject: &FactSubject,
        value: &Expression,
        scope: FactScope<'_>,
    ) -> Option<ValueFact> {
        let fact = if scope.is_integer(subject) {
            ValueFact::Integer(self.integer_interval(value, scope))
        } else if scope.is_real(subject) {
            let mut literals = selections::literal_arms(value)?
                .into_iter()
                .map(|(_, arm)| RealInterval::exact(arm.as_real()?));
            let first = literals.next()??;
            ValueFact::Real(literals.try_fold(first, |hull, arm| Some(hull.hull(arm?)))?)
        } else {
            return None;
        };
        (!fact.is_unbounded()).then_some(fact)
    }

    /// The facts that hold when `condition` evaluates to `value` here.
    pub(super) fn assuming(
        &self,
        condition: &Expression,
        value: bool,
        scope: FactScope<'_>,
    ) -> Self {
        let mut assumed = self.clone();
        assumed.path = conjoin(assumed.path, self.facts(condition, value, scope));
        if assumed.path.is_none() {
            assumed.selections.clear();
            return assumed;
        }
        assumed.narrow_selections(condition, value);
        assumed
    }

    fn forget(&mut self, name: &VarName) {
        forget_in(&mut self.path, name);
        self.selections.remove(name);
        for selection in self.selections.values_mut() {
            selection.forget_name(name);
        }
    }

    /// The facts implied when `lhs and rhs` (`conjunction`) or `lhs or rhs`
    /// evaluates to `value`.
    fn junction_facts(
        &self,
        (lhs, rhs): (&Expression, &Expression),
        conjunction: bool,
        value: bool,
        scope: FactScope<'_>,
    ) -> PathFacts {
        let lhs_facts = self.facts(lhs, value, scope);
        // `a and b` is true when both are, false when either is.
        if conjunction == value {
            // Both operands have this value: the second one's facts are read
            // with the first one's facts already known.
            let mut known = self.clone();
            known.path = conjoin(known.path, lhs_facts.clone());
            known.path.as_ref()?;
            conjoin(lhs_facts, known.facts(rhs, value, scope))
        } else {
            disjoin(lhs_facts, self.facts(rhs, value, scope))
        }
    }

    /// The facts implied when `expression` evaluates to `value`.
    fn facts(&self, expression: &Expression, value: bool, scope: FactScope<'_>) -> PathFacts {
        match expression {
            Expression::Literal {
                value: Literal::Boolean(literal),
                ..
            } => (*literal == value).then(BTreeMap::new),
            Expression::Unary {
                op: OpUnary::Not,
                rhs,
                ..
            } => self.facts(rhs, !value, scope),
            Expression::Binary {
                op: op @ (OpBinary::And | OpBinary::Or),
                lhs,
                rhs,
                ..
            } => self.junction_facts((lhs, rhs), matches!(op, OpBinary::And), value, scope),
            // `if a then b else false` is `a and b`.
            Expression::If { .. } if single_branch_and(expression).is_some() => {
                single_branch_and(expression).map_or(Some(BTreeMap::new()), |[lhs, rhs]| {
                    self.junction_facts((lhs, rhs), true, value, scope)
                })
            }
            Expression::Binary { op, lhs, rhs, .. } => self.comparison(op, lhs, rhs, value, scope),
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.boolean_facts(name.var_name(), value, 0),
            _ => Some(BTreeMap::new()),
        }
    }

    /// The facts a relation proves for a subject on either side: an Integer
    /// interval from the proven interval of the other side, a Real interval
    /// when the relation's value implies both operands are ordered, and the
    /// facts of the arms of a selection the relation admits.
    fn comparison(
        &self,
        op: &OpBinary,
        lhs: &Expression,
        rhs: &Expression,
        value: bool,
        scope: FactScope<'_>,
    ) -> PathFacts {
        let Some(relation) = Relation::of(op) else {
            return Some(BTreeMap::new());
        };
        // `<>` is the one relation a NaN operand satisfies; every other one
        // fails for it. So only these outcomes exclude a NaN operand.
        let ordered = matches!(relation, Relation::NotEqual) != value;
        let relation = if value { relation } else { relation.negated() };
        let mut facts = Some(BTreeMap::new());
        for (subject, other, relation) in [(lhs, rhs, relation), (rhs, lhs, relation.mirrored())] {
            facts = conjoin(facts, self.factor_bounds(subject, other, relation, scope));
            let Some(subject) = FactSubject::of(subject) else {
                continue;
            };
            let fact = if scope.is_integer(&subject) {
                ValueFact::Integer(relation.integer_bound(self.integer_interval(other, scope)))
            } else if scope.is_real(&subject) && ordered {
                ValueFact::Real(relation.real_bound(self.real_interval(other, scope)))
            } else {
                ValueFact::Integer(IntegerInterval::UNBOUNDED)
            };
            if !fact.is_unbounded() {
                facts = conjoin(facts, Some(BTreeMap::from([(subject.clone(), fact)])));
            }
            if subject.is_scalar()
                && let Some(selection) = self.selections.get(&subject.name)
                && let Some(literal) = ArmValue::of_literal(other)
            {
                facts = conjoin(
                    facts,
                    selection.facts_when(|arm| relation.holds(arm, literal)),
                );
            }
        }
        facts
    }

    /// The upper bounds a bounded product proves for its Integer factors:
    /// when `a * b <= u` with `u >= 0` and `b >= m >= 1`, a nonnegative `a`
    /// is at most `u / b <= u / m` and a negative one is below 0, so
    /// `a <= floor(u / m)` (`width * height == n` bounds both extents).
    fn factor_bounds(
        &self,
        product: &Expression,
        other: &Expression,
        relation: Relation,
        scope: FactScope<'_>,
    ) -> PathFacts {
        let mut facts = BTreeMap::new();
        let Expression::Binary {
            op: OpBinary::Mul | OpBinary::MulElem,
            lhs,
            rhs,
            ..
        } = product
        else {
            return Some(facts);
        };
        let Some(limit) = relation
            .integer_bound(self.integer_interval(other, scope))
            .upper
            .filter(|limit| *limit >= 0)
        else {
            return Some(facts);
        };
        for (factor, cofactor) in [(lhs, rhs), (rhs, lhs)] {
            let Some(subject) = FactSubject::of(factor).filter(|subject| scope.is_integer(subject))
            else {
                continue;
            };
            if let Some(least) = self
                .integer_interval(cofactor, scope)
                .lower
                .filter(|least| *least >= 1)
            {
                let bound = IntegerInterval {
                    lower: None,
                    upper: Some(limit / least),
                };
                facts.insert(subject, ValueFact::Integer(bound));
            }
        }
        Some(facts)
    }

    /// The facts that hold where the Boolean `name` has `value`: those of the
    /// arms of its selection that store it, and what each such arm proves of
    /// other Booleans.
    fn boolean_facts(&self, name: &VarName, value: bool, depth: usize) -> PathFacts {
        let Some(selection) = self.selections.get(name) else {
            return Some(BTreeMap::new());
        };
        selection
            .arms_when(|arm| arm.is_boolean(value))
            .fold(None, |joined, (facts, implied)| {
                let implied = (depth < MAX_IMPLICATION_DEPTH)
                    .then_some(implied)
                    .into_iter()
                    .flatten();
                let arm = implied.fold(facts.clone(), |arm, (other, holds)| {
                    conjoin(arm, self.boolean_facts(other, *holds, depth + 1))
                });
                disjoin(joined, arm)
            })
    }

    /// Keep only the arms of the Boolean `name` that store `value`, and narrow
    /// the Booleans every remaining arm proves.
    fn narrow_boolean(&mut self, name: &VarName, value: bool, depth: usize) {
        let selection = self
            .selections
            .entry(name.clone())
            .or_insert_with(Selection::booleans);
        selection.retain(|arm| arm.is_boolean(value));
        if depth >= MAX_IMPLICATION_DEPTH {
            return;
        }
        for (other, holds) in selection.implied_when(|arm| arm.is_boolean(value)) {
            self.narrow_boolean(&other, holds, depth + 1);
        }
    }

    /// Keep only the arms of a selection that `condition` having `value`
    /// admits: a Boolean read directly, or a relation with a literal.
    fn narrow_selections(&mut self, condition: &Expression, value: bool) {
        match condition {
            Expression::Unary {
                op: OpUnary::Not,
                rhs,
                ..
            } => self.narrow_selections(rhs, !value),
            Expression::Binary {
                op: OpBinary::And,
                lhs,
                rhs,
                ..
            } if value => {
                self.narrow_selections(lhs, true);
                self.narrow_selections(rhs, true);
            }
            Expression::If { .. } if value => {
                if let Some([condition, arm]) = single_branch_and(condition) {
                    self.narrow_selections(condition, true);
                    self.narrow_selections(arm, true);
                }
            }
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.narrow_boolean(name.var_name(), value, 0),
            Expression::Binary { op, lhs, rhs, .. } => {
                let Some(relation) = Relation::of(op) else {
                    return;
                };
                let relation = if value { relation } else { relation.negated() };
                self.narrow_by_relation(lhs, rhs, relation);
                self.narrow_by_relation(rhs, lhs, relation.mirrored());
            }
            _ => {}
        }
    }

    /// Keep only the arms of `subject`'s selection that stand in `relation`
    /// to the literal `other`.
    fn narrow_by_relation(&mut self, subject: &Expression, other: &Expression, relation: Relation) {
        if let Some(subject) = FactSubject::of(subject)
            && subject.is_scalar()
            && let Some(literal) = ArmValue::of_literal(other)
            && let Some(selection) = self.selections.get_mut(&subject.name)
        {
            selection.retain(|arm| relation.holds(arm, literal));
        }
    }
}

/// The value of an Integer or Real literal, or of its negation.
fn numeric_literal(expression: &Expression) -> Option<f64> {
    match expression {
        Expression::Literal {
            value: Literal::Real(value),
            ..
        } => Some(*value),
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value as f64),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => numeric_literal(rhs).map(|value| -value),
        _ => None,
    }
}

/// The value of an Integer literal, or of its negation.
fn integer_literal(expression: &Expression) -> Option<i64> {
    match expression {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => integer_literal(rhs)?.checked_neg(),
        _ => None,
    }
}

/// `subject <relation> other`.
#[derive(Clone, Copy)]
enum Relation {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
}

impl Relation {
    fn of(op: &OpBinary) -> Option<Self> {
        Some(match op {
            OpBinary::Lt => Self::Less,
            OpBinary::Le => Self::LessEqual,
            OpBinary::Gt => Self::Greater,
            OpBinary::Ge => Self::GreaterEqual,
            OpBinary::Eq => Self::Equal,
            OpBinary::Neq => Self::NotEqual,
            _ => return None,
        })
    }

    /// The relation that holds when this one is false.
    fn negated(self) -> Self {
        match self {
            Self::Less => Self::GreaterEqual,
            Self::LessEqual => Self::Greater,
            Self::Greater => Self::LessEqual,
            Self::GreaterEqual => Self::Less,
            Self::Equal => Self::NotEqual,
            Self::NotEqual => Self::Equal,
        }
    }

    /// The same relation with its operands exchanged.
    fn mirrored(self) -> Self {
        match self {
            Self::Less => Self::Greater,
            Self::LessEqual => Self::GreaterEqual,
            Self::Greater => Self::Less,
            Self::GreaterEqual => Self::LessEqual,
            Self::Equal => Self::Equal,
            Self::NotEqual => Self::NotEqual,
        }
    }

    /// Whether the selected literal `arm` stands in this relation to the
    /// literal `other`, exactly.
    fn holds(self, arm: ArmValue, other: ArmValue) -> bool {
        let Some(ordering) = arm.compare(other) else {
            return true;
        };
        match self {
            Self::Less => ordering.is_lt(),
            Self::LessEqual => ordering.is_le(),
            Self::Greater => ordering.is_gt(),
            Self::GreaterEqual => ordering.is_ge(),
            Self::Equal => ordering.is_eq(),
            Self::NotEqual => ordering.is_ne(),
        }
    }

    /// The Integer values of the subject that satisfy it for some value of
    /// `other`.
    fn integer_bound(self, other: IntegerInterval) -> IntegerInterval {
        let shifted =
            |endpoint: Option<i64>, delta: i64| endpoint.and_then(|v| v.checked_add(delta));
        match self {
            Self::Less => IntegerInterval {
                lower: None,
                upper: shifted(other.upper, -1),
            },
            Self::LessEqual => IntegerInterval {
                lower: None,
                upper: other.upper,
            },
            Self::Greater => IntegerInterval {
                lower: shifted(other.lower, 1),
                upper: None,
            },
            Self::GreaterEqual => IntegerInterval {
                lower: other.lower,
                upper: None,
            },
            Self::Equal => other,
            // Excluding one value bounds nothing as an interval.
            Self::NotEqual => IntegerInterval::UNBOUNDED,
        }
    }

    /// The closed hull of the Real values of the subject that satisfy it for
    /// some value of `other`.
    fn real_bound(self, other: RealInterval) -> RealInterval {
        match self {
            Self::Less | Self::LessEqual => RealInterval {
                lower: None,
                upper: other.upper,
            },
            Self::Greater | Self::GreaterEqual => RealInterval {
                lower: other.lower,
                upper: None,
            },
            Self::Equal => other,
            Self::NotEqual => RealInterval::UNBOUNDED,
        }
    }
}

/// The Integer and Real values of one function, scalars and arrays alike, by
/// name and declared element type.
#[derive(Default)]
pub(super) struct ValueKinds {
    integers: HashSet<VarName>,
    reals: HashSet<VarName>,
}

impl ValueKinds {
    pub(super) fn of(function: &rumoca_core::Function, flat: &flat::Model) -> Self {
        let mut kinds = Self {
            integers: HashSet::new(),
            reals: HashSet::new(),
        };
        for value in function
            .inputs
            .iter()
            .chain(&function.outputs)
            .chain(&function.locals)
        {
            match effective_function_scalar_type(flat, value) {
                Some(dae::ScalarType::Integer) => {
                    kinds.integers.insert(VarName::new(&value.name));
                }
                Some(dae::ScalarType::Real) => {
                    kinds.reals.insert(VarName::new(&value.name));
                }
                _ => {}
            }
        }
        kinds
    }

    /// The fact scope of these kinds over `shapes`, before any capture.
    pub(super) fn scope<'a>(&'a self, shapes: &'a ShapeEnvironment) -> FactScope<'a> {
        self.scope_with(shapes, &[])
    }

    /// The fact scope of these kinds over `shapes`, where `generated` are
    /// captured.
    pub(super) fn scope_with<'a>(
        &'a self,
        shapes: &'a ShapeEnvironment,
        generated: &'a [function_returns::GeneratedBooleanDefinition],
    ) -> FactScope<'a> {
        FactScope {
            shapes,
            integers: &self.integers,
            reals: &self.reals,
            generated,
        }
    }
}

impl<'scope> FunctionValidationContext<'scope> {
    /// What value facts read in this function.
    pub(super) fn fact_scope(self) -> FactScope<'scope> {
        self.scalars
            .scope_with(self.shapes, self.generated_booleans)
    }
}

/// Drop every fact about `name` from one path's facts.
fn forget_in(facts: &mut PathFacts, name: &VarName) {
    if let Some(facts) = facts {
        facts.retain(|subject, _| &subject.name != name);
    }
}
