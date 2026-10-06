//! Value facts proven by the conditions and literal assignments that reach a
//! program point of a function body.
//!
//! MLS 3.6 §11.2.6 runs a branch only when its condition holds and every
//! earlier condition of the same `if` failed, so inside the branch those
//! relations are facts about the values they compare: `radius <= 4` bounds
//! `radius` above. An assignment of a literal fixes its target's value. A
//! Boolean local carries the facts of the value it was assigned
//! (`linear := radius > 4` makes `not linear` imply `radius <= 4`), including
//! the facts of the path that assigned it. Every fact is forgotten when a
//! statement writes a value it mentions, so a fact is only ever read while the
//! values it relates are the ones it was proven about.
//!
//! Integer facts are exact intervals (`rumoca_core::IntegerInterval`). Real
//! facts come only from literal assignments and comparisons with literals,
//! whose values are never NaN, kept as closed intervals
//! (`rumoca_core::RealInterval`): a strict comparison keeps its closed hull, a
//! superset, so an empty Real interval proves a contradiction and a nonempty
//! one proves nothing more than its hull. A computed Real value carries no
//! fact.
//!
//! The facts bound compact dependent domains and while-loop pass counts, and
//! prove that a path on which a value is undefined cannot reach a later
//! branch. They never select a branch or fold a value.

use super::*;
use rumoca_core::{IntegerInterval, RealInterval};
use std::collections::BTreeMap;

/// The proven set of one scalar's values.
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

/// Facts that hold on one path: a fact per scalar. `None` is an unreachable
/// path, where every fact holds.
type PathFacts = Option<BTreeMap<VarName, ValueFact>>;

fn conjoin(lhs: PathFacts, rhs: PathFacts) -> PathFacts {
    let (mut lhs, rhs) = (lhs?, rhs?);
    for (name, fact) in rhs {
        let merged = match lhs.get(&name) {
            Some(existing) => existing.meet(fact),
            None => fact,
        };
        if merged.is_empty() {
            return None;
        }
        lhs.insert(name, merged);
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
            .filter_map(|(name, fact)| Some((name.clone(), fact.hull(*rhs.get(name)?)?)))
            .collect(),
    )
}

/// The facts implied by each value of one Boolean local.
#[derive(Clone, PartialEq, Debug)]
struct BooleanFacts {
    when_true: PathFacts,
    when_false: PathFacts,
}

impl BooleanFacts {
    /// Widen to what holds on another path too; a Boolean with no facts on
    /// that path keeps none.
    fn join(&mut self, other: Option<&Self>) -> bool {
        let Some(other) = other else {
            return false;
        };
        self.when_true = disjoin(self.when_true.take(), other.when_true.clone());
        self.when_false = disjoin(self.when_false.take(), other.when_false.clone());
        true
    }
}

/// What a fact owner reads beside the facts: the proven extents and values,
/// and which scalar names are Integer and which are Real.
#[derive(Clone, Copy)]
pub(super) struct FactScope<'a> {
    pub(super) shapes: &'a ShapeEnvironment,
    pub(super) integers: &'a HashSet<VarName>,
    pub(super) reals: &'a HashSet<VarName>,
}

/// The facts proven at one program point of a function body.
#[derive(Clone, PartialEq, Debug)]
pub(super) struct GuardFacts {
    path: PathFacts,
    booleans: BTreeMap<VarName, BooleanFacts>,
}

impl GuardFacts {
    /// No facts.
    pub(super) fn entry() -> Self {
        Self {
            path: Some(BTreeMap::new()),
            booleans: BTreeMap::new(),
        }
    }

    /// Whether no execution reaches this path: its facts contradict.
    pub(super) fn is_unreachable(&self) -> bool {
        self.path.is_none()
    }

    /// Whether these facts constrain nothing, so no later fact can contradict
    /// them.
    pub(super) fn is_trivial(&self) -> bool {
        self.path.as_ref().is_some_and(BTreeMap::is_empty) && self.booleans.is_empty()
    }

    /// Conjoin every proven Integer path fact with what `shapes` already
    /// proves.
    pub(super) fn refine(&self, shapes: &mut ShapeEnvironment) {
        for (name, fact) in self.path.iter().flatten() {
            if let ValueFact::Integer(interval) = fact {
                shapes.refine_integer_interval(name.clone(), *interval);
            }
        }
    }

    /// The largest value Integer `name` can hold on this path, when a fact
    /// bounds it above; an unreachable path bounds every value.
    pub(super) fn upper_bound(&self, name: &VarName) -> Option<i64> {
        match &self.path {
            None => Some(i64::MIN),
            Some(facts) => match facts.get(name)? {
                ValueFact::Integer(interval) => interval.upper,
                ValueFact::Real(_) => None,
            },
        }
    }

    /// The facts at the start of each branch of an `if`, and on its
    /// fall-through path when it has no `else` (the last entry).
    pub(super) fn branch_entries(
        &self,
        conditions: &[&Expression],
        scope: FactScope<'_>,
    ) -> Vec<Self> {
        let mut entries = Vec::with_capacity(conditions.len() + 1);
        let mut remaining = self.clone();
        for condition in conditions {
            entries.push(remaining.assuming(condition, true, scope));
            remaining = remaining.assuming(condition, false, scope);
        }
        entries.push(remaining);
        entries
    }

    /// The facts after paths that rejoin.
    pub(super) fn join(paths: &[Self]) -> Self {
        let (first, rest) = paths.split_first().expect("a join has at least one path");
        let mut joined = first.clone();
        for path in rest {
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
        self.booleans
            .retain(|name, facts| facts.join(other.booleans.get(name)));
    }

    /// The facts after `statement` runs from this point.
    pub(super) fn after(&mut self, statement: &rumoca_core::Statement, scope: FactScope<'_>) {
        if let rumoca_core::Statement::Assignment { comp, value, .. } = statement
            && let [part] = comp.parts()
            && part.subs.is_empty()
        {
            let target = comp.to_var_name();
            self.assign(target, value, scope);
            return;
        }
        for name in assigned_function_targets(std::slice::from_ref(statement)) {
            self.forget(&VarName::new(name));
        }
    }

    /// The facts after the whole scalar `target` is assigned `value`.
    pub(super) fn assign(&mut self, target: VarName, value: &Expression, scope: FactScope<'_>) {
        // The value is read before the write, so its facts use the facts that
        // hold before the target changes.
        let when_true = self.facts(value, true, scope);
        let when_false = self.facts(value, false, scope);
        self.forget(&target);
        if let (Some(path), Some(fact)) = (&mut self.path, literal_fact(&target, value, scope)) {
            path.insert(target.clone(), fact);
        }
        let trivial = |facts: &PathFacts| facts.as_ref().is_some_and(BTreeMap::is_empty);
        // The path facts that held at the write are implied by either value.
        if !trivial(&when_true) || !trivial(&when_false) || !trivial(&self.path) {
            let facts = BooleanFacts {
                when_true: conjoin(self.path.clone(), when_true),
                when_false: conjoin(self.path.clone(), when_false),
            };
            self.booleans.insert(target, facts);
        }
    }

    /// The facts on entry to every iteration of a loop whose body is `body`,
    /// and after the loop: a value the body writes may hold any iteration's
    /// value, and a binder (MLS §11.2.2) shadows every fact about its name.
    pub(super) fn loop_entry(&self, body: &[rumoca_core::Statement], binders: &[VarName]) -> Self {
        let mut entry = self.clone();
        for name in assigned_function_targets(body) {
            entry.forget(&VarName::new(name));
        }
        for binder in binders {
            entry.forget(binder);
        }
        entry
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
            assumed.booleans.clear();
            return assumed;
        }
        // A Boolean local read as the condition is known to have this value.
        if let Some(name) = boolean_operand(condition, value) {
            let known = assumed.booleans.entry(name.0).or_insert(BooleanFacts {
                when_true: Some(BTreeMap::new()),
                when_false: Some(BTreeMap::new()),
            });
            if name.1 {
                known.when_false = None;
            } else {
                known.when_true = None;
            }
        }
        assumed
    }

    fn forget(&mut self, name: &VarName) {
        let drop_name = |facts: &mut PathFacts| {
            if let Some(facts) = facts {
                facts.remove(name);
            }
        };
        drop_name(&mut self.path);
        self.booleans.remove(name);
        for facts in self.booleans.values_mut() {
            drop_name(&mut facts.when_true);
            drop_name(&mut facts.when_false);
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
            } => {
                let lhs = self.facts(lhs, value, scope);
                let rhs = self.facts(rhs, value, scope);
                // `a and b` is true when both are, false when either is.
                if matches!(op, OpBinary::And) == value {
                    conjoin(lhs, rhs)
                } else {
                    disjoin(lhs, rhs)
                }
            }
            Expression::Binary { op, lhs, rhs, .. } => Some(
                self.comparison(op, lhs, rhs, value, scope)
                    .unwrap_or_default(),
            ),
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.booleans.get(name.var_name()).map_or(
                Some(BTreeMap::new()),
                |facts| match value {
                    true => facts.when_true.clone(),
                    false => facts.when_false.clone(),
                },
            ),
            _ => Some(BTreeMap::new()),
        }
    }

    /// The fact a relation between a scalar and a bounded expression proves
    /// for that scalar: an Integer interval from the proven interval of the
    /// other side, or a Real interval from a literal other side.
    fn comparison(
        &self,
        op: &OpBinary,
        lhs: &Expression,
        rhs: &Expression,
        value: bool,
        scope: FactScope<'_>,
    ) -> Option<BTreeMap<VarName, ValueFact>> {
        let relation = Relation::of(op)?;
        let relation = if value { relation } else { relation.negated() };
        let mut facts = BTreeMap::new();
        for (subject, other, relation) in [(lhs, rhs, relation), (rhs, lhs, relation.mirrored())] {
            let Some(name) = scalar_reference(subject) else {
                continue;
            };
            let fact = if scope.integers.contains(&name) {
                ValueFact::Integer(
                    relation.integer_bound(scope.shapes.proven_integer_interval(other)),
                )
            } else if scope.reals.contains(&name)
                && let Some(literal) = numeric_literal(other)
            {
                ValueFact::Real(relation.real_bound(literal))
            } else {
                continue;
            };
            if !fact.is_unbounded() {
                facts.insert(name, fact);
            }
        }
        Some(facts)
    }
}

fn scalar_reference(expression: &Expression) -> Option<VarName> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    subscripts.is_empty().then(|| name.var_name().clone())
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

/// The exact value a literal assignment gives an Integer or Real target.
fn literal_fact(target: &VarName, value: &Expression, scope: FactScope<'_>) -> Option<ValueFact> {
    if scope.integers.contains(target) {
        return integer_literal(value)
            .map(|exact| ValueFact::Integer(IntegerInterval::exact(exact)));
    }
    if scope.reals.contains(target) {
        return RealInterval::exact(numeric_literal(value)?).map(ValueFact::Real);
    }
    None
}

/// `b` or `not b` for a scalar reference `b`, with the value `b` has when the
/// condition evaluates to `value`.
fn boolean_operand(condition: &Expression, value: bool) -> Option<(VarName, bool)> {
    match condition {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some((name.var_name().clone(), value)),
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => boolean_operand(rhs, !value),
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

    /// The closed hull of the Real values of the subject that satisfy it
    /// against the literal `other`.
    fn real_bound(self, other: f64) -> RealInterval {
        match self {
            Self::Less | Self::LessEqual => RealInterval {
                lower: None,
                upper: Some(other),
            },
            Self::Greater | Self::GreaterEqual => RealInterval {
                lower: Some(other),
                upper: None,
            },
            Self::Equal => RealInterval::exact(other).unwrap_or(RealInterval::UNBOUNDED),
            Self::NotEqual => RealInterval::UNBOUNDED,
        }
    }
}

#[cfg(test)]
mod tests;

/// The scalar Integer and Real values of one function, by name.
pub(super) struct ScalarKinds {
    integers: HashSet<VarName>,
    reals: HashSet<VarName>,
}

impl ScalarKinds {
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
            .filter(|value| value.effective_type.dimensions().is_empty())
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
}

impl<'scope> FunctionValidationContext<'scope> {
    /// What value facts read in this function.
    pub(super) fn fact_scope(self) -> FactScope<'scope> {
        FactScope {
            shapes: self.shapes,
            integers: &self.scalars.integers,
            reals: &self.scalars.reals,
        }
    }
}
