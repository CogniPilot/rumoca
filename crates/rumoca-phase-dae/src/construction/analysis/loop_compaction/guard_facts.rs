//! Integer facts proven by the conditions that guard a program point.
//!
//! MLS 3.6 §11.2.6 runs a branch only when its condition holds and every
//! earlier condition of the same `if` failed, so inside the branch those
//! relations are facts about the values they compare: `radius <= 4` bounds
//! `radius` above. A Boolean local carries the facts of the value it was
//! assigned (`linear := radius > 4` makes `not linear` imply `radius <= 4`),
//! including the facts of the path that assigned it. Every fact is forgotten
//! when a statement writes a value it mentions, so a fact is only ever read
//! while the values it relates are the ones it was proven about.
//!
//! These facts are one source of the Integer intervals that bound a compact
//! dependent domain; they never select a branch or fold a value.

use super::*;
use crate::construction::function_shapes::IntegerInterval;

/// Facts that hold on one path: an interval per scalar Integer. `None` is an
/// unreachable path, where every fact holds.
type PathFacts = Option<BTreeMap<VarName, IntegerInterval>>;

fn conjoin(lhs: PathFacts, rhs: PathFacts) -> PathFacts {
    let (mut lhs, rhs) = (lhs?, rhs?);
    for (name, interval) in rhs {
        let merged = lhs
            .get(&name)
            .copied()
            .unwrap_or(IntegerInterval::UNBOUNDED)
            .meet(interval);
        lhs.insert(name, merged);
    }
    Some(lhs)
}

/// What holds on either of two paths: a fact both paths prove, widened to
/// cover both intervals.
fn disjoin(lhs: PathFacts, rhs: PathFacts) -> PathFacts {
    let (Some(lhs), Some(rhs)) = (&lhs, &rhs) else {
        return lhs.or(rhs);
    };
    Some(
        lhs.iter()
            .filter_map(|(name, interval)| {
                let joined = interval.hull(*rhs.get(name)?);
                (!joined.is_unbounded()).then(|| (name.clone(), joined))
            })
            .collect(),
    )
}

/// The facts implied by each value of one Boolean local.
#[derive(Clone, PartialEq)]
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

/// The facts proven at one program point of a function body.
#[derive(Clone)]
pub(super) struct GuardFacts<'scope> {
    integers: &'scope HashSet<VarName>,
    path: PathFacts,
    booleans: BTreeMap<VarName, BooleanFacts>,
}

impl<'scope> GuardFacts<'scope> {
    /// No facts, for a body whose scalar Integer values are `integers`.
    pub(super) fn entry(integers: &'scope HashSet<VarName>) -> Self {
        Self {
            integers,
            path: Some(BTreeMap::new()),
            booleans: BTreeMap::new(),
        }
    }

    /// Conjoin every proven path fact with what `shapes` already proves.
    pub(super) fn refine(&self, shapes: &mut ShapeEnvironment) {
        for (name, interval) in self.path.iter().flatten() {
            shapes.refine_integer_interval(name.clone(), *interval);
        }
    }

    /// The facts at the start of each branch of an `if`, and on its
    /// fall-through path when it has no `else` (the last entry).
    pub(super) fn branch_entries(
        &self,
        conditions: &[&Expression],
        shapes: &ShapeEnvironment,
    ) -> Vec<Self> {
        let mut entries = Vec::with_capacity(conditions.len() + 1);
        let mut remaining = self.clone();
        for condition in conditions {
            entries.push(remaining.assuming(condition, true, shapes));
            remaining = remaining.assuming(condition, false, shapes);
        }
        entries.push(remaining);
        entries
    }

    /// The facts after paths that rejoin.
    pub(super) fn join(paths: &[Self]) -> Self {
        let (first, rest) = paths.split_first().expect("a join has at least one path");
        let mut joined = first.clone();
        for path in rest {
            joined.path = disjoin(joined.path, path.path.clone());
            joined
                .booleans
                .retain(|name, facts| facts.join(path.booleans.get(name)));
        }
        joined
    }

    /// The facts after `statement` runs from this point, with `shapes`
    /// proving the intervals of the expressions it assigns.
    pub(super) fn after(&mut self, statement: &rumoca_core::Statement, shapes: &ShapeEnvironment) {
        if let rumoca_core::Statement::Assignment { comp, value, .. } = statement
            && let [part] = comp.parts()
            && part.subs.is_empty()
        {
            let target = comp.to_var_name();
            // The value is read before the write, so its facts use the facts
            // that hold before the target changes.
            let when_true = self.facts(value, true, shapes);
            let when_false = self.facts(value, false, shapes);
            self.forget(&target);
            let trivial = |facts: &PathFacts| facts.as_ref().is_some_and(BTreeMap::is_empty);
            // The path facts that held at the write are implied by either value.
            if !trivial(&when_true) || !trivial(&when_false) || !trivial(&self.path) {
                let facts = BooleanFacts {
                    when_true: conjoin(self.path.clone(), when_true),
                    when_false: conjoin(self.path.clone(), when_false),
                };
                self.booleans.insert(target, facts);
            }
            return;
        }
        for name in assigned_function_targets(std::slice::from_ref(statement)) {
            self.forget(&VarName::new(name));
        }
    }

    /// The facts on entry to every iteration of a loop whose body is `body`,
    /// and after the loop: a value the body writes may hold any iteration's
    /// value, and a binder (MLS §11.2.2) shadows every fact about its name.
    pub(super) fn loop_entry(
        &self,
        body: &[rumoca_core::Statement],
        binders: &[rumoca_core::ForIndex],
    ) -> Self {
        let mut entry = self.clone();
        for name in assigned_function_targets(body) {
            entry.forget(&VarName::new(name));
        }
        for binder in binders {
            entry.forget(&VarName::new(&binder.ident));
        }
        entry
    }

    fn assuming(&self, condition: &Expression, value: bool, shapes: &ShapeEnvironment) -> Self {
        let mut assumed = self.clone();
        assumed.path = conjoin(assumed.path, self.facts(condition, value, shapes));
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
    fn facts(&self, expression: &Expression, value: bool, shapes: &ShapeEnvironment) -> PathFacts {
        match expression {
            Expression::Literal {
                value: Literal::Boolean(literal),
                ..
            } => (*literal == value).then(BTreeMap::new),
            Expression::Unary {
                op: OpUnary::Not,
                rhs,
                ..
            } => self.facts(rhs, !value, shapes),
            Expression::Binary {
                op: op @ (OpBinary::And | OpBinary::Or),
                lhs,
                rhs,
                ..
            } => {
                let lhs = self.facts(lhs, value, shapes);
                let rhs = self.facts(rhs, value, shapes);
                // `a and b` is true when both are, false when either is.
                if matches!(op, OpBinary::And) == value {
                    conjoin(lhs, rhs)
                } else {
                    disjoin(lhs, rhs)
                }
            }
            Expression::Binary { op, lhs, rhs, .. } => Some(
                self.comparison(op, lhs, rhs, value, shapes)
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

    /// The interval a relation between a scalar Integer and a bounded Integer
    /// expression proves for that scalar.
    fn comparison(
        &self,
        op: &OpBinary,
        lhs: &Expression,
        rhs: &Expression,
        value: bool,
        shapes: &ShapeEnvironment,
    ) -> Option<BTreeMap<VarName, IntegerInterval>> {
        let relation = Relation::of(op)?;
        let relation = if value { relation } else { relation.negated() };
        let mut facts = BTreeMap::new();
        for (subject, other, relation) in [(lhs, rhs, relation), (rhs, lhs, relation.mirrored())] {
            let Some(name) = self.integer_scalar(subject) else {
                continue;
            };
            let interval = relation.bound(shapes.proven_integer_interval(other));
            if !interval.is_unbounded() {
                facts.insert(name, interval);
            }
        }
        Some(facts)
    }

    fn integer_scalar(&self, expression: &Expression) -> Option<VarName> {
        let Expression::VarRef {
            name, subscripts, ..
        } = expression
        else {
            return None;
        };
        let name = name.var_name();
        (subscripts.is_empty() && self.integers.contains(name)).then(|| name.clone())
    }
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

/// `subject <relation> other` for Integer operands.
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

    /// The values of the subject that satisfy it for some value of `other`.
    fn bound(self, other: IntegerInterval) -> IntegerInterval {
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
}

#[cfg(test)]
mod tests;
