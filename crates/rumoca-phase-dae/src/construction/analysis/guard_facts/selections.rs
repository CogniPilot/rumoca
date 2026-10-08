//! The facts a local carries from the path that chose its value among
//! literals.
//!
//! A Boolean local `b := e` holds `true` exactly when `e` held, so the facts
//! `e` proves when true are known wherever `b` is read as true. A numeric
//! local assigned a literal or a choice of literals
//! (`valid := if c then 1.0 else 0.0`) holds each literal exactly on the path
//! that selects it, so a comparison of the local with a literal admits only
//! some arms and proves what holds on every admitted arm's path. A value no
//! arm holds is impossible, so a selection read where it admits no arm is an
//! unreachable path.

use super::*;
use std::cmp::Ordering;

/// One literal value a selection arm stores.
#[derive(Clone, Copy, PartialEq, Debug)]
pub(super) enum ArmValue {
    Boolean(bool),
    Integer(i64),
    Real(f64),
}

impl ArmValue {
    /// The value of a Boolean, Integer or Real literal or of a negated
    /// numeric literal; a NaN is no literal.
    pub(super) fn of_literal(expression: &Expression) -> Option<Self> {
        if let Expression::Literal {
            value: Literal::Boolean(value),
            ..
        } = expression
        {
            return Some(Self::Boolean(*value));
        }
        if let Some(value) = integer_literal(expression) {
            return Some(Self::Integer(value));
        }
        numeric_literal(expression)
            .filter(|value| !value.is_nan())
            .map(Self::Real)
    }

    pub(super) fn is_boolean(self, value: bool) -> bool {
        self == Self::Boolean(value)
    }

    /// The exact Real value of a numeric arm.
    pub(super) fn as_real(self) -> Option<f64> {
        match self {
            Self::Integer(value) => exact_real(value),
            Self::Real(value) => Some(value),
            Self::Boolean(_) => None,
        }
    }

    /// The exact order of two literals of one kind; numeric literals of
    /// either kind compare by value.
    pub(super) fn compare(self, other: Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Boolean(lhs), Self::Boolean(rhs)) => Some(lhs.cmp(&rhs)),
            (Self::Integer(lhs), Self::Integer(rhs)) => Some(lhs.cmp(&rhs)),
            _ => self.as_real()?.partial_cmp(&other.as_real()?),
        }
    }
}

/// An Integer as the Real of equal value, when that Real is exact.
pub(super) fn exact_real(value: i64) -> Option<f64> {
    const EXACT: i64 = 1 << 53;
    (-EXACT..=EXACT).contains(&value).then_some(value as f64)
}

/// One literal a local may hold, the facts of the path that stores it, and the
/// Booleans that path proves: `b := x and y` holds `true` only where `x` and
/// `y` held, so a read of `b` as true proves them too, for as long as the
/// values they name are unchanged.
#[derive(Clone, PartialEq, Debug)]
struct Arm {
    value: ArmValue,
    facts: PathFacts,
    implied: Vec<(VarName, bool)>,
}

/// The facts on the path of each literal a local may hold. A value with no
/// arm cannot be held.
#[derive(Clone, PartialEq, Debug)]
pub(super) struct Selection {
    arms: Vec<Arm>,
}

impl Selection {
    /// A Boolean known only to be `true` or `false`.
    pub(super) fn booleans() -> Self {
        let arm = |value| Arm {
            value: ArmValue::Boolean(value),
            facts: Some(BTreeMap::new()),
            implied: Vec::new(),
        };
        Self {
            arms: vec![arm(true), arm(false)],
        }
    }

    fn insert(&mut self, value: ArmValue, facts: PathFacts, implied: Vec<(VarName, bool)>) {
        if facts.is_none() {
            return;
        }
        match self.arms.iter_mut().find(|arm| arm.value == value) {
            Some(existing) => {
                existing.facts = disjoin(existing.facts.take(), facts);
                // Either path may have stored the value: only what both prove.
                existing.implied.retain(|entry| implied.contains(entry));
            }
            None => self.arms.push(Arm {
                value,
                facts,
                implied,
            }),
        }
    }

    /// The arms `admitted` keeps.
    pub(super) fn arms_when(
        &self,
        admitted: impl Fn(ArmValue) -> bool,
    ) -> impl Iterator<Item = (&PathFacts, &[(VarName, bool)])> {
        self.arms
            .iter()
            .filter(move |arm| admitted(arm.value))
            .map(|arm| (&arm.facts, arm.implied.as_slice()))
    }

    /// What holds on the path of any arm `admitted` keeps; `None` when it
    /// keeps none.
    pub(super) fn facts_when(&self, admitted: impl Fn(ArmValue) -> bool) -> PathFacts {
        self.arms_when(admitted)
            .fold(None, |joined, (facts, _)| disjoin(joined, facts.clone()))
    }

    /// The Booleans every arm `admitted` keeps proves.
    pub(super) fn implied_when(&self, admitted: impl Fn(ArmValue) -> bool) -> Vec<(VarName, bool)> {
        let mut arms = self.arms_when(admitted).map(|(_, implied)| implied);
        let Some(first) = arms.next() else {
            return Vec::new();
        };
        let mut common = first.to_vec();
        for implied in arms {
            common.retain(|entry| implied.contains(entry));
        }
        common
    }

    pub(super) fn retain(&mut self, admitted: impl Fn(ArmValue) -> bool) {
        self.arms.retain(|arm| admitted(arm.value));
    }

    /// Drop everything the arms record about the value `name`, which a write
    /// changes.
    pub(super) fn forget_name(&mut self, name: &VarName) {
        for arm in &mut self.arms {
            forget_in(&mut arm.facts, name);
            arm.implied.retain(|(implied, _)| implied != name);
        }
    }

    /// Widen to also hold on another path; a local with no selection on that
    /// path keeps none.
    pub(super) fn join(&mut self, other: Option<&Self>) -> bool {
        let Some(other) = other else {
            return false;
        };
        for arm in &other.arms {
            self.insert(arm.value, arm.facts.clone(), arm.implied.clone());
        }
        true
    }
}

/// The literal a value stores on each path that selects it, in order: one
/// literal, or an if-expression whose every arm is a literal, with the
/// condition that selects each arm.
/// One literal arm: the conditions (with the value each must have) that
/// select it, and the literal it stores.
pub(super) type LiteralArm<'a> = (Vec<(&'a Expression, bool)>, ArmValue);

pub(super) fn literal_arms(value: &Expression) -> Option<Vec<LiteralArm<'_>>> {
    let Expression::If {
        branches,
        else_branch,
        ..
    } = value
    else {
        return Some(vec![(Vec::new(), ArmValue::of_literal(value)?)]);
    };
    let mut arms = Vec::with_capacity(branches.len() + 1);
    let mut earlier = Vec::new();
    for (condition, arm) in branches {
        let mut selected = earlier.clone();
        selected.push((condition, true));
        arms.push((selected, ArmValue::of_literal(arm)?));
        earlier.push((condition, false));
    }
    arms.push((earlier, ArmValue::of_literal(else_branch)?));
    Some(arms)
}

/// The Booleans that hold wherever `expression` evaluates to `value`: the
/// local it reads, both operands of a true `and` (a Boolean if-expression with
/// a `false` else branch is one), both operands of a false `or`. A false `and`
/// (true `or`) leaves one operand false (true) when the other is known on this
/// path to hold the value that does not decide the result: `g := if a then b
/// else false` with `a` true makes a false `g` prove `b` false.
fn implied_booleans(
    expression: &Expression,
    value: bool,
    known: &dyn Fn(&VarName) -> Option<bool>,
    implied: &mut Vec<(VarName, bool)>,
) {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            let entry = (name.var_name().clone(), value);
            if !implied.contains(&entry) {
                implied.push(entry);
            }
        }
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => implied_booleans(rhs, !value, known, implied),
        Expression::Binary { op, lhs, rhs, .. } if matches!(op, OpBinary::And | OpBinary::Or) => {
            let conjunction = matches!(op, OpBinary::And);
            let operands = [lhs.as_ref(), rhs.as_ref()];
            implied_operands(operands, (conjunction, value), known, implied);
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } if matches!(
            else_branch.as_ref(),
            Expression::Literal {
                value: Literal::Boolean(false),
                ..
            }
        ) =>
        {
            // `if a then b else false` is `a and b`.
            for (condition, arm) in branches {
                if value || branches.len() == 1 {
                    implied_operands([condition, arm], (true, value), known, implied);
                }
            }
        }
        _ => {}
    }
}

/// The Booleans implied when the `and` (`conjunction`) or `or` of `operands`
/// evaluates to `value`.
fn implied_operands(
    operands: [&Expression; 2],
    (conjunction, value): (bool, bool),
    known: &dyn Fn(&VarName) -> Option<bool>,
    implied: &mut Vec<(VarName, bool)>,
) {
    if conjunction == value {
        // Both operands have this value.
        for operand in operands {
            implied_booleans(operand, value, known, implied);
        }
        return;
    }
    // One operand decides the result: when the other is known to hold the
    // value that does not, this one has the deciding value.
    let [lhs, rhs] = operands;
    for (operand, other) in [(lhs, rhs), (rhs, lhs)] {
        if known_boolean(other, known) == Some(conjunction) {
            implied_booleans(operand, value, known, implied);
        }
    }
}

/// The value `expression` is known to hold on this path.
fn known_boolean(
    expression: &Expression,
    known: &dyn Fn(&VarName) -> Option<bool>,
) -> Option<bool> {
    match expression {
        Expression::Literal {
            value: Literal::Boolean(value),
            ..
        } => Some(*value),
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => known(name.var_name()),
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => known_boolean(rhs, known).map(|value| !value),
        _ => None,
    }
}

impl GuardFacts {
    /// The selection a write of `value` gives `subject`: the two truth values
    /// of a Boolean, or the literal arms of a numeric value. Each arm also
    /// holds the facts of the path that wrote it.
    pub(super) fn selection_of(
        &self,
        subject: &FactSubject,
        value: &Expression,
        scope: FactScope<'_>,
    ) -> Option<Selection> {
        if scope.is_integer(subject) || scope.is_real(subject) {
            return self.literal_selection(value, scope);
        }
        let mut selection = Selection { arms: Vec::new() };
        let when_true = self.facts(value, true, scope);
        let when_false = self.facts(value, false, scope);
        let known = |name: &VarName| self.known_boolean(name);
        let mut implied_true = Vec::new();
        implied_booleans(value, true, &known, &mut implied_true);
        let mut implied_false = Vec::new();
        implied_booleans(value, false, &known, &mut implied_false);
        if trivial(&when_true)
            && trivial(&when_false)
            && trivial(&self.path)
            && implied_true.is_empty()
            && implied_false.is_empty()
        {
            return None;
        }
        // The path facts that held at the write are implied by either value.
        selection.insert(
            ArmValue::Boolean(true),
            conjoin(self.path.clone(), when_true),
            implied_true,
        );
        selection.insert(
            ArmValue::Boolean(false),
            conjoin(self.path.clone(), when_false),
            implied_false,
        );
        Some(selection)
    }

    /// The literal arms of a numeric value, each with the facts of the path
    /// that selects it; `None` unless at least two arms are literals.
    fn literal_selection(&self, value: &Expression, scope: FactScope<'_>) -> Option<Selection> {
        let arms = literal_arms(value)?;
        if arms.len() < 2 {
            return None;
        }
        let mut selection = Selection { arms: Vec::new() };
        for (conditions, literal) in arms {
            let path = conditions
                .into_iter()
                .fold(self.clone(), |path, (condition, holds)| {
                    path.assuming(condition, holds, scope)
                });
            selection.insert(literal, path.path, Vec::new());
        }
        Some(selection)
    }
}

impl Selection {
    /// Whether both selections admit the same literal values.
    pub(super) fn same_values(&self, other: &Self) -> bool {
        self.arms.len() == other.arms.len()
            && self.arms.iter().all(|arm| {
                other
                    .arms
                    .iter()
                    .any(|candidate| candidate.value == arm.value)
            })
    }
}

impl Selection {
    /// Whether this selection holds both truth values and some arm proves a
    /// Boolean among `names`.
    pub(super) fn correlates_with(&self, names: &HashSet<String>) -> bool {
        self.arms.len() >= 2
            && self.arms.iter().any(|arm| {
                matches!(arm.value, ArmValue::Boolean(_))
                    && arm
                        .implied
                        .iter()
                        .any(|(implied, _)| names.contains(implied.as_str()))
            })
    }

    /// Whether no arm is left.
    pub(super) fn is_empty(&self) -> bool {
        self.arms.is_empty()
    }
}

impl GuardFacts {
    /// The value the Boolean `name` holds on every path reaching here, when
    /// one literal arm is left.
    fn known_boolean(&self, name: &VarName) -> Option<bool> {
        match self.selections.get(name)?.arms.as_slice() {
            [
                Arm {
                    value: ArmValue::Boolean(value),
                    ..
                },
            ] => Some(*value),
            _ => None,
        }
    }
}
