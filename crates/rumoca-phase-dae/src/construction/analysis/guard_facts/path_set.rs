//! The union of the paths that leave one value undefined.
//!
//! Each path keeps its own facts, so a later branch that contradicts the
//! facts of every path proves the value defined. Joining the paths into one
//! set of facts first would keep only what all of them prove: when one path
//! decided a Boolean and another never assigned it, the Boolean's facts would
//! be dropped and no later condition could contradict the pair.

use super::*;

/// The most paths kept apart; a larger union collapses into one hull, which
/// proves less and never more.
const MAX_PATHS: usize = 16;

/// The paths that leave a value undefined, as facts per path.
#[derive(Clone, PartialEq, Debug)]
pub(in crate::construction::analysis) struct PathSet(Vec<GuardFacts>);

impl PathSet {
    /// No path: nothing reaches here.
    pub(in crate::construction::analysis) fn empty() -> Self {
        Self(Vec::new())
    }

    /// One path.
    pub(in crate::construction::analysis) fn single(facts: GuardFacts) -> Self {
        let mut paths = Self(Vec::new());
        paths.push(facts);
        paths
    }

    fn push(&mut self, facts: GuardFacts) {
        if facts.is_unreachable() || self.0.contains(&facts) {
            return;
        }
        // Paths that agree on which values every Boolean may hold differ only
        // in numeric facts; they are one path with the facts both prove.
        match self
            .0
            .iter_mut()
            .find(|known| known.same_selected_values(&facts))
        {
            Some(known) => known.join_path(&facts),
            None => self.0.push(facts),
        }
    }

    /// Whether no execution reaches any of the paths.
    pub(in crate::construction::analysis) fn is_unreachable(&self) -> bool {
        self.0.is_empty()
    }

    /// Whether some path constrains nothing, so no later fact contradicts the
    /// set.
    pub(in crate::construction::analysis) fn is_trivial(&self) -> bool {
        self.0.iter().any(GuardFacts::is_trivial)
    }

    /// Also hold on the paths of `other`.
    pub(in crate::construction::analysis) fn join_path(&mut self, other: &Self) {
        for facts in &other.0 {
            self.push(facts.clone());
        }
        self.collapse_beyond_capacity();
    }

    fn collapse_beyond_capacity(&mut self) {
        if self.0.len() > MAX_PATHS {
            self.0 = vec![GuardFacts::join(&self.0)];
        }
    }

    /// The paths after `statement` runs.
    pub(in crate::construction::analysis) fn after(
        &mut self,
        statement: &rumoca_core::Statement,
        scope: FactScope<'_>,
    ) {
        self.split_correlated(
            &super::super::function_ranges::assigned_function_targets_with_generated(
                std::slice::from_ref(statement),
                scope.generated,
            ),
        );
        if let rumoca_core::Statement::Empty { span } = statement
            && let Some(capture) = scope.generated.iter().find(|capture| capture.span == *span)
        {
            let mut alternatives = self.assuming(&capture.value, true, scope);
            alternatives.join_path(&self.assuming(&capture.value, false, scope));
            *self = alternatives;
        }
        self.map_paths(|facts| facts.after(statement, scope));
    }

    /// The paths after the whole scalar `target` is assigned `value`.
    pub(in crate::construction::analysis) fn assign(
        &mut self,
        target: &VarName,
        value: &Expression,
        scope: FactScope<'_>,
    ) {
        self.split_correlated(&HashSet::from([target.as_str().to_string()]));
        self.map_paths(|facts| facts.assign(target.clone(), value, scope));
    }

    /// The paths at the head of a loop over `body`.
    pub(in crate::construction::analysis) fn loop_entry(
        &self,
        body: &[rumoca_core::Statement],
        binders: &[VarName],
        scope: FactScope<'_>,
    ) -> Self {
        let mut entry = self.clone();
        entry.split_correlated(
            &super::super::function_ranges::assigned_function_targets_with_generated(
                body,
                scope.generated,
            ),
        );
        entry.map_paths(|facts| *facts = facts.loop_entry_on_path(body, binders, scope));
        entry
    }

    /// The paths that enter branch `ordinal` of a conditional over
    /// `conditions` (`conditions.len()` is the fall-through).
    pub(in crate::construction::analysis) fn entering(
        &self,
        conditions: &[&Expression],
        ordinal: usize,
        scope: FactScope<'_>,
    ) -> Self {
        assert!(ordinal <= conditions.len());
        let mut remaining = self.clone();
        for (index, condition) in conditions.iter().enumerate() {
            if index == ordinal {
                return remaining.assuming(condition, true, scope);
            }
            remaining = remaining.assuming(condition, false, scope);
        }
        remaining
    }

    fn assuming(&self, condition: &Expression, value: bool, scope: FactScope<'_>) -> Self {
        match condition {
            Expression::Unary {
                op: OpUnary::Not,
                rhs,
                ..
            } => self.assuming(rhs, !value, scope),
            Expression::Binary {
                op: op @ (OpBinary::And | OpBinary::Or),
                lhs,
                rhs,
                ..
            } => self.assuming_junction((lhs, rhs), (matches!(op, OpBinary::And), value), scope),
            Expression::If {
                branches,
                else_branch,
                ..
            } => self.assuming_conditional(branches, else_branch, value, scope),
            _ => {
                let mut entered = Self::empty();
                for facts in &self.0 {
                    entered.push(
                        facts
                            .branch_entries(&[condition], scope)
                            .swap_remove(usize::from(!value)),
                    );
                }
                entered.collapse_beyond_capacity();
                entered
            }
        }
    }

    fn assuming_junction(
        &self,
        (lhs, rhs): (&Expression, &Expression),
        (conjunction, value): (bool, bool),
        scope: FactScope<'_>,
    ) -> Self {
        if conjunction == value {
            return self.assuming(lhs, value, scope).assuming(rhs, value, scope);
        }
        // Disjoint alternatives: the left operand decides the result, or
        // it does not and the right operand decides it.
        let mut decided = self.assuming(lhs, value, scope);
        let right = self
            .assuming(lhs, !value, scope)
            .assuming(rhs, value, scope);
        decided.join_path(&right);
        decided
    }

    fn assuming_conditional(
        &self,
        branches: &[(Expression, Expression)],
        fallback: &Expression,
        value: bool,
        scope: FactScope<'_>,
    ) -> Self {
        let mut remaining = self.clone();
        let mut entered = Self::empty();
        for (condition, selected) in branches {
            let branch = remaining
                .assuming(condition, true, scope)
                .assuming(selected, value, scope);
            entered.join_path(&branch);
            remaining = remaining.assuming(condition, false, scope);
        }
        entered.join_path(&remaining.assuming(fallback, value, scope));
        entered
    }

    /// Split each path on the Boolean selections that correlate with a value in
    /// `written`: a Boolean captured from a value proves that value only where
    /// the write has not happened, so the paths that took each arm are kept
    /// apart across the write.
    fn split_correlated(&mut self, written: &HashSet<String>) {
        let mut pending = std::mem::take(&mut self.0);
        while let Some(facts) = pending.pop() {
            match facts.correlated_boolean(written) {
                Some(name) => {
                    pending.extend([true, false].map(|value| facts.assuming_boolean(&name, value)))
                }
                None => self.push(facts),
            }
        }
    }

    fn map_paths(&mut self, mut update: impl FnMut(&mut GuardFacts)) {
        let paths = std::mem::take(&mut self.0);
        for mut facts in paths {
            update(&mut facts);
            self.push(facts);
        }
        self.collapse_beyond_capacity();
    }
}
