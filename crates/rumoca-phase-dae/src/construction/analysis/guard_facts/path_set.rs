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
const MAX_PATHS: usize = 8;

/// The paths that leave a value undefined, as facts per path.
#[derive(Clone, PartialEq, Debug)]
pub(in crate::construction::analysis) struct UndefinedPaths(Vec<GuardFacts>);

impl UndefinedPaths {
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
        self.0.push(facts);
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
        self.map_paths(|facts| facts.after(statement, scope));
    }

    /// The paths after the whole scalar `target` is assigned `value`.
    pub(in crate::construction::analysis) fn assign(
        &mut self,
        target: &VarName,
        value: &Expression,
        scope: FactScope<'_>,
    ) {
        self.map_paths(|facts| facts.assign(target.clone(), value, scope));
    }

    /// The paths at the head of a loop over `body`.
    pub(in crate::construction::analysis) fn loop_entry(
        &self,
        body: &[rumoca_core::Statement],
        binders: &[VarName],
    ) -> Self {
        let mut entry = self.clone();
        entry.map_paths(|facts| *facts = facts.loop_entry(body, binders));
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
        let mut entered = Self(Vec::new());
        for facts in &self.0 {
            entered.push(facts.branch_entries(conditions, scope).swap_remove(ordinal));
        }
        entered.collapse_beyond_capacity();
        entered
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
