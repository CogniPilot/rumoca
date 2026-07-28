//! Target-keyed candidate index for symbolic substitution application.
//!
//! Applying a cumulative substitution list to an expression is quadratic when
//! every substitution re-walks the whole expression. This module keys each
//! substitution by the [`rumoca_core::VarNameId`]s its target can possibly
//! match, so one walk of the expression yields the small candidate set that
//! still has to be tested with the exact predicate.
//!
//! Contract: the candidate set is a strict **superset** of what the sequential
//! `for sub in substitutions` loop matches, and candidates are consumed in
//! ascending position order, so the filtered walk is observationally identical
//! to the sequential one. The `HashMap` below is a phase-internal lookup only —
//! candidates are always read back from the ascending position vector, so no
//! map iteration order is ever observable in compiler output (SPEC_0021
//! deterministic-collections carve-out).

use std::collections::HashMap;

use rumoca_core::{Expression, ExpressionVisitor, Reference, Subscript, VarNameId};
use rumoca_ir_dae::{for_each_unknown_match_key, for_each_var_ref_match_key};

use super::{Substitution, SubstitutionApplicationPlan, scalar_var_ref_key_from_reference};

/// Maps a candidate match key to the substitution positions registered under it.
#[derive(Debug, Clone, Default)]
pub(super) struct SubstitutionIndex {
    by_key: HashMap<VarNameId, Vec<u32>>,
    len: u32,
}

impl SubstitutionIndex {
    pub(super) fn new(substitutions: &[Substitution]) -> Self {
        let mut index = Self::default();
        for substitution in substitutions {
            index.push(substitution);
        }
        index
    }

    /// Register `substitution` at the next position.
    ///
    /// Keys come from both the eliminated variable name and, when present, the
    /// structured component reference: `embedded_alias_indices_for_substitution`
    /// matches on `var_ref`'s base rather than on `var_name`.
    pub(super) fn push(&mut self, substitution: &Substitution) {
        let position = self.len;
        self.len += 1;
        let mut keys = Vec::new();
        for_each_unknown_match_key(&substitution.var_name, |key| keys.push(key));
        if let Some(var_ref) = &substitution.var_ref {
            for_each_var_ref_match_key(var_ref, |key| keys.push(key));
            if let Some((base, _)) = scalar_var_ref_key_from_reference(var_ref) {
                keys.push(base.var_name().id());
            }
        }
        debug_assert!(
            !keys.is_empty(),
            "every substitution registers at least its variable-name key"
        );
        for key in keys {
            let positions = self.by_key.entry(key).or_default();
            if positions.last() != Some(&position) {
                positions.push(position);
            }
        }
    }

    pub(super) fn len(&self) -> usize {
        self.len as usize
    }

    /// Candidate positions whose target may occur anywhere in `expr`.
    ///
    /// Building the candidate set costs one walk of `expr` plus work
    /// proportional to the candidates actually collected. Nothing here is
    /// proportional to `self.len`, so applying a cumulative substitution list
    /// to one expression never pays a per-expression O(list length) term.
    pub(super) fn candidates_for_expression(&self, expr: &Expression) -> SubstitutionCandidates {
        let mut candidates = SubstitutionCandidates::default();
        self.extend_candidates(expr, None, &mut candidates);
        candidates
    }

    /// Add candidates found in `expr` whose position is strictly after `after`.
    ///
    /// Positions at or below `after` were already tested against an expression
    /// that did not yet contain `expr`, exactly as in the sequential loop, so
    /// re-adding them would not be order-equivalent.
    pub(super) fn extend_candidates_after(
        &self,
        expr: &Expression,
        after: u32,
        out: &mut SubstitutionCandidates,
    ) {
        self.extend_candidates(expr, Some(after), out);
    }

    fn extend_candidates(
        &self,
        expr: &Expression,
        after: Option<u32>,
        out: &mut SubstitutionCandidates,
    ) {
        if self.by_key.is_empty() {
            return;
        }
        CandidateCollector {
            index: self,
            after,
            out,
        }
        .visit_expression(expr);
    }

    fn collect_reference(
        &self,
        name: &Reference,
        after: Option<u32>,
        out: &mut SubstitutionCandidates,
    ) {
        let by_key = &self.by_key;
        let mut select = |key: VarNameId| {
            let Some(positions) = by_key.get(&key) else {
                return;
            };
            let start = after.map_or(0, |after| positions.partition_point(|p| *p <= after));
            for &position in &positions[start..] {
                out.insert(position);
            }
        };
        for_each_var_ref_match_key(name, &mut select);
        if let Some((base, _)) = scalar_var_ref_key_from_reference(name) {
            select(base.var_name().id());
        }
    }
}

/// Ascending, de-duplicated substitution positions awaiting the exact predicate.
///
/// The ascending vector *is* the membership set: the binary search that finds
/// the insertion point also decides whether the position is already present.
/// A side `Vec<bool>` sized by the substitution list was the obvious
/// alternative, but it re-allocates and re-zeroes `n_subs` bytes for every
/// expression, which reinstates the O(n_eq * n_subs) term the candidate index
/// exists to remove. All bookkeeping here is O(candidates collected).
#[derive(Debug, Clone, Default)]
pub(super) struct SubstitutionCandidates {
    positions: Vec<u32>,
}

impl SubstitutionCandidates {
    /// Insert `position`, keeping the vector ascending and de-duplicated.
    ///
    /// One binary search answers both questions - whether the position is
    /// already a candidate and where it belongs - so no side table sized by the
    /// substitution list is ever allocated, let alone re-zeroed per expression.
    /// Positions arrive in ascending order per match key, so the common case is
    /// an append.
    fn insert(&mut self, position: u32) {
        if let Err(at) = self.positions.binary_search(&position) {
            self.positions.insert(at, position);
        }
    }

    /// The `cursor`-th candidate in ascending order, if any.
    ///
    /// Insertions made while iterating are always strictly greater than the
    /// position currently held, so they land at or after `cursor` and never
    /// disturb positions already consumed.
    pub(super) fn position_at(&self, cursor: usize) -> Option<u32> {
        self.positions.get(cursor).copied()
    }

    #[cfg(test)]
    pub(super) fn contains(&self, position: u32) -> bool {
        self.positions.binary_search(&position).is_ok()
    }

    #[cfg(test)]
    pub(super) fn positions(&self) -> &[u32] {
        &self.positions
    }

    /// Total slots this candidate set keeps membership bookkeeping for.
    ///
    /// Pinned by `candidate_bookkeeping_is_independent_of_substitution_list_length`:
    /// it must depend only on how many candidates were actually collected, never
    /// on how long the substitution list is.
    #[cfg(test)]
    pub(super) fn tracked_slot_count(&self) -> usize {
        self.positions.len()
    }
}

struct CandidateCollector<'a> {
    index: &'a SubstitutionIndex,
    after: Option<u32>,
    out: &'a mut SubstitutionCandidates,
}

impl ExpressionVisitor for CandidateCollector<'_> {
    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        self.index.collect_reference(name, self.after, self.out);
        self.walk_var_ref(name, subscripts);
    }
}

/// A substitution list paired with the application plan built alongside it.
///
/// Growing the list through [`PlannedSubstitutions::push`] keeps the aggregate
/// alias groups, complex-field groups and candidate index up to date, so a
/// caller that applies the cumulative list once per equation no longer rebuilds
/// the whole plan per equation.
pub(super) struct PlannedSubstitutions {
    substitutions: Vec<Substitution>,
    plan: SubstitutionApplicationPlan,
}

impl PlannedSubstitutions {
    pub(super) fn new(aggregate_constructor: Option<&Reference>) -> Self {
        Self {
            substitutions: Vec::new(),
            plan: SubstitutionApplicationPlan::new(&[], aggregate_constructor),
        }
    }

    pub(super) fn push(&mut self, substitution: Substitution) {
        self.plan.push(&substitution);
        self.substitutions.push(substitution);
    }

    pub(super) fn as_slice(&self) -> &[Substitution] {
        &self.substitutions
    }

    pub(super) fn plan(&self) -> &SubstitutionApplicationPlan {
        &self.plan
    }

    pub(super) fn len(&self) -> usize {
        self.substitutions.len()
    }

    pub(super) fn into_vec(self) -> Vec<Substitution> {
        self.substitutions
    }

    /// Detached copy used for speculative trials that must not leak rejected
    /// aliases back into the committed plan.
    pub(super) fn trial_clone(&self) -> Self {
        Self {
            substitutions: self.substitutions.clone(),
            plan: self.plan.clone(),
        }
    }
}
