//! One evaluation of each eagerly demanded call and conditional per scope.
//!
//! A scope (a region body or a function body) computes every expression its
//! results demand unconditionally, whatever region those expressions are
//! reached from: a call in a result, in a call argument, or in the first
//! condition of an if-expression is evaluated by every execution of the scope.
//! Lowering such a call where the first consumer happens to sit (inside the
//! lazy branch of a sibling conditional) would evaluate it once per consumer
//! region. This module instead keeps one value per demanded call in the scope:
//! the call is issued at its first source-order use (or, for a nested region
//! that captures it earlier, when the region is built), so every later reader
//! captures that value (`environment_requirements`), and the sibling
//! conditionals that share their conditions are fused into one region pair,
//! so the calls only their arms demand are evaluated once per selected arm.
//!
//! The decision is made from the DAE expression graph at construction. Nothing
//! is cached or compared at run time (SPEC_0007, SPEC_0036).

use std::collections::{HashMap, HashSet};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{ExpressionLowerer, total_conditionals::is_total};

/// The calls and conditionals every execution of one scope computes.
#[derive(Default)]
struct EagerDemand<'dae> {
    calls: Vec<dae::ExprId<'dae>>,
    conditionals: Vec<dae::ExprId<'dae>>,
}

impl<'dae> EagerDemand<'dae> {
    /// Record one expression the walk reaches and report whether the walk
    /// continues into its operands.
    fn record(
        &mut self,
        id: dae::ExprId<'dae>,
        operation: dae::ExpressionOperation<'dae>,
        frontier: &mut Vec<dae::ExprId<'dae>>,
    ) -> bool {
        match operation {
            dae::ExpressionOperation::Call { .. } => {
                self.calls.push(id);
                true
            }
            dae::ExpressionOperation::Conditional(operands) => {
                self.conditionals.push(id);
                frontier.extend(operands.iter().next());
                false
            }
            // Operations the lowerer evaluates in this scope, operands
            // included.
            dae::ExpressionOperation::Literal(_)
            | dae::ExpressionOperation::Coordinate(_)
            | dae::ExpressionOperation::Unary { .. }
            | dae::ExpressionOperation::Binary { .. }
            | dae::ExpressionOperation::Array(_)
            | dae::ExpressionOperation::Record(_)
            | dae::ExpressionOperation::Field { .. }
            | dae::ExpressionOperation::Index { .. }
            | dae::ExpressionOperation::ArrayUpdate { .. }
            | dae::ExpressionOperation::Builtin { .. } => true,
            // A comprehension body and a loop's carried values are regions of
            // their own, and a definition is issued by the scope that owns it.
            _ => false,
        }
    }
}

/// Sibling conditional expressions that select by the same conditions.
struct ConditionalGroup<'dae> {
    conditions: Vec<dae::ExprId<'dae>>,
    members: Vec<dae::ExprId<'dae>>,
}

/// The eagerly demanded values of one scope that no consumer has issued yet.
///
/// Calls are not issued ahead of the statements that read them: the scope
/// lowers its results in source order, so a call is issued where its first
/// use lowers it and every later use reads that value. The only reader that
/// can need a value before its source-order use is a nested region, which
/// captures from the scope (see `issue_demanded_calls`).
#[derive(Default)]
pub(super) struct EagerScope<'dae> {
    /// Calls the scope computes whatever its inputs are, in walk order.
    calls: HashMap<dae::ExprId<'dae>, usize>,
    groups: Vec<ConditionalGroup<'dae>>,
    /// Group index of every member, until the group is lowered.
    group_of: HashMap<dae::ExprId<'dae>, usize>,
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    /// Record the calls and correlated conditional groups the scope demands
    /// from `roots`. Nothing is issued here: each is lowered at its first
    /// source-order use.
    pub(super) fn plan_eager_demand(&mut self, roots: impl IntoIterator<Item = dae::ExprId<'dae>>) {
        let demand = self.eager_demand(roots);
        for (ordinal, call) in demand.calls.into_iter().enumerate() {
            self.eager.calls.insert(call, ordinal);
        }
        for group in self.conditional_groups_of(demand.conditionals) {
            let ordinal = self.eager.groups.len();
            for member in &group.members {
                self.eager.group_of.insert(*member, ordinal);
            }
            self.eager.groups.push(group);
        }
    }

    /// Lower the correlated group `expression` belongs to when it is the first
    /// of its members the scope reaches; every member is then issued.
    pub(super) fn lower_group_of(
        &mut self,
        expression: dae::ExprId<'dae>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let Some(ordinal) = self.eager.group_of.get(&expression).copied() else {
            return Ok(());
        };
        let group = &mut self.eager.groups[ordinal];
        let group = ConditionalGroup {
            conditions: group.conditions.clone(),
            members: std::mem::take(&mut group.members),
        };
        for member in &group.members {
            self.eager.group_of.remove(member);
        }
        self.lower_fused_conditionals(&group)
    }

    /// Issue the scope's demanded calls that `expressions` read, before a
    /// region over them captures from the scope.
    ///
    /// A region cannot lower such a call itself: the scope computes it once,
    /// and a second lowering inside the region would be a second identity. The
    /// call is issued at this point, which is its first use in source order
    /// whenever the region precedes the call's other consumers; the region and
    /// every later consumer then read the one value.
    pub(super) fn issue_demanded_calls(
        &mut self,
        traversal: &mut dae::ExpressionTraversal<'dae>,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        if self.eager.calls.is_empty() {
            return Ok(());
        }
        let mut reached = Vec::new();
        traversal.visit_pruned(self.view, expressions, |id, _| {
            if self.cache.contains_key(&id) {
                return false;
            }
            if let Some(ordinal) = self.eager.calls.get(&id) {
                reached.push((*ordinal, id));
                return false;
            }
            true
        });
        reached.sort_unstable();
        for (_, call) in reached {
            self.expression(call)?;
        }
        Ok(())
    }

    /// Every call and conditional the scope computes whatever its inputs are.
    ///
    /// The walk follows the operands every evaluation reads: a call's
    /// arguments, and an if-expression's first condition only (its values and
    /// later conditions are entered only when selected). A comprehension body
    /// is a region of its own, and a value the scope already issued is not
    /// demanded again.
    fn eager_demand(
        &self,
        roots: impl IntoIterator<Item = dae::ExprId<'dae>>,
    ) -> EagerDemand<'dae> {
        let mut demand = EagerDemand::default();
        let mut seen = HashSet::new();
        let mut frontier = roots.into_iter().collect::<Vec<_>>();
        let mut traversal = dae::ExpressionTraversal::new();
        while !frontier.is_empty() {
            let current = std::mem::take(&mut frontier);
            traversal.visit_pruned(self.view, current, |id, node| {
                self.is_unissued(id, &mut seen)
                    && demand.record(id, node.operation(), &mut frontier)
            });
        }
        demand
    }

    /// Whether the walk reaches `id` for the first time and the scope has not
    /// issued its value.
    fn is_unissued(&self, id: dae::ExprId<'dae>, seen: &mut HashSet<dae::ExprId<'dae>>) -> bool {
        !self.cache.contains_key(&id) && seen.insert(id)
    }

    /// The groups of two or more not-yet-issued conditionals that select by
    /// the same conditions and cannot be lowered as total selections.
    fn conditional_groups_of(
        &mut self,
        conditionals: Vec<dae::ExprId<'dae>>,
    ) -> Vec<ConditionalGroup<'dae>> {
        let mut groups: Vec<ConditionalGroup<'dae>> = Vec::new();
        let mut index: HashMap<Vec<dae::ExprId<'dae>>, usize> = HashMap::new();
        for id in conditionals {
            if self.cache.contains_key(&id) {
                continue;
            }
            let Some(operands) = self.conditional_operands(id) else {
                continue;
            };
            if operands[1..]
                .iter()
                .all(|operand| is_total(self.view, &mut self.totality, *operand))
            {
                continue;
            }
            let conditions = operands[..operands.len() - 1]
                .iter()
                .step_by(2)
                .copied()
                .collect::<Vec<_>>();
            let ordinal = *index.entry(conditions.clone()).or_insert_with(|| {
                groups.push(ConditionalGroup {
                    conditions,
                    members: Vec::new(),
                });
                groups.len() - 1
            });
            groups[ordinal].members.push(id);
        }
        groups.retain(|group| group.members.len() > 1);
        groups
    }

    fn conditional_operands(&self, id: dae::ExprId<'dae>) -> Option<Vec<dae::ExprId<'dae>>> {
        let node = self.view.expression(id)?;
        let dae::ExpressionOperation::Conditional(operands) = node.operation() else {
            return None;
        };
        let operands = operands.iter().collect::<Vec<_>>();
        (operands.len() >= 3 && operands.len() % 2 == 1).then_some(operands)
    }

    /// Lower one group as the correlated tuple its members are.
    fn lower_fused_conditionals(
        &mut self,
        group: &ConditionalGroup<'dae>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let mut value_types = Vec::with_capacity(group.members.len());
        let mut arms = Vec::with_capacity(group.members.len());
        let mut at = None;
        for member in &group.members {
            let node = self
                .view
                .expression(*member)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
            at.get_or_insert(node.provenance().span());
            value_types.push(node.value_type_id());
            arms.push(
                self.conditional_operands(*member)
                    .ok_or(solve::SolveProgramConstructionError::WireMismatch)?,
            );
        }
        let at = at.ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let branches = (0..group.conditions.len())
            .map(|branch| {
                arms.iter()
                    .map(|operands| operands[2 * branch + 1])
                    .collect()
            })
            .collect::<Vec<Vec<_>>>();
        let fallback = arms
            .iter()
            .map(|operands| operands[operands.len() - 1])
            .collect::<Vec<_>>();
        let values = self.correlated_conditional_values(
            &value_types,
            &group.conditions,
            &branches,
            &fallback,
            at,
        )?;
        for (member, value) in group.members.iter().zip(values) {
            self.cache.insert(*member, value);
        }
        Ok(())
    }
}
