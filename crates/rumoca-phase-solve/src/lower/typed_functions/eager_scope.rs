//! One evaluation of each eagerly demanded call and conditional per scope.
//!
//! A scope (a region body or a function body) computes every expression its
//! results demand unconditionally, whatever region those expressions are
//! reached from: a call in a result, in a call argument, or in the first
//! condition of an if-expression is evaluated by every execution of the scope.
//! Lowering such a call where the first consumer happens to sit (inside the
//! lazy branch of a sibling conditional) would evaluate it once per consumer
//! region. This module instead issues each eagerly demanded call in the scope
//! itself, before any region is built, so every region that reads the value
//! captures it (`environment_requirements`), and fuses the sibling conditionals
//! that share their conditions into one region pair, so the calls only their
//! arms demand are evaluated once per selected arm.
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

/// Sibling conditional expressions that select by the same conditions.
struct ConditionalGroup<'dae> {
    conditions: Vec<dae::ExprId<'dae>>,
    members: Vec<dae::ExprId<'dae>>,
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    /// Issue the eagerly demanded calls of `roots`, then the correlated
    /// conditional groups among them.
    pub(super) fn lower_eager_demand(
        &mut self,
        roots: impl IntoIterator<Item = dae::ExprId<'dae>>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let demand = self.eager_demand(roots);
        for call in demand.calls {
            self.expression(call)?;
        }
        for group in self.conditional_groups_of(demand.conditionals) {
            self.lower_fused_conditionals(&group)?;
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
                if self.cache.contains_key(&id) || !seen.insert(id) {
                    return false;
                }
                match node.operation() {
                    dae::ExpressionOperation::Call { .. } => {
                        demand.calls.push(id);
                        true
                    }
                    dae::ExpressionOperation::Conditional(operands) => {
                        demand.conditionals.push(id);
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
                    // A comprehension body and a loop's carried values are
                    // regions of their own, and a definition is issued by the
                    // scope that owns it.
                    _ => false,
                }
            });
        }
        demand
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
