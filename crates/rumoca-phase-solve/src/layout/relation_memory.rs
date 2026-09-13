use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::LowerError;
use crate::lower::clock_ownership::expression_clock_owner;

/// Current relation truth, independent of condition-edge history and B.1c targets.
pub(crate) struct BufferedRelations<'dae> {
    by_relation: Vec<Option<usize>>,
    by_expression: Vec<Option<usize>>,
    parameters: std::ops::Range<usize>,
    owner: std::marker::PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> BufferedRelations<'dae> {
    pub(super) fn construct(view: dae::DaeView<'dae>, base: usize) -> Result<Self, LowerError> {
        let mut variable_clocks = vec![None; view.variable_count()];
        for (_, ownership) in view.clock_ownerships() {
            variable_clocks[ownership.variable().index() as usize] = Some(ownership.clock());
        }
        let mut by_relation = vec![None; view.relation_count()];
        let mut by_expression = vec![None; view.expression_count()];
        let mut next = base;
        for (_, root) in view.roots() {
            let relation = view
                .relation(root.relation())
                .expect("checked root relation");
            let expression = relation.expression();
            if expression_clock_owner(view, expression, |variable| {
                variable_clocks[variable.index() as usize]
            })
            .is_some()
            {
                continue;
            }
            let slot = if let Some(slot) = by_expression[expression.index() as usize] {
                slot
            } else {
                let slot = allocate_slot(&mut next, relation.provenance())?;
                by_expression[expression.index() as usize] = Some(slot);
                slot
            };
            by_relation[root.relation().index() as usize] = Some(slot);
        }
        Ok(Self {
            by_relation,
            by_expression,
            parameters: base..next,
            owner: std::marker::PhantomData,
        })
    }

    pub(crate) fn expression_slot(&self, expression: dae::ExprId<'dae>) -> Option<usize> {
        self.by_expression
            .get(expression.index() as usize)
            .copied()
            .flatten()
    }

    pub(crate) fn target(&self, relation: dae::RelationId<'dae>) -> Option<solve::ScalarSlot> {
        self.by_relation
            .get(relation.index() as usize)
            .copied()
            .flatten()
            .map(solve::scalar_slot_p)
    }

    pub(super) fn parameter_indices(&self) -> Vec<usize> {
        self.parameters.clone().collect()
    }

    pub(super) const fn end(&self) -> usize {
        self.parameters.end
    }
}

fn allocate_slot(next: &mut usize, provenance: dae::DaeProvenance) -> Result<usize, LowerError> {
    let slot = *next;
    *next = next.checked_add(1).ok_or_else(|| {
        LowerError::contract("relation-memory layout overflow", provenance.span())
    })?;
    Ok(slot)
}
