//! Reuse complete source dependency witnesses during one immutable facts pass.

use std::collections::BTreeMap;
use std::sync::Arc;

use rumoca_eval_dae::FunctionCallContext;
use rumoca_ir_dae as dae;

use super::super::super::constraints::DifferentiationFacts;
use super::super::tensor_expression::SourceValue;

pub(super) struct MaterializedSources<'dae, 'facts> {
    pub(super) view: dae::DaeView<'dae>,
    pub(super) facts: &'facts DifferentiationFacts,
    anchors: BTreeMap<SourceValue, Option<Arc<[u32]>>>,
}

impl<'dae, 'facts> MaterializedSources<'dae, 'facts> {
    pub(super) fn new(view: dae::DaeView<'dae>, facts: &'facts DifferentiationFacts) -> Self {
        Self {
            view,
            facts,
            anchors: BTreeMap::new(),
        }
    }

    pub(super) fn state_anchors(
        &mut self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<Arc<[u32]>> {
        let context = context.scoped_to_expression(self.view, expression);
        let key = SourceValue::new(expression, &context);
        self.anchors
            .entry(key)
            .or_insert_with(|| {
                self.facts
                    .materialized_state_anchors_in_context(self.view, expression.index(), &context)
                    .map(Arc::from)
            })
            .clone()
    }
}

#[cfg(test)]
mod tests;
