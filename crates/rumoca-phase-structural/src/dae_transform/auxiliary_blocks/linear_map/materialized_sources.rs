//! Reuse complete source dependency witnesses during one immutable facts pass.

use std::collections::BTreeMap;
use std::sync::Arc;

use rumoca_eval_dae::FunctionCallContext;
use rumoca_ir_dae as dae;

use super::super::super::constraints::{DifferentiationFacts, MaterializedAnchors};
use super::super::tensor_expression::SourceValue;

/// The materialization proofs of one block-derivation pass over immutable
/// facts: every root's anchors are kept per source value, and every algebraic
/// and derivative coordinate a root reaches is proved once through the shared
/// [`MaterializedAnchors`] witnesses.
pub(in crate::dae_transform::auxiliary_blocks) struct MaterializedSources<'dae, 'facts> {
    pub(in crate::dae_transform::auxiliary_blocks) view: dae::DaeView<'dae>,
    pub(in crate::dae_transform::auxiliary_blocks) facts: &'facts DifferentiationFacts,
    witnesses: MaterializedAnchors<'facts>,
    anchors: BTreeMap<SourceValue, Option<Arc<[u32]>>>,
}

impl<'dae, 'facts> MaterializedSources<'dae, 'facts> {
    pub(in crate::dae_transform::auxiliary_blocks) fn new(
        view: dae::DaeView<'dae>,
        facts: &'facts DifferentiationFacts,
    ) -> Self {
        Self {
            view,
            facts,
            witnesses: MaterializedAnchors::new(facts),
            anchors: BTreeMap::new(),
        }
    }

    pub(in crate::dae_transform::auxiliary_blocks) fn state_anchors(
        &mut self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<Arc<[u32]>> {
        let context = context.scoped_to_expression(self.view, expression);
        let key = SourceValue::new(expression, &context);
        if let Some(anchors) = self.anchors.get(&key) {
            return anchors.clone();
        }
        let anchors = self
            .witnesses
            .state_anchors_in_context(self.view, expression.index(), &context)
            .map(Arc::from);
        self.anchors.insert(key, anchors.clone());
        anchors
    }

    /// The anchors of a model-level expression, by index.
    pub(in crate::dae_transform::auxiliary_blocks) fn value_anchors(
        &mut self,
        expression: u32,
    ) -> Option<Arc<[u32]>> {
        let expression = self.view.expression_id(expression as usize)?;
        self.state_anchors(expression, &FunctionCallContext::default())
    }

    pub(in crate::dae_transform::auxiliary_blocks) fn can_materialize(
        &mut self,
        expression: u32,
    ) -> bool {
        self.value_anchors(expression).is_some()
    }
}

#[cfg(test)]
mod tests;
