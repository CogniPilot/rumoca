//! Read-only identities of construction-issued model conditional arms.

use super::*;

#[derive(Clone, Copy)]
pub struct ModelConditionalView<'dae> {
    view: DaeView<'dae>,
    owner: u32,
}

impl<'dae> ModelConditionalView<'dae> {
    pub const fn index(self) -> u32 {
        self.owner
    }
    pub fn condition(self) -> ExprId<'dae> {
        ExprId::from_raw(self.entry().condition)
    }
    pub fn guards(self) -> [ExprId<'dae>; 2] {
        self.entry()
            .arms
            .map(|arm| ExprId::from_raw(self.view.dae.storage.model_arms[arm as usize].guard))
    }
    /// Canonical operations consumed by this checked activation owner.
    /// The source condition may settle; its issued negation and parent guards
    /// retain their operation identities when a whole DAE is reconstructed.
    pub fn guard_operations(self) -> impl Iterator<Item = ExprId<'dae>> {
        let [taken, fallback] = self.guards();
        let operations = if self.parent_arm().is_none() {
            [Some(fallback), None, None]
        } else {
            let negated = match self
                .view
                .expression(fallback)
                .expect("checked model guard")
                .operation()
            {
                ExpressionOperation::Conditional(operands) => operands.get(1),
                _ => None,
            }
            .expect("checked model owner proves negated guard operand");
            [Some(negated), Some(taken), Some(fallback)]
        };
        operations.into_iter().flatten()
    }
    pub fn parent_arm(self) -> Option<ModelArm<'dae>> {
        self.view.dae.storage.model_arms[self.entry().arms[0] as usize]
            .parent
            .map(|index| ModelArm {
                index,
                marker: PhantomData,
            })
    }
    pub fn arm(self, selected: bool) -> ModelArm<'dae> {
        ModelArm {
            index: self.entry().arms[usize::from(!selected)],
            marker: PhantomData,
        }
    }
    pub fn expression(self) -> ExprId<'dae> {
        ExprId::from_raw(
            self.entry()
                .expression
                .expect("frozen conditional is closed"),
        )
    }
    pub fn provenance(self) -> DaeProvenance {
        self.entry().provenance
    }
    fn entry(self) -> &'dae crate::model::model_conditionals::ModelConditionalEntry {
        &self.view.dae.storage.model_conditionals[self.owner as usize]
    }
}

impl<'dae> DaeView<'dae> {
    pub fn model_conditional_count(self) -> usize {
        self.dae.storage.model_conditionals.len()
    }
    pub fn model_conditional_at(self, owner: usize) -> Option<ModelConditionalView<'dae>> {
        (owner < self.model_conditional_count()).then(|| ModelConditionalView {
            view: self,
            owner: owner as u32,
        })
    }
    pub fn model_conditional_of(
        self,
        expression: ExprId<'dae>,
    ) -> Option<ModelConditionalView<'dae>> {
        self.dae.storage.expressions.model_conditional_owners[expression.index() as usize]
            .map(|owner| ModelConditionalView { view: self, owner })
    }
    /// Exact activation of an expression constructed in a model arm.
    /// Consumers of a separately published relation must use this same guard
    /// for its lazy numeric surface and Boolean observation.
    pub fn model_activation_guard(self, expression: ExprId<'dae>) -> Option<ExprId<'dae>> {
        self.model_arm_of(expression)
            .map(|arm| ExprId::from_raw(self.dae.storage.model_arms[arm.index() as usize].guard))
    }
    pub fn model_arm_of(self, expression: ExprId<'dae>) -> Option<ModelArm<'dae>> {
        self.dae.storage.expressions.model_arm_scopes[expression.index() as usize].map(|index| {
            ModelArm {
                index,
                marker: PhantomData,
            }
        })
    }
}
