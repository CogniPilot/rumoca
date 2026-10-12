//! Preserve issued lexical arm identities during structural expression replay.

use rumoca_ir_dae as dae;

pub(super) struct ModelConditionalReplay<'target> {
    owners: Vec<Option<dae::ModelConditional<'target>>>,
    arms: Vec<dae::ModelArm<'target>>,
    guards: rustc_hash::FxHashSet<u32>,
}

impl<'target> ModelConditionalReplay<'target> {
    pub(super) fn new(source: dae::DaeView<'_>) -> Self {
        Self {
            owners: Vec::new(),
            arms: Vec::new(),
            guards: (0..source.model_conditional_count())
                .flat_map(|owner| {
                    source
                        .model_conditional_at(owner)
                        .expect("checked conditional owner")
                        .guard_operations()
                        .map(|guard| guard.index())
                })
                .collect(),
        }
    }

    pub(super) fn owns_guard(&self, expression: u32) -> bool {
        self.guards.contains(&expression)
    }

    pub(super) fn prepare<'source>(
        &mut self,
        source: dae::DaeView<'source>,
        source_id: dae::ExprId<'source>,
        target: &mut dae::DaeConstruction<'target>,
        rebuilt: &[Option<dae::ExprId<'target>>],
    ) -> Result<(), dae::DaeConstructionError> {
        let expression = source
            .expression(source_id)
            .expect("checked source expression");
        let provenance = expression.provenance();
        let context = source.model_arm_of(source_id);
        let closing = source.model_conditional_of(source_id);
        let needed = context
            .map(|arm| arm.index() as usize / 2)
            .into_iter()
            .chain(closing.map(|owner| owner.index() as usize))
            .max();
        if let Some(needed) = needed {
            while self.owners.len() <= needed {
                let view = source
                    .model_conditional_at(self.owners.len())
                    .expect("checked conditional owner");
                let parent = view
                    .parent_arm()
                    .map(|arm| self.mapped_arm(arm.index(), provenance))
                    .transpose()?;
                target.replay_model_arm_context(parent, provenance)?;
                let condition = mapped(rebuilt, view.condition(), provenance)?;
                let [then_guard, else_guard] = view
                    .guards()
                    .map(|guard| mapped(rebuilt, guard, provenance));
                let owner = target.replay_model_conditional(
                    condition,
                    [then_guard?, else_guard?],
                    view.provenance(),
                )?;
                self.arms
                    .extend([owner.arm(target, true), owner.arm(target, false)]);
                self.owners.push(Some(owner));
            }
        }
        let context = context
            .map(|arm| self.mapped_arm(arm.index(), provenance))
            .transpose()?;
        target.replay_model_arm_context(context, provenance)
    }

    pub(super) fn close<'source>(
        &mut self,
        source: dae::DaeView<'source>,
        source_id: dae::ExprId<'source>,
        target: &mut dae::DaeConstruction<'target>,
        rebuilt: &mut [Option<dae::ExprId<'target>>],
    ) -> Result<Option<dae::DaeProvenance>, dae::DaeConstructionError> {
        let Some(owner) = source.model_conditional_of(source_id) else {
            return Ok(None);
        };
        let expression = source
            .expression(source_id)
            .expect("checked conditional expression");
        let dae::ExpressionOperation::Conditional(operands) = expression.operation() else {
            return Err(dae::DaeConstructionError::InvalidExpressionForm {
                span: owner.provenance().span(),
            });
        };
        let values = [
            mapped(
                rebuilt,
                operands.get(1).expect("issued then arm"),
                owner.provenance(),
            )?,
            mapped(
                rebuilt,
                operands.get(2).expect("issued else arm"),
                owner.provenance(),
            )?,
        ];
        let token = self.owners[owner.index() as usize].take().ok_or(
            dae::DaeConstructionError::InvalidModelActivationScope {
                span: owner.provenance().span(),
            },
        )?;
        rebuilt[source_id.index() as usize] = Some(target.finish_model_conditional(token, values)?);
        Ok(Some(owner.provenance()))
    }

    fn mapped_arm(
        &self,
        arm: u32,
        at: dae::DaeProvenance,
    ) -> Result<dae::ModelArm<'target>, dae::DaeConstructionError> {
        self.arms
            .get(arm as usize)
            .copied()
            .ok_or(dae::DaeConstructionError::UnknownId {
                kind: "rebuilt model arm",
                index: arm,
                span: at.span(),
            })
    }
}

fn mapped<'source, 'target>(
    values: &[Option<dae::ExprId<'target>>],
    value: dae::ExprId<'source>,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
    values.get(value.index() as usize).copied().flatten().ok_or(
        dae::DaeConstructionError::IncompleteDefinition {
            kind: "model conditional operand",
            index: value.index(),
            span: at.span(),
        },
    )
}
