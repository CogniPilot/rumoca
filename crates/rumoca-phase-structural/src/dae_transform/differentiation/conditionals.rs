use super::*;

#[derive(Clone, Copy)]
pub(in crate::dae_transform) enum BranchTransform {
    Rebuild,
    Exact,
    Derivative(u8),
}

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    /// Retain the issued branch order and invariant guards. Only branch values
    /// receive the requested derivative; tensor values keep their whole shape.
    pub(super) fn materialize_parameter_conditional(
        &mut self,
        source_id: dae::ExprId<'source>,
        operands: dae::ExpressionOperands<'source>,
        derivative_order: Option<u8>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let taking_events = self.guards_taking_events(operands, derivative_order);
        if let Some(owner) = self.source.model_conditional_of(source_id) {
            if taking_events.contains(&owner.condition()) {
                let relation = self.rebuild_instantiated(owner.condition())?;
                self.take_event_owner(owner.condition(), relation);
            }
            let transform =
                derivative_order.map_or(BranchTransform::Exact, BranchTransform::Derivative);
            return self.materialize_owned_conditional(owner, transform, provenance);
        }
        let fallback = self.parameter_branch_value(
            operands.get(operands.len() - 1).unwrap(),
            derivative_order,
            provenance,
        )?;
        let mut branches = Vec::with_capacity((operands.len() - 1) / 2);
        for index in (0..operands.len() - 1).step_by(2) {
            let source_guard = operands.get(index).unwrap();
            let guard = self.rebuild_instantiated(source_guard)?;
            if taking_events.contains(&source_guard) {
                self.take_event_owner(source_guard, guard);
            }
            let value = self.parameter_branch_value(
                operands.get(index + 1).unwrap(),
                derivative_order,
                provenance,
            )?;
            branches.push((guard, value));
        }
        self.target.at(provenance).conditional(branches, fallback)
    }

    /// Record that the rebuilt guard `relation` of `source` must own an event.
    fn take_event_owner(&mut self, source: dae::ExprId<'source>, relation: dae::ExprId<'target>) {
        let span = self
            .source
            .expression(source)
            .expect("a guard is a finalized expression")
            .provenance()
            .span();
        self.event_relations.push((relation, span));
    }

    /// The guards whose relations the formal derivative of this conditional
    /// gives an event owner: none for a rebuilt value, and none while the
    /// enclosing `smooth` still certifies the requested order.
    fn guards_taking_events(
        &self,
        operands: dae::ExpressionOperands<'source>,
        derivative_order: Option<u8>,
    ) -> Vec<dae::ExprId<'source>> {
        let Some(order) = derivative_order.filter(|_| self.formal_derivatives) else {
            return Vec::new();
        };
        if super::super::smooth_order::admits_live_guards(self.smooth_certificate, order) {
            return Vec::new();
        }
        super::super::parameter_conditionals::guards_taking_events(
            self.source,
            &self.function_context,
            operands,
        )
        .unwrap_or_default()
    }

    /// MLS 3.6.5/8.5 differentiates only selected values. Each transformed
    /// occurrence receives fresh checked arms; source closed arms stay closed.
    pub(in crate::dae_transform) fn materialize_owned_conditional(
        &mut self,
        source_owner: dae::ModelConditionalView<'source>,
        transform: BranchTransform,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let condition = self.rebuild_instantiated(source_owner.condition())?;
        let owner = self.target.begin_model_conditional(condition, provenance)?;
        let Some(source) = self.source.expression(source_owner.expression()) else {
            return Err(dae::DaeConstructionError::InvalidExpressionForm {
                span: provenance.span(),
            });
        };
        let dae::ExpressionOperation::Conditional(operands) = source.operation() else {
            return Err(dae::DaeConstructionError::InvalidExpressionForm {
                span: provenance.span(),
            });
        };
        let Some(operands) = operands.conditional() else {
            return Err(dae::DaeConstructionError::InvalidExpressionForm {
                span: provenance.span(),
            });
        };
        let mut branches = operands.branches();
        let Some((_, selected)) = branches.next() else {
            return Err(dae::DaeConstructionError::InvalidExpressionForm {
                span: provenance.span(),
            });
        };
        if branches.next().is_some() {
            return Err(dae::DaeConstructionError::InvalidExpressionForm {
                span: provenance.span(),
            });
        }
        let first = self.transformed_model_branch(&owner, true, selected, transform, provenance)?;
        let fallback = self.transformed_model_branch(
            &owner,
            false,
            operands.fallback(),
            transform,
            provenance,
        )?;
        self.target
            .finish_model_conditional(owner, [first, fallback])
    }

    fn transformed_model_branch(
        &mut self,
        owner: &dae::ModelConditional<'target>,
        selected: bool,
        source: dae::ExprId<'source>,
        transform: BranchTransform,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        self.target
            .model_conditional_arm_context(owner, Some(selected))?;
        let prior = std::mem::replace(&mut self.transforming_model_arm, true);
        let result = match transform {
            BranchTransform::Rebuild => self.rebuild(source),
            BranchTransform::Exact => self.materialize_exact_value(source, provenance),
            BranchTransform::Derivative(order) => {
                self.parameter_branch_value(source, Some(order), provenance)
            }
        };
        self.transforming_model_arm = prior;
        self.target.model_conditional_arm_context(owner, None)?;
        result
    }

    fn parameter_branch_value(
        &mut self,
        source: dae::ExprId<'source>,
        derivative_order: Option<u8>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        match derivative_order {
            Some(order) => {
                let derivative = self.differentiate_order(source, order, provenance)?;
                self.materialize_derivative(derivative, source, provenance)
            }
            None => self.materialize_exact_value(source, provenance),
        }
    }
}
