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
        if let Some(owner) = self.source.model_conditional_of(source_id) {
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
            let guard = self.rebuild_instantiated(operands.get(index).unwrap())?;
            let value = self.parameter_branch_value(
                operands.get(index + 1).unwrap(),
                derivative_order,
                provenance,
            )?;
            branches.push((guard, value));
        }
        self.target.at(provenance).conditional(branches, fallback)
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
