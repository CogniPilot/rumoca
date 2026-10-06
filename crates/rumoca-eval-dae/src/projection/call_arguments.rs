//! Checked call-argument validation is independent of formal result incidence.
use super::*;

impl<'dae> Projection<'_, 'dae> {
    pub(super) fn validate_call_arguments(
        &mut self,
        arguments: &[dae::ExprId<'dae>],
    ) -> Result<(), ProjectionError> {
        let previous = self.validating_actuals;
        self.validating_actuals = true;
        let result = arguments
            .iter()
            .try_for_each(|argument| self.validate_argument_value(*argument));
        self.validating_actuals = previous;
        result
    }

    fn validate_argument_value(
        &mut self,
        argument: dae::ExprId<'dae>,
    ) -> Result<(), ProjectionError> {
        let node = self.node(argument);
        match node.operation() {
            // These are available values, including a function formal whose
            // compound actual was checked at its original call entry. A large
            // coordinate/literal array does not need scalar validation visits.
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => {
                if self
                    .function_frames
                    .last()
                    .is_some_and(|frame| frame.function() == parameter.function())
                {
                    Ok(())
                } else {
                    Err(ProjectionError::FunctionRecursion {
                        span: node.provenance().span(),
                    })
                }
            }
            dae::ExpressionOperation::Literal(_) | dae::ExpressionOperation::Coordinate(_) => {
                Ok(())
            }
            // Construction retains eager array/record operands. Validate their
            // structure once rather than enumerating the result's scalar shape.
            dae::ExpressionOperation::Array(values) | dae::ExpressionOperation::Record(values) => {
                values
                    .iter()
                    .try_for_each(|value| self.validate_argument_value(value))
            }
            dae::ExpressionOperation::Unary { operand, .. } => {
                self.validate_argument_value(operand)
            }
            dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
                self.validate_argument_value(lhs)?;
                self.validate_argument_value(rhs)
            }
            dae::ExpressionOperation::Conditional(operands) => self
                .walk_conditional(operands, |projection, value| {
                    projection.validate_argument_value(value)
                }),
            dae::ExpressionOperation::Field { base, .. } => self.validate_argument_value(base),
            dae::ExpressionOperation::Index { base, subscripts } => {
                self.validate_argument_value(base)?;
                for subscript in subscripts.iter() {
                    self.validate_argument_subscript(subscript)?;
                }
                self.validate_argument_scalars(argument)
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.validate_argument_value(definition.rhs())
            }
            _ => self.validate_argument_scalars(argument),
        }
    }

    fn validate_argument_subscript(
        &mut self,
        subscript: dae::SubscriptView<'dae>,
    ) -> Result<(), ProjectionError> {
        if let dae::SubscriptView::Index { expression, .. }
        | dae::SubscriptView::Slice { expression, .. } = subscript
        {
            self.validate_argument_value(expression)?;
        }
        Ok(())
    }

    fn validate_argument_scalars(
        &mut self,
        argument: dae::ExprId<'dae>,
    ) -> Result<(), ProjectionError> {
        let value_type = self.node(argument).value_type();
        if !value_type.is_record() {
            return self.all_scalars(argument);
        }
        for field in 0..value_type.record_field_count() {
            self.all_record_field_scalars(argument, field)?;
        }
        Ok(())
    }
}
