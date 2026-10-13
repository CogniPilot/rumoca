//! Shared dependency projection of canonical conditional state products.

use super::*;

impl<'dae> Projection<'_, 'dae> {
    pub(super) fn function_conditional_dependency(
        &mut self,
        node: dae::ExpressionView<'dae>,
        conditional: dae::FunctionConditionalId<'dae>,
        value: dae::FunctionValueId<'dae>,
        field: Option<&FieldPath>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let inputs =
            self.function_conditional_inputs(conditional, value, node.provenance().span())?;
        // The inputs are every condition and assigned source of the value across
        // all branches, and the ordered branches decide at run time which of
        // them execute: a later condition or an unselected branch may hold an
        // index that is only invalid on a path never taken.
        self.with_activation(Activation::Conditional, |projection| {
            inputs.into_iter().try_for_each(|input| {
                projection.function_conditional_input(node, input, field, scalar_index)
            })
        })
    }

    fn function_conditional_input(
        &mut self,
        node: dae::ExpressionView<'dae>,
        input: dae::ExprId<'dae>,
        field: Option<&FieldPath>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let source = self
            .view
            .expression(input)
            .expect("checked conditional source resolves");
        match field {
            Some(field) if source.value_type().is_record() => {
                self.record_field(input, field, scalar_index)
            }
            None if source.value_type().scalar_count() == node.value_type().scalar_count() => {
                self.expression(input, scalar_index)
            }
            _ => self.all_scalars(input),
        }
    }

    fn function_conditional_inputs(
        &self,
        conditional: dae::FunctionConditionalId<'dae>,
        value: dae::FunctionValueId<'dae>,
        span: Span,
    ) -> Result<Vec<dae::ExprId<'dae>>, ProjectionError> {
        let conditional = self
            .view
            .function_conditional(conditional)
            .ok_or(ProjectionError::FunctionRecursion { span })?;
        let mut inputs = Vec::new();
        if !conditional.visit_value_inputs(value, |input| inputs.push(input)) {
            return Err(ProjectionError::FunctionRecursion { span });
        }
        Ok(inputs)
    }
}
