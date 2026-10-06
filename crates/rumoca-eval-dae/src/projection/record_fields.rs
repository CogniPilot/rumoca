//! Record field projection: field selection through calls, arrays,
//! comprehensions and indexed record aggregates.
use super::*;

impl<'dae> Projection<'_, 'dae> {
    pub(super) fn record_field(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: usize,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        self.guard_memo_invalidate();
        let fragment = self.begin_parameter_fragment(expression, Some(field), scalar_index);
        if let parameter_fragments::Start::Cached(dependencies)
        | parameter_fragments::Start::Imported(dependencies) = &fragment
        {
            self.replay_parameter_fragment(
                dependencies,
                matches!(fragment, parameter_fragments::Start::Imported(_)),
            );
            return Ok(());
        }
        let result = self.record_field_uncached(expression, field, scalar_index);
        if matches!(fragment, parameter_fragments::Start::Checking) {
            self.finish_parameter_fragment(result.is_ok());
        }
        result
    }

    pub(super) fn record_field_uncached(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: usize,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let node = self.node(expression);
        // A fold boundary owns its own re-entry guard, the same way
        // `expression_to_project` treats one.
        if let dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. }
        | dae::ExpressionOperation::FunctionFoldOutput { fold, carried, .. } = node.operation()
        {
            return self.function_fold_dependency(fold, carried, Some(field), scalar_index);
        }
        if !self.visit_expression_once(expression, Some(field), scalar_index) {
            return Ok(());
        }
        match node.operation() {
            dae::ExpressionOperation::Record(fields) => self.expression(
                fields
                    .get(field)
                    .expect("checked record field ordinal is in range"),
                scalar_index,
            ),
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
                ..
            } => self.function_call_record_field(
                function,
                output,
                arguments,
                field,
                scalar_index,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.record_field(definition.rhs(), field, scalar_index)
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self.function_parameter_field(
                parameter,
                field,
                scalar_index,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::Conditional(operands) => {
                self.conditional_value(operands, Some(field), scalar_index)
            }
            dae::ExpressionOperation::Array(elements) => {
                self.record_array_field(elements, field, scalar_index)
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                self.record_comprehension_field(domain, body, field, scalar_index)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                self.indexed_record_field(expression, base, subscripts, field, scalar_index)
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => self.array_update_field(base, value, subscripts, field, scalar_index),
            _ => Err(unsupported_record_operation(node, field)),
        }
    }

    pub(super) fn function_parameter_field(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        field: usize,
        scalar_index: usize,
        span: Span,
    ) -> Result<(), ProjectionError> {
        let Some(frame) = self.function_frames.last() else {
            return Err(ProjectionError::FunctionRecursion { span });
        };
        if frame.function() != parameter.function() {
            return Err(ProjectionError::FunctionRecursion { span });
        }
        match frame {
            FunctionFrame::Actual { arguments, .. } => {
                let argument = arguments
                    .get(parameter.ordinal() as usize)
                    .copied()
                    .ok_or(ProjectionError::FunctionRecursion { span })?;
                self.in_caller_context(|projection| {
                    projection.record_field(argument, field, scalar_index)
                })
            }
            FunctionFrame::Summary { function, .. } => {
                let function = *function;
                self.capture_function_parameter(
                    function,
                    FunctionParameterDependency::RecordField {
                        activation: self.activation,
                        parameter: parameter.ordinal(),
                        field,
                        scalar: scalar_index,
                    },
                    span,
                )
            }
        }
    }

    pub(super) fn record_array_field(
        &mut self,
        elements: dae::ExpressionOperands<'dae>,
        field: usize,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let first = elements.get(0).expect("checked record array is nonempty");
        let element_count = self.record_field_scalar_count(first, field);
        let element = elements
            .get(scalar_index / element_count)
            .expect("checked record field scalar selects an array element");
        self.record_field(element, field, scalar_index % element_count)
    }

    pub(super) fn record_comprehension_field(
        &mut self,
        domain: dae::DomainId<'dae>,
        body: dae::ExprId<'dae>,
        field: usize,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let body_count = self.record_field_scalar_count(body, field);
        let point_index = scalar_index / body_count;
        let point = self
            .view
            .domain(domain)
            .expect("checked comprehension domain resolves")
            .structured()
            .index_tuple_at(point_index)
            .expect("checked comprehension domain remains valid")
            .expect("checked record field scalar selects its domain");
        self.domain_contexts.push(domain, point);
        let result = self.record_field(body, field, scalar_index % body_count);
        self.domain_contexts.pop();
        result
    }

    pub(super) fn indexed_record_field(
        &mut self,
        indexed: dae::ExprId<'dae>,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        field: usize,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let field_width = self.record_field_width(indexed, field);
        let record_index = scalar_index / field_width;
        let field_index = scalar_index % field_width;
        match self.indexed_base_scalar(
            base,
            subscripts,
            self.node(indexed).value_type().dimensions(),
            record_index,
        ) {
            Ok(base_record) => {
                self.record_field(base, field, base_record * field_width + field_index)
            }
            Err(error)
                if self.activation == Activation::Conditional
                    && self.conservative_subscript(&error) =>
            {
                self.all_record_field_scalars(base, field)?;
                self.subscripts(subscripts)
            }
            Err(error) => Err(error),
        }
    }

    pub(super) fn all_record_field_scalars(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: usize,
    ) -> Result<(), ProjectionError> {
        for scalar in 0..self.record_field_scalar_count(expression, field) {
            self.record_field(expression, field, scalar)?;
        }
        Ok(())
    }

    pub(super) fn record_field_scalar_count(
        &self,
        expression: dae::ExprId<'dae>,
        field: usize,
    ) -> usize {
        let layout = self.record_layout(expression, field);
        layout.outer_count() * layout.field_width()
    }

    pub(super) fn record_field_width(&self, expression: dae::ExprId<'dae>, field: usize) -> usize {
        self.record_layout(expression, field).field_width()
    }

    pub(super) fn record_layout(
        &self,
        expression: dae::ExprId<'dae>,
        field: usize,
    ) -> dae::RecordFieldLayout {
        let node = self.node(expression);
        self.view
            .record_field_layout(node.value_type_id(), field)
            .expect("checked record projection has a finite field layout")
    }
}
