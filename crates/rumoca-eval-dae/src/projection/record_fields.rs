//! Record field projection: field selection through calls, arrays,
//! comprehensions, indexed record aggregates and enclosing records.
use super::*;

/// The record field ordinals one projection selects, outermost first.
///
/// A projection of a field of a field (`s.identity.weight`) selects a path:
/// its scalar index runs over the extents of the projected value, then the
/// extents of every field along the path, then the scalars of the last
/// field, row major. A single field is the common case and holds no heap
/// allocation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) enum FieldPath {
    Field(usize),
    Nested(Arc<[usize]>),
}

impl FieldPath {
    /// The field the path selects first.
    pub(super) fn head(&self) -> usize {
        match self {
            Self::Field(field) => *field,
            Self::Nested(path) => path[0],
        }
    }

    /// The path below the first field, `None` for a single field.
    fn rest(&self) -> Option<Self> {
        match self {
            Self::Field(_) => None,
            Self::Nested(path) => Some(Self::from_ordinals(&path[1..])),
        }
    }

    /// `field` followed by this path.
    fn below(field: usize, path: &Self) -> Self {
        let mut ordinals = vec![field];
        match path {
            Self::Field(next) => ordinals.push(*next),
            Self::Nested(rest) => ordinals.extend(rest.iter().copied()),
        }
        Self::Nested(ordinals.into())
    }

    fn from_ordinals(ordinals: &[usize]) -> Self {
        match ordinals {
            [field] => Self::Field(*field),
            _ => Self::Nested(ordinals.into()),
        }
    }
}

impl<'dae> Projection<'_, 'dae> {
    pub(super) fn record_field(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: &FieldPath,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let fragment = self.begin_parameter_fragment(expression, Some(field.clone()), scalar_index);
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
        field: &FieldPath,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let node = self.node(expression);
        // A fold boundary owns its own re-entry guard, the same way
        // `expression_to_project` treats one.
        if let dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. }
        | dae::ExpressionOperation::FunctionFoldOutput { fold, carried, .. } = node.operation()
        {
            return self.function_fold_dependency(fold, carried, Some(field.clone()), scalar_index);
        }
        if !self.visit_expression_once(expression, Some(field.clone()), scalar_index) {
            return Ok(());
        }
        match node.operation() {
            dae::ExpressionOperation::Record(fields) => {
                let value = fields
                    .get(field.head())
                    .expect("checked record field ordinal is in range");
                match field.rest() {
                    None => self.expression(value, scalar_index),
                    Some(rest) => self.record_field(value, &rest, scalar_index),
                }
            }
            // A field of an enclosing record selects the path through it:
            // the enclosing value's extents, then this field's extents, lead
            // the scalar index exactly as they lead the selected path's.
            dae::ExpressionOperation::Field {
                base,
                field: enclosing,
            } => self.record_field(
                base,
                &FieldPath::below(enclosing as usize, field),
                scalar_index,
            ),
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
                ..
            } => self.function_call_record_field(
                expression,
                function,
                output,
                arguments,
                field,
                scalar_index,
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
            _ => Err(unsupported_record_operation(node, field.head())),
        }
    }

    pub(super) fn function_parameter_field(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        field: &FieldPath,
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
                        field: field.clone(),
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
        field: &FieldPath,
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
        field: &FieldPath,
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
        field: &FieldPath,
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
        field: &FieldPath,
    ) -> Result<(), ProjectionError> {
        for scalar in 0..self.record_field_scalar_count(expression, field) {
            self.record_field(expression, field, scalar)?;
        }
        Ok(())
    }

    /// Scalars the path selects over every element of `expression`.
    pub(super) fn record_field_scalar_count(
        &self,
        expression: dae::ExprId<'dae>,
        field: &FieldPath,
    ) -> usize {
        self.path_scalar_count(self.node(expression).value_type_id(), field)
    }

    /// Scalars the path selects in one record element of `expression`.
    pub(super) fn record_field_width(
        &self,
        expression: dae::ExprId<'dae>,
        field: &FieldPath,
    ) -> usize {
        let value_type = self.node(expression).value_type_id();
        let (layout, _) = self.record_layout(value_type, field.head());
        self.path_scalar_count(value_type, field) / layout.outer_count()
    }

    fn path_scalar_count(&self, value_type: dae::ValueTypeId<'dae>, field: &FieldPath) -> usize {
        let (layout, field_type) = self.record_layout(value_type, field.head());
        match field.rest() {
            None => layout.outer_count() * layout.field_width(),
            Some(rest) => layout.outer_count() * self.path_scalar_count(field_type, &rest),
        }
    }

    /// The packing layout of one field and the field's declared type.
    fn record_layout(
        &self,
        value_type: dae::ValueTypeId<'dae>,
        field: usize,
    ) -> (dae::RecordFieldLayout, dae::ValueTypeId<'dae>) {
        self.view
            .record_field_layout(value_type, field)
            .zip(self.view.record_field(value_type, field))
            .map(|(layout, (_, field_type))| (layout, field_type))
            .expect("checked record projection has a finite field layout")
    }
}
