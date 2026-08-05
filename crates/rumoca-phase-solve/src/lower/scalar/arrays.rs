//! Array projection: the scalar of an aggregate operand one output scalar reads.
//!
//! Comprehensions, subscripts, and array literals are projected at translation
//! time wherever the model fixes the index. A subscript only the run knows
//! falls back to an explicit selection chain over the base's checked extent.

use super::*;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn comprehension(
        &mut self,
        domain: dae::DomainId<'dae>,
        body: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let domain_view = self
            .view
            .domain(domain)
            .expect("checked comprehension domain resolves");
        let body_count = scalar_count(self.view, body);
        let point = scalar / body_count;
        let body_scalar = scalar % body_count;
        let values = domain_view
            .structured()
            .index_tuple_at(point)
            .expect("checked domain remains valid")
            .expect("checked comprehension scalar point is in range");
        self.enter_context(ScalarContextFrame::Domain {
            parent: self.context_id,
            domain,
            values: values.clone(),
        });
        self.domain_points.push((domain, values));
        let result = self.expression(body, body_scalar);
        self.domain_points.pop();
        self.leave_context();
        result
    }

    pub(super) fn index(
        &mut self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        dimensions: &[u32],
        scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let selector = ScalarSelector::from_points(self.view, &self.domain_points);
        match selector.indexed_base_scalar(base, subscripts, dimensions, scalar) {
            Ok(selected) => self.expression(base, selected),
            Err(LowerError::NonComputable { reason, .. })
                if reason == "array subscript is not compile-time computable" =>
            {
                self.dynamic_scalar_index(base, subscripts, dimensions, scalar)
            }
            Err(error) => Err(error),
        }
    }

    fn dynamic_scalar_index(
        &mut self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        result_dimensions: &[u32],
        result_scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let span = self.node(base).provenance().span();
        let base_dimensions = self.node(base).value_type().dimensions().to_vec();
        if !result_dimensions.is_empty()
            || result_scalar != 0
            || subscripts.len() != base_dimensions.len()
        {
            return Err(LowerError::non_computable(
                "runtime indexing requires one scalar index per base axis and one scalar result",
                span,
            ));
        }
        let runtime_indices = self.dynamic_scalar_indices(subscripts, span)?;
        let zero = self.constant(0.0, span)?;
        let mut selected = self.binary(dae::BinaryOperator::Divide, zero, zero, span)?;
        let count = base_dimensions
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize));
        let Some(count) = count else {
            return Err(LowerError::contract(
                "runtime indexed base scalar count overflow",
                span,
            ));
        };
        for ordinal in 0..count {
            let coordinates = row_major_coordinates(&base_dimensions, ordinal)
                .expect("checked base scalar has one row-major coordinate");
            let matches = self.dynamic_coordinate_match(&runtime_indices, &coordinates, span)?;
            let candidate = self.expression(base, ordinal)?;
            selected = self.select(matches, candidate, selected, span)?;
        }
        Ok(selected)
    }

    fn dynamic_scalar_indices(
        &mut self,
        subscripts: dae::SubscriptsView<'dae>,
        span: Span,
    ) -> Result<Vec<solve::Reg>, LowerError> {
        let mut indices = Vec::with_capacity(subscripts.len());
        for axis in 0..subscripts.len() {
            let Some(dae::SubscriptView::Index { expression, .. }) = subscripts.get(axis) else {
                return Err(LowerError::non_computable(
                    "runtime slices do not yet have a computable Solve owner",
                    span,
                ));
            };
            indices.push(self.expression(expression, 0)?);
        }
        Ok(indices)
    }

    fn dynamic_coordinate_match(
        &mut self,
        runtime_indices: &[solve::Reg],
        coordinates: &[u32],
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let mut matches = self.constant(1.0, span)?;
        for (&runtime_index, &coordinate) in runtime_indices.iter().zip(coordinates) {
            let modelica_index = self.constant(f64::from(coordinate + 1), span)?;
            let axis_matches = self.binary(
                dae::BinaryOperator::Equal,
                runtime_index,
                modelica_index,
                span,
            )?;
            matches = self.binary(dae::BinaryOperator::And, matches, axis_matches, span)?;
        }
        Ok(matches)
    }

    pub(super) fn dynamic_scalar_array_update(
        &mut self,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        base_scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let span = self.node(base).provenance().span();
        let base_dimensions = self.node(base).value_type().dimensions().to_vec();
        if !self.node(value).value_type().dimensions().is_empty()
            || subscripts.len() != base_dimensions.len()
        {
            return Err(LowerError::non_computable(
                "runtime array update requires one scalar index per base axis and one scalar value",
                span,
            ));
        }
        let coordinates =
            row_major_coordinates(&base_dimensions, base_scalar).ok_or_else(|| {
                LowerError::contract(
                    "runtime array update scalar is outside its checked base shape",
                    span,
                )
            })?;
        let runtime_indices = self.dynamic_scalar_indices(subscripts, span)?;
        let matches = self.dynamic_coordinate_match(&runtime_indices, &coordinates, span)?;
        let updated = self.expression(value, 0)?;
        let unchanged = self.expression(base, base_scalar)?;
        self.select(matches, updated, unchanged, span)
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn dynamic_record_field_array_update(
        &mut self,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        field: usize,
        base_record: usize,
        field_scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let base_dimensions = self.node(base).value_type().dimensions().to_vec();
        if !self.node(value).value_type().dimensions().is_empty()
            || subscripts.len() != base_dimensions.len()
        {
            return Err(LowerError::non_computable(
                "runtime record-array update requires one scalar index per base axis and one record value",
                span,
            ));
        }
        let coordinates =
            row_major_coordinates(&base_dimensions, base_record).ok_or_else(|| {
                LowerError::contract(
                    "runtime record-array update selects outside its checked base shape",
                    span,
                )
            })?;
        let runtime_indices = self.dynamic_scalar_indices(subscripts, span)?;
        let matches = self.dynamic_coordinate_match(&runtime_indices, &coordinates, span)?;
        let updated = self.record_field(value, field, field_scalar, span)?;
        let field_width = self
            .view
            .record_field_layout(self.node(base).value_type_id(), field)
            .expect("checked record projection has a finite field layout")
            .field_width();
        let unchanged =
            self.record_field(base, field, base_record * field_width + field_scalar, span)?;
        self.select(matches, updated, unchanged, span)
    }

    pub(super) fn select_array(
        &self,
        elements: dae::ExpressionOperands<'dae>,
        scalar: usize,
    ) -> (dae::ExprId<'dae>, usize) {
        let first = elements.get(0).expect("checked array is nonempty");
        let element_count = scalar_count(self.view, first);
        (
            elements
                .get(scalar / element_count)
                .expect("checked scalar selects an array element"),
            scalar % element_count,
        )
    }
}
