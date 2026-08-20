//! Array projection: the scalar of an aggregate operand one output scalar reads.
//!
//! Comprehensions, subscripts, and array literals are projected at translation
//! time wherever the model fixes the index. A subscript only the run knows
//! falls back to an explicit selection chain over the base's checked extent.

use super::*;

enum ArrayUpdateAxis {
    Whole(u32),
    Runtime(solve::Reg),
    ConstantMatch,
    ConstantMismatch,
}

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn pack_array_update(
        &mut self,
        expression: dae::ExprId<'dae>,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let key = (self.context_id, expression);
        if let Some(&(start, _)) = self.tensor_update_cache.get(&key) {
            return Ok(start);
        }
        let dimensions = self.node(base).value_type().dimensions().to_vec();
        if dimensions.is_empty() || dimensions.len() != subscripts.len() {
            return Err(LowerError::contract(
                "tensor update projection does not match its base rank",
                span,
            ));
        }
        let base_start = self.pack_expression(base)?;
        let mut compact = Vec::with_capacity(dimensions.len());
        let mut value_count = 1usize;
        for (axis, &extent) in dimensions.iter().enumerate() {
            let (subscript, selected_count) =
                self.pack_update_subscript(subscripts.get(axis), extent, span)?;
            value_count = value_count.checked_mul(selected_count).ok_or_else(|| {
                LowerError::contract("tensor update selected value extent overflow", span)
            })?;
            compact.push(subscript);
        }
        if value_count != scalar_count(self.view, value) {
            return Err(LowerError::contract(
                "tensor update value shape does not match its projection",
                span,
            ));
        }
        let value_start = self.pack_expression(value)?;
        let count = dimensions
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize));
        let count =
            count.ok_or_else(|| LowerError::contract("tensor update extent overflow", span))?;
        let dst_start = self.next_register;
        for _ in 0..count {
            self.register(span)?;
        }
        self.ops.push(solve::LinearOp::TensorUpdate {
            dst_start,
            base_start,
            value_start,
            dimensions: dimensions.into_boxed_slice(),
            subscripts: compact.into_boxed_slice(),
            lanes: 1,
        });
        self.tensor_update_cache.insert(key, (dst_start, count));
        Ok(dst_start)
    }

    fn pack_update_subscript(
        &mut self,
        subscript: Option<dae::SubscriptView<'dae>>,
        extent: u32,
        span: Span,
    ) -> Result<(solve::TensorUpdateSubscript, usize), LowerError> {
        match subscript {
            Some(dae::SubscriptView::Whole { .. }) | None => {
                Ok((solve::TensorUpdateSubscript::Whole, extent as usize))
            }
            Some(dae::SubscriptView::Index { expression, .. }) => {
                let register = self.expression(expression, 0)?;
                let index = self.integer_register(register).map_or_else(
                    || Ok(solve::TensorIndex::Runtime(register)),
                    |index| checked_index(index, extent, span).map(solve::TensorIndex::Constant),
                )?;
                Ok((solve::TensorUpdateSubscript::Index(index), 1))
            }
            Some(dae::SubscriptView::Slice { expression, .. }) => {
                let dimensions = self.node(expression).value_type().dimensions().to_vec();
                let count = scalar_count(self.view, expression);
                let start = self.pack_expression(expression)?;
                Ok((
                    solve::TensorUpdateSubscript::Slice {
                        start,
                        dimensions: dimensions.into_boxed_slice(),
                    },
                    count,
                ))
            }
        }
    }

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
                if reason == "array subscript is not compile-time computable"
                    || reason == "binder-valued subscript has no active domain" =>
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
        let result_coordinates = row_major_coordinates(result_dimensions, result_scalar)
            .ok_or_else(|| {
                LowerError::contract(
                    "runtime indexed result scalar is outside its checked shape",
                    span,
                )
            })?;
        let mut result_axis = 0usize;
        let mut indices = Vec::with_capacity(base_dimensions.len());
        for (axis, &extent) in base_dimensions.iter().enumerate() {
            let coordinate = result_coordinates.get(result_axis).copied();
            let (index, consumes_result_axis) =
                self.dynamic_index_subscript(subscripts.get(axis), extent, coordinate, span)?;
            indices.push(index);
            result_axis += usize::from(consumes_result_axis);
        }
        if indices
            .iter()
            .all(|index| matches!(index, solve::TensorIndex::Constant(_)))
        {
            let coordinates = indices
                .iter()
                .map(|index| match index {
                    solve::TensorIndex::Constant(coordinate) => *coordinate,
                    solve::TensorIndex::Runtime(_) => unreachable!(),
                })
                .collect::<Vec<_>>();
            let selected = flatten_coordinates(&base_dimensions, &coordinates)
                .expect("checked indexed coordinates belong to the base shape");
            return self.expression(base, selected);
        }
        if let Some(carried_base) = self.fold_carried_tensor_base(base) {
            let dst = self.register(span)?;
            self.ops.push(solve::LinearOp::LoadIndexedFoldCarried {
                dst,
                base: carried_base,
                stride: 1,
                dimensions: base_dimensions.into_boxed_slice(),
                indices: indices.into_boxed_slice(),
            });
            return Ok(dst);
        }
        if let Some(capture_base) = self.fold_capture_tensor_base(base, span)? {
            let dst = self.register(span)?;
            self.ops.push(solve::LinearOp::LoadIndexedFoldCapture {
                dst,
                base: capture_base,
                stride: 1,
                dimensions: base_dimensions.into_boxed_slice(),
                indices: indices.into_boxed_slice(),
            });
            return Ok(dst);
        }
        let base = self.pack_expression(base)?;
        let dst = self.register(span)?;
        self.ops.push(solve::LinearOp::LoadIndexedRegister {
            dst,
            base,
            stride: 1,
            dimensions: base_dimensions.into_boxed_slice(),
            indices: indices.into_boxed_slice(),
        });
        Ok(dst)
    }

    fn dynamic_index_subscript(
        &mut self,
        subscript: Option<dae::SubscriptView<'dae>>,
        extent: u32,
        result_coordinate: Option<u32>,
        span: Span,
    ) -> Result<(solve::TensorIndex, bool), LowerError> {
        match subscript {
            Some(dae::SubscriptView::Index { expression, .. }) => {
                let register = self.expression(expression, 0)?;
                let index = self.integer_register(register).map_or_else(
                    || Ok(solve::TensorIndex::Runtime(register)),
                    |index| checked_index(index, extent, span).map(solve::TensorIndex::Constant),
                )?;
                Ok((index, false))
            }
            Some(dae::SubscriptView::Whole { .. }) | None => result_coordinate
                .map(|coordinate| (solve::TensorIndex::Constant(coordinate), true))
                .ok_or_else(|| {
                    LowerError::contract(
                        "runtime indexed result rank does not match its base projection",
                        span,
                    )
                }),
            Some(dae::SubscriptView::Slice { .. }) => Err(LowerError::non_computable(
                "runtime indexed slices do not yet have a compact Solve owner",
                span,
            )),
        }
    }

    fn fold_capture_tensor_base(
        &mut self,
        expression: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<Option<usize>, LowerError> {
        let Some(deferred) = self.deferred_fold_captures.as_ref() else {
            return Ok(None);
        };
        if let Some(&base) = deferred.packed_capture_ranges.get(&expression) {
            return Ok(Some(base));
        }
        let Some(&(source, count)) = deferred.packed_expressions.get(&expression) else {
            return Ok(None);
        };
        let capture_base = deferred.sources.len();
        let mut sources = Vec::with_capacity(count);
        for offset in 0..count {
            let offset = u32::try_from(offset).map_err(|_| {
                LowerError::contract("fold tensor capture offset exceeds u32", span)
            })?;
            sources.push(source.checked_add(offset).ok_or_else(|| {
                LowerError::contract("fold tensor capture register overflows", span)
            })?);
        }
        let deferred = self
            .deferred_fold_captures
            .as_mut()
            .expect("checked deferred fold captures remain active");
        deferred.sources.extend(sources);
        deferred
            .packed_capture_ranges
            .insert(expression, capture_base);
        Ok(Some(capture_base))
    }

    pub(super) fn fold_carried_tensor_base(&self, expression: dae::ExprId<'dae>) -> Option<usize> {
        let dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. } =
            self.node(expression).operation()
        else {
            return None;
        };
        let first = self
            .function_fold_values
            .iter()
            .rev()
            .find_map(|(active, values)| {
                (*active == fold)
                    .then(|| values.get(carried as usize)?.first().copied())
                    .flatten()
            })?;
        self.ops.iter().find_map(|operation| match operation {
            solve::LinearOp::LoadFoldCarried { dst, index } if *dst == first => Some(*index),
            _ => None,
        })
    }

    pub(super) fn dynamic_scalar_indices(
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

    pub(super) fn constant_index_scalar(
        &self,
        indices: &[solve::Reg],
        dimensions: &[u32],
    ) -> Option<usize> {
        let coordinates = indices
            .iter()
            .zip(dimensions)
            .map(|(&index, &extent)| {
                let index = self.integer_register(index)?.checked_sub(1)?;
                let coordinate = u32::try_from(index).ok()?;
                (coordinate < extent).then_some(coordinate)
            })
            .collect::<Option<Vec<_>>>()?;
        flatten_coordinates(dimensions, &coordinates)
    }

    pub(super) fn dynamic_coordinate_match(
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
        let coordinates =
            row_major_coordinates(&base_dimensions, base_scalar).ok_or_else(|| {
                LowerError::contract(
                    "runtime array update scalar is outside its checked base shape",
                    span,
                )
            })?;
        let mut matches = self.constant(1.0, span)?;
        let mut value_coordinates = Vec::new();
        for (axis, (&extent, &coordinate)) in base_dimensions.iter().zip(&coordinates).enumerate() {
            match self.array_update_axis(subscripts.get(axis), extent, coordinate, span)? {
                ArrayUpdateAxis::Whole(coordinate) => value_coordinates.push(coordinate),
                ArrayUpdateAxis::Runtime(axis_matches) => {
                    matches = self.binary(dae::BinaryOperator::And, matches, axis_matches, span)?;
                }
                ArrayUpdateAxis::ConstantMatch => {}
                ArrayUpdateAxis::ConstantMismatch => {
                    return self.expression(base, base_scalar);
                }
            }
        }
        let value_scalar = flatten_coordinates(
            self.node(value).value_type().dimensions(),
            &value_coordinates,
        )
        .ok_or_else(|| {
            LowerError::contract(
                "runtime array update selection does not match its checked value shape",
                span,
            )
        })?;
        let updated = self.expression(value, value_scalar)?;
        let unchanged = self.expression(base, base_scalar)?;
        self.select(matches, updated, unchanged, span)
    }

    fn array_update_axis(
        &mut self,
        subscript: Option<dae::SubscriptView<'dae>>,
        extent: u32,
        coordinate: u32,
        span: Span,
    ) -> Result<ArrayUpdateAxis, LowerError> {
        let Some(dae::SubscriptView::Index { expression, .. }) = subscript else {
            return match subscript {
                Some(dae::SubscriptView::Slice { .. }) => Err(LowerError::non_computable(
                    "runtime array-update slices do not yet have a compact Solve owner",
                    span,
                )),
                _ => Ok(ArrayUpdateAxis::Whole(coordinate)),
            };
        };
        let runtime_index = self.expression(expression, 0)?;
        if let Some(index) = self.integer_register(runtime_index) {
            return Ok(if checked_index(index, extent, span)? == coordinate {
                ArrayUpdateAxis::ConstantMatch
            } else {
                ArrayUpdateAxis::ConstantMismatch
            });
        }
        let modelica_index = self.constant(f64::from(coordinate + 1), span)?;
        let matches = self.binary(
            dae::BinaryOperator::Equal,
            runtime_index,
            modelica_index,
            span,
        )?;
        Ok(ArrayUpdateAxis::Runtime(matches))
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
        if let Some(selected) = self.constant_index_scalar(&runtime_indices, &base_dimensions) {
            return if selected == base_record {
                self.record_field(value, field, field_scalar, span)
            } else {
                let field_width = self
                    .view
                    .record_field_layout(self.node(base).value_type_id(), field)
                    .expect("checked record projection has a finite field layout")
                    .field_width();
                self.record_field(base, field, base_record * field_width + field_scalar, span)
            };
        }
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
