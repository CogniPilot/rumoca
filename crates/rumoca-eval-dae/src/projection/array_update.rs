//! Exact coordinate dependencies of a checked tensor update.

use super::*;

impl<'dae, F> Projection<'_, 'dae, F>
where
    F: FnMut(dae::CoordinateView<'dae>, usize),
{
    pub(super) fn array_update_scalar(
        &mut self,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        scalar: usize,
    ) -> Result<(), ProjectionError> {
        match self.updated_value_scalar(base, value, subscripts, scalar) {
            Ok(Some(selected)) => self.expression(value, selected)?,
            Ok(None) => self.expression(base, scalar)?,
            Err(ProjectionError::DynamicSubscript { .. }) => {
                self.expression(base, scalar)?;
                self.all_scalars(value)?;
            }
            Err(error) => return Err(error),
        }
        self.subscripts(subscripts)
    }

    pub(super) fn array_update_field(
        &mut self,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        field: usize,
        scalar: usize,
    ) -> Result<(), ProjectionError> {
        let width = self.record_field_width(base, field);
        match self.updated_value_scalar(base, value, subscripts, scalar / width) {
            Ok(Some(selected)) => {
                self.record_field(value, field, selected * width + scalar % width)?
            }
            Ok(None) => self.record_field(base, field, scalar)?,
            Err(ProjectionError::DynamicSubscript { .. }) => {
                self.record_field(base, field, scalar)?;
                self.all_record_field_scalars(value, field)?;
            }
            Err(error) => return Err(error),
        }
        self.subscripts(subscripts)
    }

    /// Invert the checked selection for one base coordinate. An unselected
    /// component still reads its original coordinate, never the replacement.
    fn updated_value_scalar(
        &mut self,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        scalar: usize,
    ) -> Result<Option<usize>, ProjectionError> {
        let base_node = self.node(base);
        let coordinates = row_major_coordinates(base_node.value_type().dimensions(), scalar)
            .expect("checked update scalar lies within its base");
        let mut value_coordinates = Vec::new();
        let mut selected = true;
        for (axis, (&extent, &coordinate)) in base_node
            .value_type()
            .dimensions()
            .iter()
            .zip(&coordinates)
            .enumerate()
        {
            match self.updated_axis_coordinates(subscripts.get(axis), extent, coordinate)? {
                Some(indices) => value_coordinates.extend(indices),
                None => selected = false,
            }
        }
        if !selected {
            return Ok(None);
        }
        Ok(Some(
            flatten_coordinates(
                self.node(value).value_type().dimensions(),
                &value_coordinates,
            )
            .expect("checked update selection matches its replacement shape"),
        ))
    }

    fn updated_axis_coordinates(
        &mut self,
        subscript: Option<dae::SubscriptView<'dae>>,
        extent: u32,
        coordinate: u32,
    ) -> Result<Option<Vec<u32>>, ProjectionError> {
        match subscript {
            Some(dae::SubscriptView::Whole { .. }) | None => Ok(Some(vec![coordinate])),
            Some(dae::SubscriptView::Index {
                expression,
                provenance,
            }) => {
                let index = self.constant_update_integer(expression, 0)?;
                let selected = checked_index(index, extent, provenance.span())?;
                Ok((selected == coordinate).then(Vec::new))
            }
            Some(dae::SubscriptView::Slice {
                expression,
                provenance,
            }) => self.updated_slice_coordinates(expression, extent, coordinate, provenance.span()),
        }
    }

    fn updated_slice_coordinates(
        &mut self,
        expression: dae::ExprId<'dae>,
        extent: u32,
        coordinate: u32,
        span: Span,
    ) -> Result<Option<Vec<u32>>, ProjectionError> {
        let mut selected = None;
        for scalar in 0..self.scalar_count(expression) {
            let index = self.constant_update_integer(expression, scalar)?;
            if checked_index(index, extent, span)? == coordinate {
                selected = Some(scalar);
            }
        }
        Ok(selected.map(|scalar| {
            row_major_coordinates(self.node(expression).value_type().dimensions(), scalar)
                .expect("checked slice scalar resolves")
        }))
    }

    fn constant_update_integer(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<i64, ProjectionError> {
        let node = self.node(expression);
        if node.variability() != dae::ExpressionVariability::Constant {
            return Err(ProjectionError::DynamicSubscript {
                span: node.provenance().span(),
            });
        }
        self.integer(expression, scalar)
    }
}
