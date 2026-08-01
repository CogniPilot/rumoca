//! Translation-time selection over checked DAE expressions.
//!
//! [`ScalarSelector`] answers which operand scalar an output scalar reads,
//! without emitting any op. Subscripts, slices, and array updates are resolved
//! against the extents the DAE already checked, so a projection either names an
//! exact scalar or is rejected as not compile-time computable.

use super::*;

impl<'dae> ScalarSelector<'dae> {
    pub(in crate::lower) const fn view(&self) -> dae::DaeView<'dae> {
        self.view
    }

    pub(in crate::lower) fn new(
        view: dae::DaeView<'dae>,
        domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
    ) -> Self {
        Self {
            view,
            domain_points: domain_point
                .map(|(domain, values)| vec![(domain, values.to_vec())])
                .unwrap_or_default(),
        }
    }

    pub(super) fn from_points(
        view: dae::DaeView<'dae>,
        domain_points: &[(dae::DomainId<'dae>, Vec<i64>)],
    ) -> Self {
        Self {
            view,
            domain_points: domain_points.to_vec(),
        }
    }

    pub(in crate::lower) fn coordinate(
        &self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Option<(dae::CoordinateView<'dae>, usize)>, LowerError> {
        let node = self.node(expression);
        match node.operation() {
            dae::ExpressionOperation::Coordinate(coordinate) => Ok(Some((coordinate, scalar))),
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus,
                operand,
            } => self.coordinate(operand, scalar),
            dae::ExpressionOperation::Array(elements) => {
                let first = elements.get(0).expect("checked array is nonempty");
                let count = scalar_count(self.view, first);
                self.coordinate(
                    elements
                        .get(scalar / count)
                        .expect("checked scalar selects an array element"),
                    scalar % count,
                )
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                let body_count = scalar_count(self.view, body);
                let point = scalar / body_count;
                let values = self
                    .view
                    .domain(domain)
                    .expect("checked comprehension domain resolves")
                    .structured()
                    .index_tuple_at(point)
                    .expect("checked domain remains valid")
                    .expect("checked scalar selects a domain point");
                let mut nested = self.clone();
                nested.domain_points.push((domain, values));
                nested.coordinate(body, scalar % body_count)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                let selected = self.indexed_base_scalar(
                    base,
                    subscripts,
                    node.value_type().dimensions(),
                    scalar,
                )?;
                self.coordinate(base, selected)
            }
            _ => Ok(None),
        }
    }

    pub(in crate::lower) fn select_array_element(
        &self,
        mut expression: dae::ExprId<'dae>,
        mut scalar: usize,
    ) -> Result<(dae::ExprId<'dae>, usize), LowerError> {
        loop {
            let node = self.node(expression);
            let dae::ExpressionOperation::Array(elements) = node.operation() else {
                return Ok((expression, scalar));
            };
            let Some(first) = elements.get(0) else {
                return Err(LowerError::contract(
                    "structured row cannot select an empty aggregate body",
                    node.provenance().span(),
                ));
            };
            let element_count = scalar_count(self.view, first);
            if element_count == 0 {
                return Err(LowerError::contract(
                    "structured row cannot select an empty aggregate element",
                    node.provenance().span(),
                ));
            }
            expression = elements.get(scalar / element_count).ok_or_else(|| {
                LowerError::contract(
                    "structured row selects outside its checked aggregate body",
                    node.provenance().span(),
                )
            })?;
            scalar %= element_count;
        }
    }

    pub(super) fn indexed_base_scalar(
        &self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        result_dimensions: &[u32],
        result_scalar: usize,
    ) -> Result<usize, LowerError> {
        let base_dimensions = self.node(base).value_type().dimensions();
        let result_coordinates = row_major_coordinates(result_dimensions, result_scalar)
            .ok_or_else(|| {
                LowerError::contract(
                    "indexed result scalar is outside its checked shape",
                    self.node(base).provenance().span(),
                )
            })?;
        let mut result_axis = 0usize;
        let mut base_coordinates = Vec::with_capacity(base_dimensions.len());
        for (axis, &extent) in base_dimensions.iter().enumerate() {
            match subscripts.get(axis) {
                Some(dae::SubscriptView::Index {
                    expression,
                    provenance,
                }) => {
                    let index = self.integer(expression, 0)?;
                    base_coordinates.push(checked_index(index, extent, provenance.span())?);
                }
                Some(dae::SubscriptView::Whole { .. }) | None => {
                    base_coordinates.push(result_coordinates[result_axis]);
                    result_axis += 1;
                }
                Some(dae::SubscriptView::Slice {
                    expression,
                    provenance,
                }) => {
                    let (slice_scalar, end) = self.slice_scalar(
                        expression,
                        &result_coordinates,
                        result_axis,
                        provenance.span(),
                    )?;
                    let index = self.integer(expression, slice_scalar)?;
                    base_coordinates.push(checked_index(index, extent, provenance.span())?);
                    result_axis = end;
                }
            }
        }
        flatten_coordinates(base_dimensions, &base_coordinates).ok_or_else(|| {
            LowerError::contract(
                "indexed scalar projection is outside its base shape",
                self.node(base).provenance().span(),
            )
        })
    }

    pub(super) fn array_update_value_scalar(
        &self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        value_dimensions: &[u32],
        base_scalar: usize,
    ) -> Result<Option<usize>, LowerError> {
        let base_node = self.node(base);
        let base_dimensions = base_node.value_type().dimensions();
        let base_coordinates =
            row_major_coordinates(base_dimensions, base_scalar).ok_or_else(|| {
                LowerError::contract(
                    "array update scalar is outside its checked base shape",
                    base_node.provenance().span(),
                )
            })?;
        let mut value_coordinates = Vec::with_capacity(value_dimensions.len());
        for (axis, (&extent, &coordinate)) in
            base_dimensions.iter().zip(&base_coordinates).enumerate()
        {
            let Some(axis_coordinates) =
                self.array_update_axis_coordinates(subscripts.get(axis), extent, coordinate)?
            else {
                return Ok(None);
            };
            value_coordinates.extend(axis_coordinates);
        }
        flatten_coordinates(value_dimensions, &value_coordinates)
            .map(Some)
            .ok_or_else(|| {
                LowerError::contract(
                    "array update selection does not match its checked value shape",
                    base_node.provenance().span(),
                )
            })
    }

    fn array_update_axis_coordinates(
        &self,
        subscript: Option<dae::SubscriptView<'dae>>,
        extent: u32,
        coordinate: u32,
    ) -> Result<Option<Vec<u32>>, LowerError> {
        match subscript {
            Some(dae::SubscriptView::Index {
                expression,
                provenance,
            }) => {
                let index = checked_index(self.integer(expression, 0)?, extent, provenance.span())?;
                Ok((coordinate == index).then(Vec::new))
            }
            Some(dae::SubscriptView::Whole { .. }) | None => Ok(Some(vec![coordinate])),
            Some(dae::SubscriptView::Slice {
                expression,
                provenance,
            }) => {
                let slice_scalar = self.array_update_slice_scalar(
                    expression,
                    extent,
                    coordinate,
                    provenance.span(),
                )?;
                let Some(slice_scalar) = slice_scalar else {
                    return Ok(None);
                };
                row_major_coordinates(
                    self.node(expression).value_type().dimensions(),
                    slice_scalar,
                )
                .map(Some)
                .ok_or_else(|| {
                    LowerError::contract(
                        "array update slice scalar is outside its checked shape",
                        provenance.span(),
                    )
                })
            }
        }
    }

    fn array_update_slice_scalar(
        &self,
        expression: dae::ExprId<'dae>,
        extent: u32,
        coordinate: u32,
        span: Span,
    ) -> Result<Option<usize>, LowerError> {
        for slice_scalar in 0..scalar_count(self.view, expression) {
            let index = checked_index(self.integer(expression, slice_scalar)?, extent, span)?;
            if index == coordinate {
                return Ok(Some(slice_scalar));
            }
        }
        Ok(None)
    }

    fn slice_scalar(
        &self,
        expression: dae::ExprId<'dae>,
        result_coordinates: &[u32],
        result_axis: usize,
        span: Span,
    ) -> Result<(usize, usize), LowerError> {
        let slice_dimensions = self.node(expression).value_type().dimensions();
        let end = result_axis
            .checked_add(slice_dimensions.len())
            .ok_or_else(|| LowerError::contract("slice rank overflow", span))?;
        let slice_coordinates = result_coordinates
            .get(result_axis..end)
            .ok_or_else(|| LowerError::contract("slice coordinates exceed result rank", span))?;
        let scalar = flatten_coordinates(slice_dimensions, slice_coordinates).ok_or_else(|| {
            LowerError::contract("slice scalar projection is outside its shape", span)
        })?;
        Ok((scalar, end))
    }

    /// Probe the declared value of an affine derivative coefficient.
    ///
    /// This reads every parameter's declared binding, including a tunable one
    /// a simulation may override, so the result proves a property of the model
    /// as written. Lowering uses it only to reject a coefficient that is
    /// declared zero; the coefficient itself stays symbolic and is evaluated
    /// from the runtime parameter vector.
    pub(super) fn integer(
        &self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<i64, LowerError> {
        let node = self.node(expression);
        let span = node.provenance().span();
        match node.operation() {
            dae::ExpressionOperation::Literal(
                dae::DaeLiteral::Integer(value) | dae::DaeLiteral::Enumeration(value),
            ) => Ok(*value),
            dae::ExpressionOperation::Range(range) => {
                let offset = i64::try_from(scalar)
                    .map_err(|_| LowerError::contract("integer scalar overflow", span))?;
                range
                    .start()
                    .value()
                    .checked_add(
                        range
                            .effective_step()
                            .checked_mul(offset)
                            .ok_or_else(|| LowerError::contract("integer overflow", span))?,
                    )
                    .ok_or_else(|| LowerError::contract("integer overflow", span))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
                let Some((_, values)) = self
                    .domain_points
                    .iter()
                    .rev()
                    .find(|(domain, _)| *domain == binder.domain())
                else {
                    return Err(LowerError::non_computable(
                        "binder-valued subscript has no active domain",
                        span,
                    ));
                };
                values
                    .get(binder.ordinal() as usize)
                    .copied()
                    .ok_or_else(|| LowerError::contract("binder ordinal is out of range", span))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
                let variable = self
                    .view
                    .variable(parameter.into())
                    .expect("checked parameter coordinate resolves");
                let binding = variable.binding().ok_or_else(|| {
                    LowerError::non_computable(
                        "runtime parameter cannot select a static array subscript",
                        span,
                    )
                })?;
                self.integer(binding, scalar)
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let value = self.integer(operand, scalar)?;
                match operator {
                    dae::UnaryOperator::Plus => Ok(value),
                    dae::UnaryOperator::Negate => value
                        .checked_neg()
                        .ok_or_else(|| LowerError::contract("integer overflow", span)),
                    dae::UnaryOperator::Not => Err(LowerError::non_computable(
                        "Boolean negation is not an integer subscript",
                        span,
                    )),
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => integer_binary(
                operator,
                self.integer(lhs, scalar)?,
                self.integer(rhs, scalar)?,
                span,
            ),
            dae::ExpressionOperation::Array(elements) => {
                let first = elements.get(0).expect("checked array is nonempty");
                let count = scalar_count(self.view, first);
                self.integer(
                    elements
                        .get(scalar / count)
                        .expect("checked integer scalar selects an array element"),
                    scalar % count,
                )
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                let selected = self.indexed_base_scalar(
                    base,
                    subscripts,
                    node.value_type().dimensions(),
                    scalar,
                )?;
                self.integer(base, selected)
            }
            _ => Err(LowerError::non_computable(
                "array subscript is not compile-time computable",
                span,
            )),
        }
    }

    pub(super) fn node(&self, expression: dae::ExprId<'dae>) -> dae::ExpressionView<'dae> {
        self.view
            .expression(expression)
            .expect("branded expression resolves in its DAE")
    }
}
