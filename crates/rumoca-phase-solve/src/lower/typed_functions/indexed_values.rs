//! Projection of indexed values: element, slice, and tensor-view subscripts.

use super::*;

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    pub(super) fn index(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        if self.needs_indexed_slice(subscripts) {
            return self.indexed_slice(value_type, base, subscripts, at);
        }
        let base = self.expression(base)?;
        let base_types = lower_value_type_leaves(self.view, base.value_type, arithmetic_profile())?;
        let result_types = lower_value_type_leaves(self.view, value_type, arithmetic_profile())?;
        if base.leaves.len() != base_types.len() || base.leaves.len() != result_types.len() {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: at,
            });
        }
        if subscripts.iter().all(|subscript| {
            matches!(
                subscript,
                dae::SubscriptView::Whole { .. } | dae::SubscriptView::Slice { .. }
            )
        }) {
            let outer_origin = self.contiguous_slice_origin(subscripts, at)?;
            let leaves = base
                .leaves
                .iter()
                .zip(&base_types)
                .zip(&result_types)
                .map(|((&register, base_type), result_type)| {
                    let mut origin = outer_origin.clone();
                    origin.resize(base_type.dimensions().len(), 0);
                    self.builder.project_slice(
                        register,
                        origin,
                        result_type.dimensions().to_vec(),
                        at,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(LoweredValue { value_type, leaves });
        }
        if subscripts.iter().any(|subscript| {
            matches!(
                subscript,
                dae::SubscriptView::Whole { .. } | dae::SubscriptView::Slice { .. }
            )
        }) {
            if base.leaves.len() != 1 {
                return Err(solve::SolveProgramConstructionError::InvalidProjection {
                    provenance: at,
                });
            }
            let axes = self.tensor_view_axes(subscripts, result_types[0].dimensions(), at)?;
            let result = self.builder.project_view(base.leaves[0], &axes, at)?;
            return Ok(LoweredValue::scalar(value_type, result));
        }
        let index_expressions = subscripts
            .iter()
            .map(|subscript| {
                let dae::SubscriptView::Index { expression, .. } = subscript else {
                    return Err(solve::SolveProgramConstructionError::InvalidProjection {
                        provenance: at,
                    });
                };
                Ok(expression)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let static_indices = index_expressions
            .iter()
            .map(|expression| {
                let node = self.view.expression(*expression)?;
                let dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(index)) =
                    node.operation()
                else {
                    return None;
                };
                index
                    .checked_sub(1)
                    .and_then(|index| u32::try_from(index).ok())
            })
            .collect::<Option<Vec<_>>>();
        let dynamic_indices = static_indices.is_none().then(|| {
            index_expressions
                .iter()
                .map(|expression| self.expression(*expression)?.only_register(at))
                .collect::<Result<Vec<_>, _>>()
        });
        let dynamic_indices = dynamic_indices.transpose()?;
        let mut leaves = Vec::with_capacity(base.leaves.len());
        for ((&register, base_type), result_type) in
            base.leaves.iter().zip(&base_types).zip(&result_types)
        {
            let result = self.project_indexed_leaf(
                register,
                base_type.dimensions().len(),
                result_type.dimensions(),
                static_indices.as_deref(),
                dynamic_indices.as_deref(),
                at,
            )?;
            leaves.push(result);
        }
        Ok(LoweredValue { value_type, leaves })
    }

    pub(super) fn project_indexed_leaf(
        &mut self,
        register: solve::ProgramRegister<'program>,
        base_rank: usize,
        result_dimensions: &[u32],
        static_indices: Option<&[u32]>,
        dynamic_indices: Option<&[solve::ProgramRegister<'program>]>,
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let index_count = static_indices.map_or_else(
            || dynamic_indices.map_or(0, |indices| indices.len()),
            |indices| indices.len(),
        );
        if base_rank == index_count {
            if let Some(indices) = static_indices {
                return self.builder.project_element(register, indices.to_vec(), at);
            }
            let indices =
                dynamic_indices.ok_or(solve::SolveProgramConstructionError::InvalidProjection {
                    provenance: at,
                })?;
            return self.builder.project_element_dynamic(register, indices, at);
        }
        let indices = dynamic_indices
            .ok_or(solve::SolveProgramConstructionError::InvalidProjection { provenance: at })?;
        let mut axes = indices
            .iter()
            .copied()
            .map(solve::ProgramTensorViewAxis::Index)
            .collect::<Vec<_>>();
        axes.extend(
            result_dimensions
                .iter()
                .copied()
                .map(|extent| solve::ProgramTensorViewAxis::Span { origin: 0, extent }),
        );
        self.builder.project_view(register, &axes, at)
    }

    pub(super) fn contiguous_slice_origin(
        &self,
        subscripts: dae::SubscriptsView<'dae>,
        at: rumoca_core::Span,
    ) -> Result<Vec<u32>, solve::SolveProgramConstructionError> {
        subscripts
            .iter()
            .map(|subscript| match subscript {
                dae::SubscriptView::Whole { .. } => Ok(0),
                dae::SubscriptView::Slice { expression, .. } => {
                    let range = self
                        .view
                        .expression(expression)
                        .and_then(|node| match node.operation() {
                            dae::ExpressionOperation::Range(range) => Some(range),
                            _ => None,
                        })
                        .ok_or(solve::SolveProgramConstructionError::InvalidProjection {
                            provenance: at,
                        })?;
                    if range.effective_step() != 1 {
                        return Err(solve::SolveProgramConstructionError::InvalidProjection {
                            provenance: at,
                        });
                    }
                    range
                        .start()
                        .value()
                        .checked_sub(1)
                        .and_then(|index| u32::try_from(index).ok())
                        .ok_or(solve::SolveProgramConstructionError::InvalidProjection {
                            provenance: at,
                        })
                }
                dae::SubscriptView::Index { .. } => {
                    Err(solve::SolveProgramConstructionError::InvalidProjection { provenance: at })
                }
            })
            .collect()
    }

    pub(super) fn tensor_view_axes(
        &mut self,
        subscripts: dae::SubscriptsView<'dae>,
        result_dimensions: &[u32],
        at: rumoca_core::Span,
    ) -> Result<Vec<solve::ProgramTensorViewAxis<'program>>, solve::SolveProgramConstructionError>
    {
        let mut retained = result_dimensions.iter().copied();
        let axes = subscripts
            .iter()
            .map(|subscript| match subscript {
                dae::SubscriptView::Index { expression, .. } => self
                    .expression(expression)?
                    .only_register(at)
                    .map(solve::ProgramTensorViewAxis::Index),
                dae::SubscriptView::Whole { .. } => retained
                    .next()
                    .map(|extent| solve::ProgramTensorViewAxis::Span { origin: 0, extent })
                    .ok_or(solve::SolveProgramConstructionError::InvalidProjection {
                        provenance: at,
                    }),
                dae::SubscriptView::Slice { expression, .. } => {
                    let range = self
                        .view
                        .expression(expression)
                        .and_then(|node| match node.operation() {
                            dae::ExpressionOperation::Range(range) => Some(range),
                            _ => None,
                        })
                        .ok_or(solve::SolveProgramConstructionError::InvalidProjection {
                            provenance: at,
                        })?;
                    if range.effective_step() != 1 {
                        return Err(solve::SolveProgramConstructionError::InvalidProjection {
                            provenance: at,
                        });
                    }
                    let origin = range
                        .start()
                        .value()
                        .checked_sub(1)
                        .and_then(|index| u32::try_from(index).ok())
                        .ok_or(solve::SolveProgramConstructionError::InvalidProjection {
                            provenance: at,
                        })?;
                    let extent = retained.next().ok_or(
                        solve::SolveProgramConstructionError::InvalidProjection { provenance: at },
                    )?;
                    Ok(solve::ProgramTensorViewAxis::Span { origin, extent })
                }
            })
            .collect::<Result<Vec<_>, _>>()?;
        if retained.next().is_some() {
            return Err(solve::SolveProgramConstructionError::InvalidProjection { provenance: at });
        }
        Ok(axes)
    }
}
