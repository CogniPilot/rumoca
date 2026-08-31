//! Function-fold lowering for [`ExpressionLowerer`].
//!
//! A fold projects a function call over an aggregate domain. These methods
//! decide whether each fold parameter and output resolves statically or needs a
//! dynamic projection, and cache the outputs so one call is emitted per domain
//! point rather than per referenced result.

use super::*;

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    pub(super) fn lower_function_fold_parameter_at(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        if self.function_scope == Some(fold.function()) {
            return self.lower_direct_function_fold_value(fold, carried, indices, span);
        }
        let values = self
            .function_fold_values
            .iter()
            .rev()
            .find_map(|(active, values)| (*active == fold).then_some(values))
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "function loop parameter escaped its checked fold".to_owned(),
            })?;
        let value_type = self
            .view
            .function_fold(fold)
            .and_then(|fold| fold.parameter_values().rhs(carried as usize))
            .and_then(|value| self.view.expression(value))
            .map(|value| value.value_type())
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "function loop parameter type does not resolve".to_owned(),
            })?;
        let Some(scalar) = literal_scalar_index(value_type.dimensions(), indices) else {
            return self.lower_dynamic_function_fold_parameter(
                fold,
                carried,
                value_type.dimensions(),
                indices,
                span,
            );
        };
        values
            .get(carried as usize)
            .and_then(|value| value.get(scalar as usize))
            .cloned()
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "function loop parameter scalar is out of range".to_owned(),
            })
    }

    fn lower_dynamic_function_fold_parameter(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        dimensions: &[u32],
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let values = self
            .function_fold_values
            .iter()
            .rev()
            .find_map(|(active, values)| (*active == fold).then_some(values))
            .and_then(|values| values.get(carried as usize))
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "function loop parameter escaped its checked fold".to_owned(),
            })?;
        let candidates = self.proven_dynamic_projection_candidates(dimensions, indices, span)?;
        let mut selected = Vec::with_capacity(candidates.len());
        for candidate in candidates {
            let projection = candidate
                .iter()
                .map(|index| gast::Expression::Integer(i64::from(*index)))
                .collect::<Vec<_>>();
            let scalar = literal_scalar_index(dimensions, &projection)
                .expect("literal checked projection has a scalar ordinal");
            let value = values.get(scalar as usize).cloned().ok_or_else(|| {
                GalecTargetError::LoweringInternal {
                    detail: "function loop parameter scalar is out of range".to_owned(),
                }
            })?;
            selected.push((candidate, value));
        }
        select_dynamic_typed_expression(indices, selected)
    }

    pub(super) fn lower_function_fold_output_at(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        if self.function_scope == Some(fold.function()) {
            return self.lower_direct_function_fold_value(fold, carried, indices, span);
        }
        let fold_view =
            self.view
                .function_fold(fold)
                .ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail: "function fold identity does not resolve".to_owned(),
                })?;
        let output = fold_view
            .output_values()
            .rhs(carried as usize)
            .and_then(|value| self.view.expression(value))
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "function fold output type does not resolve".to_owned(),
            })?;
        let Some(scalar) = literal_scalar_index(output.value_type().dimensions(), indices) else {
            return self.lower_dynamic_function_fold_output(
                fold,
                carried,
                output.value_type().dimensions(),
                indices,
                span,
            );
        };
        let key = self.function_fold_output_key(fold, carried, scalar);
        if let Some(value) = self.function_fold_output_cache.get(&key) {
            return Ok(value.clone());
        }

        let mut values = fold_view
            .initial_values()
            .rhs_iter()
            .map(|initial| self.lower_function_value_scalars(initial))
            .collect::<Result<Vec<_>, _>>()?;
        let domain = self
            .view
            .domain(fold_view.domain())
            .expect("checked function fold domain resolves");
        let structured = domain.structured().validated().map_err(|error| {
            unsupported(
                "function-fold-domain",
                format!("checked function fold domain became invalid: {error}"),
                span,
            )
        })?;
        for point in 0..structured.scalar_count() {
            let binder_values = structured
                .index_tuple_at(point)
                .expect("checked function fold point is in range")
                .into_iter()
                .map(gast::Expression::Integer)
                .collect();
            self.enter_iteration_point(
                IterationOwner::FunctionFold {
                    function: fold.function().index(),
                    fold: fold.ordinal(),
                },
                fold_view.domain().index(),
                binder_values,
            )?;
            self.function_fold_values.push((fold, values));
            let updates = fold_view
                .update_values()
                .rhs_iter()
                .map(|update| self.lower_function_value_scalars(update))
                .collect::<Result<Vec<_>, _>>();
            let (_, previous) = self
                .function_fold_values
                .pop()
                .expect("function fold frame was just pushed");
            self.leave_iteration_point();
            values = updates?;
            debug_assert_eq!(previous.len(), values.len());
        }
        self.cache_function_fold_outputs(&key, values)?;
        self.function_fold_output_cache
            .get(&key)
            .cloned()
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "function fold output scalar is out of range".to_owned(),
            })
    }

    fn function_fold_output_key(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        scalar: u32,
    ) -> FunctionFoldOutputKey {
        FunctionFoldOutputKey {
            call_path: self
                .call_frames
                .iter()
                .map(|frame| MaterializedCallKey {
                    owner: frame.owner.index(),
                    function: frame.function.index(),
                    arguments: frame
                        .arguments
                        .iter()
                        .map(|argument| argument.index())
                        .collect(),
                    indices: frame.indices.clone(),
                    iteration_path: self.iteration_path(),
                })
                .collect(),
            iteration_path: self.iteration_path(),
            fold: fold.ordinal(),
            carried,
            scalar,
        }
    }

    fn cache_function_fold_outputs(
        &mut self,
        key: &FunctionFoldOutputKey,
        values: Vec<Vec<TypedExpression>>,
    ) -> Result<(), GalecTargetError> {
        for (carried_ordinal, carried_values) in values.into_iter().enumerate() {
            let carried = checked_fold_ordinal(
                carried_ordinal,
                "function fold carried-value capacity exceeded",
            )?;
            for (scalar_ordinal, value) in carried_values.into_iter().enumerate() {
                let scalar =
                    checked_fold_ordinal(scalar_ordinal, "function fold scalar capacity exceeded")?;
                self.function_fold_output_cache.insert(
                    FunctionFoldOutputKey {
                        call_path: key.call_path.clone(),
                        iteration_path: key.iteration_path.clone(),
                        fold: key.fold,
                        carried,
                        scalar,
                    },
                    value,
                );
            }
        }
        Ok(())
    }

    fn lower_direct_function_fold_value(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let target = self
            .view
            .function_fold(fold)
            .and_then(|fold| fold.targets().nth(carried as usize))
            .and_then(|target| {
                self.view
                    .function(target.function())
                    .and_then(|function| function.values().find(|value| value.id() == target))
            })
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "direct function loop target does not resolve".to_owned(),
            })?;
        let value_type = self
            .view
            .value_type(target.value_type())
            .expect("checked direct function loop target type resolves");
        Ok(TypedExpression {
            expression: gast::Expression::Ref(gast::Reference::Local(gast::RefPart {
                name: user_functions::value_name(target)?,
                subscripts: indices.to_vec(),
                span,
            })),
            scalar_type: scalar_type(
                value_type.scalar_type(),
                target.name().as_str(),
                target.declaration().span(),
            )?,
        })
    }

    fn lower_dynamic_function_fold_output(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        dimensions: &[u32],
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let candidates = self.proven_dynamic_projection_candidates(dimensions, indices, span)?;
        let mut selected = Vec::with_capacity(candidates.len());
        for candidate in candidates {
            let projection = candidate
                .iter()
                .map(|index| gast::Expression::Integer(i64::from(*index)))
                .collect::<Vec<_>>();
            let value = self.lower_function_fold_output_at(fold, carried, &projection, span)?;
            selected.push((candidate, value));
        }
        select_dynamic_typed_expression(indices, selected)
    }

    fn proven_dynamic_projection_candidates(
        &self,
        dimensions: &[u32],
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<Vec<Vec<u32>>, GalecTargetError> {
        if dimensions.len() != indices.len() {
            return Err(unsupported(
                "dynamic-function-fold-projection",
                "function loop projection rank does not match its checked value".to_owned(),
                span,
            ));
        }
        for (index, extent) in indices.iter().zip(dimensions) {
            if constant_integer(index).is_none() {
                self.prove_dynamic_index(index, *extent, span)?;
            }
        }
        Ok(row_major_indices(dimensions)
            .into_iter()
            .filter(|candidate| {
                indices.iter().zip(candidate).all(|(index, candidate)| {
                    !matches!(index, gast::Expression::Integer(found) if *found != i64::from(*candidate))
                })
            })
            .collect())
    }

    fn lower_function_value_scalars(
        &mut self,
        value: dae::ExprId<'dae>,
    ) -> Result<Vec<TypedExpression>, GalecTargetError> {
        let value_type = self
            .view
            .expression(value)
            .expect("checked function value resolves")
            .value_type();
        let count = value_type.scalar_count().ok_or_else(|| {
            unsupported(
                "record-function-fold",
                "record-valued function loop state is not yet supported".to_owned(),
                self.view
                    .expression(value)
                    .expect("checked function value resolves")
                    .provenance()
                    .span(),
            )
        })?;
        (0..count)
            .map(|scalar| {
                let indices = value_type
                    .scalar_subscripts(scalar)
                    .expect("checked primitive value has scalar subscripts");
                self.lower_element(value, &indices)
            })
            .collect()
    }
}

fn checked_fold_ordinal(value: usize, detail: &str) -> Result<u32, GalecTargetError> {
    u32::try_from(value).map_err(|_| GalecTargetError::LoweringInternal {
        detail: detail.to_owned(),
    })
}

fn select_dynamic_typed_expression(
    indices: &[gast::Expression],
    mut selected: Vec<(Vec<u32>, TypedExpression)>,
) -> Result<TypedExpression, GalecTargetError> {
    let (_, fallback) = selected
        .pop()
        .ok_or_else(|| GalecTargetError::LoweringInternal {
            detail: "checked dynamic function-loop projection has no candidate".to_owned(),
        })?;
    if selected.is_empty() {
        return Ok(fallback);
    }
    let scalar_type = fallback.scalar_type;
    let branches = selected
        .into_iter()
        .map(|(candidate, value)| {
            debug_assert_eq!(value.scalar_type, scalar_type);
            let condition = indices
                .iter()
                .zip(candidate)
                .filter(|(index, _)| constant_integer(index).is_none())
                .map(|(index, candidate)| {
                    gast::Expression::binary(
                        gast::BinaryOp::Eq,
                        index.clone(),
                        gast::Expression::Integer(i64::from(candidate)),
                    )
                })
                .reduce(|lhs, rhs| gast::Expression::binary(gast::BinaryOp::And, lhs, rhs))
                .expect("dynamic function-loop projection has one dynamic index");
            (condition, value.expression)
        })
        .collect();
    Ok(TypedExpression {
        expression: gast::Expression::If(gast::IfExpression::new(branches, fallback.expression)),
        scalar_type,
    })
}
