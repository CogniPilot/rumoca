//! Checked Modelica function inlining, call-scoped assertions, and record
//! field projection for the scalar GALEC expression boundary.

use super::expression_projection::{SelectionBranch, SelectionValue};
use super::*;

#[derive(Clone, Copy)]
struct RecordFieldProjection<'a> {
    field: usize,
    indices: &'a [gast::Expression],
    scalar_type: gast::ScalarType,
    span: Span,
}

fn selection_indices(indices: &[gast::Expression]) -> Vec<Option<i64>> {
    indices
        .iter()
        .map(|index| match index {
            gast::Expression::Integer(value) => Some(*value),
            _ => None,
        })
        .collect()
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

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    pub(super) fn materialize_eager_call(
        &mut self,
        call: dae::ExprId<'dae>,
    ) -> Result<(), GalecTargetError> {
        let node = self
            .view
            .expression(call)
            .expect("checked eager call identity resolves");
        let dae::ExpressionOperation::Call {
            function,
            arguments,
            ..
        } = node.operation()
        else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "an eager-call plan referenced a non-call expression".to_owned(),
            });
        };
        self.materialize_function_call(call, function, arguments, node.provenance().span())?;
        Ok(())
    }

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
        let key = FunctionFoldOutputKey {
            call_path: self
                .call_frames
                .iter()
                .map(|frame| MaterializedCallKey {
                    function: frame.function.index(),
                    arguments: frame
                        .arguments
                        .iter()
                        .map(|argument| argument.index())
                        .collect(),
                    indices: frame.indices.clone(),
                })
                .collect(),
            fold: fold.ordinal(),
            carried,
            scalar,
        };
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
        let structured = domain.structured();
        let point_count = structured.scalar_count().map_err(|error| {
            unsupported(
                "function-fold-domain",
                format!("checked function fold domain became invalid: {error}"),
                span,
            )
        })?;
        for point in 0..point_count {
            let binder_values = structured
                .index_tuple_at(point)
                .expect("checked function fold domain remains valid")
                .expect("checked function fold point is in range")
                .into_iter()
                .map(gast::Expression::Integer)
                .collect();
            self.comprehension_frames.push(ComprehensionFrame {
                domain: fold_view.domain().index(),
                binders: binder_values,
            });
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
            self.comprehension_frames.pop();
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

    pub(super) fn lower_call_at(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        if self.materialize_function_values
            && user_functions::is_directly_lowerable(self.view, function)
        {
            return self.lower_materialized_function_call(
                call, function, output, arguments, indices, span,
            );
        }
        let result = self.enter_function_call(call, function, output, arguments, indices, span)?;
        let lowered = self.lower_at(result, indices);
        self.call_frames.pop();
        lowered
    }

    fn lower_materialized_function_call(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let function_view = self
            .view
            .function(function)
            .expect("checked function identity resolves");
        let result_type_id = function_view
            .result_types()
            .get(output as usize)
            .expect("checked directly lowerable function output resolves");
        let result_type = self
            .view
            .value_type(result_type_id)
            .expect("checked function result type resolves");
        let selected_scalar_type = scalar_type(
            result_type.scalar_type(),
            function_view.name().as_str(),
            function_view.declaration().span(),
        )?;
        let selected =
            user_functions::flattened_result_index(self.view, function_view, output, None)
                .ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail:
                        "primitive function result is missing from the flattened GALEC interface"
                            .to_owned(),
                })?;
        let names = self.materialize_function_call(call, function, arguments, span)?;
        let name = names
            .get(selected)
            .expect("checked flattened function output resolves")
            .clone();
        Ok(TypedExpression {
            expression: gast::Expression::Ref(gast::Reference::Local(gast::RefPart {
                name,
                subscripts: indices.to_vec(),
                span,
            })),
            scalar_type: selected_scalar_type,
        })
    }

    fn materialize_function_call(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        arguments: dae::ExpressionOperands<'dae>,
        span: Span,
    ) -> Result<Vec<gast::Name>, GalecTargetError> {
        let function_view = self
            .view
            .function(function)
            .expect("checked function identity resolves");
        let key = MaterializedFunctionCallKey {
            call_path: self
                .call_frames
                .iter()
                .map(|frame| MaterializedCallKey {
                    function: frame.function.index(),
                    arguments: frame
                        .arguments
                        .iter()
                        .map(|argument| argument.index())
                        .collect(),
                    indices: frame.indices.clone(),
                })
                .collect(),
            activation_path: self.conditional_activation_path.clone(),
            function: function.index(),
            arguments: arguments.iter().map(|argument| argument.index()).collect(),
        };
        let names = if let Some(names) = self.materialized_function_calls.get(&key) {
            names.clone()
        } else {
            let names = self.materialized_result_names(call, function_view, span)?;
            let arguments = self.lower_direct_function_arguments(function_view, arguments, span)?;
            self.pending_prefix_statements.push(gast::Spanned::new(
                gast::Statement::MultiAssignment {
                    targets: names.iter().cloned().map(gast::Reference::local).collect(),
                    call: gast::FunctionCall {
                        function: user_functions::function_name(self.view, function_view)?,
                        arguments,
                    },
                },
                span,
            ));
            self.materialized_function_calls.insert(key, names.clone());
            self.called_user_functions.insert(function.index());
            names
        };
        Ok(names)
    }

    /// Lower one aggregate-valued function assignment directly into its
    /// checked destination storage.
    ///
    /// Modelica functions are pure and GALEC multi-assignment commits outputs
    /// after the function body has evaluated. A single primitive aggregate
    /// result therefore needs no intermediate result tensor, including when an
    /// input aliases the destination: the callee reads its inputs before its
    /// final output copy. Multi-result and record calls retain separate
    /// materialized outputs because their atomic grouping has more than one
    /// destination.
    pub(super) fn lower_direct_aggregate_call_assignment(
        &mut self,
        expression: dae::ExprId<'dae>,
        target: gast::Name,
        span: Span,
    ) -> Result<Option<gast::Spanned<gast::Statement>>, GalecTargetError> {
        let node = self
            .view
            .expression(expression)
            .expect("checked direct call assignment resolves");
        if node.value_type().dimensions().is_empty() {
            return Ok(None);
        }
        let dae::ExpressionOperation::Call {
            function,
            output,
            arguments,
        } = node.operation()
        else {
            return Ok(None);
        };
        let function_view = self
            .view
            .function(function)
            .expect("checked direct call function resolves");
        if output != 0
            || function_view.result_types().len() != 1
            || self
                .view
                .value_type(
                    function_view
                        .result_types()
                        .get(0)
                        .expect("single checked function result exists"),
                )
                .expect("checked direct call result type resolves")
                .is_record()
            || !user_functions::is_directly_lowerable(self.view, function)
        {
            return Ok(None);
        }
        let arguments = self.lower_direct_function_arguments(function_view, arguments, span)?;
        self.called_user_functions.insert(function.index());
        Ok(Some(gast::Spanned::new(
            gast::Statement::MultiAssignment {
                targets: vec![gast::Reference::local(target)],
                call: gast::FunctionCall {
                    function: user_functions::function_name(self.view, function_view)?,
                    arguments,
                },
            },
            span,
        )))
    }

    /// Lower a single record-valued call into the flattened storage owned by
    /// the checked destination record.
    pub(super) fn lower_direct_record_call_assignment(
        &mut self,
        expression: dae::ExprId<'dae>,
        target: dae::FunctionValueView<'dae>,
        target_type: &dae::ValueType,
        span: Span,
    ) -> Result<Option<gast::Spanned<gast::Statement>>, GalecTargetError> {
        if !target_type.is_record() || !target_type.dimensions().is_empty() {
            return Ok(None);
        }
        let node = self
            .view
            .expression(expression)
            .expect("checked direct record call assignment resolves");
        let dae::ExpressionOperation::Call {
            function,
            output,
            arguments,
        } = node.operation()
        else {
            return Ok(None);
        };
        let function_view = self
            .view
            .function(function)
            .expect("checked direct record call function resolves");
        let Some(result_type) = function_view.result_types().get(output as usize) else {
            return Ok(None);
        };
        if output != 0
            || function_view.result_types().len() != 1
            || result_type != target.value_type()
            || !user_functions::is_directly_lowerable(self.view, function)
        {
            return Ok(None);
        }
        let arguments = self.lower_direct_function_arguments(function_view, arguments, span)?;
        let mut targets = Vec::with_capacity(target_type.record_field_count());
        for ordinal in 0..target_type.record_field_count() {
            let (field_name, _) = self
                .view
                .record_field(target.value_type(), ordinal)
                .expect("checked direct record target field resolves");
            targets.push(gast::Reference::local(
                user_functions::record_value_field_name(target, field_name)?,
            ));
        }
        self.called_user_functions.insert(function.index());
        Ok(Some(gast::Spanned::new(
            gast::Statement::MultiAssignment {
                targets,
                call: gast::FunctionCall {
                    function: user_functions::function_name(self.view, function_view)?,
                    arguments,
                },
            },
            span,
        )))
    }

    fn materialized_result_names(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionView<'dae>,
        span: Span,
    ) -> Result<Vec<gast::Name>, GalecTargetError> {
        let mut names = Vec::new();
        let outputs = function
            .values()
            .filter(|value| value.role() == dae::FunctionValueRole::Output)
            .collect::<Vec<_>>();
        for (output, result_type_id) in outputs.into_iter().zip(function.result_types().iter()) {
            let result_type = self
                .view
                .value_type(result_type_id)
                .expect("checked function result type resolves");
            if result_type.is_record() {
                self.append_materialized_record_results(
                    result_type_id,
                    result_type,
                    call,
                    function,
                    output.name().as_str(),
                    span,
                    &mut names,
                )?;
            } else {
                self.append_materialized_result(
                    result_type,
                    call,
                    function,
                    output.name().as_str(),
                    span,
                    &mut names,
                )?;
            }
        }
        Ok(names)
    }

    fn append_materialized_record_results(
        &mut self,
        result_type_id: dae::ValueTypeId<'dae>,
        result_type: &dae::ValueType,
        call: dae::ExprId<'dae>,
        function: dae::FunctionView<'dae>,
        output_name: &str,
        span: Span,
        names: &mut Vec<gast::Name>,
    ) -> Result<(), GalecTargetError> {
        for field in 0..result_type.record_field_count() {
            let (field_name, field_type) = self
                .view
                .record_field(result_type_id, field)
                .expect("checked direct record result field resolves");
            let field_type = self
                .view
                .value_type(field_type)
                .expect("checked direct record result field type resolves");
            self.append_materialized_result(
                field_type,
                call,
                function,
                &format!("{output_name}.{field_name}"),
                span,
                names,
            )?;
        }
        Ok(())
    }

    fn append_materialized_result(
        &mut self,
        result_type: &dae::ValueType,
        call: dae::ExprId<'dae>,
        function: dae::FunctionView<'dae>,
        result_name: &str,
        span: Span,
        names: &mut Vec<gast::Name>,
    ) -> Result<(), GalecTargetError> {
        let result_scalar = scalar_type(result_type.scalar_type(), function.name().as_str(), span)?;
        let materialization = self.temporary_counter;
        let name = crate::mangle::galec_variable_name(&format!(
            "rumoca.tmp.{}.{}.{}.call{}.result{}",
            self.temporary_namespace,
            function.name(),
            result_name,
            call.index(),
            materialization
        ))?;
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(result_scalar),
            name: name.clone(),
            dimensions: user_functions::dimensions(result_type.dimensions()),
            range: gast::RangeAttributes::default(),
            span,
        });
        names.push(name);
        Ok(())
    }

    fn lower_direct_function_arguments(
        &mut self,
        function: dae::FunctionView<'dae>,
        arguments: dae::ExpressionOperands<'dae>,
        span: Span,
    ) -> Result<Vec<gast::Expression>, GalecTargetError> {
        let mut lowered = Vec::new();
        for (parameter, argument) in function.parameters().zip(arguments.iter()) {
            let parameter_type = self
                .view
                .value_type(parameter.value_type())
                .expect("checked direct function parameter type resolves");
            if !parameter_type.is_record() {
                lowered.push(self.lower_function_argument(argument, span)?);
                continue;
            }
            for field in 0..parameter_type.record_field_count() {
                let (_, field_type) = self
                    .view
                    .record_field(parameter.value_type(), field)
                    .expect("checked direct record argument field resolves");
                lowered
                    .push(self.lower_function_record_argument(argument, field, field_type, span)?);
            }
        }
        Ok(lowered)
    }

    fn lower_function_argument(
        &mut self,
        argument: dae::ExprId<'dae>,
        call_span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let node = self
            .view
            .expression(argument)
            .expect("checked function argument resolves");
        if node.value_type().dimensions().is_empty() {
            return self.lower(argument).map(|value| value.expression);
        }
        if let Some(reference) = self.direct_aggregate_function_argument(argument)? {
            return Ok(reference);
        }
        let scalar_type = scalar_type(
            node.value_type().scalar_type(),
            "<function-argument>",
            node.provenance().span(),
        )?;
        if expression_contains_array(self.view, argument) {
            let value = self.lower_aggregate_expression_as(argument, scalar_type)?;
            return self.materialize_aggregate_function_argument(
                value,
                node.value_type().dimensions(),
                scalar_type,
                call_span,
            );
        }
        self.materialize_tensor_function_argument(
            argument,
            None,
            node.value_type().dimensions(),
            scalar_type,
            call_span,
        )
    }

    /// Preserve a whole-array reference at a checked input-only function boundary.
    ///
    /// Copying every tensor coordinate into a generated argument local is only
    /// necessary for computed aggregate expressions. A materialized variable or
    /// current function parameter already has the exact checked shape. A
    /// function-local value read also carries its exact reaching-definition
    /// proof in DAE; function-body validation accepts that read only at a
    /// statement where the definition is current. GALEC construction proves
    /// that input parameters are not written. Keeping those references intact
    /// avoids both a redundant bounded copy and a second full-size stack
    /// allocation in embedded C.
    pub(super) fn direct_aggregate_function_argument(
        &self,
        argument: dae::ExprId<'dae>,
    ) -> Result<Option<gast::Expression>, GalecTargetError> {
        let node = self
            .view
            .expression(argument)
            .expect("checked function argument resolves");
        let span = node.provenance().span();

        if let dae::ExpressionOperation::Field { base, field } = node.operation() {
            if let Some(value) = self
                .materialized_shared_record_fields
                .get(&(base.index(), field as usize))
            {
                return Ok(Some(value.clone()));
            }
            if let Some(value) = self.direct_record_function_argument(base, field as usize)? {
                return Ok(Some(value));
            }
        }

        if let dae::ExpressionOperation::FunctionValue { definition, .. } = node.operation() {
            if self.function_scope != Some(definition.id().function()) {
                return Ok(None);
            }
            let value = self
                .view
                .function(definition.id().function())
                .expect("checked function identity resolves")
                .values()
                .find(|value| value.id() == definition.target())
                .expect("checked function definition target resolves");
            return Ok(Some(gast::Expression::Ref(gast::Reference::Local(
                gast::RefPart {
                    name: user_functions::value_name(value)?,
                    subscripts: Vec::new(),
                    span,
                },
            ))));
        }

        let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() else {
            return Ok(None);
        };

        if let dae::CoordinateView::FunctionParameter(parameter) = coordinate {
            if self.function_scope != Some(parameter.function()) {
                return Ok(None);
            }
            let parameter = self
                .view
                .function(parameter.function())
                .expect("checked function identity resolves")
                .parameters()
                .find(|candidate| candidate.id() == parameter)
                .expect("checked function parameter resolves");
            return Ok(Some(gast::Expression::Ref(gast::Reference::Local(
                gast::RefPart {
                    name: user_functions::parameter_name(parameter)?,
                    subscripts: Vec::new(),
                    span,
                },
            ))));
        }

        if matches!(coordinate, dae::CoordinateView::Algebraic(_)) && self.inline_causal_locals {
            return Ok(None);
        }
        let variable = match coordinate {
            dae::CoordinateView::Parameter(_)
            | dae::CoordinateView::Input(_)
            | dae::CoordinateView::State(_)
            | dae::CoordinateView::Algebraic(_)
            | dae::CoordinateView::DiscreteReal(_)
            | dae::CoordinateView::DiscreteValue(_)
            | dae::CoordinateView::PreDiscreteReal(_)
            | dae::CoordinateView::PreDiscreteValue(_) => coordinate_variable(coordinate, span)?,
            _ => return Ok(None),
        };
        let (variable, previous) = variable;
        let classified = self.by_id.get(&variable.index()).ok_or_else(|| {
            GalecTargetError::UnknownVariableReference {
                name: format!("#{}", variable.index()),
                span: Some(span),
            }
        })?;
        let name = if previous {
            self.pre_names
                .get(&variable.index())
                .ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail: format!(
                        "pre-coordinate for `{}` was not collected",
                        classified.variable.name()
                    ),
                })?
                .clone()
        } else {
            classified.name.clone()
        };
        let reference = gast::RefPart {
            name,
            subscripts: Vec::new(),
            span,
        };
        Ok(Some(gast::Expression::Ref(
            if classified.class == VariableClass::Local {
                gast::Reference::Local(reference)
            } else {
                gast::Reference::State(vec![reference])
            },
        )))
    }

    fn lower_function_record_argument(
        &mut self,
        argument: dae::ExprId<'dae>,
        field: usize,
        field_type_id: dae::ValueTypeId<'dae>,
        call_span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        if let Some(value) = self
            .materialized_shared_record_fields
            .get(&(argument.index(), field))
        {
            return Ok(value.clone());
        }
        let field_type = self
            .view
            .value_type(field_type_id)
            .expect("checked direct record argument field type resolves");
        if let Some(reference) = self.direct_record_function_argument(argument, field)? {
            return Ok(reference);
        }
        let scalar = scalar_type(
            field_type.scalar_type(),
            "<function-record-argument>",
            call_span,
        )?;
        if field_type.dimensions().is_empty() {
            return self
                .lower_record_field_at(argument, field, &[], scalar, call_span)
                .map(|value| value.expression);
        }
        let value = self.lower_aggregate_record_field(argument, field, field_type_id)?;
        self.materialize_aggregate_function_argument(
            value,
            field_type.dimensions(),
            scalar,
            call_span,
        )
    }

    /// Project one field of a checked current record directly to its flattened
    /// GALEC storage. `FunctionValue` carries the exact reaching definition,
    /// so this does not recover identity from source names.
    pub(super) fn direct_record_function_argument(
        &self,
        argument: dae::ExprId<'dae>,
        field: usize,
    ) -> Result<Option<gast::Expression>, GalecTargetError> {
        let node = self
            .view
            .expression(argument)
            .expect("checked record function argument resolves");
        let span = node.provenance().span();
        match node.operation() {
            dae::ExpressionOperation::FunctionValue { definition, .. }
                if self.function_scope == Some(definition.id().function()) =>
            {
                let value = self
                    .view
                    .function(definition.id().function())
                    .expect("checked function identity resolves")
                    .values()
                    .find(|value| value.id() == definition.target())
                    .expect("checked record definition target resolves");
                let (field_name, _) = self
                    .view
                    .record_field(value.value_type(), field)
                    .expect("checked record value field resolves");
                Ok(Some(gast::Expression::Ref(gast::Reference::Local(
                    gast::RefPart {
                        name: user_functions::record_value_field_name(value, field_name)?,
                        subscripts: Vec::new(),
                        span,
                    },
                ))))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) if self.function_scope == Some(parameter.function()) => {
                let parameter = self
                    .view
                    .function(parameter.function())
                    .expect("checked function identity resolves")
                    .parameters()
                    .find(|candidate| candidate.id() == parameter)
                    .expect("checked record parameter resolves");
                let (field_name, _) = self
                    .view
                    .record_field(parameter.value_type(), field)
                    .expect("checked record parameter field resolves");
                Ok(Some(gast::Expression::Ref(gast::Reference::Local(
                    gast::RefPart {
                        name: user_functions::record_parameter_field_name(parameter, field_name)?,
                        subscripts: Vec::new(),
                        span,
                    },
                ))))
            }
            _ => Ok(None),
        }
    }

    /// Preserve a whole aggregate record field when the record expression or
    /// its selected constructor field already denotes checked storage.
    pub(super) fn direct_aggregate_record_field(
        &self,
        expression: dae::ExprId<'dae>,
        field: usize,
    ) -> Result<Option<gast::Expression>, GalecTargetError> {
        if let Some(reference) = self.direct_record_function_argument(expression, field)? {
            return Ok(Some(reference));
        }
        let node = self
            .view
            .expression(expression)
            .expect("checked aggregate record field resolves");
        let dae::ExpressionOperation::Record(fields) = node.operation() else {
            return Ok(None);
        };
        let Some(field) = fields.get(field) else {
            return Ok(None);
        };
        self.direct_aggregate_function_argument(field)
    }

    fn materialize_aggregate_function_argument(
        &mut self,
        value: gast::Expression,
        dimensions: &[u32],
        scalar: gast::ScalarType,
        call_span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let name = self.declare_function_argument(dimensions, scalar, call_span);
        self.pending_prefix_statements.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(name.clone()),
                value,
            },
            call_span,
        ));
        Ok(gast::Expression::Ref(gast::Reference::local(name)))
    }

    fn materialize_tensor_function_argument(
        &mut self,
        argument: dae::ExprId<'dae>,
        record_field: Option<usize>,
        dimensions: &[u32],
        scalar: gast::ScalarType,
        call_span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let name = self.declare_function_argument(dimensions, scalar, call_span);

        let iterators = dimensions
            .iter()
            .enumerate()
            .map(|(axis, _)| {
                gast::Name::ident(format!(
                    "rumoca_argument_{}_{}_{}",
                    self.temporary_namespace, self.temporary_counter, axis
                ))
            })
            .collect::<Vec<_>>();
        self.temporary_counter += 1;
        let bounds_depth = self.loop_index_bounds.len();
        for (iterator, &extent) in iterators.iter().zip(dimensions) {
            self.loop_index_bounds.push(LoopIndexBound {
                name: iterator.clone(),
                minimum: 1,
                maximum: i64::from(extent),
            });
        }
        let indices = iterators
            .iter()
            .cloned()
            .map(|iterator| gast::Expression::Ref(gast::Reference::local(iterator)))
            .collect::<Vec<_>>();
        let prefix_start = self.pending_prefix_statements.len();
        let value = match record_field {
            Some(field) => self.lower_record_field_at(argument, field, &indices, scalar, call_span),
            None => self.lower_at(argument, &indices),
        };
        self.loop_index_bounds.truncate(bounds_depth);
        let value = coerce(value?, scalar, call_span)?;
        let prefixes = self.pending_prefix_statements.split_off(prefix_start);
        let (before, mut body) = user_functions::partition_tensor_prefixes(prefixes, &iterators);
        body.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::Local(gast::RefPart {
                    name: name.clone(),
                    subscripts: indices,
                    span: call_span,
                }),
                value,
            },
            call_span,
        ));
        if let Some(fused) = user_functions::fuse_guarded_tensor_loop(
            before.clone(),
            body.clone(),
            &iterators,
            dimensions,
            call_span,
        ) {
            self.pending_prefix_statements.extend(fused);
            return Ok(gast::Expression::Ref(gast::Reference::local(name)));
        }
        self.pending_prefix_statements.extend(before);
        for (iterator, &extent) in iterators.iter().zip(dimensions).rev() {
            body = vec![gast::Spanned::new(
                gast::Statement::For(gast::ForLoop {
                    iterator: Some(iterator.clone()),
                    start: gast::Expression::Integer(1),
                    step: None,
                    stop: gast::Expression::Integer(i64::from(extent)),
                    body,
                }),
                call_span,
            )];
        }
        self.pending_prefix_statements
            .push(body.pop().expect("tensor argument has one outer loop"));
        Ok(gast::Expression::Ref(gast::Reference::local(name)))
    }

    fn declare_function_argument(
        &mut self,
        dimensions: &[u32],
        scalar: gast::ScalarType,
        span: Span,
    ) -> gast::Name {
        let name = gast::Name::ident(format!(
            "rumoca_{}_argument_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(scalar),
            name: name.clone(),
            dimensions: user_functions::dimensions(dimensions),
            range: gast::RangeAttributes::default(),
            span,
        });
        name
    }

    pub(super) fn lower_aggregate_expression_as(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar_type: gast::ScalarType,
    ) -> Result<gast::Expression, GalecTargetError> {
        let node = self
            .view
            .expression(expression)
            .expect("checked aggregate argument resolves");
        self.lower_aggregate_expression_at(
            expression,
            node.value_type().dimensions(),
            &mut Vec::new(),
            scalar_type,
        )
    }

    pub(super) fn lower_aggregate_record_field(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: usize,
        field_type: dae::ValueTypeId<'dae>,
    ) -> Result<gast::Expression, GalecTargetError> {
        let field_type = self
            .view
            .value_type(field_type)
            .expect("checked record field type resolves");
        let span = self
            .view
            .expression(expression)
            .expect("checked record expression resolves")
            .provenance()
            .span();
        let scalar_type = scalar_type(field_type.scalar_type(), "<record-field>", span)?;
        self.lower_aggregate_record_field_at(
            expression,
            field,
            field_type.dimensions(),
            &mut Vec::new(),
            scalar_type,
            span,
        )
    }

    fn lower_aggregate_record_field_at(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: usize,
        remaining: &[u32],
        indices: &mut Vec<gast::Expression>,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let Some((&extent, tail)) = remaining.split_first() else {
            let value =
                self.lower_record_field_at(expression, field, indices, scalar_type, span)?;
            return coerce(value, scalar_type, span);
        };
        let mut elements = Vec::with_capacity(extent as usize);
        for index in 1..=extent {
            indices.push(gast::Expression::Integer(i64::from(index)));
            elements.push(self.lower_aggregate_record_field_at(
                expression,
                field,
                tail,
                indices,
                scalar_type,
                span,
            )?);
            indices.pop();
        }
        Ok(gast::Expression::Array(elements))
    }

    fn lower_aggregate_expression_at(
        &mut self,
        expression: dae::ExprId<'dae>,
        remaining: &[u32],
        indices: &mut Vec<gast::Expression>,
        scalar_type: gast::ScalarType,
    ) -> Result<gast::Expression, GalecTargetError> {
        let Some((&extent, tail)) = remaining.split_first() else {
            let span = self
                .view
                .expression(expression)
                .expect("checked aggregate argument resolves")
                .provenance()
                .span();
            return coerce(self.lower_at(expression, indices)?, scalar_type, span);
        };
        let mut elements = Vec::with_capacity(extent as usize);
        for index in 1..=extent {
            indices.push(gast::Expression::Integer(i64::from(index)));
            elements.push(self.lower_aggregate_expression_at(
                expression,
                tail,
                indices,
                scalar_type,
            )?);
            indices.pop();
        }
        Ok(gast::Expression::Array(elements))
    }

    fn enter_function_call(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<dae::ExprId<'dae>, GalecTargetError> {
        if self.call_frames.iter().any(|frame| frame.call == call) {
            return Err(unsupported(
                "recursive-function",
                "recursive checked function cannot be inlined into GALEC".to_owned(),
                span,
            ));
        }
        let result = self.function_result(function, output)?;
        let call_rank = self
            .view
            .expression(call)
            .expect("checked function call resolves")
            .value_type()
            .dimensions()
            .len();
        let call_indices =
            indices
                .get(..call_rank)
                .ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail: "function call projection is shorter than its checked result rank"
                        .to_owned(),
                })?;
        self.call_frames.push(CallFrame {
            call,
            function,
            arguments: arguments.iter().collect(),
            // A field projection appends the field's dimensions after any
            // outer record-array dimensions. Those suffix coordinates select
            // the result *after* this call executes and therefore are not part
            // of the call identity.
            indices: selection_indices(call_indices),
        });
        if let Err(error) = self.capture_function_assertions(function) {
            self.call_frames.pop();
            return Err(error);
        }
        Ok(result)
    }

    fn capture_function_assertions(
        &mut self,
        function: dae::FunctionId<'dae>,
    ) -> Result<(), GalecTargetError> {
        let function_view = self
            .view
            .function(function)
            .expect("checked function identity resolves");
        let Some(assertion) = first_function_assertion(function_view.statements()) else {
            return Ok(());
        };
        if !self.capture_assertions {
            return Err(unsupported(
                "function-assertion",
                format!(
                    "function `{}` contains a call-scoped assertion",
                    function_view.name()
                ),
                assertion,
            ));
        }
        let key = FunctionAssertionCallKey {
            path: self
                .call_frames
                .iter()
                .map(|frame| {
                    let span = self
                        .view
                        .expression(frame.call)
                        .expect("checked call expression resolves")
                        .provenance()
                        .span();
                    FunctionAssertionCallSite {
                        function: frame.function.index(),
                        arguments: frame
                            .arguments
                            .iter()
                            .map(|argument| argument.index())
                            .collect(),
                        indices: frame.indices.clone(),
                        span,
                    }
                })
                .collect(),
        };
        if !self.seen_assertion_calls.insert(key) {
            return Ok(());
        }
        self.lower_function_assertions(function_view.statements())
    }

    fn lower_function_assertions(
        &mut self,
        statements: dae::FunctionStatements<'dae>,
    ) -> Result<(), GalecTargetError> {
        for statement in statements {
            match statement {
                dae::FunctionStatementView::Assertion {
                    condition,
                    provenance,
                    ..
                } => {
                    self.lower_function_assertion(condition, provenance.span())?;
                }
                dae::FunctionStatementView::For {
                    fold,
                    statements,
                    provenance,
                } if first_function_assertion(statements.clone()).is_some() => {
                    self.lower_function_loop_assertions(fold, statements, provenance.span())?;
                }
                dae::FunctionStatementView::Assignment { .. }
                | dae::FunctionStatementView::AssignmentGroup { .. }
                | dae::FunctionStatementView::For { .. } => {}
            }
        }
        Ok(())
    }

    fn lower_function_loop_assertions(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        statements: dae::FunctionStatements<'dae>,
        span: Span,
    ) -> Result<(), GalecTargetError> {
        let fold = self
            .view
            .function_fold(fold)
            .expect("checked function fold resolves");
        let domain = self
            .view
            .domain(fold.domain())
            .expect("checked function fold domain resolves");
        let binders = &domain.structured().binders;
        let depth = self.loop_index_bounds.len();
        let names = binders
            .iter()
            .enumerate()
            .map(|(ordinal, binder)| {
                gast::Name::ident(format!(
                    "rumoca_{}_{}_{}",
                    binder.display_name, depth, ordinal
                ))
            })
            .collect::<Vec<_>>();
        let frame_binders = names
            .iter()
            .cloned()
            .map(|name| gast::Expression::Ref(gast::Reference::local(name)))
            .collect();
        for (binder, name) in binders.iter().zip(&names) {
            self.loop_index_bounds.push(LoopIndexBound {
                name: name.clone(),
                minimum: binder.lower.min(binder.upper),
                maximum: binder.lower.max(binder.upper),
            });
        }
        self.comprehension_frames.push(ComprehensionFrame {
            domain: fold.domain().index(),
            binders: frame_binders,
        });
        let assertion_start = self.pending_prefix_statements.len();
        let lowered = self.lower_function_assertions(statements);
        self.comprehension_frames.pop();
        self.loop_index_bounds.truncate(depth);
        lowered?;

        let mut body = self.pending_prefix_statements.split_off(assertion_start);
        for (binder, name) in binders.iter().zip(names).rev() {
            body = vec![gast::Spanned::new(
                gast::Statement::For(gast::ForLoop {
                    iterator: Some(name),
                    start: gast::Expression::Integer(binder.lower),
                    step: (binder.step != 1).then_some(gast::Expression::Integer(binder.step)),
                    stop: gast::Expression::Integer(binder.upper),
                    body,
                }),
                span,
            )];
        }
        self.pending_prefix_statements.extend(body);
        Ok(())
    }

    pub(super) fn lower_function_assertion(
        &mut self,
        condition: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<(), GalecTargetError> {
        let condition = self.lower(condition)?;
        require_boolean(&condition, span)?;
        if condition.expression == gast::Expression::Bool(true) {
            return Ok(());
        }
        let signal = gast::Spanned::new(
            gast::Statement::Signal(vec![gast::Identifier::new(
                gast::PredefinedSignal::InvalidArgument.name(),
            )]),
            span,
        );
        self.pending_prefix_statements.push(gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches: vec![gast::IfBranch {
                    condition: gast::Condition::Expression(gast::Expression::Not(Box::new(
                        condition.expression,
                    ))),
                    body: vec![signal],
                    span,
                }],
                else_body: None,
            }),
            span,
        ));
        Ok(())
    }

    fn function_result(
        &self,
        function: dae::FunctionId<'dae>,
        output: u32,
    ) -> Result<dae::ExprId<'dae>, GalecTargetError> {
        let function_view = self
            .view
            .function(function)
            .expect("checked function identity resolves");
        // GAL-025: an MLS §12.9 external body is foreign code with no GALEC
        // projection. Report the exact interface instead of inlining nothing.
        if let Some(external) = function_view.external() {
            return Err(GalecTargetError::ExternalFunction {
                function: function_view.name().to_string(),
                language: external.language().as_str().to_owned(),
                span: function_view.declaration().span(),
            });
        }
        function_view
            .result_values()
            .rhs(output as usize)
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!("checked function output {output} is missing"),
            })
    }

    pub(super) fn lower_record_field_at(
        &mut self,
        base: dae::ExprId<'dae>,
        field: usize,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        if let Some(gast::Expression::Ref(reference)) = self
            .materialized_shared_record_fields
            .get(&(base.index(), field))
        {
            let reference = match reference {
                gast::Reference::Local(part) => gast::Reference::Local(gast::RefPart {
                    name: part.name.clone(),
                    subscripts: indices.to_vec(),
                    span,
                }),
                gast::Reference::State(_) => {
                    unreachable!("shared record storage is always function-local")
                }
            };
            return Ok(TypedExpression {
                expression: gast::Expression::Ref(reference),
                scalar_type,
            });
        }
        let node = self
            .view
            .expression(base)
            .expect("checked record base resolves");
        match node.operation() {
            dae::ExpressionOperation::Record(fields) => {
                let value =
                    fields
                        .get(field)
                        .ok_or_else(|| GalecTargetError::LoweringInternal {
                            detail: "checked record field ordinal is missing".to_owned(),
                        })?;
                self.lower_at(value, indices)
            }
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => self.lower_record_call_field(
                base,
                function,
                output,
                arguments,
                RecordFieldProjection {
                    field,
                    indices,
                    scalar_type,
                    span,
                },
            ),
            dae::ExpressionOperation::FunctionValue { definition, .. } => self
                .lower_function_value_record_field(definition, field, indices, scalar_type, span),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => {
                if self.function_scope == Some(parameter.function()) {
                    let function = self
                        .view
                        .function(parameter.function())
                        .expect("checked direct record function resolves");
                    let parameter_view = function
                        .parameters()
                        .nth(parameter.ordinal() as usize)
                        .expect("checked direct record parameter resolves");
                    let (field_name, field_type) = self
                        .view
                        .record_field(parameter_view.value_type(), field)
                        .expect("checked direct record parameter field resolves");
                    let field_type = self
                        .view
                        .value_type(field_type)
                        .expect("checked direct record parameter field type resolves");
                    return Ok(TypedExpression {
                        expression: self.lower_local_reference(
                            user_functions::record_parameter_field_name(
                                parameter_view,
                                field_name,
                            )?,
                            field_type.dimensions(),
                            indices,
                            span,
                        )?,
                        scalar_type,
                    });
                }
                let argument = self
                    .call_frames
                    .iter()
                    .rev()
                    .find(|frame| frame.function == parameter.function())
                    .and_then(|frame| frame.arguments.get(parameter.ordinal() as usize))
                    .copied()
                    .ok_or_else(|| GalecTargetError::LoweringInternal {
                        detail: "record function parameter escaped its checked call".to_owned(),
                    })?;
                self.lower_record_field_at(argument, field, indices, scalar_type, span)
            }
            dae::ExpressionOperation::Conditional(operands) => {
                self.lower_conditional_record_field(operands, field, indices, scalar_type, span)
            }
            _ => Err(unsupported(
                "record-field-projection",
                format!(
                    "checked record field base {:?} is not yet supported by the Rumoca GALEC projection",
                    node.kind()
                ),
                span,
            )),
        }
    }

    fn lower_record_call_field(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        projection: RecordFieldProjection<'_>,
    ) -> Result<TypedExpression, GalecTargetError> {
        if self.materialize_function_values
            && user_functions::is_directly_lowerable(self.view, function)
        {
            let function_view = self
                .view
                .function(function)
                .expect("checked direct record function resolves");
            let selected = user_functions::flattened_result_index(
                self.view,
                function_view,
                output,
                Some(projection.field as u32),
            )
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "record field is missing from the flattened GALEC function interface"
                    .to_owned(),
            })?;
            let names =
                self.materialize_function_call(call, function, arguments, projection.span)?;
            let name = names
                .get(selected)
                .expect("checked flattened record output resolves")
                .clone();
            return Ok(TypedExpression {
                expression: gast::Expression::Ref(gast::Reference::Local(gast::RefPart {
                    name,
                    subscripts: projection.indices.to_vec(),
                    span: projection.span,
                })),
                scalar_type: projection.scalar_type,
            });
        }
        let result = self.enter_function_call(
            call,
            function,
            output,
            arguments,
            projection.indices,
            projection.span,
        )?;
        let lowered = self.lower_record_field_at(
            result,
            projection.field,
            projection.indices,
            projection.scalar_type,
            projection.span,
        );
        self.call_frames.pop();
        lowered
    }

    fn lower_function_value_record_field(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
        field: usize,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        if self.function_scope == Some(definition.id().function()) {
            let value = self
                .view
                .function(definition.id().function())
                .expect("checked function identity resolves")
                .values()
                .find(|value| value.id() == definition.target())
                .expect("checked function definition target resolves");
            let (field_name, field_type) = self
                .view
                .record_field(value.value_type(), field)
                .expect("checked function record field resolves");
            let field_type = self
                .view
                .value_type(field_type)
                .expect("checked function record field type resolves");
            return Ok(TypedExpression {
                expression: self.lower_local_reference(
                    user_functions::record_value_field_name(value, field_name)?,
                    field_type.dimensions(),
                    indices,
                    span,
                )?,
                scalar_type,
            });
        }
        let field = u32::try_from(field).map_err(|_| {
            unsupported(
                "record-field-capacity",
                "record field ordinal exceeds the GALEC projection capacity".to_owned(),
                span,
            )
        })?;
        let Some(key) = self.function_value_key(definition, indices, vec![field]) else {
            return self.lower_record_field_at(
                definition.rhs(),
                field as usize,
                indices,
                scalar_type,
                span,
            );
        };
        if let Some(name) = self.materialized_function_values.get(&key) {
            return Ok(TypedExpression {
                expression: gast::Expression::Ref(gast::Reference::local(name.clone())),
                scalar_type,
            });
        }
        let value = self.lower_record_field_at(
            definition.rhs(),
            field as usize,
            indices,
            scalar_type,
            span,
        )?;
        self.store_materialized_function_value(key, value, scalar_type, span)
    }

    fn lower_conditional_record_field(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        field: usize,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        self.conditional_depth += 1;
        let result =
            self.lower_conditional_record_field_inner(operands, field, indices, scalar_type, span);
        self.conditional_depth -= 1;
        result
    }

    fn lower_conditional_record_field_inner(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        field: usize,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let entry_materialization = self.conditional_materialization_snapshot();
        let activation_operands = conditional_activation_operands(operands);
        let mut branches = Vec::new();
        for ordinal in (0..operands.len() - 1).step_by(2) {
            self.conditional_activation_path
                .push(ConditionalActivationKey {
                    kind: ConditionalActivationKind::ConditionalRecord,
                    operands: activation_operands.clone(),
                    branch: conditional_activation_ordinal(
                        ordinal / 2,
                        "conditional branch ordinal",
                    )?,
                });
            let condition_start = self.pending_prefix_statements.len();
            let condition = self.lower(
                operands
                    .get(ordinal)
                    .expect("checked conditional condition"),
            );
            let condition = match condition {
                Ok(condition) => condition,
                Err(error) => {
                    self.conditional_activation_path.pop();
                    return Err(error);
                }
            };
            if let Err(error) = require_boolean(&condition, span) {
                self.conditional_activation_path.pop();
                return Err(error);
            }
            let condition_prefix = self.pending_prefix_statements.split_off(condition_start);
            let condition_materialization = self.conditional_materialization_snapshot();
            let value_start = self.pending_prefix_statements.len();
            let value = self.lower_record_field_at(
                operands
                    .get(ordinal + 1)
                    .expect("checked conditional value"),
                field,
                indices,
                scalar_type,
                span,
            );
            let value = match value {
                Ok(value) => value,
                Err(error) => {
                    self.conditional_activation_path.pop();
                    return Err(error);
                }
            };
            branches.push(SelectionBranch {
                condition_prefix,
                condition: condition.expression,
                value: SelectionValue {
                    prefix: self.pending_prefix_statements.split_off(value_start),
                    expression: coerce(value, scalar_type, span)?,
                },
            });
            self.restore_conditional_materialization(&condition_materialization);
            self.conditional_activation_path.pop();
        }
        self.conditional_activation_path
            .push(ConditionalActivationKey {
                kind: ConditionalActivationKind::ConditionalRecord,
                operands: activation_operands,
                branch: conditional_activation_ordinal(
                    operands.len() / 2,
                    "conditional fallback ordinal",
                )?,
            });
        let fallback_start = self.pending_prefix_statements.len();
        let fallback = self.lower_record_field_at(
            operands
                .get(operands.len() - 1)
                .expect("checked conditional fallback"),
            field,
            indices,
            scalar_type,
            span,
        );
        let fallback = match fallback {
            Ok(fallback) => fallback,
            Err(error) => {
                self.conditional_activation_path.pop();
                return Err(error);
            }
        };
        let fallback = SelectionValue {
            prefix: self.pending_prefix_statements.split_off(fallback_start),
            expression: coerce(fallback, scalar_type, span)?,
        };
        self.conditional_activation_path.pop();
        self.restore_conditional_materialization(&entry_materialization);
        Ok(self.lower_lazy_selection(branches, fallback, scalar_type, span))
    }
}

fn conditional_activation_ordinal(ordinal: usize, kind: &str) -> Result<u32, GalecTargetError> {
    u32::try_from(ordinal).map_err(|_| GalecTargetError::LoweringInternal {
        detail: format!("{kind} exceeds the activation-key capacity"),
    })
}

fn expression_contains_array<'dae>(view: dae::DaeView<'dae>, root: dae::ExprId<'dae>) -> bool {
    let mut found = false;
    dae::for_each_expression(view, root, |_, expression| {
        found |= matches!(expression.operation(), dae::ExpressionOperation::Array(_));
    });
    found
}
