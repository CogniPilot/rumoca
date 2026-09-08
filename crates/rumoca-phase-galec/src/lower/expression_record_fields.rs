//! Checked record-field projection for scalar GALEC expressions.

use super::expression_functions::EnteredFunctionCall;
use super::expression_projection::{SelectionBranch, SelectionValue};
use super::*;

#[derive(Clone, Copy)]
struct RecordFieldProjection<'a> {
    field: usize,
    indices: &'a [gast::Expression],
    scalar_type: gast::ScalarType,
    span: Span,
}

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    pub(super) fn lower_record_field_at(
        &mut self,
        base: dae::ExprId<'dae>,
        field: usize,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let key = self.shared_record_field_key(base.index(), field);
        if let Some(gast::Expression::Ref(reference)) =
            self.materialized_shared_record_fields.get(&key)
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
        let node = self.view.exact_expression(base);
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
                ..
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
            )) => self.lower_function_parameter_record_field(
                parameter,
                field,
                indices,
                scalar_type,
                span,
            ),
            dae::ExpressionOperation::Conditional(operands) => self.lower_conditional_record_field(
                base,
                operands,
                field,
                indices,
                scalar_type,
                span,
            ),
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

    fn lower_function_parameter_record_field(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        field: usize,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        if self.function_scope == Some(parameter.function()) {
            let parameter_view = self.view.exact_function_parameter(parameter);
            let (field_name, field_type) = self
                .view
                .record_field(parameter_view.value_type(), field)
                .ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail: "checked direct record parameter field resolves".to_owned(),
                })?;
            let field_type = self.view.exact_value_type(field_type);
            return Ok(TypedExpression {
                expression: self.lower_local_reference(
                    user_functions::record_parameter_field_name(parameter_view, field_name)?,
                    field_type.dimensions(),
                    indices,
                    span,
                )?,
                scalar_type,
            });
        }
        let PreparedInlineArgument::Record(fields) = self.prepared_inline_argument(parameter)?
        else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "record function parameter has a primitive prepared argument".to_owned(),
            });
        };
        let value = fields
            .get(field)
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "record parameter field escaped its prepared argument".to_owned(),
            })?;
        if value.dimensions.is_empty() {
            if !indices.is_empty() {
                return Err(GalecTargetError::LoweringInternal {
                    detail: "scalar prepared record field received array indices".to_owned(),
                });
            }
            return Ok(TypedExpression {
                expression: value.expression.clone(),
                scalar_type: value.scalar_type,
            });
        }
        let gast::Expression::Ref(gast::Reference::Local(reference)) = &value.expression else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "prepared aggregate record field is not local storage".to_owned(),
            });
        };
        Ok(TypedExpression {
            expression: self.lower_local_reference(
                reference.name.clone(),
                &value.dimensions,
                indices,
                span,
            )?,
            scalar_type: value.scalar_type,
        })
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
            let function_view = self.view.exact_function(function);
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
                .ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail: "checked flattened record output resolves".to_owned(),
                })?
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
        self.lower_entered_function_call(
            EnteredFunctionCall {
                call,
                function,
                output,
                arguments,
                indices: projection.indices,
                span: projection.span,
            },
            |lowerer, result| {
                lowerer.lower_record_field_at(
                    result,
                    projection.field,
                    projection.indices,
                    projection.scalar_type,
                    projection.span,
                )
            },
        )
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
            let value = self.view.exact_function_value(definition.target());
            let (field_name, field_type) = self
                .view
                .record_field(value.value_type(), field)
                .ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail: "checked function record field resolves".to_owned(),
                })?;
            let field_type = self.view.exact_value_type(field_type);
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
        expression: dae::ExprId<'dae>,
        operands: dae::ExpressionOperands<'dae>,
        field: usize,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        self.conditional_depth += 1;
        let result = self.lower_conditional_record_field_inner(
            expression,
            operands,
            field,
            indices,
            scalar_type,
            span,
        );
        self.conditional_depth -= 1;
        result
    }

    fn lower_conditional_record_field_inner(
        &mut self,
        expression: dae::ExprId<'dae>,
        operands: dae::ExpressionOperands<'dae>,
        field: usize,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let entry_materialization = self.conditional_materialization_snapshot();
        let activation_operands = conditional_activation_operands(operands);
        let selection = self.selection_point(
            ConditionalActivationKind::ConditionalRecord,
            expression,
            &activation_operands,
            indices,
        )?;
        if operands.len() % 2 != 1 {
            return Err(GalecTargetError::LoweringInternal {
                detail:
                    "checked conditional pairs every condition with a value and carries a fallback"
                        .to_owned(),
            });
        }
        let pair_count = operands.len() / 2;
        let mut operand_iter = operands.iter();
        let mut branches = Vec::new();
        for pair_ordinal in 0..pair_count {
            let (Some(condition_operand), Some(value_operand)) =
                (operand_iter.next(), operand_iter.next())
            else {
                return Err(GalecTargetError::LoweringInternal {
                    detail: "checked conditional pairs a condition with a value".to_owned(),
                });
            };
            let branch_activation = ConditionalActivationKey {
                kind: ConditionalActivationKind::ConditionalRecord,
                operands: activation_operands.clone(),
                selection,
                branch: conditional_activation_ordinal(pair_ordinal, "conditional branch ordinal")?,
            };
            let condition_start = self.pending_prefix_statements.len();
            let condition = self.lower(condition_operand);
            let condition = condition?;
            require_boolean(&condition, span)?;
            let condition_prefix = self.pending_prefix_statements.split_off(condition_start);
            let condition_materialization = self.conditional_materialization_snapshot();
            self.conditional_activation_path.push(branch_activation);
            let value_start = self.pending_prefix_statements.len();
            let value =
                self.lower_record_field_at(value_operand, field, indices, scalar_type, span);
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
                selection,
                branch: conditional_activation_ordinal(
                    operands.len() / 2,
                    "conditional fallback ordinal",
                )?,
            });
        let Some(fallback_operand) = operand_iter.next() else {
            return Err(GalecTargetError::LoweringInternal {
                detail: "checked conditional carries a fallback operand".to_owned(),
            });
        };
        let fallback_start = self.pending_prefix_statements.len();
        let fallback =
            self.lower_record_field_at(fallback_operand, field, indices, scalar_type, span);
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
