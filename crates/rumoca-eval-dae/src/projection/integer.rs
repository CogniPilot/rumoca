//! Checked Integer evaluation for subscripts, sizes and Integer profiles.
use super::*;

impl<'dae> Projection<'_, 'dae> {
    pub(super) fn integer(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<i64, ProjectionError> {
        let raw = expression.index();
        if !self.integer_stack.insert(raw) {
            return Err(ProjectionError::DynamicSubscript {
                span: self.node(expression).provenance().span(),
            });
        }
        let result = self.integer_inner(expression, scalar_index);
        self.integer_stack.remove(&raw);
        result
    }

    pub(super) fn integer_inner(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<i64, ProjectionError> {
        let node = self.node(expression);
        self.expect_scalar_index(node, scalar_index)?;
        let span = node.provenance().span();
        match node.operation() {
            dae::ExpressionOperation::Literal(
                dae::DaeLiteral::Integer(value) | dae::DaeLiteral::Enumeration(value),
            ) => Ok(*value),
            dae::ExpressionOperation::Range(range) => {
                let offset = i64::try_from(scalar_index)
                    .map_err(|_| ProjectionError::IntegerOverflow { span })?;
                range
                    .start()
                    .value()
                    .checked_add(
                        range
                            .effective_step()
                            .checked_mul(offset)
                            .ok_or(ProjectionError::IntegerOverflow { span })?,
                    )
                    .ok_or(ProjectionError::IntegerOverflow { span })
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
                let Some((_, point)) = self
                    .domain_contexts
                    .points
                    .iter()
                    .rev()
                    .find(|(domain, _)| *domain == binder.domain())
                else {
                    return Err(ProjectionError::DynamicSubscript { span });
                };
                point
                    .get(binder.ordinal() as usize)
                    .copied()
                    .ok_or(ProjectionError::DynamicSubscript { span })
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self.integer_parameter(parameter, scalar_index, span),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
                let variable = self
                    .view
                    .variable(parameter.into())
                    .expect("checked parameter coordinate resolves");
                let binding = variable
                    .binding()
                    .ok_or(ProjectionError::DynamicSubscript { span })?;
                self.integer(binding, scalar_index)
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let value = self.integer(operand, scalar_index)?;
                match operator {
                    dae::UnaryOperator::Plus => Ok(value),
                    dae::UnaryOperator::Negate => value
                        .checked_neg()
                        .ok_or(ProjectionError::IntegerOverflow { span }),
                    dae::UnaryOperator::Not => Err(ProjectionError::DynamicSubscript { span }),
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                let lhs = self.integer(lhs, scalar_index)?;
                let rhs = self.integer(rhs, scalar_index)?;
                integer_binary(operator, lhs, rhs, span)
            }
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
                ..
            } => self.integer_call(function, output, arguments, scalar_index, span),
            dae::ExpressionOperation::Array(elements) => {
                let first = elements.get(0).expect("checked array is nonempty");
                let element_count = self.scalar_count(first);
                self.integer(
                    elements
                        .get(scalar_index / element_count)
                        .expect("checked integer array projection selects an element"),
                    scalar_index % element_count,
                )
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                let base_index = self.indexed_base_scalar(
                    base,
                    subscripts,
                    node.value_type().dimensions(),
                    scalar_index,
                )?;
                self.integer(base, base_index)
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.integer(definition.rhs(), scalar_index)
            }
            _ => Err(ProjectionError::DynamicSubscript { span }),
        }
    }

    pub(super) fn integer_parameter(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        scalar_index: usize,
        span: Span,
    ) -> Result<i64, ProjectionError> {
        let Some(frame) = self.function_frames.last() else {
            return Err(ProjectionError::FunctionRecursion { span });
        };
        if frame.function() != parameter.function() {
            return Err(ProjectionError::FunctionRecursion { span });
        }
        let ordinal = parameter.ordinal();
        match frame {
            FunctionFrame::Actual { arguments, .. } => {
                let argument = arguments
                    .get(ordinal as usize)
                    .copied()
                    .ok_or(ProjectionError::FunctionRecursion { span })?;
                self.in_caller_context(|projection| projection.integer(argument, scalar_index))
            }
            FunctionFrame::Summary { function, integers } => {
                if let Some(binding) = integers
                    .iter()
                    .find(|binding| binding.parameter == ordinal && binding.scalar == scalar_index)
                {
                    return Ok(binding.value);
                }
                // An unbound conditional selector retains the ordinary dynamic
                // base/subscript union. Do not request a stronger actual-value
                // certificate from the caller or specialize Boolean/Real inputs.
                if self.activation == Activation::Conditional {
                    return Err(ProjectionError::DynamicSubscript { span });
                }
                let function = function.index();
                if let Some(capture) = self
                    .function_summary_captures
                    .last_mut()
                    .filter(|capture| capture.function == function)
                    && !capture.needed_integers.contains(&(ordinal, scalar_index))
                {
                    capture.cacheable = false;
                    capture.needed_integers.push((ordinal, scalar_index));
                }
                Err(ProjectionError::DynamicSubscript { span })
            }
        }
    }

    pub(super) fn integer_call(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
        span: Span,
    ) -> Result<i64, ProjectionError> {
        if self.function_frames.len() >= 256 {
            return Err(ProjectionError::FunctionRecursion { span });
        }
        let arguments = arguments.iter().collect::<Vec<_>>();
        let result = self.function_result(function, output, span)?;
        self.push_frame(FunctionFrame::Actual {
            function,
            arguments,
        });
        let value = self.integer(result, scalar_index);
        self.pop_frame();
        value
    }
}

fn integer_binary(
    operator: dae::BinaryOperator,
    lhs: i64,
    rhs: i64,
    span: Span,
) -> Result<i64, ProjectionError> {
    let overflow = || ProjectionError::IntegerOverflow { span };
    match operator {
        dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => {
            lhs.checked_add(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => {
            lhs.checked_sub(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
            lhs.checked_mul(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide if rhs != 0 => {
            lhs.checked_div(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Power | dae::BinaryOperator::ElementwisePower if rhs >= 0 => lhs
            .checked_pow(u32::try_from(rhs).map_err(|_| overflow())?)
            .ok_or_else(overflow),
        _ => Err(ProjectionError::DynamicSubscript { span }),
    }
}
