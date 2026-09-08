use super::*;

pub(in crate::wire) fn raw_expression_span(expression: &Expression) -> Span {
    match expression {
        Expression::Binary { span, .. }
        | Expression::Unary { span, .. }
        | Expression::VarRef { span, .. }
        | Expression::BuiltinCall { span, .. }
        | Expression::FunctionCall { span, .. }
        | Expression::StringConversion { span, .. }
        | Expression::Literal { span, .. }
        | Expression::If { span, .. }
        | Expression::Array { span, .. }
        | Expression::Tuple { span, .. }
        | Expression::Range { span, .. }
        | Expression::ArrayComprehension { span, .. }
        | Expression::Index { span, .. }
        | Expression::FieldAccess { span, .. }
        | Expression::Empty { span } => *span,
    }
}

pub(in crate::wire) fn validate_when_equation(
    checker: &mut WireSemanticChecker<'_>,
    equation: &WhenEquation,
) -> Result<(), FlatWireError> {
    require_span(equation.span(), "when equation")?;
    match equation {
        WhenEquation::Assign { target, value, .. } => {
            checker.validate_named_write_target(target)?;
            checker.visit_expression(value)
        }
        WhenEquation::Reinit { state, value, .. } => {
            checker.validate_named_write_target(state)?;
            checker.visit_expression(value)
        }
        WhenEquation::Assert {
            condition,
            message,
            level,
            ..
        } => {
            checker.visit_expression(condition)?;
            checker.visit_expression(message)?;
            if let Some(level) = level {
                checker.visit_expression(level)?;
            }
            Ok(())
        }
        WhenEquation::Terminate { message, .. } => checker.visit_expression(message),
        WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (condition, equations) in branches {
                checker.visit_expression(condition)?;
                for equation in equations {
                    validate_when_equation(checker, equation)?;
                }
            }
            if let Some(equations) = else_branch {
                for equation in equations {
                    validate_when_equation(checker, equation)?;
                }
            }
            Ok(())
        }
        WhenEquation::FunctionCallOutputs {
            outputs, function, ..
        } => {
            checker.visit_expression(function)?;
            let Expression::FunctionCall {
                name,
                args,
                call_kind: FunctionCallKind::Invocation,
                ..
            } = function
            else {
                return Err(FlatWireError::InvalidFunctionCall {
                    function: VarName::new("<when-output>"),
                    reason: "a when output group must be owned by an actual function invocation",
                });
            };
            let resolved =
                name.resolved_function()
                    .ok_or_else(|| FlatWireError::InvalidFunctionCall {
                        function: name.var_name().clone(),
                        reason: "a when output group requires an exact function identity",
                    })?;
            let target = checker
                .targets
                .by_function_instance
                .get(&resolved.instance_id)
                .ok_or_else(|| FlatWireError::InvalidFunctionCall {
                    function: name.var_name().clone(),
                    reason: "the resolved function instance is absent",
                })?;
            if outputs.len() > target.function.outputs.len() {
                return Err(FlatWireError::InvalidFunctionCall {
                    function: name.var_name().clone(),
                    reason: "the when equation claims more outputs than the exact function interface",
                });
            }
            let vector_prefix = checker.validate_call_argument_shapes(
                target,
                args,
                resolved,
                FunctionCallKind::Invocation,
            )?;
            for (output, slot) in outputs.iter().zip(&target.function.outputs) {
                if function_param_record_identity(checker.model, slot)?.is_some() {
                    return Err(FlatWireError::InvalidFunctionCall {
                        function: name.var_name().clone(),
                        reason: "a named when output cannot authenticate a record-valued target identity",
                    });
                }
                let mut expected_dimensions = vector_prefix.clone();
                expected_dimensions.extend_from_slice(slot.dimensions());
                checker.validate_named_write_target_shape(output, &expected_dimensions)?;
            }
            Ok(())
        }
    }
}
