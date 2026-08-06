//! Checked Modelica function inlining, call-scoped assertions, and record
//! field projection for the scalar GALEC expression boundary.

use super::*;

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    pub(super) fn lower_call_at(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let result = self.enter_function_call(call, function, output, arguments, span)?;
        let lowered = self.lower_at(result, indices);
        self.call_frames.pop();
        lowered
    }

    fn enter_function_call(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
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
        self.call_frames.push(CallFrame {
            call,
            function,
            arguments: arguments.iter().collect(),
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
                    statements,
                    provenance,
                    ..
                } if first_function_assertion(statements.clone()).is_some() => {
                    return Err(unsupported(
                        "function-loop-assertion",
                        "a compact function-loop assertion needs one checked GALEC loop action"
                            .to_owned(),
                        provenance.span(),
                    ));
                }
                dae::FunctionStatementView::Assignment { .. }
                | dae::FunctionStatementView::For { .. } => {}
            }
        }
        Ok(())
    }

    fn lower_function_assertion(
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
        self.pending_assertions.push(gast::Spanned::new(
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
            } => {
                let result = self.enter_function_call(base, function, output, arguments, span)?;
                let lowered = self.lower_record_field_at(result, field, indices, scalar_type, span);
                self.call_frames.pop();
                lowered
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.lower_record_field_at(definition.rhs(), field, indices, scalar_type, span)
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => {
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
                let mut branches = Vec::new();
                for ordinal in (0..operands.len() - 1).step_by(2) {
                    let condition =
                        self.lower(operands.get(ordinal).expect("checked condition operand"))?;
                    require_boolean(&condition, span)?;
                    let value = self.lower_record_field_at(
                        operands.get(ordinal + 1).expect("checked value operand"),
                        field,
                        indices,
                        scalar_type,
                        span,
                    )?;
                    branches.push((condition.expression, coerce(value, scalar_type, span)?));
                }
                let fallback = self.lower_record_field_at(
                    operands
                        .get(operands.len() - 1)
                        .expect("checked conditional fallback"),
                    field,
                    indices,
                    scalar_type,
                    span,
                )?;
                Ok(TypedExpression {
                    expression: gast::Expression::If(gast::IfExpression {
                        branches,
                        else_value: Box::new(coerce(fallback, scalar_type, span)?),
                    }),
                    scalar_type,
                })
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
}
