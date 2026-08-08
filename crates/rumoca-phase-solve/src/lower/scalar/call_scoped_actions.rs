//! Specialize function assertions at one exact call context.

use super::*;
use crate::lower::call_scoped_actions::CollectedCallAssertion;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn schedule_function_assertions(
        &self,
        statements: dae::FunctionStatements<'dae>,
        call_span: Span,
    ) -> Result<(), LowerError> {
        for statement in statements {
            self.schedule_function_statement_assertion(statement, call_span)?;
        }
        Ok(())
    }

    fn schedule_function_statement_assertion(
        &self,
        statement: dae::FunctionStatementView<'dae>,
        call_span: Span,
    ) -> Result<(), LowerError> {
        match statement {
            dae::FunctionStatementView::Assignment { .. }
            | dae::FunctionStatementView::AssignmentGroup { .. } => Ok(()),
            dae::FunctionStatementView::Assertion {
                condition,
                message,
                provenance,
            } => self.collect_assertion(condition, message, provenance, call_span),
            dae::FunctionStatementView::For {
                fold, statements, ..
            } => self.schedule_asserting_fold(fold, statements, call_span),
        }
    }

    fn schedule_asserting_fold(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        statements: dae::FunctionStatements<'dae>,
        call_span: Span,
    ) -> Result<(), LowerError> {
        if !has_assertion(statements.clone()) {
            return Ok(());
        }
        self.schedule_fold_assertions(fold, statements, call_span)
    }

    fn schedule_fold_assertions(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        statements: dae::FunctionStatements<'dae>,
        call_span: Span,
    ) -> Result<(), LowerError> {
        let fold_view = self.view.function_fold(fold).ok_or_else(|| {
            LowerError::contract("function fold identity does not resolve", call_span)
        })?;
        let point_count = self
            .view
            .domain(fold_view.domain())
            .expect("checked function fold domain resolves")
            .structured()
            .scalar_count()
            .map_err(|error| LowerError::contract(error.to_string(), call_span))?;
        for point in 0..point_count {
            for statement in statements.clone() {
                self.schedule_fold_statement_assertion(fold, point, statement, call_span)?;
            }
        }
        Ok(())
    }

    fn schedule_fold_statement_assertion(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        point: usize,
        statement: dae::FunctionStatementView<'dae>,
        call_span: Span,
    ) -> Result<(), LowerError> {
        match statement {
            dae::FunctionStatementView::Assignment { .. }
            | dae::FunctionStatementView::AssignmentGroup { .. } => Ok(()),
            dae::FunctionStatementView::Assertion {
                condition,
                message,
                provenance,
            } => {
                self.collect_fold_assertion(fold, point, condition, message, provenance, call_span)
            }
            dae::FunctionStatementView::For { statements, .. }
                if has_assertion(statements.clone()) =>
            {
                Err(LowerError::non_computable(
                    "nested loop-carried function assertions do not yet have an exact Solve schedule",
                    call_span,
                ))
            }
            dae::FunctionStatementView::For { .. } => Ok(()),
        }
    }

    fn collect_assertion(
        &self,
        condition: dae::ExprId<'dae>,
        message: dae::ExprId<'dae>,
        provenance: dae::DaeProvenance,
        call_span: Span,
    ) -> Result<(), LowerError> {
        let message = self.literal_assertion_message(message, call_span)?;
        let root_program = self
            .active_clock
            .is_none()
            .then(|| self.assertion_root_program(condition, call_span))
            .transpose()?;
        let action_program = self.assertion_action_program(condition, call_span)?;
        self.insert_assertion(root_program, action_program, message, provenance)
    }

    fn collect_fold_assertion(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        point: usize,
        condition: dae::ExprId<'dae>,
        message: dae::ExprId<'dae>,
        provenance: dae::DaeProvenance,
        call_span: Span,
    ) -> Result<(), LowerError> {
        let message = self.literal_assertion_message(message, call_span)?;
        let root_program = self
            .active_clock
            .is_none()
            .then(|| self.fold_assertion_root_program(fold, point, condition, call_span))
            .transpose()?;
        let action_program =
            self.fold_assertion_action_program(fold, point, condition, call_span)?;
        self.insert_assertion(root_program, action_program, message, provenance)
    }

    fn insert_assertion(
        &self,
        root_program: Option<Vec<solve::LinearOp>>,
        action_program: Vec<solve::LinearOp>,
        message: String,
        provenance: dae::DaeProvenance,
    ) -> Result<(), LowerError> {
        self.layout
            .call_scoped_actions
            .borrow_mut()
            .insert(CollectedCallAssertion {
                root_program,
                action_program,
                action: solve::SolveEventAction {
                    kind: solve::SolveEventActionKind::Assert,
                    message: solve::SolveEventMessage {
                        parts: vec![solve::SolveEventMessagePart::Text(message)],
                    },
                    span: provenance.span(),
                    origin: provenance.origin().to_string(),
                },
            });
        Ok(())
    }

    fn assertion_root_program(
        &self,
        condition: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut compiler = self.fork_for_call_action();
        let activation = compiler.activation(span)?;
        let condition = compiler.expression(condition, 0)?;
        let safe = compiler.constant(-1.0, span)?;
        let failed = compiler.constant(1.0, span)?;
        let active_indicator = compiler.select(condition, safe, failed, span)?;
        let root = compiler.select(activation, active_indicator, safe, span)?;
        compiler
            .ops
            .push(solve::LinearOp::StoreOutput { src: root });
        Ok(compiler.ops)
    }

    fn assertion_action_program(
        &self,
        condition: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut compiler = self.fork_for_call_action();
        let activation = compiler.activation(span)?;
        let condition = compiler.expression(condition, 0)?;
        let failed = compiler.unary(dae::UnaryOperator::Not, condition, span)?;
        let active_failure = compiler.binary(dae::BinaryOperator::And, activation, failed, span)?;
        compiler.ops.push(solve::LinearOp::StoreOutput {
            src: active_failure,
        });
        Ok(compiler.ops)
    }

    fn fold_assertion_root_program(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        point: usize,
        condition: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut compiler = self.fork_for_call_action();
        compiler.bind_function_fold_point(fold, point, span)?;
        let activation = compiler.activation(span)?;
        let condition = compiler.expression(condition, 0)?;
        let safe = compiler.constant(-1.0, span)?;
        let failed = compiler.constant(1.0, span)?;
        let active_indicator = compiler.select(condition, safe, failed, span)?;
        let root = compiler.select(activation, active_indicator, safe, span)?;
        compiler
            .ops
            .push(solve::LinearOp::StoreOutput { src: root });
        Ok(compiler.ops)
    }

    fn fold_assertion_action_program(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        point: usize,
        condition: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut compiler = self.fork_for_call_action();
        compiler.bind_function_fold_point(fold, point, span)?;
        let activation = compiler.activation(span)?;
        let condition = compiler.expression(condition, 0)?;
        let failed = compiler.unary(dae::UnaryOperator::Not, condition, span)?;
        let active_failure = compiler.binary(dae::BinaryOperator::And, activation, failed, span)?;
        compiler.ops.push(solve::LinearOp::StoreOutput {
            src: active_failure,
        });
        Ok(compiler.ops)
    }

    fn bind_function_fold_point(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        point: usize,
        span: Span,
    ) -> Result<(), LowerError> {
        let values = self.function_fold_values_before(fold, point, span)?;
        self.enter_function_fold_context(fold, point, values, span)
    }

    fn function_fold_values_before(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        point: usize,
        span: Span,
    ) -> Result<Vec<Vec<solve::Reg>>, LowerError> {
        let fold_view = self
            .view
            .function_fold(fold)
            .ok_or_else(|| LowerError::contract("function fold identity does not resolve", span))?;
        let mut values = fold_view
            .initial_values()
            .rhs_iter()
            .map(|initial| self.expression_scalars(initial))
            .collect::<Result<Vec<_>, _>>()?;
        for previous in 0..point {
            self.enter_function_fold_context(fold, previous, values, span)?;
            let updates = fold_view
                .update_values()
                .rhs_iter()
                .map(|update| self.expression_scalars(update))
                .collect::<Result<Vec<_>, _>>();
            self.leave_function_fold_context();
            values = updates?;
        }
        Ok(values)
    }

    fn expression_scalars(
        &mut self,
        expression: dae::ExprId<'dae>,
    ) -> Result<Vec<solve::Reg>, LowerError> {
        (0..scalar_count(self.view, expression))
            .map(|scalar| self.expression(expression, scalar))
            .collect()
    }

    fn enter_function_fold_context(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        point: usize,
        values: Vec<Vec<solve::Reg>>,
        span: Span,
    ) -> Result<(), LowerError> {
        let fold_view = self
            .view
            .function_fold(fold)
            .ok_or_else(|| LowerError::contract("function fold identity does not resolve", span))?;
        let indices = self
            .view
            .domain(fold_view.domain())
            .expect("checked function fold domain resolves")
            .structured()
            .index_tuple_at(point)
            .expect("checked function fold domain remains valid")
            .ok_or_else(|| LowerError::contract("function fold point is out of range", span))?;
        self.enter_context(ScalarContextFrame::Domain {
            parent: self.context_id,
            domain: fold_view.domain(),
            values: indices.clone(),
        });
        self.domain_points.push((fold_view.domain(), indices));
        self.enter_context(ScalarContextFrame::Fold {
            parent: self.context_id,
            fold,
            values: values.clone(),
        });
        self.function_fold_values.push((fold, values));
        Ok(())
    }

    fn leave_function_fold_context(&mut self) {
        self.function_fold_values
            .pop()
            .expect("function fold context was just entered");
        self.leave_context();
        self.domain_points
            .pop()
            .expect("function fold domain context was just entered");
        self.leave_context();
    }

    fn activation(&mut self, span: Span) -> Result<solve::Reg, LowerError> {
        let guards = self.activation_path.clone();
        let mut activation = self.constant(1.0, span)?;
        if let Some(clock) = self.active_clock {
            let parameter = self
                .layout
                .clock_activations
                .get(clock.index() as usize)
                .copied()
                .ok_or_else(|| {
                    LowerError::contract("clock-owned assertion has no activation parameter", span)
                })?;
            let clock_active = self.load_slot(solve::scalar_slot_p(parameter), span)?;
            activation = self.binary(dae::BinaryOperator::And, activation, clock_active, span)?;
        }
        for guard in guards {
            let mut value = self.expression(guard.condition, 0)?;
            if !guard.expected {
                value = self.unary(dae::UnaryOperator::Not, value, span)?;
            }
            activation = self.binary(dae::BinaryOperator::And, activation, value, span)?;
        }
        Ok(activation)
    }

    fn fork_for_call_action(&self) -> Self {
        let mut compiler = Self::new(self.view, self.layout, None);
        compiler.domain_points = self.domain_points.clone();
        compiler.function_arguments = self.function_arguments.clone();
        compiler.active_clock = self.active_clock;
        compiler.sampled_source = self.sampled_source;
        compiler.derivative_definitions = self.derivative_definitions;
        compiler.parameter_substitutions = self.parameter_substitutions;
        compiler.active_parameters = self.active_parameters.clone();
        compiler.activation_path = self.activation_path.clone();
        compiler.active_call_assertions = self.active_call_assertions.clone();
        compiler.call_action_compilation = true;
        compiler
    }

    fn literal_assertion_message(
        &self,
        message: dae::ExprId<'dae>,
        call_span: Span,
    ) -> Result<String, LowerError> {
        let expression = self
            .view
            .expression(message)
            .expect("checked function assertion message resolves");
        match expression.operation() {
            dae::ExpressionOperation::Literal(dae::DaeLiteral::String(message)) => {
                Ok(message.clone())
            }
            _ => Err(LowerError::non_computable(
                "call-scoped assertion messages with runtime conversions do not yet have a call-specialized Solve schedule",
                call_span,
            )),
        }
    }
}

fn has_assertion(statements: dae::FunctionStatements<'_>) -> bool {
    statements.into_iter().any(|statement| match statement {
        dae::FunctionStatementView::Assignment { .. }
        | dae::FunctionStatementView::AssignmentGroup { .. } => false,
        dae::FunctionStatementView::Assertion { .. } => true,
        dae::FunctionStatementView::For { statements, .. } => has_assertion(statements),
    })
}
